{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Main (
    main
  ) where

import Data.Proxy
import System.Environment

import Control.Monad.Trans (liftIO)

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM
import Control.Exception (finally)

import Servant.API
import Servant.Server
import Snap.Core
import Snap.Snaplet
import Snap.Http.Server
import Snap.Util.FileServe

import Reflex

import Servant.Server.Reflex
import Servant.Server.Reflex.Comms
import Servant.Server.Reflex.Comms.Serial

import API

api :: Proxy MyAPI
api = Proxy

type MyGuest = Endpoint () (Snap [Payload]) :<|>
               Endpoint String (Snap NoContent) :<|>
               Endpoint Int (Snap NoContent)

myGuest :: Proxy MyGuest
myGuest = Proxy

-- We should do two versions of this:
-- - one like below, where multiple calls can bu happening and completing in arbitrary order
--   - should probably play with a version of this where each post effects the behavior immediately but returns the ticket after an arbitrary delay
--   - should use bounded queues for the inputs, to deal with back pressure
--
-- - another version, where the calls to each point are linearised, and so the ticket handling can be 
--   done with an input queue and an output queue, which can be taken care or entirely within the host
--   - do we even need tickets in this setting?
--   - should be able to handle random delays (and other calls happening at the same time)
--   - could use TMVar on the input to do that
--     - read from it, do the processing, take the TMVar after the response has been received

infixl 4 <@>
(<@>) :: Reflex t => Behavior t (a -> b) -> Event t a -> Event t b
(<@>) b = push $ \x -> do
  f <- sample b
  return . Just . f $ x

infixl 4 <@
(<@) :: Reflex t => Behavior t b -> Event t a -> Event t b
(<@) = tag

guest :: App t m 'Serial MyGuest
guest e = do
  let
    eGet :<|> ePost :<|> eDelete = e

  let
    remove i xs
      | i < 0 = xs
      | i >= length xs = xs
      | otherwise = let (ys, _ : zs) = splitAt (length xs - 1 - i) xs in ys ++ zs

  bList <- accum (flip ($)) [] . leftmost $ [
      (:) <$> ePost
    , remove <$> eDelete
    ]

  performEvent_ $ (liftIO . putStrLn $ "FRP: get") <$ eGet
  performEvent_ $ (liftIO . putStrLn $ "FRP: post") <$ ePost
  performEvent_ $ (liftIO . putStrLn $ "FRP: delete") <$ eDelete

  let
    eGetOut =
      (return . fmap Payload . reverse) <$> bList <@ eGet
    ePostOut =
      return NoContent <$ ePost
    eDeleteOut =
      return NoContent <$ eDelete

  return $
    eGetOut :<|>
    ePostOut :<|>
    eDeleteOut

apiServer :: Source 'Serial MyGuest -> ServerT MyAPI (Handler () ())
apiServer source = handleGet :<|> handlePost :<|> handleDelete
  where
    sGet :<|> sPost :<|> sDelete = source
    handleGet = do
      liftIO . putStrLn $ "get"
      res <- liftIO $ reqRes sGet ()
      liftSnap res

    handlePost (Payload s) = do
      liftIO . putStrLn $ "post " ++ s
      res <- liftIO $ reqRes sPost s
      liftSnap res

    handleDelete i = do
      liftIO . putStrLn $ "delete " ++ show i
      res <- liftIO $ reqRes sDelete i
      liftSnap res

app :: String -> Source 'Serial MyGuest -> SnapletInit () ()
app baseDir source = makeSnaplet "test" "test" Nothing $ do
  addRoutes
    [ ("api", serveSnap api $ apiServer source)
    , ("", serveDirectory baseDir)
    ]
  return ()

main :: IO ()
main = do
  baseDir : _ <- getArgs
  source <- atomically $ mkSource (Proxy :: Proxy 'Serial) myGuest
  eventThreadId <- forkIO $ host (Proxy :: Proxy 'Serial) myGuest source guest
  finally (serveSnaplet defaultConfig $ app baseDir source) $
    killThread eventThreadId
