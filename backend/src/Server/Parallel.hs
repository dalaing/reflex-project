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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Main (
    main
  ) where

import Data.Proxy
import System.Environment

import Control.Monad (void, forever)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Ref
import Data.Foldable (traverse_)
import Data.Hashable
import Data.IORef (readIORef)
import Data.Functor.Identity

import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Concurrent.STM
import Control.Exception (finally)
import qualified Control.Concurrent.STM.Map as SM

import Servant.API
import Servant.Server
import Snap.Core
import Snap.Snaplet
import Snap.Http.Server
import Snap.Util.FileServe

import Reflex
import Reflex.Host.Class
import Data.Dependent.Sum

import Servant.Server.Reflex
import Servant.Server.Reflex.Comms
import Servant.Server.Reflex.Comms.Parallel

import API

api :: Proxy MyAPI
api = Proxy

type MyGuest = Endpoint () (Snap [Payload]) :<|>
               Endpoint String (Snap NoContent) :<|>
               Endpoint Int (Snap NoContent)

myGuest :: Proxy MyGuest
myGuest = Proxy

-- We should do two versions of this:
-- - one like below, where multiple calls can be happening and completing in arbitrary order
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

guest :: App t m 'Parallel MyGuest
guest e = do
  let
    eGet :<|> ePost :<|> eDelete = e

  let 
    rem i xs
      | i < 0 = xs
      | i >= length xs = xs
      | otherwise = let (ys, _ : zs) = splitAt i xs in ys ++ zs

  bList <- accum (flip ($)) [] . leftmost $ [
      ((:) . snd) <$> ePost
    , (\x -> rem (snd x)) <$> eDelete
    ]

  performEvent_ $ (liftIO . putStrLn $ "FRP: get") <$ eGet
  performEvent_ $ (liftIO . putStrLn $ "FRP: post") <$ ePost
  performEvent_ $ (liftIO . putStrLn $ "FRP: delete") <$ eDelete

  let
    fnGet = return . fmap Payload . reverse
    eGetOut = (\l (t, _) -> (t, fnGet l)) <$> bList <@> eGet

    ePostOut = (\(t, _) -> (t, return NoContent)) <$> ePost
    eDeleteOut = (\(t, _) -> (t, return NoContent)) <$> eDelete

  return $
    eGetOut :<|>
    ePostOut :<|>
    eDeleteOut

apiServer :: Source 'Parallel MyGuest -> ServerT MyAPI (Handler () ())
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

app :: String -> Source 'Parallel MyGuest -> SnapletInit () ()
app baseDir source = makeSnaplet "test" "test" Nothing $ do
  addRoutes 
    [ ("api", serveSnap api $ apiServer source)
    , ("", serveDirectory baseDir)
    ]
  return ()

main :: IO ()
main = do
  baseDir : _ <- getArgs
  source <- atomically $ mkSource (Proxy :: Proxy 'Parallel) myGuest
  eventThreadId <- forkIO $ host (Proxy :: Proxy 'Parallel) myGuest source guest
  finally (serveSnaplet defaultConfig $ app baseDir source) $ do
    killThread eventThreadId
