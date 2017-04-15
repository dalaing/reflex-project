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
import Snap.Snaplet
import Snap.Http.Server
import Snap.Util.FileServe

import Reflex

import Servant.Server.Reflex
import Servant.Server.Reflex.Comms

import API

api :: Proxy MyAPI
api = Proxy

type MyGuest = ToServantHost MyAPI

myGuest :: Proxy MyGuest
myGuest = Proxy

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
    remove i xs
      | i < 0 = xs
      | i >= length xs = xs
      | otherwise = let (ys, _ : zs) = splitAt i xs in ys ++ zs

  bList <- accum (flip ($)) [] . leftmost $ [
      ((:) . value . snd) <$> ePost
    , (remove . snd) <$> eDelete
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
apiServer = serve (Proxy :: Proxy 'Parallel) (Proxy :: Proxy MyAPI)

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
  finally (serveSnaplet defaultConfig $ app baseDir source) $
    killThread eventThreadId
