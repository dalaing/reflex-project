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

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
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

import API

api :: Proxy MyAPI
api = Proxy

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

data Input t =
  Input {
    ieGet    :: Event t ()
  , iePost   :: Event t String
  , ieDelete :: Event t Int
  }

data Output t =
  Output {
    oeGet    :: Event t (Snap [Payload])
  , oePost   :: Event t (Snap NoContent)
  , oeDelete :: Event t (Snap NoContent)
  }

infixl 4 <@>
(<@>) :: Reflex t => Behavior t (a -> b) -> Event t a -> Event t b
(<@>) b = push $ \x -> do
  f <- sample b
  return . Just . f $ x

infixl 4 <@
(<@) :: Reflex t => Behavior t b -> Event t a -> Event t b
(<@) = tag

type App t m = ( ReflexHost t
               , PrimMonad (HostFrame t)
               , MonadIO (HostFrame t)
               , MonadHold t m
               , MonadFix m
               , Ref m ~ Ref IO
               )
             => Input t
             -> PerformEventT t m (Output t)

guest :: App t m
guest (Input eGet ePost eDelete) = do

  let 
    rem i xs
      | i < 0 = xs
      | i >= length xs = xs
      | otherwise = let (ys, _ : zs) = splitAt (length xs - 1 - i) xs in ys ++ zs

  bList <- accum (flip ($)) [] . leftmost $ [
      (:) <$> ePost
    , rem <$> eDelete
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
    Output
      eGetOut
      ePostOut
      eDeleteOut

host :: Source -> (forall t m. App t m) -> IO ()
host source guest = runSpiderHost $ do
  (eGetReq, eGetTriggerRef) <- newEventWithTriggerRef
  (ePostReq, ePostTriggerRef) <- newEventWithTriggerRef
  (eDeleteReq, eDeleteTriggerRef) <- newEventWithTriggerRef

  (Output eGetRes ePostRes eDeleteRes, FireCommand fire) <- hostPerformEventT $ guest (Input eGetReq ePostReq eDeleteReq)

  hGetRes <- subscribeEvent eGetRes
  hPostRes <- subscribeEvent ePostRes
  hDeleteRes <- subscribeEvent eDeleteRes

  let
    trigger :: Ref (SpiderHost Global) (Maybe (EventTrigger (SpiderTimeline Global) a))
            -> a
            -> (SpiderHost Global) ()
    trigger t v = do
      mTrigger <- liftIO . readIORef $ t
      case mTrigger of
        Just e -> fire [e :=> Identity v] readPhase >>= traverse_ handleOutput
        Nothing -> return ()

    readPhase = do
      mGet <- readEvent hGetRes >>= sequence
      mPost <- readEvent hPostRes >>= sequence
      mDelete <- readEvent hDeleteRes >>= sequence
      return (mGet, mPost, mDelete)

    handleOutput (mGet, mPost, mDelete) = do
      case mGet of
        Nothing -> return ()
        Just g -> liftIO . atomically $
          enqueueResponse (sGet source) g
      case mPost of
        Nothing -> return ()
        Just p -> liftIO . atomically $
          enqueueResponse (sPost source) p
      case mDelete of
        Nothing -> return ()
        Just d -> liftIO . atomically $
          enqueueResponse (sDelete source) d

  forever $ do
    -- this doesn't do fair reading from the sources
    -- could probably do something about that
    req <- liftIO . atomically $ readIn source

    case req of
      IGet g -> trigger eGetTriggerRef g
      IPost p -> trigger ePostTriggerRef p
      IDelete d -> trigger eDeleteTriggerRef d

    return ()

data ReqRes a b =
  ReqRes {
    rrReq :: TMVar a
  , rrRes :: TMVar b
  }

mkReqRes :: STM (ReqRes a b)
mkReqRes =
  ReqRes <$> newEmptyTMVar <*> newEmptyTMVar

enqueueRequest :: ReqRes a b -> a -> STM ()
enqueueRequest (ReqRes req _) a = do
  putTMVar req a

waitForResponse :: ReqRes a b -> STM b
waitForResponse (ReqRes _ res) = do
  takeTMVar res

-- for outside of the FRP network
reqRes :: ReqRes a b -> a -> IO b
reqRes rr a = do
  atomically $ enqueueRequest rr a
  atomically $ waitForResponse rr

-- for inside of the the FRP network

waitForRequest :: ReqRes a b -> STM a
waitForRequest (ReqRes req _) =
  readTMVar req

enqueueResponse :: ReqRes a b -> b -> STM ()
enqueueResponse (ReqRes req res) b = do
  putTMVar res b
  _ <- takeTMVar req
  return ()

data Source =
  Source {
    sGet :: ReqRes () (Snap [Payload])
  , sPost :: ReqRes String (Snap NoContent)
  , sDelete :: ReqRes Int (Snap NoContent)
  }

data In =
    IGet ()
  | IPost String
  | IDelete Int

readIn :: Source -> STM In
readIn (Source g p d) =
  (IGet <$> waitForRequest g) `orElse`
  (IPost <$> waitForRequest p) `orElse`
  (IDelete <$> waitForRequest d)

mkSource :: STM Source
mkSource =
  Source <$> mkReqRes <*> mkReqRes <*> mkReqRes

apiServer :: Source -> ServerT MyAPI (Handler () ())
apiServer source = handleGet :<|> handlePost :<|> handleDelete
  where
    handleGet = do
      liftIO . putStrLn $ "get"
      res <- liftIO $ reqRes (sGet source) ()
      liftSnap res

    handlePost (Payload s) = do
      liftIO . putStrLn $ "post " ++ s
      res <- liftIO $ reqRes (sPost source) s
      liftSnap res

    handleDelete i = do
      liftIO . putStrLn $ "delete " ++ show i
      res <- liftIO $ reqRes (sDelete source) i
      liftSnap res

app :: String -> Source -> SnapletInit () ()
app baseDir source = makeSnaplet "test" "test" Nothing $ do
  addRoutes 
    [ ("api", serveSnap api $ apiServer source)
    , ("", serveDirectory baseDir)
    ]
  return ()

main :: IO ()
main = do
  baseDir : _ <- getArgs
  source <- atomically $ mkSource
  forkIO $ host source guest
  serveSnaplet defaultConfig $ app baseDir source
