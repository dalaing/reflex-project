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
    ieGet    :: Event t (Ticket, ())
  , iePost   :: Event t (Ticket, String)
  , ieDelete :: Event t (Ticket, String)
  }

data Output t =
  Output {
    oeGet    :: Event t (Ticket, Snap [Payload])
  , oePost   :: Event t (Ticket, Snap [Payload])
  , oeDelete :: Event t (Ticket, Snap NoContent)
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

  bList <- accum (flip ($)) [] . leftmost $ [
      ((:) . snd) <$> ePost
    , (\x -> filter (/= snd x)) <$> eDelete
    ]

  performEvent_ $ (liftIO . putStrLn $ "FRP: get") <$ eGet
  performEvent_ $ (liftIO . putStrLn $ "FRP: post") <$ ePost
  performEvent_ $ (liftIO . putStrLn $ "FRP: delete") <$ eDelete

  let
    fnGet = return . fmap Payload
    eGetOut = (\l (t, _) -> (t, fnGet l)) <$> bList <@> eGet

    fnPost = return . fmap Payload
    ePostOut = (\xs (t, x) -> (t, fnPost $ x : xs)) <$> bList <@> ePost

    fnDel = return NoContent
    eDeleteOut = (\(t, _) -> (t, fnDel)) <$> eDelete

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
        Just (t, g) -> liftIO . atomically $
          enqueueResponse (sGet source) t g
      case mPost of
        Nothing -> return ()
        Just (t, p) -> liftIO . atomically $
          enqueueResponse (sPost source) t p
      case mDelete of
        Nothing -> return ()
        Just (t, d) -> liftIO . atomically $
          enqueueResponse (sDelete source) t d

  forever $ do
    -- this doesn't do fair reading from the sources
    -- could probably do something about that
    req <- liftIO . atomically $ readIn source

    case req of
      IGet g -> trigger eGetTriggerRef g
      IPost p -> trigger ePostTriggerRef p
      IDelete d -> trigger eDeleteTriggerRef d

    return ()

newtype Ticket = Ticket Int
  deriving (Eq, Ord, Show, Hashable)

data ReqRes a b =
  ReqRes {
    rrTicket :: TVar Ticket
  , rrReq :: TBQueue (Ticket, a)
  , rrRes :: SM.Map Ticket b
  }

mkReqRes :: Int -> STM (ReqRes a b)
mkReqRes size =
  ReqRes <$> newTVar (Ticket 0) <*> newTBQueue size <*> SM.empty

getTicket :: TVar Ticket -> STM Ticket
getTicket tv = do
  Ticket t <- readTVar tv
  writeTVar tv $ Ticket (succ t)
  return $ Ticket t

enqueueRequest :: ReqRes a b -> a -> STM Ticket
enqueueRequest (ReqRes tv req _) a = do
  t <- getTicket tv
  writeTBQueue req (t, a)
  return t

waitForResponse :: ReqRes a b -> Ticket -> STM b
waitForResponse (ReqRes _ _ res) t = do
  v <- SM.lookup t res
  case v of
    Just x ->
      return x
    Nothing -> do
      retry

-- for outside of the FRP network
reqRes :: ReqRes a b -> a -> IO b
reqRes rr a = do
  t <- atomically $ enqueueRequest rr a
  atomically $ waitForResponse rr t

-- for inside of the FRP network
peekRequest :: ReqRes a b -> STM (Maybe (Ticket, a))
peekRequest (ReqRes _ req _) =
  tryReadTBQueue req

enqueueResponse :: ReqRes a b -> Ticket -> b -> STM ()
enqueueResponse (ReqRes _ _ res) t b =
  SM.insert t b res

data Source =
  Source {
    sGet :: ReqRes () (Snap [Payload])
  , sPost :: ReqRes String (Snap [Payload])
  , sDelete :: ReqRes String (Snap NoContent)
  }

data In =
    IGet (Ticket, ())
  | IPost (Ticket, String)
  | IDelete (Ticket, String)

readIn :: Source -> STM In
readIn (Source g p d) =
  (IGet <$> readTBQueue (rrReq g)) `orElse`
  (IPost <$> readTBQueue (rrReq p)) `orElse`
  (IDelete <$> readTBQueue (rrReq d))

mkSource :: Int -> STM Source
mkSource size =
  Source <$> mkReqRes size <*> mkReqRes size <*> mkReqRes size

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

    handleDelete s = do
      liftIO . putStrLn $ "delete " ++ s
      res <- liftIO $ reqRes (sDelete source) s
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
  source <- atomically $ mkSource 10
  forkIO $ host source guest
  serveSnaplet defaultConfig $ app baseDir source
