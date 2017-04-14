{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Servant.Server.Reflex (
    ServantHost(..)
  , App
  , host
  ) where

import Control.Monad (forever)
import Data.Functor.Identity (Identity(..))
import Data.Proxy (Proxy(..))

import Control.Monad.STM (STM, atomically, orElse)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Ref (MonadRef(..))
import Control.Monad.Trans (MonadIO(..))
import Data.Foldable (traverse_)
import Data.IORef (readIORef)

import Servant.API

import Reflex
import Reflex.Host.Class
import Data.Dependent.Sum (DSum(..))

import Servant.Server.Reflex.Comms
import qualified Servant.Server.Reflex.Comms.Serial as S
import qualified Servant.Server.Reflex.Comms.Parallel as P

data Endpoint i o

class ServantHost (c :: Comms) x where
  type Input t c x
  type Output t c x
  type TriggerType t (m :: * -> *) c x
  type HandleType t c x
  type ReadType c x
  type Source c x

  mkSource :: Proxy c
           -> Proxy x
           -> STM (Source c x)
  newTrigger :: (MonadReflexCreateTrigger t m, MonadRef m, Ref m ~ Ref IO)
             => Proxy c
             -> Proxy x
             -> m (Input t c x, TriggerType t m c x)
  doTrigger :: MonadReflexHost t m
            => Proxy c
            -> Proxy x
            -> (forall a. Ref m (Maybe (EventTrigger t a)) -> a -> m ())
            -> TriggerType t m c x
            -> Source c x
            -> STM (m ())
  subscribe :: MonadSubscribeEvent t m
            => Proxy c
            -> Proxy x
            -> Output t c x
            -> m (HandleType t c x)
  readHandle :: MonadReadEvent t m
             => Proxy c
             -> Proxy x
             -> HandleType t c x
             -> m (ReadType c x)
  handleOut :: MonadIO m
            => Proxy c
            -> Proxy x
            -> Source c x
            -> ReadType c x
            -> m ()

instance ServantHost 'Serial (Endpoint i o) where
  type Input t 'Serial (Endpoint i o) = Event t i
  type Output t 'Serial (Endpoint i o) = Event t o
  type TriggerType t m 'Serial (Endpoint i o) = Ref m (Maybe (EventTrigger t i))
  type HandleType t 'Serial (Endpoint i o) = EventHandle t o
  type ReadType 'Serial (Endpoint i o) = Maybe o
  type Source 'Serial (Endpoint i o) = S.ReqRes i o

  mkSource _ _ = S.mkReqRes
  newTrigger _ _ = newEventWithTriggerRef
  doTrigger _ _ trigger t s =
    fmap (trigger t) (S.waitForRequest s)
  subscribe _ _ = subscribeEvent
  readHandle _ _ h = readEvent h >>= sequence
  handleOut _ _ s m =
    case m of
      Nothing -> return ()
      Just x -> liftIO . atomically $
        S.enqueueResponse s x

instance ServantHost 'Parallel (Endpoint i o) where
  type Input t 'Parallel (Endpoint i o) = Event t (P.Ticket, i)
  type Output t 'Parallel (Endpoint i o) = Event t (P.Ticket, o)
  type TriggerType t m 'Parallel (Endpoint i o) = Ref m (Maybe (EventTrigger t (P.Ticket, i)))
  type HandleType t 'Parallel (Endpoint i o) = EventHandle t (P.Ticket, o)
  type ReadType 'Parallel (Endpoint i o) = Maybe (P.Ticket, o)
  type Source 'Parallel (Endpoint i o) = P.ReqRes i o

  mkSource _ _ = P.mkReqRes 100
  newTrigger _ _ = newEventWithTriggerRef
  doTrigger _ _ trigger t s =
    fmap (trigger t) (P.waitForRequest s)
  subscribe _ _ = subscribeEvent
  readHandle _ _ h = readEvent h >>= sequence
  handleOut _ _ s m =
    case m of
      Nothing -> return ()
      Just (t, x) -> liftIO . atomically $
        P.enqueueResponse s t x

instance (ServantHost c a, ServantHost c b) => ServantHost c (a :<|> b) where
  type Input t c (a :<|> b) = Input t c a :<|> Input t c b
  type Output t c (a :<|> b) = Output t c a :<|> Output t c b
  type TriggerType t m c (a :<|> b) = TriggerType t m c a :<|> TriggerType t m c b
  type HandleType t c (a :<|> b) = HandleType t c a :<|> HandleType t c b
  type ReadType c (a :<|> b) = ReadType c a :<|> ReadType c b
  type Source c (a :<|> b) = Source c a :<|> Source c b

  mkSource c _ =
    (:<|>) <$> mkSource c (Proxy :: Proxy a)
           <*> mkSource c (Proxy :: Proxy b)

  newTrigger c _ = do
    (xi, xt) <- newTrigger c (Proxy :: Proxy a)
    (yi, yt) <- newTrigger c (Proxy :: Proxy b)
    return (xi :<|> yi, xt :<|> yt)

  doTrigger c _ trigger (tx :<|> ty) (sx :<|> sy) =
    doTrigger c (Proxy :: Proxy a) trigger tx sx `orElse`
    doTrigger c (Proxy :: Proxy b) trigger ty sy

  subscribe c _ (x :<|> y) = do
    hx <- subscribe c (Proxy :: Proxy a) x
    hy <- subscribe c (Proxy :: Proxy b) y
    return $ hx :<|> hy

  readHandle c _ (hx :<|> hy) = do
    mx <- readHandle c (Proxy :: Proxy a) hx
    my <- readHandle c (Proxy :: Proxy b) hy
    return $ mx :<|> my

  handleOut c _ (sx :<|> sy) (mx :<|> my) = do
    handleOut c (Proxy :: Proxy a) sx mx
    handleOut c (Proxy :: Proxy b) sy my

type App t m c sh = ( ReflexHost t
                    , PrimMonad (HostFrame t)
                    , MonadIO (HostFrame t)
                    , MonadHold t m
                    , MonadFix m
                    , Ref m ~ Ref IO
                    )
                  => Input t c sh
                  -> PerformEventT t m (Output t c sh)

host :: ServantHost c x
     => Proxy c
     -> Proxy x
     -> Source c x
     -> (forall t m. App t m c x)
     -> IO ()
host c x s guest = runSpiderHost $ do
  (inputs, triggers) <- newTrigger c x
  (outputs, FireCommand fire) <- hostPerformEventT $ guest inputs
  handles <- subscribe c x outputs

  let
    trigger :: Ref (SpiderHost Global) (Maybe (EventTrigger (SpiderTimeline Global) a)) -> a -> (SpiderHost Global) ()
    trigger tr v = do
      mTrigger <- liftIO . readIORef $ tr
      case mTrigger of
        Just e -> fire [e :=> Identity v] (readHandle c x handles) >>= traverse_ (handleOut c x s)
        Nothing -> return ()

  _ <- forever $ do
    act <- liftIO . atomically $ doTrigger c x trigger triggers s
    act

  return ()
