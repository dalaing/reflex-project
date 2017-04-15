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
{-# LANGUAGE DeriveGeneric #-}
module Servant.Server.Reflex (
    ServantHost(..)
  , AsServantHost(..)
  , Endpoint
  , ReqRes(..)
  , App
  , host
  ) where

import Control.Monad (forever)
import Data.Functor.Identity (Identity(..))
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, KnownNat)

import Control.Monad.STM (STM, atomically, orElse)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Ref (MonadRef(..))
import Control.Monad.Trans (MonadIO(..))
import Data.Foldable (traverse_)
import Data.IORef (readIORef)

import Servant.API
import Servant.API.ContentTypes (AllCTRender(..), AllCTUnrender(..))
import Snap.Core (Snap, MonadSnap(..))
import Snap.Snaplet (Handler)
import Servant.Server (ServerT)

import Reflex
import Reflex.Host.Class
import Data.Dependent.Sum (DSum(..))

import Servant.Server.Reflex.Comms

import qualified Servant.Server.Reflex.Comms.Serial as S
import qualified Servant.Server.Reflex.Comms.Parallel as P
import Servant.Server.Reflex.Util

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

class ReqRes (c :: Comms) where
  reqRes :: Proxy c -> Source c (Endpoint a b) -> a -> IO b

instance ReqRes 'Serial where
  reqRes _ = S.reqRes

instance ReqRes 'Parallel where
  reqRes _ = P.reqRes

class HasEndpoint api where
  type EndpointInput api :: [*]
  type EndpointOutput api :: [*]

  serve' :: Proxy api
         -> (Gather (EndpointInput api) -> Handler () () (Collapsed (EndpointOutput api)))
         -> ServerT api (Handler () ())

class AsServantHost api where
  type ToServantHost api

  serve :: ReqRes c
        => Proxy c
        -> Proxy api
        -> Source c (ToServantHost api)
        -> ServerT api (Handler () ())

instance (AsServantHost a, AsServantHost b)
    => AsServantHost (a :<|> b) where
  type ToServantHost (a :<|> b) =
    ToServantHost a :<|> ToServantHost b

  serve c _ (sa :<|> sb) =
    serve c (Proxy :: Proxy a) sa :<|>
    serve c (Proxy :: Proxy b) sb

instance (KnownSymbol capture, FromHttpApiData a, HasEndpoint api)
      => HasEndpoint (Capture capture a :> api) where
  type EndpointInput (Capture capture a :> api) =
    a ': EndpointInput api
  type EndpointOutput (Capture capture a :> api) =
    EndpointOutput api

  serve' _ f = \x ->
    serve' (Proxy :: Proxy api) (\xs -> f (GCons x xs))

instance (KnownSymbol capture, FromHttpApiData a, HasEndpoint api, CollapseList (EndpointInput (Capture capture a :> api)))
      => AsServantHost (Capture capture a :> api) where
  type ToServantHost (Capture capture a :> api) =
    Endpoint
      (Collapsed (EndpointInput (Capture capture a :> api)))
      (Snap (Collapsed (EndpointOutput (Capture capture a :> api))))

  serve c a s = serve' a $ \g -> do
    res <- liftIO $ reqRes c s (collapse g)
    liftSnap res

instance (KnownSymbol capture, FromHttpApiData a, HasEndpoint api)
      => HasEndpoint (CaptureAll capture a :> api) where
  type EndpointInput (CaptureAll capture a :> api) =
    [a] ': EndpointInput api
  type EndpointOutput (CaptureAll capture a :> api) =
    EndpointOutput api

  serve' _ f = \x ->
    serve' (Proxy :: Proxy api) (\xs -> f (GCons x xs))

instance (KnownSymbol capture, FromHttpApiData a, HasEndpoint api, CollapseList (EndpointInput (CaptureAll capture a :> api)))
      => AsServantHost (CaptureAll capture a :> api) where
  type ToServantHost (CaptureAll capture a :> api) =
    Endpoint
      (Collapsed (EndpointInput (CaptureAll capture a :> api)))
      (Snap (Collapsed (EndpointOutput (CaptureAll capture a :> api))))

  serve c a s = serve' a $ \g -> do
    res <- liftIO $ reqRes c s (collapse g)
    liftSnap res

instance (KnownSymbol sym, FromHttpApiData a, HasEndpoint api)
      => HasEndpoint (Header sym a :> api) where
  type EndpointInput (Header sym a :> api) =
    Maybe a ': EndpointInput api
  type EndpointOutput (Header sym a :> api) =
    EndpointOutput api

  serve' _ f = \x ->
    serve' (Proxy :: Proxy api) (\xs -> f (GCons x xs))

instance (KnownSymbol sym, FromHttpApiData a, HasEndpoint api, CollapseList (EndpointInput (Header sym a :> api)))
      => AsServantHost (Header sym a :> api) where
  type ToServantHost (Header sym a :> api) =
    Endpoint
      (Collapsed (EndpointInput (Header sym a :> api)))
      (Snap (Collapsed (EndpointOutput (Header sym a :> api))))

  serve c a s = serve' a $ \g -> do
    res <- liftIO $ reqRes c s (collapse g)
    liftSnap res

instance (KnownSymbol sym, FromHttpApiData a, HasEndpoint api)
      => HasEndpoint (QueryParam sym a :> api) where
  type EndpointInput (QueryParam sym a :> api) =
    Maybe a ': EndpointInput api
  type EndpointOutput (QueryParam sym a :> api) =
    EndpointOutput api

  serve' _ f = \x ->
    serve' (Proxy :: Proxy api) (\xs -> f (GCons x xs))

instance (KnownSymbol sym, FromHttpApiData a, HasEndpoint api, CollapseList (EndpointInput (QueryParam sym a :> api)))
      => AsServantHost (QueryParam sym a :> api) where
  type ToServantHost (QueryParam sym a :> api) =
    Endpoint
      (Collapsed (EndpointInput (QueryParam sym a :> api)))
      (Snap (Collapsed (EndpointOutput (QueryParam sym a :> api))))

  serve c a s = serve' a $ \g -> do
    res <- liftIO $ reqRes c s (collapse g)
    liftSnap res

instance (KnownSymbol sym, FromHttpApiData a, HasEndpoint api)
      => HasEndpoint (QueryParams sym a :> api) where
  type EndpointInput (QueryParams sym a :> api) =
    [a] ': EndpointInput api
  type EndpointOutput (QueryParams sym a :> api) =
    EndpointOutput api

  serve' _ f = \x ->
    serve' (Proxy :: Proxy api) (\xs -> f (GCons x xs))

instance (KnownSymbol sym, FromHttpApiData a, HasEndpoint api, CollapseList (EndpointInput (QueryParams sym a :> api)))
      => AsServantHost (QueryParams sym a :> api) where
  type ToServantHost (QueryParams sym a :> api) =
    Endpoint
      (Collapsed (EndpointInput (QueryParams sym a :> api)))
      (Snap (Collapsed (EndpointOutput (QueryParams sym a :> api))))

  serve c a s = serve' a $ \g -> do
    res <- liftIO $ reqRes c s (collapse g)
    liftSnap res

instance (KnownSymbol sym, HasEndpoint api)
      => HasEndpoint (QueryFlag sym :> api) where
  type EndpointInput (QueryFlag sym :> api) =
    Bool ': EndpointInput api
  type EndpointOutput (QueryFlag sym :> api) =
    EndpointOutput api

  serve' _ f = \x ->
    serve' (Proxy :: Proxy api) (\xs -> f (GCons x xs))

instance (KnownSymbol sym, HasEndpoint api, CollapseList (EndpointInput (QueryFlag sym :> api)))
      => AsServantHost (QueryFlag sym :> api) where
  type ToServantHost (QueryFlag sym :> api) =
    Endpoint
      (Collapsed (EndpointInput (QueryFlag sym :> api)))
      (Snap (Collapsed (EndpointOutput (QueryFlag sym :> api))))

  serve c a s = serve' a $ \g -> do
    res <- liftIO $ reqRes c s (collapse g)
    liftSnap res

instance (AllCTUnrender list a, HasEndpoint api)
      => HasEndpoint (ReqBody list a :> api) where
  type EndpointInput (ReqBody list a :> api) =
    a ': EndpointInput api
  type EndpointOutput (ReqBody list a :> api) =
    EndpointOutput api

  serve' _ f = \x ->
    serve' (Proxy :: Proxy api) (\xs -> f (GCons x xs))

instance (AllCTUnrender list a, HasEndpoint api, CollapseList (EndpointInput (ReqBody list a :> api)))
      => AsServantHost (ReqBody list a :> api) where
  type ToServantHost (ReqBody list a :> api) =
    Endpoint
      (Collapsed (EndpointInput (ReqBody list a :> api)))
      (Snap (Collapsed (EndpointOutput (ReqBody list a :> api))))

  serve c a s = serve' a $ \g -> do
    res <- liftIO $ reqRes c s (collapse g)
    liftSnap res

instance (KnownSymbol path, HasEndpoint api)
      => HasEndpoint (path :> api) where
  type EndpointInput (path :> api) =
    EndpointInput api
  type EndpointOutput (path :> api) =
    EndpointOutput api

  serve' _ f =
    serve' (Proxy :: Proxy api) f

instance (KnownSymbol path, HasEndpoint api, CollapseList (EndpointInput (path :> api)))
       => AsServantHost (path :> api) where
  type ToServantHost (path :> api) =
    Endpoint
      (Collapsed (EndpointInput (path :> api)))
      (Snap (Collapsed (EndpointOutput (path :> api))))

  serve c a s = serve' a $ \g -> do
    res <- liftIO $ reqRes c s (collapse g)
    liftSnap res

instance (AllCTRender ctypes a, ReflectMethod method, KnownNat status)
     => HasEndpoint (Verb (method :: k1) status ctypes a) where
  type EndpointInput (Verb method status ctypes a) =
    '[]
  type EndpointOutput (Verb method status ctypes a) =
    '[a]

  serve' _ f =
    f GNil

instance (AllCTRender ctypes a, ReflectMethod method, KnownNat status)
     => AsServantHost (Verb (method :: k1) status ctypes a) where
  type ToServantHost (Verb method status ctypes a) =
    Endpoint
      (Collapsed (EndpointInput (Verb method status ctypes a)))
      (Snap (Collapsed (EndpointOutput (Verb method status ctypes a))))

  serve c a s = serve' a $ \g -> do
    res <- liftIO $ reqRes c s (collapse g)
    liftSnap res

{-
-- TODO unwind the headers hlist into the output list
instance {-# OVERLAPPING #-} (AllCTRender ctypes a, ReflectMethod method, KnownNat status, GetHeaders (Headers h a))
      => HasEndpoint (Verb method status ctypes (Headers h a)) where
  type EndpointInput (Verb method status ctypes (Headers h a)) =
    '[]
  type EndpointOutput (Verb method status ctypes (Headers h a)) =
    '[Headers h a]
-}

