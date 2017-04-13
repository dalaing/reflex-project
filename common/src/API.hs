{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
module API (
    Payload(..)
  , MyAPI
  ) where

import GHC.Generics

import Data.Aeson
import Servant.API

data Payload = Payload { value :: String }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON Payload
instance ToJSON Payload

type MyAPI =
       "payloads" :> Get '[JSON] [Payload]
  :<|> "payloads" :> ReqBody '[JSON] Payload :> PostNoContent '[JSON] NoContent
  :<|> "payloads" :> Capture "index" Int :> DeleteNoContent '[JSON] NoContent

