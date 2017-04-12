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
  :<|> "payloads" :> ReqBody '[JSON] Payload :> Post '[JSON] [Payload]
  :<|> "payloads" :> Capture "payload" String :> DeleteNoContent '[JSON] NoContent

-- we post a string
-- - the server will wait one minute and then return the list of strings as they are at that point in time
-- we delete a string
-- we get, it gives us the current list of strings

-- this implies we have Behavior (Set String) being passed to each of event handlers

-- we possibly pass around an stm map which the handlers can wait on
