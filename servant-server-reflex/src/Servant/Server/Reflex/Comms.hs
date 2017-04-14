{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Servant.Server.Reflex.Comms (
    Comms(..)
  ) where

data Comms =
    Serial
  | Parallel
  deriving (Eq, Ord, Show)
