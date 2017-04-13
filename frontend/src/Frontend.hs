{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Proxy

import Reflex
import Reflex.Dom

import Servant.API
import Servant.Reflex

import API

runGUI :: forall t m. MonadWidget t m => m ()
runGUI = do
  let
    doGet :<|> goPost :<|> doDelete = 
      client
        (Proxy :: Proxy MyAPI)
        (Proxy :: Proxy m)
        (Proxy :: Proxy ())
        (constDyn (BasePath "api"))

  -- add an input and a button to do posts
  -- for each entry, add a button rigged up to delete it
  --
  -- start with a get, do a get after each post or delete to update the list

  return ()

main :: IO ()
main = mainWidget $ do
         el "div" $ text "Welcome to Reflex"
         runGUI
