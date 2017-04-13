{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

import Data.Proxy
import Text.Read (readMaybe)

import Control.Error

import Reflex
import Reflex.Dom

import Servant.API
import Servant.Reflex

import qualified Data.Map as M
import qualified Data.Text as T

import API

postWidget :: forall t m. MonadWidget t m 
           => m (Event t ())
postWidget = do
  let
    _ :<|> doPost :<|> _ = 
      client
        (Proxy :: Proxy MyAPI)
        (Proxy :: Proxy m)
        (Proxy :: Proxy ())
        (constDyn (BasePath "api"))
  el "div" $ do
    tPost <- textInput def
    eDoPost <- button "Post"

    eDonePost <-
      doPost (fmap (Right . Payload . T.unpack) . _textInput_value $ tPost) $
      eDoPost

    return $ () <$ fmapMaybe reqSuccess eDonePost

itemWidget :: forall t m. MonadWidget t m 
           => Int 
           -> Dynamic t String 
           -> m (Event t ())
itemWidget k dValue = do
  let
    _ :<|> _ :<|> doDelete = 
      client
        (Proxy :: Proxy MyAPI)
        (Proxy :: Proxy m)
        (Proxy :: Proxy ())
        (constDyn (BasePath "api"))

  eDoDelete <- el "li" $ el "div" $ do
    dynText (fmap T.pack dValue)
    button "Delete"

  eDoneDelete <-
    doDelete (constDyn . Right $ k) eDoDelete

  return $ () <$ fmapMaybe reqSuccess eDoneDelete

listItemWidget :: forall t m . MonadWidget t m 
               => Event t () 
               -> m (Event t ())
listItemWidget eGet = do
  let
    doGet :<|> _ :<|> _ = 
      client
        (Proxy :: Proxy MyAPI)
        (Proxy :: Proxy m)
        (Proxy :: Proxy ())
        (constDyn (BasePath "api"))
  getResp <- doGet eGet

  dGet <- 
    holdDyn [] . 
    fmapMaybe reqSuccess $ 
    getResp

  let dGetMap = fmap (M.fromList . zip [0..] . fmap API.value) dGet

  dMap <- el "ul" $ listWithKey dGetMap itemWidget

  let squash m = if M.null m then Nothing else Just ()

  return .
    switch .
    current . 
    fmap (fmapMaybe squash. mergeMap) $
    dMap

runGUI :: forall t m. MonadWidget t m => m ()
runGUI = do
  ePostBuild <- getPostBuild

  ePostDone <- postWidget

  el "br" $ return ()

  el "div" $ do
    eDoGet <- button "Get"

    rec
      eDeletes <- listItemWidget $ leftmost [ePostBuild, eDoGet, ePostDone, eDeletes]
    return ()

  return ()

main :: IO ()
main = mainWidget runGUI
