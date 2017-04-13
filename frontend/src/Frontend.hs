{-# LANGUAGE OverloadedStrings #-}
module Main where

import Reflex
import Reflex.Dom

import Servant.Reflex

import API

main :: IO ()
main = mainWidget $ el "div" $ text "Welcome to Reflex"
