{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Client (
    runGet
  , runGet'
  , runPost
  , runPost'
  , runDelete
  , runDelete'
  ) where

import Data.Proxy

import Control.Monad.Except (runExceptT)
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)

import Servant.API
import Servant.Client

import API

api :: Proxy MyAPI
api = Proxy

getPayload :: Manager -> BaseUrl -> ClientM [Payload]
postPayload :: Payload -> Manager -> BaseUrl -> ClientM [Payload]
deletePayload :: String -> Manager -> BaseUrl -> ClientM NoContent

getPayload :<|> postPayload :<|> deletePayload = client api

defaultBaseUrl :: BaseUrl
defaultBaseUrl = BaseUrl Http "0.0.0.0" 8000 "/api"

runGet :: IO [String]
runGet = runGet' defaultBaseUrl

runGet' :: BaseUrl -> IO [String]
runGet' url = do
  manager <- newManager defaultManagerSettings
  res <- runExceptT $ getPayload manager url
  case res of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      return []
    Right x -> return $ fmap value x

runPost :: String -> IO [String]
runPost s = runPost' s defaultBaseUrl

runPost' :: String -> BaseUrl -> IO [String]
runPost' s url = do
  manager <- newManager defaultManagerSettings
  res <- runExceptT $ postPayload (Payload s) manager url
  case res of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      return []
    Right x -> return $ fmap value x

runDelete :: String -> IO ()
runDelete s = runDelete' s defaultBaseUrl

runDelete' :: String -> BaseUrl -> IO ()
runDelete' s url = do
  manager <- newManager defaultManagerSettings
  res <- runExceptT $ deletePayload s manager url
  case res of
    Left err -> do
      putStrLn $ "Error: " ++ show err
    Right _ -> return ()
