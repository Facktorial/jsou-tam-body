{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import ORISScrapper
import CSVator
import Types (prettyJSONToText, EventAnalResult, EventInfo)

import Data.Maybe (isNothing)
import Data.Aeson as Aeson
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy as TL
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import System.IO (hFlush, stdout)
import Data.Time.Clock (getCurrentTime)
import Control.Exception (try, SomeException)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as L8


data ApiResponse = ApiResponse
  { message :: Text
  , eventResult :: Maybe EventAnalResult
  , err :: Maybe Text
  , rankingCategories :: Value
  , eventInfo :: Maybe EventInfo
  } deriving (Generic, Show)

instance ToJSON ApiResponse where
  toJSON (ApiResponse msg result err rts info) = object
    [ "success" .= isNothing err
    , "message" .= msg
    , "eventResult" .= result
    , "error" .= err
    , "rankingtypes" .= rts
    , "eventInfo" .= info
    ]

-- API definition
type API = "api" :> "hello" :> Get '[JSON] ApiResponse
      :<|> "api" :> Capture "id" Int :> Capture "gender" String :> Capture "forceage" Bool :> Get '[JSON] ApiResponse

logRequest :: String -> Handler ()
logRequest msg = liftIO $ do
  time <- getCurrentTime
  putStrLn $ "[" ++ show time ++ "] " ++ msg
  hFlush stdout

captureRunOris :: IO[Text]
captureRunOris = do
  result <- try runOrisWithOutput
  case result of
    Left e -> return [T.pack $ "Error running ORIS: " ++ show (e:: SomeException)]
    Right output -> return output

captureAnalysis :: Int -> String -> Bool -> IO (Either Text EventAnalResult)
captureAnalysis id category forceAge = do
  let age = if forceAge then 21 else 0
  result <- try $ analyzeEvent age id category
  case result of
    Left e -> return (Left $ T.pack $ "Error running ORIS: " ++ show (e:: SomeException))
    Right output -> return output

helloHandler :: Handler ApiResponse
helloHandler = do
  logRequest "GET /api/hello"
  orisResult <- liftIO captureRunOris
  return $ ApiResponse
    "\\_ -> Hello from Haskell!"
    Nothing
    (Just $ T.unlines orisResult)
    Null
    Nothing

analyzeHandler :: Int -> String -> Bool -> Handler ApiResponse
analyzeHandler eventId racerGender forceAge = do
  logRequest $ "GET /api/" ++ show eventId ++ "/" ++ racerGender ++ "/" ++ show forceAge
  analysisResult <- liftIO $ captureAnalysis eventId racerGender forceAge
  rankingTypes <- liftIO $ getRankingTypes
  eventInfo <- liftIO $ extractEventInfo eventId

  case analysisResult of
    Left err -> return $ ApiResponse
      ("Analysis failed for event " <> T.pack (show eventId ++ "," ++ racerGender))
      Nothing
      (Just $ err)
      Null
      Nothing
    Right result -> return $ ApiResponse
      ("Analysis for event " <> T.pack (show eventId ++ "," ++ racerGender))
      (Just result)
      Nothing
      (case rankingTypes of
        Left err -> String (T.pack err)
        Right value -> value)
      eventInfo


server :: Server API
server = helloHandler :<|> analyzeHandler

-- CORS setup
corsPolicy = simpleCorsResourcePolicy
  { corsRequestHeaders = ["Content-Type"]
  , corsMethods = ["GET", "POST", "OPTIONS"]
  , corsOrigins = Nothing -- Allow all origins
  }

app :: Application
app = cors (const $ Just corsPolicy) $ serve (Proxy :: Proxy API) server

main :: IO ()
main = do
  port <- maybe 8000 read <$> lookupEnv "PORT"
  --putStrLn $ "🚀 Haskell API running on port " ++ show port
  putStrLn $ "Haskell API running on port " ++ show port
  putStrLn "Endpoint:"
  putStrLn "  /api/hello"
  putStrLn "  /api/{id}/{gender}"
  putStrLn "  /api/{id}/{gender}/{forceAge}"
  run port app
