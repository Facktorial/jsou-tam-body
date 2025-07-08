{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import ORISScrapper
import CSVator
import Types (prettyJSONToText, EventAnalResult, EventInfo, Event(..), Standings(..), RegNo)

import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Data.Ord (comparing)
import qualified Data.List as L
import Data.Aeson as Aeson
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy as TL
import GHC.Generics
import Network.HTTP.Simple (HttpException)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import System.IO (hFlush, stdout)
import Data.Time.Clock (getCurrentTime)
import Control.Exception (try, SomeException, IOException)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as L8


--takeResultsCount :: Int
--takeResultsCount = 12

data ApiResponse a = ApiResponse
  { message :: Text
  , err :: Maybe Text
  --, eventResult :: Maybe EventAnalResult
  --, rankingCategories :: Value
  --, eventInfo :: Maybe EventInfo
  , payload :: a
  } deriving (Generic, Show)

type AnalyzeResponse = ApiResponse (Maybe EventAnalResult, Value, Maybe EventInfo)
type EgoResponse = ApiResponse [Event]
type StandingsResponse = ApiResponse (Standings, Value)

instance ToJSON AnalyzeResponse where
  toJSON (ApiResponse msg err (result, rts, info)) = object
    [ "success" .= isNothing err
    , "message" .= msg
    , "eventResult" .= result
    , "error" .= err
    , "rankingtypes" .= rts
    , "eventInfo" .= info
    ]

instance ToJSON (ApiResponse [Event]) where
  toJSON (ApiResponse msg err events) = object
    [ "success" .= isNothing err
    , "message" .= msg
    , "error" .= err
    , "events" .= events
    ]

instance ToJSON StandingsResponse where
  toJSON (ApiResponse msg err (standings, rts)) = object
    [ "success" .= isNothing err
    , "message" .= msg
    , "error" .= err
    , "standings" .= standings
    , "rankingtypes" .= rts
    ]

-- API definition
type API = "api" :> Capture "id" Int :> Capture "gender" String :> Capture "forceage" Bool :> Get '[JSON] AnalyzeResponse
      :<|> "api" :> "backgroundcheck" :> Capture "regno:" RegNo :> Get '[JSON] EgoResponse
      :<|> "api" :> "backgroundcheck" :> "standings" :> Capture "regno:" RegNo :> Get '[JSON] StandingsResponse

logRequest :: String -> Handler ()
logRequest msg = liftIO $ do
  time <- getCurrentTime
  putStrLn $ "[" ++ show time ++ "] " ++ msg
  hFlush stdout

capturePersonal :: RegNo -> IO (Either Text [Event])
capturePersonal regno = do
  result <- try $ analyzeRunner regno
  case result of
    Left (e :: HttpException) -> return $ Left $ "Http Error running background check: " <> T.pack (show e)
    Right (Left err) -> return $ Left $ "Some Error running background check: " <> err
    Right (Right output) -> return $ Right output

captureStandings :: RegNo -> IO (Either Text Standings)
captureStandings regno = do
  standings <- fetchStandings regno
  case standings of 
    Left e -> return $ Left $ "Some Error while fetching standings: " <> T.pack (show e)
    Right output -> return $ Right output

captureAnalysis :: Int -> String -> Bool -> IO (Either Text EventAnalResult)
captureAnalysis id category forceAge = do
  let age = if forceAge then 21 else 0
  result <- try $ analyzeEvent age id category
  case result of
    --Left (e :: HttpException) -> return $ Left $ "Http Error while analyze event: " <> T.pack (show e)
    --Left (e :: IOException) -> return $ Left $ "IO Error while analyze event: " <> T.pack (show e)
    Left (e :: SomeException) -> return $ Left $ "Some Error while analyze event: " <> T.pack (show e)
    Right output -> return output

runnerHandler :: RegNo -> Handler EgoResponse
runnerHandler regno = do
  logRequest $ "GET /api/backgroundcheck/" ++ regno
  backgroundCheckResult <- liftIO $ capturePersonal regno

  case backgroundCheckResult of
    Left err -> return $ ApiResponse
      ("Background-check of is loser failed: " <> T.pack (show regno))
      (Just err)
      []
    Right result -> return $ ApiResponse
      ("Background-check for this loser: " <> T.pack (show regno))
      Nothing
      --(L.take takeResultsCount $
      (L.sortBy (flip (comparing points)) result)

standingsHandler :: RegNo -> Handler StandingsResponse
standingsHandler regno = do
  logRequest $ "GET /api/backgroundcheck/standings/" ++ regno
  backgroundCheckResult <- liftIO $ captureStandings regno
  rankingTypes <- liftIO $ getRankingTypes

  case backgroundCheckResult of
    Left err -> return $ ApiResponse
      ("Background-check of is loser failed: " <> (T.pack $ show regno))
      (Just err)
      ( Standings $ Map.fromList []
      , Null
      )
    Right standings -> return $ ApiResponse
      ("Background-check for this loser: " <> (T.pack $ show regno))
      Nothing
      ( standings
      , (case rankingTypes of
         Left err -> String (T.pack err)
         Right value -> value)
      )

analyzeHandler :: Int -> String -> Bool -> Handler AnalyzeResponse
analyzeHandler eventId racerGender forceAge = do
  logRequest $ "GET /api/" ++ show eventId ++ "/" ++ racerGender ++ "/" ++ show forceAge
  analysisResult <- liftIO $ captureAnalysis eventId racerGender forceAge
  rankingTypes <- liftIO $ getRankingTypes
  eventInfo <- liftIO $ extractEventInfo eventId ""

  case analysisResult of
    Left err -> return $ ApiResponse
      ("Analysis failed for eventid: " <> T.pack (show eventId) <> ".")
      (Just $ err)
      ( Nothing
      , Null
      , Nothing
      )
    Right result -> return $ ApiResponse
      ("Analysis for event " <> T.pack (show eventId ++ "," ++ racerGender))
      Nothing
      ( (Just result)
      , (case rankingTypes of
         Left err -> String (T.pack err)
         Right value -> value)
      , eventInfo
      )


server :: Server API
server = analyzeHandler :<|> runnerHandler :<|> standingsHandler

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
  putStrLn "================================="
  putStrLn ""
  port <- maybe 8000 read <$> lookupEnv "PORT"
  --putStrLn $ "🚀 Haskell API running on port " ++ show port
  putStrLn $ "Haskell API running on port " ++ show port
  putStrLn "Endpoints:"
  putStrLn "  /api/{id}/{gender}"
  putStrLn "  /api/{id}/{gender}/{forceAge}"
  putStrLn "  /api/backgroundcheck/standings/{regno}/"
  putStrLn "  /api/backgroundcheck/{regno}/"
  run port app
