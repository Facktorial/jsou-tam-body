{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson as Aeson
import Data.Aeson.Types (object, (.=))

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy as TL

prettyJSONToText :: ToJSON a => a -> Text
prettyJSONToText = TL.toStrict . TL.decodeUtf8 . encodePretty

type RegNo = String
type Category = String
type Age = Int
type Name = String
type Ranking = Int
type Points = Int
type PointsBase = Double
type PointsCoef = Double
type Coef = Points

data Entry = Entry RegNo Category deriving (Show, Generic, ToJSON)

data RunnerInfo = RunnerInfo
  { runnerRegNo :: RegNo
  , runnerName :: Name
  , runnerRankings :: [RankingInfo]
  } deriving (Show, Generic)

data RankingInfo = RankingInfo
  { runnersRank :: Ranking
  , runnersPoints :: Points
  , runnersCoef :: Points 
  } deriving (Show, Generic)

type CategoryPoints = Map.Map Category [RunnerInfo]
type RaceEntries = [(Category, [RegNo])]

type ResultRacersMap = Map.Map Int (Map.Map Category [(Name, Ranking, Coef)])
type ResultCoefsMap  = Map.Map Int (Map.Map Category PointsBase)

newtype EvName = EvName Text deriving (Show, Generic)
instance ToJSON EvName
instance FromJSON EvName
newtype Discipline = Discipline Text deriving (Show, Generic)
instance ToJSON Discipline
instance FromJSON Discipline
newtype Place = Place Text deriving (Show, Generic)
instance ToJSON Place
instance FromJSON Place
newtype Koef = Koef Double deriving (Show, Generic)
instance ToJSON Koef
instance FromJSON Koef

data EventInfo = EventInfo
  { eventName :: EvName
  , eventDiscipline :: Discipline
  , eventPlace :: Place
  , eventKS :: Koef
  , eventKZ :: Koef
  } deriving (Show, Generic)

data RacerResult = RacerResult
  { racerName :: Name
  , racerRanking :: Ranking
  , racerCoef :: Coef
  } deriving (Show, Generic)

data EventAnalResult = EventAnalResult
  { racers :: Map.Map Int (Map.Map Category [RacerResult])
  , coefs  :: ResultCoefsMap
  } deriving (Show, Generic)

instance ToJSON RankingInfo
instance ToJSON RunnerInfo  
instance ToJSON RacerResult
instance ToJSON EventAnalResult
instance ToJSON EventInfo
