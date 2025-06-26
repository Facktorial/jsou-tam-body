{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module ORISScrapper where
import CSVator
import Types

import Network.HTTP.Simple
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.KeyMap (fromList)
import Data.Aeson.Key (fromText)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Exception (try, SomeException)
import GHC.Generics
import Data.Maybe (fromMaybe, catMaybes, listToMaybe, fromJust, isJust)
import qualified Data.Map as Map
import Data.List (intercalate, isPrefixOf)
import qualified Debug.Trace
import Data.Either (rights)
import Text.Read (readMaybe)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Vector as V

import Data.List (sortBy)
import Data.Ord (comparing, Down(..))

pbCount :: Int
pbCount = 4

myRankingTypes :: [Int]
myRankingTypes = [0..((length rankingTypes) - 1)]

-- | Base URL for the ORIS API
baseUrl :: String
baseUrl = "https://oris.orientacnisporty.cz/API/"

makeApiRequest :: String -> [(String, String)] -> IO (Either String Value)
makeApiRequest url params = do
  baseRequest <- parseRequest url
  let request = setRequestQueryString queryParams baseRequest
      queryParams = map (\(k, v) -> (BS.pack k, Just $ BS.pack v)) params

  Debug.Trace.traceIO ("Request:\n" ++ show request)
  
  result <- try $ httpJSON request
  case result of
    Left e -> return $ Left $ "Request failed: " ++ show (e :: SomeException)
    Right response -> return $ Right $ getResponseBody response

makeJsonApiRequest :: String -> [(String, String)] -> IO (Either String Value)
makeJsonApiRequest method args = makeApiRequest baseUrl params
  where
    params = [("format", "json"), ("method", method)] ++ args

getCSOSClubList :: IO (Either String Value)
getCSOSClubList = makeApiRequest "getCSOSClubList" params
  where
    params = [("method", "getCSOSClubList")]

getRankingTypes :: IO (Either String Value)
getRankingTypes = makeJsonApiRequest "getRankingTypes" params
  where
    params = [("sport", "1")]

getEvent :: Int -> IO (Either String Value)
getEvent eventId = makeJsonApiRequest "getEvent" params
  where
    params = [("id", show eventId)]

getEventEntries :: Int -> Category -> IO (Either String Value)
getEventEntries eventId cls = makeJsonApiRequest "getEventEntries" params
   where
     baseParams = [("eventid", show eventId)]
     params = if null cls then baseParams else ("classname", cls) : baseParams

-- getEventResults :: Int -> IO (Either String Value)
-- getEventResults eventId = makeApiRequest baseUrl params
--   where
--     params = [("format", "json"), ("method", "getEventResults"), ("eventid", show eventId)]

getEventRankResults :: Int -> IO (Either String Value)
getEventRankResults eventId = makeApiRequest baseUrl params
  where
    params = [("format", "json"), ("method", "getEventRankResults"), ("eventid", show eventId)]

getUserEntries :: Int -> IO (Either String Value)
getUserEntries userId = makeApiRequest baseUrl params
  where
    params = [("format", "json"), ("method", "getUserEventEntries"), ("userid", show userId)]

-- getSomething :: String -> String -> Int -> IO (Either String Value)
-- getSomething method idType idValue = makeApiRequest baseUrl params
--   where
--     params = [("format", "json"), ("method", method), (idType, show idValue)]

extractText :: KM.Key -> KM.KeyMap Value -> Text
extractText key obj =
  case KM.lookup key obj of
    Just (String txt) -> txt
    _ -> ""

readDoubleFromCSV :: T.Text -> Maybe Double
readDoubleFromCSV = readMaybe . T.unpack . T.replace "," "."

extractEventInfo :: Int -> IO (Maybe EventInfo)
extractEventInfo eventId = do
  response <- getEvent eventId

  case response of
    Right (Object obj) -> 
      case KM.lookup "Data" obj of
        Just (Object dataObj) -> do
          let name = EvName $ extractText "Name" dataObj
              disc = Discipline $ case KM.lookup "Discipline" dataObj of
                Just (Object disObj) ->
                  extractText "NameEN" disObj
                _ -> ""
              plac = Place $ extractText "Place" dataObj
              ks = Koef $ maybe
                0.00
                id
                (readDoubleFromCSV $ extractText "RankingKS" dataObj)
              kz = Koef $ maybe
                1.00
                id
                (readDoubleFromCSV $ extractText "RankingKoef" dataObj)

          return $ Just (EventInfo name disc plac ks kz)
        _ -> return Nothing
    _ -> return Nothing

filterEntriesByCategories :: [Entry] -> [Category] -> [(Category, [RegNo])]
filterEntriesByCategories entries categories = map getRegNos categories 
    where 
        filtered cat = filter (\(Entry no cls) -> cat == cls) entries 
        regNos = map (\(Entry no _) -> no)
        getRegNos cat = (cat, regNos $ filtered cat)

makeEntries :: Value -> Either String [RegNo] 
makeEntries content = case content of
    Object obj -> do
      let classItems = KM.toList obj

      -- Debug.Trace.traceShow (classItems) (return ())

      case mapM extractEntries classItems of
        Success categories -> Right $ concat categories
        Error err -> Left $ "Parse error: " ++ err
    _ -> Left "Root is not an object"

extractEntries :: (KM.Key, Value) -> Result [RegNo]
extractEntries (key, value) = case value of
    Object obj -> do
      let classItems = KM.toList obj

      let results = mapM (extractThatEntry . snd) classItems
      case results of
        Success maybeRegNos -> Success $ catMaybes maybeRegNos
        Error err -> Success []
    _ -> Success []

extractThatEntry :: Value -> Result (Maybe RegNo)
extractThatEntry (Object obj) = do
      let classItems = KM.toList obj
      let results = getter obj

      case results of
        Success mNumber -> Success mNumber
        Error err -> Success $ Nothing
      
      where
        getter :: KM.KeyMap Value -> Result (Maybe RegNo)
        getter obj = 
            case KM.lookup "RegNo" obj of
              Just (String num) -> 
                Debug.Trace.traceShow (": " ++ show num) $
                Success $ Just $ T.unpack num
              Just _ -> Error "RegNo field exists but is not a string or number"
              Nothing -> Success Nothing
extractThatEntry _ = Success Nothing

getCategories :: Value -> Either String [Category]
getCategories jsonData = case jsonData of
    Object obj -> do
      dataObj <- case KM.lookup "Data" obj of
        Just v -> case v of
          Object d -> Right d
          _ -> Left "Data is not an object"
        Nothing -> Left "No Data field found"
      
      classesObj <- case KM.lookup "Classes" dataObj of
        Just v -> case v of
          Object c -> Right c
          _ -> Left "Classes is not an object"
        Nothing -> Left "No Classes field found"
      
      let classItems = KM.toList classesObj

      case mapM parseCategory classItems of
        Success categories -> Right $ catMaybes categories
        Error err -> Left $ "Parse error: " ++ err
    _ -> Left "Root is not an object"

parseCategory :: (KM.Key, Value) -> Result (Maybe Category)
parseCategory (key, value) = 
  --Debug.Trace.traceShow ("parseCategory key: " ++ show key) $
  case value of
    Object obj -> case KM.lookup "Name" obj of
      Just (String clsName) -> Success $ Just $ T.unpack clsName
      _ -> Success Nothing
    _ -> Success Nothing


-- FIXME: get through API last version
getLastRankingVersion :: String
getLastRankingVersion = "fix-this-feature"

loadActualRankings :: String -> IO (Either String RankingsData)
loadActualRankings actualDate
    | null actualDate = downloadRankings "last"
    | otherwise = downloadRankings actualDate

getActualRankings :: String -> IO (Either String RankingsData)
getActualRankings actualDate = do
  isCached <- checkIfCachedCSV actualDate
  if isCached
    then loadCsvsFromFilesByDate actualDate
    else loadActualRankings actualDate


processEntries :: RaceEntries -> [CSVData] -> CategoryPoints
processEntries ents rankings = Map.fromList
  [ (cat, [mkRunnerInfo rankings x | x <- runners])
  | (cat, runners) <- ents
  ]

mkRunnerInfo :: [CSVData] -> RegNo -> RunnerInfo
mkRunnerInfo tables regno = RunnerInfo
  { runnerRegNo = regno
  , runnerName = firstname ++ " " ++ lastname
  , runnerRankings = [extractRunnerInfo regno table | table <- tables]
  }
    where
      page = head tables
      runnerResults = V.filter (\m -> Map.lookup "Reg. c." m == Just regno) page
      firstname = case V.toList runnerResults of
        [] -> "unknown - FIXME"
        (runner:_) -> fromJust $ Map.lookup "Jmeno" $ V.head runnerResults
      lastname = case V.toList runnerResults of
        [] -> ""
        (runner:_) -> fromJust $ Map.lookup "Prijmeni" $ V.head runnerResults

extractRunnerInfo :: RegNo -> CSVData -> RankingInfo
extractRunnerInfo regno table = RankingInfo
  { runnersRank = rank
  , runnersPoints = points
  , runnersCoef = coef
  } where
      runnerResults = V.filter (\m -> Map.lookup "Reg. c." m == Just regno) table
      (rank, points, coef) = case V.toList runnerResults of
        [] -> (-1, 0, 0) 
        (results:_) ->
                ( read $ fromJust $ Map.lookup "Poradi" results
                , read $ fromJust $ Map.lookup "Body" results
                , read $ fromJust $ Map.lookup "Rank.c" results
                )
 
getNBestByType :: Int -> CategoryPoints -> CategoryPoints
getNBestByType amount runners = Map.map (take amount) runners

sortRunners :: Int -> CategoryPoints -> CategoryPoints
sortRunners rankType runners = Map.map (sortByNthRanking rankType) runners
    where
       sortByNthRanking :: Int -> [RunnerInfo] -> [RunnerInfo]
       sortByNthRanking rT rs = sortBy (comparing (Down . getPointsFromNthRanking rT)) rs

       getPointsFromNthRanking :: Int -> RunnerInfo -> Int
       getPointsFromNthRanking rT runner = runnersCoef $ pss !! rT
         where
           pss = runnerRankings runner

calcPB :: [(Name, Points)] -> Double 
calcPB pbs
  | length pbs < pbCount = 0
  | otherwise =
        Debug.Trace.traceShow (pbs) $
        (1.0 / fromIntegral pbCount) * fromIntegral totalCoef
      where
        totalCoef = sum $
          map
              (\(_, coef) -> coef)
             (take pbCount pbs)

getRacer :: Int -> RunnerInfo -> RacerResult
getRacer rT info = RacerResult name rank coef
  where
    name = runnerName info
    rank = runnersRank ((runnerRankings info) !! rT)
    coef = runnersCoef ((runnerRankings info) !! rT)

analyzeEvent :: Age -> Int -> String -> IO (Either Text EventAnalResult)
analyzeEvent age_in id gender = do    
    eventResult <- getEvent id
    categoriesResult <- handleEvents eventResult

    case categoriesResult of
      Left err -> return (Left $ T.pack err)
      Right categories -> do
          entriesRaw <- mapM (\cls -> getEventEntries id cls) categories

          --let _hole = entriesRaw :: _
          let entries =  mapM makeEntries (rights entriesRaw) 
          let entriesResult = zip categories (concat $ rights [entries])

          rankings <- getActualRankings getLastRankingVersion
          case rankings of
            Left err -> return (Left $ T.pack err)
            Right ranks -> do
              -- FIXME: 
              let raceGender = if gender == "H" then "M" else "F"
              let genderRankings = fromJust $ Map.lookup raceGender ranks -- [CSVData]
              
              let results = processEntries entriesResult genderRankings 

              let runnersByRanking rT = Map.fromListWith (flip (++))
                                [(categ, [(runnerName runner, runnersCoef $ (runnerRankings runner) !! rT)])
                                  | categ <- Map.keys partialResult
                                  , runner <- Map.findWithDefault [] categ partialResult
                                ]
                                where
                                  partialResult = sortRunners rT results

              let allRunnersAllRankings = Map.fromList
                    [(rT, runnersByRanking rT) | rT <- myRankingTypes] 

              let pbResults = Map.fromList 
                    [ (rT, Map.fromList 
                        [(cat, calcPB $ bests cat rT) 
                        | cat <- Map.keys results
                        ])
                    | rT <- myRankingTypes
                    ]
                      where
                        pbs     r = fromJust $ Map.lookup r allRunnersAllRankings
                        bests c r = fromJust $ Map.lookup c (pbs r)

              let pouredRacers = Map.fromList
                    [(rT, Map.fromList
                          [(cat, [getRacer rT x | x <- fromJust $ Map.lookup cat (sortRunners rT results)]) 
                          | cat <- Map.keys results
                          ])
                    | rT <- myRankingTypes
                    ]

              -- mapM_ 
              --   (\(_, val) -> mapM_ (mapM_ (\(n, r, c) -> print (show r ++ ". " ++ n ++ "   " ++ show c))) val)
              --   (Map.toList pouredRacers)
              mapM_ print (Map.toList pbResults)

              let result = EventAnalResult pouredRacers pbResults
              return (Right result)

  where 
    age = if age_in > 0 then show age_in else ""

    handleEvents (Left err) = return $ Left $ "Failed to get event: " ++ err
    handleEvents (Right event) = handleCategories (getCategories event)

    handleCategories (Left err) = return $ Left $ "Failed to parse categories: " ++ err
    handleCategories (Right categories) = do

        let filtered = filter 
                        (T.isPrefixOf (T.pack $ gender ++ age) . T.pack)
                        categories
        return $ Right filtered

    formatResult func (label, result) = case result of
      Left err -> T.pack $ label ++ " ERROR: " ++ err
      Right val -> T.pack $ label ++ " SUCCESS\n" ++ (func val)

printResuls analysis = undefined

runOrisWithOutput :: IO [Text]
runOrisWithOutput = do
  -- Run each test and collect results
  result1 <- return $ Right ""
  result2 <- getCSOSClubList
  result3 <- getEvent 8972
  
  let results = [ ("Testing Orienteering Scraper...", result1),
                  ("Club list test:", result2),
                  ("Event test:", result3)
                ]
  
  let formatResult (label, result) = case result of
        Left err -> T.pack $ label ++ " ERROR: " ++ err
        Right val -> T.pack $ label ++ " SUCCESS: " ++ show val
  
  return $ map formatResult results

-- Helper functions for working with the data

-- | Pretty print JSON response
-- prettyPrintJson :: Value -> IO ()
-- prettyPrintJson = LBS.putStrLn . encodePretty
-- 
-- -- | Extract specific field from JSON response
-- extractField :: Text -> Value -> Maybe Value
-- extractField fieldName (Object obj) = case fromJSON (Object obj) of
--   Success val -> Just val
--   Error _ -> Nothing
-- extractField _ _ = Nothing
-- 
-- -- | Convert CSV records to JSON for easier processing
-- csvToJson :: V.Vector (Map.Map String String) -> Value
-- csvToJson records = Array $ V.map (Object . fromList . map (\(k, v) -> (fromText $ T.pack k, String $ T.pack v)) . Map.toList) records
