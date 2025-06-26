{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module CSVator where


import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Network.HTTP.Simple
import qualified Data.Text as T
import Data.Text (Text)
import Data.Csv hiding ((.=))
import qualified Data.Csv as Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Vector as V
import Control.Exception (try, SomeException)
import qualified Data.Map as Map
import qualified Debug.Trace
import Control.Monad (forM)
import Data.Either (rights, isRight)

type CSVRow = Map.Map String String
type CSVData = V.Vector CSVRow
type RankingsData = Map.Map String [CSVData]

csvSeparator :: Char
csvSeparator = ';'

obSportType :: String
obSportType = "1"

rankingTypes :: [Int]
rankingTypes = [1..9]

-- | Base URL for ranking export
rankingUrl :: String
rankingUrl = "https://oris.orientacnisporty.cz/ranking_export"

csvDir :: FilePath
csvDir = "csv_data"

csvFileName :: String -> String -> String -> FilePath
csvFileName gender timePeriod rtype = csvDir </>
  (gender ++ "_" ++ timePeriod ++ "_" ++ rtype ++ ".csv")

saveCsvData :: FilePath -> BS.ByteString -> IO (Either String ())
saveCsvData filePath csvData = do
  result <- try $ do
    createDirectoryIfMissing True csvDir
    BS.writeFile filePath csvData
  case result of
    Left e -> return $ Left $ "Failed to save CSV file: " ++ show (e :: SomeException)
    Right _ -> return $ Right ()

downloadAndLoadCsv :: String -> String -> Int -> IO (Either String (V.Vector (Map.Map String String)))
downloadAndLoadCsv gender date rankingType = do
  let params = [
          ("date", date)
        , ("sport", obSportType)
        , ("gender", gender)
        , ("csv", "1")
        , ("ranktype", show rankingType)
        ]
      queryParams = map (\(k, v) -> (BS.pack k, Just $ BS.pack v)) params
      fileName = csvFileName gender date (show rankingType)
  baseRequest <- parseRequest rankingUrl
  let request = setRequestQueryString queryParams baseRequest
  
  result <- try $ httpBS request
  case result of
    Left e -> return $ Left $ "CSV download failed: " ++ show (e :: SomeException)
    Right response -> do
      let csvDataStrict = getResponseBody response
          csvDataLazy = BL.fromStrict csvDataStrict

      saveResult <- saveCsvData fileName csvDataStrict
      case saveResult of
        Left saveErr -> putStrLn $ "Warning: " ++ saveErr
        Right _ -> putStrLn $ "CSV saved to: " ++ fileName

      case parseCSV csvDataLazy of
        Left err -> return $ Left $ "CSV parsing failed: " ++ err
        Right records -> return $ Right records

parseCSV :: BL.ByteString -> Either String CSVData
parseCSV csvData =
  case decodeWith csvOptions NoHeader csvData of
    Left err -> Left err
    Right records -> 
      case V.uncons records of
        Nothing -> Right V.empty
        Just (headerRow, dataRows) -> 
          let headers = V.toList $ V.map (T.unpack . TE.decodeUtf8) headerRow
              convertRow row = Map.fromList $ zip headers (map (T.unpack . TE.decodeUtf8) $ V.toList row)
          in Right $ V.map convertRow dataRows
  where
    csvOptions = defaultDecodeOptions { decDelimiter = fromIntegral (fromEnum csvSeparator) }

loadCsvFromFile :: FilePath -> IO (Either String CSVData)
loadCsvFromFile filePath = do
  result <- try $ do
    csvData <- BL.readFile filePath
    return $ parseCSV csvData
  
  case result of
    Left e -> return $ Left $ "Failed to load cached file: " ++ show (e :: SomeException)
    Right (Left parsedErr) -> return $ Left $ "Failed to parse cached file: " ++ parsedErr
    Right (Right parsed) -> return $ Right parsed

sequenceEithers :: [Either String a] -> Either String [a]
sequenceEithers = sequence

loadCsvsFromFilesByDate :: String -> IO (Either String RankingsData)
loadCsvsFromFilesByDate sTime = do
    let loadGender = (\gender -> forM rankingTypes $ \rt -> loadCsvFromFile $ csvFileName gender sTime (show rt))

    hRankings <- loadGender "M"
    fRankings <- loadGender "F"

    case (sequence hRankings, sequence fRankings) of
        (Right h, Right f) -> return $ Right $ Map.fromList [("M", h), ("F", f)]
        (Left err, _)      -> return $ Left $ "Failed to load male rankings: " ++ err
        (_, Left err)      -> return $ Left $ "Failed to load female rankings: " ++ err

checkIfCachedCSV :: String -> IO (Bool)
checkIfCachedCSV date = doesFileExist filename
  where
    filename = csvFileName "M" date "1"

downloadRankings :: String -> IO (Either String RankingsData)
downloadRankings sTime = do
  mRankings <- try $ do
    hRankings <- forM rankingTypes $ \i -> downloadAndLoadCsv "M" sTime i
    fRankings <- forM rankingTypes $ \i -> downloadAndLoadCsv "F" sTime i

    let successfullDownloadsM = rights hRankings
        successfullDownloadsF = rights fRankings

    if all (all isRight) [hRankings, fRankings] 
      then do
        putStrLn "Ranking Data downloaded successfully"
        return $ Right $ Map.fromList [
            ("M", successfullDownloadsM),
            ("F", successfullDownloadsF)
            ]
    else
      return $ Left "Failed to download some ranking data"

  case mRankings of
    Left e -> return $ Left $ "Download rankings failed: " ++ show (e :: SomeException)
    Right result -> return result

