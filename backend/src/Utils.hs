module Utils where

import Data.Time
import Data.Time.Format
import Debug.Trace


timeFormatStr :: String
timeFormatStr = "%Y-%m-%d"

lastDayOfPrevMonth2yrAgo :: Day -> Day
lastDayOfPrevMonth2yrAgo currentDay =
    let (year, month, _) = toGregorian currentDay
        (prevYr, prevMn) = if month <= 1
                            then (year - 1, 12)
                            else (year, month)
        firstDayOfCurrentMonth = fromGregorian (prevYr-2) prevMn 1
    in addDays (-1) firstDayOfCurrentMonth

lastDayOfPrevMonth :: Day -> Day
lastDayOfPrevMonth currentDay = 
    let (year, month, _) = toGregorian currentDay
        (prevYr, prevMn) = if month <= 1
                            then (year - 1, 12)
                            else (year, month)
        firstDayOfCurrentMonth = fromGregorian prevYr prevMn 1
    in addDays (-1) firstDayOfCurrentMonth

checkWithinTwoYearsPure :: Day -> String -> Bool
checkWithinTwoYearsPure purgeFreeZone dateStr =
    case parseTimeM True defaultTimeLocale timeFormatStr dateStr of
        Nothing -> False
        Just parsedDay -> parsedDay > purgeFreeZone

checkWithinTwoYears :: String -> IO Bool
checkWithinTwoYears dateStr = do
     now <- getCurrentTime
     
     let currentDay = utctDay now
         purgeDay   = lastDayOfPrevMonth currentDay 
         purgeDate  = addGregorianYearsClip (-2) purgeDay
 
     return $ checkWithinTwoYearsPure purgeDate dateStr

start = fromGregorian 2023 6 29
end = fromGregorian 2023 9 1
start2 = fromGregorian 2025 6 29
end2 = fromGregorian 2025 9 1
start3 = fromGregorian 2022 12 29
end3 = fromGregorian 2023 2 1
days = [start..end]
days2 = [start2..end2]
days3 = [start3..end3]

testfn = mapM_ (\x -> do
      let dateStr = formatTime defaultTimeLocale timeFormatStr x
      result <- checkWithinTwoYears dateStr
      print (x, result)
    ) days

testfn2 = mapM_ (\x -> do
      let dateStr = formatTime defaultTimeLocale timeFormatStr x
      result <- checkWithinTwoYears dateStr
      print (x, result)
    ) days2

testfn3 = mapM_ (\x -> do
      let dateStr = formatTime defaultTimeLocale timeFormatStr x
      result <- checkWithinTwoYears dateStr
      print (x, result)
    ) days3
