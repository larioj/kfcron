{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent  (threadDelay)
import           Control.Monad       (when)
import           Data.Foldable       (for_)
import           Data.Maybe          (catMaybes, fromMaybe)
import           Data.String.Utils   (strip)
import           Data.Time.Clock     (NominalDiffTime, UTCTime)
import           Data.Time.Clock     (addUTCTime, diffUTCTime, getCurrentTime)
import           Data.Time.Format    (defaultTimeLocale, formatTime)
import           Data.Time.LocalTime (TimeZone, getCurrentTimeZone,
                                      utcToLocalTime)
import           Data.Yaml           (decodeFileEither)
import           KanbanFlow.Token    (Token)
import           KfCron.Blueprint    (Blueprint (..))
import qualified KfCron.Board        as Board
import           KfCron.Config       (Config (..))
import           KfCron.Task         (Task (..))
import qualified KfCron.Task         as Task
import           System.Cron         (CronSchedule, nextMatch, scheduleMatches)
import           System.Directory    (doesPathExist)
import           System.Environment  (getArgs)
import           Util                (die, nth)

updateDelay :: NominalDiffTime
updateDelay = 60 -- seconds

usageMsg :: String
usageMsg = "usage: kfcron [path-to-schedule] [path-to-token]"

main :: IO ()
main = do
  blueprintPath <- argAt 0
  blueprint <- die "unable to load blueprint" =<<
    loadBlueprint blueprintPath
  tokenPath <- argAt 1
  token <- die "unable to load token" =<<
    loadToken tokenPath
  now <- getCurrentTime
  execBlueprint (Config tokenPath blueprintPath) token blueprint now

argAt :: Int -> IO String
argAt n = do
  args <- getArgs
  case nth n args of
    Just a  -> return a
    Nothing -> fail usageMsg

showTime :: TimeZone -> UTCTime -> String
showTime zone t =
  formatTime defaultTimeLocale "%c" (utcToLocalTime zone t)

loadBlueprint :: FilePath -> IO (Maybe Blueprint)
loadBlueprint path = do
  result <- decodeFileEither path
  case result of
    Right b -> return $ Just b
    Left e -> do
      putStr "Error loading blueprint: "
      putStrLn $ show e
      return Nothing

loadToken :: FilePath -> IO (Maybe Token)
loadToken path = do
  exists <- doesPathExist path
  if exists
    then fmap (Just . strip) $ readFile path
      else return Nothing

sleepTill :: UTCTime -> IO ()
sleepTill deadline = do
  now <- getCurrentTime
  seconds <- pure . floor $ diffUTCTime deadline now
  when (deadline > now) $ do
    threadDelay (seconds * 1000000)
    return ()

nextEarliestMatch :: UTCTime -> [CronSchedule] -> Maybe UTCTime
nextEarliestMatch lastMatch schedules =
  case catMaybes $ fmap (flip nextMatch lastMatch) schedules of
    []      -> Nothing
    matches -> Just $ minimum matches

execBlueprint :: Config -> Token -> Blueprint -> UTCTime -> IO ()
execBlueprint (Config tokenPath blueprintPath) oldToken oldBlueprint execTime = do
  blueprint@(Blueprint tasks) <-
    fmap (fromMaybe oldBlueprint) (loadBlueprint blueprintPath)
  token <-
    fmap (fromMaybe oldToken) (loadToken tokenPath)
  board <- Board.load token
  now <- getCurrentTime
  for_ tasks $ \task ->
    when (scheduleMatches (schedule task) execTime) $ do
      t <- showTime <$> getCurrentTimeZone <*> getCurrentTime
      putStrLn $ "[" <> t <> "] Creating Task: " <> show task
      Task.create token board task
      return ()
  now <- getCurrentTime
  nextScheduledTime <- pure . nextEarliestMatch execTime . fmap schedule $ tasks
  nextExecTime <-
    pure . fromMaybe (addUTCTime updateDelay now) $ nextScheduledTime
  sleepTill nextExecTime
  execBlueprint (Config tokenPath blueprintPath) token blueprint nextExecTime
