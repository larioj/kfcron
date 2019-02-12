{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Time.LocalTime (TimeZone, getCurrentTimeZone, utcToLocalTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import           Control.Concurrent (threadDelay)
import           Control.Monad      (when)
import           Data.Foldable      (for_)
import           Data.Maybe         (catMaybes, fromMaybe)
import           Data.Time.Clock    (NominalDiffTime, UTCTime)
import           Data.Time.Clock    (addUTCTime, diffUTCTime, getCurrentTime)
import           Data.Yaml          (decodeFileEither)
import           KanbanFlow.Token   (Token)
import           KfCron.Blueprint   (Blueprint (..))
import qualified KfCron.Board       as Board
import           KfCron.Task        (Task (..))
import qualified KfCron.Task        as Task
import           System.Cron        (CronSchedule, nextMatch, scheduleMatches)
import           System.Environment (getArgs)
import           Util               (headMaybe)

updateDelay :: NominalDiffTime
updateDelay = 60 -- seconds

main :: IO ()
main = do
  args <- getArgs
  blueprintPath <- maybe (fail "usage: kfcron path") pure (headMaybe args)
  maybeBlueprint <- loadBlueprint blueprintPath
  blueprint <- maybe (fail "unable to load blueprint") pure (maybeBlueprint)
  now <- getCurrentTime
  execBlueprint blueprintPath blueprint now

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

execBlueprint :: FilePath -> Blueprint -> UTCTime -> IO ()
execBlueprint blueprintPath oldBlueprint execTime = do
  blueprint@(Blueprint token tasks) <-
    fmap (fromMaybe oldBlueprint) (loadBlueprint blueprintPath)
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
  execBlueprint blueprintPath blueprint nextExecTime
