{-# LANGUAGE DeriveGeneric #-}

module KfCron.CronSchedule where

import           Data.Aeson   (FromJSON (..), withText)
import           GHC.Generics (Generic)
import           System.Cron  (parseCronSchedule)
import qualified System.Cron  as Cron

type CronSchedule = Cron.CronSchedule

instance FromJSON Cron.CronSchedule where
  parseJSON = withText "CronSchedule" $ either fail return . parseCronSchedule
