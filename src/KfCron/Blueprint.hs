{-# LANGUAGE DeriveGeneric #-}

module KfCron.Blueprint where

import           Data.Aeson       (FromJSON)
import           GHC.Generics     (Generic)
import           KanbanFlow.Token (Token)
import           KfCron.Task      (Task)

data Blueprint = Blueprint
  { tasks :: [Task]
  } deriving (Generic, Show, Eq)

instance FromJSON Blueprint
