{-# LANGUAGE DeriveGeneric #-}

module KanbanFlow.TaskId where

import           Data.Aeson      (FromJSON)
import           GHC.Generics    (Generic)
import           KanbanFlow.Uuid (Uuid)

data TaskId = TaskId
  { taskId :: Uuid
  } deriving (Generic, Show, Eq, Ord)

instance FromJSON TaskId
