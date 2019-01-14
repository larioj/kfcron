{-# LANGUAGE DeriveGeneric #-}

module KfCron.Task where

import           Data.Aeson          (FromJSON)
import           Data.Map            ((!))
import           GHC.Generics        (Generic)
import qualified KanbanFlow.Board    as KfBoard
import           KanbanFlow.Color    (Color)
import qualified KanbanFlow.Color    as Color
import qualified KanbanFlow.Color    (Color)
import qualified KanbanFlow.Column   as Column
import           KanbanFlow.Name     (Name)
import qualified KanbanFlow.Task     as KfTask
import           KanbanFlow.TaskId   (TaskId)
import           KanbanFlow.Token    (Token)
import           KanbanFlow.Uuid     (Uuid)
import           KfCron.Board        (Board)
import qualified KfCron.Board        as Board
import           KfCron.CronSchedule (CronSchedule)
import qualified Network.HTTP.Simple as Http

data Task = Task
  { name     :: Name
  , column   :: String
  , category :: String
  , schedule :: CronSchedule
  } deriving (Generic, Show, Eq)

instance FromJSON Task

empty :: Task
empty = Task undefined undefined undefined undefined

postPayload :: Board -> Task -> KfTask.Task
postPayload board task =
  KfTask.empty
    { KfTask.name = name task
    , KfTask.columnId = Column.uniqueId $ Board.column board ! column task
    , KfTask.color = Color.value $ Board.color board ! category task
    }

create :: Token -> Board -> Task -> IO TaskId
create token board task = do
  res <- KfTask.post token (postPayload board task)
  return $ Http.getResponseBody res
