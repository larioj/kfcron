module KfCron.Board where

import           Data.Map            (Map)
import qualified Data.Map            as Map
import qualified KanbanFlow.Board    as KfBoard
import           KanbanFlow.Color    (Color)
import qualified KanbanFlow.Color    as Color
import           KanbanFlow.Column   (Column)
import qualified KanbanFlow.Column   as Column
import           KanbanFlow.Name     (Name)
import           KanbanFlow.Token    (Token)
import           KanbanFlow.Uuid     (Uuid)
import qualified Network.HTTP.Simple as Http

data Board = Board
  { _id    :: Uuid
  , name   :: Name
  , column :: Map Name Column
  , color  :: Map Name Color
  } deriving (Show, Eq, Ord)

empty :: Board
empty = Board undefined undefined undefined undefined

load :: Token -> IO Board
load token = do
  board <- fmap Http.getResponseBody $ KfBoard.get token
  return $
    empty
      { _id = KfBoard._id board
      , name = KfBoard.name board
      , column =
          Map.fromList . fmap (\c -> (Column.name c, c)) $ KfBoard.columns board
      , color =
          Map.fromList . fmap (\c -> (Color.name c, c)) $ KfBoard.colors board
      }
