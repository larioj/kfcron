{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module KanbanFlow.Task where

import           Data.Aeson          (ToJSON, defaultOptions, genericToEncoding,
                                      toEncoding)
import           GHC.Generics        (Generic)
import           KanbanFlow.Board    (Board)
import           KanbanFlow.Color    (Color)
import           KanbanFlow.Name     (Name)
import           KanbanFlow.TaskId   (TaskId)
import           KanbanFlow.Token    (Token)
import           KanbanFlow.Url      (Url)
import           KanbanFlow.Util     (addAuthorizationHeader)
import           KanbanFlow.Uuid     (Uuid)
import qualified Network.HTTP.Simple as Http
import           Util                (build)

data Task = Task
  { name     :: Name
  , columnId :: Uuid
  , color    :: String
  } deriving (Generic, Show, Eq, Ord)

instance ToJSON Task where
  toEncoding = genericToEncoding defaultOptions

empty :: Task
empty = Task undefined undefined undefined

url :: Url
url = "https://kanbanflow.com/api/v1/tasks"

postRequest :: Token -> Task -> Http.Request
postRequest token task =
  build (Http.parseRequest_ url) $
  Http.setRequestMethod "POST" .
  addAuthorizationHeader token . Http.setRequestBodyJSON task

post :: Token -> Task -> IO (Http.Response TaskId)
post token task = Http.httpJSON $ postRequest token task
