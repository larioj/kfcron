{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module KanbanFlow.Board where

import           Data.Aeson             (FromJSON)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base64 as Base64
import           GHC.Generics           (Generic)
import           KanbanFlow.Color       (Color)
import           KanbanFlow.Column      (Column)
import           KanbanFlow.Name        (Name)
import           KanbanFlow.Token       (Token)
import           KanbanFlow.Url         (Url)
import           KanbanFlow.Util        (addAuthorizationHeader)
import           KanbanFlow.Uuid        (Uuid)
import qualified Network.HTTP.Simple    as Http
import           Util                   (build)

data Board = Board
  { _id     :: Uuid
  , name    :: Name
  , columns :: [Column]
  , colors  :: [Color]
  } deriving (Generic, Show, Eq, Ord)

instance FromJSON Board

url :: Url
url = "https://kanbanflow.com/api/v1/board"

request :: Token -> Http.Request
request token =
  build (Http.parseRequest_ url) $
  Http.setRequestMethod "GET" . addAuthorizationHeader token

get :: Token -> IO (Http.Response Board)
get = Http.httpJSON . request
