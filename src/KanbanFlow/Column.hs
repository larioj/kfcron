{-# LANGUAGE DeriveGeneric #-}

module KanbanFlow.Column where

import           Data.Aeson      (FromJSON)
import           GHC.Generics    (Generic)
import           KanbanFlow.Name (Name)
import           KanbanFlow.Uuid (Uuid)

data Column = Column
  { name     :: Name
  , uniqueId :: Uuid
  } deriving (Generic, Show, Eq, Ord)

instance FromJSON Column
