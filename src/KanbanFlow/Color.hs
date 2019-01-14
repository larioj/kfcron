{-# LANGUAGE DeriveGeneric #-}

module KanbanFlow.Color where

import           Data.Aeson      (FromJSON)
import           GHC.Generics    (Generic)
import           KanbanFlow.Name (Name)

data Color = Color
  { name        :: Name
  , value       :: String
  , description :: Maybe String
  } deriving (Generic, Show, Eq, Ord)

instance FromJSON Color
