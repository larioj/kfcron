{-# LANGUAGE OverloadedStrings #-}

module KanbanFlow.Util where

import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base64 as Base64
import           Data.ByteString.Char8  (pack)
import           KanbanFlow.Token       (Token)
import qualified Network.HTTP.Simple    as Http

addAuthorizationHeader :: Token -> Http.Request -> Http.Request
addAuthorizationHeader token =
  Http.addRequestHeader
    "Authorization"
    ("Basic " <> Base64.encode ("apiToken:" <> pack token))
