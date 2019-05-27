{-# LANGUAGE OverloadedStrings #-}

module Util where

import           Data.List ((!!))
import           Data.Map  (Map)

build :: a -> (a -> b) -> b
build a f = f a

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe l  = Just $ head l

nth :: Int -> [a] -> Maybe a
nth i l =
  if i < length l && i >= 0
    then Just (l !! i)
      else Nothing

die :: String -> Maybe a -> IO a
die msg ma = maybe (fail msg) pure ma
