{-# LANGUAGE OverloadedStrings #-}

module Util where

import           Data.Map (Map)

build :: a -> (a -> b) -> b
build a f = f a

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe l  = Just $ head l
