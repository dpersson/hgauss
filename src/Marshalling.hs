{-# LANGUAGE OverloadedStrings #-}
module Marshalling where

import Data.Time.Format
import Data.Time
import Data.Aeson
import Data.Text
import Data.Maybe

newtype Some = Some { date :: UTCTime } deriving Show

instance FromJSON Some where
  parseJSON (Object x) = (x .: pack "date") >>= \v -> Some <$> parseTimeM True defaultTimeLocale "%FT%T" v

test :: Some
test = fromJust $ (decode "{\"date\":\"1999-12-01T12:12:12\"}" :: Maybe Some)