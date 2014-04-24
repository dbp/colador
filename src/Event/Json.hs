{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Event.Json where

import Data.Text (Text)
import Data.Aeson.TH
import Event

-- NOTE(dbp 2014-04-23): In theory, this distinction is now irrelevant.
-- The only problem is that I doubt deriveJSON will work as-is.
mapEvent :: Event -> MapEvent
mapEvent (Event key title' _ _ startYear' endYear') =
  MapEvent key title' "/static/nature2.gif"
           (fromIntegral key) (fromIntegral key)
           startYear' endYear'

data MapEvent = MapEvent {
  id :: Int,
  title :: Text,
  img :: Text,
  x :: Double,
  y :: Double,
  startYear :: Int,
  endYear :: Int
  } deriving (Show)

$(deriveJSON defaultOptions ''MapEvent)
