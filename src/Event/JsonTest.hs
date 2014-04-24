{-# LANGUAGE OverloadedStrings #-}
module Event.JsonTest where

import Data.Aeson
import Snap.Test.BDD
import Test.Common
import Control.Applicative
import Application
import Event
import Event.Json


eventJsonTests :: SnapTesting App ()
eventJsonTests = do
  it "produces JSON" $ do
    let mapEvent' = mapEvent (Event 1 "Alabaster" "Baltimore" "Crenshaw" 1492 1494)
        jsonObject = object ["startYear" .= (1492 :: Int),
                             "y".= (1 :: Int),
                             "x".= (1 :: Int),
                             "title".= ("Alabaster" :: String),
                             "id".= (1 :: Int),
                             "img".= ("/static/nature2.gif" :: String),
                             "endYear".= (1494 :: Int)]
    should $ equal <$> val (toJSON mapEvent') <*> val jsonObject
