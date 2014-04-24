{-# Language OverloadedStrings, GADTs, TemplateHaskell, QuasiQuotes, FlexibleInstances, TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables, FlexibleContexts #-}

module Event.Digestive where

import Prelude hiding ((++))
import Control.Applicative
import qualified Data.Text as T
import Data.Text (Text)
import Heist
import Heist.Interpreted
import Text.Digestive

import Event (Event'(..), Event, NewEvent)
import qualified Event as E
import Helpers
import Application

requiredTextField :: Text -> Text -> Form Text AppHandler Text
requiredTextField name defaultValue = name .: check "must not be blank" (not . T.null) (text $ Just defaultValue)

eventForm :: Maybe NewEvent -> Form Text AppHandler NewEvent
eventForm maybeEvent = case maybeEvent of
  Nothing -> form (Event () "" "" "" 0 0)
  (Just event) -> form event
  where
    form (Event _ _title _content _citation _startYear _endYear) =
      Event () <$> requiredTextField "title" _title
      <*> requiredTextField "content" _content
      <*> requiredTextField "citation" _citation
      <*> "startYear" .: stringRead "must be a number" (Just _startYear)
      <*> "endYear" .: stringRead "must be a number" (Just _endYear)

eventEditPath :: Event -> Text
eventEditPath event = (eventPath event) ++ "/edit"

eventPath :: Event -> Text
eventPath event = "/events/" ++ (showText $ E.id event)

eventsSplice :: [Event] -> Splices (Splice AppHandler)
eventsSplice events = "events" ## mapSplices (runChildrenWith . eventSplice) events

eventSplice :: Event -> Splices (Splice AppHandler)
eventSplice e@(Event _id _title _content _citation _startYear _endYear) = do
  "eventId" ## textSplice $ showText _id
  "editLink" ## textSplice $ eventEditPath e
  "eventLink" ## textSplice $ eventPath e
  "eventX" ## textSplice $ showText $ _id * 25
  "eventY" ## textSplice $ showText $ _id * 25
  "eventTitle" ## textSplice _title
  "eventContent" ## textSplice _content
  "eventCitation" ## textSplice _citation
  "eventYears" ## textSplice $ T.pack $ unwords $ map show [_startYear.._endYear]
  "eventStart" ## textSplice $ showText _startYear
  "eventEnd" ## textSplice $ showText _endYear
