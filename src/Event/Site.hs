{-# Language OverloadedStrings, GADTs, TemplateHaskell, QuasiQuotes, FlexibleInstances, TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables, FlexibleContexts #-}

module Event.Site where

import Prelude hiding ((++))
import Data.ByteString (ByteString)
import Data.Aeson
import qualified Data.ByteString.Char8 as B8
import Snap.Core
import Snap.Snaplet.Heist
import Text.Digestive.Snap (runForm)
import Text.Digestive.Heist

import Event.Digestive
import Event (Event)
import qualified Event as E
import Event.Json
import Helpers

import Application

home :: AppHandler ()
home = redirect "/events"

routes :: [(ByteString, AppHandler ())]
routes = [("", ifTop $ eventIndexHandler)
                    ,(":id", restMethodDispatcher (\_method -> case _method of
                                                     DELETE -> deleteEventHandler
                                                     GET -> showEventHandler
                                                     _ -> home))
                    ,("new", newEventHandler)
                    ,(":id/edit", editEventHandler)
                    ,("map", mapHandler)
                    ,("index.json", eventsJsonHandler)]

eventsHandler :: ByteString -> AppHandler ()
eventsHandler template =  do
  events <- E.selectAll
  renderWithSplices template (eventsSplice events)

eventIndexHandler :: AppHandler ()
eventIndexHandler = eventsHandler "events/index"

mapHandler :: AppHandler ()
mapHandler = eventsHandler "events/map"

eventsJsonHandler :: AppHandler ()
eventsJsonHandler = do
  events <- E.selectAll
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS $ encode $ map mapEvent events

showEventHandler :: AppHandler ()
showEventHandler = do
  maybeEventKey <- eventKeyParam "id"
  case maybeEventKey of
    Nothing -> home
    Just eventKey -> do
      maybeEvent <- E.get eventKey
      case maybeEvent of
        Nothing -> home
        Just event -> renderWithSplices "/events/show" $ eventSplice event

newEventHandler :: AppHandler ()
newEventHandler = do
  response <- runForm "new-event" (eventForm Nothing)
  case response of
    (v, Nothing) -> renderWithSplices "events/form" (digestiveSplices v)
    (_, Just e) -> do
      E.insert e
      home

editEventHandler :: AppHandler ()
editEventHandler = do
  maybeEventKey <- eventKeyParam "id"
  case maybeEventKey of
    Nothing -> home
    Just eventKey -> do
      maybeEvent <- E.get eventKey
      response <- runForm "edit-event" (eventForm $ fmap E.stripId maybeEvent)
      case response of
        (v, Nothing) -> renderWithSplices "events/form" (digestiveSplices v)
        (_, Just e) -> do
          E.replace eventKey e
          home

deleteEventHandler :: AppHandler ()
deleteEventHandler = do
  maybeEventKey <- eventKeyParam "id"
  case maybeEventKey of
    Nothing -> home
    Just eventKey -> do
      E.deleteBy eventKey
      home

eventKeyParam :: MonadSnap m => ByteString -> m (Maybe Int)
eventKeyParam name = fmap (fmap (read . B8.unpack)) (getParam name)
