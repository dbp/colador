{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, NoMonomorphismRestriction, QuasiQuotes #-}

module Event.HandlerTest where

import Prelude hiding ((++))
import qualified Data.Map as M
import Control.Monad (void)
import Control.Monad.Trans
import qualified Control.Monad.Trans.State as S (get)
import qualified System.IO.Streams as S (write)
import Snap.Snaplet.Groundhog.Postgresql hiding (get)
import Snap.Test.BDD
import TestCommon
import Data.Text.Encoding
import qualified Data.Text as T
import Data.Text (Text)

-- BDD IMPORTS

import Text.XML.Cursor (fromDocument)
import           Snap.Test (RequestBuilder, getResponseBody)
import           Snap.Core (Response(..), getHeader)
import           Snap.Snaplet (Handler, SnapletInit)
import           Snap.Snaplet.Test (runHandler, evalHandler)
import           Control.Exception (SomeException, catch)
import Text.HTML.DOM (parseLBS)
import qualified Data.ByteString.Lazy as LBS (fromStrict)
import Text.XML.Scraping (innerHtml)
import Text.XML.Selector.TH
import Text.XML.Selector.Types
  
-- end BDD IMPORTS

import Control.Applicative

import Application
import Event
import Event.Digestive

insertEvent = eval $ gh $ insert (Event "Alabaster" "Baltimore" "Crenshaw" (YearRange 1492 1494))

data Html = Html Text | EmptyHtml
data CssSelector = CssSelector [JQSelector]

css :: Applicative m => [JQSelector] -> m CssSelector
css = pure . CssSelector

-- stolen from Snap.Test.BDD

writeRes :: TestLog -> SnapTesting b ()
writeRes log' = do (_,_,out) <- S.get
                   lift $ S.write (Just log') out

runHandlerSafe :: TestRequest -> Handler b b v -> SnapletInit b b -> IO (Either Text Response)
runHandlerSafe req site app =
  catch (runHandler (Just "test") req site app) (\(e::SomeException) -> return $ Left (T.pack $ show e))

-- end stolen

should :: SnapTesting state TestLog -> SnapTesting state ()
should test = do res <- test
                 writeRes res

haveSelector :: Html -> CssSelector -> TestLog
haveSelector (Html body) (CssSelector selector) =
    let root = (fromDocument . parseLBS) $ LBS.fromStrict $ encodeUtf8 body
        cs = queryT selector root
    in
    case cs of
      [] -> TestFail $ T.concat ["Expected to see something."]
      _ -> TestPass ""


haveText :: Html -> Text -> TestLog
haveText EmptyHtml _ = TestFail "no html"
haveText (Html body) match = if T.isInfixOf match body then TestPass "" else TestFail message
  where
    message = T.concat ["Expected body to contain \"", match, "\". Actually saw:\n\n", body, "\n\n"]


get1 :: Text -> SnapTesting state Html
get1 path = do
  (site, app, _) <- S.get
  res <- liftIO $ runHandlerSafe (get $ encodeUtf8 path) site app
  case res of
    Left err -> do
      writeRes (TestError err)
      return $ EmptyHtml
    Right response -> do
      body <- liftIO $ getResponseBody response
      return $ Html $ decodeUtf8 body

eventTests :: SnapTesting App ()
eventTests = cleanup (void $ gh $ deleteAll (undefined :: Event)) $
  do
     it "#index" $ do
       eventKey <- insertEvent
       should $ haveSelector <$> (get1 "/events") <*> css [jq| table.table td |]
       should $ haveText <$> get1 "/events" <*> pure "Alabaster"
       should $ haveText <$> get1 "/events" <*> pure "Crenshaw"
       contains (get "/events") "<td"
       contains (get "/events") "Alabaster"
       contains (get "/events") "Crenshaw"
       contains (get "/events") "href='/events/new'"
       notcontains (get "/events") "Baltimore"
       contains (get "/events") $ eventEditPath eventKey
     it "#map" $ do
       _eventKey <- insertEvent
       contains (get "/events/map") "<svg"
       contains (get "/events/map") "<image xlink:href='/static/LAMap-grid.gif'"
     it "#show" $ do
       eventKey <- insertEvent
       let showPath = encodeUtf8 $ eventPath eventKey
       notcontains (get showPath) "<form"
       contains (get showPath) "Alabaster"
       contains (get showPath) "Baltimore"
       contains (get showPath) "Crenshaw"
     it "#new" $ do
       contains (get "/events/new") "<form"
       contains (get "/events/new") "title"
       contains (get "/events/new") "content"
       contains (get "/events/new") "startYear"
       contains (get "/events/new") "endYear"
     it "#edit" $ do
       eventKey <- insertEvent
       let editPath = encodeUtf8 $ eventEditPath eventKey
       contains (get editPath) "<form"
       contains (get editPath) "Alabaster"
       contains (get editPath) "Baltimore"
       contains (get editPath) "Crenshaw"
       it "#update" $ do
         changes (0 +)
           (gh $ countAll (undefined :: Event))
           (post editPath $ params [("new-event.title", "a"),
                                    ("new-event.content", "b"),
                                    ("new-event.citation", "c")])
     it "#create" $ do
       changes (1 +)
         (gh $ countAll (undefined :: Event))
         (post "/events/new" $ params [("new-event.title", "a"),
                                       ("new-event.content", "b"),
                                       ("new-event.citation", "c")])
     it "#deletes" $ do
       eventKey <- insertEvent
       changes (-1 +)
         (gh $ countAll (undefined :: Event))
         (post (encodeUtf8 $ eventPath eventKey) $ params [("_method", "DELETE")])
     it "validates presence of title, content and citation" $ do
       let expectedEvent = Event "a" "b" "c" (YearRange 1200 1300)
       form (Value expectedEvent) (eventForm Nothing) $
         M.fromList [("title", "a"), ("content", "b"), ("citation", "c"),
                     ("startYear", "1200"), ("endYear", "1300")]
       form (ErrorPaths ["title", "content", "citation"]) (eventForm Nothing) $ M.fromList []
