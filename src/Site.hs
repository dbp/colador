{-# Language OverloadedStrings, GADTs, FlexibleInstances, TypeFamilies, NoMonomorphismRestriction, ScopedTypeVariables, FlexibleContexts #-}

module Site where

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Snap.Snaplet
import Snap.Core
import Snap.Snaplet.Heist
import Snap.Snaplet.PostgresqlSimple hiding (Query)
import Snap.Util.FileServe
import Event.Site as Event

import Application

routes :: [(ByteString, Handler App App ())]
routes = [("/events", route Event.routes),
          ("/static", serveDirectory "static")]

methodWrapper :: AppHandler () -> AppHandler ()
methodWrapper site = do _method <- (>>= readMethod) <$> getParam "_method"
                        case _method of
                          Nothing -> return ()
                          Just m -> modifyRequest (\r -> r { rqMethod = m })
                        site
  where readMethod "GET" = Just GET
        readMethod "POST" = Just POST

app :: SnapletInit App App
app = makeSnaplet "colador" "An event mapping application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    g <- nestSnaplet "postgres" postgres pgsInit
    addRoutes Site.routes
    return $ App h g
