{-# LANGUAGE Arrows, TemplateHaskell, GADTs, QuasiQuotes,
    FlexibleInstances, DeriveDataTypeable, PackageImports,
    TypeFamilies, MultiParamTypeClasses, OverloadedStrings #-}

module Event where

import Prelude hiding (id, all)
import Data.Text (Text)
import Data.Maybe (listToMaybe, fromMaybe)
import Control.Monad (void)
import Control.Applicative ((<$>))

import Karamaan.Opaleye.Manipulation ()

import Opaleye
-- NOTE(dbp 2014-04-23): API weakness that there is a distinction between Expr and Query...
import qualified Karamaan.Opaleye.ExprArr as E (constant, eq)
import Application

data Event' a b c d e f = Event {
  id :: a,
  title :: b,
  content :: c,
  citation :: d,
  yearStart :: e,
  yearEnd :: f
  }

-- NOTE(dbp 2014-04-23): Boilerplate, good candidate for some TH :)
type Event = Event' Int Text Text Text Int Int
type NewEvent = Event' () Text Text Text Int Int
type EventSpec = Event' String String String String String
type EventWire = Event' (Wire Int) (Wire Text) (Wire Text) (Wire Text) (Wire Int) (Wire Int)
type EventWireExpr = Event' (Maybe (Wire Int)) (Maybe (Wire Text))
                            (Maybe (Wire Text)) (Maybe (Wire Text))
                            (Maybe (Wire Int)) (Maybe (Wire Int))
$(makeAdaptorAndInstance "pEvent" ''Event')


table :: Table EventWire
table = Table "Event" (Event (Wire "id") (Wire "title") (Wire "content") (Wire "citation")
                             (Wire "years#start_year") (Wire "years#end_year"))

all :: Query EventWire
all = makeTableTDef table

get :: Int -> AppHandler (Maybe Event)
get i = listToMaybe <$> runO q
  where q = proc () -> do event <- all -< ()
                          i' <- constant i -< ()
                          restrict <<< eq -< (i', id event)
                          returnA -< event

expr :: NewEvent -> Expr EventWireExpr
expr (Event _ t' co' ci' ys' ye') =
  proc () -> do
    t  <- E.constant t' -< ()
    co <- E.constant co' -< ()
    ci <- E.constant ci' -< ()
    ys <- E.constant ys' -< ()
    ye <- E.constant ye' -< ()
    returnA -< (Event Nothing (Just t) (Just co) (Just ci) (Just ys) (Just ye))

idExpr :: ExprArr a a
idExpr = proc e -> returnA -< e

byId :: Int -> ExprArr EventWire (Wire Bool)
byId i = proc (Event id' _ _ _ _ _) -> do i' <- E.constant i -< ()
                                          E.eq -< (i', id')

insert :: NewEvent -> AppHandler (Maybe Event)
insert event = listToMaybe <$> insOR table (expr event) idExpr

replace :: Int -> NewEvent -> AppHandler ()
replace i event = void $ updO table q (byId i)
  where q :: ExprArr EventWire EventWireExpr
        q = proc _ -> expr event -< ()

deleteBy :: Int -> AppHandler ()
deleteBy i = void $ delO table (byId i)

deleteAll :: AppHandler ()
deleteAll = void $ delO table q
  where q :: ExprArr EventWire (Wire Bool)
        q = proc (_) -> E.constant True -< ()

selectAll :: AppHandler [Event]
selectAll = runO all

countAll :: AppHandler Int
countAll = (fromMaybe 0 . listToMaybe) <$> runO (aggregate count q)
  where q = proc () -> do
             event <- all -< ()
             returnA -< (id event)

stripId :: Event -> NewEvent
stripId e = e { id = () }
