{-# LANGUAGE Arrows, FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses, NoMonomorphismRestriction #-}

module Opaleye (
    module Karamaan.Opaleye.Reexports
  , module Karamaan.Opaleye.Table
  , module Karamaan.Opaleye.SQL
  , module Karamaan.Opaleye.Operators2
  , module Karamaan.Opaleye.RunQuery
  , module Control.Category
  , module Control.Arrow
  , module Data.Profunctor
  , module Data.Profunctor.Product
  , module Data.Profunctor.Product.Default
  , module Data.Profunctor.Product.TH
  , module Karamaan.Opaleye.Wire
  , module Karamaan.Opaleye.ExprArr
  , showQuery
  , restrictNullable
  , runO
  , insO
  , delO
  , insOR
  , updO
  ) where

import Prelude hiding (not)


import Data.Int (Int64)
import Data.Text (Text, unpack)
import Data.String (fromString)

{-import Data.Time.Clock (UTCTime)-}
{-import Data.Time.LocalTime (LocalTime, localTimeToUTC, utc)-}
{-import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)-}
{-import System.Time (ClockTime(TOD), toUTCTime)-}

import Control.Monad.Trans (liftIO)
import Control.Category ((<<<))
import Control.Lens (use, view)

import Snap.Snaplet (snapletValue)
import Snap.Snaplet.PostgresqlSimple (pgPool)
import qualified Database.PostgreSQL.Simple as SQL
import Data.Pool

import Control.Arrow (returnA, second)
import Karamaan.Opaleye.Reexports
import Karamaan.Opaleye.Operators2 (not)
import Karamaan.Opaleye.Table (makeTableDef, Table(Table))
import Karamaan.Opaleye.SQL (showSqlForPostgresDefault)
import Karamaan.Opaleye.Unpackspec (Unpackspec)
import Karamaan.Opaleye.RunQuery (QueryRunner, fieldQueryRunner)
import Karamaan.Opaleye.Wire (Wire(Wire))
import Karamaan.Opaleye.ExprArr (ExprArr, Expr)
import Karamaan.Plankton ((.:))
import Karamaan.Opaleye.Wire (Wire(Wire))
import Karamaan.Opaleye.RunQuery as RQ
import qualified Karamaan.Opaleye.HaskellDB as H
import Data.Profunctor
import Data.Profunctor.Product (ProductProfunctor, empty, (***!),
                                ProductContravariant, point, (***<),
                                defaultEmpty, defaultProfunctorProduct,
                                defaultPoint, defaultContravariantProduct,
                                PPOfContravariant(PPOfContravariant),
                                unPPOfContravariant)

import Data.Profunctor.Product.Default (Default, def)

import Karamaan.Opaleye.QueryArr (Query)
import Karamaan.Opaleye.ExprArr (ExprArr, Expr, mul)
import Karamaan.Opaleye.Manipulation
import Karamaan.Opaleye.Table (Table(Table), makeTableTDef)
import Karamaan.Opaleye.Values ((.:.))

import Database.HaskellDB.Sql (SqlDelete, SqlInsert, SqlUpdate)
import Database.HaskellDB.Query (ShowConstant(..))
import Database.HaskellDB.PrimQuery (Literal(DateLit))
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Profunctor.Product
import Data.Profunctor.Product.Default (Default, def)

import Application

instance ShowConstant Text where
  showConstant = showConstant . unpack

-- -- NOTE(dbp 2014-04-03): Ridiculous conversion because HaskellDB uses deprecated old-time library.
-- instance ShowConstant UTCTime where
--   showConstant = DateLit . toUTCTime . uncurry TOD . second truncate
--                          . properFraction . utcTimeToPOSIXSeconds

-- instance ShowConstant LocalTime where
--   showConstant = DateLit . toUTCTime . uncurry TOD . second truncate
--                          . properFraction . utcTimeToPOSIXSeconds . localTimeToUTC utc

showQuery :: Default (PPOfContravariant Unpackspec) a a
          => Query a
          -> IO ()
showQuery = putStrLn . showSqlForPostgresDefault

restrictNullable :: QueryArr (Wire a, Wire (Nullable a)) (Wire a)
restrictNullable = proc (d, i) -> do restrict <<< not <<< isNull -< i
                                     fromNullable -< (d, i)


withPgConn :: (SQL.Connection -> AppHandler a) -> AppHandler a
withPgConn f  =  do sdb <- use postgres
                    let pool = pgPool (view snapletValue sdb)
                    withResource pool f

runO :: Default QueryRunner a b => Query a -> AppHandler [b]
runO q = withPgConn $ \con -> liftIO $ RQ.runQuery def q con

delO :: Default TableExprRunner t a =>
        Table t -> ExprArr a (Wire Bool) -> AppHandler Int64
delO t e = withPgConn $ \con -> liftIO $ SQL.execute_ con $ fromString (arrangeDeleteSqlDef t e)

insO :: (Default (PPOfContravariant Assocer) t' t',
                 Default TableMaybeWrapper t t')
     => Table t -> Expr t' -> AppHandler Int64
insO t e = withPgConn $ \con -> liftIO $ SQL.execute_ con $ fromString (arrangeInsertSqlDef t e)

insOR :: (Default (PPOfContravariant Assocer) t' t',
                     Default TableMaybeWrapper t t',
                     Default TableExprRunner t u,
                     Default (PPOfContravariant AssocerE) r r
         , SQL.FromRow r)
         => Table t -> Expr t' -> ExprArr u r -> AppHandler [r]
insOR t e r = withPgConn $ \con -> liftIO $ SQL.query_ con sql
  where sql = fromString $ arrangeInsertReturningSqlDef t e r

updO ::  (Default TableExprRunner t u,
                  Default (PPOfContravariant Assocer) t' t',
                  Default TableMaybeWrapper t t') =>
     Table t -> ExprArr u t' -> ExprArr u (Wire Bool)
       -> AppHandler Int64
updO t e e' = withPgConn $ \con -> liftIO $ SQL.execute_ con $ fromString (arrangeUpdateSqlDef t e e')
