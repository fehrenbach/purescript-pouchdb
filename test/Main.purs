module Test.Main where

import Prelude
import Test.Unit.Assert as Assert
import Control.Monad.Aff (attempt)
import Control.Monad.Aff.AVar (modifyVar, takeVar, makeVar, AVAR)
import Control.Monad.Aff.Console (logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Argonaut (class DecodeJson, class EncodeJson, JObject, decodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Either (Either(..))
import Data.Newtype (wrap)
import Database.PouchDB.Aff (ReplicationEvent(..), swap, Document(..), destroy, get, info, pouchDB, create, save, startReplication, singleShotReplication)
import Database.PouchDB.FFI (POUCHDB)
import Test.Unit (failure, suite, test)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Unsafe.Coerce (unsafeCoerce)

-- TODO This should perhaps be a newtype
data Movie = Movie { title :: String,
                     year :: Int,
                     actors :: Array String }

instance encodeJsonMovie :: EncodeJson Movie where
  encodeJson (Movie {title, year, actors}) =
    "type" := "movie"
    ~> "title" := title
    ~> "year" := year
    ~> "actors" := actors
    ~> jsonEmptyObject

instance eqMovie :: Eq Movie where
  eq (Movie {title: tl, year: yl, actors: al}) (Movie {title: tr, year: yr, actors: ar}) =
    tl == tr && yl == yr && al == ar

instance showMovie :: Show Movie where
  show (Movie {title}) = "Movie " <> title

assertType :: String -> JObject -> Either String Unit
assertType expected json = do
  typ <- json .? "type"
  if typ == expected
    then Right unit
    else Left ("expected type " <> expected <> " but got " <> typ)

instance decodeJsonMovie :: DecodeJson Movie where
  decodeJson json = do
    obj <- decodeJson json
    assertType "movie" obj
    title <- obj .? "title"
    year <- obj .? "year"
    actors <- obj .? "actors"
    pure $ Movie {title, year, actors}

replicationEventHandler :: ReplicationEvent -> _
replicationEventHandler (Change c) = do
  log "change"
  log (unsafeCoerce c)
replicationEventHandler Active = do
  log "ACTIVE!!"
replicationEventHandler _ = do
  log "oho!"

main :: forall e. Eff (pouchdb :: POUCHDB, console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR | e) Unit
main = runTest do
  let juno = Movie {title: "Juno", year: 2007, actors: ["Ellen Page", "Michael Cera"]}
  suite "info" do
    test "local database name" do
      db <- pouchDB "localdatabase"
      i <- info db
      Assert.equal i.db_name "localdatabase"
  suite "simple read&write" do
    test "write doc response" do
      db <- pouchDB "localdatabase"
      Document id _ m <- create db (wrap "juno") juno
      Assert.equal id (wrap "juno")
      Assert.equal m juno
    test "read written doc" do
      db <- pouchDB "localdatabase"
      Document id rev m <- get db (wrap "juno")
      Assert.equal id (wrap "juno")
      Assert.assertFalse "rev empty" (rev == wrap "")
      Assert.equal m juno
    test "save doc" do
      db <- pouchDB "localdatabase"
      d <- get db (wrap "juno")
      let d' = map (\(Movie m) -> Movie (m {actors = ["Ellen Page", "Michael Cera", "Olivia Thirlby"]})) d
      d'' <- save db d'
      d''' <- get db (wrap "juno")
      Assert.equal d'' d'''
    test "swap" do
      db <- pouchDB "localdatabase"
      (Document _ _ j) <- swap db (\(Movie m) -> Movie (m {actors = ["Ellen Page", "Michael Cera"]})) (wrap "juno")
      Assert.equal j juno
  suite "error cases" do
    test "create doc again conflicts" do
      db <- pouchDB "localdatabase"
      a <- attempt $ create db (wrap "juno") juno
      case a of
        -- TODO provide a way to deal with (common) errors nicely
        Left e -> Assert.equal ((unsafeCoerce e).name) "conflict"
        Right r -> failure "attempting to write a document twice should fail"
  suite "replication" do
    test "local/local" do
      source <- pouchDB "localdatabase"
      target <- pouchDB "localsync"
      i <- info target
      Assert.equal 0 i.doc_count
      singleShotReplication source target
      i' <- info target
      Assert.equal 1 i'.doc_count
  suite "destroy" do
    test "0 documents after destroy" do
      db <- pouchDB "localdatabase"
      destroy db
      reopened <- pouchDB "localdatabase"
      {doc_count} <- info reopened
      Assert.equal 0 doc_count
    test "clean up" do
      db <- pouchDB "localdatabase"
      destroy db
      db' <- pouchDB "localsync"
      destroy db'
