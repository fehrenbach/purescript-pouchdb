module Test.Main where

import Prelude

import Control.Parallel.Class (parallel, sequential)
import Data.Either (Either(..))
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Time.Duration (Milliseconds(..))
import Database.PouchDB (Checkpoint(..), Id(..), Rev, bulkGet, changesLiveSinceNow, createDoc, deleteDoc, destroy, getDoc, info, pouchDBLocal, saveDoc, singleShotReplication)
import Effect (Effect)
import Effect.Aff (Error, attempt, delay, runAff)
import Effect.Aff.AVar (empty, put, take)
import Simple.JSON (class ReadForeign, class WriteForeign)
import Test.Unit (failure, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Unsafe.Coerce (unsafeCoerce)

newtype Actor = Actor { _id :: Id Actor, _rev :: Rev Actor, name :: String }

derive instance newtypeActor :: Newtype Actor _
derive newtype instance writeForeignActor :: WriteForeign Actor
derive newtype instance readForeignActor :: ReadForeign Actor


newtype Movie = Movie { _id :: Id Movie, _rev :: Rev Movie, title :: String, year :: Int, actors :: Array (Id Actor), sequel :: Maybe (Id Movie)}

derive instance newtypeMovie :: Newtype Movie _
derive newtype instance writeForeignMovie :: WriteForeign Movie
derive newtype instance readForeignMovie :: ReadForeign Movie

newtype Abc = Abc { _id :: Id Abc, _rev :: Rev Abc, a :: String, b :: Boolean, c :: Array Int }

derive instance newtypeAbc :: Newtype Abc _
derive newtype instance writeForeignAbc :: WriteForeign Abc
derive newtype instance readForeignAbc :: ReadForeign Abc

-- TODO figure out how to properly do setup and teardown, this is all a bit brittle
main :: Effect Unit
main = runTest do
  let juno = {title: "Juno", year: 2007, actors: map wrap ["Ellen Page", "Michael Cera"], sequel: Nothing }
  suite "info" do
    test "local database name" do
      db <- pouchDBLocal { name: "localdatabase"}
      i <- info db
      Assert.equal i.db_name "localdatabase"
  suite "simple read&write" do
    test "write doc response" do
      db <- pouchDBLocal { name: "localdatabase"}
      Movie m <- createDoc db (wrap "juno") juno
      Assert.equal m._id (wrap "juno")
      Assert.equal m.title juno.title
      Assert.equal m.sequel juno.sequel
    test "read written doc" do
      db <- pouchDBLocal { name: "localdatabase"}
      m :: Movie <- getDoc db (wrap "juno")
      Assert.equal (unwrap m)._id (wrap "juno")
      Assert.equal (unwrap m).title juno.title
      Assert.assertFalse "rev empty" ((unwrap m)._rev == wrap "")
    test "save doc" do
      db <- pouchDBLocal { name: "localdatabase"}
      Movie d <- getDoc db (wrap "juno")
      let d' = d {actors = map wrap ["Ellen Page", "Michael Cera", "Olivia Thirlby"]}
      Movie d'' <- saveDoc db (Movie d')
      Movie d''' <- getDoc db (wrap "juno")
      Assert.equal d''.actors d'''.actors
  suite "error cases" do
    test "create doc again conflicts" do
      db <- pouchDBLocal { name: "localdatabase"}
      a :: Either Error Movie <- attempt $ createDoc db (wrap "juno") juno
      case a of
        -- TODO provide a way to deal with (common) errors nicely
        Left e -> Assert.equal ((unsafeCoerce e).name) "conflict"
        Right r -> failure "attempting to write a document twice should fail"
    test "read deleted doc fails" do
      db <- pouchDBLocal { name: "localdatabase"}
      junoDoc :: Movie <- getDoc db (wrap "juno")
      deletedJuno :: Movie <- deleteDoc db junoDoc
      rereadJuno :: Either Error Movie <- attempt $ getDoc db (wrap "juno")
      case rereadJuno of
        Left e -> do
          Assert.equal "not_found" ((unsafeCoerce e).name)
          Assert.equal "deleted" ((unsafeCoerce e).reason)
        Right r -> failure "fetching deleted document should fail"
    test "recreate deleted document succeeds" do
      db <- pouchDBLocal { name: "localdatabase"}
      Movie m <- createDoc db (wrap "juno") juno
      Assert.equal m._id (wrap "juno")
  suite "single-shot replication" do
    test "local/local" do
      source <- pouchDBLocal {name: "localdatabase"}
      target <- pouchDBLocal {name: "localsync"}
      i <- info target
      Assert.equal 0 i.doc_count
      _ <- singleShotReplication source target { checkpoint: Neither }
      i' <- info target
      Assert.equal 1 i'.doc_count
  suite "destroy" do
    test "0 documents after destroy" do
      db <- pouchDBLocal { name: "localdatabase"}
      destroy db
      reopened <- pouchDBLocal { name: "localdatabase"}
      {doc_count} <- info reopened
      Assert.equal 0 doc_count
    test "clean up" do
      db <- pouchDBLocal { name: "localdatabase"}
      destroy db
      db' <- pouchDBLocal { name: "localsync"}
      destroy db'
  suite "bulk" do
    test "bulkGet" do
      db <- pouchDBLocal { name: "localdatabase"}
      a :: Abc <- createDoc db (Id "a") {a: "a", b: false, c: []}
      b :: Abc <- createDoc db (Id "b") {a: "b", b: true, c: [1]}
      c :: Abc <- createDoc db (Id "c") {a: "c", b: false, c: [2, 2]}
      acb :: Array Abc <- bulkGet db [Id "a", Id "c", Id "b"]
      Assert.equal (map (\(Abc {a:a'}) -> a') acb) ["a", "c", "b"]
      destroy db
  suite "changes" do
    test "change event fired within reasonable time" do
      db <- pouchDBLocal { name: "changes"}
      -- TODO this looks like it's broken since updating to Aff v4. If
      -- we comment out the changesLiveSinceNow line, we can put
      -- Assert.equal true false at the end and pulp still reports
      -- passing tests...

      -- This is a bit of a mess. The idea is as follows:
      -- 1. We put a document a
      -- 2. We subscribe to future changes with a handler that writes to an AVar
      -- 3. We put a document b and race that with a delayed put to the AVar directly (fake id, rev, deleted)
      -- 4. We read the AVar and check that we actually got the change for b, not for a, and not the timeout fake
      a :: Abc <- createDoc db (Id "a") {a: "a", b: false, c: []}
      var <- empty
      changesLiveSinceNow db (\r -> void $ runAff (const (pure unit)) (put r var))
      _ <- sequential $ oneOf
             [ parallel (do b :: Abc <- createDoc db (Id "b") {a: "b", b: true, c: [1]}
                            pure unit)
             , parallel (do delay (Milliseconds 50.0)
                            put { id: "nope", rev: "there was a timeout", deleted: false } var)
             ]
      res <- take var
      Assert.equal res.id "b"
      destroy db
