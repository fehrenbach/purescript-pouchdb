module Test.Main where

import Prelude

import Control.Monad.Aff (attempt)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (Error)
import Data.Either (Either(..))
import Data.Newtype (wrap)
import Database.PouchDB (Document, Id, createDoc, deleteDoc, destroy, getDoc, info, pouchDB, saveDoc, singleShotReplication)
import Database.PouchDB.FFI (POUCHDB)
import Test.Unit (failure, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Unsafe.Coerce (unsafeCoerce)

type Actor = (name :: String)

type Movie = (title :: String, year :: Int, actors :: Array (Id Actor))


-- juno :: forall e. PouchDB -> Aff (pouchdb :: POUCHDB | e) (Document Movie)
-- juno db = do ellenPage <- createDoc db (Id "ellen_page") { name: "Ellen Page" }
--              createDoc db (Id "movie:juno") { name: "Juno", actors: [ ellenPage._id ] }

main :: forall e. Eff (pouchdb :: POUCHDB, console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR | e) Unit
main = runTest do
  let juno = {title: "Juno", year: 2007, actors: ["Ellen Page", "Michael Cera"]}
  suite "info" do
    test "local database name" do
      db <- pouchDB "localdatabase"
      i <- info db
      Assert.equal i.db_name "localdatabase"
  suite "simple read&write" do
    test "write doc response" do
      db <- pouchDB "localdatabase"
      m <- createDoc db (wrap "juno") juno
      Assert.equal m._id (wrap "juno")
      Assert.equal m.title juno.title
    test "read written doc" do
      db <- pouchDB "localdatabase"
      m :: Document Movie <- getDoc db (wrap "juno")
      Assert.equal m._id (wrap "juno")
      Assert.equal m.title juno.title
      Assert.assertFalse "rev empty" (m._rev == wrap "")
    test "save doc" do
      db <- pouchDB "localdatabase"
      d :: Document Movie <- getDoc db (wrap "juno")
      let d' = d {actors = map wrap ["Ellen Page", "Michael Cera", "Olivia Thirlby"]}
      d'' <- saveDoc db d'
      d''' :: Document Movie <- getDoc db (wrap "juno")
      Assert.equal d''.actors d'''.actors
  suite "error cases" do
    test "create doc again conflicts" do
      db <- pouchDB "localdatabase"
      a <- attempt $ createDoc db (wrap "juno") juno
      case a of
        -- TODO provide a way to deal with (common) errors nicely
        Left e -> Assert.equal ((unsafeCoerce e).name) "conflict"
        Right r -> failure "attempting to write a document twice should fail"
    test "read deleted doc fails" do
      db <- pouchDB "localdatabase"
      junoDoc :: Document Movie <- getDoc db (wrap "juno")
      deletedJuno :: Document Movie <- deleteDoc db junoDoc
      rereadJuno :: Either Error (Document Movie) <- attempt $ getDoc db (wrap "juno")
      case rereadJuno of
        Left e -> do
          Assert.equal "not_found" ((unsafeCoerce e).name)
          Assert.equal "deleted" ((unsafeCoerce e).reason)
        Right r -> failure "fetching deleted document should fail"
    test "recreate deleted document succeeds" do
      db <- pouchDB "localdatabase"
      m <- createDoc db (wrap "juno") juno
      Assert.equal m._id (wrap "juno")
  suite "single-shot replication" do
    test "local/local" do
      source <- pouchDB "localdatabase"
      target <- pouchDB "localsync"
      i <- info target
      Assert.equal 0 i.doc_count
      _ <- singleShotReplication source target
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
