module Test.Main where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, fromObject, toObject, jsonEmptyObject, (.?), (:=), (~>))
import Data.Either (Either(..))
import Data.Foreign (ForeignError(ForeignError), fail, writeObject)
import Data.Foreign.Class (class AsForeign, class IsForeign, readProp, (.=))
import Database.PouchDB.Aff (Document (..), destroy, get, info, pouchDB, create)
import Database.PouchDB.FFI (POUCHDB)
import Prelude
import Test.Unit (suite, test, success)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Unsafe.Coerce (unsafeCoerce)

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

instance decodeJsonMovie :: DecodeJson Movie where
  decodeJson json = do
    obj <- decodeJson json
    -- typ <- obj .? "type"
    title <- obj .? "title"
    year <- obj .? "year"
    actors <- obj .? "actors"
    -- TODO should probably do that first
--    if (typ /= "movie")
--      then Left (ForeignError "Try to parse doc with type != 'movie' as Movie")
    pure $ Movie {title, year, actors}

main :: forall e. Eff (pouchdb :: POUCHDB, console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR | e) Unit
main = runTest do
  suite "info" do
    test "local database name" do
      db <- pouchDB "localdatabase"
      i <- info db
      Assert.equal i.db_name "localdatabase"
  suite "simple read write" do
    test "write doc response" do
      db <- pouchDB "localdatabase"
      r <- create db (unsafeCoerce "juno") (Movie {title: "Juno", year: 2007, actors: ["Ellen Page", "Michael Cera"]})
      log (unsafeCoerce r)
      -- Assert.equal true r.ok
      -- Assert.equal (unsafeCoerce r.id) "mydoc"
      -- Assert.assertFalse "rev empty" ((unsafeCoerce r.rev) == "")
    test "read written doc" do
      db <- pouchDB "localdatabase"
      doc@(Document id rev (m :: Movie)) <- get db (unsafeCoerce "juno")
      log (unsafeCoerce doc)
      log (unsafeCoerce m)
  -- suite "write conflict" do
  --   test "write doc twice conflicts" do
  --     _ <- put db (toForeign {_id:"doublewrite"})
  --     a <- attempt $ put db (toForeign {_id:"doublewrite"})
  --     case a of
  --       -- TODO parse errors
  --       Left e -> Assert.equal ((unsafeCoerce e).name) "conflict"
  --       Right r -> failure "attempting to write a document twice should fail"
  suite "destroy" do
    test "clean up localdatabase" do
      db <- pouchDB "localdatabase"
      destroy db
      -- TODO check that it's actually gone (recreated with 0 docs or something)
      success
