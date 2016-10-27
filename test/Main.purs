module Test.Main where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Aff.Console (log)
import Database.PouchDB.Aff (POUCHDB, Document (..), destroy, get, info, pouchDB, create)
import Prelude
import Test.Unit (suite, test, success)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Unsafe.Coerce (unsafeCoerce)
import Data.Foreign (ForeignError(ForeignError), fail, writeObject)
import Data.Foreign.Class (class AsForeign, class IsForeign, readProp, (.=))

data Movie = Movie { title :: String,
                     year :: Int,
                     actors :: Array String }

instance asForeignMovie :: AsForeign Movie where
  -- TODO if this works, clean it up a bit
  write (Movie {title, year, actors}) =
    writeObject [ "type" .= "movie"
                , "title" .= title
                , "year" .= year
                , "actors" .= actors ]

instance isForeignMovie :: IsForeign Movie where
  read value = do
    typ :: String <- readProp "type" value
    title <- readProp "title" value
    year <- readProp "year" value
    actors <- readProp "actors" value
    -- TODO should probably do that first
    if (typ /= "movie")
      then fail (ForeignError "Try to parse doc with type != 'movie' as Movie")
      else pure $ Movie {title, year, actors}

main :: forall e. Eff (pouchdb :: POUCHDB, console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR | e) Unit
main = runTest do
  let db = pouchDB "localdatabase"
  suite "info" do
    test "local database name" do
      i <- info db
      Assert.equal i.db_name "localdatabase"
  suite "simple read write" do
    test "write doc response" do
      r <- create db (unsafeCoerce "juno") (Movie { title: "Juno", year: 2007, actors: ["Ellen Page", "Michael Cera"]})
      log (unsafeCoerce r)
      -- Assert.equal true r.ok
      -- Assert.equal (unsafeCoerce r.id) "mydoc"
      -- Assert.assertFalse "rev empty" ((unsafeCoerce r.rev) == "")
    test "read written doc" do
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
    test "returns ok:true" do
      r <- destroy (pouchDB "create-and-destroy")
      Assert.assert "not okay" r.ok
    test "clean up localdatabase" do
      _ <- destroy db
      success