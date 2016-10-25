module Test.Main where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Aff (attempt)
import Database.PouchDB.Aff (POUCHDB, Id, destroy, get, info, pouchDB, put)
import Prelude (bind, Unit, (==), ($))
import Test.Unit (suite, test, success, failure)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Data.Foreign (toForeign)
import Unsafe.Coerce (unsafeCoerce)
import Data.Either (Either (Left, Right))

main :: forall e. Eff (pouchdb :: POUCHDB, console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR | e) Unit
main = runTest do
  let db = pouchDB "localdatabase"
  suite "info" do
    test "local database name" do
      i <- info db
      Assert.equal i.db_name "localdatabase"
  suite "simple read write" do
    test "write doc response" do
      r <- put db (toForeign {_id:"mydoc", title:"test"})
      Assert.equal true r.ok
      Assert.equal (unsafeCoerce r.id) "mydoc"
      Assert.assertFalse "rev empty" ((unsafeCoerce r.rev) == "")
    test "read written doc" do
      d <- get db (unsafeCoerce "mydoc")
      let doc = unsafeCoerce d :: {title :: String, _id :: Id}
      Assert.equal "test" doc.title
      Assert.equal "mydoc" (unsafeCoerce doc._id)
      Assert.assert "ok" true
  suite "write conflict" do
    test "write doc twice conflicts" do
      _ <- put db (toForeign {_id:"doublewrite"})
      a <- attempt $ put db (toForeign {_id:"doublewrite"})
      case a of
        -- TODO parse errors
        Left e -> Assert.equal ((unsafeCoerce e).name) "conflict"
        Right r -> failure "attempting to write a document twice should fail"
  suite "destroy" do
    test "returns ok:true" do
      r <- destroy (pouchDB "create-and-destroy")
      Assert.assert "not okay" r.ok
    test "clean up localdatabase" do
      _ <- destroy db
      success