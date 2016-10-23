module Test.Main where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Database.PouchDB.Aff (destroy, info, pouchDB, POUCHDB)
import Prelude (bind, Unit)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main :: forall e. Eff (pouchdb :: POUCHDB, console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR | e) Unit
main = runTest do
  let db = pouchDB "localdatabase"
  suite "info" do
    test "local database name" do
      i <- info db
      Assert.equal i.db_name "localdatabase"
  suite "destroy" do
    test "returns ok:true" do
      r <- destroy (pouchDB "create-and-destroy")
      Assert.assert "not okay" r.ok
