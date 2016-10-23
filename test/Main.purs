module Test.Main where

import Database.PouchDB.Aff (info, pouchDB)

import Prelude (bind)
import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert

main = runTest do
  suite "pouchdb" do
    let db = pouchDB "localdatabase"
    test "info" do
      i <- info db
      Assert.equal i.db_name "localdatabase"

-- launchAff $ do
--   let db = pouchDB "thedatabase"
--   i <- info db
--   let foo = {_id: "test5", abc: "cde"}
--   let i' = unsafeCoerce i
--   log (unsafeCoerce (toForeign foo))
--   log i'
--   bar <- put db (toForeign foo)
--   log (unsafeCoerce bar)
--   bar' <- get db (Id "test5")
--   log (unsafeCoerce bar')
--   test5 <- get db (Id "test5")
--   log (unsafeCoerce test5)
--   deleted <- remove db test5
--   log (unsafeCoerce deleted)
--   i' <- info db
--   log (unsafeCoerce i')
