module Main where

import Prelude
import PouchDB
import Data.Foreign
import Control.Monad.Aff
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, throwException, error, message, try)
import Unsafe.Coerce (unsafeCoerce)

main :: Eff (console :: CONSOLE, err :: EXCEPTION, pouchdb :: POUCHDB) _
main = launchAff $ do
  let db = pouchDB "thedatabase"
  i <- info db
  let foo = {_id: "test5", abc: "cde"}
  let i' = unsafeCoerce i
  log (unsafeCoerce (toForeign foo))
  log i'
  bar <- put db (toForeign foo)
  log (unsafeCoerce bar)
  bar' <- get db (Id "test5")
  log (unsafeCoerce bar')
  test5 <- get db (Id "test5")
  log (unsafeCoerce test5)
  deleted <- remove db test5
  log (unsafeCoerce deleted)
  i' <- info db
  log (unsafeCoerce i')
