module Main where

import Prelude
import PouchDB (info, pouchDB, PouchDB, POUCHDB, PouchDoc)
import Control.Monad.Aff
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, throwException, error, message, try)
import Unsafe.Coerce (unsafeCoerce)

main :: Eff (console :: CONSOLE, err :: EXCEPTION, pouchdb :: POUCHDB) _
main = launchAff $ do
  let db = pouchDB "thedatabase"
  i <- info db
  let i' = unsafeCoerce i
  log i'
