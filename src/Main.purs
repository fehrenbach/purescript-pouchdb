module Main where

import Prelude
import PouchDB (info, pouchDB, PouchDB, PouchDoc)
import Control.Monad.Aff
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, throwException, error, message, try)
import Unsafe.Coerce (unsafeCoerce)

info' :: forall e. PouchDB -> Aff e PouchDoc
info' db = makeAff (\e s -> info db e s)

main = launchAff do
  let db = pouchDB "thedatabase"
  i <- info' db
  let i' = unsafeCoerce i
  log i'

