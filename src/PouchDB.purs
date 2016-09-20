module PouchDB where

import Prelude
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Exception (Error)
import Data.Foreign

foreign import data POUCHDB :: !

foreign import data PouchDB :: *

type PouchDoc = Foreign

foreign import pouchDB :: String -> PouchDB

foreign import f_info :: forall e a.
  PouchDB ->
  (Error -> Eff e Unit) ->
  ({db_name :: String, doc_count :: Number, update_seq :: Number} -> Eff a Unit)
  -> Eff a Unit

info :: forall e. PouchDB -> Aff (pouchdb :: POUCHDB | e) {db_name :: String, doc_count :: Number, update_seq :: Number}
info db = makeAff (\e s -> f_info db e s)
