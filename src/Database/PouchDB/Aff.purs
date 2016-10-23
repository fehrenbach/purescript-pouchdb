module Database.PouchDB.Aff where

import Prelude
import Database.PouchDB.FFI (PouchDB)
import Database.PouchDB.FFI as FFI
import Control.Monad.Aff (Aff, makeAff)
import Data.Foreign (Foreign, unsafeFromForeign, writeObject)

foreign import data POUCHDB :: !

-- type Document = Foreign

newtype Id = Id String
newtype Rev = Rev String

empty :: Foreign
empty = writeObject []

foreignToInfo :: Foreign -> {db_name :: String, doc_count :: Number, update_seq :: Number}
foreignToInfo f = unsafeFromForeign f

info :: forall e. PouchDB -> Aff (pouchdb :: POUCHDB | e) {db_name :: String, doc_count :: Number, update_seq :: Number}
info db = makeAff (\e s -> FFI.info db e (s <<< foreignToInfo))

pouchDB :: String -> PouchDB
pouchDB name = FFI.pouchDB name empty
