module Database.PouchDB.Aff where

import Prelude
import Database.PouchDB.FFI (PouchDB)
import Database.PouchDB.FFI as FFI
import Control.Monad.Aff (Aff, makeAff)
import Data.Foreign (Foreign, unsafeFromForeign, writeObject)

foreign import data POUCHDB :: !

type Document = Foreign

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

--| Deletes the database.
--|
--| You should not try to use the handle again.
--| If you need a new database with the same name, call `pouchDB` again.
destroy :: forall e. PouchDB -> Aff (pouchdb :: POUCHDB | e) {ok :: Boolean}
destroy db = makeAff (\e s -> FFI.destroy db empty e (s <<< unsafeFromForeign))

--| Create or update document
--|
--| The document needs to have a field `_id`.
--| For updating an existing document, it also has to have the current `_rev`.
put :: forall e. PouchDB -> Document -> Aff (pouchdb :: POUCHDB | e) {ok :: Boolean, id :: Id , rev :: Rev}
put db doc = makeAff (\e s -> FFI.put db doc empty e (s <<< unsafeFromForeign))

get :: forall e. PouchDB -> Id -> Aff (pouchdb :: POUCHDB | e) Document
get db (Id id) = makeAff (\e s -> FFI.get db id empty e s)