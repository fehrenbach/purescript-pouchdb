module Database.PouchDB where

import Prelude

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Except (runExcept)
import Data.Array (zipWith)
import Data.Either (Either(..), either)
import Data.Foreign (F, Foreign, toForeign)
import Data.Generic (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Record (insert, set)
import Data.Traversable (sequence)
import Database.PouchDB.FFI (POUCHDB, PouchDB)
import Database.PouchDB.FFI as FFI
import Simple.JSON (class ReadForeign, class WriteForeign, read, write)
import Type.Prelude (class RowLacks, SProxy(..))
import Unsafe.Coerce (unsafeCoerce)


newtype Id (d :: Type) = Id String

derive instance newtypeId :: Newtype (Id d) _
derive instance eqId :: Eq (Id d)
derive instance ordId :: Ord (Id d)
derive instance genericId :: Generic (Id d)
derive newtype instance showId :: Show (Id d)
derive newtype instance readForeignId :: ReadForeign (Id d)
derive newtype instance writeForeignId :: WriteForeign (Id d)


newtype Rev (d :: Type) = Rev String

derive instance newtypeRev :: Newtype (Rev d) _
derive instance eqRev :: Eq (Rev d)
derive instance ordRev :: Ord (Rev d)
derive instance genericRev :: Generic (Rev d)
derive newtype instance showRev :: Show (Rev d)
derive newtype instance readForeignRev :: ReadForeign (Rev d)
derive newtype instance writeForeignRev :: WriteForeign (Rev d)


--| Fetch a document by `_id`.
getDoc :: forall doc dat e.
  ReadForeign doc =>
  Newtype doc { _id :: Id doc, _rev :: Rev doc | dat } =>
  PouchDB -> Id doc -> Aff (pouchdb :: POUCHDB | e) doc
getDoc db (Id id) = makeAff (\kE kS ->
  FFI.get db id (toForeign {}) kE (\r -> either (kE <<< error <<< show) kS (runExcept (read r))))


pouchDB :: forall e. String -> Aff (pouchdb :: POUCHDB | e) PouchDB
pouchDB name = liftEff $ FFI.pouchDB (write { name } )


pouchDB' :: forall e. { name :: String, auth :: { username :: String, password :: String } } -> Aff (pouchdb :: POUCHDB | e) PouchDB
pouchDB' options = liftEff $ FFI.pouchDB (write options)


--| Get information about a database
info :: forall e. PouchDB -> Aff (pouchdb :: POUCHDB | e) { db_name :: String, doc_count :: Int, update_seq :: Int }
info db = makeAff (\kE kS -> FFI.info db kE (\r -> either (kE <<< error <<< show) kS (runExcept (read r))))


--| Deletes the database.
--|
--| DO NOT try to use the handle again.
--| You can create a new PouchDB object using the same database name.
destroy :: forall e. PouchDB -> Aff (pouchdb :: POUCHDB | e) Unit
destroy db = makeAff (\kE kS -> FFI.destroy db (toForeign {}) kE (\_ -> kS unit))


--| Save the given proto-document as a new document with the given id.
--|
--| The document doc needs to be a newtype over a with `_id`, `_rev`
--| and some data. The proto-document is just the data record, without
--| `_id` and `_rev` fields.
--|
--| This is the recommended way to generate documents, rather than relying on
--| PouchDB to make up a random document ID for you.
createDoc :: forall doc dat e.
  WriteForeign { _id :: Id doc | dat } =>
  RowLacks "_id" dat =>
  RowLacks "_rev" ( _id :: Id doc | dat ) =>
  Newtype doc { _id :: Id doc, _rev :: Rev doc | dat } =>
  PouchDB -> Id doc -> {| dat} -> Aff (pouchdb :: POUCHDB | e) doc
createDoc db i protoDoc = makeAff (\kE kS ->
  FFI.put db (write doc) (toForeign {}) kE
  (\r -> kS (wrap (insert (SProxy :: SProxy "_rev") (Rev r.rev) doc))))
  where
    doc :: { _id :: Id doc | dat }
    doc = insert (SProxy :: SProxy "_id") i protoDoc

--| Write a (modified) document back to the database.
--|
--| On success, returns the document itself with updated `_rev`.
saveDoc :: forall doc dat e.
  WriteForeign doc =>
  Newtype doc { _id :: Id doc, _rev :: Rev doc | dat } =>
  PouchDB -> doc -> Aff (pouchdb :: POUCHDB | e) doc
saveDoc db doc = makeAff (\kE kS ->
  FFI.put db (write doc) (toForeign {}) kE
  (\r -> kS (wrap (unwrap doc) {_rev = Rev r.rev})))


--| Delete a document from the database
--|
--| Unlike PouchDB's `remove`, this does not clear all fields, but merely sets `_deleted: true`.
deleteDoc :: forall doc dat e.
  WriteForeign { _deleted :: Boolean, _id :: Id doc, _rev :: Rev doc | dat } =>
  RowLacks "_deleted" (_id :: Id doc, _rev :: Rev doc | dat) =>
  Newtype doc { _id :: Id doc, _rev :: Rev doc | dat } =>
  PouchDB -> doc -> Aff (pouchdb :: POUCHDB | e) doc
deleteDoc db doc = makeAff (\kE kS ->
  FFI.put db (write deleted) (toForeign {}) kE
  (\r -> kS (wrap (set (SProxy :: SProxy "_rev") (wrap r.rev) udoc))))
  where
    udoc = unwrap doc
    deleted :: { _deleted :: Boolean, _id :: Id doc, _rev :: Rev doc | dat }
    deleted = insert (SProxy :: SProxy "_deleted") true udoc

--| Fetch multiple documents at once.
--|
--| This will throw the first parse error only.
--|
--| This uses PouchDB's `allDocs` under the hood. It should not be
--| confused with PouchDB's actual `bulkGet` which is used mainly
--| internally for replication.
bulkGet :: forall doc dat e.
  ReadForeign doc =>
  Newtype doc { _id :: Id doc, _rev :: Rev doc | dat } =>
  PouchDB -> Array (Id doc) -> Aff (pouchdb :: POUCHDB | e) (Array doc)
bulkGet db ids = makeAff (\kE kS ->
  FFI.allDocs db (write {keys: ids, include_docs: true}) kE (\r -> either (kE <<< error <<< show) kS (runExcept (read (toForeign r.rows)))))


--| Write multiple (modified) documents back to the database.
--|
--| Note that this is not a transaction. Individual writes might fail but other changes might already have been made.
--| This function will throw (only) the first error, if any.
--|
--| As far as I can tell, PouchDB will increase the revision number even if you write the same exact document that's already there. You might want to avoid that.
bulkSave :: forall dat doc e.
  ReadForeign doc =>
  WriteForeign doc =>
  Newtype doc { _id :: Id doc, _rev :: Rev doc | dat } =>
  PouchDB -> Array doc -> Aff (pouchdb :: POUCHDB | e) (Array doc)
bulkSave db docs = makeAff (\kE kS ->
  FFI.bulkDocs db (map write docs) (toForeign {}) kE (\r -> either kE kS (sequence (zipWith decodeRow docs r))))
  where
    decodeRow :: doc -> Foreign -> Either Error doc
    decodeRow doc f = case runExcept (read f :: F { ok :: Boolean, id :: String, rev :: String }) of
      Right r -> Right (wrap (set (SProxy :: SProxy "_rev") (wrap r.rev) (unwrap doc)))
      -- TODO err here is a parse error
      -- we might want to read f as a proper PouchDB error instead and properly report that
      -- e.g. { status: 409, name: 'conflict', message: 'Document update conflict', error: true }
      Left err -> Left (error (show err))


type ReplicationInfo ext =
  { doc_write_failures :: Int
  , docs_read :: Int
  , docs_written :: Int
  , errors :: Array Foreign
  , last_seq :: Int
  , ok :: Boolean
  , start_time :: Foreign -- TODO I *think* this is a Date. Might want to use purescript-js-date/purescript-datetime.
  | ext }

data ReplicationEvent =
    Change (ReplicationInfo (docs :: Array Foreign))
  | Complete (ReplicationInfo (status :: String))
  | Paused Foreign
  | Active
  | Denied Foreign
  | Error Foreign

--| Start a replication (live, retrying, one way).
--|
--| This sets the `retry` option, to repeatedly try to reconnect on connection loss.
--|
--| TODO: Find a good way to expose the change/paused/error/... events.
--| TODO: Allow cancelling.
startReplication :: forall e.
                    PouchDB ->
                    PouchDB ->
                    (ReplicationEvent -> Eff (pouchdb :: POUCHDB | e) Unit) ->
                    Aff (pouchdb :: POUCHDB | e) Unit
startReplication source target eh = do
  ee <- liftEff $ FFI.replicate source target (write {live: true, retry: true})
  liftEff $ _on ee "change" (\i -> eh (Change (unsafeCoerce i)))
  liftEff $ _on ee "active" (\_ -> eh Active)
  pure unit

-- Register a single-argument event handler on a node-style EventEmitter.
-- ee.on("event", function (ev) { effectful code })
foreign import _on :: forall e.
  Foreign -> -- EventEmitter
  String -> -- event name
  (Foreign -> Eff e Unit) -> -- handler taking 1 argument
  Eff e Unit

-- TODO can we return a value *and* allow cancellation?
--| Single-shot replication from source to target.
--|
--| This blocks execution and only returns when replication is complete or encounters an error.
singleShotReplication :: forall e.
  PouchDB ->
  PouchDB ->
  Aff (pouchdb :: POUCHDB | e) (ReplicationInfo (end_time :: Foreign, status :: String))
singleShotReplication source target = makeAff (\kE kS ->
  -- For now (no cancellation) return the actual value (for debugging purposes)
  FFI.replicateTo source target (toForeign {}) kE (\r -> kS (unsafeCoerce r)))


--| Simple keys query, no reduce, include docs.
--|
--| Throws the first parse error only (if any).
viewKeysInclude :: forall l doc dat k v e.
  ReadForeign { doc :: doc, id :: Id l, key :: k, value :: v } =>
  WriteForeign k =>
  Newtype doc { _id :: Id doc, _rev :: Rev doc | dat } =>
  PouchDB -> String -> Array k -> Aff (pouchdb :: POUCHDB | e) (Array { doc :: doc, id :: Id l, key :: k, value :: v })
viewKeysInclude db view keys = makeAff (\kE kS ->
  FFI.query db (write view) (write { keys, reduce: false, include_docs: true }) kE
    (\r -> either (kE <<< error <<< show) kS (runExcept (read (toForeign r.rows)))))


--| Fetch all docs between startkey and endkey (inclusive)
--|
--| Consider using `docIdStartsWith` when looking for doc ids starting with some string.
allDocsRange :: forall l dat doc k v e. -- I think k and/or v might be restricted to be the key of the document, but I'm not sure
  ReadForeign { doc :: doc, id :: Id l, key :: k, value :: v } =>
  Newtype doc { _id :: Id doc, _rev :: Rev doc | dat } =>
  PouchDB -> { startkey :: String, endkey :: String } -> Aff (pouchdb :: POUCHDB | e) (Array { doc :: doc, id :: Id l, key :: k, value :: v })
allDocsRange db {startkey, endkey} = makeAff (\kE kS ->
  FFI.allDocs db (write { startkey, endkey, include_docs: true }) kE
    (\r -> either (kE <<< error <<< show) kS (runExcept (read (toForeign r.rows)))))


--| Construct a {startkey, endkey} record for range queries that should
--| match everything that starts with a given string. As recommended in
--| in the CouchDB docs, we just append the special character \uFFF0 to
--| the string to obtain the endkey.
--| (This means it doesn't work if you use that in your doc ids/keys!)
--| https://wiki.apache.org/couchdb/View_collation#String_Ranges
docIdStartsWith :: String -> { startkey :: String, endkey :: String }
docIdStartsWith s = { startkey: s, endkey: s <> "￰" }
