module Database.PouchDB where

import Prelude

import Control.Monad.Aff (Aff, throwError)
import Control.Monad.Aff.Compat (fromEffFnAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Except (runExcept)
import Data.Array (zipWith)
import Data.Either (Either(..), either)
import Data.Foreign (F, Foreign, readBoolean, readNullOrUndefined, readString, toForeign)
import Data.Foreign.Index ((!))
import Data.Generic (class Generic)
import Data.Maybe (maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Record (insert, set)
import Data.Traversable (sequence, traverse)
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

-- I feel like this should be in some library somewhere
class Subrow (r :: # Type) (s :: # Type)
instance subrow :: Union r t s => Subrow r s

data Adapter = Idb | Leveldb | Websql | Http

instance writeForeignAdapter :: WriteForeign Adapter where
  writeImpl Idb = write "idb"
  writeImpl Leveldb = write "leveldb"
  writeImpl Websql = write "websql"
  writeImpl Http = write "http"

--| Create or open a local database
--|
--| For example, the follwing opens (and creates if it does not exist) the local database "movies" with automatic compaction enabled:
--| ```purescript
--| do moviedb <- pouchDBLocal { name: "movies", auto_compaction: true }
--|    -- do something with your database
--| ```
--|
--| `name` is required, for the other options, see https://pouchdb.com/api.html#create_database
pouchDBLocal :: forall e options.
  WriteForeign { name :: String | options } =>
  Subrow options (adapter :: Adapter, auto_compaction :: Boolean, revs_limit :: Int) =>
  { name :: String | options } -> Aff (pouchdb :: POUCHDB | e) PouchDB
pouchDBLocal options = liftEff $ FFI.pouchDB (write options)

--| Create or open a remote database
--|
--| For example, the follwing opens a connection to the "query-movies" database on cloudant.com without trying to create it should it not exist already (`skip_setup`):
--| ```purescript
--| do moviedb <- pouchDBRemote { name: "https://examples.cloudant.com/query-movies", skip_setup: true }
--|    -- do something with your database
--| ```
--|
--| `name` is required, for the other options, see https://pouchdb.com/api.html#create_database
pouchDBRemote :: forall e options.
  WriteForeign { name :: String | options } =>
  Subrow options ( ajax :: { cache :: Boolean, timeout :: Int, withCredentials :: Boolean }
                 , auth :: { username :: String, password :: String }
                 , skip_setup :: Boolean) =>
  { name :: String | options } -> Aff (pouchdb :: POUCHDB | e) PouchDB
pouchDBRemote options = liftEff $ FFI.pouchDB (write options)


--| Fetch a document by `_id`.
getDoc :: forall doc dat e.
  ReadForeign doc =>
  Newtype doc { _id :: Id doc, _rev :: Rev doc | dat } =>
  PouchDB -> Id doc -> Aff (pouchdb :: POUCHDB | e) doc
getDoc db (Id id) = do
  r <- fromEffFnAff (FFI.get db id (toForeign {}))
  -- eh, not really happy with this error handling...
  either (throwError <<< error <<< show) pure (runExcept (read r))


--| Get information about a database
info :: forall e. PouchDB -> Aff (pouchdb :: POUCHDB | e) { db_name :: String, doc_count :: Int, update_seq :: Int }
info db = do
  r <- fromEffFnAff (FFI.info db)
  either (throwError <<< error <<< show) pure (runExcept (read r))


--| Deletes the database.
--|
--| DO NOT try to use the handle again.
--| You can create a new PouchDB object using the same database name.
destroy :: forall e. PouchDB -> Aff (pouchdb :: POUCHDB | e) Unit
destroy db = void (fromEffFnAff (FFI.destroy db (toForeign {})))


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
createDoc db i protoDoc = do
  r <- fromEffFnAff (FFI.put db (write doc) (toForeign {}))
  pure (wrap (insert (SProxy :: SProxy "_rev") (Rev r.rev) doc))
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
saveDoc db doc = do
  r <- fromEffFnAff (FFI.put db (write doc) (toForeign {}))
  pure (wrap (unwrap doc) {_rev = Rev r.rev})


--| Delete a document from the database
--|
--| Unlike PouchDB's `remove`, this does not clear all fields, but merely sets `_deleted: true`.
deleteDoc :: forall doc dat e.
  WriteForeign { _deleted :: Boolean, _id :: Id doc, _rev :: Rev doc | dat } =>
  RowLacks "_deleted" (_id :: Id doc, _rev :: Rev doc | dat) =>
  Newtype doc { _id :: Id doc, _rev :: Rev doc | dat } =>
  PouchDB -> doc -> Aff (pouchdb :: POUCHDB | e) doc
deleteDoc db doc = do
  r <- fromEffFnAff (FFI.put db (write deleted) (toForeign {}))
  pure (wrap (set (SProxy :: SProxy "_rev") (wrap r.rev) udoc))
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
bulkGet db ids = do
  r <- fromEffFnAff (FFI.allDocs db (write {keys: ids, include_docs: true}))
  either (throwError <<< error <<< show) pure (runExcept (traverse (read <<< _.doc <<< unsafeCoerce) r.rows))


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
bulkSave db docs = do
  r <- fromEffFnAff (FFI.bulkDocs db (map write docs) (toForeign {}))
  either throwError pure (sequence (zipWith decodeRow docs r))
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
  | Paused Foreign -- should this be (NullOrUndefined Error)?
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
  liftEff $ _on ee "active" (\_ -> eh Active)
  liftEff $ _on ee "change" (\i -> eh (Change (unsafeCoerce i)))
  liftEff $ _on ee "complete" (\i -> eh (Complete (unsafeCoerce i)))
  liftEff $ _on ee "denied" (\f -> eh (Denied f))
  liftEff $ _on ee "error" (\e -> eh (Error e))
  liftEff $ _on ee "paused" (\e -> eh (Paused e))
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
singleShotReplication source target = do
  r <- fromEffFnAff (FFI.replicateTo source target (toForeign {}))
  pure (unsafeCoerce r)


--| Simple keys query, no reduce, no docs included.
viewKeys :: forall e l k v.
  ReadForeign { id :: Id l, key :: k, value :: v } =>
  WriteForeign k =>
  -- Should this constrain the type variable l?
  PouchDB -> String -> Array k -> Aff (pouchdb :: POUCHDB | e) (Array { id :: Id l, key :: k, value :: v })
viewKeys db view keys = do
  r <- fromEffFnAff (FFI.query db (write view) (write { keys, reduce: false }))
  either (throwError <<< error <<< show) pure (runExcept (read (toForeign r.rows)))


--| Simple keys query, no reduce, include docs.
--|
--| Throws the first parse error only (if any).
viewKeysInclude :: forall l doc dat k v e.
  ReadForeign { doc :: doc, id :: Id l, key :: k, value :: v } =>
  WriteForeign k =>
  Newtype doc { _id :: Id doc, _rev :: Rev doc | dat } =>
  PouchDB -> String -> Array k -> Aff (pouchdb :: POUCHDB | e) (Array { doc :: doc, id :: Id l, key :: k, value :: v })
viewKeysInclude db view keys = do
    r <- fromEffFnAff (FFI.query db (write view) (write { keys, reduce: false, include_docs: true }))
    either (throwError <<< error <<< show) pure (runExcept (read (toForeign r.rows)))


--| Simple keys query, no reduce, only return docs.
--|
--| This is like `viewKeysInclude`, except that you only get the `doc`s.
--|
--| This is useful to look up a bunch of documents by a secondary key.
--| Something like `bulkGet`, except for a different key than `_id`.
viewKeysDoc :: forall doc dat k e.
  ReadForeign doc =>
  WriteForeign k =>
  Newtype doc { _id :: Id doc, _rev :: Rev doc | dat } =>
  PouchDB -> String -> Array k -> Aff (pouchdb :: POUCHDB | e) (Array doc)
viewKeysDoc db view keys = do
    r <- fromEffFnAff (FFI.query db (write view) (write { keys, reduce: false, include_docs: true }))
    either (throwError <<< error <<< show) pure (runExcept (read (toForeign (docs r))))
  where
    docs :: { offset :: Int, rows :: Array Foreign, total_rows :: Int } -> Array Foreign
    docs r = map _.doc (unsafeCoerce r.rows) -- TODO we could do this with a destructive map


--| Range query with limit, no reduce, no docs.
viewRangeLimit :: forall e k l v.
  ReadForeign k =>
  WriteForeign k =>
  ReadForeign v =>
  -- Should this constrain the type variable l?
  PouchDB -> String -> {startkey :: k, endkey :: k} -> Int -> Aff (pouchdb :: POUCHDB | e) (Array { id :: Id l, key :: k, value :: v })
viewRangeLimit db view {startkey, endkey} limit = do
  r <- fromEffFnAff (FFI.query db (write view) (write { startkey, endkey, limit, reduce: false }))
  either (throwError <<< error <<< show) pure (runExcept (read (toForeign r.rows)))


--| Fetch all docs between startkey and endkey (inclusive)
--|
--| Consider using `rangeFromPrefix` when looking for doc ids starting with some string.
allDocsRange :: forall dat doc e.
  ReadForeign doc =>
  Newtype doc { _id :: Id doc, _rev :: Rev doc | dat } =>
  PouchDB -> { startkey :: String, endkey :: String } -> Aff (pouchdb :: POUCHDB | e) (Array doc)
allDocsRange db {startkey, endkey} = do
  r <- fromEffFnAff (FFI.allDocs db (write { startkey, endkey, include_docs: true }))
  either (throwError <<< error <<< show) pure (runExcept (traverse (read <<< _.doc <<< unsafeCoerce) r.rows))


--| Construct a {startkey, endkey} record for range queries that should
--| match everything that starts with a given string. As recommended in
--| in the CouchDB docs, we just append the special character \uFFF0 to
--| the string to obtain the endkey.
--| (This means it doesn't work if you use that in your doc ids/keys!)
--| https://wiki.apache.org/couchdb/View_collation#String_Ranges
rangeFromPrefix :: String -> { startkey :: String, endkey :: String }
rangeFromPrefix s = { startkey: s, endkey: s <> "￰" }


--| Reduce query over a range of keys, with group_level.
--|
--| The types here are less than ideal, because we can have compound keys with different types, e.g. movie Id (as string) and year (as Int).
viewRangeGroupLevel :: forall e k v. -- NOTE: We could probably solve the weak typing issues using HLists
  WriteForeign k =>
  ReadForeign k =>
  ReadForeign v =>
  PouchDB -> String -> { startkey :: Array k, endkey :: Array k } -> Int -> Aff (pouchdb :: POUCHDB | e) (Array { key :: Array k, value :: v })
viewRangeGroupLevel db view { startkey, endkey } group_level = do
  r <- fromEffFnAff (FFI.query db (write view) (write { startkey, endkey, group_level }))
  either (throwError <<< error <<< show) pure (runExcept (read (toForeign r.rows)))


--| Subscribe to future changes
changesLiveSinceNow :: forall e.
  PouchDB -> ({ id :: String, rev :: String, deleted :: Boolean } -> Eff (pouchdb :: POUCHDB | e) Unit) -> Aff (pouchdb :: POUCHDB | e) Unit
changesLiveSinceNow db handler = liftEff $ do
  c <- FFI.changes db (write { live: true, since: "now" })
  liftEff $ _on c "change" changeHandler
  -- TODO on "error" handler?
  -- TODO cancel?
  pure unit
  where
    changeHandler :: Foreign -> Eff (pouchdb :: POUCHDB | e) Unit
    changeHandler f = case runExcept (parse f) of
      Left _ -> pure unit -- TODO error handler?
      Right r -> handler r
    parse f = do
      id <- f ! "id" >>= readString
      rev <- f ! "changes" ! 0 ! "rev" >>= readString
      maybeDeleted <- f ! "deleted" >>= readNullOrUndefined
      deleted <- maybe (pure false) readBoolean maybeDeleted
      pure { id, rev, deleted }
