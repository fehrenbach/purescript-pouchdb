module Database.PouchDB where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Data.Array (zipWith)
import Data.Either (Either(..), either)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Traversable (sequence, traverse)
import Database.PouchDB.FFI (PouchDB)
import Database.PouchDB.FFI as FFI
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (fromEffectFnAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)
import Foreign (Foreign, ForeignError, readBoolean, readNullOrUndefined, readString, unsafeToForeign)
import Foreign.Index ((!))
import Prim.Row (class Lacks, class Union)
import Record (insert, set)
import Simple.JSON (class ReadForeign, class WriteForeign, read, write)
import Type.Prelude (SProxy(..))
import Unsafe.Coerce (unsafeCoerce)


newtype Id (d :: Type) = Id String

derive instance newtypeId :: Newtype (Id d) _
derive instance eqId :: Eq (Id d)
derive instance ordId :: Ord (Id d)
derive newtype instance showId :: Show (Id d)
derive newtype instance readForeignId :: ReadForeign (Id d)
derive newtype instance writeForeignId :: WriteForeign (Id d)


newtype Rev (d :: Type) = Rev String

derive instance newtypeRev :: Newtype (Rev d) _
derive instance eqRev :: Eq (Rev d)
derive instance ordRev :: Ord (Rev d)
derive newtype instance showRev :: Show (Rev d)
derive newtype instance readForeignRev :: ReadForeign (Rev d)
derive newtype instance writeForeignRev :: WriteForeign (Rev d)

-- I feel like this should be in some library somewhere
class Subrow (r :: # Type) (s :: # Type)
instance subrow :: Union r t s => Subrow r s

-- | Create or open a local database
-- |
-- | For example, the follwing opens (and creates if it does not exist) the local database "movies" with automatic compaction enabled:
-- | ```purescript
-- | do moviedb <- pouchDBLocal { name: "movies", auto_compaction: true }
-- |    -- do something with your database
-- | ```
-- |
-- | `name` is required, for the other options, see https://pouchdb.com/api.html#create_database
pouchDBLocal :: forall options.
  WriteForeign { name :: String | options } =>
  Subrow options (adapter :: String, auto_compaction :: Boolean, revs_limit :: Int) =>
  { name :: String | options } -> Aff PouchDB
pouchDBLocal options = fromEffectFnAff (FFI.pouchDB (write options))

-- | Create or open a remote database
-- |
-- | For example, the follwing opens a connection to the "query-movies" database on cloudant.com without trying to create it should it not exist already (`skip_setup`):
-- | ```purescript
-- | do moviedb <- pouchDBRemote { name: "https://examples.cloudant.com/query-movies", skip_setup: true }
-- |    -- do something with your database
-- | ```
-- |
-- | `name` is required, for the other options, see https://pouchdb.com/api.html#create_database
pouchDBRemote :: forall options.
  WriteForeign { name :: String | options } =>
  Subrow options ( ajax :: { cache :: Boolean, timeout :: Int, withCredentials :: Boolean }
                 , auth :: { username :: String, password :: String }
                 , skip_setup :: Boolean) =>
  { name :: String | options } -> Aff PouchDB
pouchDBRemote options = fromEffectFnAff (FFI.pouchDB (write options))


-- | Fetch a document by `_id`.
getDoc :: forall doc dat.
  ReadForeign doc =>
  Newtype doc { _id :: Id doc, _rev :: Rev doc | dat } =>
  PouchDB -> Id doc -> Aff doc
getDoc db (Id id) = do
  r <- fromEffectFnAff (FFI.get db id (unsafeToForeign {}))
  -- eh, not really happy with this error handling...
  either (throwError <<< error <<< show) pure (read r)


-- | Get information about a database
info :: PouchDB -> Aff { db_name :: String, doc_count :: Int, update_seq :: Int }
info db = do
  r <- fromEffectFnAff (FFI.info db)
  either (throwError <<< error <<< show) pure (read r)


-- | Deletes the database.
-- |
-- | DO NOT try to use the handle again.
-- | You can create a new PouchDB object using the same database name.
destroy :: PouchDB -> Aff Unit
destroy db = void (fromEffectFnAff (FFI.destroy db (unsafeToForeign {})))


-- | Save the given proto-document as a new document with the given id.
-- |
-- | The document doc needs to be a newtype over a with `_id`, `_rev`
-- | and some data. The proto-document is just the data record, without
-- | `_id` and `_rev` fields.
-- |
-- | This is the recommended way to generate documents, rather than relying on
-- | PouchDB to make up a random document ID for you.
createDoc :: forall doc dat.
  WriteForeign { _id :: Id doc | dat } =>
  Lacks "_id" dat =>
  Lacks "_rev" dat =>
  Newtype doc { _id :: Id doc, _rev :: Rev doc | dat } =>
  PouchDB -> Id doc -> {| dat} -> Aff doc
createDoc db i protoDoc = do
  r <- fromEffectFnAff (FFI.put db (write doc) (unsafeToForeign {}))
  pure (wrap (insert (SProxy :: SProxy "_rev") (Rev r.rev) doc))
  where
    doc :: { _id :: Id doc | dat }
    doc = insert (SProxy :: SProxy "_id") i protoDoc


-- | Write a (modified) document back to the database.
-- |
-- | On success, returns the document itself with updated `_rev`.
saveDoc :: forall doc dat.
  WriteForeign doc =>
  Newtype doc { _id :: Id doc, _rev :: Rev doc | dat } =>
  PouchDB -> doc -> Aff doc
saveDoc db doc = do
  r <- fromEffectFnAff (FFI.put db (write doc) (unsafeToForeign {}))
  pure (wrap (unwrap doc) {_rev = Rev r.rev})


-- | Delete a document from the database
-- |
-- | Unlike PouchDB's `remove`, this does not clear all fields, but merely sets `_deleted: true`.
deleteDoc :: forall doc dat.
  WriteForeign { _deleted :: Boolean, _id :: Id doc, _rev :: Rev doc | dat } =>
  Lacks "_deleted" dat =>
  Newtype doc { _id :: Id doc, _rev :: Rev doc | dat } =>
  PouchDB -> doc -> Aff doc
deleteDoc db doc = do
  r <- fromEffectFnAff (FFI.put db (write deleted) (unsafeToForeign {}))
  pure (wrap (set (SProxy :: SProxy "_rev") (wrap r.rev) udoc))
  where
    udoc = unwrap doc
    deleted :: { _deleted :: Boolean, _id :: Id doc, _rev :: Rev doc | dat }
    deleted = insert (SProxy :: SProxy "_deleted") true udoc


-- | Fetch multiple documents at once.
-- |
-- | This will throw the first parse error only.
-- |
-- | This uses PouchDB's `allDocs` under the hood. It should not be
-- | confused with PouchDB's actual `bulkGet` which is used mainly
-- | internally for replication.
bulkGet :: forall doc dat.
  ReadForeign doc =>
  Newtype doc { _id :: Id doc, _rev :: Rev doc | dat } =>
  PouchDB -> Array (Id doc) -> Aff (Array doc)
bulkGet db ids = do
  r <- fromEffectFnAff (FFI.allDocs db (write {keys: ids, include_docs: true}))
  either (throwError <<< error <<< show) pure (traverse (read <<< _.doc <<< unsafeCoerce) r.rows)


-- | Write multiple (modified) documents back to the database.
-- |
-- | Note that this is not a transaction. Individual writes might fail but other changes might already have been made.
-- | This function will throw (only) the first error, if any.
-- |
-- | As far as I can tell, PouchDB will increase the revision number even if you write the same exact document that's already there. You might want to avoid that.
bulkSave :: forall dat doc.
  ReadForeign doc =>
  WriteForeign doc =>
  Newtype doc { _id :: Id doc, _rev :: Rev doc | dat } =>
  PouchDB -> Array doc -> Aff (Array doc)
bulkSave db docs = do
  r <- fromEffectFnAff (FFI.bulkDocs db (map write docs) (unsafeToForeign {}))
  either throwError pure (sequence (zipWith decodeRow docs r))
  where
    decodeRow :: doc -> Foreign -> Either Error doc
    decodeRow doc f = case read f :: Either (NonEmptyList ForeignError) { ok :: Boolean, id :: String, rev :: String } of
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

-- TODO these could be more precise, I think
data ReplicationEvent =
    Change (ReplicationInfo (docs :: Array Foreign))
  | Complete (ReplicationInfo (status :: String))
  | Paused Foreign -- should this be (NullOrUndefined Error)?
  | Active
  | Denied Foreign
  | Error Foreign

data Checkpoint = SourceOnly | TargetOnly | Neither

instance writeForeignCheckpoint :: WriteForeign Checkpoint where
  writeImpl SourceOnly = write "source"
  writeImpl TargetOnly = write "target"
  writeImpl Neither = write false


-- Should we pass a record of event handlers instead of this ReplicationEvent datatype thing?
-- | Start a continous (live) replication from source to target.
-- |
-- | This sets the `live` option to `true`. For an explanation of the other options see https://pouchdb.com/api.html#replication
-- |
-- | The event handler will be called with replication events as they happen.
-- |
-- | Returns an effect with which you can cancel the replication.
startReplication :: forall options.
  WriteForeign { live :: Boolean | options } =>
  Subrow options ( retry :: Boolean
                 , checkpoint :: Checkpoint
                 , batch_size :: Int
                 , batches_limit :: Int ) =>
  Lacks "live" options =>
  PouchDB -> PouchDB -> {| options} -> (ReplicationEvent -> Effect Unit) -> Effect (Effect Unit)
startReplication source target options eh = do
  ee <- FFI.replicate source target (write o)
  _on ee "active" (\_ -> eh Active)
  _on ee "change" (\i -> eh (Change (unsafeCoerce i)))
  _on ee "complete" (\i -> eh (Complete (unsafeCoerce i)))
  _on ee "denied" (\f -> eh (Denied f))
  _on ee "error" (\e -> eh (Error e))
  _on ee "paused" (\e -> eh (Paused e))
  pure (_cancel ee)
  where o :: { live :: Boolean | options }
        o = insert (SProxy :: SProxy "live") true options


-- Register a single-argument event handler on a node-style EventEmitter.
-- ee.on("event", function (ev) { effectful code })
foreign import _on ::
  Foreign -> -- EventEmitter
  String -> -- event name
  (Foreign -> Effect Unit) -> -- handler taking 1 argument
  Effect Unit

-- Call .cancel() on a replication object
foreign import _cancel ::
  Foreign -> Effect Unit


-- TODO can we return a value *and* allow cancellation?
-- | Single-shot replication from source to target.
-- |
-- | This is not `live` replication, and thus does not accept the `retry` option. For other options, see https://pouchdb.com/api.html#replication
-- |
-- | This "blocks execution" and only returns when replication is complete or encounters an error.
singleShotReplication :: forall options.
  WriteForeign { | options } =>
  Subrow options ( checkpoint :: Checkpoint
                 , batch_size :: Int
                 , batches_limit :: Int ) =>
  PouchDB ->
  PouchDB ->
  { | options } ->
  Aff (ReplicationInfo (end_time :: Foreign, status :: String))
singleShotReplication source target options = do
  r <- fromEffectFnAff (FFI.replicateTo source target (write options))
  pure (unsafeCoerce r)


-- | Simple keys query, no reduce, no docs included.
viewKeys :: forall l k v.
  ReadForeign { id :: Id l, key :: k, value :: v } =>
  WriteForeign k =>
  -- Should this constrain the type variable l?
  PouchDB -> String -> Array k -> Aff (Array { id :: Id l, key :: k, value :: v })
viewKeys db view keys = do
  r <- fromEffectFnAff (FFI.query db (write view) (write { keys, reduce: false }))
  either (throwError <<< error <<< show) pure (read (unsafeToForeign r.rows))


-- | Simple keys query, no reduce, include docs.
-- |
-- | Throws the first parse error only (if any).
viewKeysInclude :: forall l doc dat k v.
  ReadForeign { doc :: doc, id :: Id l, key :: k, value :: v } =>
  WriteForeign k =>
  Newtype doc { _id :: Id doc, _rev :: Rev doc | dat } =>
  PouchDB -> String -> Array k -> Aff (Array { doc :: doc, id :: Id l, key :: k, value :: v })
viewKeysInclude db view keys = do
    r <- fromEffectFnAff (FFI.query db (write view) (write { keys, reduce: false, include_docs: true }))
    either (throwError <<< error <<< show) pure (read (unsafeToForeign r.rows))


-- | Simple keys query, no reduce, only return docs.
-- |
-- | This is like `viewKeysInclude`, except that you only get the `doc`s.
-- |
-- | This is useful to look up a bunch of documents by a secondary key.
-- | Something like `bulkGet`, except for a different key than `_id`.
viewKeysDoc :: forall doc dat k.
  ReadForeign doc =>
  WriteForeign k =>
  Newtype doc { _id :: Id doc, _rev :: Rev doc | dat } =>
  PouchDB -> String -> Array k -> Aff (Array doc)
viewKeysDoc db view keys = do
    r <- fromEffectFnAff (FFI.query db (write view) (write { keys, reduce: false, include_docs: true }))
    either (throwError <<< error <<< show) pure (read (unsafeToForeign (docs r)))
  where
    docs :: { offset :: Int, rows :: Array Foreign, total_rows :: Int } -> Array Foreign
    docs r = map _.doc (unsafeCoerce r.rows) -- TODO we could do this with a destructive map


-- | Range query with limit, no reduce, no docs.
viewRangeLimit :: forall k l v.
  ReadForeign k =>
  WriteForeign k =>
  ReadForeign v =>
  -- Should this constrain the type variable l?
  PouchDB -> String -> {startkey :: k, endkey :: k} -> Int -> Aff (Array { id :: Id l, key :: k, value :: v })
viewRangeLimit db view {startkey, endkey} limit = do
  r <- fromEffectFnAff (FFI.query db (write view) (write { startkey, endkey, limit, reduce: false }))
  either (throwError <<< error <<< show) pure (read (unsafeToForeign r.rows))


-- | Fetch all docs between startkey and endkey (inclusive)
-- |
-- | Consider using `rangeFromPrefix` when looking for doc ids starting with some string.
allDocsRange :: forall dat doc.
  ReadForeign doc =>
  Newtype doc { _id :: Id doc, _rev :: Rev doc | dat } =>
  PouchDB -> { startkey :: String, endkey :: String } -> Aff (Array doc)
allDocsRange db {startkey, endkey} = do
  r <- fromEffectFnAff (FFI.allDocs db (write { startkey, endkey, include_docs: true }))
  either (throwError <<< error <<< show) pure (traverse (read <<< _.doc <<< unsafeCoerce) r.rows)

-- | Make a query to the primary index, like PouchDB's `allDocs`,
-- | optionally with a start and end key.
-- |
-- | The options are those that do not affect the return type, in
-- | particular `include_docs` is not allowed. If you need the
-- | documents, consider `allDocsRange`.
allDocsNoIncludeRange
  :: forall options.
     Subrow options (startkey :: String, endkey :: String, inclusive_end :: Boolean, limit :: Int, descending :: Boolean) =>
     PouchDB -> {| options } -> Aff { offset :: Int, total_rows :: Int, rows :: Array { id :: String, key :: String, value :: { rev :: String } } }
allDocsNoIncludeRange db opts = coerce (fromEffectFnAff (FFI.allDocs db (unsafeCoerce opts)))
  where coerce = unsafeCoerce :: Aff { total_rows :: Int, offset :: Int, rows :: Array Foreign } -> Aff { offset :: Int, total_rows :: Int, rows :: Array { id :: String, key :: String, value :: { rev :: String } } }

-- | Construct a {startkey, endkey} record for range queries that should
-- | match everything that starts with a given string. As recommended in
-- | in the CouchDB docs, we just append the special character \uFFF0 to
-- | the string to obtain the endkey.
-- | (This means it doesn't work if you use that in your doc ids/keys!)
-- | https://wiki.apache.org/couchdb/View_collation#String_Ranges
rangeFromPrefix :: String -> { startkey :: String, endkey :: String }
rangeFromPrefix s = { startkey: s, endkey: s <> "￰" }


-- | Reduce query over a range of keys, with group_level.
-- |
-- | The types here are less than ideal, because we can have compound keys with different types, e.g. movie Id (as string) and year (as Int).
viewRangeGroupLevel :: forall k v. -- NOTE: We could probably solve the weak typing issues using HLists
  WriteForeign k =>
  ReadForeign k =>
  ReadForeign v =>
  PouchDB -> String -> { startkey :: Array k, endkey :: Array k } -> Int -> Aff (Array { key :: Array k, value :: v })
viewRangeGroupLevel db view { startkey, endkey } group_level = do
  r <- fromEffectFnAff (FFI.query db (write view) (write { startkey, endkey, group_level }))
  either (throwError <<< error <<< show) pure (read (unsafeToForeign r.rows))


-- | Subscribe to future changes
changesLiveSinceNow ::
  PouchDB -> ({ id :: String, rev :: String, deleted :: Boolean } -> Effect Unit) -> Aff Unit
changesLiveSinceNow db handler = liftEffect $ do
  c <- FFI.changes db (write { live: true, since: "now" })
  liftEffect $ _on c "change" changeHandler
  -- TODO on "error" handler?
  -- TODO cancel?
  pure unit
  where
    changeHandler :: Foreign -> Effect Unit
    changeHandler f = case runExcept (parse f) of
      Left _ -> pure unit -- TODO error handler?
      Right r -> handler r
    parse f = do
      id <- f ! "id" >>= readString
      rev <- f ! "changes" ! 0 ! "rev" >>= readString
      maybeDeleted <- f ! "deleted" >>= readNullOrUndefined
      deleted <- maybe (pure false) readBoolean maybeDeleted
      pure { id, rev, deleted }
