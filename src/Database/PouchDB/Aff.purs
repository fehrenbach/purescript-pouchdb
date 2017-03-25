module Database.PouchDB.Aff where

import Prelude
import Database.PouchDB.FFI as FFI
import Control.Alt ((<|>))
import Control.Monad.Aff (attempt, Aff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error, Error)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (class DecodeJson, class EncodeJson, JObject, Json, decodeJson, encodeJson, fromObject, (.?))
import Data.Array (zipWith)
import Data.Either (Either(..))
import Data.Foreign (Foreign, toForeign, writeObject)
import Data.Generic (class Generic)
import Data.Newtype (class Newtype)
import Data.StrMap (StrMap)
import Data.Traversable (sequence)
import Database.PouchDB.FFI (PouchDB, POUCHDB)
import Unsafe.Coerce (unsafeCoerce)

data Document d = Document Id Rev d

instance decodeJsonDocument :: DecodeJson d => DecodeJson (Document d) where
  decodeJson json = do
    -- a response is the document with additional _id and _rev
    -- this uses the default instance for StrMap
    response <- decodeJson json
    id <- response .? "_id"
    rev <- response .? "_rev"
    -- parse the whole response as the actual payload
    -- this uses the (DecodeJson d) instance defined by the user for the payload
    payload <- decodeJson json
    pure $ Document (Id id) (Rev rev) payload

instance encodeJsonDocument :: EncodeJson d => EncodeJson (Document d) where
  encodeJson (Document (Id i) (Rev r) d) =
    -- This is a bit of a hack, because we need to serialize the
    -- payload to an object, then on that object set _id and _rev.
    unsafeCoerce $ (unsafeCoerce (encodeJson d)) {_id = i, _rev = r}

derive instance eqDocument :: Eq a => Eq (Document a)
derive instance ordDocument :: Ord d => Ord (Document d)
derive instance genericDocument :: Generic d => Generic (Document d)

instance showDocument :: Show d => Show (Document d) where
  show (Document (Id id) (Rev rev) d) =
    "(Document " <> id <> " " <> rev <> " " <> show d <> ")"

instance functorDocument :: Functor Document where
  map f (Document id rev d) = Document id rev (f d)


newtype Id = Id String

derive instance newtypeId :: Newtype Id _
derive instance eqId :: Eq Id
derive instance ordId :: Ord Id
derive instance genericId :: Generic Id
derive newtype instance showId :: Show Id
derive newtype instance encodeJsonId :: EncodeJson Id
derive newtype instance decodeJsonId :: DecodeJson Id


newtype Rev = Rev String

derive instance newtypeRev :: Newtype Rev _
derive instance eqRev :: Eq Rev
derive instance ordRev :: Ord Rev
derive instance genericRev :: Generic Rev
derive newtype instance showRev :: Show Rev
derive newtype instance encodeJsonRev :: EncodeJson Rev
derive newtype instance decodeJsonRev :: DecodeJson Rev


empty :: Foreign
empty = writeObject []

info :: forall e.
        PouchDB ->
        Aff (pouchdb :: POUCHDB | e) {db_name :: String, doc_count :: Int, update_seq :: Int}
info db = makeAff (\e s -> FFI.info db e (s <<< unsafeFromInfo))
  where unsafeFromInfo = unsafeCoerce -- PouchDB says these fields are always there, let's trust them.

pouchDB :: forall e. String -> Aff (pouchdb :: POUCHDB | e) PouchDB
pouchDB name = liftEff $ FFI.pouchDB name empty

--| Deletes the database.
--|
--| You should not try to use the handle again.
--| If you need a new database with the same name, call `pouchDB` again.
destroy :: forall e.
           PouchDB ->
           Aff (pouchdb :: POUCHDB | e) Unit
destroy db = makeAff (\kE kS -> FFI.destroy db empty kE (\_ -> kS unit))

--| Save the given data as a new document with the given id.
--|
--| This is the recommended way to generate documents, rather than relying on
--| PouchDB to make up a random document ID for you.
createDoc :: forall d e. EncodeJson d =>
             PouchDB -> Id -> d ->
             Aff (pouchdb :: POUCHDB | e) (Document d)
createDoc db (Id docId) doc = makeAff (\kE kS ->
  FFI.put db docWithId empty kE (success kS))
    where docWithId = unsafeCoerce $ (unsafeCoerce (encodeJson doc)) {_id = docId}
          success kS {id: newId, rev: newRev} = kS (Document (Id newId) (Rev newRev) doc)

--| Write a (modified) document back to the database.
saveDoc :: forall d e. EncodeJson d =>
           PouchDB -> Document d ->
           Aff (pouchdb :: POUCHDB | e) (Document d)
saveDoc db doc@(Document _ _ payload) = makeAff (\kE kS ->
    -- TODO this coercion is not ideal, but there's no EncodeJsonObject class,
    -- but we know (well, require rather) that we encode to an object.
    FFI.put db (unsafeCoerce (encodeJson doc)) empty kE (success kS))
  where
    success kS {ok, id, rev} = kS (Document (Id id) (Rev rev) payload)

--| Write multiple (modified) documents back to the database.
--| Note that this is not a transaction. Individual writes might fail (in which case this function raises an error (currently reporting only the first error)) but other changes might already have been made.
--| As far as I can tell, PouchDB will increase the revision number even if you write the same exact document that's already there. You might want to avoid that.
bulkSave :: forall d e. EncodeJson d =>
            PouchDB -> Array (Document d) ->
            Aff (pouchdb :: POUCHDB | e) (Array (Document d))
bulkSave db docs = makeAff (\kE kS ->
  FFI.bulkDocs db (unsafeCoerce (map encodeJson docs)) empty kE (success kE kS))
  where
    success :: (Error -> Eff (pouchdb :: POUCHDB | e) Unit)
            -> (Array (Document d) -> Eff (pouchdb :: POUCHDB | e) Unit)
            -> Array (StrMap Json)
            -> Eff (pouchdb :: POUCHDB | e) Unit
    success kE kS rows =
      let docsOrErrors = zipWith decodeRow rows docs
      -- TODO This is not great, it only returns the first error, and the errors themselves are not very useful.
      in case sequence docsOrErrors of
        (Left e) -> kE (error e)
        (Right decodedDocs) -> kS decodedDocs
    decodeRow :: StrMap Json -> Document d -> Either String (Document d)
    decodeRow m d = (decodeSuc m d) <|> (decodeErr m)
    decodeSuc :: StrMap Json -> Document d -> Either String (Document d)
    decodeSuc m (Document _ _ d) = do
      ok <- m .? "ok"
      newId <- m .? "id"
      newRev <- m .? "rev"
      if ok then Right (Document newId newRev d) else Left "Not ok."
    decodeErr :: StrMap Json -> Either String (Document d)
    decodeErr e = Left ("PouchDB reported an error: " <> show e)


--| Fetch a document from the database.
getDoc :: forall d e. DecodeJson d =>
          PouchDB -> Id ->
          Aff (pouchdb :: POUCHDB | e) (Document d)
getDoc db (Id id) = makeAff (\kE kS ->
  FFI.get db id empty kE
          (\r -> case decodeJson (fromObject r) of
             -- TODO figure out whether we can properly attach data to an error
             Left parseError -> kE (error $ "parse error: " <> parseError)
             Right d -> kS d))

--| Fetch multiple documents at once.
--|
--| This should be the same as making multiple calls to `get`.
--| This uses PouchDB's `allDocs` under the hood. It should not be
--| confused with PouchDB's actual `bulkGet` which is used mainly
--| internally for replication.
bulkGet :: forall d e. DecodeJson d =>
           PouchDB -> Array Id ->
           Aff (pouchdb :: POUCHDB | e) (Array (Document d))
bulkGet db ids = makeAff (\kE kS ->
  FFI.allDocs db (unsafeCoerce {keys: ids, include_docs: true}) kE
    (\r -> case sequence (map (_.doc >>> decodeJson) ((unsafeCoerce r).rows)) of
             Left parseError -> kE (error $ "parse error in at least one row: " <> parseError)
             Right d -> kS d))

--| Get a document, apply a function, and save it back.
--|
--| In case of concurrent modification, this function will retry (as often as necessary).
modifyDoc :: forall d d' e. (EncodeJson d', DecodeJson d) =>
             PouchDB -> (d -> d') -> Id ->
             Aff (pouchdb :: POUCHDB | e) (Document d')
modifyDoc db f id = do
  d <- getDoc db id
  a <- attempt $ saveDoc db (map f d)
  case a of
    Left e -> if isConflict e then modifyDoc db f id else throwError e
    Right v -> pure v
    where
      isConflict e = (unsafeCoerce e).name == "conflict"

--| Delete a document from the database
--|
--| Unlike PouchDB's `remove`, this does not clear all fields, but merely sets `_deleted: true`.
deleteDoc :: forall d e. EncodeJson d =>
             PouchDB -> Document d -> Aff (pouchdb :: POUCHDB | e) (Document d)
deleteDoc db doc@(Document _ _ docContent) = makeAff (\kE kS ->
  FFI.put db json empty kE (success kS))
  where
    json = unsafeCoerce ((unsafeCoerce (encodeJson doc)) { _deleted = true })
    success kS {ok, id, rev} = kS (Document (Id id) (Rev rev) docContent)

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
    Change (ReplicationInfo (docs :: Array JObject))
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
  ee <- liftEff $ FFI.replicate source target (unsafeCoerce {live: true, retry: true})
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
  FFI.replicateTo source target empty kE (\r -> kS (unsafeCoerce r)))


-- NOTE: The CouchDB documentation does not apply to PouchDB. In CouchDB group=true is the same as group_level=exact, in PouchDB the latter does not work.
-- NOTE: Don't trust the PouchDB documentation, either:
--   - options.reduce does not default to false, if the view is in a design_document and has an associated reduce function.
--   - Other values than false for options.reduce ('_sum', '_count', '_stats', presumably functions) don't do anything if the view is in a design_document and has an associated reduce function, or {map: .., reduce: ..} version is used.
--   - Other values than false for options.reduce seem to not be working in general. Use {map: .., reduce: ..} notation, I guess.

-- reduce -> no include_docs (.. conflicts, attachments, binary)
-- group / group_level only meaningful with reduce

-- limit, skip, descending is for (possibly reduced) output, not view output before reduction

-- db.query("ddocs/yard-list", {group:true}, function (err, res) { console.log(res) })


--| Query the database using view and reduce functions stored in a design document.
--|
--| `decodeJson` will be called on every row in the result.
--| A row is a record of the form `{ key :: Json, value :: Json }`.
-- TODO would it make sense to offer a version without decoding built-in?
viewAllGroup :: forall d e.
  DecodeJson d =>
  PouchDB ->
  String -> -- "designdocname/viewname"
  Aff (pouchdb :: POUCHDB | e) (Array d) -- Array/List
viewAllGroup db view = makeAff (\kE kS ->
  FFI.query db
    (unsafeCoerce view)
    (unsafeCoerce {group: true, reduce: true})
    kE
    -- TODO `sequence` will give us the first error. Can we do better?
    (\r -> case sequence (map decodeJson ((unsafeCoerce r).rows)) of
             Left parseError -> kE (error $ "parse error in at least one row: " <> parseError)
             Right d -> kS d))

-- TODO I don't use it. remove?
--| Simple range query, no reduce, include docs.
viewRangeInclude :: forall d e.
  DecodeJson d =>
  PouchDB ->
  String ->
  { startkey :: Json, endkey :: Json } ->
  Aff (pouchdb :: POUCHDB | e) (Array d)
viewRangeInclude db view { startkey, endkey } = makeAff (\kE kS ->
  FFI.query db
    (unsafeCoerce view)
    (unsafeCoerce { startkey, endkey, reduce: false, include_docs: true })
    kE
    (\r -> case sequence (map decodeJson ((unsafeCoerce r).rows)) of
             Left parseError -> kE (error $ "parse error in at least one row: " <> parseError)
             Right d -> kS d))

--| This is useful for decoding view results, like `viewKeysInclude`.
--|
--| For example, we can use the following code to easly decode a view that emits all actor ids from all movie documents. This assumes we already have decoding logic for `Movie`.
--| The `doc` field will be the movie document; the `key` field will contain the actor's id; the `id` field will contain the movie's id; the `value` is whatever the view emitted, here we assume it's `null`.
--| ```
--| viewResult :: Array (ViewRow Movie Id Unit) <- liftAff $ viewKeysInclude localDB "someddoc/movies-with-actor" [ actorid ]
--| ```
newtype ViewRow d k v =
  ViewRow { doc :: Document d
          , id :: Id
          , key :: k
          , value :: v }

derive instance newtypeViewRow :: Newtype (ViewRow d k v) _

instance decodeViewRow :: (DecodeJson d, DecodeJson k, DecodeJson v) => DecodeJson (ViewRow d k v) where
  decodeJson json = do
    obj <- decodeJson json
    doc <- obj .? "doc"
    id <- obj .? "id"
    key <- obj .? "key"
    value <- obj .? "value"
    pure $ ViewRow { doc, id, key, value }


--| Simple keys query, no reduce, include docs.
viewKeysInclude :: forall d e k.
  (DecodeJson d, EncodeJson k) =>
  PouchDB ->
  String ->
  Array k ->
  Aff (pouchdb :: POUCHDB | e) (Array d)
viewKeysInclude db view keys = makeAff (\kE kS ->
  FFI.query db
    (unsafeCoerce view)
    (unsafeCoerce { keys : encodeJson (map encodeJson keys)
                  , reduce : false
                  , include_docs: true })
    kE
    (\r -> case sequence (map decodeJson ((unsafeCoerce r).rows)) of
             Left parseError -> kE (error $ "parse error in at least one row: " <> parseError)
             Right d -> kS d))

--| Fetch all docs between startkey and endkey (inclusive)
--| Consider using `docIdStartsWith` when looking for doc ids starting with some string.
allDocsRange :: forall d e.
  DecodeJson d =>
  PouchDB ->
  { startkey :: String, endkey :: String } ->
  Aff (pouchdb :: POUCHDB | e) (Array (Document d))
allDocsRange db {startkey, endkey} = makeAff (\kE kS ->
  FFI.allDocs db (toForeign { startkey, endkey, include_docs: true }) kE
    (\r -> case sequence (map (decodeJson <<< _.doc <<< unsafeCoerce) r.rows) of
             Left parseError -> kE (error $ "parse error in at least one row: " <> parseError)
             Right d -> kS d))

--| Construct a {startkey, endkey} record for range queries that should
--| match everything that starts with a given string. As recommended in
--| in the CouchDB docs, we just append the special character \uFFF0 to
--| the string to obtain the endkey.
--| (This means it doesn't work if you use that in your doc ids/keys!)
--| https://wiki.apache.org/couchdb/View_collation#String_Ranges
docIdStartsWith :: String -> { startkey :: String, endkey :: String }
docIdStartsWith s = { startkey: s, endkey: s <> "￰" }

viewRangeGroupLevel :: forall e k v.
  (DecodeJson k, DecodeJson v) =>
  PouchDB ->
  String ->
  { startkey :: Array Json,
    endkey :: Array Json } ->
  Int ->
  Aff (pouchdb :: POUCHDB | e) (Array { key :: k, value :: v })
viewRangeGroupLevel db view {startkey, endkey} group_level = makeAff (\kE kS ->
  FFI.query db (toForeign view) (toForeign { startkey, endkey, group_level, reduce: true }) kE (success kE kS))
  where
    success kE kS res = case sequence (map parseRow (unsafeCoerce res).rows) of
      Left parseError -> kE (error $ "parse error in at least one row: " <> parseError)
      Right d -> kS d
    parseRow { key, value } = do parsedKey <- decodeJson key
                                 parsedValue <- decodeJson value
                                 Right { key: parsedKey, value: parsedValue }
