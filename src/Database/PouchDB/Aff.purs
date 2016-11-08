module Database.PouchDB.Aff where

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, fromObject, (.?))
import Data.Either (Either (..))
import Data.Foreign (Foreign, writeObject)
import Data.Newtype (class Newtype)
import Database.PouchDB.FFI (PouchDB, POUCHDB)
import Database.PouchDB.FFI as FFI
import Prelude
import Unsafe.Coerce (unsafeCoerce)

-- TODO remove (or move into its own library or something)
dbg :: forall a. String -> a -> a
dbg s a = unsafePerformEff $ do
  log s
  log (unsafeCoerce a)
  pure a

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

instance eqDocument :: Eq d => Eq (Document d) where
  eq (Document (Id id_l) (Rev rev_l) d_l) (Document (Id id_r) (Rev rev_r) d_r) =
    id_l == id_r && rev_l == rev_r && d_l == d_r

instance showDocument :: Show d => Show (Document d) where
  show (Document (Id id) (Rev rev) d) =
    "Document " <> id <> " " <> rev <> " (" <> show d <> ")"

newtype Id = Id String

derive instance newtypeId :: Newtype Id _
derive instance eqId :: Eq Id

instance showId :: Show Id where
  show (Id id) = id

newtype Rev = Rev String

derive instance newtypeRev :: Newtype Rev _
derive instance eqRev :: Eq Rev

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
create :: forall d e. EncodeJson d =>
          PouchDB -> Id -> d ->
          Aff (pouchdb :: POUCHDB | e) (Document d)
create db (Id id) doc = makeAff (\kE kS ->
  FFI.put db docWithId empty kE (success kS))
    where docWithId = unsafeCoerce $ (unsafeCoerce (encodeJson doc)) {_id = id}
          success kS {ok, id, rev} = kS (Document (Id id) (Rev rev) doc)

update :: forall d e. EncodeJson d =>
          PouchDB -> Document d ->
          Aff (pouchdb :: POUCHDB | e) (Document d)
update db doc@(Document _ _ payload) = makeAff (\kE kS ->
    -- TODO this coercion is not ideal, but there's no EncodeJsonObject class, but we know we encode to an object.
    FFI.put db (unsafeCoerce (encodeJson doc)) empty kE (success kS))
  where
    success kS {ok, id, rev} = kS (Document (Id id) (Rev rev) payload)

get :: forall d e. DecodeJson d =>
       PouchDB -> Id ->
       Aff (pouchdb :: POUCHDB | e) (Document d)
get db (Id id) = makeAff (\kE kS ->
  FFI.get db id empty
          (\e -> kE e)
          (\r -> case decodeJson (fromObject r) of
             -- TODO figure out whether we can properly attach data to an error
             Left parseError -> kE (error $ "parse error: " <> show parseError)
             Right d -> kS d))
