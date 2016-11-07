module Database.PouchDB.Aff where

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, fromObject, toObject, (.?))
import Data.Either (Either (..))
import Data.Foreign (Foreign, unsafeFromForeign, writeObject, toForeign)
import Data.Foreign.Class (class IsForeign, class AsForeign, read, readProp, write)
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

data Document d =
  Document Id Rev d

instance decodeJsonDocument :: DecodeJson d => DecodeJson (Document d) where
  decodeJson json = do
    -- FIXME this is wrong!
    doc <- unsafeCoerce json
    id <- doc .? "_id"
    rev <- doc .? "_rev"
    d <- decodeJson json
    pure $ Document (Id id) (Rev rev) d

instance encodeJsonDocument :: EncodeJson d => EncodeJson (Document d) where
  encodeJson (Document (Id i) (Rev r) d) =
    -- This is a bit of a hack, because we need to serialize the
    -- payload to an object, then on that object set _id and _rev.
    unsafeCoerce $ (unsafeCoerce (encodeJson d)) {_id = i, _rev = r}


newtype Id = Id String

derive instance newtypeId :: Newtype Id _

newtype Rev = Rev String

derive instance newtypeRev :: Newtype Rev _

empty :: Foreign
empty = writeObject []

info :: forall e. PouchDB -> Aff (pouchdb :: POUCHDB | e) {db_name :: String, doc_count :: Int, update_seq :: Int}
info db = makeAff (\e s -> FFI.info db e (s <<< unsafeFromInfo))
  where unsafeFromInfo = unsafeCoerce -- PouchDB says these fields are always there, let's trust them.

pouchDB :: forall e. String -> Aff (pouchdb :: POUCHDB | e) PouchDB
pouchDB name = liftEff $ FFI.pouchDB name empty

--| Deletes the database.
--|
--| You should not try to use the handle again.
--| If you need a new database with the same name, call `pouchDB` again.
destroy :: forall e. PouchDB -> Aff (pouchdb :: POUCHDB | e) Unit
destroy db = makeAff (\kE kS -> FFI.destroy db empty kE (\_ -> kS unit))

-- TODO change return type to Document
create :: forall d e. EncodeJson d => PouchDB -> Id -> d -> Aff (pouchdb :: POUCHDB | e) { ok :: Boolean, id :: String, rev :: String }

create db (Id id) doc = makeAff (\kE kS ->
  FFI.put db docWithId empty kE kS)
    where docWithId = unsafeCoerce $ (unsafeCoerce (encodeJson doc)) {_id = id}

-- TODO change return type to Aff .. (Document d) with updated id rev
update :: forall d e. EncodeJson d => PouchDB -> (Document d) -> Aff (pouchdb :: POUCHDB | e) {ok :: Boolean, id :: Id, rev :: Rev}
update db doc = makeAff (\kE kS ->
    -- TODO this coercion is not ideal, but there's no EncodeJsonObject class, but we know we encode to an object.
    FFI.put db (unsafeCoerce (encodeJson doc)) empty kE (kS <<< newtypeResult))
  where
    newtypeResult :: {ok :: Boolean, id :: String, rev :: String} -> {ok :: Boolean, id :: Id, rev :: Rev}
    newtypeResult = unsafeCoerce

get :: forall d e. DecodeJson d => PouchDB -> Id -> Aff (pouchdb :: POUCHDB | e) (Document d)
get db (Id id) = makeAff (\kE kS ->
  FFI.get db id empty
          (\e -> kE e)
          (\r -> case decodeJson (fromObject r) of
             -- TODO figure out whether we can properly attach data to an error
             Left parseError -> kE (error $ "parse error: " <> show parseError)
             Right d -> kS d))
