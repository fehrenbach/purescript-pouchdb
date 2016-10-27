module Database.PouchDB.Aff where

import Prelude
import Data.Newtype (class Newtype)
import Database.PouchDB.FFI (PouchDB)
import Database.PouchDB.FFI as FFI
import Control.Monad.Aff (Aff, makeAff)
import Data.Foreign (Foreign, unsafeFromForeign, writeObject, toForeign)
import Data.Foreign.Class (class IsForeign, class AsForeign, read, readProp, write)
import Control.Monad.Except (runExcept)
import Data.Either (Either (..))
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Exception (error)

foreign import data POUCHDB :: !

-- TODO remove (or move into its own library or something)
dbg :: forall a. String -> a -> a
dbg s a = unsafePerformEff $ do
  log s
  log (unsafeCoerce a)
  pure a

data Document d =
  Document Id Rev d

instance isForeignDocument :: IsForeign d => IsForeign (Document d) where
  read value = do
    id <- readProp "_id" value
    rev <- readProp "_rev" value
    d <- read value
    pure $ Document id rev d

instance asForeignDocument :: AsForeign d => AsForeign (Document d) where
  write (Document i r d) =
    -- This is a bit of a hack, because we need to serialize the
    -- payload to an object, then on that object set _id and _rev.
    toForeign $ (unsafeCoerce (write d)) {_id = write i, _rev = write r}


newtype Id = Id String

derive instance newtypeId :: Newtype Id _
derive newtype instance asForeignId :: AsForeign Id
derive newtype instance isForeignId :: IsForeign Id


newtype Rev = Rev String

derive instance newtypeRev :: Newtype Rev _
derive newtype instance asForeignRev :: AsForeign Rev
derive newtype instance isForeignRev :: IsForeign Rev


empty :: Foreign
empty = writeObject []

info :: forall e. PouchDB -> Aff (pouchdb :: POUCHDB | e) {db_name :: String, doc_count :: Int, update_seq :: Int}
info db = makeAff (\e s -> FFI.info db e (s <<< unsafeFromForeign))

pouchDB :: String -> PouchDB
pouchDB name = FFI.pouchDB name empty

--| Deletes the database.
--|
--| You should not try to use the handle again.
--| If you need a new database with the same name, call `pouchDB` again.
destroy :: forall e. PouchDB -> Aff (pouchdb :: POUCHDB | e) {ok :: Boolean}
destroy db = makeAff (\e s -> FFI.destroy db empty e (s <<< unsafeFromForeign))

-- TODO change return type to Document
create :: forall d e. AsForeign d => PouchDB -> Id -> d -> Aff (pouchdb :: POUCHDB | e) Foreign
create db (Id id) doc = makeAff (\kE kS ->
  FFI.put db docWithId empty kE kS)
    where docWithId = toForeign $ (unsafeCoerce (write doc)) {_id = id}

-- TODO change return type to Aff .. (Document d) with updated id rev
update :: forall d e. AsForeign d => PouchDB -> (Document d) -> Aff (pouchdb :: POUCHDB | e) {ok :: Boolean, id :: Id, rev :: Rev}
update db doc = makeAff (\kE kS ->
  FFI.put db (write doc) empty kE (kS <<< unsafeFromForeign))  

get :: forall d e. IsForeign d => PouchDB -> Id -> Aff (pouchdb :: POUCHDB | e) (Document d)
get db (Id id) = makeAff (\kE kS ->
  FFI.get db id empty
          (\e -> kE e)
          (\r -> case runExcept (read r) of
             -- TODO figure out whether we can properly attach data to an error
             Left parseError -> kE (error $ "parse error: " <> show parseError)
             Right d -> kS d))