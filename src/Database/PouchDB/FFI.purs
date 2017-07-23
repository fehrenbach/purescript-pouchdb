module Database.PouchDB.FFI where

import Prelude (Unit)
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Exception (Error)
import Data.Foreign (Foreign)
import Data.Argonaut.Core (JObject)

foreign import data PouchDB :: Type

foreign import data POUCHDB :: Effect

--| https://pouchdb.com/api.html#create_database
foreign import pouchDB :: forall e. Foreign -> Eff (pouchdb :: POUCHDB | e) PouchDB

--| https://pouchdb.com/api.html#delete_database
foreign import destroy :: forall e.
  PouchDB ->
  Foreign ->
  (Error -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  ({ok::Boolean} -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  Eff (pouchdb :: POUCHDB | e) Unit

--| https://pouchdb.com/api.html#create_document
foreign import put :: forall e.
  PouchDB ->
  JObject ->
  Foreign ->
  (Error -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  ({ok::Boolean, id::String, rev::String} -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  Eff (pouchdb :: POUCHDB | e) Unit

--| https://pouchdb.com/api.html#create_document
foreign import post :: forall e.
  PouchDB ->
  JObject ->
  Foreign ->
  (Error -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  ({ok::Boolean, id::String, rev::String} -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  Eff (pouchdb :: POUCHDB | e) Unit

--| https://pouchdb.com/api.html#fetch_document
foreign import get :: forall e.
  PouchDB ->
  String ->
  Foreign ->
  (Error -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  (JObject -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  Eff (pouchdb :: POUCHDB | e) Unit

--| https://pouchdb.com/api.html#delete_document
foreign import remove :: forall e.
  PouchDB ->
  JObject ->
  Foreign ->
  (Error -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  ({ok::Boolean, id::String, rev::String} -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  Eff (pouchdb :: POUCHDB | e) Unit

--| https://pouchdb.com/api.html#batch_create
foreign import bulkDocs :: forall e.
  PouchDB ->
  Array JObject ->
  Foreign ->
  (Error -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  (Array JObject -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  Eff (pouchdb :: POUCHDB | e) Unit

--| https://pouchdb.com/api.html#batch_fetch
foreign import allDocs :: forall e.
  PouchDB ->
  Foreign ->
  (Error -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  ({total_rows::Int, offset::Int, rows::Array JObject} -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  Eff (pouchdb :: POUCHDB | e) Unit

--| https://pouchdb.com/api.html#changes
-- TODO This returns an event emitter.
-- I'm not quite sure what to do with that.
-- Maybe we should use some library, like this https://github.com/joneshf/purescript-node-events
foreign import changes :: forall e.
  PouchDB ->
  Foreign ->
  Eff (pouchdb :: POUCHDB | e) Foreign

--| https://pouchdb.com/api.html#replication
-- TODO This returns an event emitter, like changes.
foreign import replicate :: forall e.
  PouchDB ->
  PouchDB ->
  Foreign ->
  Eff (pouchdb :: POUCHDB | e) Foreign

--| Single-shot replication
--|
--| Do not set `live : true` on the option parameter!
foreign import replicateTo :: forall e.
  PouchDB ->
  PouchDB ->
  Foreign ->
  (Error -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  (Foreign -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  Eff (pouchdb :: POUCHDB | e) Unit

--| https://pouchdb.com/api.html#sync
foreign import sync :: forall e.
  PouchDB ->
  PouchDB ->
  Foreign ->
  Eff (pouchdb :: POUCHDB | e) Foreign

--| https://pouchdb.com/api.html#save_attachment
foreign import putAttachment :: forall e.
  PouchDB ->
  String ->
  String ->
  String ->
  Foreign -> -- Blob in browser, or Buffer in node, or base64 string
  String ->
  (Error -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  ({ok::Boolean, id::String, rev::String} -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  Eff (pouchdb :: POUCHDB | e) Unit

--| https://pouchdb.com/api.html#get_attachment
-- TODO Attachments are Blobs in browsers and Buffers in node, not sure how to deal with that.
foreign import getAttachment :: forall e.
  PouchDB ->
  String ->
  String ->
  Foreign ->
  (Error -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  (Foreign -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  Eff (pouchdb :: POUCHDB | e) Unit

--| https://pouchdb.com/api.html#delete_attachment
foreign import removeAttachment :: forall e.
  PouchDB ->
  String ->
  String ->
  String ->
  (Error -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  (Foreign -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  Eff (pouchdb :: POUCHDB | e) Unit

--| https://pouchdb.com/api.html#query_database
foreign import query :: forall e.
  PouchDB ->
  Foreign ->
  Foreign ->
  (Error -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  (Foreign -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  Eff (pouchdb :: POUCHDB | e) Unit

--| https://pouchdb.com/api.html#view_cleanup
foreign import viewCleanup :: forall e.
  PouchDB ->
  (Error -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  ({ok::Boolean} -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  Eff (pouchdb :: POUCHDB | e) Unit

--| https://pouchdb.com/api.html#database_information
foreign import info :: forall e.
  PouchDB ->
  (Error -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  (JObject -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  Eff (pouchdb :: POUCHDB | e) Unit

--| https://pouchdb.com/api.html#compaction
foreign import compact :: forall e.
  PouchDB ->
  Foreign ->
  (Error -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  ({ok::Boolean} -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  Eff (pouchdb :: POUCHDB | e) Unit

--| https://pouchdb.com/api.html#revisions_diff
foreign import revsDiff :: forall e.
  PouchDB ->
  Foreign ->
  (Error -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  (JObject -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  Eff (pouchdb :: POUCHDB | e) Unit

--| https://pouchdb.com/api.html#bulk_get
foreign import bulkGet :: forall e.
  PouchDB ->
  Foreign ->
  (Error -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  ({results::Array Foreign} -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  Eff (pouchdb :: POUCHDB | e) Unit

--| https://pouchdb.com/api.html#close_database
foreign import close :: forall e.
  PouchDB ->
  (Unit -> Eff (pouchdb :: POUCHDB | e) Unit) ->
  Eff (pouchdb :: POUCHDB | e) Unit
