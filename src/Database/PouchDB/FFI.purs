module Database.PouchDB.FFI where

import Effect (Effect)
import Effect.Aff.Compat (EffectFnAff)
import Foreign (Foreign)
import Prelude (Unit)

foreign import data PouchDB :: Type

--| https://pouchdb.com/api.html#create_database
foreign import pouchDB :: Foreign -> EffectFnAff PouchDB

--| https://pouchdb.com/api.html#delete_database
foreign import destroy :: 
  PouchDB -> Foreign ->
  EffectFnAff { ok :: Boolean }

--| https://pouchdb.com/api.html#create_document
foreign import put :: 
  PouchDB -> Foreign -> Foreign ->
  EffectFnAff {ok::Boolean, id::String, rev::String}

--| https://pouchdb.com/api.html#create_document
foreign import post :: 
  PouchDB -> Foreign -> Foreign ->
  EffectFnAff {ok::Boolean, id::String, rev::String}

--| https://pouchdb.com/api.html#fetch_document
foreign import get :: 
  PouchDB -> String -> Foreign ->
  EffectFnAff Foreign

--| https://pouchdb.com/api.html#delete_document
foreign import remove ::
  PouchDB -> Foreign -> Foreign ->
  EffectFnAff {ok::Boolean, id::String, rev::String}

--| https://pouchdb.com/api.html#batch_create
foreign import bulkDocs ::
  PouchDB -> Array Foreign -> Foreign ->
  EffectFnAff (Array Foreign)

--| https://pouchdb.com/api.html#batch_fetch
foreign import allDocs :: 
  PouchDB -> Foreign ->
  EffectFnAff {total_rows::Int, offset::Int, rows::Array Foreign}

--| https://pouchdb.com/api.html#changes
-- TODO This returns an event emitter.
-- I'm not quite sure what to do with that.
-- Maybe we should use some library, like this https://github.com/joneshf/purescript-node-events
foreign import changes :: 
  PouchDB -> Foreign ->
  Effect Foreign

--| https://pouchdb.com/api.html#replication
-- TODO This returns an event emitter, like changes.
foreign import replicate ::
  PouchDB -> PouchDB -> Foreign ->
  Effect Foreign

--| Single-shot replication
--|
--| Do not set `live : true` on the option parameter!
foreign import replicateTo :: 
  PouchDB -> PouchDB -> Foreign ->
  EffectFnAff Foreign

--| https://pouchdb.com/api.html#sync
foreign import sync ::
  PouchDB -> PouchDB -> Foreign ->
  Effect Foreign

--| https://pouchdb.com/api.html#save_attachment
foreign import putAttachment ::
  PouchDB -> String -> String -> String ->
  Foreign -> -- Blob in browser, or Buffer in node, or base64 string
  String ->
  EffectFnAff {ok::Boolean, id::String, rev::String}

--| https://pouchdb.com/api.html#get_attachment
-- TODO Attachments are Blobs in browsers and Buffers in node, not sure how to deal with that.
foreign import getAttachment ::
  PouchDB -> String -> String -> Foreign ->
  EffectFnAff Foreign

--| https://pouchdb.com/api.html#delete_attachment
foreign import removeAttachment ::
  PouchDB -> String -> String -> String ->
  EffectFnAff Foreign

--| https://pouchdb.com/api.html#query_database
foreign import query ::
  PouchDB -> Foreign -> Foreign ->
  EffectFnAff { offset :: Int, rows :: Array Foreign, total_rows :: Int }

--| https://pouchdb.com/api.html#view_cleanup
foreign import viewCleanup :: 
  PouchDB ->
  EffectFnAff { ok :: Boolean }

--| https://pouchdb.com/api.html#database_information
foreign import info ::
  PouchDB ->
  EffectFnAff Foreign

--| https://pouchdb.com/api.html#compaction
foreign import compact :: 
  PouchDB -> Foreign ->
  EffectFnAff Foreign

--| https://pouchdb.com/api.html#revisions_diff
foreign import revsDiff ::
  PouchDB -> Foreign ->
  EffectFnAff Foreign

--| https://pouchdb.com/api.html#bulk_get
foreign import bulkGet ::
  PouchDB -> Foreign ->
  EffectFnAff {results::Array Foreign}

--| https://pouchdb.com/api.html#close_database
foreign import close ::
  PouchDB ->
  (Unit -> Effect Unit) ->
  Effect Unit
