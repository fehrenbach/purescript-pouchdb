module Database.PouchDB.FFI where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Exception (Error)
import Data.Foreign

foreign import data PouchDB :: *

-- https://pouchdb.com/api.html#create_database
foreign import pouchDB :: String -> Foreign -> PouchDB

-- https://pouchdb.com/api.html#delete_database
foreign import destroy :: forall e.
  PouchDB ->
  Foreign ->
  (Error -> Eff e Unit) ->
  (Foreign -> Eff e Unit) ->
  Eff e Unit

-- https://pouchdb.com/api.html#create_document
foreign import put :: forall e.
  PouchDB ->
  Foreign ->
  Foreign ->
  (Error -> Eff e Unit) ->
  (Foreign -> Eff e Unit) ->
  Eff e Unit

-- https://pouchdb.com/api.html#fetch_document
foreign import get :: forall e.
  PouchDB ->
  String ->
  Foreign ->
  (Error -> Eff e Unit) ->
  (Foreign -> Eff e Unit) ->
  Eff e Unit

-- https://pouchdb.com/api.html#delete_document
foreign import remove :: forall e.
  PouchDB ->
  Foreign -> -- doc
  Foreign -> -- options
  (Error -> Eff e Unit) ->
  (Foreign -> Eff e Unit) ->
  Eff e Unit

-- https://pouchdb.com/api.html#batch_create
foreign import bulkDocs :: forall e.
  PouchDB ->
  [Foreign] ->
  Foreign ->
  (Error -> Eff e Unit) ->
  ([Foreign] -> Eff e Unit) ->
  Eff e Unit

-- https://pouchdb.com/api.html#batch_fetch
foreign import allDocs :: forall e.
  PouchDB ->
  Foreign ->
  (Error -> Eff e Unit) ->
  (Foreign -> Eff e Unit) ->
  Eff e Unit

-- https://pouchdb.com/api.html#changes
-- This returns an event emitter.
-- I'm not quite sure what to do with that.
-- Maybe we should use some library, like this https://github.com/joneshf/purescript-node-events
foreign import changes :: forall e.
  PouchDB ->
  Foreign ->
  Eff e Foreign

-- https://pouchdb.com/api.html#replication
-- This returns an event emitter, like changes.
foreign import replicate :: forall e.
  PouchDB ->
  PouchDB ->
  Foreign ->
  Eff e Foreign

-- https://pouchdb.com/api.html#sync
foreign import sync :: forall e.
  PouchDB ->
  PouchDB ->
  Foreign ->
  Eff e Foreign

-- https://pouchdb.com/api.html#save_attachment
foreign import putAttachment :: forall e.
  PouchDB ->
  String ->
  String ->
  String ->
  Foreign -> -- Blob in browser, or Buffer in node, or base64 string
  String ->
  (Error -> Eff e Unit) ->
  (Foreign -> Eff e Unit) ->
  Eff e Unit

-- https://pouchdb.com/api.html#get_attachment
-- TODO

-- https://pouchdb.com/api.html#delete_attachment
-- TODO

-- https://pouchdb.com/api.html#query_database
-- TODO

-- https://pouchdb.com/api.html#view_cleanup
foreign import viewCleanup :: forall e.
  PouchDB ->
  (Error -> Eff e Unit) ->
  (Foreign -> Eff e Unit) ->
  Eff e Unit

-- https://pouchdb.com/api.html#database_information
foreign import info :: forall e.
  PouchDB ->
  (Error -> Eff e Unit) ->
  (Foreign -> Eff e Unit) ->
  Eff e Unit

-- https://pouchdb.com/api.html#compaction
foreign import compact :: forall e.
  PouchDB ->
  Foreign ->
  (Error -> Eff e Unit) ->
  (Foreign -> Eff e Unit) ->
  Eff e Unit

-- https://pouchdb.com/api.html#revisions_diff
foreign import revsDiff :: forall e.
  PouchDB ->
  Foreign ->
  (Error -> Eff e Unit) ->
  (Foreign -> Eff e Unit) ->
  Eff e Unit

-- https://pouchdb.com/api.html#bulk_get
foreign import bulkGet :: forall e.
  PouchDB ->
  Foreign ->
  (Error -> Eff e Unit) ->
  (Foreign -> Eff e Unit) ->
  Eff e Unit

-- https://pouchdb.com/api.html#close_database
-- TODO
