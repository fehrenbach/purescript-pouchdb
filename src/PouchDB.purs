module PouchDB where

import Prelude
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Exception (Error)
import Data.Foreign

foreign import data POUCHDB :: !

foreign import data PouchDB :: *

type Document = Foreign

foreign import pouchDB :: String -> PouchDB

foreign import f_info :: forall e.
  PouchDB ->
  (Error -> Eff e Unit) ->
  ({db_name :: String, doc_count :: Number, update_seq :: Number} -> Eff e Unit) ->
  Eff e Unit

info :: forall e. PouchDB -> Aff (pouchdb :: POUCHDB | e) {db_name :: String, doc_count :: Number, update_seq :: Number}
info db = makeAff (\e s -> f_info db e s)

foreign import f_put :: forall e.
  PouchDB ->
  Document ->
  (Error -> Eff e Unit) ->
  ({ok :: Boolean, id :: String, rev :: String} -> Eff e Unit) ->
  Eff e Unit

-- put is a bit weird: in some cases (document not already in the database) it accepts
-- a "proto" document, a document without the _rev field.
-- TODO consider splitting behaviour of put into two:
-- - create new document (requires _id, no _rev) and
-- - update existing document (with _id and _rev)
-- - in this sense, post is another special case of put which does not even require _id
-- In JavaScript this is all fine, because we expect every field to be potentially null/undefined.
-- It looks like in PureScript having unsaved documents lying around might be slightly awkward.
put :: forall e. PouchDB -> Document -> Aff (pouchdb :: POUCHDB | e) {ok :: Boolean, id :: String, rev :: String}
put db doc = makeAff (\e s -> f_put db doc e s)
