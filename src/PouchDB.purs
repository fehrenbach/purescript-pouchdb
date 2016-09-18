module PouchDB where

import Prelude
import Control.Monad.Eff

foreign import data PouchDB :: *

-- TODO properly specify these
foreign import data PouchError :: *
foreign import data PouchDoc :: *

foreign import pouchDB :: String -> PouchDB

foreign import info :: forall e a. PouchDB -> (PouchError -> Eff e Unit) -> (PouchDoc -> Eff a Unit) -> Eff a PouchDoc