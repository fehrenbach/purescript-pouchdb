module PouchDB where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Exception (Error)

foreign import data PouchDB :: *

-- TODO properly specify these
foreign import data PouchDoc :: *

foreign import pouchDB :: String -> PouchDB

foreign import info :: forall e a. PouchDB -> (Error -> Eff e Unit) -> (PouchDoc -> Eff a Unit) -> Eff a Unit
