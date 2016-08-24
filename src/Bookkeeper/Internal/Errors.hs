{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
module Bookkeeper.Internal.Errors where

import qualified Data.Type.Map as Map
import GHC.TypeLits (TypeError, ErrorMessage(..))
import GHC.Exts

type Contains book field exp = Contains' book field book exp

type family Contains' book field orig exp :: Constraint where
   Contains' '[] field '[] exp = TypeError (Text "The provided Book is empty!")
   Contains' '[] field orig exp
      = TypeError (Text "The provided Book does not contain the field "
              :<>: ShowType field
              :$$: Text "Book type:"
              :$$: ShowType orig
              )
   Contains' ((k Map.:-> v) ': m) k orig exp = (v ~ exp)
   Contains' (any ': m) k     orig exp = Contains' m k orig exp
