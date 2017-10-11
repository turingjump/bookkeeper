{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Bookkeeper.Lens where

import GHC.OverloadedLabels
import Bookkeeper.Internal
import GHC.TypeLits (Symbol)

-- * Lenses

-- | Build a lens from a field
--
-- @
-- julian ^. rlens #age
--    = 28
-- @
-- Symbols can also be used directly as lenses, so you probably don't need to use 'rlens':
--
-- @
-- julian ^. #age
--    = 28
-- @
--
-- @
-- julian & #age .~ 29
--    = Book {age = 29, name = "Julian K. Arni"}
-- @
rlens :: (Settable field val' old new, Gettable field old val, Functor f) =>
          Key field -> (val -> f val') -> (Book' old -> f (Book' new))
rlens f = lens (\r -> get f r) (\r v -> set f v r)
  where lens sa sbt afb s = sbt s <$> afb (sa s)

instance (Settable field valnew old new,
          Gettable field old val,
          Functor f,
          s2ft ~ (Book' old -> f (Book' new)))
         => IsLabel (field :: Symbol)
                    ((val -> f valnew) -> s2ft) where
  fromLabel = rlens (Key @field)

