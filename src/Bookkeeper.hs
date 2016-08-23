module Bookkeeper
  ( Book
  , emptyBook
  , (:=>)
  , Key
  -- * Getters
  , (?:)
  , get
  -- * Setters
  , set
  , (=:)

  -- * Modifying
  , modify
  , (%:)

  -- * Re-exports
  , (&)

  -- * For coercion
  -- | These types should not be used, but need to be in scope for coercion,
  -- which is used when setting or modifying a value.
  , ChooseFirst(..)
  ) where

import Bookkeeper.Internal
import Data.Function
