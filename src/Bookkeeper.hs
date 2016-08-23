module Bookkeeper
  (
  -- * Preamble
-- | The examples here presume the following setup:
--
-- >>> import Data.Char (toUpper)
-- >>> type Person = Book '[ "name" :=> String , "age" :=> Int ]
-- >>> let julian :: Person = emptyBook & #age =: 28 & #name =: "Julian K. Arni"
--
-- The OverloadedLabels and TypeOperators extensions are also required.

  -- * Initialization
   emptyBook
  -- * Getters
  , (?:)
  , get
  -- * Setters
  , set
  , (=:)

  -- * Modifying
  , modify
  , (%:)

  -- * Types
  , Book
  , (:=>)
  , Key
  -- * Re-exports
  , (&)

  -- * For coercion
  -- | These types should not be used, but need to be in scope for coercion,
  -- which is used when setting or modifying a value.
  , ChooseFirst(..)
  ) where

import Bookkeeper.Internal
import Data.Function
