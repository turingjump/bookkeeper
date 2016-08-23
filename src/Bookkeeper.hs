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

  -- * Updating
  , update
  , (%:)

  -- * Re-exports
  , (&)

  , ChooseFirst(..)
  ) where

import Bookkeeper.Internal
import Data.Function
