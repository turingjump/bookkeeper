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
  -- * Re-exports
  , (&)

  , ChooseFirst(..)
  ) where

import Bookkeeper.Internal
import Data.Function
