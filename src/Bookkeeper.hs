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
  ) where

import Bookkeeper.Internal
import Data.Function
