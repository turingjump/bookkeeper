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
--
-- A word of warning: The signatures for most of the functions are quite
-- arcane, even though their behaviour is intuitive.

  -- * Initialization
   emptyBook

  -- * Getters
  , Gettable
  , (?:)
  , get

  -- * Setters
  , Settable
  , set
  , (=:)

  -- * Modifying
  , Modifiable
  , modify
  , (%:)

  -- * Union
  , Unionable
  , union

  -- * Sorting
  , Sorted
  , sorted

  -- * Deleting
  , delete
  , Delete

  -- * Ledger
  , Ledger

  -- ** Split
  , Split
  , split
  , getIf

  -- ** Split
  , getSubset
  , Subset

  -- ** Option
  , Optionable
  , option

  -- * Types
  , Book
  , Book'
  , (:=>)
  , Key

  -- * Operations
  , bmap
  , bmapConstraint
  , bcollapse
  , bcollapseWithKeys
  , BKeys(bkeys)
  , bsequence
  , bproxies
  , All
  , All2
  , And
  , IsEqTo

  -- * From Haskell record
  , fromRecord

  -- * Re-exports
  , (&)
  , Const(..)
  , Identity(..)

  ) where

import Bookkeeper.Internal
import Bookkeeper.Internal.Types
import Data.Function
import Data.Functor.Const
import Data.Functor.Identity
