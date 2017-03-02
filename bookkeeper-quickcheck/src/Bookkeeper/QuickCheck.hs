{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Bookkeeper.QuickCheck where

import Test.QuickCheck
import Bookkeeper
import Data.Proxy

-- | A @Book@ packed with generators of the specified type.
--
-- This makes it easy to manipulate the generation. For example, given:
--
-- > type Person = Book '[ "name" :=> String , "age" :=> Int ]
--
-- If we wanted a generator for only people with age 20, we could write:
--
-- >>> let a :: Book' Gen '[ "age" :=> Int, "name" :=> String ] = arbitraryBook
-- >>> generate $ bsequence $ set' #age (return 20) a :: IO Person
arbitraryBook :: All Arbitrary xs => Book' Gen xs
arbitraryBook = bmapConstraint (Proxy :: Proxy Arbitrary) go bproxies
  where
    go :: Arbitrary a => Proxy a -> Gen a
    go _ = arbitrary

instance All Arbitrary xs => Arbitrary (Book' Identity xs) where
  arbitrary = bsequence arbitraryBook

-- $setup
-- >>> import Data.Function ((&))
-- >>> type Person = Book '[ "name" :=> String , "age" :=> Int ]
-- >>> let julian :: Person = emptyBook & #age =: 28 & #name =: "Julian K. Arni"
