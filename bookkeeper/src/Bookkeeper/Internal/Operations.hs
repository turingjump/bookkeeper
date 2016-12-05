{-# LANGUAGE PolyKinds #-}
module Bookkeeper.Internal.Operations where

import Bookkeeper.Internal.Types
import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits (symbolVal, KnownSymbol)
import Data.Functor.Identity
import Data.Functor.Const

-- | Maps a natural transformation over every record.
bmap :: (forall x. f x -> g x) -> Book' f entries -> Book' g entries
bmap _ BNil = BNil
bmap nat (BCons value rest) = BCons (nat value) (bmap nat rest)

-- | Map a class method over every record.
bmapConstraint :: All c entries => Proxy c -> (forall x . c x => f x -> g x) -> Book' f entries -> Book' g entries
bmapConstraint _ _   BNil = BNil
bmapConstraint p nat (BCons value rest) = BCons (nat value) (bmapConstraint p nat rest)

-- | Collapse a map into a list.
bcollapse :: Book' (Const a) entries -> [a]
bcollapse BNil = []
bcollapse (BCons (Const h) rest) = h : bcollapse rest

-- | Collapse a map, including the keys.
bcollapseWithKeys :: forall a entries. BKeys entries => Book' (Const a) entries -> [(String, a)]
bcollapseWithKeys b = zip (bkeys (Proxy :: Proxy entries)) (bcollapse b)

class BKeys entries where
  bkeys :: Proxy entries -> [String]

instance BKeys '[] where
  bkeys _ = []

instance (KnownSymbol key, BKeys rest) => BKeys (key :=> val ': rest) where
  bkeys _ = symbolVal (Proxy :: Proxy key) : bkeys (Proxy :: Proxy rest)

-- | Analogous to 'Data.Traversable.sequence'.
bsequence :: Monad m => Book' m entries -> m (Book' Identity entries)
bsequence BNil = return BNil
bsequence (BCons mvalue mrest) = do
  value <- mvalue
  rest <- bsequence mrest
  return $ BCons (return value) rest

-- Make a book filled with @Proxy@s.
bproxies :: Book' Proxy entries
bproxies = bmap (const Proxy) undefined

