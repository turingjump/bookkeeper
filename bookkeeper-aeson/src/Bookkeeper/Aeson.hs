{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Bookkeeper.Aeson where

import           Bookkeeper
import           Bookkeeper.Internal.Types
import           Data.Aeson
import           Data.Bifunctor
import           Data.Proxy
import qualified Data.Text                 as T
import           GHC.TypeLits

-- | @ToJSON@ instance for @Book@s. Does the obvious thing:
--
-- >>> encode julian
-- "{\"age\":28,\"name\":\"Julian K. Arni\"}"
instance (All (ToJSON `Compose` f) a, BKeys a) => ToJSON (Book' f a) where
  toJSON b = object $ first T.pack <$> list
    where
      list = bcollapseWithKeys
           $ bmapConstraint (Proxy :: Proxy (ToJSON `Compose` f)) (Const . toJSON) b

-- | @FromJSON@ instance for @Book@s. Does the obvious thing:
--
-- >>> decode "{\"age\":28,\"name\":\"Julian K. Arni\"}" :: Maybe Person
-- Just Book {#age = Identity 28, #name = Identity "Julian K. Arni"}
instance {-# OVERLAPPING #-}
  (KnownSymbol key, FromJSON (f value))
  => FromJSON (Book' f '[key :=> value]) where
  parseJSON (Object v) = go <$> v .: key
    where
      key = T.pack $ symbolVal (Proxy :: Proxy key)
      go x = BCons x BNil
  parseJSON _ = fail "expecting object"

instance {-# OVERLAPPABLE #-}
  (KnownSymbol key, FromJSON (f value), FromJSON (Book' f rest))
  => FromJSON (Book' f (key :=> value ': rest)) where
  parseJSON o@(Object v) = go $ v .: key
    where
      key = T.pack $ symbolVal (Proxy :: Proxy key)
      go x = BCons <$> x <*> parseJSON o
  parseJSON _ = fail "expecting object"

-- $setup
-- >>> import Data.Function ((&))
-- >>> type Person = Book '[ "name" :=> String , "age" :=> Int ]
-- >>> let julian :: Person = emptyBook & #age =: 28 & #name =: "Julian K. Arni"
