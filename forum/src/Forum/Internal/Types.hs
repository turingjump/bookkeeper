{-# LANGUAGE DeriveAnyClass #-}
module Forum.Internal.Types where

import GHC.TypeLits
import GHC.Generics (Generic)
import Hasql.Class (Encodable, Decodable)

newtype PrimaryKey (tbl :: Symbol) val = PrimaryKey val
  deriving (Eq, Show, Read, Generic, Ord, Encodable, Decodable)


newtype ForeignKey (tbl :: Symbol) val = ForeignKey val
  deriving (Eq, Show, Read, Generic, Ord, Encodable, Decodable)
