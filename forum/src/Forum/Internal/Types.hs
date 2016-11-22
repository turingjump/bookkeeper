{-# LANGUAGE DeriveAnyClass #-}
module Forum.Internal.Types where

import GHC.TypeLits
import GHC.Generics (Generic)
import Hasql.Class (Encodable, Decodable)
import Hasql.Pool (Pool)
import qualified Database.HsSqlPpp.Catalog as Sql

newtype PrimaryKey (tbl :: Symbol) val = PrimaryKey val
  deriving (Eq, Show, Read, Generic, Ord, Encodable, Decodable)

newtype ForeignKey (tbl :: Symbol) val = ForeignKey val
  deriving (Eq, Show, Read, Generic, Ord, Encodable, Decodable)

data DB = DB
  { dbCatalog :: Sql.Catalog
  , dbName :: String
  , dbConnectionPool :: Pool
  }
