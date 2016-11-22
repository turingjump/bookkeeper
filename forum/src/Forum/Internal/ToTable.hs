{-# LANGUAGE PolyKinds #-}
module Forum.Internal.ToTable where

import Bookkeeper
import Data.Bifunctor (second)
import Hasql.Class (Encodable, Decodable)
import Data.Proxy (Proxy(..))
import GHC.TypeLits
import qualified Data.Text as T
import qualified Database.HsSqlPpp.Types as Sql
import qualified Database.HsSqlPpp.Catalog as Sql

import Forum.Internal.Types

-- * SqlValue

class (Encodable a, Decodable a) => SqlValue (a :: *) where
  sqlType :: Proxy a -> Sql.Type

instance SqlValue a => SqlValue (PrimaryKey tbl a) where
  sqlType _ = sqlType (Proxy :: Proxy a)
instance SqlValue a => SqlValue (ForeignKey tbl a) where
  sqlType _ = sqlType (Proxy :: Proxy a)

instance SqlValue Bool where sqlType _ = Sql.ScalarType "bool"
instance SqlValue T.Text where sqlType _ = Sql.ScalarType "varchar"
{-instance SqlValue String where sqlType _ = Sql.ScalarType "varchar"-}


-- * ToTable

class ToTable (a :: [*]) where
  toTable :: Proxy a -> [(T.Text, Sql.Type)]

instance (SqlValue fieldVal, KnownSymbol fieldName)
  => ToTable '[ fieldName :=> (fieldVal :: *) ] where
  toTable _ = [(T.pack $ symbolVal (Proxy :: Proxy fieldName)
              , sqlType (Proxy :: Proxy fieldVal))]

instance (SqlValue fieldVal, ToTable restOfTable, KnownSymbol fieldName)
  => ToTable (fieldName :=> (fieldVal :: *) ': restOfTable) where
  toTable _
    = ( T.pack $ symbolVal (Proxy :: Proxy fieldName)
      , sqlType (Proxy :: Proxy fieldVal))
     : toTable (Proxy :: Proxy restOfTable)

-- * ToCatalogUpdate

class ToCatalogUpdate (a :: [*]) where
  toCatalogUpdate :: Proxy a -> [Sql.CatalogUpdate]

instance ToCatalogUpdate '[] where
  toCatalogUpdate _ = []

instance (KnownSymbol tableName, ToTable table, ToCatalogUpdate rest)
  => ToCatalogUpdate ( tableName :=> table ': rest) where
  toCatalogUpdate _ = Sql.CatCreateTable tableName columns
                    : toCatalogUpdate (Proxy :: Proxy rest)
    where
      tableName = ("public", T.pack $ symbolVal (Proxy :: Proxy tableName))

      columns  = second typeToCatNameExtra <$> toTable (Proxy :: Proxy table)

      typeToCatNameExtra :: Sql.Type -> Sql.CatNameExtra
      typeToCatNameExtra (Sql.ScalarType t) = Sql.mkCatNameExtra t
