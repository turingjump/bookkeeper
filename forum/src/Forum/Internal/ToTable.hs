{-# LANGUAGE PolyKinds #-}
module Forum.Internal.ToTable where

import Bookkeeper
import Data.Bifunctor (second)
import Data.Reflection (Reifies(..))
import Data.Int
import Data.Proxy (Proxy(..))
import GHC.TypeLits
import qualified Data.Text as T
import qualified Database.HsSqlPpp.Types as Sql
import qualified Database.HsSqlPpp.Catalog as Sql

import Forum.Internal.Types

-- * SqlValue



class SqlValue (haskellType :: *) where
  sqlType :: Proxy haskellType -> Sql.Type
  isNullable :: Proxy haskellType -> Bool
  isNullable _ = False

instance SqlValue a => SqlValue (PrimaryKey tbl a) where
  sqlType _ = sqlType (Proxy :: Proxy a)

instance SqlValue a => SqlValue (ForeignKey tbl a) where
  sqlType _ = sqlType (Proxy :: Proxy a)
  isNullable _ = isNullable (Proxy :: Proxy a)

instance SqlValue a => SqlValue (Maybe a) where
  sqlType _ = sqlType (Proxy :: Proxy a)
  isNullable _ = True

instance SqlValue Bool where sqlType _ = Sql.ScalarType "bool"
instance SqlValue T.Text where sqlType _ = Sql.ScalarType "varchar"
instance SqlValue Int where sqlType _ = Sql.ScalarType "bigint"
{-instance SqlValue String where sqlType _ = Sql.ScalarType "varchar"-}

-- * HasSqlValue

class (Reifies (SqlType haskellType) Sql.Type) => HasSqlValue (haskellType :: *) where
  type SqlType haskellType
  type IsNullable haskellType :: Bool
  type IsNullable haskellType = 'False

instance HasSqlValue T.Text where type SqlType T.Text = Scalar "varchar"
instance HasSqlValue String where type SqlType String = Scalar "varchar"
instance HasSqlValue Bool   where type SqlType Bool   = Scalar "bool"
instance HasSqlValue Int16  where type SqlType Int16  = Scalar "smallint"
instance HasSqlValue Int32  where type SqlType Int32  = Scalar "integer"
instance HasSqlValue Int64  where type SqlType Int64  = Scalar "bigint"
instance HasSqlValue Float  where type SqlType Float  = Scalar "real"
instance HasSqlValue Double where type SqlType Double = Scalar "double"
instance (HasSqlValue a, IsNullable a ~ 'False) => HasSqlValue (Maybe a) where
  type SqlType (Maybe a) = SqlType a
  type IsNullable (Maybe a) = 'True

data Scalar (s :: Symbol)

instance KnownSymbol s => Reifies (Scalar s) Sql.Type where
  reflect _ = Sql.ScalarType (T.pack $ symbolVal (Proxy :: Proxy s))


toSqlType :: forall a. HasSqlValue a => Proxy a -> Sql.Type
toSqlType _ = reflect (Proxy :: Proxy (SqlType a))

-- * ToTable

class ToTable (a :: [*]) where
  toTable :: Proxy a -> [(T.Text, Sql.Type)]

instance (HasSqlValue fieldVal, KnownSymbol fieldName)
  => ToTable '[ fieldName :=> fieldVal ] where
  toTable _ = [(T.pack $ symbolVal (Proxy :: Proxy fieldName)
              , toSqlType (Proxy :: Proxy fieldVal))]

instance (HasSqlValue fieldVal, ToTable (snd ': restOfTable), KnownSymbol fieldName)
  => ToTable (fieldName :=> fieldVal ': snd ': restOfTable) where
  toTable _
    = ( T.pack $ symbolVal (Proxy :: Proxy fieldName)
      , toSqlType (Proxy :: Proxy fieldVal))
     : toTable (Proxy :: Proxy (snd ': restOfTable))

-- * ToCatalogUpdate

toCatalog :: ToCatalogUpdate a => Proxy a -> Either [Sql.TypeError] Sql.Catalog
toCatalog p = Sql.updateCatalog (toCatalogUpdate p) undefined

class ToCatalogUpdate (a :: [*]) where
  toCatalogUpdate :: Proxy a -> [Sql.CatalogUpdate]

instance {-# OVERLAPPING #-} ToCatalogUpdate '[] where
  toCatalogUpdate _ = []

instance {-# OVERLAPPABLE #-}
  (KnownSymbol tableName, ToTable table, ToCatalogUpdate rest)
  => ToCatalogUpdate ( tableName :=> Book' f table ': rest) where
  toCatalogUpdate _ = Sql.CatCreateTable tableName columns
                    : toCatalogUpdate (Proxy :: Proxy rest)
    where
      tableName = ("public", T.pack $ symbolVal (Proxy :: Proxy tableName))

      columns  = second typeToCatNameExtra <$> toTable (Proxy :: Proxy table)

      typeToCatNameExtra :: Sql.Type -> Sql.CatNameExtra
      typeToCatNameExtra (Sql.ScalarType t) = Sql.mkCatNameExtra t
