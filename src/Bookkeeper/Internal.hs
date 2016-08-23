{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Bookkeeper.Internal where

import GHC.OverloadedLabels
import GHC.Generics (Generic)
import qualified Data.Type.Map as Map
import GHC.TypeLits (Symbol)
import Data.Kind (Type)
import Data.Type.Map (Map, Mapping((:->)))

-- Using a type synonym allows the user to write the fields in any order, and
-- yet have the underlying value always have sorted fields.
type Book a = Book' (Map.AsMap a)
data Book' (a :: [Mapping Symbol Type]) = Book { getBook :: Map a }

emptyBook :: Book '[]
emptyBook = Book Map.Empty

type a :=> b = a ':-> b

instance Monoid (Book' '[]) where
  mempty = emptyBook
  _ `mappend` _ = emptyBook

instance (s ~ s') => IsLabel s (Key s') where
  fromLabel _ = Key

data Key (a :: Symbol) = Key
  deriving (Eq, Show, Read, Generic)

get :: forall field book val. (Map.Submap '[field :=> val] book, Map.Lookup book field ~ 'Just val )
  => Key field -> Book' book -> val
get _ (Book bk) = case (Map.submap bk :: Map '[field :=> val]) of
        Map.Ext _ v Map.Empty -> v

-- | Flipped and infix version of 'get'.
--
-- >>> julian ?: #name
-- "Julian K. Arni"
(?:) :: forall field book val. (Map.Submap '[field :=> val] book, Map.Lookup book field ~ 'Just val )
  => Book' book -> Key field -> val
(?:) = flip get

-- | Sets or updates a field to a value.
--
-- >>> let julian' = set #likesDoctest True julian
-- >>> get #likesDoctest julian'
-- True
set :: forall field val old .
  ( Map.Unionable '[field :=> val] (MapMapT (Do ChooseFirst) old)
  , MapMap (Do ChooseFirst) old
  , MapMap (Undo ChooseFirst) (Map.Union '[field :=> val] (MapMapT (Do ChooseFirst) old))
  )
  => Key field -> val -> Book' old -> Book'
    (MapMapT (Undo ChooseFirst) (Map.Union '[field :=> val] (MapMapT (Do ChooseFirst) old)))
set _ v (Book bk)
    = Book $ mapMap (Undo getChooseFirst)
           $ Map.union new
           $ mapMap (Do ChooseFirst) bk
  where
    new = Map.Ext (Map.Var :: Map.Var field) v Map.Empty


(=:) :: forall field val old .
  ( Map.Unionable '[field :=> val] (MapMapT (Do ChooseFirst) old)
  , MapMap (Do ChooseFirst) old
  , MapMap (Undo ChooseFirst) (Map.Union '[field :=> val] (MapMapT (Do ChooseFirst) old))
  )
  => Key field -> val -> Book' old -> Book'
    (MapMapT (Undo ChooseFirst) (Map.Union '[field :=> val] (MapMapT (Do ChooseFirst) old)))
(=:) = set


--

class MapMap f map where
  type MapMapT f map :: [Mapping Symbol Type]
  mapMap :: f -> Map map -> Map (MapMapT f map)


instance MapMap f '[] where
  type MapMapT f '[] = '[]
  mapMap _ m = m

instance MapMap (Do f) rest => MapMap (Do f) (k :=> a ': rest)   where
  type MapMapT (Do f) (k :=> a ': rest) = k :=> f a ': (MapMapT (Do f) rest)

  mapMap (Do f) (Map.Ext k v rest) = Map.Ext k (f v) $ mapMap (Do f) rest

data Undo f = Undo (forall a. f a -> a)
data Do   f = Do (forall a. a -> f a)

data ChooseFirst a = ChooseFirst { getChooseFirst :: a }
 deriving (Eq, Show, Read, Generic)

instance Map.Combinable a (ChooseFirst b) where
  combine a _ = ChooseFirst a

type instance Map.Combine a (ChooseFirst b) = ChooseFirst a


-- $setup
-- >>> import Data.Function ((&))
-- >>> type Person = Book '[ "name" :=> String , "age" :=> Int ]
-- >>> let julian :: Person = emptyBook & #age =: 28 & #name =: "Julian K. Arni"
