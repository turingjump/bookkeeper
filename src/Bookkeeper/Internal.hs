{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Bookkeeper.Internal where

import GHC.TypeLits (CmpSymbol)
import Data.Type.Equality (type (==))
import Data.Proxy
import Control.Monad.Identity

import Bookkeeper.Internal.Types


-- Using a type synonym allows the user to write the fields in any order, and
-- yet have the underlying value always have sorted fields.
type Book xs = Book' Identity (Sort xs '[])


------------------------------------------------------------------------------
-- Setters and getters
------------------------------------------------------------------------------

-- * Getters

-- | @Gettable field val book@ is the constraint needed to get a value of type
-- @val@ from the field @field@ in the book of type @Book book@.
type Gettable field book val = (Subset book '[ field :=> val ])

-- | Get a value by key, if it exists.
--
-- >>> get #age julian
-- 28
--
-- If the key does not exist, throws a type error
-- >>> get #moneyFrom julian
-- ...
-- ...  • The provided Book does not contain the field "moneyFrom"
-- ...    Book type:
-- ...    '["age" ':-> Int, "name" ':-> String]
-- ...  • In the expression: get #moneyFrom julian
-- ...
get :: forall field book val. (Gettable field book val)
  => Key field -> Book' Identity book -> val
get _ bk = case (getSubset bk :: Book' Identity '[field :=> val]) of
  BCons _ (Identity v) BNil -> v

-- | Flipped and infix version of 'get'.
--
-- >>> julian ?: #name
-- "Julian K. Arni"
(?:) :: forall field book val. (Gettable field book val)
  => Book' Identity book -> Key field -> val
(?:) = flip get
infixl 3 ?:

-- * Setters

-- | 'Settable field val old new' is a constraint needed to set the the field
-- 'field' to a value of type 'val' in the book of type 'Book old'. The
-- resulting book will have type 'Book new'.

-- | Sets or updates a field to a value.
--
-- >>> set #likesDoctest True julian
-- Book {age = 28, likesDoctest = True, name = "Julian K. Arni"}
set :: ( Insertable key value old ) => Key key -> value -> Book' Identity old -> Book' Identity (Insert key value old)
set key value = insert key (Identity value)

-- | Infix version of 'set'
--
-- >>> julian & #age =: 29
-- Book {age = 29, name = "Julian K. Arni"}
(=:) :: ( Insertable key value old ) => Key key -> value -> Book' Identity old -> Book' Identity (Insert key value old)
(=:) = set
infix 3 =:

-- * Modifiers

-- | @Modifiable field val val' old new@ is a constraint needed to apply a
-- function of type @val -> val'@ to the field @field@ in the book of type
-- @Book old@. The resulting book will have type @Book new@.
type Modifiable field originalValue newValue originalBook =
  ( Gettable field originalBook originalValue
  , Insertable field newValue originalBook
  )

-- | Apply a function to a field.
--
-- >>> julian & modify #name (fmap toUpper)
-- Book {age = 28, name = "JULIAN K. ARNI"}
--
-- If the key does not exist, throws a type error
-- >>> modify #height (\_ -> 132) julian
-- ...
-- ...  • The provided Book does not contain the field "height"
-- ...    Book type:
-- ...    '["age" ':-> Int, "name" ':-> String]
-- ...  • In the expression: modify #height (\ _ -> 132) julian
-- ...
modify :: (Modifiable key originalValue newValue originalBook)
  =>  Key key -> (originalValue -> newValue) -> Book' Identity originalBook
  -> Book' Identity (Insert key newValue originalBook)
modify p f b = set p v b
  where v = f $ get p b

-- | Infix version of 'modify'.
--
-- >>> julian & #name %: fmap toUpper
-- Book {age = 28, name = "JULIAN K. ARNI"}
(%:) :: (Modifiable key originalValue newValue originalBook)
  =>  Key key -> (originalValue -> newValue) -> Book' Identity originalBook
  -> Book' Identity (Insert key newValue originalBook)
(%:) = modify
infixr 3 %:


type Deletable key oldBook = Subset oldBook (Delete key oldBook)
-- | Delete a field from a 'Book', if it exists. If it does not, returns the
-- @Book@ unmodified.
--
-- >>> get #name $ delete #name julian
-- ...
-- ...  • The provided Book does not contain the field "name"
-- ...    Book type:
-- ...    '["age" ':-> Int]
-- ...  • In the expression: get #name
-- ...
delete :: forall key oldBook f .
        ( Deletable key oldBook
        ) => Key key -> Book' f oldBook -> Book' f (Delete key oldBook)
delete _ bk = getSubset bk

{-


-- | Generate a @Book@ from an ordinary Haskell record via GHC Generics.
--
-- >>> data Test = Test {  field1 :: String, field2 :: Int, field3 :: Char } deriving Generic
-- >>> fromRecord (Test "hello" 0 'c')
-- Book {field1 = "hello", field2 = 0, field3 = 'c'}
--
-- Trying to convert a datatype which is not a record will result in a type
-- error:
--
-- >>> data SomeSumType = LeftSide | RightSide deriving Generic
-- >>> fromRecord LeftSide
-- ...
-- ... • Cannot convert sum types into Books
-- ...
--
-- >>> data Unit = Unit deriving Generic
-- >>> fromRecord Unit
-- ...
-- ... • Cannot convert non-record types into Books
-- ...
fromRecord :: (Generic a, FromGeneric (Rep a) bookRep) => a -> Book' bookRep
fromRecord = fromGeneric . from

-- $setup
-- >>> import Data.Function ((&))
-- >>> import Data.Char (toUpper)
-- >>> type Person = Book '[ "name" :=> String , "age" :=> Int ]
-- >>> let julian :: Person = emptyBook & #age =: 28 & #name =: "Julian K. Arni"
-}

------------------------------------------------------------------------------
-- Internal stuff
------------------------------------------------------------------------------

-- Insertion sort for simplicity.
type family Sort unsorted sorted where
   Sort '[] sorted = sorted
   Sort (key :=> value ': xs) sorted = Sort xs (Insert key value sorted)

type family Insert key value oldMap where
  Insert key value '[] = '[ key :=> value ]
  Insert key value (key :=> someValue ': restOfMap) = (key :=> value ': restOfMap)
  Insert key value (focusKey :=> someValue ': restOfMap)
    = Ifte (CmpSymbol key focusKey == 'LT)
         (key :=> value ': focusKey :=> someValue ': restOfMap)
         (key :=> value ': focusKey :=> someValue ': restOfMap)

type family Ifte cond iftrue iffalse where
  Ifte 'True iftrue iffalse = iftrue
  Ifte 'False iftrue iffalse = iffalse

------------------------------------------------------------------------------
-- Subset
------------------------------------------------------------------------------

class Subset set subset where
  getSubset :: Book' f set -> Book' f subset

instance Subset '[] '[] where getSubset = id
instance (Subset tail1 tail2) => Subset (head ': tail1) (head ': tail2) where
  getSubset (BCons key value oldBook) = BCons key value $ getSubset oldBook
instance (Subset tail subset) => Subset (head ': tail) subset where
  getSubset (BCons _key _value oldBook) = getSubset oldBook

------------------------------------------------------------------------------
-- Insertion
------------------------------------------------------------------------------

class Insertable key value oldMap where
  insert :: Key key -> f value -> Book' f oldMap -> Book' f (Insert key value oldMap)

instance Insertable key value '[] where
  insert key value oldBook = BCons key value oldBook

instance Insertable key value (key :=> someValue ': restOfMap) where
  insert key value (BCons _ _ oldBook) = BCons key value oldBook

instance
  ( Insertable' (CmpSymbol key oldKey) key value
     (oldKey :=> oldValue ': restOfMap)
     (Insert key value (oldKey :=> oldValue ': restOfMap))
  ) => Insertable key value (oldKey :=> oldValue ': restOfMap) where
  insert key value oldBook = insert' flag key value oldBook
    where
      flag :: Proxy (CmpSymbol key oldKey)
      flag = Proxy

class Insertable' flag key value oldMap newMap where
  insert' :: Proxy flag -> Key key -> f value -> Book' f oldMap -> Book' f newMap

instance Insertable' 'LT key value
  oldMap
  (key :=> value ': oldMap) where
  insert' _ key value oldBook = BCons key value oldBook
instance Insertable' 'EQ key value
  (oldKey :=> oldValue ': restOfMap)
  (key :=> value ': restOfMap) where
  insert' _ key value (BCons _ _ oldBook) = BCons key value oldBook
instance (newMap ~ Insert key value restOfMap, Insertable key value restOfMap) => Insertable' 'GT key value
  (oldKey :=> oldValue ': restOfMap)
  (oldKey :=> oldValue ': newMap) where
  insert' _ key value (BCons oldKey oldValue oldBook) = BCons oldKey oldValue (insert key value oldBook)

type family Delete keyToDelete oldBook where
  Delete keyToDelete (keyToDelete :=> someValue ': xs) = xs
  Delete keyToDelete (anotherKey :=> someValue ': xs)
    = (anotherKey :=> someValue ': Delete keyToDelete xs)
  Delete keyToDelete '[] = '[]
