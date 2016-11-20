{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Bookkeeper.Internal where

import Control.Monad.Identity
import GHC.Generics
import Bookkeeper.Internal.Types


-- Using a type synonym allows the user to write the fields in any order, and
-- yet have the underlying value always have sorted fields.
type Book xs = Book' Identity (Sort xs '[])

type Ledger ledger = Ledger' Identity (Sort ledger '[])

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
  BCons (Identity v) BNil -> v
{-# INLINE get #-}

-- | Flipped and infix version of 'get'.
--
-- >>> julian ?: #name
-- "Julian K. Arni"
(?:) :: forall field book val. (Gettable field book val)
  => Book' Identity book -> Key field -> val
(?:) = flip get
infixl 3 ?:
{-# INLINE (?:) #-}

-- * Setters

-- | 'Settable field value old' is a constraint needed to set the the field
-- 'field' to a value of type 'value' in the book of type 'Book old'.
type Settable field value oldBook = Insertable field value oldBook

-- | Sets or updates a field to a value.
--
-- >>> set #likesDoctest True julian
-- Book {age = 28, likesDoctest = True, name = "Julian K. Arni"}
set :: ( Insertable key value old ) => Key key -> value -> Book' Identity old -> Book' Identity (Insert key value old)
set key value = insert key (Identity value)
{-# INLINE set #-}

-- | Infix version of 'set'
--
-- >>> julian & #age =: 29
-- Book {age = 29, name = "Julian K. Arni"}
(=:) :: ( Insertable key value old ) => Key key -> value -> Book' Identity old -> Book' Identity (Insert key value old)
(=:) = set
infix 3 =:
{-# INLINE (=:) #-}

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
{-# INLINE modify #-}

-- | Infix version of 'modify'.
--
-- >>> julian & #name %: fmap toUpper
-- Book {age = 28, name = "JULIAN K. ARNI"}
(%:) :: (Modifiable key originalValue newValue originalBook)
  =>  Key key -> (originalValue -> newValue) -> Book' Identity originalBook
  -> Book' Identity (Insert key newValue originalBook)
(%:) = modify
infixr 3 %:
{-# INLINE (%:) #-}


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
{-# INLINE delete #-}


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
fromRecord :: (Generic a, FromGeneric (Rep a) bookRep) => a -> Book' Identity bookRep
fromRecord = fromGeneric . from

-- $setup
-- >>> import Data.Function ((&))
-- >>> import Data.Char (toUpper)
-- >>> type Person = Book '[ "name" :=> String , "age" :=> Int ]
-- >>> let julian :: Person = emptyBook & #age =: 28 & #name =: "Julian K. Arni"
