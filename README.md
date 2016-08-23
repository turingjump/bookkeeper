# Bookkeeper

Bookkeeper is a new Haskell library that uses the new OverlodaedLabels feature
of GHC 8 to provide a new take on datatypes and records:

~~~ {.haskell}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
import Bookkeeper


jane :: Book '[ "name" :=> String, "age" :=> Int ]
jane = emptyBook
     & #name =: "Jane"
     & #age =: 30

-- >>> jane ?: #name
-- "Jane"
~~~

Accesing a field that does not exist is a type error, made nicer with GHCs new
custom type errors:

~~~ {.haskell}
 -- >>> jane ?: #address
--   • The provided Book does not contain the field "address"
--     Book type:
--     '["age" ':-> Int, "name" ':-> String]
~~~

~~~ {.haskell}
main :: IO ()
main = return ()
~~~
