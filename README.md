# Bookkeeper [![Build Status][ci-img]][ci]

Bookkeeper is a new Haskell library that uses the new OverloadedLabels feature
of GHC 8 to provide a new take on datatypes and records:

~~~ {.haskell}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
import Bookkeeper


jane :: Book '[ "name" :=> String, "age" :=> Int ]
jane = emptyBook
     & #name =: "Jane"
     & #age =: 30

-- >>> jane
-- Book {age = 30, name = "Jane"}
-- >>> jane ?: #name
-- "Jane"
~~~

It bears some similarities to Nikita Volkov's [record](https://nikita-volkov.github.io/record/)
library, but requires no Template Haskell.

Accesing a field that does not exist is a type error, made nicer with GHCs new
custom type errors:

~~~ {.haskell}
 -- >>> jane ?: #address
--   • The provided Book does not contain the field "address"
--     Book type:
--     '["age" ':-> Int, "name" ':-> String]
~~~


The order in which fields are inserted or appear in types does not matter. That
is, in:

~~~ {.haskell}
-- type A = Book '[ "field1" :=> Int,  "field2" :=> Bool]
-- type B = Book '[ "field2" :=> Bool, "field1" :=> Int ]
~~~

Types `A` and `B` are the same.

You can set, modify, or get fields. See the haddocks for more information.


~~~ {.haskell}
main :: IO ()
main = return ()
~~~

[ci-img]:   https://travis-ci.org/turingjump/bookkeeper.svg?branch=master
[ci]:       https://travis-ci.org/turingjump/bookkeeper
