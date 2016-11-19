module Main where

import Bookkeeper

type LongRecord = Book
  '[ "h" :=> Bool
   , "b" :=> Bool
   , "o" :=> Bool
   , "n" :=> Bool
   , "d" :=> Bool
   , "c" :=> Bool
   , "l" :=> Bool
   , "g" :=> Bool
   , "a" :=> Bool
   , "f" :=> Bool
   , "k" :=> Bool
   , "m" :=> Bool
   , "i" :=> Bool
   , "j" :=> Bool
   ]

main :: IO ()
main = print (t ?: #o)
  where
    t = emptyBook
          & #a =: True
          & #b =: True
          & #c =: True
          & #d =: True
          & #e =: True
          & #f =: True
          & #g =: True
          & #h =: True
          & #i =: True
          & #j =: True
          & #k =: True
          & #l =: True
          & #m =: True
          & #n =: True
          & #o =: True
