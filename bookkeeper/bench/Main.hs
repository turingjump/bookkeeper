module Main where

import Bookkeeper
import Criterion.Main
import GHC.Prim

type PersonB = Book '[ "name" :=> String, "age" :=> Int ]
data PersonR = PersonR { name :: !String, age :: {-# NOUNPACK #-} !Int } deriving (Eq, Show)

pb :: PersonB
pb = emptyBook
   & #name =: ""
   & #age =: 0

pr :: PersonR
pr = PersonR
   { name = ""
   , age = 0
   }

modB :: Int -> PersonB
modB 0 = pb
modB n = modB (n - 1) & #age %: (+ n)

modR :: Int -> PersonR
modR 0 = pr
modR n = let p = modR (n - 1) in p { age = age p + n }

main :: IO ()
main = defaultMain
  [ bgroup "modB" [ bench "10" $ nf (\x -> get #age $ modB x) 10
                  , bench "100" $ nf (\x -> get #age $ modB x) 100
                  ]
  , bgroup "modR" [ bench "10" $ nf (\x -> age $ modR x) 10
                  , bench "100" $ nf (\x -> age $ modR x) 100
                  ]
  ]
