{-# OPTIONS_GHC -O -ddump-rule-firings #-}
-- {-# OPTIONS_GHC -ddump-simpl #-}
module Main (main) where

import Bookkeeper
{-import Criterion.Main-}

type PersonB = Book '[ "name" :=> String, "age" :=> Int ]
{-type PersonB = Book '[ "age" :=> Int ]-}
{-data PersonR = PersonR { name :: !String, age :: [># NOUNPACK #<] !Int } deriving (Eq, Show)-}

pb :: PersonB
pb = emptyBook
   & #name =: ""
   & #age =: 0

{-pr :: PersonR-}
{-pr = PersonR-}
   {-{ name = ""-}
   {-, age = 0-}
   {-}-}

{-modB :: Int -> PersonB-}
{-modB 0 = pb-}
{-modB n = modify #age (+ n) $ modB (n - 1)-}

{-modR :: Int -> PersonR-}
{-modR 0 = pr-}
{-modR n = let p = modR (n - 1) in p { age = age p + n }-}

fusionB :: PersonB
fusionB = (modify #name ('c':) (modify #name ('a':) pb))


fusionB' :: PersonB
fusionB' =
  let l x = (modify #name ('c':) (modify #name ('a':) x))
  in l pb
{-fusionR :: PersonR -> PersonR-}
{-fusionR = go . go . go . go . go . go . go . go . go . go . go . go . go-}
  {-where-}
    {-go p = p { age = succ $ age p  }-}


main :: IO ()
main = do
  print $ get #age fusionB
  print $ get #age fusionB'
  {-[ bgroup "modB" [ bench "10" $ nf (\x -> get #age $ modB x) 10-}
                  {-, bench "100" $ nf (\x -> get #age $ modB x) 100-}
                  {-]-}
  {-, bgroup "modR" [ bench "10" $ nf (\x -> age $ modR x) 10-}
                  {-, bench "100" $ nf (\x -> age $ modR x) 100-}
                  {-]-}
  {-[ bgroup "fusion" [ bench "fusion: Book" $ nf (\x -> get #age $ fusionB x) pb-}
                    {-, bench "fusion: Record" $ nf (\x -> age $ fusionR x) pr-}
                    {-]-}
  {-]-}
