module SumErrorSpec (spec) where

import Data.Functor.Identity
import SumError
import Test.Hspec

spec :: Spec
spec = describe "SumErrorT" $ do

  it "allows throwing and catching in order errors" $ do
    let fn n = resolve
         $ catchSumError #notLarge (\_ -> return 200)
         $ catchSumError #notPositive (\_ -> return 202)
         $ catchSumError #notEven (\_ -> return 204)
         $ eg n
    fn 1 `shouldBe` Identity 204
    fn (-2) `shouldBe` Identity 202
    fn 10 `shouldBe` Identity 200

eg :: ( MonadSumError m "notPositive" ()
      , MonadSumError m "notEven" ()
      , MonadSumError m "notLarge" String
      ) => Int -> m Int
eg n
  | n <= 0         = throwSumError #notPositive ()
  | n < 100        = throwSumError #notLarge
                   $ "Number " ++ show n ++ " ought to be larger than 100"
  | n `mod` 2 == 1 = throwSumError #notEven ()
  | otherwise      = return $ n `div` 2

