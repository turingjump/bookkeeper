module BookkeeperSpec (spec) where

import Data.Char (toUpper)
import Test.Hspec

import Bookkeeper

spec :: Spec
spec = describe "books" $ do

    it "allows creation" $ do
      let _p :: Person
             = emptyBook
             & #name =: "Julian K. Arni"
             & #age  =:  28
      typeLevelTest

    it "allows creation out of order" $ do
      let _p :: Person
             = emptyBook
             & #age  =:  28
             & #name =: "Julian K. Arni"
      typeLevelTest

    it "allows access" $ do
      let p :: Person
             = emptyBook
             & #name =: "Julian K. Arni"
             & #age  =:  28
      get #name p  `shouldBe` "Julian K. Arni"

    it "allows update" $ do
      let p :: Person
             = emptyBook
             & #name =: "Julian K. Arni"
             & #age =: 28
             & #name %: fmap toUpper
      get #name p `shouldBe` "JULIAN K. ARNI"

    it "allows extension" $ do
      let p :: Person
             = emptyBook
             & #name =: "Julian K. Arni"
             & #age =: 28
      let p' = #email =: "jkarni<at>turingjump<dot>com" $ p
      get #email p' `shouldBe` ("jkarni<at>turingjump<dot>com" :: String)

type Person = Book '[ "name" :=> String , "age" :=> Int]

typeLevelTest :: Expectation
typeLevelTest = True `shouldBe` True
