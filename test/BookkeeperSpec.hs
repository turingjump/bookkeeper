module BookkeeperSpec (spec) where

import Data.Char (toUpper)
import Test.Hspec

import Bookkeeper

spec :: Spec
spec = describe "books" $ do
    let p :: Person
           = emptyBook
           & #name =: "Julian K. Arni"
           & #age =: 28

    it "allows creation" $ do
      typeLevelTest

    it "allows creation out of order" $ do
      let _p :: Person
             = emptyBook
             & #age  =:  28
             & #name =: "Julian K. Arni"
      typeLevelTest

    it "allows access" $ do
      get #name p  `shouldBe` "Julian K. Arni"

    it "allows update" $ do
      let p' :: Person
             = p & #name %: fmap toUpper
      get #name p' `shouldBe` "JULIAN K. ARNI"

    it "allows extension" $ do
      let p' = #email =: "jkarni<at>turingjump<dot>com" $ p
      get #email p' `shouldBe` ("jkarni<at>turingjump<dot>com" :: String)

    it "allows deleting" $ do
      let _p = delete #name p
      typeLevelTest

    it "allows nested updates" $ do
      let timeTravelingJulian = p & #child =: p
          p' = timeTravelingJulian & #child %: #name %: fmap toUpper
      p' ?: #name `shouldBe` "Julian K. Arni"
      p' ?: #child ?: #name `shouldBe` "JULIAN K. ARNI"

    it "has a decent show instance" $ do
      show p `shouldBe` "Book {age = 28, name = \"Julian K. Arni\"}"

type Person = Book '[ "name" :=> String , "age" :=> Int]


typeLevelTest :: Expectation
typeLevelTest = True `shouldBe` True
