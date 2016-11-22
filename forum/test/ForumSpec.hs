{-# LANGUAGE QuasiQuotes #-}
module ForumSpec (spec) where

import Forum
import Schema
import qualified Data.Text as T
import Test.Hspec

spec :: Spec
spec = describe "forum" $ around withEmptyDb $ do
  let discoverer = emptyBook & #discovererId =: 1
                             & #firstName =: "Carl"
                             & #lastName =: "Linnaeus"

  it "allows inserting and querying" $ \db -> do
    runSql [sql| INSERT INTO discoverer VALUES $discoverer |]
    result <- runSql [sql| SELECT (firstName) FROM discoverer |]
    result `shouldBe` [subSet discoverer]

  it "allows WHERE clauses" $ \db -> do
    runSql [sql| INSERT INTO discoverer VALUES $discoverer |]
    result <- runSql [sql| SELECT (firstName) FROM discoverer WHERE lastName = 0 |]
    result `shouldBe` []

  it "types WHERE on primary keys as Maybe" $ \db -> do
    runSql [sql| INSERT INTO discoverer VALUES $discoverer |]
    result <- runSql [sql| SELECT (firstName) FROM discoverer WHERE discovererId = 0 |]
    result `shouldBe` Nothing
