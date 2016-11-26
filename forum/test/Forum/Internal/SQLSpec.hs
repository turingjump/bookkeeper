module Forum.Internal.SQLSpec (spec) where

import Forum.Internal.SQL
import Test.Hspec

spec :: Spec
spec = do
  parseSQLSpec

parseSQLSpec :: Spec
parseSQLSpec = describe "parseSQL" $ do

  it "returns all params" $ do
    let p = case parseSQL "SELECT * FROM tbl WHERE a = $p1 AND b = $p2;" of
             Left e -> error $ show e
             Right (_, v) -> v
    p `shouldBe` ["p1", "p2"]

