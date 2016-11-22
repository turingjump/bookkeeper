module ForumSpec (spec) where

spec :: Spec
spec = describe "forum" $ do
    let discoverer = emptyBook & #discovererId =: 1
                               & #firstName =: "Carl"
                               & #lastName =: "Linnaeus"

  it "allows inserting and querying" $ do
    runSql [sql| INSERT $discoverer INTO discoverer |]
    result <- runSql [sql| SELECT (firstName) FROM discoverer |]
    result `shouldBe` [subSet discoverer]

  it "allows WHERE clauses" $ do
    runSql [sql| INSERT $discoverer INTO discoverer |]
    result <- runSql [sql| SELECT (firstName) FROM discoverer WHERE lastName = 0 |]
    result `shouldBe` []

  it "types WHERE on primary keys as Maybe" $ do
    runSql [sql| INSERT $discoverer INTO discoverer |]
    result <- runSql [sql| SELECT (firstName) FROM discoverer WHERE discovererId = 0 |]
    result `shouldBe` Nothing

------------------------------------------------------------------------------
-- Setup

withEmptyDb :: (DB -> IO a) -> IO a
withEmptyDb action = do
  db <- getOrCreateDB "forum-test" (Proxy :: Proxy Schema)
  result <- action db
  deleteDB db
  return result


------------------------------------------------------------------------------
-- Schema and Types

type Species =
  '[ "speciesId"  :=> PrimaryKey "species" Int
   , "name"       :=> T.Text
   , "genus"      :=> ForeignKey "genus" Int
   , "discoverer" :=> Maybe (ForeignKey "discoverer" Int)
   ]

type Genus =
  '[ "genusId"    :=> PrimaryKey "genus" Int
   , "name"       :=> T.Text
   , "genus"      :=> ForeignKey "family" Int
   , "discoverer" :=> Maybe (ForeignKey "discoverer" Int)
   ]

type Family =
  '[ "familyId"   :=> PrimaryKey "family" Int
   , "name"       :=> T.Text
   , "discoverer" :=> Maybe (ForeignKey "discoverer" Int)
   ]

type Discoverer =
  '[ "discovererId" :=> PrimaryKey "family" Int
   , "firstName"    :=> T.Text
   , "lastName"     :=> T.Text
   ]

type Schema =
  '[ "species"    :=> Species
   , "genus"      :=> Genus
   , "family"     :=> Family
   , "discoverer" :=> Discoverer
   ]
