module Schema where

import Forum
import qualified Data.Text as T

------------------------------------------------------------------------------
-- Setup

withEmptyDb :: (DB -> IO a) -> IO a
withEmptyDb action = do
  db <- getOrCreateDB "forum-test" (Proxy :: Proxy Schema)
  result <- action db
  deleteDB db
  return result

sql :: QuasiQuoter
sql = sqlQQFor (Proxy :: Proxy Schema)

------------------------------------------------------------------------------
-- Schema and Types

type Species = Book
  '[ "speciesId"  :=> PrimaryKey "species" Int
   , "name"       :=> T.Text
   , "genus"      :=> ForeignKey "genus" Int
   , "discoverer" :=> Maybe (ForeignKey "discoverer" Int)
   ]

type Genus = Book
  '[ "genusId"    :=> PrimaryKey "genus" Int
   , "name"       :=> T.Text
   , "genus"      :=> ForeignKey "family" Int
   , "discoverer" :=> Maybe (ForeignKey "discoverer" Int)
   ]

type Family = Book
  '[ "familyId"   :=> PrimaryKey "family" Int
   , "name"       :=> T.Text
   , "discoverer" :=> Maybe (ForeignKey "discoverer" Int)
   ]

type Discoverer = Book
  '[ "discovererId" :=> PrimaryKey "family" Int
   , "firstName"    :=> T.Text
   , "lastName"     :=> T.Text
   ]

type Schema = Book
  '[ "species"    :=> Species
   {-, "genus"      :=> Genus-}
   {-, "family"     :=> Family-}
   {-, "discoverer" :=> Discoverer-}
   ]
