{-# LANGUAGE TemplateHaskell #-}
module Forum.Internal.SQL where

import Data.Proxy (Proxy(Proxy))
import Data.Char (isAlphaNum)
import qualified Data.ByteString.Char8 as BS
import qualified Hasql.Query as Hasql
import qualified Hasql.Class as Hasql
import qualified Database.HsSqlPpp.Catalog as Sql
import qualified Database.HsSqlPpp.Syntax as Sql
import qualified Database.HsSqlPpp.Parse as Sql
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Bookkeeper.Internal.Types (Book')

import Forum.Internal.Types
import Forum.Internal.ToTable

parseSQL :: String -> Either Sql.ParseErrorExtra ([Sql.Statement], [String])
parseSQL s = (, params) <$> parsed
  where
    parsed = Sql.parseStatements Sql.defaultParseFlags
                                 "" Nothing (LT.pack . unwords $ stmt)
    (stmt, params, _) = foldr go ([], [], 0) (words s)
    -- 'count' is used rather than 'length' for efficiency
    go ('$':word) (stmt, params, count)
      = (('?' : extra) : stmt , param : params, count + 1)
         where
           (param, extra) = span isAlphaNum word
    go word (stmt, params, count)
      = (word : stmt, params, count)

-- | Runs type-checking on the statement, and returns the inferred type
typeCheckSQL :: Sql.Statement -> Sql.Catalog -> TH.Q TH.Type
typeCheckSQL s cat = case Sql.typeCheckStatements Sql.defaultTypeCheckFlags cat [s] of
  (_, [typechecked]) -> case typechecked of
    QueryStatement annot _ -> case Sql.anType annot of
      Just typ -> do
        qtyp <- newName "queryType"
        [t| forall x. (HasSqlType typ qtyp) => qtyp |]

makeStatement :: String -> [String] -> TH.Q TH.Exp
makeStatement stmt' params = [e| Hasql.stmtList (BS.pack $stmt) True |]
  where
    stmt = TH.liftString stmt'

sqlQQFor :: forall a f. ToCatalogUpdate a => Proxy (Book' f (a :: [*])) -> TH.QuasiQuoter
sqlQQFor _ = case sqlQQForSchema <$> toCatalog (Proxy :: Proxy a) of
  Left e -> error $ "Error constructing catalog: " ++ show e
  Right v -> v

sqlQQForSchema :: Sql.Catalog -> TH.QuasiQuoter
sqlQQForSchema catalog = TH.QuasiQuoter
  { TH.quoteExp = \s -> case parseSQL s of
      Left err -> error $ show err
      Right (_, params) -> makeStatement s params
  , TH.quotePat = undefined
  , TH.quoteDec = undefined
  , TH.quoteType = undefined
  }

getOrCreateDB :: String -> Proxy (a :: *) -> IO DB
getOrCreateDB = undefined

deleteDB :: DB -> IO ()
deleteDB = undefined
