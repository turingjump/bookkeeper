
module Main (main) where

-- Runs doctest on all files in "src" dir. Assumes:
--   (a) You are using hpack
--   (b) The library has a 'default-extensions' section

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)
import Data.Yaml

newtype Exts = Exts { getExts :: [String] }
  deriving (Eq, Show, Read)

instance FromJSON Exts where
  parseJSON (Object v) = do
    lib <- v .: "library"
    Exts <$> lib .: "default-extensions"
  parseJSON _ = fail "expecting object"

main :: IO ()
main = do
  hpack' <- decodeFile "package.yaml"
  extensions <- case hpack' of
    Nothing -> return $ Exts mempty
    Just v  -> return v
  files <- glob "src/**/*.hs"
  doctest $ files ++ fmap ("-X" ++) (getExts extensions)
