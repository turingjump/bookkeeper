module Bookkeeper.Internal.Operations where

import Bookkeeper.Internal.Types
import Data.Functor.Identity

-- | Maps a natural transformation over every record.
bmap :: (forall x. f x -> g x) -> Book' f entries -> Book' g entries
bmap _ BNil = BNil
bmap nat (BCons key value rest) = BCons key (nat value) (bmap nat rest)

bsequence :: Monad m => Book' m entries -> m (Book' Identity entries)
bsequence BNil = return BNil
bsequence (BCons key mvalue mrest) = do
  value <- mvalue
  rest <- bsequence mrest
  return $ BCons key (return value) rest
