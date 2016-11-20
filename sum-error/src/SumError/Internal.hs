{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module SumError.Internal where

import Bookkeeper
import Bookkeeper.Internal.Types (Ledger')

import Data.Functor.Identity
import Control.Monad.Except

-- | A type for extensible errors.
-- The type is kept opaque to ensure the error types are sorted and nubbed.
newtype SumErrorT ledger m a = SumErrorT ( ExceptT (Ledger' Identity ledger) m a )
  deriving (Functor, Applicative, Monad, MonadIO, Foldable, Traversable
           , MonadError (Ledger' Identity ledger))

runSumErrorT :: SumErrorT ledger m a -> m (Either (Ledger' Identity ledger) a)
runSumErrorT (SumErrorT e) = runExceptT e


resolve :: Monad m => SumErrorT '[] m a -> m a
resolve e = do
  Right val <- runSumErrorT e
  return val

class (Monad m ) => MonadSumError m error value | m error -> value where
   throwSumError :: Key error -> value -> m a

instance (Optionable error value ledger, Monad m)
    => MonadSumError (SumErrorT ledger m) error value where
  throwSumError key errorValue = SumErrorT (throwError $ option key $ errorValue)

class MonadSumError m error value
  => MonadCatchSumError m m' error value | m error -> m', m' error value -> m where
  catchSumError :: Key error -> (value -> m' a) -> m a -> m' a

instance (ledger' ~ Delete key ledger, Monad m, Optionable key value ledger
        , Split key ledger value)
  => MonadCatchSumError (SumErrorT ledger m) (SumErrorT ledger' m) key value where
  catchSumError key handler original = SumErrorT . ExceptT $ do
    mval <- runSumErrorT original
    case mval of
      Right e  -> return $ Right e
      Left err -> case split key err of
        -- this could be prettier
        Right err1 -> runSumErrorT $ handler err1
        Left err2  -> runSumErrorT $ throwError err2


