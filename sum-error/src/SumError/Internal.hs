module SumError.Internal where

-- | A type for extensible errors.
-- The type is kept opaque to ensure the error types are sorted and nubbed.
newtype SumErrorT ledger m a = SumErrorT ( ExceptT (Ledger' Identity ledger) m a )
  deriving (Functor, Applicative, Monad, MonadIO, Foldable, Traversable)

runSumErrorT :: SumErrorT ledger m a -> m (Either (Ledger' Identity ledger) a)
runSumErrorT (SumErrorT e) = runExceptT e

throwSumError :: (Monad m, Optionable errorName errorValue ledger)
    => Key errorName -> errorValue -> SumErrorT ledger m a
throwSumError key errorValue = SumErrorT (throwError $ option key $ Identity errorValue)

catchSumError :: (Monad m)
    => Key errorName
    -> (errorValue -> SumErrorT ledgerWithoutError m a)
    -> SumErrorT ledgerWithError
    -> SumErrorT ledgerWithoutError m a
catchSumError key handler original = case original
