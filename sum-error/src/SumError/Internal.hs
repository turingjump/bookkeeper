{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module SumError.Internal where

import Bookkeeper
import Bookkeeper.Internal.Types (Ledger')
import Data.Functor.Classes
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.RWS

import Data.Functor.Identity
import Control.Monad.Except

-- | A type for extensible errors.
-- The type is kept opaque to ensure the error types are sorted and nubbed.
newtype SumErrorT ledger m a = SumErrorT ( ExceptT (Ledger' Identity ledger) m a )
  deriving (Functor, Applicative, Monad, MonadIO, Foldable, Traversable
           , MonadError (Ledger' Identity ledger), MonadState s, MonadWriter r
           , MonadReader r, MonadCont, MonadFix, MonadRWS r w s)

type SumError ledger a = SumErrorT ledger Identity a

deriving instance (Ord a, Ord1 m, Ord (Ledger' Identity ledger))
  => Ord (SumErrorT ledger m a)
deriving instance (Ord1 m, Ord (Ledger' Identity ledger))
  => Ord1 (SumErrorT ledger m)
deriving instance (Eq a, Eq1 m, Eq (Ledger' Identity ledger))
  => Eq (SumErrorT ledger m a)
deriving instance (Eq1 m, Eq (Ledger' Identity ledger))
  => Eq1 (SumErrorT ledger m)

runSumErrorT :: SumErrorT ledger m a -> m (Either (Ledger' Identity ledger) a)
runSumErrorT (SumErrorT e) = runExceptT e

runSumError :: SumError ledger a -> Either (Ledger' Identity ledger) a
runSumError (SumErrorT e) = runIdentity $ runExceptT e

-- | If all errors have been caught, this can be safely converted to a value.
resolveT :: Monad m => SumErrorT '[] m a -> m a
resolveT e = do
  Right val <- runSumErrorT e
  return val

-- | Like 'resolveT', but for 'SumEror'
resolve :: SumError '[] a -> a
resolve = runIdentity . resolveT

-- | @MonadSumError m error value@ indicates that monad @m@ allows throwing
-- a labelled error value of type @value@ and label @error@.
class (Monad m ) => MonadSumError m error value | m error -> value where
   throwSumError :: Key error -> value -> m a

instance (Optionable error value ledger, Monad m)
    => MonadSumError (SumErrorT ledger m) error value where
  throwSumError key errorValue = SumErrorT (throwError $ option key $ errorValue)

-- | @MonadCatchError m m' error value@ indicates that monad @m@ allows
-- catching errores of type @value labelled by @error@. The resulting monad may
-- differ from the original monad by e.g. having the corresponding exception
-- removed.
class (Monad m', MonadSumError m error value)
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
