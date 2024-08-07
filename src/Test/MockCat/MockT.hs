{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
module Test.MockCat.MockT (MockT(..), Definition(..), runMockT) where
import Control.Monad.State
import GHC.TypeLits (KnownSymbol)
import Data.Data (Proxy)
import Test.MockCat.Mock (Mock)
import Data.Foldable (for_)

newtype MockT m a = MockT { st :: StateT [Definition] m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

data Definition = forall f p sym. KnownSymbol sym => Definition {
  symbol :: Proxy sym,
  mock :: Mock f p,
  verify :: Mock f p -> IO ()
}

runMockT :: MonadIO m => MockT m a -> m a
runMockT (MockT s) = do
  r <- runStateT s []
  let
    !a = fst r
    defs = snd r
  for_ defs (\(Definition _ m v) -> liftIO $ v m)
  pure a
