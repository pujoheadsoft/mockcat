{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
module Property.ConcurrentCountProp where

import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO, run, assert)
import Control.Concurrent.Async (async, wait)
import Control.Monad (replicateM_, replicateM)
import Control.Monad.IO.Unlift (withRunInIO, MonadUnliftIO)
import Test.MockCat hiding (any)
import qualified Test.MockCat as MC (any)

-- Class for local concurrent actions (defined independently instead of reusing existing ConcurrencySpec)
class Monad m => PropConcurrencyAction m where
  propAction :: Int -> m Int

makeAutoLiftMock [t|PropConcurrencyAction|]

-- | Property verifying that the total number of calls is recorded without loss even when the same mock function is called concurrently from multiple threads.
--   Arguments: threads (1..20), callsPerThread (1..30)
prop_concurrent_total_apply_count :: Property
prop_concurrent_total_apply_count =
  forAll (chooseInt (1,20)) $ \threads ->
    forAll (chooseInt (1,30)) $ \callsPerThread ->
      monadicIO $ do
        let totalCalls = threads * callsPerThread
        -- Pre-declare expected count with `expects` inside `runMockT`, and have it automatically verified after concurrent calls.
        run $ runMockT $ do
          _propAction ((MC.any ~> (1 :: Int)))
            `expects` do
              called (times totalCalls)
          parallelInvoke threads callsPerThread
          pure ()
        assert True

-- Call propAction concurrently
parallelInvoke :: (PropConcurrencyAction m, MonadUnliftIO m) => Int -> Int -> m ()
parallelInvoke threads callsPerThread = withRunInIO $ \runInIO -> do
  as <- replicateM threads (async $ runInIO $ replicateM_ callsPerThread (propAction 42 >> pure ()))
  mapM_ wait as
