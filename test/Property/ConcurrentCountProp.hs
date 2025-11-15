{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
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

-- ローカル並行アクション用クラス (既存 ConcurrencySpec を再利用せず独立定義)
class Monad m => PropConcurrencyAction m where
  propAction :: Int -> m Int

makeMock [t|PropConcurrencyAction|]

-- | 並行に複数スレッドから同じモック関数を呼び出しても、合計呼び出し回数が失われずに記録されることを検証する property。
--   引数: threads (1..20), callsPerThread (1..30)
prop_concurrent_total_apply_count :: Property
prop_concurrent_total_apply_count =
  forAll (chooseInt (1,20)) $ \threads ->
    forAll (chooseInt (1,30)) $ \callsPerThread ->
      monadicIO $ do
        let totalCalls = threads * callsPerThread
        -- runMockT 内で applyTimesIs により期待回数を事前宣言し、並行呼び出し後に自動検証させる
        run $ runMockT $ do
          _propAction (MC.any |> (1 :: Int)) `applyTimesIs` totalCalls
          parallelInvoke threads callsPerThread
          pure ()
        assert True

-- 並行に propAction を呼び出す
parallelInvoke :: (PropConcurrencyAction m, MonadUnliftIO m) => Int -> Int -> m ()
parallelInvoke threads callsPerThread = withRunInIO $ \runInIO -> do
  as <- replicateM threads (async $ runInIO $ replicateM_ callsPerThread (propAction 42 >> pure ()))
  mapM_ wait as
