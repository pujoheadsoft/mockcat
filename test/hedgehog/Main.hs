module Main where

import System.Exit (exitFailure)
import System.Environment (lookupEnv)
import PoC.HedgehogIntegration as PoC
import PoC.NormalParamSpecGen as ParamNorm
import PoC.ParamSpecIntGenBridge as ParamBridge
import Hedgehog (check)

-- | Standalone Hedgehog runner (experimental PoC).
--   環境変数 HEDGEHOG_FAILING=1 を与えると shrink 観察用の
--   失敗 property (prop_order_shrink_example_hh) を実行し、
--   「失敗したら成功扱い」に変換してスイートを green に保つ。
main :: IO ()
main = do
  wantFailing <- lookupEnv "HEDGEHOG_FAILING"
  baseResults <- sequence
    [ check PoC.prop_singleCall_hh
    , check PoC.prop_multiCallCount_hh
    , check PoC.prop_order_preserved_hh
    , check ParamNorm.prop_genFromNormalInt_sound
    , check ParamBridge.prop_paramSpec_bridge_sound
    ]
  failingResult <- case wantFailing of
    Just "1" -> do
      r <- check PoC.prop_order_shrink_example_hh
      pure (not r)  -- 失敗を期待: r==False なら True に反転
    _ -> pure True
  if and (failingResult : baseResults) then pure () else exitFailure
