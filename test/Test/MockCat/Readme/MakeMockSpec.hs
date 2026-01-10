{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Test.MockCat.Readme.MakeMockSpec (spec) where

import Test.Hspec
import Test.MockCat

class Monad m => FileSystem m where
  readFile :: FilePath -> m String
  writeFile :: FilePath -> String -> m ()

-- [Strict Mode] デフォルトの動作。「mock」関数と挙動が一致します。
-- 戻り値の型が `m a` の場合、スタブ定義の右辺には `m a` 型の値（例: `pure @IO "value"`, `throwIO Error`）を記述する必要があります。
-- Haskell の型システムに対して正直で、明示的な記述を好む場合に推奨されます。
makeMock [t|FileSystem|]

-- Dummy program for makeMock example
myProgram :: FileSystem m => FilePath -> m ()
myProgram _ = pure ()

spec :: Spec
spec = do
  it "filesystem test" $ do
    result <- runMockT $ do
      -- [Strict Mode] (makeMock 使用時): 明示的に pure で包む
      _readFile $ "config.txt" ~> pure @IO "debug=true"
      _writeFile $ "log.txt" ~> "start" ~> pure @IO ()

      -- テスト対象コードの実行（モックが注入される）
      myProgram "config.txt"
    
    result `shouldBe` ()