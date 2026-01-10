{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Test.MockCat.Readme.MakeAutoLiftMockSpec (spec) where

import Test.Hspec
import Test.MockCat

class Monad m => FileSystem m where
  readFile :: FilePath -> m String
  writeFile :: FilePath -> String -> m ()

-- [Auto-Lift Mode] 利便性重視のモード。
-- 純粋な値を自動的にモナド（m String など）に包んで返します。
makeAutoLiftMock [t|FileSystem|]

-- Dummy program for makeMock example
myProgram :: FileSystem m => FilePath -> m ()
myProgram _ = pure ()

spec :: Spec
spec = do
  it "filesystem test" $ do
    result <- runMockT $ do
      -- [Auto-Lift Mode] (makeAutoLiftMock 使用時): 値は自動的に包まれる (便利)
      _readFile $ "config.txt" ~> "debug=true"
      _writeFile $ "log.txt" ~> "start" ~> ()

      -- テスト対象コードの実行（モックが注入される）
      myProgram "config.txt"
    
    result `shouldBe` ()