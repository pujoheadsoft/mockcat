{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module ReadmeVerifyAutoLiftSpec (spec) where

import Test.Hspec
import Test.MockCat
import Control.Monad.IO.Class (liftIO)
import Prelude hiding (readFile, writeFile, any)

class Monad m => FileSystem m where
  readFile :: FilePath -> m String
  writeFile :: FilePath -> String -> m ()

-- makeAutoLiftMock generates methods that automatically lift to MockT
makeAutoLiftMock [t|FileSystem|]

-- Dummy program for makeAutoLiftMock example
-- Note: In real usage, this would likely be the same program code, 
-- but deployed in a context where 'm' can be 'MockT IO'.
myProgram :: FileSystem m => FilePath -> m ()
myProgram _ = pure ()

spec :: Spec
spec = do
  describe "README Examples (AutoLift)" $ do
    it "User Guide 2. makeAutoLiftMock" $ do
      -- makeAutoLiftMock generates functions that run directly in MockT usually,
      -- but specifically it lifts the result.
      -- Here we verify the generated code compiles and runs.
      -- makeAutoLiftMock automatically lifts the result, so we provide pure values in expectations.
      _ <- runMockT $ (do
         _readFile $ ("config.txt" :: String) ~> ("debug=true" :: String)
         _writeFile $ ("log.txt" :: String) ~> ("start" :: String) ~> ()
         myProgram "config.txt"
         liftIO $ pure () :: MockT IO ())
      pure ()
