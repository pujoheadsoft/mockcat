{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Test.MockCat.TypeFamilySpec (spec) where

import Test.Hspec
import Test.MockCat
import Data.Kind (Type)

class Monad m => MonadKeyValue m where
  type Key m :: Type
  getValue :: Key m -> m String

deriveMockInstances [t|MonadKeyValue|]

spec :: Spec
spec = do
  describe "deriveMockInstances with Type Families" $ do
    it "can lift MonadKeyValue to MockT" $ do
      withMock $ do
        runMockT $ do
          -- This just verifies it compiles and the instance is valid
          pure ()

instance MonadKeyValue IO where
  type Key IO = String
  getValue k = pure $ "Value for " ++ k
