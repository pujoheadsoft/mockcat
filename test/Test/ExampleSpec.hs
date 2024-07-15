{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
module Test.ExampleSpec where

import Test.Hspec
import Test.MockCat
import Test.HMock

data User = User {
  userId :: String,
  name :: String
} deriving (Show, Eq)

class Monad m => UserManager m where
  find :: String -> m User
  save :: User -> m ()

data Logic = Logic {
  update :: User -> User
}

execute :: UserManager m => Logic -> String -> m ()
execute logic userId = do
  user <- find userId
  let updatedUser = update logic user
  save updatedUser

makeMockable [t|UserManager|]

spec :: Spec
spec = do
  describe "Class Mock" do
    it "2 arity" do
      let
        user = User { userId = "uid", name = "oldName" }
        updatedUser = User { userId = "uid", name = "newName" }
      updateFun <- mockFun $ user |> updatedUser
      runMockT $ do
        expect $ Find "uid" |-> user
        expect $ Save updatedUser
        execute (Logic { update = updateFun }) "uid"
      "" `shouldBe` ""

