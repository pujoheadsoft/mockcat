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
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Test.ExampleSpec (spec) where

import Test.Hspec
import Test.MockCat hiding (expect)
--import Test.HMock
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Identity

-- data User = User {
--   userId :: String,
--   name :: String
-- } deriving (Show, Eq)

-- class Monad m => UserManager m where
--   find :: String -> m User
--   save :: User -> m ()

-- data Logic = Logic {
--   update :: User -> User
-- }

-- execute :: UserManager m => Logic -> String -> m ()
-- execute logic userId = do
--   user <- find userId
--   let updatedUser = update logic user
--   save updatedUser

-- makeMockable [t|UserManager|]

spec :: Spec
spec = do
  describe "Class Mock" do
    it "2 arity" do
      -- let
      --   user = User { userId = "uid", name = "oldName" }
      --   updatedUser = User { userId = "uid", name = "newName" }
      -- updateFun <- mockFun $ user |> updatedUser
      
      -- runMockT $ do
      --   expect $ Find "uid" |-> user
      --   expect $ Save updatedUser
      --   execute (Logic { update = updateFun }) "uid"

      "" `shouldBe` ""
    it "" do
      (_, x) <- runMockT (prettyPrint (MyObj "something1")) (Settings "x")
      x `shouldBe` "something1x\n"

data MyObj = MyObj String

prettyPrint :: (Monad m, MonadReadDB m, MonadStdout m) => MyObj -> m ()
prettyPrint (MyObj m) = do
  (Settings s) <- readSettings
  putStrLnM (m <> s)

class MonadReadDB m where
  readSettings :: m Settings

class MonadStdout m where
  putStrLnM :: String -> m ()

data Settings = Settings String -- whatever you like

newtype MockT m a = MockT (WriterT String (ReaderT Settings m) a)
    deriving (Functor, Applicative, Monad)

runMockT :: MockT m a -> Settings -> m (a, String)
runMockT (MockT w) = runReaderT $ runWriterT w

runMock :: MockT Identity a -> Settings -> (a, String)
runMock = fmap runIdentity . runMockT

instance MonadTrans MockT where
  lift = MockT . lift . lift

instance Monad m => MonadReadDB (MockT m) where
  readSettings = MockT $ lift $ ask

instance Monad m => MonadStdout (MockT m) where
  putStrLnM s = MockT $ tell $ s <> "\n"