{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{- HLINT ignore "Use newtype instead of data" -}
module Test.MockCat.DeriveSpec (spec) where

import Test.Hspec
import Test.MockCat
import Control.Monad (void)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..), runReaderT)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.IO.Class (MonadIO(..))

-- Target class for deriveMockInstances
class Monad m => Logger m where
  logInfo :: String -> m ()

instance Logger IO where
  logInfo _ = pure ()

deriveMockInstances [t|Logger|]

-- Target class for deriveNoopInstance
class Monad m => Auditor m where
  audit :: String -> m ()

deriveNoopInstance [t|Auditor|]

-- Custom error for testing
data CustomError = CustomError String deriving (Eq, Show)

-- [Direct Proof] Verify MonadError can be mocked.
-- For monad type classes (methods return 'm a'), makeAutoLiftMock is recommended.
-- This verifies the fix for "Not in scope: type variable ‘a’" in polymorphic methods.
makeAutoLiftMock [t|MonadError CustomError|]

-- Isolated monad to avoid MonadError IOException IO conflict
newtype TestM a = TestM { runTestM :: IO a } deriving (Functor, Applicative, Monad, MonadIO)

spec :: Spec
spec = do
  describe "MTL instances" $ do
    it "can use catchError directly in MockT" $ do
      let action = runMockT $ do
            let action' = throwError "error" :: MockT (ExceptT String IO) Int
            action' `catchError` (\e -> if e == "error" then pure 42 else throwError e)
      runExceptT action `shouldReturn` Right (42 :: Int)

    it "can use ask directly in MockT" $ do
      let action = runMockT (ask :: MockT (ReaderT String IO) String)
      runReaderT action "env" `shouldReturn` "env"

  describe "deriveMockInstances" $ do
    it "can derive custom typeclass instances (lifts to base monad)" $ do
      runMockT (logInfo "hello" :: MockT IO ()) `shouldReturn` ()

  describe "deriveNoopInstance" $ do
    it "doesn't require stubs for noop methods" $ do
      runMockT (audit "something" :: MockT IO ()) `shouldReturn` ()

  describe "makeMock/makeAutoLiftMock with MonadError" $ do
    it "can mock standard MonadError directly using isolated TestM" $ do
      let action :: MockT TestM ()
          action = do
            -- 1. Setup stub for throwError
            -- Note: throwError (CustomError "fail") returns MockT TestM (), so the stub return type must match.
            _throwError (CustomError "fail" ~> (pure () :: MockT TestM ()))

            -- 2. Call throwError
            void (throwError (CustomError "fail") :: MockT TestM ())

      runTestM (runMockT action) `shouldReturn` ()
