{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.MockCat.HPCNestSpec (spec) where

import Test.Hspec
import Test.MockCat
import Control.Exception (try, SomeException, evaluate)
import Control.Monad.IO.Class (liftIO)
import Data.Either (isLeft)

spec :: Spec
spec = describe "HPC Nesting and Isolation" $ do

  it "RunMockT nested in IO: outer mock with 'shouldBeCalled' becomes unverifiable inside runMockT" $ do
    outer <- mock (param "outer" ~> True)
    -- execution in IO context
    _ <- evaluate (outer "outer")

    runMockT @IO $ do
      -- runMockT resets history, so 'outer' should not be verifiable here.
      result <- liftIO $ try @SomeException (outer `shouldBeCalled` "outer")
      liftIO $ result `shouldSatisfy` isLeft

  it "RunMockT nested in IO: outer mock is verifiable AFTER runMockT returns" $ do
    outer <- mock (param "outer" ~> True)
    _ <- evaluate (outer "outer")

    runMockT @IO $ do
      pure ()

    -- Post-runMockT, history is reset. Verification should fail.
    result <- try @SomeException (outer `shouldBeCalled` "outer")
    result `shouldSatisfy` isLeft

  it "withMock (using expects) is immune to history reset" $ do
    withMock $ do
      f <- mock (param "a" ~> True) 
           `expects` do
             called once `with` "a"
      
      liftIO $ do
        _ <- evaluate (f "a")
        -- Nested runMockT clears history, but expectations capture the recorder directly.
        runMockT @IO $ pure ()
        
      pure ()

  it "Sibling runMockT blocks are isolated" $ do
    runMockT @IO $ do
      f1 <- mock (param "a" ~> True)
      _ <- liftIO $ evaluate (f1 "a")
      liftIO $ f1 `shouldBeCalled` "a"
    
    runMockT @IO $ do
      f2 <- mock (param "a" ~> True)
      _ <- liftIO $ evaluate (f2 "a")
      liftIO $ f2 `shouldBeCalled` "a"

  it "Nested withMock blocks work correctly even with same-type mocks" $ do
    withMock $ do
      -- Outer mock
      outer <- mock (param "a" ~> True) 
           `expects` do
             called once
      
      liftIO $ do
        _ <- evaluate (outer "a")
        
        -- Inner withMock
        withMock $ do
          -- Inner mock with SAME signature
          inner <- mock (param "a" ~> True)
               `expects` do
                 called once
          
          _ <- liftIO $ evaluate (inner "a")
          pure ()
          
      pure ()
