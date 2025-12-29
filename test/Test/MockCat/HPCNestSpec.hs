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

  it "Nested runMockT: inner block clears outer block's history" $ do
    runMockT @IO $ do
      outer <- mock (param "a" ~> True)
      _ <- liftIO $ evaluate (outer "a")
      
      -- At this point, outer is verifiable
      liftIO $ outer `shouldBeCalled` "a"
      
      liftIO $ runMockT @IO $ do
        -- Inner block resets history. 
        -- 'outer' (defined in parent runMockT) relies on history lookup (HPC).
        -- So verifies should fail here.
        result <- liftIO $ try @SomeException (outer `shouldBeCalled` "a")
        liftIO $ result `shouldSatisfy` isLeft
        
        -- Inner mock should work checking
        inner <- mock (param "b" ~> True)
        _ <- liftIO $ evaluate (inner "b")
        liftIO $ inner `shouldBeCalled` "b"

      -- After inner block returns, history is NOT restored (resetMockHistory is destructive).
      -- So outer mock remains broken.
      result <- liftIO $ try @SomeException (outer `shouldBeCalled` "a")
      liftIO $ result `shouldSatisfy` isLeft

  it "withMock inside runMockT works correctly" $ do
    runMockT @IO $ do
      f <- mock (param "a" ~> True)
      _ <- liftIO $ evaluate (f "a")
      
      liftIO $ withMock $ do
        g <- mock (param "b" ~> True) `expects` do called once
        _ <- liftIO $ evaluate (g "b")
        pure ()
      
      -- withMock does not reset history, so 'f' should still be verifiable
      liftIO $ f `shouldBeCalled` "a"
