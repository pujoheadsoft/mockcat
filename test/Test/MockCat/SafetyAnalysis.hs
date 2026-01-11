{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{- HLINT ignore "Redundant pure" -}
{- HLINT ignore "Use camelCase" -}

module Test.MockCat.SafetyAnalysis where

import Prelude hiding (any)
import Test.MockCat
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)
import GHC.IO (evaluate)

{- 
  This module defines execution paths to analyze the usage of unsafePerformIO in mockcat.
  It is used to generate GHC Core dumps which are then analyzed by verify_mock_unsafe.sh.
  
  Unique return values (strings) are used for each pattern to prevent GHC 
  from sharing common sub-expressions in Core, ensuring each path is analyzed distinctly.
-}

-- =============================================================================
-- Phase 1: Plain IO (5 patterns)
-- =============================================================================

path_plainIO_stub :: String -> String
path_plainIO_stub = stub (any @String ~> ("plainIO_stub" :: String))

path_plainIO_mock :: IO (String -> String)
path_plainIO_mock = mock (any @String ~> ("plainIO_mock" :: String))

path_plainIO_mock_shouldBeCalled :: IO ()
path_plainIO_mock_shouldBeCalled = do
  f <- mock (any @String ~> ("plainIO_mock_sbc" :: String))
  void $ evaluate (f "test")
  f `shouldBeCalled` "test"

path_plainIO_mockM :: IO (String -> IO String)
path_plainIO_mockM = mockM (any @String ~> ("plainIO_mockM" :: String))

path_plainIO_mockM_shouldBeCalled :: IO ()
path_plainIO_mockM_shouldBeCalled = do
  f :: (String -> IO String) <- mockM (any @String ~> ("plainIO_mockM_sbc" :: String))
  void $ f "test"
  f `shouldBeCalled` "test"


-- =============================================================================
-- Phase 2: withMock (7 patterns)
-- =============================================================================

path_withMock_stub :: IO ()
path_withMock_stub = withMock do
  let f = stub (any @String ~> ("withMock_stub" :: String))
  liftIO $ void $ evaluate (f "test")

path_withMock_mock :: IO ()
path_withMock_mock = withMock do
  f <- mock (any @String ~> ("withMock_mock" :: String))
  liftIO $ void $ evaluate (f "test")

path_withMock_mock_expects :: IO ()
path_withMock_mock_expects = withMock do
  f <- mock (any @String ~> ("withMock_mock_exp" :: String)) `expects` (called once :: Expectations (Param String) ())
  liftIO $ void $ evaluate (f "test")

path_withMock_mock_shouldBeCalled :: IO ()
path_withMock_mock_shouldBeCalled = withMock do
  f <- mock (any @String ~> ("withMock_mock_sbc" :: String))
  liftIO $ void $ evaluate (f "test")
  liftIO $ f `shouldBeCalled` "test"

path_withMock_mockM :: IO ()
path_withMock_mockM = withMock do
  f <- mockM (any @String ~> "withMock_mockM")
  void $ f "test"
  pure ()

path_withMock_mockM_expects :: IO ()
path_withMock_mockM_expects = withMock do
  f <- mockM (any @String ~> "withMock_mockM_exp") `expects` called once
  void $ f "test"
  pure ()

path_withMock_mockM_shouldBeCalled :: IO ()
path_withMock_mockM_shouldBeCalled = withMock do
  f <- mockM (any @String ~> "withMock_mockM_sbc")
  void $ f "test"
  liftIO $ f `shouldBeCalled` "test"


-- =============================================================================
-- Phase 3: withMockIO (7 patterns)
-- =============================================================================

path_withMockIO_stub :: IO ()
path_withMockIO_stub = withMockIO do
  let f = stub (any @String ~> "withMockIO_stub")
  void $ evaluate (f "test")

path_withMockIO_mock :: IO ()
path_withMockIO_mock = withMockIO do
  f <- mock (any @String ~> "withMockIO_mock")
  void $ evaluate (f "test")

path_withMockIO_mock_expects :: IO ()
path_withMockIO_mock_expects = withMockIO do
  f <- mock (any @String ~> "withMockIO_mock_exp") `expects` (called once :: Expectations (Param String) ())
  void $ evaluate (f "test")

path_withMockIO_mock_shouldBeCalled :: IO ()
path_withMockIO_mock_shouldBeCalled = withMockIO do
  f <- mock (any @String ~> "withMockIO_mock_sbc")
  void $ evaluate (f "test")
  f `shouldBeCalled` "test"

path_withMockIO_mockM :: IO ()
path_withMockIO_mockM = withMockIO do
  f <- mockM (any @String ~> "withMockIO_mockM")
  void $ f "test"
  pure ()

path_withMockIO_mockM_expects :: IO ()
path_withMockIO_mockM_expects = withMockIO do
  f <- mockM (any @String ~> "withMockIO_mockM_exp") `expects` called once
  void $ f "test"
  pure ()

path_withMockIO_mockM_shouldBeCalled :: IO ()
path_withMockIO_mockM_shouldBeCalled = withMockIO do
  f <- mockM (any @String ~> "withMockIO_mockM_sbc")
  void $ f "test"
  f `shouldBeCalled` "test"


-- =============================================================================
-- Phase 4: runMockT (7 patterns)
-- =============================================================================

path_runMockT_stub :: IO ()
path_runMockT_stub = runMockT do
  let f = stub (any @String ~> "runMockT_stub")
  liftIO $ void $ evaluate (f "test")

path_runMockT_mock :: IO ()
path_runMockT_mock = runMockT do
  f <- mock (any @String ~> "runMockT_mock")
  liftIO $ void $ evaluate (f "test")

path_runMockT_mock_expects :: IO ()
path_runMockT_mock_expects = runMockT do
  f <- mock (any @String ~> "runMockT_mock_exp") `expects` (called once :: Expectations (Param String) ())
  liftIO $ void $ evaluate (f "test")

path_runMockT_mock_shouldBeCalled :: IO ()
path_runMockT_mock_shouldBeCalled = runMockT do
  f <- mock (any @String ~> "runMockT_mock_sbc")
  liftIO $ void $ evaluate (f "test")
  liftIO $ f `shouldBeCalled` "test"

path_runMockT_mockM :: IO ()
path_runMockT_mockM = runMockT do
  f <- mockM (any @String ~> "runMockT_mockM")
  void $ f "test"
  pure ()

path_runMockT_mockM_expects :: IO ()
path_runMockT_mockM_expects = runMockT do
  f <- mockM (any @String ~> "runMockT_mockM_exp") `expects` called once
  void $ f "test"
  pure ()

path_runMockT_mockM_shouldBeCalled :: IO ()
path_runMockT_mockM_shouldBeCalled = runMockT do
  f <- mockM (any @String ~> "runMockT_mockM_sbc")
  void $ f "test"
  liftIO $ f `shouldBeCalled` "test"
