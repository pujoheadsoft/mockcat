{-# LANGUAGE BlockArguments #-}
module Test.MockCat.Readme.ReadmeSpec (spec) where

import Test.Hspec
import qualified Test.MockCat.Readme.QuickStartDemoSpec as QuickStartDemo
import qualified Test.MockCat.Readme.WithMockSpec as WithMock
import qualified Test.MockCat.Readme.WithMockIOSpec as WithMockIO
import qualified Test.MockCat.Readme.MakeMockSpec as MakeMock
import qualified Test.MockCat.Readme.MakeAutoLiftMockSpec as MakeAutoLiftMock
import qualified Test.MockCat.Readme.ShouldBeCalledSpec as ShouldBeCalled
import qualified Test.MockCat.Readme.MatcherSpec as Matcher
import Control.Monad (when)

spec :: Bool -> Spec
spec strictWorks = do
  describe "Readme Tests" do
    QuickStartDemo.spec
    WithMock.spec
    WithMockIO.spec
    MakeMock.spec
    MakeAutoLiftMock.spec
    Matcher.spec
    when strictWorks $ do
      ShouldBeCalled.spec