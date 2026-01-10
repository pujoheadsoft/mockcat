{-# LANGUAGE BlockArguments #-}
import Test.Hspec (hspec, describe, it, pendingWith)
import Test.MockCat.TestHelper (checkStrictVerificationWorks)

import Test.MockCat.MockSpec as Mock
import Test.MockCat.DeriveSpec as Derive
import Test.MockCat.ConsSpec as Cons
import Test.MockCat.ParamSpec as Param
import Test.MockCat.ExampleSpec as Example
import Test.MockCat.TypeClassMinimalSpec as TypeClassMinimal
import Test.MockCat.TypeClassSpec as TypeClass
import Test.MockCat.TypeClassTHSpec as TypeClassTH
import Test.MockCat.PartialMockSpec as PartialMock
import Test.MockCat.PartialMockTHSpec as PartialMockTH
import Test.MockCat.ConcurrencySpec as Concurrency
import Test.MockCat.StubSpec as Stub
import Test.MockCat.Internal.MockRegistrySpec as Registry
import Test.MockCat.MockTSpec as MockTSpec
import Test.MockCat.TH.TypeUtilsSpec as THTypeUtils
import Test.MockCat.TH.ContextBuilderSpec as THContextBuilder
import Test.MockCat.TH.ClassAnalysisSpec as THClassAnalysis
import Test.MockCat.TH.FunctionBuilderSpec as THFunctionBuilder
import Test.MockCat.ShouldBeCalledSpec as ShouldBeCalled
import Test.MockCat.ShouldBeCalledMockMSpec as ShouldBeCalledMockM
import Test.MockCat.WithMockSpec as WithMock
import Test.MockCat.WithMockIOSpec as WithMockIO
import Test.MockCat.ShouldBeCalledErrorDiffSpec as ShouldBeCalledErrorDiff
import Test.MockCat.WithMockErrorDiffSpec as WithMockErrorDiff
import Test.MockCat.THCompareSpec as THCompare
import qualified Test.MockCat.MultipleMocksSpec as MultipleMocks
import qualified Test.MockCat.TypeFamilySpec as TypeFamily
import Test.MockCat.UnsafeCheck ()
import Test.QuickCheck ()
import qualified Property.ConcurrentCountProp as ConcurrencyProp
import qualified Property.LazyEvalProp as LazyEvalProp
import qualified Property.ScriptProps as ScriptProps
import qualified Property.OrderProps as OrderProps
import qualified Property.AdditionalProps as AdditionalProps
import qualified Property.ReinforcementProps as ReinforcementProps
import qualified Property.ParamSpecNormalizeProp as ParamSpecNormalizeProp
import qualified Property.ParamSpecMergeProp as ParamSpecMergeProp
import qualified Property.ParamSpecRangeMergeRandomProp as ParamSpecRangeMergeRandomProp
import qualified Test.MockCat.HpcSpec as HpcSpec
import qualified Test.MockCat.Readme.ReadmeSpec as Readme

main :: IO ()
main = do
  strictWorks <- checkStrictVerificationWorks
  -- If strict verification works (Standard), verify everything.
  -- If it fails (HPC enabled), skip standard verification tests that would falsely fail.
  let conditionalSpec = if strictWorks 
        then id 
        else \_ -> describe "Standard Tests (Skipped due to HPC/Coverage detection)" $ 
                        it "All standard verification tests skipped" $ 
                          pendingWith "Strict Verification disabled (HPC active)"

  hspec $ do
    -- ALWAYS RUN: HpcSpec & Safe Tests (Cons, Param, Mock, Stub, Expects-based, etc.)
    HpcSpec.spec
    Cons.spec
    Param.spec
    Derive.spec
    Mock.spec
    Example.spec
    TypeClassMinimal.spec
    TypeClass.spec
    TypeClassTH.spec
    PartialMock.spec
    PartialMockTH.spec
    Concurrency.spec
    Stub.spec
    MockTSpec.spec
    THCompare.spec
    THTypeUtils.spec
    THContextBuilder.spec
    THClassAnalysis.spec
    THFunctionBuilder.spec
    WithMock.spec
    WithMockErrorDiff.spec
    WithMockIO.spec
    MultipleMocks.spec
    TypeFamily.spec

    ConcurrencyProp.spec
    LazyEvalProp.spec
    ScriptProps.spec
    OrderProps.spec
    AdditionalProps.spec
    ReinforcementProps.spec
    
    ParamSpecNormalizeProp.spec
    ParamSpecMergeProp.spec
    ParamSpecRangeMergeRandomProp.spec

    -- CONDITIONAL RUN: Standard Tests using 'shouldBeCalled' (Unsafe under HPC)
    conditionalSpec $ do
      Registry.spec
      ShouldBeCalled.spec
      ShouldBeCalledMockM.spec
      ShouldBeCalledErrorDiff.spec

    Readme.spec strictWorks
