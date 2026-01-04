import Test.Hspec (hspec, describe, it)
import Test.MockCat.MockSpec as Mock
import Test.MockCat.DeriveSpec as Derive
import Test.MockCat.ConsSpec as Cons
import Test.MockCat.ParamSpec as Param
import Test.MockCat.AssociationListSpec as AssociationList
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
import ReadmeVerifySpec as ReadmeVerify
import qualified Test.MockCat.HPCFallbackSpec as HPCFallback
import qualified Test.MockCat.MultipleMocksSpec as MultipleMocks
import Test.MockCat.UnsafeCheck ()
import Test.QuickCheck (property)
import qualified Property.ConcurrentCountProp as ConcurrencyProp
import qualified Property.LazyEvalProp as LazyEvalProp
import qualified Property.ScriptProps as ScriptProps
import qualified Property.OrderProps as OrderProps
import qualified Property.AdditionalProps as AdditionalProps
import qualified Property.ReinforcementProps as ReinforcementProps
import qualified Property.ParamSpecNormalizeProp as ParamSpecNormalizeProp
import qualified Property.ParamSpecMergeProp as ParamSpecMergeProp
import qualified Property.ParamSpecRangeMergeRandomProp as ParamSpecRangeMergeRandomProp

main :: IO ()
main = hspec $ do
    Cons.spec
    Param.spec
    Derive.spec
    Mock.spec
    AssociationList.spec
    Example.spec
    TypeClassMinimal.spec
    TypeClass.spec
    TypeClassTH.spec
    PartialMock.spec
    PartialMockTH.spec
    Concurrency.spec
    Stub.spec
    MockTSpec.spec
    Registry.spec
    THCompare.spec
    THTypeUtils.spec
    THContextBuilder.spec
    THClassAnalysis.spec
    THFunctionBuilder.spec
    ShouldBeCalled.spec
    ShouldBeCalledMockM.spec
    WithMock.spec
    WithMockIO.spec
    ShouldBeCalledErrorDiff.spec
    WithMockErrorDiff.spec
    ReadmeVerify.spec
    HPCFallback.spec
    MultipleMocks.spec
    describe "Property Concurrency" $ do
      it "total apply count is preserved across threads" $ property ConcurrencyProp.prop_concurrent_total_apply_count
    describe "Property Lazy Evaluation" $ do
      it "unforced stub action is not counted" $ property LazyEvalProp.prop_lazy_unforced_not_counted
      it "forced stub action is counted" $ property LazyEvalProp.prop_lazy_forced_counted
    describe "Property Script Generator" $ do
      it "script count matches recorded calls" $ property ScriptProps.prop_script_count_matches
    describe "Property Order / PartialOrder" $ do
      it "in-order script succeeds" $ property OrderProps.prop_inorder_succeeds
      it "adjacent swap fails order verification" $ property OrderProps.prop_adjacent_swap_fails
      it "subset partial order succeeds" $ property OrderProps.prop_partial_order_subset_succeeds
      it "reversed pair fails partial order" $ property OrderProps.prop_partial_order_reversed_pair_fails
    describe "Property Additional (Predicate / Multi-case / Isolation / Duplicates)" $ do
      it "predicate param counts match" $ property AdditionalProps.prop_predicate_param_match_counts
      it "multi-case progression saturates" $ property AdditionalProps.prop_multicase_progression
      it "runMockT isolation of counts" $ property AdditionalProps.prop_runMockT_isolation
      it "partial order with duplicates behaves" $ property AdditionalProps.prop_partial_order_duplicates
    describe "Property Reinforcement (Negative predicate / Partial force / Interleave)" $ do
      it "predicate negative not counted" $ property ReinforcementProps.prop_predicate_negative_not_counted
      it "lazy partial force in concurrency counts only forced" $ property ReinforcementProps.prop_lazy_partial_force_concurrency
      it "interleaved duplicate partial order semantics" $ property ReinforcementProps.prop_partial_order_interleaved_duplicates
    ParamSpecNormalizeProp.spec
    ParamSpecMergeProp.spec
    ParamSpecRangeMergeRandomProp.spec