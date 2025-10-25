import Test.Hspec (hspec, describe, it)
import Test.MockCat.MockSpec as Mock
import Test.MockCat.ConsSpec as Cons
import Test.MockCat.ParamSpecSpec as Param
import Test.MockCat.AssociationListSpec as AssociationList
import Test.MockCat.ExampleSpec as Example
import Test.MockCat.TypeClassSpec as TypeClass
import Test.MockCat.TypeClassTHSpec as TypeClassTH
import Test.MockCat.PartialMockSpec as PartialMock
import Test.MockCat.PartialMockTHSpec as PartialMockTH
import Test.MockCat.ConcurrencySpec as Concurrency
import Test.QuickCheck (property)
import qualified PoC.QuickCheckIntegration as PoC
import qualified PoC.ParamSpecGen as ParamSpecGen
import qualified PoC.ScenarioDSLPoC as ScenarioPoC
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
    Mock.spec
    AssociationList.spec
    Example.spec
    TypeClass.spec
    TypeClassTH.spec
    PartialMock.spec
    PartialMockTH.spec
    Concurrency.spec
    describe "PoC QuickCheck integration" $ do
      it "single call example" $ property PoC.prop_singleCall_example
      it "multi call count example" $ property PoC.prop_multiCallCount_example
    describe "PoC ParamSpec -> Gen" $ do
      it "exact reproduces the same value" $ property ParamSpecGen.prop_exact
      it "rangeInt stays within bounds" $ property ParamSpecGen.prop_rangeIntWithin
      it "enum chooses only listed" $ property ParamSpecGen.prop_enumOnly
    describe "PoC Scenario DSL" $ do
      it "scenario order preserved" $ property ScenarioPoC.prop_scenario_order
      it "scenario call count matches" $ property ScenarioPoC.prop_scenario_count
    describe "Property Concurrency" $ do
      it "total apply count is preserved across threads" $ property ConcurrencyProp.prop_concurrent_total_apply_count
    describe "Property Lazy Evaluation" $ do
      it "unforced stub action is not counted" $ property LazyEvalProp.prop_lazy_unforced_not_counted
      it "forced stub action is counted" $ property LazyEvalProp.prop_lazy_forced_counted
    describe "Property Script Generator" $ do
      it "script count matches recorded applications" $ property ScriptProps.prop_script_count_matches
    describe "Property Order / PartialOrder" $ do
      it "in-order script succeeds" $ property OrderProps.prop_inorder_succeeds
      it "adjacent swap fails order verification" $ property OrderProps.prop_adjacent_swap_fails
      it "subset partial order succeeds" $ property OrderProps.prop_partial_order_subset_succeeds
      it "reversed pair fails partial order" $ property OrderProps.prop_partial_order_reversed_pair_fails
    describe "Property Additional (Predicate / Multi-case / Isolation / Unused / Duplicates)" $ do
      it "predicate param counts match" $ property AdditionalProps.prop_predicate_param_match_counts
      it "multi-case progression saturates" $ property AdditionalProps.prop_multicase_progression
      it "runMockT isolation of counts" $ property AdditionalProps.prop_runMockT_isolation
      it "neverApply (unused) passes" $ property AdditionalProps.prop_neverApply_unused
      it "partial order with duplicates behaves" $ property AdditionalProps.prop_partial_order_duplicates
    describe "Property Reinforcement (Negative predicate / Partial force / Interleave)" $ do
      it "predicate negative not counted" $ property ReinforcementProps.prop_predicate_negative_not_counted
      it "lazy partial force in concurrency counts only forced" $ property ReinforcementProps.prop_lazy_partial_force_concurrency
      it "interleaved duplicate partial order semantics" $ property ReinforcementProps.prop_partial_order_interleaved_duplicates
    ParamSpecNormalizeProp.spec
    ParamSpecMergeProp.spec
    ParamSpecRangeMergeRandomProp.spec