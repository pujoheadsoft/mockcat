import Test.Hspec (hspec)
import Test.MockCat.MockSpec as Mock
import Test.MockCat.ConsSpec as Cons
import Test.MockCat.ParamSpec as Param
import Test.MockCat.AssociationListSpec as AssociationList
import Test.MockCat.ExampleSpec as Example
import Test.MockCat.TypeClassSpec as TypeClass
import Test.MockCat.TypeClassTHSpec as TypeClassTH

main :: IO ()
main = do
  hspec $ do
    Cons.spec
    Param.spec
    Mock.spec
    AssociationList.spec
    Example.spec
    TypeClass.spec
    TypeClassTH.spec