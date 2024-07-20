import Test.Hspec (hspec)
import Test.MockCat.MockSpec as Mock
import Test.MockCat.ConsSpec as Cons
import Test.MockCat.ParamSpec as Param

main :: IO ()
main = do
  hspec $ do
    Cons.spec
    Param.spec
    Mock.spec