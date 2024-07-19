import Test.Hspec (hspec)
import Test.MockCatSpec as MockCat
import Test.MockCat.ConsSpec as Cons
import Test.MockCat.ParamSpec as Param

main :: IO ()
main = do
  hspec $ do
    Cons.spec
    Param.spec
    MockCat.spec