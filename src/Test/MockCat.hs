{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.MockCat where
import Test.MockCat.Param
import Test.MockCat.Cons

data Mock fun params = Mock (Maybe MockName) fun
type MockName = String

class MockBuilder params fun verifyParams | params -> fun, params -> verifyParams where
  build :: Maybe MockName -> params -> IO (Mock fun verifyParams)


-- instance (Show a, Eq a)
--   => MockBuilder (Param a #> Param r) (a -> r) (Param a) where
--   build name params = do
--     s <- store
--     createMock name s.calledParamsList (\a2 -> extractReturnValueWithValidate name params (p a2) s)

-- createMock :: Eq params => Show params => Maybe MockName -> CalledParamsList params -> fun -> IO (Mock fun params)
-- createMock name l fn = pure $ Mock name fn

-- type CalledParamsList params = [params]

-- extractReturnValueWithValidate ::
--      ParamDivider params args (Param r)
--   => Eq args
--   => Show args
--   => Maybe MockName
--   -> params
--   -> args
--   -> CalledParamsStore args
--   -> r
-- extractReturnValueWithValidate name params inputParams s =
--   let
--     expectedArgs = args params
--     r = returnValue params
--     _ = validateWithStoreParams name s expectedArgs inputParams
--   in r