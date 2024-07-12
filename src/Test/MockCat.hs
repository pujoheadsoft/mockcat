{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Test.MockCat where
import Test.MockCat.Param
import Test.MockCat.Cons
import Data.Maybe
import Data.List (intercalate)
import Test.MockCat.ParamDivider

data Mock fun params = Mock (Maybe MockName) fun (Verifier params)
type MockName = String
newtype Verifier params = Verifier (CalledParamsList params)

class MockBuilder params fun verifyParams | params -> fun, params -> verifyParams where
  build :: Maybe MockName -> params -> IO (Mock fun verifyParams)


instance (Show a, Eq a)
  => MockBuilder (Param a :> Param r) (a -> r) (Param a) where
  build name params = do
    s <- _store
    createMock name (calledParamsList s) (\a2 -> extractReturnValueWithValidate name params (p a2) s)

p :: a -> Param a
p = param

_store :: IO (CalledParamsStore params)
_store = undefined

createMock :: Eq params => Show params => Maybe MockName -> CalledParamsList params -> fun -> IO (Mock fun params)
createMock name l fn = pure $ Mock name fn (Verifier l)

type CalledParamsList params = [params]

extractReturnValueWithValidate ::
     ParamDivider params args (Param r)
  => Eq args
  => Show args
  => Maybe MockName
  -> params
  -> args
  -> CalledParamsStore args
  -> r
extractReturnValueWithValidate name params inputParams s =
  let
    expectedArgs = args params
    r = returnValue params
    _ = validateWithStoreParams name s expectedArgs inputParams
  in r

validateWithStoreParams :: Eq a => Show a => Maybe MockName -> CalledParamsStore a -> a -> a -> ()
validateWithStoreParams name s expected actual = validateParams name expected (storeCalledParams s actual)

data CalledParamsStore params = CalledParamsStore {
  calledParamsList :: CalledParamsList params,
  store :: params -> IO ()
}

storeCalledParams :: CalledParamsStore a -> a -> a
storeCalledParams (CalledParamsStore { .. }) a = const a (store a)

validateParams :: Eq a => Show a => Maybe MockName -> a -> a -> ()
validateParams name expected actual =
  if expected == actual then ()
  else error $ message name expected actual


{-
  Function was not called with expected arguments.
  expected: 1, "2", 3
  but was : 1, "1", 1
-}
message :: Show a => Maybe MockName -> a -> a -> String
message name expected actual =
  intercalate "\n" [
    "function" <> mockNameLabel name <> "was not called with expected arguments.",
    "  expected: " <> show expected,
    "  but was : " <> show actual
  ]

mockNameLabel :: Maybe MockName -> String
mockNameLabel = fromMaybe " " . enclose " " . enclose "`"

enclose :: String -> Maybe String -> Maybe String
enclose e = fmap (\v -> e <> v <> e)