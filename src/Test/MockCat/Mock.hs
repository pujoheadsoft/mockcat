{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use null" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | This module provides the following functions.

  - Create mocks that can be stubbed and verified.

  - Create stub function.

  - Verify applied mock function.
-}
module Test.MockCat.Mock
  ( Mock
  , MockBuilder
  , build
  -- , createMock
  -- , createNamedMock
  -- , createConstantMock
  -- , createNamedConstantMock
  , createStubFn
  , createNamedStubFn
  -- , stubFn
  -- , stubFnMock
  , shouldApplyTo
  , shouldApplyTimes
  , shouldApplyInOrder
  , shouldApplyInPartialOrder
  , shouldApplyTimesGreaterThanEqual
  , shouldApplyTimesLessThanEqual
  , shouldApplyTimesGreaterThan
  , shouldApplyTimesLessThan
  , shouldApplyToAnything
  , shouldApplyTimesToAnything
  , to
  , onCase
  , cases
  , casesIO
  -- , createMockIO
  , stubFnMockIO
  , createStubFnIO
  , createPureStubFn
  , createConstantStubFn
  , createNamedConstantStubFn
  )
where

import Control.Monad (guard, when, ap)
import Data.Function ((&))

import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef')
import Data.List (elemIndex, intercalate)
import Data.Maybe

import GHC.IO (unsafePerformIO, evaluate)
import Test.MockCat.Cons
import Test.MockCat.Param
import Test.MockCat.AssociationList (AssociationList, lookup, update, insert, empty, member)
import Prelude hiding (lookup)
import GHC.Stack (HasCallStack)
import Control.Monad.Trans
import Control.Monad.State
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Test.MockCat.Internal.Types
import Test.MockCat.Internal.Core
import Test.MockCat.Internal.Message
import Test.MockCat.Internal.Builder
import Test.MockCat.Verify
import Data.Typeable (Typeable, eqT)
import Type.Reflection (typeRep, TyCon, typeRepTyCon, splitApps)
import Data.Type.Equality ((:~:) (Refl))
import Data.Dynamic (toDyn)
import Test.MockCat.Internal.Registry
  ( attachVerifierToFn
  , registerUnitMeta
  , UnitMeta
  , withUnitGuard
  , markUnitUsed
  , isGuardActive
  )

{- | Create a mock.
From this mock, you can generate stub functions and verify the functions.

  @
  import Test.Hspec
  import Test.MockCat
  ...
  it "stub & verify" do
    -- create a mock
    m \<- createMock $ "value" |\> True
    -- stub function
    let f = stubFn m
    -- assert
    f "value" \`shouldBe\` True
    -- verify
    m \`shouldApplyTo\` "value"
  @

  If you do not need verification and only need stub functions, you can use @'mockFun'@.

-}
createMock ::
  (MonadIO m, MockBuilder params fn verifyParams) =>
  params ->
  m (Mock fn verifyParams)
createMock params = liftIO $ build Nothing params

{- | Create a constant mock.
From this mock, you can generate constant functions and verify the functions.

  @
  import Test.Hspec
  import Test.MockCat
  ...
  it "stub & verify" do
    m \<- createConstantMock "foo"
    stubFn m \`shouldBe\` "foo"
    shouldApplyToAnything m
  @
-}
createConstantMock :: MonadIO m => a -> m (Mock a ())
createConstantMock a = liftIO $ build Nothing $ param a

{- | Create a named mock. If the test fails, this name is used. This may be useful if you have multiple mocks.

  @
  import Test.Hspec
  import Test.MockCat
  ...
  it "named mock" do
    m \<- createNamedMock "mock" $ "value" |\> True
    stubFn m "value" \`shouldBe\` True
  @
-}
createNamedMock ::
  (MonadIO m, MockBuilder params fn verifyParams) =>
  MockName ->
  params ->
  m (Mock fn verifyParams)
createNamedMock name params = liftIO $ build (Just name) params

-- | Create a named constant mock.
createNamedConstantMock :: MonadIO m => MockName -> fn -> m (Mock fn ())
createNamedConstantMock name a = liftIO $ build (Just name) (param a)

-- | Extract the stub function from the mock.
stubFn :: forall m fn. (IsMock m, MockFn m ~ fn) => m -> fn
stubFn = mockStubFn

stubFnMock :: Mock fn v -> fn
stubFnMock (Mock f _ _) = f
stubFnMock (NamedMock _ f _ _) = f

{- | Create a stub function.
  @
  import Test.Hspec
  import Test.MockCat
  ...
  it "stub function" do
    f \<- createStubFn $ "value" |\> True
    f "value" \`shouldBe\` True
  @
-}
createStubFn ::
  ( MonadIO m
  , MockBuilder params fn verifyParams
  , Typeable verifyParams
  , Typeable fn
  ) =>
  params ->
  m fn
createStubFn params = registerAndReturn =<< createMock params

createConstantStubFn :: (MonadIO m, Typeable b) => b -> m b
createConstantStubFn params = registerAndReturn =<< createConstantMock params

createPureStubFn ::
  (StubBuilder params fn) =>
  params ->
  fn
createPureStubFn params = buildStub Nothing params

-- | Create a named stub function.
createNamedStubFn ::
  ( MonadIO m
  , MockBuilder params fn verifyParams
  , Typeable verifyParams
  , Typeable fn
  ) =>
  String ->
  params ->
  m fn
createNamedStubFn name params = registerAndReturn =<< createNamedMock name params

createNamedConstantStubFn :: (MonadIO m, Typeable b) => String -> b -> m b
createNamedConstantStubFn name params = registerAndReturn =<< createNamedConstantMock name params


to :: (a -> IO ()) -> a -> IO ()
to f = f

{- | Make a case for stub functions.  
This can be used to create stub functions that return different values depending on their arguments.

  @
  it "test" do
    f <-
      createStubFn $ do
        onCase $ "a" |> "return x"
        onCase $ "b" |> "return y"

    f "a" `shouldBe` "return x"
    f "b" `shouldBe` "return y"
  @
-}
onCase :: a -> Cases a ()
onCase a = Cases $ do
  st <- get
  put (st ++ [a])

{- | Make a list of patterns of arguments and returned values.  
This can be used to create stub functions that return different values depending on their arguments.

  @
  it "test" do
    f <-
      createStubFn $ cases [
        "a" |> "return x",
        "b" |> "return y"
      ]

    f "a" `shouldBe` "return x"
    f "b" `shouldBe` "return y"
  @
-}
cases :: [a] -> Cases a ()
cases a = Cases $ put a

{- | IO version of @'cases'@.  
@casesIO ["a", ""]@ has the same meaning as @cases [ pure \@IO "a", pure \@IO ""]@.
-}
casesIO :: [a] -> Cases (IO a) ()
casesIO = Cases . (put . map pure)



-- ------------------




createMockIO ::
  forall params fn fnM verifyParams m.
  ( MonadIO m
  , MockIOBuilder params fn verifyParams
  , LiftFunTo fn fnM m
  ) =>
  params ->
  m (MockIO m fnM verifyParams)
createMockIO params = do
  mockIO <- liftIO (buildIO Nothing params)
  pure $ widenMock mockIO

stubFnMockIO :: MockIO m fn params -> fn
stubFnMockIO (MockIO f _ _) = f
stubFnMockIO (NamedMockIO _ f _ _) = f

createStubFnIO ::
  forall params fn verifyParams m fnM.
  ( MockIOBuilder params fn verifyParams
  , MonadIO m
  , LiftFunTo fn fnM m
  , Typeable fnM
  ) =>
  params ->
  m fnM
createStubFnIO params = registerAndReturn =<< createMockIO params

class LiftFunTo funIO funM (m :: Type -> Type) | funIO m -> funM where
  liftFunTo :: Proxy m -> funIO -> funM

instance MonadIO m => LiftFunTo (IO r) (m r) m where
  liftFunTo _ = liftIO

instance LiftFunTo restIO restM m => LiftFunTo (a -> restIO) (a -> restM) m where
  liftFunTo p f a = liftFunTo p (f a)

widenMock ::
  forall m funIO funM params.
  ( LiftFunTo funIO funM m
  , Typeable (Verifier params)) => 
  MockIO IO funIO params ->
  MockIO m funM params
widenMock (MockIO f verifier _) = MockIO (liftFunTo (Proxy :: Proxy m) f) verifier (toDyn verifier)
widenMock (NamedMockIO name f verifier _) = NamedMockIO name (liftFunTo (Proxy :: Proxy m) f) verifier (toDyn verifier)


registerAndReturn ::
  forall m mock.
  ( MonadIO m
  , IsMock mock
  , Typeable (Verifier (MockParams mock))
  , Typeable (MockParams mock)
  , Typeable (MockFn mock)
  ) =>
  mock ->
  m (MockFn mock)
registerAndReturn mock = do
  baseValue <- liftIO $ evaluate (stubFn mock)
  let name = mockName mock
      verifier@(Verifier ref) = mockVerifier mock
  case eqT :: Maybe (MockParams mock :~: ()) of
    Just Refl -> do
      meta <- liftIO $ registerUnitMeta ref
      liftIO $ writeIORef ref appliedRecord
      let trackedValue = wrapUnitStub ref meta baseValue
      liftIO $
        withUnitGuard meta $ do
          attachVerifierToFn trackedValue (name, verifier)
          attachVerifierToFn baseValue (name, verifier)
      pure trackedValue
    Nothing -> do
      liftIO $ attachVerifierToFn baseValue (name, verifier)
      pure baseValue

ioTyCon :: TyCon
ioTyCon = typeRepTyCon (typeRep @(IO ()))

wrapUnitStub ::
  forall fn.
  Typeable fn =>
  IORef (AppliedRecord ()) ->
  UnitMeta ->
  fn ->
  fn
wrapUnitStub ref meta value =
  perform $ do
    guardActive <- isGuardActive meta
    if guardActive || isIOType (Proxy :: Proxy fn)
      then pure value
      else do
        markUnitUsed meta
        appendAppliedParams ref ()
        pure value

isIOType :: forall a. Typeable a => Proxy a -> Bool
isIOType _ =
  case splitApps (typeRep @a) of
    (tc, _) -> tc == ioTyCon