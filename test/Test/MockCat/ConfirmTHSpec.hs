{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE FunctionalDependencies #-}

module Test.MockCat.ConfirmTHSpec (spec) where

import Test.MockCat.SharedSpecDefs
import Test.MockCat.TH
import Test.Hspec
-- import Control.Monad.Reader.Class (MonadReader)

spec :: Spec
spec = pure ()

makeMock [t|Teletype|]
-- makeAutoLiftMock [t|FileOperation|]
-- makeAutoLiftMock [t|ApiOperation|]
-- makeMock [t|TestClass|]
-- makeAutoLiftMock [t|UserDefinedClass|]
-- makeAutoLiftMock [t|AssocTypeTest|]
-- makeAutoLiftMock [t|DefaultMethodTest|]
-- makeMock [t|ExplicitlyReturnMonadicValuesTest|]
-- makeMock [t|MultiApplyTest|]
-- makeMock [t|MonadStateSub|]
-- makeAutoLiftMock [t|MonadReader String|]
-- makeMock [t|MonadVar2_1Sub|]
-- makeMock [t|MonadVar2_2Sub|]
-- makeMock [t|MonadVar3_1Sub|]
-- makeMock [t|MonadVar3_2Sub|]
-- makeMock [t|MonadVar3_3Sub|]
-- makeMock [t|ParamThreeMonad Int Bool|]
-- makeMock [t|Finder Int String|]