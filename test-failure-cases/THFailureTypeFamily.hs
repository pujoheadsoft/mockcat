{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module THFailureTypeFamily where

import Test.MockCat
import Prelude hiding (any)

-- Case 2: deriveMockInstances with unsupported declaration (Type Family)
class Monad m => UnsupportedClass m where
  type family UnsupportedType m :: *
  unsupportedMethod :: m ()

deriveMockInstances [t|UnsupportedClass|]
