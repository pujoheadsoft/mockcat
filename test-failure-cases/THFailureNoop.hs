{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module THFailureNoop where

import Test.MockCat
import Prelude hiding (any)

-- Case 1: deriveNoopInstance with incorrect return type
class Monad m => BadNoop m where
  bad :: m Int

deriveNoopInstance [t|BadNoop|]
