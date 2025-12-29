{-# LANGUAGE MagicHash #-}

-- | This module isolates StableName operations and disables HPC to workaround
--   instability issues with GHC 9.8.4 and coverage enabled.
{-# OPTIONS_GHC -fno-hpc #-}

module Test.MockCat.Internal.GHC.StableName
  ( makeStableName
  , hashStableName
  , StableName
  , eqStableName
  ) where

import System.Mem.StableName (StableName)
import qualified System.Mem.StableName as S

{-# NOINLINE makeStableName #-}
makeStableName :: a -> IO (StableName a)
makeStableName = S.makeStableName

{-# NOINLINE hashStableName #-}
hashStableName :: StableName a -> Int
hashStableName = S.hashStableName

{-# NOINLINE eqStableName #-}
eqStableName :: StableName a -> StableName b -> Bool
eqStableName = S.eqStableName
