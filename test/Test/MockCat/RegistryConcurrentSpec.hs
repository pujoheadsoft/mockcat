{-# LANGUAGE ScopedTypeVariables #-}
module Test.MockCat.RegistryConcurrentSpec (spec) where

import Test.Hspec
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.Dynamic
import Unsafe.Coerce (unsafeCoerce)
import System.Mem.StableName (makeStableName, hashStableName)
import System.Random (randomRIO)

import qualified Test.MockCat.Internal.Registry.Core as Registry

spec :: Spec
spec = describe "Registry concurrent correctness" $ do
  it "stabilizes to a single canonical dynamic under contention" $ do
    let name = "_concurrent_test"
        threads = 20
        iterations = 200
    let creatorAction :: Int -> IO Dynamic
        creatorAction cid = do
          let f :: Int -> Int
              f x = cid + x
          pure (toDyn (f :: Int -> Int))

    let worker i = replicateM_ iterations $ do
          r <- randomRIO (1 :: Int, 3)
          case r of
            1 -> void $ Registry.registerNameWithCreator name creatorAction
            2 -> do
              let g :: Int -> Int
                  g x = i + x
              Registry.registerNameAndFn (g :: Int -> Int) name
            _ -> do
              let h :: Int -> Int
                  h x = i * 2 + x
              Registry.registerNameForce name (toDyn (h :: Int -> Int))
    as <- mapM (async . worker) [1 .. threads]
    mapM_ wait as

    samples <- replicateM 50 $ do
      md <- Registry.lookupFnByName name
      case md of
        Nothing -> pure Nothing
        Just d -> do
          let fn = unsafeCoerce d :: Int -> Int
          sn <- makeStableName fn
          pure (Just (hashStableName sn))
    let present = [h | Just h <- samples]
    present `shouldSatisfy` (not . null)
    let uniques = foldr (\x acc -> if x `elem` acc then acc else x : acc) [] present
    length uniques `shouldBe` 1


