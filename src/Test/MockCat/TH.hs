{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Test.MockCat.TH where

import Language.Haskell.TH
import Test.MockCat.Param

newtype X a = X a deriving (Show)

-- 型がラップされているかどうかを判定する型ファミリ
type family IsWrapped a where
  IsWrapped (X a) = 'True
  IsWrapped a     = 'False

-- Template HaskellでwrapX関数を生成
wrapX :: Q Exp -> Q Exp
wrapX exp = do
  x <- exp
  let ty = case x of
             SigE _ t -> t
             _        -> error "Expected a type signature"
  if isWrappedType ty
    then return x
    else [| X $exp |]

-- 型がラップされているかどうかをチェック
isWrappedType :: Type -> Bool
isWrappedType (AppT (ConT name) _) = name == ''X
isWrappedType _ = False

{-
ghci> [|param "a" |> True|]
InfixE (Just (AppE (VarE Test.MockCat.Param.param) (LitE (StringL "a")))) (VarE Test.MockCat.Param.|>) (Just (ConE GHC.Types.True))

"a" |> True
any |> True

(eq "a") |> (eq True)

-}

-- -- 関数を生成
-- $(makeWrapX ''X)

-- -- テスト
-- main :: IO ()
-- main = do
--   print $ wrapX 5          -- X 5
--   print $ wrapX (X 5)      -- X 5
--   print $ wrapX "hello"    -- X "hello"
--   print $ wrapX (X "hello") -- X "hello"