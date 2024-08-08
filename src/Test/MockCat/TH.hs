{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.MockCat.TH (showExp, expectByExpr, makeMock) where

import Language.Haskell.TH (Exp (..), Lit (..), Pat (..), Q, pprint, Name, Dec (..), Info (..), reify, conT, instanceD, cxt, appT, varT, mkName, DecQ, litE, stringL, normalB, clause, funD, varP, litT, strTyLit, Type (..), conP, Quote (newName), runQ, Cxt, Pred, TyVarBndr (..))
import Language.Haskell.TH.PprLib (Doc, hcat, parens, text, empty)
import Language.Haskell.TH.Syntax (nameBase)
import Test.MockCat.Param (Param(..))
import Test.MockCat.MockT
import Data.Data (Proxy(..))
import Data.List (find, nub)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.State (modify, get)
import Data.Maybe (fromMaybe, mapMaybe)
import GHC.IO (unsafePerformIO)
import Control.Monad (replicateM)
import Language.Haskell.TH.Lib
import Text.Printf (IsChar(toChar))
import Language.Haskell.TH (TyVarBndr)

showExp :: Q Exp -> Q String
showExp qexp = show . pprintExp <$> qexp

pprintExp :: Exp -> Doc
pprintExp (VarE name) = text (nameBase name)
pprintExp (ConE name) = text (nameBase name)
pprintExp (LitE lit) = pprintLit lit
pprintExp (AppE e1 e2) = parens $ hcat [pprintExp e1, text " ", pprintExp e2]
pprintExp (InfixE e1 e2 e3) = pprintInfixE e1 e2 e3
pprintExp (LamE pats body) = parens $ hcat [text "\\", pprintPats pats, text " -> ", pprintExp body]
pprintExp (TupE exps) = parens $ hcat (map (maybe (text "") pprintExp) exps)
pprintExp (ListE exps) = parens $ hcat (map pprintExp exps)
pprintExp (SigE e _) = pprintExp e
pprintExp x = text (pprint x)

pprintInfixE :: Maybe Exp -> Exp -> Maybe Exp -> Doc
pprintInfixE e1 e2 e3 =
  parens $
    hcat
      [ maybe (text "") pprintExp e1,
        maybe (text "") (const (text " ")) e1,
        pprintExp e2,
        text " ",
        maybe (text "") pprintExp e3
      ]

pprintPats :: [Pat] -> Doc
pprintPats = hcat . map pprintPat

pprintPat :: Pat -> Doc
pprintPat (VarP name) = text (nameBase name)
pprintPat p = text (pprint p)

pprintLit :: Lit -> Doc
pprintLit (IntegerL n) = text (show n)
pprintLit (RationalL r) = text (show r)
pprintLit (StringL s) = text (show s)
pprintLit (CharL c) = text (show c)
pprintLit l = text (pprint l)

{- | Create a conditional parameter based on @Q Exp@. 

  In applying a mock function, if the argument does not satisfy this condition, an error is raised.

  The conditional expression is displayed in the error message.
-}
expectByExpr :: Q Exp -> Q Exp
expectByExpr qf = do
  str <- showExp qf
  [|ExpectCondition $qf str|]

-- Template Haskell code
makeMock :: Q Type -> Q [Dec]
makeMock t = generateMock t

className :: Type -> Name
className (ConT name) = name
className d = error $ "unsupported class definition: " <> show d

{-
  1. 制約 => Clazz x y z where の パラメーター x y z を抽出。
  2. x, y, z のうちどれがMonadのパラメーターなのかを制約から探す。
  3. MonadかMonadのラッパーになっているやつを探し出す。
  4. MonadのラッパーがなかったらそもそもNGとする。
  5. A m, B n のようにモナドのラッパーごとにvarが異なるのはNGとする。
  6. A m, B m のようにモナドのラッパーのvarが同じならOK
     A m n, B m x のように複数varがあっても共通したvarがあればOK
  7. 共通するvarをMockTのvarとする。
     A m n o のように複数ある場合、A (MockT m) n o とする。
  8. 制約はそのまま使うが、A m nのようになっている場合 A (MockT m) n のように置き換える。
-}
data CxtInfo = CxtInfo { t :: Type,  monadName :: Maybe Name} deriving (Show)

cxtInfo :: Type -> CxtInfo
cxtInfo = undefined

cxtInfos :: [Type] -> [CxtInfo]
cxtInfos types = cxtInfo <$> types

extractMonadVarName :: Type -> Maybe Name
extractMonadVarName (AppT (ConT name) (VarT varName))
  | name == ''Monad = Just varName
extractMonadVarName _ = Nothing

findMonadVarNames :: [Type] -> [Name]
findMonadVarNames = mapMaybe extractMonadVarName

getClassNames :: Type -> Q [Name]
getClassNames (AppT (ConT name) _) = pure [name]
getClassNames (AppT t _) = getClassNames t
getClassNames (ForallT _ _ pred) = getClassNames pred
getClassNames _ = pure []

_extractMonadVarNames types names = do
  -- 1階層目のMonadになってるNameを集める
  let monadVarNames = findMonadVarNames types
  -- 1階層目のクラスを集める
  classNames <- concat <$> mapM getClassNames types
  -- 祖先のMonadになってるNameを集める
  ancestorMonadVarNames <- concat <$> mapM (recExtractMonadVarNames names) classNames

  pure $ monadVarNames ++ ancestorMonadVarNames
  where
  recExtractMonadVarNames acc className = do
    if className `elem` acc then pure []
    else do
      ClassI (ClassD types _ _ _ _) _ <- reify className
      _extractMonadVarNames types (className : acc)

extractMonadVarNames :: [Type] -> Q [Name]
extractMonadVarNames types = _extractMonadVarNames types []

generateMock :: Q Type -> Q [Dec]
generateMock t = do
  name <- className <$> t
  reify name >>= \case
    ClassI (ClassD _ _ [] _ _) _ -> fail $ "A type parameter is required for class " <> show name
    ClassI (ClassD ctx _ typeParameters funDeps methods) _ -> do
      x <- extractMonadVarNames ctx
      if length (nub x) > 1 then fail "Monad parameter must be unique."
      else if null x then fail "Monad parameter not found."
      else do
        --fail $ "monad params:" <> show x
        fail $ show ctx <> " ===" <> show typeParameters
        let classNameStr = nameBase name

        instDec <- instanceD
          --(cxt [appT (conT ''Monad) (varT $ mkName "m")])
          (pure ctx)
          (appT (conT name) (appT (conT ''MockT) (varT $ mkName "m")))
          (map (generateMockMethod classNameStr) methods)

        funcDecs <- mapM (generateMockFunction classNameStr) methods

        pure $ instDec : funcDecs
    t -> error $ "unsupported type: " <> show t

createInstanceType :: Name -> [TyVarBndr ()] -> Q Type
createInstanceType className tvbs = do
  let types = map tyVarBndrToType tvbs
  pure $ foldl AppT (ConT className) types

tyVarBndrToType :: TyVarBndr () -> Type
tyVarBndrToType (PlainTV name _) = VarT name
tyVarBndrToType (KindedTV name _ kind) = SigT (VarT name) kind

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates xs = length xs /= length (nub xs)

generateMockMethod :: String -> Dec -> Q Dec
generateMockMethod classNameStr (SigD funName funType) = do
  names <- sequence $ typeToNames funType
  let r = mkName "result"
      params = varP <$> names
      args = varE <$> names
      funNameStr = "_" <> nameBase funName

      funBody =  [| MockT $ do
                      defs <- get
                      let mock = fromMaybe (error $ "no answer found stub function `" ++ funNameStr ++ "`.") $ findParam (Proxy :: Proxy $(litT (strTyLit funNameStr))) defs
                          $(bangP $ varP r) = $(generateStubFnCall [| mock |] args)
                      pure $(varE r) |]
      funClause = clause params (normalB funBody) []

  -- x <- mapM runQ params
  -- y <- mapM runQ args
  -- error $ show names <> ":" <> show x <> " ==> " <> show y
  -- x <- mapM runQ params
  -- error $ show funType <> " ==> " <> pprint x

  funD funName [funClause]
generateMockMethod classNameStr _ = error "adfasd"

generateStubFnCall :: Q Exp -> [Q Exp] -> Q Exp
generateStubFnCall mock args = do
  foldl appE [| stubFn $(mock) |] args

generateMockFunction :: String -> Dec -> Q Dec
generateMockFunction classNameStr (SigD funName funType) = do
  let funNameStr = "_" <> nameBase funName
      mockFunName = mkName funNameStr
      mockBody = [| MockT $ modify (++ [Definition
                      (Proxy :: Proxy $(litT (strTyLit funNameStr)))
                      (unsafePerformIO $ createNamedMock $(litE (stringL funNameStr)) p)
                      shouldApplyAnythingTo]) |]

  funD mockFunName [clause [varP $ mkName "p"] (normalB mockBody) []]
generateMockFunction classNameStr _ = error ""

findParam :: KnownSymbol sym => Proxy sym -> [Definition] -> Maybe a
findParam pa definitions = do
  let definition = find (\(Definition s _ _) -> symbolVal s == symbolVal pa) definitions
  fmap (\(Definition _ mock _) -> unsafeCoerce mock) definition

typeToNames :: Type -> [Q Name]
typeToNames (AppT (AppT ArrowT t1) t2) = [newName "a"] ++ typeToNames t2
typeToNames _ = []
