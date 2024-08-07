{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.MockCat.TH (showExp, expectByExpr, makeMock) where

import Language.Haskell.TH (Exp (..), Lit (..), Pat (..), Q, pprint, Name, Dec (..), Info (..), reify, conT, instanceD, cxt, appT, varT, mkName, DecQ, litE, stringL, normalB, clause, funD, varP, litT, strTyLit, Type (..), conP, Quote (newName), runQ)
import Language.Haskell.TH.PprLib (Doc, hcat, parens, text, empty)
import Language.Haskell.TH.Syntax (nameBase)
import Test.MockCat.Param (Param(..))
import Test.MockCat.MockT
import Data.Data (Proxy(..))
import Data.List (find)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.State (modify, get)
import Data.Maybe (fromMaybe)
import GHC.IO (unsafePerformIO)
import Control.Monad (replicateM)
import Language.Haskell.TH.Lib
import Text.Printf (IsChar(toChar))

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
makeMock :: [Name] -> Q [Dec]
makeMock classNames = concat <$> mapM generateMock classNames

generateMock :: Name -> Q [Dec]
generateMock className = reify className >>= \case
  ClassI (ClassD _ _ _ _ methods) _ -> do
    let classNameStr = nameBase className
        classType = conT className

    instDec <- instanceD
      (cxt [appT (conT ''Monad) (varT $ mkName "m")])
      (appT classType (appT (conT ''MockT) (varT $ mkName "m")))
      (map (generateMockMethod classNameStr) methods)

    funcDecs <- mapM (generateMockFunction classNameStr) methods

    pure $ instDec : funcDecs
  _ -> error "xxxx"

generateMockMethod :: String -> Dec -> Q Dec
generateMockMethod classNameStr (SigD funName funType) = do
  names <- sequence $ typeToNames funType
  let 
      params = varP <$> names
      args = varE <$> names
      funNameStr = "_" <> nameBase funName
      funBody =  [| MockT $ do
                      defs <- get
                      let mock = fromMaybe (error $ "no answer found stub function `" ++ funNameStr ++ "`.") $ findParam (Proxy :: Proxy $(litT (strTyLit funNameStr))) defs
                          !result = $(generateStubFnCall [| mock |] args)
                      pure result |]
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
  where
    patToExp (VarP name) = varE name
    patToExp _ = error "Unsupported pattern"


patToExp :: Quote m => m Pat -> m Exp
patToExp pat = (\p -> case p of
  (VarP name) -> VarE name
  _ -> error "Unsupported pattern"
  ) <$> pat

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

typeToPats :: Type -> [Q Pat]
typeToPats (AppT (AppT ArrowT t1) t2) = [varP =<< newName "a"] ++ typeToPats t2
typeToPats _ = []

typeToNames :: Type -> [Q Name]
typeToNames (AppT (AppT ArrowT t1) t2) = [newName "a"] ++ typeToNames t2
typeToNames _ = []

typeToPatList :: Type -> [Q Pat]
typeToPatList (ArrowT `AppT` t1 `AppT` t2) = do
  let
    p1 = typeToPat t1
    ps = typeToPatList t2
  p1 : ps
typeToPatList t = do
  let p = typeToPat t
  [p]

typeToPat :: Type -> Q Pat
typeToPat (ConT name) = conP name []
typeToPat (AppT t1 t2) = do
  let
    p1 = typeToPat t1
    p2 = typeToPat t2
  conP (mkName ":") [p1, p2]
typeToPat (TupleT 0) = tupP []
typeToPat (TupleT n) = do
  ps <- replicateM n (newName "x" >>= varP)
  tupP $ pure <$> ps
typeToPat ListT = do
  p <- newName "xs" >>= varP
  listP [pure p]
typeToPat (VarT name) = varP name
typeToPat a = error $ "Unsupported type" <> show a