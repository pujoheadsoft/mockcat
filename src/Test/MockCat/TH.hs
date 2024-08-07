{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Test.MockCat.TH (showExp, expectByExpr, makeMock) where

import Language.Haskell.TH (Exp (..), Lit (..), Pat (..), Q, pprint, Name, Dec (..), Info (..), reify, conT, instanceD, cxt, appT, varT, mkName, DecQ, litE, stringL, normalB, clause, funD, varP, litT, strTyLit)
import Language.Haskell.TH.PprLib (Doc, hcat, parens, text)
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
  let funNameStr = "_" <> nameBase funName
      funBody = [| MockT $ do
                      defs <- get
                      let mock = fromMaybe (error $ "no answer found stub function `" ++ funNameStr ++ "`.") $ findParam (Proxy :: Proxy $(litT (strTyLit funNameStr))) defs
                          !result = stubFn mock
                      pure result|]
  let funClause = clause [] (normalB funBody) []

  funD funName [funClause]
generateMockMethod classNameStr _ = error "adfasd"

generateMockFunction :: String -> Dec -> Q Dec
generateMockFunction classNameStr (SigD funName funType) = do
  let funNameStr = nameBase funName
      mockFunName = mkName $ "_" ++ funNameStr
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