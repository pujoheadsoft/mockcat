{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
module Test.MockCat.TH where
import Language.Haskell.TH (Exp (..), Q, Pat (..), Lit (..), pprint)
import Language.Haskell.TH.PprLib (Doc, hcat, text, parens)
import Language.Haskell.TH.Syntax (nameBase)

showExpr :: Q Exp -> Q String
showExpr qexp = show . pprExp <$> qexp

pprExp :: Exp -> Doc
pprExp (VarE name)         = text (nameBase name)
pprExp (ConE name)         = text (nameBase name)
pprExp (LitE lit)          = pprLit lit
pprExp (AppE e1 e2)        = parens $ hcat [pprExp e1, text " ", pprExp e2]
pprExp (InfixE e1 e2 e3)   = pprInfixE e1 e2 e3
pprExp (LamE pats body)    = parens $ hcat [text "\\", pprPats pats, text " -> ", pprExp body]
pprExp (TupE exps)         = parens $ hcat (map (maybe (text "") pprExp) exps)
pprExp (ListE exps)        = parens $ hcat (map pprExp exps)
pprExp (SigE e _)          = pprExp e
pprExp x                   = text (pprint x)

pprInfixE :: Maybe Exp -> Exp -> Maybe Exp -> Doc
pprInfixE e1 e2 e3 = parens $ hcat [
  maybe (text "") pprExp e1,
  maybe (text "") (const (text " ")) e1,
  pprExp e2,
  text " ",
  maybe (text "") pprExp e3]

pprPats :: [Pat] -> Doc
pprPats = hcat . map pprPat

pprPat :: Pat -> Doc
pprPat (VarP name) = text (nameBase name)
pprPat p           = text (pprint p)

pprLit :: Lit -> Doc
pprLit (IntegerL n) = text (show n)
pprLit (RationalL r) = text (show r)
pprLit (StringL s) = text (show s)
pprLit (CharL c) = text (show c)
pprLit l = text (pprint l)
