{-# LANGUAGE TemplateHaskell #-}
module Test.MockCat.TH where
import Language.Haskell.TH

showExpr :: Q Exp -> Q String
showExpr e = pprint <$> e