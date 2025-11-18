{-# LANGUAGE TemplateHaskell #-}

module Test.MockCat.TH.ClassInfo
  ( ClassName2VarNames (..),
    VarName2ClassNames (..),
    toClassInfos,
    toClassInfo,
    getTypeNames,
    filterClassInfo,
    filterMonadicVarInfos,
    hasMonadInVarInfo,
    getClassName,
    getClassNames
  )
where

import Data.Text (pack, splitOn, unpack)
import Language.Haskell.TH
  ( Name,
    Pred,
    Type (..),
  )

data ClassName2VarNames = ClassName2VarNames Name [Name]

instance Show ClassName2VarNames where
  show (ClassName2VarNames cName varNames) = showClassDef cName varNames

data VarName2ClassNames = VarName2ClassNames Name [Name]

instance Show VarName2ClassNames where
  show (VarName2ClassNames varName classNames) = show varName <> " class is " <> unwords (showClassName <$> classNames)

toClassInfos :: [Pred] -> [ClassName2VarNames]
toClassInfos = map toClassInfo

toClassInfo :: Pred -> ClassName2VarNames
toClassInfo (AppT t1 t2) =
  let (ClassName2VarNames name vars) = toClassInfo t1
   in ClassName2VarNames name (vars ++ getTypeNames t2)
toClassInfo (ConT name) = ClassName2VarNames name []
toClassInfo _ = error "Unsupported Type structure"

getTypeNames :: Pred -> [Name]
getTypeNames (VarT name) = [name]
getTypeNames (ConT name) = [name]
getTypeNames _ = []

filterClassInfo :: Name -> [ClassName2VarNames] -> [ClassName2VarNames]
filterClassInfo name = filter (hasVarName name)
  where
    hasVarName :: Name -> ClassName2VarNames -> Bool
    hasVarName target (ClassName2VarNames _ varNames) = target `elem` varNames

filterMonadicVarInfos :: [VarName2ClassNames] -> [VarName2ClassNames]
filterMonadicVarInfos = filter hasMonadInVarInfo

hasMonadInVarInfo :: VarName2ClassNames -> Bool
hasMonadInVarInfo (VarName2ClassNames _ classNames) = ''Monad `elem` classNames

getClassName :: Type -> Name
getClassName (ConT name) = name
getClassName (AppT ty _) = getClassName ty
getClassName d = error $ "unsupported class definition: " <> show d

getClassNames :: Type -> [Name]
getClassNames (AppT (ConT name1) (ConT name2)) = [name1, name2]
getClassNames (AppT ty (ConT name)) = getClassNames ty ++ [name]
getClassNames (AppT ty1 ty2) = getClassNames ty1 ++ getClassNames ty2
getClassNames _ = []

showClassName :: Name -> String
showClassName n = splitLast "." $ show n

showClassDef :: Name -> [Name] -> String
showClassDef className varNames = showClassName className <> " " <> unwords (show <$> varNames)

splitLast :: String -> String -> String
splitLast delimiter = last . split delimiter

split :: String -> String -> [String]
split delimiter str = unpack <$> splitOn (pack delimiter) (pack str)


