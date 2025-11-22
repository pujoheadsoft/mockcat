{-# LANGUAGE TemplateHaskellQuotes #-}
module Test.MockCat.TH.ClassAnalysis
  ( ClassName2VarNames (..),
    VarName2ClassNames (..),
    toClassInfos,
    toClassInfo,
    getTypeNames,
    filterClassInfo,
    filterMonadicVarInfos,
    hasMonadInVarInfo,
    getClassName,
    getClassNames,
    VarAppliedType (..),
    applyVarAppliedTypes,
    updateType,
    findClass,
    hasClass
  )
where

import Control.Monad (guard)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (pack, splitOn, unpack)
import Language.Haskell.TH
  ( Name,
    Pred,
    Type (..),
  )

-- From former ClassInfo.hs
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

-- From former VarApplied.hs
data VarAppliedType = VarAppliedType Name (Maybe Name)
  deriving (Show)

applyVarAppliedTypes :: [VarAppliedType] -> Type -> Type
applyVarAppliedTypes varAppliedTypes = transform
  where
    mapping =
      Map.fromList
        [ (varName, className)
        | VarAppliedType varName (Just className) <- varAppliedTypes
        ]
    transform (VarT n) =
      case Map.lookup n mapping of
        Just className -> ConT className
        Nothing -> VarT n
    transform (AppT t1 t2) = AppT (transform t1) (transform t2)
    transform (SigT t k) = SigT (transform t) k
    transform (ParensT t) = ParensT (transform t)
    transform (InfixT t1 n t2) = InfixT (transform t1) n (transform t2)
    transform (UInfixT t1 n t2) = UInfixT (transform t1) n (transform t2)
    transform (ForallT tvs ctx t) = ForallT tvs (map transform ctx) (transform t)
    transform t = t

updateType :: Type -> [VarAppliedType] -> Type
updateType (AppT (VarT v1) (VarT v2)) varAppliedTypes =
  let x = maybe (VarT v1) ConT (findClass v1 varAppliedTypes)
      y = maybe (VarT v2) ConT (findClass v2 varAppliedTypes)
   in AppT x y
updateType ty _ = ty

hasClass :: Name -> [VarAppliedType] -> Bool
hasClass varName = any (\(VarAppliedType v c) -> v == varName && isJust c)

findClass :: Name -> [VarAppliedType] -> Maybe Name
findClass varName types = do
  guard $ hasClass varName types
  (VarAppliedType _ c) <- find (\(VarAppliedType v _) -> v == varName) types
  c


