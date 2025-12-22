{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Test.MockCat.Internal.Message where

import Data.List (intercalate, maximumBy, elemIndex)
import Data.Ord (comparing)
import Data.Char (isLower)
import Data.Text (pack, replace, unpack)
import Test.MockCat.Internal.Types

message :: Show a => Maybe MockName -> a -> a -> String
message name expected actual =
  let expectedStr = formatStr (show expected)
      actualStr = formatStr (show actual)
      diffLine = "            " <> diffPointer expectedStr actualStr
      mainMessage = "function" <> mockNameLabel name <> " was not applied to the expected arguments."
   in case structuralDiff expectedStr actualStr of
        [] ->
           intercalate "\n"
             [ mainMessage,
               "  expected: " <> expectedStr,
               "   but got: " <> actualStr,
               diffLine
             ]
        ds ->
           let diffMessages = formatDifferences ds
            in intercalate "\n"
                 [ mainMessage,
                   diffMessages,
                   "",
                   "Full context:",
                   "  expected: " <> expectedStr,
                   "   but got: " <> actualStr,
                   diffLine
                 ]

diffPointer :: String -> String -> String
diffPointer expected actual =
  let commonPrefixLen = length $ takeWhile id $ zipWith (==) expected actual
      diffLen = max (length expected) (length actual) - commonPrefixLen
   in replicate commonPrefixLen ' ' <> replicate diffLen '^'

mockNameLabel :: Maybe MockName -> String
mockNameLabel = maybe mempty (" " <>) . enclose "`"

enclose :: String -> Maybe String -> Maybe String
enclose e = fmap (\v -> e <> v <> e)

-- Normalize a show-produced string for consistency in diffs
formatStr :: String -> String
formatStr s
  | null s = s
  | head s == '(' && last s == ')' = "(" <> formatInner (init (tail s)) <> ")"
  | head s == '[' && last s == ')' = "[" <> formatInner (init (tail s)) <> "]" -- Wait, mismatch parens? No, just typo in my thought.
  | head s == '[' && last s == ']' = "[" <> formatInner (init (tail s)) <> "]"
  | head s == '{' && last s == '}' = "{" <> formatInner (init (tail s)) <> "}"
  | otherwise = formatInner s
  where
    formatInner inner =
      let tokens = map (trim . quoteToken . trim) (splitByComma inner)
       in intercalate ", " tokens

-- Helper: quote a token if it looks like an unquoted alpha token
quoteToken :: String -> String
quoteToken s
  | null s = s
  | head s == '"' = s
  | head s == '(' = s
  | head s == '[' = s
  | any isSpecial s = s -- Don't quote if it contains special characters indicating it's not a simple token
  | not (null s) && isLower (head s) = '"' : s ++ "\""
  | otherwise = s
  where
    isSpecial c = c `elem` "{}= "

verifyFailedMessage :: Show a => Maybe MockName -> InvocationList a -> a -> VerifyFailed
verifyFailedMessage name invocationList expected =
  let expectedStr = formatStr (show expected)
      actualStr = formatInvocationList invocationList
      diffLine = "            " <> diffPointer expectedStr actualStr
      mainMessage = "function" <> mockNameLabel name <> " was not called with the expected arguments."
   in VerifyFailed $ case structuralDiff expectedStr actualStr of
        [] ->
           intercalate "\n"
             [ mainMessage,
               "  expected: " <> expectedStr,
               "   but got: " <> actualStr,
               diffLine
             ]
        ds ->
           let diffMessages = formatDifferences ds
            in intercalate "\n"
                 [ mainMessage,
                   diffMessages,
                   "",
                   "Full context:",
                   "  expected: " <> expectedStr,
                   "   but got: " <> actualStr,
                   diffLine
                 ]

data Difference = Difference
  { diffPath :: String,
    diffExpected :: String,
    diffActual :: String
  }
  deriving (Show, Eq)

formatDifferences :: [Difference] -> String
formatDifferences [d] = 
  let label = if null (diffPath d) then "Specific difference:" else "Specific difference in `" <> diffPath d <> "`:"
   in intercalate "\n"
        [ "  " <> label,
          "    expected: " <> diffExpected d,
          "     but got: " <> diffActual d,
          "              " <> diffPointer (diffExpected d) (diffActual d)
        ]
formatDifferences ds = 
  "  Specific differences:\n" <> intercalate "\n" (map formatDiff ds)
  where
    formatDiff d =
      let pathLabel = if null (diffPath d) then "root" else "`" <> diffPath d <> "`"
       in intercalate "\n"
            [ "    - " <> pathLabel <> ":",
              "        expected: " <> diffExpected d,
              "         but got: " <> diffActual d
            ]

structuralDiff :: String -> String -> [Difference]
structuralDiff = structuralDiff' ""

structuralDiff' :: String -> String -> String -> [Difference]
structuralDiff' path expected actual =
  if isList expected && isList actual
    then
      let items1 = extractListItems expected
          items2 = extractListItems actual
          -- We need to track indices for lists
          indexedMismatches = filter (\(_, (i1, i2)) -> i1 /= i2) (zip [0 :: Int ..] (zip items1 items2))
      in concatMap (\(idx, (i1, i2)) ->
           let newPath = path <> "[" <> show idx <> "]"
               nested = structuralDiff' newPath i1 i2
            in if null nested
                 then [Difference newPath i1 i2]
                 else nested
         ) indexedMismatches
    else if isRecord expected && isRecord actual
      then
        let fields1 = extractFields expected
            fields2 = extractFields actual
            mismatches = filter (\(f1, f2) -> f1 /= f2 && isField f1 && isField f2) (zip fields1 fields2)
         in concatMap (\(f1, f2) -> 
              let fieldName = getFieldName f1
                  val1 = getFieldValue f1
                  val2 = getFieldValue f2
                  newPath = if null path then fieldName else path <> "." <> fieldName
                  nested = structuralDiff' newPath val1 val2
               in if null nested
                    then [Difference newPath val1 val2]
                    else nested
            ) mismatches
    else []

-- utilities for message formatting
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile (== ' ')

splitByComma :: String -> [String]
splitByComma = go (0 :: Int) (0 :: Int) (0 :: Int) ""
  where
    go _ _ _ acc [] = if null acc then [] else [trim $ reverse acc]
    go p l b acc (c : cs)
      | c == '(' = go (p + 1) l b (c : acc) cs
      | c == ')' = go (max 0 (p - 1)) l b (c : acc) cs
      | c == '[' = go p (l + 1) b (c : acc) cs
      | c == ']' = go p (max 0 (l - 1)) b (c : acc) cs
      | c == '{' = go p l (b + 1) (c : acc) cs
      | c == '}' = go p l (max 0 (b - 1)) (c : acc) cs
      | c == ',' && p == 0 && l == 0 && b == 0 = (trim $ reverse acc) : go 0 0 0 "" cs
      | otherwise = go p l b (c : acc) cs

formatInvocationList :: Show a => InvocationList a -> String
formatInvocationList invocationList
  | null invocationList = "Function was never called"
  | length invocationList == 1 =
    -- show single element without surrounding list brackets, but quote tokens appropriately
    let s = formatStr (show (head invocationList))
     in s
  | otherwise =
    -- for multiple invocations, show as a list but ensure tokens are quoted where appropriate
    let ss = map (formatStr . show) invocationList
     in "[" <> intercalate ", " ss <> "]"

_replace :: Show a => String -> a -> String
_replace r s = unpack $ replace (pack r) (pack "") (pack (show s))

messageForMultiMock :: Show a => Maybe MockName -> [a] -> a -> String
messageForMultiMock name expecteds actual =
  let expectedStrs = map (formatStr . show) expecteds
      actualStr = formatStr (show actual)
      nearest = chooseNearest actualStr expectedStrs
   in intercalate
        "\n"
        [ "function" <> mockNameLabel name <> " was not called with the expected arguments.",
          "  expected one of the following:",
          intercalate "\n" $ ("    " <>) <$> expectedStrs,
          "  but got:",
          "    " <> actualStr,
          "    " <> diffPointer nearest actualStr
        ]

chooseNearest :: String -> [String] -> String
chooseNearest _ [] = ""
chooseNearest actual expectations =
  let commonPrefixLen s1 s2 = length $ takeWhile id $ zipWith (==) s1 s2
      scores = map (\e -> (commonPrefixLen actual e, e)) expectations
   in snd $ maximumBy (comparing fst) scores


isRecord :: String -> Bool
isRecord s = '{' `elem` s && '}' `elem` s

extractFields :: String -> [String]
extractFields s = maybe [] splitByComma (extractInner '{' '}' s)

extractInner :: Char -> Char -> String -> Maybe String
extractInner open close s =
  case dropWhile (/= open) s of
    (x:rest) | x == open -> Just (takeBalanced (1 :: Int) rest)
    _ -> Nothing
  where
    takeBalanced :: Int -> String -> String
    takeBalanced 0 _ = ""
    takeBalanced _ [] = ""
    takeBalanced n (c:cs)
      | c == open = c : takeBalanced (n+1) cs
      | c == close = if n == 1 then "" else c : takeBalanced (n-1) cs
      | otherwise = c : takeBalanced n cs

isField :: String -> Bool
isField = ('=' `elem`)

getFieldName :: String -> String
getFieldName = trim . takeWhile (/= '=')

getFieldValue :: String -> String
getFieldValue = trim . drop 1 . dropWhile (/= '=')

isList :: String -> Bool
isList s = not (null s) && head s == '[' && last s == ']'

extractListItems :: String -> [String]
extractListItems s = maybe [] splitByComma (extractInner '[' ']' s)

listMismatchIndex :: [String] -> [String] -> Maybe Int
listMismatchIndex s1 s2 = elemIndex False (zipWith (==) s1 s2)

verifyOrderFailedMesssage :: Show a => VerifyOrderResult a -> String
verifyOrderFailedMesssage VerifyOrderResult {index, appliedValue, expectedValue} =
  let callIndex = showHumanReadable (index + 1)
      expectedStr = formatStr (show expectedValue)
      actualStr = formatStr (show appliedValue)
      prefix = "   but got " <> callIndex <> " call: "
      spaces = replicate (length prefix) ' '
   in intercalate
        "\n"
        [ "  expected " <> callIndex <> " call: " <> expectedStr,
          prefix <> actualStr,
          spaces <> diffPointer expectedStr actualStr
        ]
  where
    showHumanReadable :: Int -> String
    showHumanReadable 1 = "1st"
    showHumanReadable 2 = "2nd"
    showHumanReadable 3 = "3rd"
    showHumanReadable n = show n <> "th"