{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Test.MockCat.Internal.Message where

import Data.List (intercalate, maximumBy)
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
        Just sd ->
           intercalate "\n"
             [ mainMessage,
               sd,
               "",
               "Full context:",
               "  expected: " <> expectedStr,
               "   but got: " <> actualStr,
               diffLine
             ]
        Nothing ->
           intercalate "\n"
             [ mainMessage,
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
verifyFailedMessage name appliedParams expected =
  let expectedStr = formatStr (show expected)
      actualStr = formatAppliedParamsList appliedParams
      diffLine = "            " <> diffPointer expectedStr actualStr
      mainMessage = "function" <> mockNameLabel name <> " was not applied to the expected arguments."
   in VerifyFailed $ case structuralDiff expectedStr actualStr of
        Just sd ->
           intercalate "\n"
             [ mainMessage,
               sd,
               "",
               "Full context:",
               "  expected: " <> expectedStr,
               "   but got: " <> actualStr,
               diffLine
             ]
        Nothing ->
           intercalate "\n"
             [ mainMessage,
               "  expected: " <> expectedStr,
               "   but got: " <> actualStr,
               diffLine
             ]

-- utilities for message formatting
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile (== ' ')

splitByComma :: String -> [String]
splitByComma = go 0 0 0 ""
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

formatAppliedParamsList :: Show a => InvocationList a -> String
formatAppliedParamsList appliedParams
  | null appliedParams = "It has never been applied"
  | length appliedParams == 1 =
    -- show single element without surrounding list brackets, but quote tokens appropriately
    let s = formatStr (show (head appliedParams))
     in s
  | otherwise =
    -- for multiple applied params, show as a list but ensure tokens are quoted where appropriate
    let ss = map (formatStr . show) appliedParams
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
        [ "function" <> mockNameLabel name <> " was not applied to the expected arguments.",
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

structuralDiff :: String -> String -> Maybe String
structuralDiff expected actual =
  if '{' `elem` expected && '{' `elem` actual
    then
      let extractFields s =
            let inner = takeWhile (/= '}') $ drop 1 $ dropWhile (/= '{') s
             in splitByComma inner
          fields1 = extractFields expected
          fields2 = extractFields actual
          mismatches = filter (\(f1, f2) -> f1 /= f2 && '=' `elem` f1 && '=' `elem` f2) (zip fields1 fields2)
       in case mismatches of
            ((f1, f2) : _) ->
              let val1 = trim $ drop 1 $ dropWhile (/= '=') f1
                  val2 = trim $ drop 1 $ dropWhile (/= '=') f2
                  fieldName = trim $ takeWhile (/= '=') f1
               in Just $ intercalate "\n"
                    [ "  Specific difference in `" <> fieldName <> "`:",
                      "    expected: " <> val1,
                      "     but got: " <> val2,
                      "             " <> diffPointer val1 val2
                    ]
            _ -> Nothing
    else Nothing

verifyOrderFailedMesssage :: Show a => VerifyOrderResult a -> String
verifyOrderFailedMesssage VerifyOrderResult {index, appliedValue, expectedValue} =
  let appliedCount = showHumanReadable (index + 1)
      expectedStr = formatStr (show expectedValue)
      actualStr = formatStr (show appliedValue)
      prefix = "   but got " <> appliedCount <> " applied: "
      spaces = replicate (length prefix) ' '
   in intercalate
        "\n"
        [ "  expected " <> appliedCount <> " applied: " <> expectedStr,
          prefix <> actualStr,
          spaces <> diffPointer expectedStr actualStr
        ]
  where
    showHumanReadable :: Int -> String
    showHumanReadable 1 = "1st"
    showHumanReadable 2 = "2nd"
    showHumanReadable 3 = "3rd"
    showHumanReadable n = show n <> "th"