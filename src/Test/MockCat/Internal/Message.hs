{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Test.MockCat.Internal.Message where

import Data.List (intercalate, maximumBy)
import Data.Ord (comparing)
import Data.Char (isLower, isSpace)
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
formatStr s =
  let inner = if not (null s) && head s == '(' && last s == ')' then init (tail s) else s
      tokens = map (trim . quoteToken . trim) (splitByComma inner)
   in if not (null s) && head s == '(' && last s == ')'
        then "(" <> intercalate ", " tokens <> ")"
        else intercalate ", " tokens

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
splitByComma s = case break (== ',') s of
  (a, ',' : rest) -> a : splitByComma rest
  (a, _) -> [a]

formatAppliedParamsList :: Show a => InvocationList a -> String
formatAppliedParamsList appliedParams
  | null appliedParams = "It has never been applied"
  | length appliedParams == 1 =
    -- show single element without surrounding list brackets, but quote tokens appropriately
    let s = show (head appliedParams)
        inner = if not (null s) && head s == '(' && last s == ')' then init (tail s) else s
        tokens = map (trim . quoteToken . trim) (splitByComma inner)
     in intercalate ", " tokens
  | otherwise =
    -- for multiple applied params, show as a list but ensure tokens are quoted where appropriate
    let ss = map show appliedParams
        processed = map (\t -> let inner = if not (null t) && head t == '(' && last t == ')' then init (tail t) else t
                                 in intercalate ", " $ map (trim . quoteToken . trim) (splitByComma inner)) ss
     in show processed

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
  let parts1 = map trim $ splitByComma expected
      parts2 = map trim $ splitByComma actual
      mismatches = filter (\(p1, p2) -> p1 /= p2 && '=' `elem` p1 && '=' `elem` p2) (zip parts1 parts2)
   in case mismatches of
        ((p1, p2) : _) ->
          let fieldName = trim $ takeWhile (not . (\c -> isSpace c || c == '=')) (dropWhile (not . isLower) (takeWhile (/= '=') p1))
              val1 = trim $ takeWhile (/= '}') $ drop 1 $ dropWhile (/= '=') p1
              val2 = trim $ takeWhile (/= '}') $ drop 1 $ dropWhile (/= '=') p2
           in Just $ intercalate "\n"
                [ "Mismatch in field `" <> fieldName <> "`:",
                  "  expected: " <> val1,
                  "   but got: " <> val2,
                  "            " <> diffPointer val1 val2
                ]
        _ -> Nothing

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