module Test.MockCat.Internal.Message where

import Data.List (intercalate)
import Data.Char (isLower)
import Data.Text (pack, replace, unpack)
import Test.MockCat.Internal.Types

message :: Show a => Maybe MockName -> a -> a -> String
message name expected actual =
  intercalate
    "\n"
    [ "function" <> mockNameLabel name <> " was not applied to the expected arguments.",
      "  expected: " <> showForMessage (show expected),
      "   but got: " <> showForMessage (show actual)
    ]

mockNameLabel :: Maybe MockName -> String
mockNameLabel = maybe mempty (" " <>) . enclose "`"

enclose :: String -> Maybe String -> Maybe String
enclose e = fmap (\v -> e <> v <> e)

-- Quote a show-produced string when appropriate for error messages.
showForMessage :: String -> String
showForMessage s =
  -- if it's a parenthesised compound, keep as-is; otherwise quote alpha-only tokens
  let trimmed = s
   in if not (null trimmed) && head trimmed == '(' && last trimmed == ')'
        then trimmed
        else quoteToken trimmed

-- Helper: quote a token if it looks like an unquoted alpha token
quoteToken :: String -> String
quoteToken s
  | null s = s
  | head s == '"' = s
  | head s == '(' = s
  | head s == '[' = s
  | not (null s) && isLower (head s) = '"' : s ++ "\""
  | otherwise = s

verifyFailedMessage :: Show a => Maybe MockName -> AppliedParamsList a -> a -> VerifyFailed
verifyFailedMessage name appliedParams expected =
  VerifyFailed $
    intercalate
      "\n"
      [ "function" <> mockNameLabel name <> " was not applied to the expected arguments.",
        "  expected: " <> showForMessage (show expected),
        "   but got: " <> formatAppliedParamsList appliedParams
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

formatAppliedParamsList :: Show a => AppliedParamsList a -> String
formatAppliedParamsList appliedParams
  | null appliedParams = "It has never been applied"
  | length appliedParams == 1 =
    -- show single element without surrounding list brackets, but quote tokens appropriately
    let s = show (head appliedParams)
        inner = if not (null s) && head s == '(' && last s == ')' then init (tail s) else s
        tokens = map (trim . quoteToken . trim) (splitByComma inner)
     in intercalate "," tokens
  | otherwise =
    -- for multiple applied params, show as a list but ensure tokens are quoted where appropriate
    let ss = map show appliedParams
        processed = map (\t -> let inner = if not (null t) && head t == '(' && last t == ')' then init (tail t) else t
                                in intercalate "," $ map (trim . quoteToken . trim) (splitByComma inner)) ss
     in show processed

_replace :: Show a => String -> a -> String
_replace r s = unpack $ replace (pack r) (pack "") (pack (show s))

