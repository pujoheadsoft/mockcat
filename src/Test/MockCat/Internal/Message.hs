module Test.MockCat.Internal.Message where

import Data.List (intercalate)
import Data.Char (isLower)
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


