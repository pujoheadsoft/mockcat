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
      mainMessage = "function" <> mockNameLabel name <> " was not called with the expected arguments."
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
formatStr s =
  case s of
    [] -> s
    c:cs ->
      case reverse cs of
        [] -> formatInner s
        l:ls ->
          let inner = reverse ls
          in case (c, l) of
               ('(', ')') -> "(" <> formatInner inner <> ")"
               ('[', ')') -> "[" <> formatInner inner <> "]" -- Preserving the weird legacy case
               ('[', ']') -> "[" <> formatInner inner <> "]"
               ('{', '}') -> "{" <> formatInner inner <> "}"
               _          -> formatInner s
  where
    formatInner inner =
      let tokens = map (trim . quoteToken . trim) (splitByComma inner)
       in intercalate ", " tokens

-- Helper: quote a token if it looks like an unquoted alpha token
quoteToken :: String -> String
quoteToken s = case s of
  [] -> s
  '"':_ -> s
  '(':_ -> s
  '[':_ -> s
  c:_
    | any isSpecial s -> s
    | isLower c -> '"' : s ++ "\""
    | otherwise -> s
  where
    isSpecial c = c `elem` "{}= "

verifyFailedMessage :: Show a => Maybe MockName -> InvocationList a -> a -> VerifyFailed
verifyFailedMessage name invocationList expected =
  let expectedStr = formatStr (show expected)
      mainMessage = "function" <> mockNameLabel name <> " was not called with the expected arguments."
   in VerifyFailed $ case invocationList of
        [] ->
          intercalate "\n"
            [ mainMessage,
              "  expected: " <> expectedStr,
              "  but the function was never called"
            ]
        _ -> countWithArgsMismatchMessageWithDiff name expected invocationList

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
structuralDiff' path expected actual
  | isList expected && isList actual = diffLists path expected actual
  | isRecord expected && isRecord actual = diffRecords path expected actual
  | otherwise = []

diffLists :: String -> String -> String -> [Difference]
diffLists path expected actual =
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

diffRecords :: String -> String -> String -> [Difference]
diffRecords path expected actual =
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

-- utilities for message formatting
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile (== ' ')

splitByComma :: String -> [String]
splitByComma = go (0 :: Int) (0 :: Int) (0 :: Int) ""
  where
    go _ _ _ acc [] = [trim $ reverse acc | not (null acc)]
    go p l b acc (c : cs)
      | c == '(' = go (p + 1) l b (c : acc) cs
      | c == ')' = go (max 0 (p - 1)) l b (c : acc) cs
      | c == '[' = go p (l + 1) b (c : acc) cs
      | c == ']' = go p (max 0 (l - 1)) b (c : acc) cs
      | c == '{' = go p l (b + 1) (c : acc) cs
      | c == '}' = go p l (max 0 (b - 1)) (c : acc) cs
      | c == ',' && p == 0 && l == 0 && b == 0 = trim (reverse acc) : go 0 0 0 "" cs
      | otherwise = go p l b (c : acc) cs

formatInvocationList :: Show a => InvocationList a -> String
formatInvocationList invocationList = case invocationList of
  [] -> "(never called)"
  [x] -> formatStr (show x)
  _ -> "[" <> intercalate ", " (map (formatStr . show) invocationList) <> "]"

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

countWithArgsMismatchMessageWithDiff :: Show a => Maybe MockName -> a -> [a] -> String
countWithArgsMismatchMessageWithDiff mockName expected actuals =
  let expectedStr = formatStr (show expected)
      actualStrs = map (formatStr . show) actuals
      (nearest, nearestIdx) = chooseNearestWithIndex expectedStr actualStrs
      diffLine = "              " <> diffPointer expectedStr nearest

      header = "function" <> mockNameLabel mockName <> " was not called with the expected arguments."

      closestMatchSection =
        intercalate "\n"
          [ "  Closest match:"
          , "    expected: " <> expectedStr
          , "     but got: " <> nearest
          , diffLine
          ]

      diffs = case structuralDiff expectedStr nearest of
        [] -> ""
        ds -> formatDifferences ds

      historySection = formatCallHistory actualStrs nearestIdx
   in intercalate "\n" $
        [ header
        , ""
        , closestMatchSection
        ]
        ++ (if null diffs then [] else [diffs])
        ++
        [ ""
        , historySection
        ]

chooseNearestWithIndex :: String -> [String] -> (String, Int)
chooseNearestWithIndex _ [] = ("", -1)
chooseNearestWithIndex pivot candidates =
  let commonPrefixLen s1 s2 = length $ takeWhile id $ zipWith (==) s1 s2
      scores = zipWith (\idx c -> (commonPrefixLen pivot c, c, idx)) [0..] candidates
      (_, bestStr, bestIdx) = maximumBy (comparing (\(score, _, _) -> score)) scores
   in (bestStr, bestIdx)

formatCallHistory :: [String] -> Int -> String
formatCallHistory actuals nearestIdx =
  let count = length actuals
      limit = 5
      (shown, hidden) = splitAt limit actuals
      
      formatItem idx s =
        let num = idx + 1
            prefix = if idx == nearestIdx then "    [Closest] " else "              "
            -- number alignment: "1. " vs "10. "
            numStr = show num <> "."
            paddedNum = if num < 10 then numStr <> " " else numStr
         in prefix <> paddedNum <> s

      items = zipWith formatItem [0..] shown
      
      moreMessage = 
        if null hidden 
          then [] 
          else ["              ... (" <> show (length hidden) <> " more calls)"]

   in intercalate "\n"
        ( [ "  Call history (" <> show count <> " calls):" ]
          ++ items
          ++ moreMessage
        )


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
isList s = case s of
  '[':cs -> case reverse cs of
              ']':_ -> True
              _ -> False
  _ -> False

extractListItems :: String -> [String]
extractListItems s = maybe [] splitByComma (extractInner '[' ']' s)

listMismatchIndex :: [String] -> [String] -> Maybe Int
listMismatchIndex s1 s2 = elemIndex False (zipWith (==) s1 s2)

verifyOrderFailedMesssage :: Show a => VerifyOrderResult a -> String
verifyOrderFailedMesssage VerifyOrderResult {index, calledValue, expectedValue} =
  let callIndex = showHumanReadable (index + 1)
      expectedStr = formatStr (show expectedValue)
      actualStr = formatStr (show calledValue)
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