module TestUtils (
  runBaseParser,
  runElementParser,
) where

import RTF.ElementParser
import RTF.Parse
import Text.Megaparsec

runBaseParser :: Parser c -> Text -> Either String c
runBaseParser p d = case runParser p "RTFElement" d of
  Left e -> Left $ errorBundlePretty e
  Right value -> Right value

runElementParser :: ElementParser c -> Text -> Either String c
runElementParser p d = case runBaseParser parseRTFElements d of
  Left e -> Left e
  Right contents -> case runParser p "RTFElements" contents of
    Left e -> Left $ errorBundlePretty e
    Right value -> Right value
