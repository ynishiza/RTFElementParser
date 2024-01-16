module TestUtils (
  runBaseParser,
  runElementParser,
  -- Lens
  _RTFControlWord,
  _RTFControlSymbol,
  _RTFGroup,
  _RTFText,
  _SpaceSuffix,
  _RTFControlParam,
  _NoSuffix,
  multiline,
) where

import Control.Lens
import GHC.Exts (fromString)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
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

$(makePrisms ''RTFElement)
$(makePrisms ''RTFControlSuffix)

multiline :: QuasiQuoter
multiline =
  QuasiQuoter
    { quoteExp = \s -> [|fromString s|]
    , quoteDec = undefined
    , quoteType = undefined
    , quotePat = undefined
    }
