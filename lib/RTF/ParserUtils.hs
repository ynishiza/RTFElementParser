module RTF.ParserUtils (
  errorLabelString,
  errorLabelText,
  errorToken,
  skipNewLines,
  trimNewLines,
  charBlockEnd,
  parseText,
  inClass,
  notInClass,
  decimal,
  (<??>),
  _stateInput,
  _stateOffset,
  _statePosState,
  _stateParseErrors,
  Void,
) where

import Control.Monad
import Data.List.NonEmpty qualified as N
import Data.Text qualified as T
import Data.Void (Void)
import RTF.Types
import RTF.Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

$(makeLensesWith dataLensRules ''State)
$(makeLensesWith dataLensRules ''SourcePos)
$(makeLensesWith dataLensRules ''PosState)

(<??>) :: (Ord e, Stream s) => Parsec e s a -> Text -> Parsec e s a
a <??> b = a <?> T.unpack b

errorLabelString :: String -> ErrorItem t
errorLabelString "" = error "errorLabelString: error label must not be empty"
errorLabelString s = Label $ N.fromList s

errorLabelText :: T.Text -> ErrorItem t
errorLabelText = errorLabelString . T.unpack

errorToken :: t -> ErrorItem t
errorToken = Tokens . N.singleton

-- New lines are ignored in RTF
-- A new line plain text uses a RTF symbol instead
--
-- e.g.
--        \n        ignored
--        \\n       symbol \n
--
-- See README for the RTF specs.
skipNewLines :: (Token s ~ Char, Ord e, Stream s) => Parsec e s ()
skipNewLines = void $ takeWhileP (Just "newline") (inClass charNewline)

trimNewLines :: (Token s ~ Char, Ord e, Stream s) => Parsec e s a -> Parsec e s a
trimNewLines p = skipNewLines *> p <* skipNewLines

charBlockEnd :: Text
charBlockEnd = ";"

parseText :: (Tokens s ~ Text, Ord e, Stream s) => Text -> Parsec e s Text
parseText = string

inClass :: String -> Char -> Bool
inClass = flip elem

decimal :: (Ord e, Num a) => Parsec e Text a
decimal = L.signed (return ()) L.decimal

notInClass :: String -> Char -> Bool
notInClass x = not . inClass x
