{-# OPTIONS_GHC -Wno-orphans #-}

module RTF.ElementParser (
  -- * ElementParser
  ElementParser,
  RTFParseError (..),

  -- ** Parser utils
  rtfText,
  rtfText_,
  rtfSymbol,
  rtfSymbol_,
  rtfControlWord,
  rtfControlWordLabel,
  rtfControlWordLabel_,
  rtfControlWordValue,
  rtfControlWordValue_,
  rtfGroup,
) where

import Control.Lens
import Data.List (intercalate)
import Data.List.NonEmpty qualified as N
import Data.Set qualified as S
import Data.Text qualified as T
import Debug.Trace (trace)
import RTF.Parse
import RTF.ParserUtils
import Text.Megaparsec

data RTFParseError = RTFGroupError Int RTFElement (ParseErrorBundle [RTFElement] RTFParseError)
  deriving (Show, Eq)

instance Ord RTFParseError where
  RTFGroupError n _ _ <= RTFGroupError m _ _ = n <= m

type ElementParser = ParsecT RTFParseError [RTFElement] Identity

enableDebug :: Bool
-- enableDebug = True
enableDebug = False

trace_ :: String -> a -> a
trace_ = if enableDebug then trace else (\_ x -> x)

deriving instance Ord RTFControlSuffix
deriving instance Ord RTFControlPrefix
deriving instance Ord RTFElement

instance VisualStream [RTFElement] where
  showTokens _ = showTokens_ . N.toList
  tokensLength p s = length $ showTokens p s

instance ShowErrorComponent RTFParseError where
  showErrorComponent (RTFGroupError _ g e) =
    errorBundlePretty e
      & dropWhile (/= ':')
      & (header <>)
   where
    header =
      "\n"
        <> show g

showToken :: RTFElement -> String
showToken = show

showTokens_ :: [RTFElement] -> String
showTokens_ = intercalate "," . (showToken <$>)

joinTokens :: [String] -> String
joinTokens = intercalate "," . filter (not . null)

instance TraversableStream [RTFElement] where
  reachOffset newOffset p@PosState{..} =
    trace_
      debugInfo
      ( Just (lineText <> lineTrailing)
      , PosState
          { pstateInput = unconsumed
          , pstateOffset = newOffset
          , pstateSourcePos = newSourcePos
          , pstateTabWidth = mkPos 1
          , pstateLinePrefix = linePrefix
          }
      )
   where
    SourcePos{..} = pstateSourcePos
    idx = newOffset - pstateOffset
    (consumed, unconsumed) = splitAt idx pstateInput
    linePrefix = joinTokens [pstateLinePrefix, showTokens_ consumed]
    lineText = joinTokens [linePrefix, showTokens_ $ take 10 unconsumed]
    lineTrailing = if length unconsumed > 10 then "..." else ""
    newSourcePos =
      SourcePos
        { sourceName = "RTF"
        , sourceLine = sourceLine
        , sourceColumn = mkPos $ if null linePrefix then 1 else length linePrefix + 2
        }
    debugInfo =
      "\ninput (offset, PosState):\t"
        <> show (newOffset, p)
        <> "\n(consumed, line):\t\t"
        <> show (consumed, lineText)
        <> "\nnew PosState:\t\t"
        <> show newSourcePos

rtfSymbol_ :: Char -> ElementParser Char
rtfSymbol_ symbol = rtfSymbol (\x -> if x == symbol then Right symbol else Left (errorToken (RTFControlSymbol symbol)))

rtfSymbol :: (Char -> Either (ErrorItem RTFElement) c) -> ElementParser c
rtfSymbol f = try $ do
  x@(RTFControlSymbol c) <- satisfyWith "RTFControlSymbol" (has _RTFControlSymbol)
  valueOrError x (f c)

rtfControlWordValue_ :: Text -> (Int -> c) -> ElementParser c
rtfControlWordValue_ name f = rtfControlWordValue (\n v -> if name == n then Right (f v) else Left (errorLabelText ("RTFControlSymbol " <> name)))

rtfControlWordValue :: (Text -> Int -> Either (ErrorItem RTFElement) c) -> ElementParser c
rtfControlWordValue f = rtfControlWord f'
 where
  f' _ name (RTFControlParam v) = f name v
  f' _ name _ = Left $ errorLabelText $ "RTFControlWord * " <> name <> " (RTFControlParam *)"

rtfControlWordLabel_ :: Text -> ElementParser Text
rtfControlWordLabel_ name = rtfControlWordLabel (\x -> if x == name then Right x else Left (errorLabelText $ "RTFControlWord " <> name))

rtfControlWordLabel :: (Text -> Either (ErrorItem RTFElement) c) -> ElementParser c
rtfControlWordLabel f = rtfControlWord f'
 where
  f' _ name end
    | end == SpaceSuffix || end == NoSuffix = f name
    | otherwise = Left $ errorLabelText $ "RTFControlWord " <> name

rtfControlWord :: (RTFControlPrefix -> Text -> RTFControlSuffix -> Either (ErrorItem RTFElement) c) -> ElementParser c
rtfControlWord f = try $ do
  x@(RTFControlWord prefix n suffix) <- satisfyWith "RTFControlWord" (has _RTFControlWord)
  valueOrError x (f prefix n suffix)

rtfText_ :: Text -> ElementParser Text
rtfText_ text = rtfText (\x -> if x == text then Right text else Left (errorToken $ RTFText text))

rtfText :: (Text -> Either (ErrorItem RTFElement) Text) -> ElementParser Text
rtfText f = try $ do
  x@(RTFText t) <- satisfyWith "RTFText" (has _RTFText)
  valueOrError x (f t)

rtfGroup :: Text -> ElementParser c -> ElementParser c
rtfGroup msg p = try $ do
  (RTFGroup g) <- satisfyWith "RTFGroup" (has _RTFGroup)
  offset <- getOffset
  case runParser (p <* eof) (T.unpack msg) g of
    Left e -> parseError $ FancyError (offset - 1) $ S.singleton $ ErrorCustom $ RTFGroupError offset (RTFGroup g) e
    Right v -> return v

valueOrError :: RTFElement -> Either (ErrorItem RTFElement) c -> ElementParser c
valueOrError _ (Right v) = return v
valueOrError unexpectedTerm (Left e) = do
  o <- getOffset
  parseError $ TrivialError (o - 1) (Just $ errorToken unexpectedTerm) (S.singleton e)

satisfyWith :: Text -> (RTFElement -> Bool) -> ElementParser RTFElement
satisfyWith expected = region mapError . satisfy
 where
  mapError (TrivialError offset unexpectedValue _) = TrivialError offset unexpectedValue $ S.singleton $ errorLabelText expected
  mapError e = e
