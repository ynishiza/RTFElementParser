{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use $>" #-}
{-# HLINT ignore "Eta reduce" #-}

{- |
  See README for RTF specs
-}
module RTF.Parse (
  charBlockEnd,
  ByteString,
  parseRTFControlWord,
  parseRTFControlWordBase,
  parseRTFGroupWith,
  parseRTFElement,
  parseRTFElements,
  parseText,
  (<??>),
  module X,
  Parser,
) where

import Control.Applicative hiding (many, some)
import Control.Monad
import Data.ByteString (ByteString)
import Data.Functor
import Data.Set qualified as S
import Data.Text qualified as T
import RTF.ParserUtils
import RTF.Types as X
import RTF.Utils as X
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (takeWhile)

type Parser = Parsec Void Text

parseRTFElements :: Parser [RTFElement]
parseRTFElements =
  trimNewLines
    $
    -- Note: newline is ignored in RTF
    filter notNewLine
    <$> many (trimNewLines parseRTFElement)

notNewLine :: RTFElement -> Bool
notNewLine (RTFText "\n") = False
notNewLine _ = True

parseRTFElement :: Parser RTFElement
parseRTFElement =
  group
    <|> parseRTFControlWord wordName
    <|> symbol
    <|> text
    <?> "RTFElement"
 where
  symbol =
    ( do
        -- Note: Backtrack
        c <- parseControl $ satisfy (notInClass charExtendedControlName)
        o <- getOffset
        case rtfControlSymbol c of
          Right v -> return v
          Left e -> do
            let err = FancyError (o - 1) $ S.singleton $ ErrorFail e
            parseError err
    )
      <?> "RTFControlSymbol"
  wordName =
    takeWhile1P (Just "name character") (inClass charExtendedControlName)
      <?> "RTFControlWord"
  group =
    RTFGroup
      <$> parseRTFGroupWith parseRTFElements
      <?> "RTFGroup"
  text =
    RTFText
      <$> isNonEmpty (takeWhile1P (Just "text content") (not . (`elem` charReserved)))
      <??> "RTFText"
  isNonEmpty :: Parser Text -> Parser Text
  isNonEmpty p = do
    d <- p
    guard $ T.length d > 0
    return d

parseRTFGroupWith :: Parser a -> Parser a
parseRTFGroupWith p =
  between
    (char '{')
    (char '}')
    $ trimNewLines p

parseRTFControlWord :: Parser Text -> Parser RTFElement
parseRTFControlWord name = trimNewLines $ parseRTFControlWordBase name

parseRTFControlWordBase :: Parser Text -> Parser RTFElement
parseRTFControlWordBase name =
  ( do
      prefixPart <- optional prefix
      case prefixPart of
        -- case: with prefix
        -- A prefix must be followed by a name so no backtrack.
        Just x -> RTFControlWord x <$> wordName
        -- case: without prefix
        -- If the parser fails,
        Nothing -> RTFControlWord NoPrefix <$> wordName
  )
    <*> ( trailingSpace
            <|> (RTFControlParam <$> decimal <?> "RTFControlParam")
            <|> return NoSuffix
        )
 where
  -- )

  prefix =
    (parseControl (char '*') *> return StarPrefix)
      <?> "RTFControlPrefix"
  wordName = parseControl name <?> "RTFControlWord name"
  trailingSpace =
    char ' '
      >> return SpaceSuffix
      <?> "SpaceSuffix"

-- Note: need to backtrack (i.e. try) on failure since
-- there are multiple control types
-- i.e. multiple terms beginning with '\'
--
--    \{          control symbol
--    \froman     control word
--    \'8e        escape sequence
--
parseControl :: Parser a -> Parser a
parseControl p = try $ char charControl *> p
