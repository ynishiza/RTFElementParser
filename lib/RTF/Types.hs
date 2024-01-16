{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use record patterns" #-}

{- |
  See README for references
-}
module RTF.Types (
  rtfControlSymbol,
  getRtfControlSymbol,
  --
  RTFElement (..),
  Word8,
  Text,
  RTFControlPrefix (..),
  RTFControlSuffix (..),
  charControl,
  charExtendedControlName,
  charSymbol,
  charReserved,
  charNewline,
  charOptionalDestination,
  isWordWithSpaceSuffix,
  isWordWithNoSuffix,
  isWordWithStartPrefix,
  isRTFControlWord,
  isRTFText,
  isRTFControlSymbol,
  isRTFGroup,
) where

import Data.Word
import GHC.Generics
import RTF.Utils

charControl :: Char
charControl = '\\'

charReserved :: String
charReserved = "\\{}"

charOptionalDestination :: Char
charOptionalDestination = '*'

{- |
  Officially, RTF names are all lower case.
  However, Apple's extended RTF may include upper case.

  e.g. \NeXTGraphic
-}
charExtendedControlName :: String
charExtendedControlName = charControlName <> ['A' .. 'Z']

charControlName :: String
charControlName = ['a' .. 'z']

charNewline :: String
charNewline = ['\n', '\r', '\f']

charNonSymbol :: String
charNonSymbol = ['0' .. '9'] <> [charOptionalDestination] <> charExtendedControlName

-- 8 bits
charSymbol :: String
charSymbol = filter (not . (`elem` charNonSymbol)) $ toEnum <$> [0 .. 127]

data RTFControlPrefix = StarPrefix | NoPrefix
  deriving stock (Eq, Show, Generic)

data RTFControlSuffix = RTFControlParam Int | SpaceSuffix | NoSuffix
  deriving stock (Eq, Show, Generic)

isWordWithStartPrefix :: RTFElement -> Bool
isWordWithStartPrefix (RTFControlWord StarPrefix _ _) = True
isWordWithStartPrefix _ = False

isWordWithNoSuffix :: RTFElement -> Bool
isWordWithNoSuffix (RTFControlWord _ _ NoSuffix) = True
isWordWithNoSuffix _ = False

isWordWithSpaceSuffix :: RTFElement -> Bool
isWordWithSpaceSuffix (RTFControlWord _ _ SpaceSuffix) = True
isWordWithSpaceSuffix _ = False

getRtfControlSymbol :: RTFElement -> Maybe Char
getRtfControlSymbol (RTFControlSymbol s) = Just s
getRtfControlSymbol _ = Nothing

rtfControlSymbol :: Char -> Either String RTFElement
rtfControlSymbol c
  | c `elem` charSymbol = Right $ RTFControlSymbol c
  | otherwise = Left $ "Invalid symbol " <> ['\'', c, '\'']

{- |
  RTF specs in the README
-}
data RTFElement
  = -- |
    --
    -- Important symbols
    -- @
    --  \\\\        escaped slash
    --  \\\\n       literal new line. New lines in RTF are ignored otherwise.
    --  \\{         escaped brackets i.e. not a group
    --  \\}
    -- @
    RTFControlSymbol Char
  | -- |
    --
    -- e.g.
    -- @
    --  \\froman                 no prefix nor suffix
    --  \\red0                   param suffix
    --  \\*\\expandedcolors       star prefix
    -- @
    RTFControlWord RTFControlPrefix Text RTFControlSuffix
  | RTFGroup [RTFElement]
  | RTFText Text
  deriving stock (Eq, Show, Generic)

isRTFControlSymbol :: RTFElement -> Bool
isRTFControlSymbol (RTFControlSymbol _) = True
isRTFControlSymbol _ = False

isRTFControlWord :: RTFElement -> Bool
isRTFControlWord (RTFControlWord _ _ _) = True
isRTFControlWord _ = False

isRTFGroup :: RTFElement -> Bool
isRTFGroup (RTFGroup _) = True
isRTFGroup _ = False

isRTFText :: RTFElement -> Bool
isRTFText (RTFText _) = True
isRTFText _ = False
