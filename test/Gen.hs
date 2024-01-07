module Gen (
  rtfWordName,
  rtfWord,
  rtfSymbol,
  rtfGroup,
) where

import Control.Monad
import Hedgehog
import Hedgehog.Gen
import Hedgehog.Range as Range

rtfWord :: Gen String
rtfWord = (<>) <$> prefix <*> ((<>) <$> wordBase <*> suffix)
 where
  wordBase = ("\\" <>) <$> rtfWordName
  prefix :: Gen String
  prefix =
    frequency
      [ (3, return "")
      , (1, return ("\\*"))
      ]
  suffix :: Gen String
  suffix =
    frequency
      [ (3, return "")
      , (1, return " ")
      , (1, show <$> int16 (Range.linear 0 maxBound))
      ]

rtfWordName :: Gen String
rtfWordName = list (linearFrom 1 1 100) (element nameChars)

rtfSymbol :: Gen String
rtfSymbol =
  (\c -> ['\\', c])
    <$> frequency
      [ (3, element symbol :: Gen Char)
      , (1, element commonSymbols)
      ]
 where
  symbol = Prelude.filter (not . (`elem` nonSymboChars)) $ toEnum <$> [0 .. 127]
  commonSymbols = "{}\\\n\t"
  nonSymboChars = nameChars <> ['*'] <> commonSymbols <> ['0'..'9']

nameChars :: String
nameChars = ['a' .. 'z'] ++ ['A' .. 'Z'] 

rtfGroup :: Gen String
rtfGroup = recursive ((surround . join <$>) . sequence) (replicate 10 nonGroup) [rtfGroup]
 where
  nonGroup =
    frequency
      [ (1, rtfWordName)
      , (1, rtfSymbol)
      ]
  surround x = "{" <> x <> "}"
