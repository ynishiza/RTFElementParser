{-# LANGUAGE QuasiQuotes #-}

module Spec (
  spec,
) where

import Control.Monad.Combinators
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Void
import RTF.ElementParser
import RTF.Parse (Parser, parseRTFElements)
import RTF.Types
import RTF.Utils
import Test.Hspec hiding (runIO)
import Text.Megaparsec

data ContentParseResult a
  = FileParseError (ParseErrorBundle Text Void)
  | ContentParseError (ParseErrorBundle [RTFElement] RTFParseError)
  | Success a
  deriving (Eq, Show)

parseDoc_ :: ElementParser c -> Text -> ContentParseResult c
parseDoc_ p d = case runParser parseRTFElements "RTFElement" d of
  Left e -> FileParseError e
  Right contents -> case runParser p "" contents of
    Left e -> ContentParseError e
    Right value -> Success value

spec :: Spec
spec = describe "RTF Parsers" $ do
  let testError :: (HasCallStack) => ElementParser c -> Text -> Text -> Expectation
      testError p b expected = case parseDoc_ p b of
        ContentParseError e -> do
          T.strip (T.pack $ errorBundlePretty e) `shouldBe` T.strip expected
        FileParseError e -> do
          T.strip (T.pack $ errorBundlePretty e) `shouldBe` T.strip expected
        _ -> expectationFailure $ "Failed to test: " <> T.unpack expected

  specConvert
  describe "Notes.RTF.ElementParser" $ do
    it "[error message] multiple errors" $ do
      let withError p = do
            o <- getOffset
            registerParseError $ FancyError o $ S.singleton $ ErrorFail "FAIL"
            p

      testError
        (count 3 (withError (rtfControlWordLabel_ "abc")) >> withError (rtfText_ "hello"))
        "\\abc \\abc \\abc hello"
        [multiline|
RTF:1:1:
  |
1 | RTFControlWord NoPrefix "abc" SpaceSuffix,RTFControlWord NoPrefix "abc" SpaceSuffix,RTFControlWord NoPrefix "abc" SpaceSuffix,RTFText "hello"
  | ^
FAIL

RTF:1:43:
  |
1 | RTFControlWord NoPrefix "abc" SpaceSuffix,RTFControlWord NoPrefix "abc" SpaceSuffix,RTFControlWord NoPrefix "abc" SpaceSuffix,RTFText "hello"
  |                                           ^
FAIL

RTF:1:85:
  |
1 | RTFControlWord NoPrefix "abc" SpaceSuffix,RTFControlWord NoPrefix "abc" SpaceSuffix,RTFControlWord NoPrefix "abc" SpaceSuffix,RTFText "hello"
  |                                                                                     ^
FAIL

RTF:1:127:
  |
1 | RTFControlWord NoPrefix "abc" SpaceSuffix,RTFControlWord NoPrefix "abc" SpaceSuffix,RTFControlWord NoPrefix "abc" SpaceSuffix,RTFText "hello"
  |                                                                                                                               ^
FAIL
|]

    describe "RTFControlWord" $ do
      let pNoParam = rtfControlWordLabel_ "a"
      let pParam = rtfControlWordValue_ "a" (\x -> if x == 1 then Just x else Nothing)

      it "[error message] wrong word parsing a word with no parameter" $ do
        testError
          pNoParam
          "\\b abc"
          [multiline|
RTF:1:1:
  |
1 | RTFControlWord NoPrefix "b" SpaceSuffix,RTFText "abc"
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "b" SpaceSuffix
expecting RTFControlWord a
|]

        testError
          pNoParam
          "\\b1"
          [multiline|
RTF:1:1:
  |
1 | RTFControlWord NoPrefix "b" (RTFControlParam 1)
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "b" (RTFControlParam 1)
expecting RTFControlWord b
|]

      it "[error message] wrong word parsing a word with parameter" $ do
        testError
          pParam
          "\\a abc"
          [multiline|
RTF:1:1:
  |
1 | RTFControlWord NoPrefix "a" SpaceSuffix,RTFText "abc"
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "a" SpaceSuffix
expecting RTFControlWord * a (RTFControlParam *)
|]

        testError
          pParam
          "\\b1 abc"
          [multiline|
RTF:1:1:
  |
1 | RTFControlWord NoPrefix "b" (RTFControlParam 1),RTFText " abc"
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "b" (RTFControlParam 1)
expecting RTFControlSymbol a|]

      it "[error message] not a word" $ do
        testError
          pNoParam
          "\\& abc"
          [multiline|
RTF:1:1:
  |
1 | RTFControlSymbol '&',RTFText " abc"
  | ^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlSymbol '&'
expecting RTFControlWord
|]

      it "[error message] error location in a sequence" $ do
        testError
          (count 3 pNoParam)
          "\\a\\a\\b\\c"
          [multiline|
RTF:1:75:
  |
1 | RTFControlWord NoPrefix "a" NoSuffix,RTFControlWord NoPrefix "a" NoSuffix,RTFControlWord NoPrefix "b" NoSuffix,RTFControlWord NoPrefix "c" NoSuffix
  |                                                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "b" NoSuffix
expecting RTFControlWord a
|]

        testError
          (count 3 pParam)
          "\\a1\\a1\\b\\c"
          [multiline|
RTF:1:97:
  |
1 | RTFControlWord NoPrefix "a" (RTFControlParam 1),RTFControlWord NoPrefix "a" (RTFControlParam 1),RTFControlWord NoPrefix "b" NoSuffix,RTFControlWord NoPrefix "c" NoSuffix
  |                                                                                                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "b" NoSuffix
expecting RTFControlWord * b (RTFControlParam *)
|]

    describe "RTFControlSymbol" $ do
      let pHash = rtfSymbol_ '#'

      it "[error message] non-symbol character" $ do
        -- case: wrong symbol
        testError
          pHash
          "\\0 abc"
          [multiline|
RTFElement:1:2:
  |
1 | \0 abc
  |  ^
Invalid symbol '0'
|]
        testError
          (rtfText_ "abc" *> pHash)
          "abc\\0 abc"
          [multiline|
RTFElement:1:5:
  |
1 | abc\0 abc
  |     ^
Invalid symbol '0'
|]

      it "[error message] wrong symbol" $ do
        -- case: wrong symbol
        testError
          pHash
          "\\@ abc"
          [multiline|
RTF:1:1:
  |
1 | RTFControlSymbol '@',RTFText " abc"
  | ^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlSymbol '@'
expecting RTFControlSymbol '#'
|]

      it "[error message] not a symbol" $ do
        -- case: wrong type
        testError
          pHash
          "\\a abc"
          [multiline|
RTF:1:1:
  |
1 | RTFControlWord NoPrefix "a" SpaceSuffix,RTFText "abc"
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "a" SpaceSuffix
expecting RTFControlSymbol
|]

      it "[error message] location in a sequence" $ do
        testError
          (count 10 pHash)
          "\\#\\#\\@ abc"
          [multiline|
RTF:1:43:
  |
1 | RTFControlSymbol '#',RTFControlSymbol '#',RTFControlSymbol '@',RTFText " abc"
  |                                           ^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlSymbol '@'
expecting RTFControlSymbol '#'
|]

    describe "RTFGroup" $ do
      let pGroup = rtfGroup "Group" (rtfSymbol_ '&' >> rtfControlWordLabel_ "abc" >> rtfText_ "hello")

      it "[error message] wrong group content" $ do
        testError
          pGroup
          "{\\#}"
          [multiline|
RTF:1:1:
  |
1 | RTFGroup [RTFControlSymbol '#']
  | ^

RTFGroup [RTFControlSymbol '#']:1:1:
  |
1 | RTFControlSymbol '#'
  | ^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlSymbol '#'
expecting RTFControlSymbol '&'
|]

        testError
          pGroup
          "{\\& a}"
          [multiline|
RTF:1:1:
  |
1 | RTFGroup [RTFControlSymbol '&',RTFText " a"]
  | ^

RTFGroup [RTFControlSymbol '&',RTFText " a"]:1:22:
  |
1 | RTFControlSymbol '&',RTFText " a"
  |                      ^^^^^^^^^^^^
unexpected RTFText " a"
expecting RTFControlWord
|]

        testError
          pGroup
          "{\\&\\abc abc}"
          [multiline|
RTF:1:1:
  |
1 | RTFGroup [RTFControlSymbol '&',RTFControlWord NoPrefix "abc" SpaceSuffix,RTFText "abc"]
  | ^

RTFGroup [RTFControlSymbol '&',RTFControlWord NoPrefix "abc" SpaceSuffix,RTFText "abc"]:1:64:
  |
1 | RTFControlSymbol '&',RTFControlWord NoPrefix "abc" SpaceSuffix,RTFText "abc"
  |                                                                ^^^^^^^^^^^^^
unexpected RTFText "abc"
expecting RTFText "hello"
|]

        testError
          pGroup
          "{\\&\\abc hello\\#}"
          [multiline|
RTF:1:1:
  |
1 | RTFGroup [RTFControlSymbol '&',RTFControlWord NoPrefix "abc" SpaceSuffix,RTFText "hello",RTFControlSymbol '#']
  | ^

RTFGroup [RTFControlSymbol '&',RTFControlWord NoPrefix "abc" SpaceSuffix,RTFText "hello",RTFControlSymbol '#']:1:80:
  |
1 | RTFControlSymbol '&',RTFControlWord NoPrefix "abc" SpaceSuffix,RTFText "hello",RTFControlSymbol '#'
  |                                                                                ^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlSymbol '#'
expecting end of input
|]
        testError
          (pGroup >> rtfText_ "def")
          "{\\&\\abc hello}abc"
          [multiline|
RTF:1:91:
  |
1 | RTFGroup [RTFControlSymbol '&',RTFControlWord NoPrefix "abc" SpaceSuffix,RTFText "hello"],RTFText "abc"
  |                                                                                           ^^^^^^^^^^^^^
unexpected RTFText "abc"
expecting RTFText "def"
|]

      it "[error message] not a group" $ do
        testError
          pGroup
          "\\a"
          [multiline|
RTF:1:1:
  |
1 | RTFControlWord NoPrefix "a" NoSuffix
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "a" NoSuffix
expecting RTFGroup
|]

      it "[error message] location in a sequence" $ do
        testError
          (count 3 pGroup)
          "{\\&\\abc hello}{\\&\\abc hello}{\\&\\d} abc"
          [multiline|
RTF:1:181:
  |
1 | RTFGroup [RTFControlSymbol '&',RTFControlWord NoPrefix "abc" SpaceSuffix,RTFText "hello"],RTFGroup [RTFControlSymbol '&',RTFControlWord NoPrefix "abc" SpaceSuffix,RTFText "hello"],RTFGroup [RTFControlSymbol '&',RTFControlWord NoPrefix "d" NoSuffix],RTFText " abc"
  |                                                                                                                                                                                     ^

RTFGroup [RTFControlSymbol '&',RTFControlWord NoPrefix "d" NoSuffix]:1:22:
  |
1 | RTFControlSymbol '&',RTFControlWord NoPrefix "d" NoSuffix
  |                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "d" NoSuffix
expecting RTFControlWord abc
|]

    describe "RTFText" $ do
      let pHello = rtfText_ "Hello"

      it "[error message] not a word" $ do
        testError
          (count 3 pHello)
          "\\abc"
          [multiline|
RTF:1:1:
  |
1 | RTFControlWord NoPrefix "abc" NoSuffix
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFControlWord NoPrefix "abc" NoSuffix
expecting RTFText
|]

      it "[error message] wrong text" $ do
        testError
          (count 3 pHello)
          "This is a pen"
          [multiline|
RTF:1:1:
  |
1 | RTFText "This is a pen"
  | ^^^^^^^^^^^^^^^^^^^^^^^
unexpected RTFText "This is a pen"
expecting RTFText "Hello"
|]

specConvert :: Spec
specConvert = describe "Notes.RTF.Convert" $ do
  let
    parser = parseRTFElements <* eof
    testError :: (HasCallStack) => Parser a -> Text -> Text -> Expectation
    testError p text expected = case runParser p "" text of
      Left e -> T.strip (T.pack $ errorBundlePretty e) `shouldBe` T.strip expected
      Right _ -> expectationFailure "Expected failure"

  describe "[parseRTFElements]" $ do
    it "[error message] word with prefix" $ do
      -- case: no name
      testError
        parser
        "\\* abc"
        [multiline|
1:3:
  |
1 | \* abc
  |   ^
unexpected space
expecting RTFControlWord name
  |]

      -- case: bad name
      testError
        parser
        "\\*\\1 \\abc"
        [multiline|
1:4:
  |
1 | \*\1 \abc
  |    ^
unexpected '1'
expecting RTFControlWord name
  |]

    it "[error message] invalid group" $ do
      -- case: symbol at beginning
      testError
        parser
        "{ abc"
        [multiline|
1:6:
  |
1 | { abc
  |      ^
unexpected end of input
expecting '}', RTFElement, newline, or text content
 |]

      testError
        parser
        " abc}"
        [multiline|
1:5:
  |
1 |  abc}
  |     ^
unexpected '}'
expecting RTFElement, end of input, newline, or text content
 |]

    it "[error message] invalid symbol" $ do
      -- case: symbol at beginning
      testError
        parser
        "\\9"
        [multiline|
1:2:
  |
1 | \9
  |  ^
Invalid symbol '9'
 |]

      -- case: symbol in middle
      testError
        parser
        "abc\\9def"
        [multiline|
1:5:
  |
1 | abc\9def
  |     ^
Invalid symbol '9'
|]
