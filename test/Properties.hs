module Properties (
  groups,
) where

import Control.Lens
import Data.Char
import Data.Foldable
import Data.Maybe
import Data.Text qualified as T
import GHC.Exts (IsString (..))
import Gen
import Hedgehog
import RTF.Parse
import RTF.Render
import TestUtils
import Text.Megaparsec hiding (label)

groups :: Group
groups = $$discover

prop_rtfSymbol :: Property
prop_rtfSymbol = property $ do
  symbolElem <- testParse =<< forAll rtfSymbol
  let c = fromJust $ preview _RTFControlSymbol symbolElem
  cover 1 "{" (c == '{')
  cover 1 "}" (c == '}')
  cover 1 "\\n" (c == '\n')
  cover 1 "\\t" (c == '\t')
  cover 1 "\\" (c == '\\')
  cover 1 "near upper boundary" (c `elem` ("|~\127" :: String))
  cover 1 "near lower boundary" (c `elem` ("\0\1\2\3\4\5\6" :: String))

prop_rtfControlWord :: Property
prop_rtfControlWord = property $ do
  wordElem <- testParse =<< forAll rtfWord
  let param = preview (_RTFControlWord . _3 . _RTFControlParam) wordElem
      name = fromJust $ preview (_RTFControlWord . _2) wordElem

  testEquality wordElem
  cover 10 "name: long" (T.length name > 10)
  cover 1 "name: short" (T.length name < 3)
  cover 10 "name: upper case start" (isUpper $ T.head name)
  cover 10 "prefix: star" (isWordWithSpaceSuffix wordElem)
  cover 10 "suffix: none" (isWordWithNoSuffix wordElem)
  cover 10 "suffix: space" (isWordWithSpaceSuffix wordElem)
  cover 1 "suffix: large param" (maybe False (> 10000) param)
  cover (0.1) "suffix: small param" (maybe False (< 1000) param)

prop_rtfGroup :: Property
prop_rtfGroup = property $ do
  groupElem <- testParse =<< forAll rtfGroup
  let groupElements (RTFGroup v) = v
      groupElements _ = []
      groupDepth :: RTFElement -> Int
      groupDepth (RTFGroup v) = sum (groupDepth <$> v) + 1
      groupDepth _ = 0

  label (labelName $ "group top level size:" <> show (length $ groupElements groupElem))
  cover 1 "group top level size > 5" (length (groupElements groupElem) > 5)
  cover 1 "group depth > 3" (groupDepth groupElem > 3)

testParse :: (MonadTest m) => String -> m RTFElement
testParse s = case runBaseParser (parseRTFElement <* eof) (T.pack s) of
  Right v -> return v
  Left _ -> Hedgehog.failure

labelName :: (Show a) => a -> LabelName
labelName = fromString . show

coverEnum :: (MonadTest f, Show a, Eq a, Bounded a, Enum a) => a -> f ()
coverEnum v = traverse_ (\x -> cover minCover (labelName v) (x == v)) values
 where
  minCover = realToFrac $ 100 `div` (length values * 2)
  values = [minBound .. maxBound]

testEquality :: RTFElement -> PropertyT IO ()
testEquality x = do
  tripping x renderRTFElement (runBaseParser parseRTFElement)
