module RTF.Render (
  renderRTFElement,
  renderRTFGroup,
  module X,
) where

import Data.Text qualified as T
import RTF.Types as X
import TextShow

controlWith :: Text -> Text
controlWith = (T.pack [charControl] <>)

renderRTFElement :: RTFElement -> Text
renderRTFElement (RTFControlSymbol symbol) = T.pack [charControl, symbol]
renderRTFElement (RTFControlWord prefix name suffix) = renderPrefix prefix <> controlWith name <> renderSuffix suffix
renderRTFElement (RTFGroup content) = renderRTFGroup $ T.intercalate "" $ renderRTFElement <$> content
renderRTFElement (RTFText text) = text

renderPrefix :: RTFControlPrefix -> Text
renderPrefix NoPrefix = ""
renderPrefix StarPrefix = controlWith "*"

renderSuffix :: RTFControlSuffix -> Text
renderSuffix NoSuffix = ""
renderSuffix SpaceSuffix = " "
renderSuffix (RTFControlParam n) = showt n

renderRTFGroup :: Text -> Text
renderRTFGroup t = "{" <> t <> "}"
