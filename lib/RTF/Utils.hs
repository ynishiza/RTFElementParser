{-# LANGUAGE UndecidableInstances #-}

module RTF.Utils (
  module X,
  Text,
  TextShow (..),
  dataLensRules,
  makeLensesWith,
  makePrisms,
  multiline,
) where

import Control.Lens hiding (from, to)
import Control.Monad
import Data.Function as X
import Data.Functor as X
import Data.Text (Text)
import GHC.Exts (fromString)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import TextShow (TextShow (..))

lensNamer :: FieldNamer
lensNamer = mappingNamer $ \s -> ['_' : s]

dataLensRules :: LensRules
dataLensRules =
  lensRules
    & set lensField lensNamer

multiline :: QuasiQuoter
multiline =
  QuasiQuoter
    { quoteExp = \s -> [|fromString s|]
    , quoteDec = undefined
    , quoteType = undefined
    , quotePat = undefined
    }
