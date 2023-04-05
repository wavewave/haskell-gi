#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.TextTag where

import Data.GI.Base.ShortPrelude
import qualified Data.GI.Base.ShortPrelude as SP
import qualified Data.GI.Base.Overloading as O
import qualified Prelude as P

import qualified Data.GI.Base.Attributes as GI.Attributes
import qualified Data.GI.Base.BasicTypes as B.Types
import qualified Data.GI.Base.ManagedPtr as B.ManagedPtr
import qualified Data.GI.Base.GArray as B.GArray
import qualified Data.GI.Base.GClosure as B.GClosure
import qualified Data.GI.Base.GError as B.GError
import qualified Data.GI.Base.GHashTable as B.GHT
import qualified Data.GI.Base.GVariant as B.GVariant
import qualified Data.GI.Base.GValue as B.GValue
import qualified Data.GI.Base.GParamSpec as B.GParamSpec
import qualified Data.GI.Base.CallStack as B.CallStack
import qualified Data.GI.Base.Properties as B.Properties
import qualified Data.GI.Base.Signals as B.Signals
import qualified Control.Monad.IO.Class as MIO
import qualified Data.Coerce as Coerce
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import qualified Foreign.Ptr as FP
import qualified GHC.OverloadedLabels as OL
import qualified GHC.Records as R

newtype TextTag = TextTag (SP.ManagedPtr TextTag)
instance SP.ManagedPtrNewtype TextTag where
instance B.Types.TypedObject TextTag where
instance B.Types.GObject TextTag
class (SP.GObject o, O.IsDescendantOf TextTag o) => IsTextTag o
instance (SP.GObject o, O.IsDescendantOf TextTag o) => IsTextTag o
instance O.HasParentTypes TextTag
toTextTag :: (MIO.MonadIO m, IsTextTag o) => o -> m TextTag
instance B.GValue.IsGValue (Maybe TextTag) where
#if defined(ENABLE_OVERLOADING)
data TextTagEventSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagAccumulativeMarginPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagBackgroundPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagBackgroundFullHeightPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagBackgroundFullHeightSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagBackgroundGdkPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagBackgroundRgbaPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagBackgroundSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagDirectionPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagEditablePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagEditableSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagFallbackPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagFallbackSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagFamilyPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagFamilySetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagFontPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagFontDescPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagFontFeaturesPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagFontFeaturesSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagForegroundPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagForegroundGdkPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagForegroundRgbaPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagForegroundSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagIndentPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagIndentSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagInvisiblePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagInvisibleSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagJustificationPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagJustificationSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagLanguagePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagLanguageSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagLeftMarginPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagLeftMarginSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagLetterSpacingPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagLetterSpacingSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagNamePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagParagraphBackgroundPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagParagraphBackgroundGdkPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagParagraphBackgroundRgbaPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagParagraphBackgroundSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagPixelsAboveLinesPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagPixelsAboveLinesSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagPixelsBelowLinesPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagPixelsBelowLinesSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagPixelsInsideWrapPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagPixelsInsideWrapSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagRightMarginPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagRightMarginSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagRisePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagRiseSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagScalePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagScaleSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagSizePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagSizePointsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagSizeSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagStretchPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagStretchSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagStrikethroughPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagStrikethroughRgbaPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagStrikethroughRgbaSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagStrikethroughSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagStylePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagStyleSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagTabsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagTabsSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagUnderlinePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagUnderlineRgbaPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagUnderlineRgbaSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagUnderlineSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagVariantPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagVariantSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagWeightPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagWeightSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagWrapModePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagWrapModeSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagChangedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagEventMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagGetPriorityMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextTagSetPriorityMethodInfo
#endif
