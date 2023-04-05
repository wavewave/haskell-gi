#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.CellRendererText where

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

newtype CellRendererText = CellRendererText (SP.ManagedPtr CellRendererText)
instance SP.ManagedPtrNewtype CellRendererText where
instance B.Types.TypedObject CellRendererText where
instance B.Types.GObject CellRendererText
class (SP.GObject o, O.IsDescendantOf CellRendererText o) => IsCellRendererText o
instance (SP.GObject o, O.IsDescendantOf CellRendererText o) => IsCellRendererText o
instance O.HasParentTypes CellRendererText
toCellRendererText :: (MIO.MonadIO m, IsCellRendererText o) => o -> m CellRendererText
instance B.GValue.IsGValue (Maybe CellRendererText) where
#if defined(ENABLE_OVERLOADING)
data CellRendererTextEditedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextAlignSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextAlignmentPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextAttributesPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextBackgroundPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextBackgroundGdkPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextBackgroundRgbaPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextBackgroundSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextEditablePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextEditableSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextEllipsizePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextEllipsizeSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextFamilyPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextFamilySetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextFontPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextFontDescPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextForegroundPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextForegroundGdkPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextForegroundRgbaPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextForegroundSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextLanguagePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextLanguageSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextMarkupPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextMaxWidthCharsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextPlaceholderTextPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextRisePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextRiseSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextScalePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextScaleSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextSingleParagraphModePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextSizePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextSizePointsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextSizeSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextStretchPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextStretchSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextStrikethroughPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextStrikethroughSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextStylePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextStyleSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextTextPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextUnderlinePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextUnderlineSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextVariantPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextVariantSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextWeightPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextWeightSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextWidthCharsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextWrapModePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextWrapWidthPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererTextSetFixedHeightFromFontMethodInfo
#endif
