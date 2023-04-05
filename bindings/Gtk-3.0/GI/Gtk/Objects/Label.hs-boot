#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.Label where

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

newtype Label = Label (SP.ManagedPtr Label)
instance SP.ManagedPtrNewtype Label where
instance B.Types.TypedObject Label where
instance B.Types.GObject Label
class (SP.GObject o, O.IsDescendantOf Label o) => IsLabel o
instance (SP.GObject o, O.IsDescendantOf Label o) => IsLabel o
instance O.HasParentTypes Label
toLabel :: (MIO.MonadIO m, IsLabel o) => o -> m Label
instance B.GValue.IsGValue (Maybe Label) where
#if defined(ENABLE_OVERLOADING)
data LabelActivateCurrentLinkSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelActivateLinkSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelCopyClipboardSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelMoveCursorSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelPopulatePopupSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelAnglePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelAttributesPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelCursorPositionPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelEllipsizePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelJustifyPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelLabelPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelLinesPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelMaxWidthCharsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelMnemonicKeyvalPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelMnemonicWidgetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelPatternPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelSelectablePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelSelectionBoundPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelSingleLineModePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelTrackVisitedLinksPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelUseMarkupPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelUseUnderlinePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelWidthCharsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelWrapPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelWrapModePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelXalignPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelYalignPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelGetAngleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelGetAttributesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelGetCurrentUriMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelGetEllipsizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelGetJustifyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelGetLabelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelGetLayoutMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelGetLayoutOffsetsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelGetLineWrapMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelGetLineWrapModeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelGetLinesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelGetMaxWidthCharsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelGetMnemonicKeyvalMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelGetMnemonicWidgetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelGetSelectableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelGetSelectionBoundsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelGetSingleLineModeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelGetTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelGetTrackVisitedLinksMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelGetUseMarkupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelGetUseUnderlineMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelGetWidthCharsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelGetXalignMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelGetYalignMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelSelectRegionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelSetAngleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelSetAttributesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelSetEllipsizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelSetJustifyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelSetLabelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelSetLineWrapMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelSetLineWrapModeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelSetLinesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelSetMarkupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelSetMarkupWithMnemonicMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelSetMaxWidthCharsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelSetMnemonicWidgetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelSetPatternMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelSetSelectableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelSetSingleLineModeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelSetTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelSetTextWithMnemonicMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelSetTrackVisitedLinksMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelSetUseMarkupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelSetUseUnderlineMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelSetWidthCharsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelSetXalignMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LabelSetYalignMethodInfo
#endif
