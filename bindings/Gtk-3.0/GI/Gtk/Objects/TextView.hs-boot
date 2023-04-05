#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.TextView where

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

newtype TextView = TextView (SP.ManagedPtr TextView)
instance SP.ManagedPtrNewtype TextView where
instance B.Types.TypedObject TextView where
instance B.Types.GObject TextView
class (SP.GObject o, O.IsDescendantOf TextView o) => IsTextView o
instance (SP.GObject o, O.IsDescendantOf TextView o) => IsTextView o
instance O.HasParentTypes TextView
toTextView :: (MIO.MonadIO m, IsTextView o) => o -> m TextView
instance B.GValue.IsGValue (Maybe TextView) where
#if defined(ENABLE_OVERLOADING)
data TextViewBackspaceSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewCopyClipboardSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewCutClipboardSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewDeleteFromCursorSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewExtendSelectionSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewInsertAtCursorSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewInsertEmojiSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewMoveCursorSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewMoveViewportSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewPasteClipboardSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewPopulatePopupSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewPreeditChangedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewSelectAllSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewSetAnchorSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewToggleCursorVisibleSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewToggleOverwriteSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewAcceptsTabPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewBottomMarginPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewBufferPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewCursorVisiblePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewEditablePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewImModulePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewIndentPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewInputHintsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewInputPurposePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewJustificationPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewLeftMarginPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewMonospacePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewOverwritePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewPixelsAboveLinesPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewPixelsBelowLinesPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewPixelsInsideWrapPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewPopulateAllPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewRightMarginPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewTabsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewTopMarginPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewWrapModePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewAddChildAtAnchorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewAddChildInWindowMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewBackwardDisplayLineMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewBackwardDisplayLineStartMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewBufferToWindowCoordsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewForwardDisplayLineMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewForwardDisplayLineEndMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewGetAcceptsTabMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewGetBorderWindowSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewGetBottomMarginMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewGetBufferMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewGetCursorLocationsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewGetCursorVisibleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewGetDefaultAttributesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewGetEditableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewGetHadjustmentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewGetIndentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewGetInputHintsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewGetInputPurposeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewGetIterAtLocationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewGetIterAtPositionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewGetIterLocationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewGetJustificationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewGetLeftMarginMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewGetLineAtYMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewGetLineYrangeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewGetMonospaceMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewGetOverwriteMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewGetPixelsAboveLinesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewGetPixelsBelowLinesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewGetPixelsInsideWrapMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewGetRightMarginMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewGetTabsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewGetTopMarginMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewGetVadjustmentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewGetVisibleRectMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewGetWindowMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewGetWindowTypeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewGetWrapModeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewImContextFilterKeypressMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewMoveChildMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewMoveMarkOnscreenMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewMoveVisuallyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewPlaceCursorOnscreenMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewResetCursorBlinkMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewResetImContextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewScrollMarkOnscreenMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewScrollToIterMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewScrollToMarkMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewSetAcceptsTabMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewSetBorderWindowSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewSetBottomMarginMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewSetBufferMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewSetCursorVisibleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewSetEditableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewSetIndentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewSetInputHintsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewSetInputPurposeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewSetJustificationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewSetLeftMarginMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewSetMonospaceMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewSetOverwriteMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewSetPixelsAboveLinesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewSetPixelsBelowLinesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewSetPixelsInsideWrapMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewSetRightMarginMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewSetTabsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewSetTopMarginMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewSetWrapModeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewStartsDisplayLineMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TextViewWindowToBufferCoordsMethodInfo
#endif
