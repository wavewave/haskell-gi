#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.Entry where

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

newtype Entry = Entry (SP.ManagedPtr Entry)
instance SP.ManagedPtrNewtype Entry where
instance B.Types.TypedObject Entry where
instance B.Types.GObject Entry
class (SP.GObject o, O.IsDescendantOf Entry o) => IsEntry o
instance (SP.GObject o, O.IsDescendantOf Entry o) => IsEntry o
instance O.HasParentTypes Entry
toEntry :: (MIO.MonadIO m, IsEntry o) => o -> m Entry
instance B.GValue.IsGValue (Maybe Entry) where
#if defined(ENABLE_OVERLOADING)
data EntryActivateSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryBackspaceSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCopyClipboardSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCutClipboardSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryDeleteFromCursorSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryIconPressSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryIconReleaseSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryInsertAtCursorSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryInsertEmojiSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryMoveCursorSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryPasteClipboardSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryPopulatePopupSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryPreeditChangedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryToggleOverwriteSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryActivatesDefaultPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryAttributesPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryBufferPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCapsLockWarningPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCursorPositionPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryEditablePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryEnableEmojiCompletionPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryHasFramePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryImModulePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryInnerBorderPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryInputHintsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryInputPurposePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryInvisibleCharPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryInvisibleCharSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryMaxLengthPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryMaxWidthCharsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryOverwriteModePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryPlaceholderTextPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryPopulateAllPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryPrimaryIconActivatablePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryPrimaryIconGiconPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryPrimaryIconNamePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryPrimaryIconPixbufPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryPrimaryIconSensitivePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryPrimaryIconStockPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryPrimaryIconStorageTypePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryPrimaryIconTooltipMarkupPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryPrimaryIconTooltipTextPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryProgressFractionPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryProgressPulseStepPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryScrollOffsetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySecondaryIconActivatablePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySecondaryIconGiconPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySecondaryIconNamePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySecondaryIconPixbufPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySecondaryIconSensitivePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySecondaryIconStockPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySecondaryIconStorageTypePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySecondaryIconTooltipMarkupPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySecondaryIconTooltipTextPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySelectionBoundPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryShadowTypePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryShowEmojiIconPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryTabsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryTextPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryTextLengthPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryTruncateMultilinePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryVisibilityPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryWidthCharsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryXalignPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetActivatesDefaultMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetAlignmentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetAttributesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetBufferMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetCompletionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetCurrentIconDragSourceMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetCursorHadjustmentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetHasFrameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetIconActivatableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetIconAreaMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetIconAtPosMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetIconGiconMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetIconNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetIconPixbufMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetIconSensitiveMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetIconStockMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetIconStorageTypeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetIconTooltipMarkupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetIconTooltipTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetInnerBorderMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetInputHintsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetInputPurposeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetInvisibleCharMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetLayoutMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetLayoutOffsetsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetMaxLengthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetMaxWidthCharsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetOverwriteModeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetPlaceholderTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetProgressFractionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetProgressPulseStepMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetTabsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetTextAreaMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetTextLengthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetVisibilityMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGetWidthCharsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryGrabFocusWithoutSelectingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryImContextFilterKeypressMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryLayoutIndexToTextIndexMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryProgressPulseMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryResetImContextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySetActivatesDefaultMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySetAlignmentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySetAttributesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySetBufferMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySetCompletionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySetCursorHadjustmentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySetHasFrameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySetIconActivatableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySetIconDragSourceMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySetIconFromGiconMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySetIconFromIconNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySetIconFromPixbufMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySetIconFromStockMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySetIconSensitiveMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySetIconTooltipMarkupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySetIconTooltipTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySetInnerBorderMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySetInputHintsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySetInputPurposeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySetInvisibleCharMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySetMaxLengthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySetMaxWidthCharsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySetOverwriteModeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySetPlaceholderTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySetProgressFractionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySetProgressPulseStepMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySetTabsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySetTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySetVisibilityMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntrySetWidthCharsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryTextIndexToLayoutIndexMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryUnsetInvisibleCharMethodInfo
#endif
