#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.ListBox where

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

newtype ListBox = ListBox (SP.ManagedPtr ListBox)
instance SP.ManagedPtrNewtype ListBox where
instance B.Types.TypedObject ListBox where
instance B.Types.GObject ListBox
class (SP.GObject o, O.IsDescendantOf ListBox o) => IsListBox o
instance (SP.GObject o, O.IsDescendantOf ListBox o) => IsListBox o
instance O.HasParentTypes ListBox
toListBox :: (MIO.MonadIO m, IsListBox o) => o -> m ListBox
instance B.GValue.IsGValue (Maybe ListBox) where
#if defined(ENABLE_OVERLOADING)
data ListBoxActivateCursorRowSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxMoveCursorSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxRowActivatedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxRowSelectedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxSelectAllSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxSelectedRowsChangedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxToggleCursorRowSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxUnselectAllSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxActivateOnSingleClickPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxSelectionModePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxBindModelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxDragHighlightRowMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxDragUnhighlightRowMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxGetActivateOnSingleClickMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxGetAdjustmentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxGetRowAtIndexMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxGetRowAtYMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxGetSelectedRowMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxGetSelectedRowsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxGetSelectionModeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxInsertMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxInvalidateFilterMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxInvalidateHeadersMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxInvalidateSortMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxPrependMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxSelectAllMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxSelectRowMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxSelectedForeachMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxSetActivateOnSingleClickMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxSetAdjustmentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxSetFilterFuncMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxSetHeaderFuncMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxSetPlaceholderMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxSetSelectionModeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxSetSortFuncMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxUnselectAllMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListBoxUnselectRowMethodInfo
#endif
