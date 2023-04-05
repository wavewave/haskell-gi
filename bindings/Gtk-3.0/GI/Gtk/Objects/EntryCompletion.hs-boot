#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.EntryCompletion where

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

newtype EntryCompletion = EntryCompletion (SP.ManagedPtr EntryCompletion)
instance SP.ManagedPtrNewtype EntryCompletion where
instance B.Types.TypedObject EntryCompletion where
instance B.Types.GObject EntryCompletion
class (SP.GObject o, O.IsDescendantOf EntryCompletion o) => IsEntryCompletion o
instance (SP.GObject o, O.IsDescendantOf EntryCompletion o) => IsEntryCompletion o
instance O.HasParentTypes EntryCompletion
toEntryCompletion :: (MIO.MonadIO m, IsEntryCompletion o) => o -> m EntryCompletion
instance B.GValue.IsGValue (Maybe EntryCompletion) where
#if defined(ENABLE_OVERLOADING)
data EntryCompletionActionActivatedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionCursorOnMatchSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionInsertPrefixSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionMatchSelectedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionNoMatchesSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionCellAreaPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionInlineCompletionPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionInlineSelectionPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionMinimumKeyLengthPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionModelPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionPopupCompletionPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionPopupSetWidthPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionPopupSingleMatchPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionTextColumnPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionCompleteMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionComputePrefixMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionDeleteActionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionGetCompletionPrefixMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionGetEntryMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionGetInlineCompletionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionGetInlineSelectionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionGetMinimumKeyLengthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionGetModelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionGetPopupCompletionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionGetPopupSetWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionGetPopupSingleMatchMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionGetTextColumnMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionInsertActionMarkupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionInsertActionTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionInsertPrefixMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionSetInlineCompletionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionSetInlineSelectionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionSetMatchFuncMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionSetMinimumKeyLengthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionSetModelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionSetPopupCompletionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionSetPopupSetWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionSetPopupSingleMatchMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EntryCompletionSetTextColumnMethodInfo
#endif
