#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.PrintOperation where

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

newtype PrintOperation = PrintOperation (SP.ManagedPtr PrintOperation)
instance SP.ManagedPtrNewtype PrintOperation where
instance B.Types.TypedObject PrintOperation where
instance B.Types.GObject PrintOperation
class (SP.GObject o, O.IsDescendantOf PrintOperation o) => IsPrintOperation o
instance (SP.GObject o, O.IsDescendantOf PrintOperation o) => IsPrintOperation o
instance O.HasParentTypes PrintOperation
toPrintOperation :: (MIO.MonadIO m, IsPrintOperation o) => o -> m PrintOperation
instance B.GValue.IsGValue (Maybe PrintOperation) where
#if defined(ENABLE_OVERLOADING)
data PrintOperationBeginPrintSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationCreateCustomWidgetSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationCustomWidgetApplySignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationDoneSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationDrawPageSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationEndPrintSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationPaginateSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationPreviewSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationRequestPageSetupSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationStatusChangedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationUpdateCustomWidgetSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationAllowAsyncPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationCurrentPagePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationCustomTabLabelPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationDefaultPageSetupPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationEmbedPageSetupPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationExportFilenamePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationHasSelectionPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationJobNamePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationNPagesPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationNPagesToPrintPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationPrintSettingsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationShowProgressPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationStatusPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationStatusStringPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationSupportSelectionPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationTrackPrintStatusPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationUnitPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationUseFullPagePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationCancelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationDrawPageFinishMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationGetDefaultPageSetupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationGetEmbedPageSetupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationGetErrorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationGetHasSelectionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationGetNPagesToPrintMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationGetPrintSettingsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationGetStatusMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationGetStatusStringMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationGetSupportSelectionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationIsFinishedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationRunMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationSetAllowAsyncMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationSetCurrentPageMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationSetCustomTabLabelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationSetDefaultPageSetupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationSetDeferDrawingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationSetEmbedPageSetupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationSetExportFilenameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationSetHasSelectionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationSetJobNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationSetNPagesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationSetPrintSettingsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationSetShowProgressMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationSetSupportSelectionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationSetTrackPrintStatusMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationSetUnitMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintOperationSetUseFullPageMethodInfo
#endif
