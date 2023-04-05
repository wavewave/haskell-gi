#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.FlowBox where

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

newtype FlowBox = FlowBox (SP.ManagedPtr FlowBox)
instance SP.ManagedPtrNewtype FlowBox where
instance B.Types.TypedObject FlowBox where
instance B.Types.GObject FlowBox
class (SP.GObject o, O.IsDescendantOf FlowBox o) => IsFlowBox o
instance (SP.GObject o, O.IsDescendantOf FlowBox o) => IsFlowBox o
instance O.HasParentTypes FlowBox
toFlowBox :: (MIO.MonadIO m, IsFlowBox o) => o -> m FlowBox
instance B.GValue.IsGValue (Maybe FlowBox) where
#if defined(ENABLE_OVERLOADING)
data FlowBoxActivateCursorChildSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxChildActivatedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxMoveCursorSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxSelectAllSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxSelectedChildrenChangedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxToggleCursorChildSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxUnselectAllSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxActivateOnSingleClickPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxColumnSpacingPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxHomogeneousPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxMaxChildrenPerLinePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxMinChildrenPerLinePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxRowSpacingPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxSelectionModePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxBindModelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxGetActivateOnSingleClickMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxGetChildAtIndexMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxGetChildAtPosMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxGetColumnSpacingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxGetHomogeneousMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxGetMaxChildrenPerLineMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxGetMinChildrenPerLineMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxGetRowSpacingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxGetSelectedChildrenMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxGetSelectionModeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxInsertMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxInvalidateFilterMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxInvalidateSortMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxSelectAllMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxSelectChildMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxSelectedForeachMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxSetActivateOnSingleClickMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxSetColumnSpacingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxSetFilterFuncMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxSetHadjustmentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxSetHomogeneousMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxSetMaxChildrenPerLineMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxSetMinChildrenPerLineMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxSetRowSpacingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxSetSelectionModeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxSetSortFuncMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxSetVadjustmentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxUnselectAllMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FlowBoxUnselectChildMethodInfo
#endif
