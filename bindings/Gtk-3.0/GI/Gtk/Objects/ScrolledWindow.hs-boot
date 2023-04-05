#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.ScrolledWindow where

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

newtype ScrolledWindow = ScrolledWindow (SP.ManagedPtr ScrolledWindow)
instance SP.ManagedPtrNewtype ScrolledWindow where
instance B.Types.TypedObject ScrolledWindow where
instance B.Types.GObject ScrolledWindow
class (SP.GObject o, O.IsDescendantOf ScrolledWindow o) => IsScrolledWindow o
instance (SP.GObject o, O.IsDescendantOf ScrolledWindow o) => IsScrolledWindow o
instance O.HasParentTypes ScrolledWindow
toScrolledWindow :: (MIO.MonadIO m, IsScrolledWindow o) => o -> m ScrolledWindow
instance B.GValue.IsGValue (Maybe ScrolledWindow) where
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowEdgeOvershotSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowEdgeReachedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowMoveFocusOutSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowScrollChildSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowHadjustmentPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowHscrollbarPolicyPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowKineticScrollingPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowMaxContentHeightPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowMaxContentWidthPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowMinContentHeightPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowMinContentWidthPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowOverlayScrollingPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowPropagateNaturalHeightPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowPropagateNaturalWidthPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowShadowTypePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowVadjustmentPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowVscrollbarPolicyPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowWindowPlacementPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowWindowPlacementSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowAddWithViewportMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowGetCaptureButtonPressMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowGetHadjustmentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowGetHscrollbarMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowGetKineticScrollingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowGetMaxContentHeightMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowGetMaxContentWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowGetMinContentHeightMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowGetMinContentWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowGetOverlayScrollingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowGetPlacementMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowGetPolicyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowGetPropagateNaturalHeightMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowGetPropagateNaturalWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowGetShadowTypeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowGetVadjustmentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowGetVscrollbarMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowSetCaptureButtonPressMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowSetHadjustmentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowSetKineticScrollingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowSetMaxContentHeightMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowSetMaxContentWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowSetMinContentHeightMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowSetMinContentWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowSetOverlayScrollingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowSetPlacementMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowSetPolicyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowSetPropagateNaturalHeightMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowSetPropagateNaturalWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowSetShadowTypeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowSetVadjustmentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScrolledWindowUnsetPlacementMethodInfo
#endif
