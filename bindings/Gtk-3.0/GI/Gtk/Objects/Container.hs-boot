#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.Container where

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

newtype Container = Container (SP.ManagedPtr Container)
instance SP.ManagedPtrNewtype Container where
instance B.Types.TypedObject Container where
instance B.Types.GObject Container
class (SP.GObject o, O.IsDescendantOf Container o) => IsContainer o
instance (SP.GObject o, O.IsDescendantOf Container o) => IsContainer o
instance O.HasParentTypes Container
toContainer :: (MIO.MonadIO m, IsContainer o) => o -> m Container
instance B.GValue.IsGValue (Maybe Container) where
#if defined(ENABLE_OVERLOADING)
data ContainerAddSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ContainerCheckResizeSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ContainerRemoveSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ContainerSetFocusChildSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ContainerBorderWidthPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ContainerChildPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ContainerResizeModePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ContainerAddMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ContainerCheckResizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ContainerChildGetPropertyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ContainerChildNotifyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ContainerChildNotifyByPspecMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ContainerChildSetPropertyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ContainerChildTypeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ContainerForallMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ContainerForeachMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ContainerGetBorderWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ContainerGetChildrenMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ContainerGetFocusChainMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ContainerGetFocusChildMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ContainerGetFocusHadjustmentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ContainerGetFocusVadjustmentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ContainerGetPathForChildMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ContainerGetResizeModeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ContainerPropagateDrawMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ContainerRemoveMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ContainerResizeChildrenMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ContainerSetBorderWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ContainerSetFocusChainMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ContainerSetFocusChildMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ContainerSetFocusHadjustmentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ContainerSetFocusVadjustmentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ContainerSetReallocateRedrawsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ContainerSetResizeModeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ContainerUnsetFocusChainMethodInfo
#endif
