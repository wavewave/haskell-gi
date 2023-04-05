#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.Menu where

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

newtype Menu = Menu (SP.ManagedPtr Menu)
instance SP.ManagedPtrNewtype Menu where
instance B.Types.TypedObject Menu where
instance B.Types.GObject Menu
class (SP.GObject o, O.IsDescendantOf Menu o) => IsMenu o
instance (SP.GObject o, O.IsDescendantOf Menu o) => IsMenu o
instance O.HasParentTypes Menu
toMenu :: (MIO.MonadIO m, IsMenu o) => o -> m Menu
instance B.GValue.IsGValue (Maybe Menu) where
#if defined(ENABLE_OVERLOADING)
data MenuMoveScrollSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuPoppedUpSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuAccelGroupPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuAccelPathPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuActivePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuAnchorHintsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuAttachWidgetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuMenuTypeHintPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuMonitorPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuRectAnchorDxPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuRectAnchorDyPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuReserveToggleSizePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuTearoffStatePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuTearoffTitlePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuAttachMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuAttachToWidgetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuDetachMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuGetAccelGroupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuGetAccelPathMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuGetActiveMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuGetAttachWidgetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuGetMonitorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuGetReserveToggleSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuGetTearoffStateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuGetTitleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuPlaceOnMonitorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuPopdownMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuPopupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuPopupAtPointerMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuPopupAtRectMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuPopupAtWidgetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuPopupForDeviceMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuReorderChildMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuRepositionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuSetAccelGroupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuSetAccelPathMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuSetActiveMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuSetMonitorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuSetReserveToggleSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuSetScreenMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuSetTearoffStateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuSetTitleMethodInfo
#endif
