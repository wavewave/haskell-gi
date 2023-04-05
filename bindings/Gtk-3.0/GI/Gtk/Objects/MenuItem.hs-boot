#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.MenuItem where

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

newtype MenuItem = MenuItem (SP.ManagedPtr MenuItem)
instance SP.ManagedPtrNewtype MenuItem where
instance B.Types.TypedObject MenuItem where
instance B.Types.GObject MenuItem
class (SP.GObject o, O.IsDescendantOf MenuItem o) => IsMenuItem o
instance (SP.GObject o, O.IsDescendantOf MenuItem o) => IsMenuItem o
instance O.HasParentTypes MenuItem
toMenuItem :: (MIO.MonadIO m, IsMenuItem o) => o -> m MenuItem
instance B.GValue.IsGValue (Maybe MenuItem) where
#if defined(ENABLE_OVERLOADING)
data MenuItemActivateSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuItemActivateItemSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuItemDeselectSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuItemSelectSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuItemToggleSizeAllocateSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuItemToggleSizeRequestSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuItemAccelPathPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuItemLabelPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuItemRightJustifiedPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuItemSubmenuPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuItemUseUnderlinePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuItemActivateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuItemDeselectMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuItemGetAccelPathMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuItemGetLabelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuItemGetReserveIndicatorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuItemGetRightJustifiedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuItemGetSubmenuMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuItemGetUseUnderlineMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuItemSelectMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuItemSetAccelPathMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuItemSetLabelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuItemSetReserveIndicatorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuItemSetRightJustifiedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuItemSetSubmenuMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuItemSetUseUnderlineMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuItemToggleSizeAllocateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuItemToggleSizeRequestMethodInfo
#endif
