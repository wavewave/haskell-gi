#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.ToolItem where

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

newtype ToolItem = ToolItem (SP.ManagedPtr ToolItem)
instance SP.ManagedPtrNewtype ToolItem where
instance B.Types.TypedObject ToolItem where
instance B.Types.GObject ToolItem
class (SP.GObject o, O.IsDescendantOf ToolItem o) => IsToolItem o
instance (SP.GObject o, O.IsDescendantOf ToolItem o) => IsToolItem o
instance O.HasParentTypes ToolItem
toToolItem :: (MIO.MonadIO m, IsToolItem o) => o -> m ToolItem
instance B.GValue.IsGValue (Maybe ToolItem) where
#if defined(ENABLE_OVERLOADING)
data ToolItemCreateMenuProxySignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolItemToolbarReconfiguredSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolItemIsImportantPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolItemVisibleHorizontalPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolItemVisibleVerticalPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolItemGetEllipsizeModeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolItemGetExpandMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolItemGetHomogeneousMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolItemGetIconSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolItemGetIsImportantMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolItemGetOrientationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolItemGetProxyMenuItemMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolItemGetReliefStyleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolItemGetTextAlignmentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolItemGetTextOrientationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolItemGetTextSizeGroupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolItemGetToolbarStyleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolItemGetUseDragWindowMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolItemGetVisibleHorizontalMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolItemGetVisibleVerticalMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolItemRebuildMenuMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolItemRetrieveProxyMenuItemMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolItemSetExpandMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolItemSetHomogeneousMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolItemSetIsImportantMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolItemSetProxyMenuItemMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolItemSetTooltipMarkupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolItemSetTooltipTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolItemSetUseDragWindowMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolItemSetVisibleHorizontalMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolItemSetVisibleVerticalMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolItemToolbarReconfiguredMethodInfo
#endif
