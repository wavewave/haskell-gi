#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.PlacesSidebar where

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

newtype PlacesSidebar = PlacesSidebar (SP.ManagedPtr PlacesSidebar)
instance SP.ManagedPtrNewtype PlacesSidebar where
instance B.Types.TypedObject PlacesSidebar where
instance B.Types.GObject PlacesSidebar
class (SP.GObject o, O.IsDescendantOf PlacesSidebar o) => IsPlacesSidebar o
instance (SP.GObject o, O.IsDescendantOf PlacesSidebar o) => IsPlacesSidebar o
instance O.HasParentTypes PlacesSidebar
toPlacesSidebar :: (MIO.MonadIO m, IsPlacesSidebar o) => o -> m PlacesSidebar
instance B.GValue.IsGValue (Maybe PlacesSidebar) where
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarDragActionAskSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarDragActionRequestedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarDragPerformDropSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarMountSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarOpenLocationSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarPopulatePopupSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarShowConnectToServerSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarShowEnterLocationSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarShowErrorMessageSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarShowOtherLocationsSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarShowOtherLocationsWithFlagsSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarShowStarredLocationSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarUnmountSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarLocalOnlyPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarLocationPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarOpenFlagsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarPopulateAllPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarShowConnectToServerPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarShowDesktopPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarShowEnterLocationPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarShowOtherLocationsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarShowRecentPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarShowStarredLocationPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarShowTrashPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarAddShortcutMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarGetLocalOnlyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarGetLocationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarGetNthBookmarkMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarGetOpenFlagsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarGetShowConnectToServerMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarGetShowDesktopMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarGetShowEnterLocationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarGetShowOtherLocationsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarGetShowRecentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarGetShowStarredLocationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarGetShowTrashMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarListShortcutsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarRemoveShortcutMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarSetDropTargetsVisibleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarSetLocalOnlyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarSetLocationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarSetOpenFlagsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarSetShowConnectToServerMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarSetShowDesktopMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarSetShowEnterLocationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarSetShowOtherLocationsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarSetShowRecentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarSetShowStarredLocationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PlacesSidebarSetShowTrashMethodInfo
#endif
