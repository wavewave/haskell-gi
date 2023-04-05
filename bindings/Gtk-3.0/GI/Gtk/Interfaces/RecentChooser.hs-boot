#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Interfaces.RecentChooser where

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

newtype RecentChooser = RecentChooser (SP.ManagedPtr RecentChooser)
instance SP.ManagedPtrNewtype RecentChooser where
instance B.Types.TypedObject RecentChooser where
instance B.Types.GObject RecentChooser
class (SP.GObject o, O.IsDescendantOf RecentChooser o) => IsRecentChooser o
instance (SP.GObject o, O.IsDescendantOf RecentChooser o) => IsRecentChooser o
instance O.HasParentTypes RecentChooser
toRecentChooser :: (MIO.MonadIO m, IsRecentChooser o) => o -> m RecentChooser
instance B.GValue.IsGValue (Maybe RecentChooser) where
#if defined(ENABLE_OVERLOADING)
data RecentChooserFilterPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserLimitPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserLocalOnlyPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserRecentManagerPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserSelectMultiplePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserShowIconsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserShowNotFoundPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserShowPrivatePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserShowTipsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserSortTypePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserAddFilterMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserGetCurrentItemMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserGetCurrentUriMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserGetFilterMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserGetItemsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserGetLimitMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserGetLocalOnlyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserGetSelectMultipleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserGetShowIconsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserGetShowNotFoundMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserGetShowPrivateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserGetShowTipsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserGetSortTypeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserGetUrisMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserListFiltersMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserRemoveFilterMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserSelectAllMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserSelectUriMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserSetCurrentUriMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserSetFilterMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserSetLimitMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserSetLocalOnlyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserSetSelectMultipleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserSetShowIconsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserSetShowNotFoundMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserSetShowPrivateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserSetShowTipsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserSetSortFuncMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserSetSortTypeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserUnselectAllMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserUnselectUriMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserItemActivatedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentChooserSelectionChangedSignalInfo
#endif
