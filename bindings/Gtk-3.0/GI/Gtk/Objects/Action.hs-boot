#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.Action where

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

newtype Action = Action (SP.ManagedPtr Action)
instance SP.ManagedPtrNewtype Action where
instance B.Types.TypedObject Action where
instance B.Types.GObject Action
class (SP.GObject o, O.IsDescendantOf Action o) => IsAction o
instance (SP.GObject o, O.IsDescendantOf Action o) => IsAction o
instance O.HasParentTypes Action
toAction :: (MIO.MonadIO m, IsAction o) => o -> m Action
instance B.GValue.IsGValue (Maybe Action) where
#if defined(ENABLE_OVERLOADING)
data ActionActivateSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionActionGroupPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionAlwaysShowImagePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionGiconPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionHideIfEmptyPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionIconNamePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionIsImportantPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionLabelPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionNamePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionSensitivePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionShortLabelPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionStockIdPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionTooltipPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionVisiblePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionVisibleHorizontalPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionVisibleOverflownPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionVisibleVerticalPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionActivateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionBlockActivateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionConnectAcceleratorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionCreateIconMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionCreateMenuMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionCreateMenuItemMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionCreateToolItemMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionDisconnectAcceleratorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionGetAccelClosureMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionGetAccelPathMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionGetAlwaysShowImageMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionGetGiconMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionGetIconNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionGetIsImportantMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionGetLabelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionGetNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionGetProxiesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionGetSensitiveMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionGetShortLabelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionGetStockIdMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionGetTooltipMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionGetVisibleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionGetVisibleHorizontalMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionGetVisibleVerticalMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionIsSensitiveMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionIsVisibleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionSetAccelGroupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionSetAccelPathMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionSetAlwaysShowImageMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionSetGiconMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionSetIconNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionSetIsImportantMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionSetLabelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionSetSensitiveMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionSetShortLabelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionSetStockIdMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionSetTooltipMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionSetVisibleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionSetVisibleHorizontalMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionSetVisibleVerticalMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ActionUnblockActivateMethodInfo
#endif
