#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.UIManager where

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

newtype UIManager = UIManager (SP.ManagedPtr UIManager)
instance SP.ManagedPtrNewtype UIManager where
instance B.Types.TypedObject UIManager where
instance B.Types.GObject UIManager
class (SP.GObject o, O.IsDescendantOf UIManager o) => IsUIManager o
instance (SP.GObject o, O.IsDescendantOf UIManager o) => IsUIManager o
instance O.HasParentTypes UIManager
toUIManager :: (MIO.MonadIO m, IsUIManager o) => o -> m UIManager
instance B.GValue.IsGValue (Maybe UIManager) where
#if defined(ENABLE_OVERLOADING)
data UIManagerActionsChangedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data UIManagerAddWidgetSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data UIManagerConnectProxySignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data UIManagerDisconnectProxySignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data UIManagerPostActivateSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data UIManagerPreActivateSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data UIManagerAddTearoffsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data UIManagerUiPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data UIManagerAddUiMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data UIManagerAddUiFromFileMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data UIManagerAddUiFromResourceMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data UIManagerAddUiFromStringMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data UIManagerEnsureUpdateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data UIManagerGetAccelGroupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data UIManagerGetActionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data UIManagerGetActionGroupsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data UIManagerGetAddTearoffsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data UIManagerGetToplevelsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data UIManagerGetUiMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data UIManagerGetWidgetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data UIManagerInsertActionGroupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data UIManagerNewMergeIdMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data UIManagerRemoveActionGroupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data UIManagerRemoveUiMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data UIManagerSetAddTearoffsMethodInfo
#endif
