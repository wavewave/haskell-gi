#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.EventController where

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

newtype EventController = EventController (SP.ManagedPtr EventController)
instance SP.ManagedPtrNewtype EventController where
instance B.Types.TypedObject EventController where
instance B.Types.GObject EventController
class (SP.GObject o, O.IsDescendantOf EventController o) => IsEventController o
instance (SP.GObject o, O.IsDescendantOf EventController o) => IsEventController o
instance O.HasParentTypes EventController
toEventController :: (MIO.MonadIO m, IsEventController o) => o -> m EventController
instance B.GValue.IsGValue (Maybe EventController) where
#if defined(ENABLE_OVERLOADING)
data EventControllerPropagationPhasePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EventControllerWidgetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EventControllerGetPropagationPhaseMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EventControllerGetWidgetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EventControllerHandleEventMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EventControllerResetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EventControllerSetPropagationPhaseMethodInfo
#endif
