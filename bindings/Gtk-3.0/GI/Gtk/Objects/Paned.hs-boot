#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.Paned where

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

newtype Paned = Paned (SP.ManagedPtr Paned)
instance SP.ManagedPtrNewtype Paned where
instance B.Types.TypedObject Paned where
instance B.Types.GObject Paned
class (SP.GObject o, O.IsDescendantOf Paned o) => IsPaned o
instance (SP.GObject o, O.IsDescendantOf Paned o) => IsPaned o
instance O.HasParentTypes Paned
toPaned :: (MIO.MonadIO m, IsPaned o) => o -> m Paned
instance B.GValue.IsGValue (Maybe Paned) where
#if defined(ENABLE_OVERLOADING)
data PanedAcceptPositionSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PanedCancelPositionSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PanedCycleChildFocusSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PanedCycleHandleFocusSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PanedMoveHandleSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PanedToggleHandleFocusSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PanedMaxPositionPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PanedMinPositionPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PanedPositionPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PanedPositionSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PanedWideHandlePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PanedAdd1MethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PanedAdd2MethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PanedGetChild1MethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PanedGetChild2MethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PanedGetHandleWindowMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PanedGetPositionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PanedGetWideHandleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PanedPack1MethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PanedPack2MethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PanedSetPositionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PanedSetWideHandleMethodInfo
#endif
