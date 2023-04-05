#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.Gesture where

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

newtype Gesture = Gesture (SP.ManagedPtr Gesture)
instance SP.ManagedPtrNewtype Gesture where
instance B.Types.TypedObject Gesture where
instance B.Types.GObject Gesture
class (SP.GObject o, O.IsDescendantOf Gesture o) => IsGesture o
instance (SP.GObject o, O.IsDescendantOf Gesture o) => IsGesture o
instance O.HasParentTypes Gesture
toGesture :: (MIO.MonadIO m, IsGesture o) => o -> m Gesture
instance B.GValue.IsGValue (Maybe Gesture) where
#if defined(ENABLE_OVERLOADING)
data GestureBeginSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GestureCancelSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GestureEndSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GestureSequenceStateChangedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GestureUpdateSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GestureNPointsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GestureWindowPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GestureGetBoundingBoxMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GestureGetBoundingBoxCenterMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GestureGetDeviceMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GestureGetGroupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GestureGetLastEventMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GestureGetLastUpdatedSequenceMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GestureGetPointMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GestureGetSequenceStateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GestureGetSequencesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GestureGetWindowMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GestureGroupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GestureHandlesSequenceMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GestureIsActiveMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GestureIsGroupedWithMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GestureIsRecognizedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GestureSetSequenceStateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GestureSetStateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GestureSetWindowMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GestureUngroupMethodInfo
#endif
