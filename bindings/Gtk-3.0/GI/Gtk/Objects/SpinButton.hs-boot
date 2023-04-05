#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.SpinButton where

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

newtype SpinButton = SpinButton (SP.ManagedPtr SpinButton)
instance SP.ManagedPtrNewtype SpinButton where
instance B.Types.TypedObject SpinButton where
instance B.Types.GObject SpinButton
class (SP.GObject o, O.IsDescendantOf SpinButton o) => IsSpinButton o
instance (SP.GObject o, O.IsDescendantOf SpinButton o) => IsSpinButton o
instance O.HasParentTypes SpinButton
toSpinButton :: (MIO.MonadIO m, IsSpinButton o) => o -> m SpinButton
instance B.GValue.IsGValue (Maybe SpinButton) where
#if defined(ENABLE_OVERLOADING)
data SpinButtonChangeValueSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SpinButtonInputSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SpinButtonOutputSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SpinButtonValueChangedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SpinButtonWrappedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SpinButtonAdjustmentPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SpinButtonClimbRatePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SpinButtonDigitsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SpinButtonNumericPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SpinButtonSnapToTicksPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SpinButtonUpdatePolicyPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SpinButtonValuePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SpinButtonWrapPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SpinButtonConfigureMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SpinButtonGetAdjustmentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SpinButtonGetDigitsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SpinButtonGetIncrementsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SpinButtonGetNumericMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SpinButtonGetRangeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SpinButtonGetSnapToTicksMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SpinButtonGetUpdatePolicyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SpinButtonGetValueMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SpinButtonGetValueAsIntMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SpinButtonGetWrapMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SpinButtonSetAdjustmentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SpinButtonSetDigitsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SpinButtonSetIncrementsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SpinButtonSetNumericMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SpinButtonSetRangeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SpinButtonSetSnapToTicksMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SpinButtonSetUpdatePolicyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SpinButtonSetValueMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SpinButtonSetWrapMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SpinButtonSpinMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SpinButtonUpdateMethodInfo
#endif
