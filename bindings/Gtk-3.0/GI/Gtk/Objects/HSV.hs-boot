#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.HSV where

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

newtype HSV = HSV (SP.ManagedPtr HSV)
instance SP.ManagedPtrNewtype HSV where
instance B.Types.TypedObject HSV where
instance B.Types.GObject HSV
class (SP.GObject o, O.IsDescendantOf HSV o) => IsHSV o
instance (SP.GObject o, O.IsDescendantOf HSV o) => IsHSV o
instance O.HasParentTypes HSV
toHSV :: (MIO.MonadIO m, IsHSV o) => o -> m HSV
instance B.GValue.IsGValue (Maybe HSV) where
#if defined(ENABLE_OVERLOADING)
data HSVChangedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data HSVMoveSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data HSVGetColorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data HSVGetMetricsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data HSVIsAdjustingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data HSVSetColorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data HSVSetMetricsMethodInfo
#endif
