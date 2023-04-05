#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.Scale where

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

newtype Scale = Scale (SP.ManagedPtr Scale)
instance SP.ManagedPtrNewtype Scale where
instance B.Types.TypedObject Scale where
instance B.Types.GObject Scale
class (SP.GObject o, O.IsDescendantOf Scale o) => IsScale o
instance (SP.GObject o, O.IsDescendantOf Scale o) => IsScale o
instance O.HasParentTypes Scale
toScale :: (MIO.MonadIO m, IsScale o) => o -> m Scale
instance B.GValue.IsGValue (Maybe Scale) where
#if defined(ENABLE_OVERLOADING)
data ScaleFormatValueSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScaleDigitsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScaleDrawValuePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScaleHasOriginPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScaleValuePosPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScaleAddMarkMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScaleClearMarksMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScaleGetDigitsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScaleGetDrawValueMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScaleGetHasOriginMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScaleGetLayoutMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScaleGetLayoutOffsetsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScaleGetValuePosMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScaleSetDigitsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScaleSetDrawValueMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScaleSetHasOriginMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ScaleSetValuePosMethodInfo
#endif
