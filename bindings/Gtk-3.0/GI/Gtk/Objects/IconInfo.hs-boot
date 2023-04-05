#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.IconInfo where

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

newtype IconInfo = IconInfo (SP.ManagedPtr IconInfo)
instance SP.ManagedPtrNewtype IconInfo where
instance B.Types.TypedObject IconInfo where
instance B.Types.GObject IconInfo
class (SP.GObject o, O.IsDescendantOf IconInfo o) => IsIconInfo o
instance (SP.GObject o, O.IsDescendantOf IconInfo o) => IsIconInfo o
instance O.HasParentTypes IconInfo
toIconInfo :: (MIO.MonadIO m, IsIconInfo o) => o -> m IconInfo
instance B.GValue.IsGValue (Maybe IconInfo) where
#if defined(ENABLE_OVERLOADING)
data IconInfoGetAttachPointsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconInfoGetBaseScaleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconInfoGetBaseSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconInfoGetBuiltinPixbufMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconInfoGetDisplayNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconInfoGetEmbeddedRectMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconInfoGetFilenameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconInfoIsSymbolicMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconInfoLoadIconMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconInfoLoadIconAsyncMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconInfoLoadIconFinishMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconInfoLoadSurfaceMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconInfoLoadSymbolicMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconInfoLoadSymbolicAsyncMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconInfoLoadSymbolicFinishMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconInfoLoadSymbolicForContextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconInfoLoadSymbolicForContextAsyncMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconInfoLoadSymbolicForContextFinishMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconInfoLoadSymbolicForStyleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconInfoSetRawCoordinatesMethodInfo
#endif
