#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.Image where

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

newtype Image = Image (SP.ManagedPtr Image)
instance SP.ManagedPtrNewtype Image where
instance B.Types.TypedObject Image where
instance B.Types.GObject Image
class (SP.GObject o, O.IsDescendantOf Image o) => IsImage o
instance (SP.GObject o, O.IsDescendantOf Image o) => IsImage o
instance O.HasParentTypes Image
toImage :: (MIO.MonadIO m, IsImage o) => o -> m Image
instance B.GValue.IsGValue (Maybe Image) where
#if defined(ENABLE_OVERLOADING)
data ImageFilePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ImageGiconPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ImageIconNamePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ImageIconSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ImageIconSizePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ImagePixbufPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ImagePixbufAnimationPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ImagePixelSizePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ImageResourcePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ImageStockPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ImageStorageTypePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ImageSurfacePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ImageUseFallbackPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ImageClearMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ImageGetAnimationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ImageGetGiconMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ImageGetIconNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ImageGetIconSetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ImageGetPixbufMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ImageGetPixelSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ImageGetStockMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ImageGetStorageTypeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ImageSetFromAnimationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ImageSetFromFileMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ImageSetFromGiconMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ImageSetFromIconNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ImageSetFromIconSetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ImageSetFromPixbufMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ImageSetFromResourceMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ImageSetFromStockMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ImageSetFromSurfaceMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ImageSetPixelSizeMethodInfo
#endif
