#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.GLArea where

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

newtype GLArea = GLArea (SP.ManagedPtr GLArea)
instance SP.ManagedPtrNewtype GLArea where
instance B.Types.TypedObject GLArea where
instance B.Types.GObject GLArea
class (SP.GObject o, O.IsDescendantOf GLArea o) => IsGLArea o
instance (SP.GObject o, O.IsDescendantOf GLArea o) => IsGLArea o
instance O.HasParentTypes GLArea
toGLArea :: (MIO.MonadIO m, IsGLArea o) => o -> m GLArea
instance B.GValue.IsGValue (Maybe GLArea) where
#if defined(ENABLE_OVERLOADING)
data GLAreaCreateContextSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GLAreaRenderSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GLAreaResizeSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GLAreaAutoRenderPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GLAreaContextPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GLAreaHasAlphaPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GLAreaHasDepthBufferPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GLAreaHasStencilBufferPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GLAreaUseEsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GLAreaAttachBuffersMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GLAreaGetAutoRenderMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GLAreaGetContextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GLAreaGetErrorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GLAreaGetHasAlphaMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GLAreaGetHasDepthBufferMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GLAreaGetHasStencilBufferMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GLAreaGetRequiredVersionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GLAreaGetUseEsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GLAreaMakeCurrentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GLAreaQueueRenderMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GLAreaSetAutoRenderMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GLAreaSetErrorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GLAreaSetHasAlphaMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GLAreaSetHasDepthBufferMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GLAreaSetHasStencilBufferMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GLAreaSetRequiredVersionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data GLAreaSetUseEsMethodInfo
#endif
