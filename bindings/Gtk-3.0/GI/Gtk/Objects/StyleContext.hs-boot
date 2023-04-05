#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.StyleContext where

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

newtype StyleContext = StyleContext (SP.ManagedPtr StyleContext)
instance SP.ManagedPtrNewtype StyleContext where
instance B.Types.TypedObject StyleContext where
instance B.Types.GObject StyleContext
class (SP.GObject o, O.IsDescendantOf StyleContext o) => IsStyleContext o
instance (SP.GObject o, O.IsDescendantOf StyleContext o) => IsStyleContext o
instance O.HasParentTypes StyleContext
toStyleContext :: (MIO.MonadIO m, IsStyleContext o) => o -> m StyleContext
instance B.GValue.IsGValue (Maybe StyleContext) where
#if defined(ENABLE_OVERLOADING)
data StyleContextChangedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextDirectionPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextPaintClockPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextParentPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextScreenPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextAddClassMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextAddProviderMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextAddRegionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextCancelAnimationsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextGetBackgroundColorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextGetBorderMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextGetBorderColorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextGetColorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextGetDirectionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextGetFontMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextGetFrameClockMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextGetJunctionSidesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextGetMarginMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextGetPaddingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextGetParentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextGetPathMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextGetPropertyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextGetScaleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextGetScreenMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextGetSectionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextGetStateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextGetStylePropertyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextHasClassMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextHasRegionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextInvalidateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextListClassesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextListRegionsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextLookupColorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextLookupIconSetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextNotifyStateChangeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextPopAnimatableRegionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextPushAnimatableRegionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextRemoveClassMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextRemoveProviderMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextRemoveRegionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextRestoreMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextSaveMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextScrollAnimationsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextSetBackgroundMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextSetDirectionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextSetFrameClockMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextSetJunctionSidesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextSetParentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextSetPathMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextSetScaleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextSetScreenMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextSetStateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextStateIsRunningMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StyleContextToStringMethodInfo
#endif
