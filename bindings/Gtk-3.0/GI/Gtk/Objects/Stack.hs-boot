#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.Stack where

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

newtype Stack = Stack (SP.ManagedPtr Stack)
instance SP.ManagedPtrNewtype Stack where
instance B.Types.TypedObject Stack where
instance B.Types.GObject Stack
class (SP.GObject o, O.IsDescendantOf Stack o) => IsStack o
instance (SP.GObject o, O.IsDescendantOf Stack o) => IsStack o
instance O.HasParentTypes Stack
toStack :: (MIO.MonadIO m, IsStack o) => o -> m Stack
instance B.GValue.IsGValue (Maybe Stack) where
#if defined(ENABLE_OVERLOADING)
data StackHhomogeneousPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StackHomogeneousPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StackInterpolateSizePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StackTransitionDurationPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StackTransitionRunningPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StackTransitionTypePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StackVhomogeneousPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StackVisibleChildPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StackVisibleChildNamePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StackAddNamedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StackAddTitledMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StackGetChildByNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StackGetHhomogeneousMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StackGetHomogeneousMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StackGetInterpolateSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StackGetTransitionDurationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StackGetTransitionRunningMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StackGetTransitionTypeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StackGetVhomogeneousMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StackGetVisibleChildMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StackGetVisibleChildNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StackSetHhomogeneousMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StackSetHomogeneousMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StackSetInterpolateSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StackSetTransitionDurationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StackSetTransitionTypeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StackSetVhomogeneousMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StackSetVisibleChildMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StackSetVisibleChildFullMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StackSetVisibleChildNameMethodInfo
#endif
