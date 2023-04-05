#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Structs.WidgetPath where

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

newtype WidgetPath = WidgetPath (SP.ManagedPtr WidgetPath)
instance SP.ManagedPtrNewtype WidgetPath where
instance O.HasParentTypes WidgetPath
instance B.Types.TypedObject WidgetPath where
instance B.Types.GBoxed WidgetPath
instance B.GValue.IsGValue (Maybe WidgetPath) where
#if defined(ENABLE_OVERLOADING)
data WidgetPathAppendForWidgetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathAppendTypeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathAppendWithSiblingsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathCopyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathFreeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathGetObjectTypeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathHasParentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathIsTypeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathIterAddClassMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathIterAddRegionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathIterClearClassesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathIterClearRegionsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathIterGetNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathIterGetObjectNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathIterGetObjectTypeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathIterGetSiblingIndexMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathIterGetSiblingsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathIterGetStateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathIterHasClassMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathIterHasNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathIterHasQclassMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathIterHasQnameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathIterHasQregionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathIterHasRegionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathIterListClassesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathIterListRegionsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathIterRemoveClassMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathIterRemoveRegionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathIterSetNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathIterSetObjectNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathIterSetObjectTypeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathIterSetStateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathLengthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathPrependTypeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathRefMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathToStringMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathUnrefMethodInfo
#endif
