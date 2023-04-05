#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.TreeStore where

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

newtype TreeStore = TreeStore (SP.ManagedPtr TreeStore)
instance SP.ManagedPtrNewtype TreeStore where
instance B.Types.TypedObject TreeStore where
instance B.Types.GObject TreeStore
class (SP.GObject o, O.IsDescendantOf TreeStore o) => IsTreeStore o
instance (SP.GObject o, O.IsDescendantOf TreeStore o) => IsTreeStore o
instance O.HasParentTypes TreeStore
toTreeStore :: (MIO.MonadIO m, IsTreeStore o) => o -> m TreeStore
instance B.GValue.IsGValue (Maybe TreeStore) where
#if defined(ENABLE_OVERLOADING)
data TreeStoreAppendMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeStoreClearMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeStoreInsertMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeStoreInsertAfterMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeStoreInsertBeforeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeStoreInsertWithValuesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeStoreIsAncestorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeStoreIterDepthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeStoreIterIsValidMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeStoreMoveAfterMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeStoreMoveBeforeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeStorePrependMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeStoreRemoveMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeStoreSetColumnTypesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeStoreSetValueMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeStoreSetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeStoreSwapMethodInfo
#endif
