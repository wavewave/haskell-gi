#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.ListStore where

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

newtype ListStore = ListStore (SP.ManagedPtr ListStore)
instance SP.ManagedPtrNewtype ListStore where
instance B.Types.TypedObject ListStore where
instance B.Types.GObject ListStore
class (SP.GObject o, O.IsDescendantOf ListStore o) => IsListStore o
instance (SP.GObject o, O.IsDescendantOf ListStore o) => IsListStore o
instance O.HasParentTypes ListStore
toListStore :: (MIO.MonadIO m, IsListStore o) => o -> m ListStore
instance B.GValue.IsGValue (Maybe ListStore) where
#if defined(ENABLE_OVERLOADING)
data ListStoreAppendMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListStoreClearMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListStoreInsertMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListStoreInsertAfterMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListStoreInsertBeforeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListStoreInsertWithValuesvMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListStoreIterIsValidMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListStoreMoveAfterMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListStoreMoveBeforeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListStorePrependMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListStoreRemoveMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListStoreReorderMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListStoreSetColumnTypesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListStoreSetValueMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListStoreSetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ListStoreSwapMethodInfo
#endif
