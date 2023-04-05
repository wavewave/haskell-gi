#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.TreeSelection where

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

newtype TreeSelection = TreeSelection (SP.ManagedPtr TreeSelection)
instance SP.ManagedPtrNewtype TreeSelection where
instance B.Types.TypedObject TreeSelection where
instance B.Types.GObject TreeSelection
class (SP.GObject o, O.IsDescendantOf TreeSelection o) => IsTreeSelection o
instance (SP.GObject o, O.IsDescendantOf TreeSelection o) => IsTreeSelection o
instance O.HasParentTypes TreeSelection
toTreeSelection :: (MIO.MonadIO m, IsTreeSelection o) => o -> m TreeSelection
instance B.GValue.IsGValue (Maybe TreeSelection) where
#if defined(ENABLE_OVERLOADING)
data TreeSelectionChangedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeSelectionModePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeSelectionCountSelectedRowsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeSelectionGetModeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeSelectionGetSelectedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeSelectionGetSelectedRowsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeSelectionGetTreeViewMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeSelectionIterIsSelectedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeSelectionPathIsSelectedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeSelectionSelectAllMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeSelectionSelectIterMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeSelectionSelectPathMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeSelectionSelectRangeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeSelectionSelectedForeachMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeSelectionSetModeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeSelectionSetSelectFunctionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeSelectionUnselectAllMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeSelectionUnselectIterMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeSelectionUnselectPathMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeSelectionUnselectRangeMethodInfo
#endif
