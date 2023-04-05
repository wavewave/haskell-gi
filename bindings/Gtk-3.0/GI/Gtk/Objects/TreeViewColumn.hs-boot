#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.TreeViewColumn where

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

newtype TreeViewColumn = TreeViewColumn (SP.ManagedPtr TreeViewColumn)
instance SP.ManagedPtrNewtype TreeViewColumn where
instance B.Types.TypedObject TreeViewColumn where
instance B.Types.GObject TreeViewColumn
class (SP.GObject o, O.IsDescendantOf TreeViewColumn o) => IsTreeViewColumn o
instance (SP.GObject o, O.IsDescendantOf TreeViewColumn o) => IsTreeViewColumn o
instance O.HasParentTypes TreeViewColumn
toTreeViewColumn :: (MIO.MonadIO m, IsTreeViewColumn o) => o -> m TreeViewColumn
instance B.GValue.IsGValue (Maybe TreeViewColumn) where
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnClickedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnAlignmentPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnCellAreaPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnClickablePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnExpandPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnFixedWidthPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnMaxWidthPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnMinWidthPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnReorderablePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnResizablePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSizingPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSortColumnIdPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSortIndicatorPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSortOrderPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSpacingPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnTitlePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnVisiblePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnWidgetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnWidthPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnXOffsetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnAddAttributeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnCellGetPositionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnCellGetSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnCellIsVisibleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnCellSetCellDataMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnClearMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnClearAttributesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnClickedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnFocusCellMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetAlignmentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetButtonMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetClickableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetExpandMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetFixedWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetMaxWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetMinWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetReorderableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetResizableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetSizingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetSortColumnIdMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetSortIndicatorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetSortOrderMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetSpacingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetTitleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetTreeViewMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetVisibleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetWidgetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetXOffsetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnPackEndMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnPackStartMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnQueueResizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSetAlignmentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSetCellDataFuncMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSetClickableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSetExpandMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSetFixedWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSetMaxWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSetMinWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSetReorderableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSetResizableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSetSizingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSetSortColumnIdMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSetSortIndicatorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSetSortOrderMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSetSpacingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSetTitleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSetVisibleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSetWidgetMethodInfo
#endif
