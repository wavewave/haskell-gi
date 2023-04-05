#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.TreeView where

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

newtype TreeView = TreeView (SP.ManagedPtr TreeView)
instance SP.ManagedPtrNewtype TreeView where
instance B.Types.TypedObject TreeView where
instance B.Types.GObject TreeView
class (SP.GObject o, O.IsDescendantOf TreeView o) => IsTreeView o
instance (SP.GObject o, O.IsDescendantOf TreeView o) => IsTreeView o
instance O.HasParentTypes TreeView
toTreeView :: (MIO.MonadIO m, IsTreeView o) => o -> m TreeView
instance B.GValue.IsGValue (Maybe TreeView) where
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnsChangedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewCursorChangedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewExpandCollapseCursorRowSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewMoveCursorSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewRowActivatedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewRowCollapsedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewRowExpandedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSelectAllSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSelectCursorParentSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSelectCursorRowSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewStartInteractiveSearchSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewTestCollapseRowSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewTestExpandRowSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewToggleCursorRowSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewUnselectAllSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewActivateOnSingleClickPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewEnableGridLinesPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewEnableSearchPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewEnableTreeLinesPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewExpanderColumnPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewFixedHeightModePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewHeadersClickablePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewHeadersVisiblePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewHoverExpandPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewHoverSelectionPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewLevelIndentationPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewModelPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewReorderablePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewRubberBandingPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewRulesHintPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSearchColumnPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewShowExpandersPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewTooltipColumnPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewAppendColumnMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewCollapseAllMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewCollapseRowMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewColumnsAutosizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewConvertBinWindowToTreeCoordsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewConvertBinWindowToWidgetCoordsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewConvertTreeToBinWindowCoordsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewConvertTreeToWidgetCoordsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewConvertWidgetToBinWindowCoordsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewConvertWidgetToTreeCoordsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewCreateRowDragIconMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewEnableModelDragDestMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewEnableModelDragSourceMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewExpandAllMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewExpandRowMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewExpandToPathMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetActivateOnSingleClickMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetBackgroundAreaMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetBinWindowMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetCellAreaMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetColumnMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetColumnsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetCursorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetDestRowAtPosMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetDragDestRowMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetEnableSearchMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetEnableTreeLinesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetExpanderColumnMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetFixedHeightModeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetGridLinesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetHadjustmentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetHeadersClickableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetHeadersVisibleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetHoverExpandMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetHoverSelectionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetLevelIndentationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetModelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetNColumnsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetPathAtPosMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetReorderableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetRubberBandingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetRulesHintMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetSearchColumnMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetSearchEntryMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetSelectionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetShowExpandersMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetTooltipColumnMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetTooltipContextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetVadjustmentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetVisibleRangeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewGetVisibleRectMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewInsertColumnMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewInsertColumnWithDataFuncMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewIsBlankAtPosMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewIsRubberBandingActiveMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewMapExpandedRowsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewMoveColumnAfterMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewRemoveColumnMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewRowActivatedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewRowExpandedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewScrollToCellMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewScrollToPointMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSetActivateOnSingleClickMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSetColumnDragFunctionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSetCursorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSetCursorOnCellMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSetDestroyCountFuncMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSetDragDestRowMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSetEnableSearchMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSetEnableTreeLinesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSetExpanderColumnMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSetFixedHeightModeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSetGridLinesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSetHadjustmentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSetHeadersClickableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSetHeadersVisibleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSetHoverExpandMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSetHoverSelectionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSetLevelIndentationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSetModelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSetReorderableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSetRowSeparatorFuncMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSetRubberBandingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSetRulesHintMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSetSearchColumnMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSetSearchEntryMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSetSearchEqualFuncMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSetSearchPositionFuncMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSetShowExpandersMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSetTooltipCellMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSetTooltipColumnMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSetTooltipRowMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewSetVadjustmentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewUnsetRowsDragDestMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data TreeViewUnsetRowsDragSourceMethodInfo
#endif
