#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.IconView where

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

newtype IconView = IconView (SP.ManagedPtr IconView)
instance SP.ManagedPtrNewtype IconView where
instance B.Types.TypedObject IconView where
instance B.Types.GObject IconView
class (SP.GObject o, O.IsDescendantOf IconView o) => IsIconView o
instance (SP.GObject o, O.IsDescendantOf IconView o) => IsIconView o
instance O.HasParentTypes IconView
toIconView :: (MIO.MonadIO m, IsIconView o) => o -> m IconView
instance B.GValue.IsGValue (Maybe IconView) where
#if defined(ENABLE_OVERLOADING)
data IconViewActivateCursorItemSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewItemActivatedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewMoveCursorSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewSelectAllSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewSelectCursorItemSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewSelectionChangedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewToggleCursorItemSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewUnselectAllSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewActivateOnSingleClickPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewCellAreaPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewColumnSpacingPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewColumnsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewItemOrientationPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewItemPaddingPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewItemWidthPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewMarginPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewMarkupColumnPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewModelPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewPixbufColumnPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewReorderablePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewRowSpacingPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewSelectionModePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewSpacingPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewTextColumnPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewTooltipColumnPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewConvertWidgetToBinWindowCoordsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewCreateDragIconMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewEnableModelDragDestMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewEnableModelDragSourceMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewGetActivateOnSingleClickMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewGetCellRectMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewGetColumnSpacingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewGetColumnsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewGetCursorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewGetDestItemAtPosMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewGetDragDestItemMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewGetItemAtPosMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewGetItemColumnMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewGetItemOrientationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewGetItemPaddingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewGetItemRowMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewGetItemWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewGetMarginMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewGetMarkupColumnMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewGetModelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewGetPathAtPosMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewGetPixbufColumnMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewGetReorderableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewGetRowSpacingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewGetSelectedItemsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewGetSelectionModeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewGetSpacingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewGetTextColumnMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewGetTooltipColumnMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewGetTooltipContextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewGetVisibleRangeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewItemActivatedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewPathIsSelectedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewScrollToPathMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewSelectAllMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewSelectPathMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewSelectedForeachMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewSetActivateOnSingleClickMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewSetColumnSpacingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewSetColumnsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewSetCursorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewSetDragDestItemMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewSetItemOrientationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewSetItemPaddingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewSetItemWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewSetMarginMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewSetMarkupColumnMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewSetModelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewSetPixbufColumnMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewSetReorderableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewSetRowSpacingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewSetSelectionModeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewSetSpacingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewSetTextColumnMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewSetTooltipCellMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewSetTooltipColumnMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewSetTooltipItemMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewUnselectAllMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewUnselectPathMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewUnsetModelDragDestMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconViewUnsetModelDragSourceMethodInfo
#endif
