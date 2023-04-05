#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.CellArea where

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

newtype CellArea = CellArea (SP.ManagedPtr CellArea)
instance SP.ManagedPtrNewtype CellArea where
instance B.Types.TypedObject CellArea where
instance B.Types.GObject CellArea
class (SP.GObject o, O.IsDescendantOf CellArea o) => IsCellArea o
instance (SP.GObject o, O.IsDescendantOf CellArea o) => IsCellArea o
instance O.HasParentTypes CellArea
toCellArea :: (MIO.MonadIO m, IsCellArea o) => o -> m CellArea
instance B.GValue.IsGValue (Maybe CellArea) where
#if defined(ENABLE_OVERLOADING)
data CellAreaAddEditableSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaApplyAttributesSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaFocusChangedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaRemoveEditableSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaEditWidgetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaEditedCellPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaFocusCellPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaActivateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaActivateCellMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaAddMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaAddFocusSiblingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaApplyAttributesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaAttributeConnectMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaAttributeDisconnectMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaAttributeGetColumnMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaCellGetPropertyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaCellSetPropertyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaCopyContextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaCreateContextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaEventMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaFocusMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaForeachMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaForeachAllocMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaGetCellAllocationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaGetCellAtPositionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaGetCurrentPathStringMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaGetEditWidgetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaGetEditedCellMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaGetFocusCellMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaGetFocusFromSiblingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaGetFocusSiblingsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaGetPreferredHeightMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaGetPreferredHeightForWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaGetPreferredWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaGetPreferredWidthForHeightMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaGetRequestModeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaHasRendererMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaInnerCellAreaMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaIsActivatableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaIsFocusSiblingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaRemoveMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaRemoveFocusSiblingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaRenderMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaRequestRendererMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaSetFocusCellMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellAreaStopEditingMethodInfo
#endif
