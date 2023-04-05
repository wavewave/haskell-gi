#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.Widget where

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

newtype Widget = Widget (SP.ManagedPtr Widget)
instance SP.ManagedPtrNewtype Widget where
instance B.Types.TypedObject Widget where
instance B.Types.GObject Widget
class (SP.GObject o, O.IsDescendantOf Widget o) => IsWidget o
instance (SP.GObject o, O.IsDescendantOf Widget o) => IsWidget o
instance O.HasParentTypes Widget
toWidget :: (MIO.MonadIO m, IsWidget o) => o -> m Widget
instance B.GValue.IsGValue (Maybe Widget) where
#if defined(ENABLE_OVERLOADING)
data WidgetAccelClosuresChangedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetButtonPressEventSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetButtonReleaseEventSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetCanActivateAccelSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetChildNotifySignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetCompositedChangedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetConfigureEventSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDamageEventSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDeleteEventSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDestroySignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDestroyEventSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDirectionChangedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragBeginSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragDataDeleteSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragDataGetSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragDataReceivedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragDropSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragEndSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragFailedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragLeaveSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragMotionSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDrawSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetEnterNotifyEventSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetEventSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetEventAfterSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetFocusSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetFocusInEventSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetFocusOutEventSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGrabBrokenEventSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGrabFocusSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGrabNotifySignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetHideSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetHierarchyChangedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetKeyPressEventSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetKeyReleaseEventSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetKeynavFailedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetLeaveNotifyEventSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetMapSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetMapEventSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetMnemonicActivateSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetMotionNotifyEventSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetMoveFocusSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetParentSetSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPopupMenuSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPropertyNotifyEventSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetProximityInEventSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetProximityOutEventSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetQueryTooltipSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetRealizeSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetScreenChangedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetScrollEventSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSelectionClearEventSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSelectionGetSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSelectionNotifyEventSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSelectionReceivedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSelectionRequestEventSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetShowSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetShowHelpSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSizeAllocateSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetStateChangedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetStateFlagsChangedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetStyleSetSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetStyleUpdatedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetTouchEventSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetUnmapSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetUnmapEventSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetUnrealizeSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetVisibilityNotifyEventSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetWindowStateEventSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetAppPaintablePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetCanDefaultPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetCanFocusPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetCompositeChildPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDoubleBufferedPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetEventsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetExpandPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetFocusOnClickPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetHalignPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetHasDefaultPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetHasFocusPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetHasTooltipPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetHeightRequestPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetHexpandPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetHexpandSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetIsFocusPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetMarginPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetMarginBottomPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetMarginEndPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetMarginLeftPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetMarginRightPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetMarginStartPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetMarginTopPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetNamePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetNoShowAllPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetOpacityPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetParentPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetReceivesDefaultPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetScaleFactorPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSensitivePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetStylePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetTooltipMarkupPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetTooltipTextPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetValignPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetVexpandPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetVexpandSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetVisiblePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetWidthRequestPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetWindowPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetActivateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetAddAcceleratorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetAddDeviceEventsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetAddEventsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetAddMnemonicLabelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetAddTickCallbackMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetCanActivateAccelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetChildFocusMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetChildNotifyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetClassPathMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetComputeExpandMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetCreatePangoContextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetCreatePangoLayoutMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDestroyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDestroyedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDeviceIsShadowedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragBeginMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragBeginWithCoordinatesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragCheckThresholdMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragDestAddImageTargetsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragDestAddTextTargetsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragDestAddUriTargetsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragDestFindTargetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragDestGetTargetListMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragDestGetTrackMotionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragDestSetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragDestSetProxyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragDestSetTargetListMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragDestSetTrackMotionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragDestUnsetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragGetDataMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragHighlightMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragSourceAddImageTargetsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragSourceAddTextTargetsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragSourceAddUriTargetsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragSourceGetTargetListMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragSourceSetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragSourceSetIconGiconMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragSourceSetIconNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragSourceSetIconPixbufMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragSourceSetIconStockMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragSourceSetTargetListMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragSourceUnsetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDragUnhighlightMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetDrawMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetEnsureStyleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetErrorBellMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetEventMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetFreezeChildNotifyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetAccessibleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetActionGroupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetAllocatedBaselineMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetAllocatedHeightMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetAllocatedSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetAllocatedWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetAllocationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetAncestorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetAppPaintableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetCanDefaultMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetCanFocusMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetChildRequisitionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetChildVisibleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetClipMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetClipboardMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetCompositeNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetDeviceEnabledMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetDeviceEventsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetDirectionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetDisplayMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetDoubleBufferedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetEventsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetFocusOnClickMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetFontMapMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetFontOptionsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetFrameClockMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetHalignMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetHasTooltipMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetHasWindowMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetHexpandMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetHexpandSetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetMappedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetMarginBottomMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetMarginEndMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetMarginLeftMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetMarginRightMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetMarginStartMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetMarginTopMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetModifierMaskMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetModifierStyleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetNoShowAllMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetOpacityMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetPangoContextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetParentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetParentWindowMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetPathMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetPointerMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetPreferredHeightMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetPreferredHeightForWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetPreferredSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetPreferredWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetPreferredWidthForHeightMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetRealizedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetReceivesDefaultMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetRequestModeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetRequisitionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetRootWindowMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetScaleFactorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetScreenMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetSensitiveMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetSettingsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetSizeRequestMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetStateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetStateFlagsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetStyleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetStyleContextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetSupportMultideviceMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetTemplateChildMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetTooltipMarkupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetTooltipTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetTooltipWindowMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetToplevelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetValignMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetValignWithBaselineMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetVexpandMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetVexpandSetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetVisibleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetVisualMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGetWindowMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGrabAddMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGrabDefaultMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGrabFocusMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetGrabRemoveMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetHasDefaultMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetHasFocusMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetHasGrabMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetHasRcStyleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetHasScreenMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetHasVisibleFocusMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetHideMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetHideOnDeleteMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetInDestructionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetInitTemplateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetInputShapeCombineRegionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetInsertActionGroupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetIntersectMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetIsAncestorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetIsCompositedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetIsDrawableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetIsFocusMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetIsSensitiveMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetIsToplevelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetIsVisibleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetKeynavFailedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetListAccelClosuresMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetListActionPrefixesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetListMnemonicLabelsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetMapMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetMnemonicActivateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetModifyBaseMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetModifyBgMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetModifyCursorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetModifyFgMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetModifyFontMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetModifyStyleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetModifyTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetOverrideBackgroundColorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetOverrideColorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetOverrideCursorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetOverrideFontMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetOverrideSymbolicColorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetPathMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetQueueAllocateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetQueueComputeExpandMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetQueueDrawMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetQueueDrawAreaMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetQueueDrawRegionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetQueueResizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetQueueResizeNoRedrawMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetRealizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetRegionIntersectMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetRegisterWindowMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetRemoveAcceleratorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetRemoveMnemonicLabelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetRemoveTickCallbackMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetRenderIconMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetRenderIconPixbufMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetReparentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetResetRcStylesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetResetStyleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSendExposeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSendFocusChangeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetAccelPathMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetAllocationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetAppPaintableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetCanDefaultMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetCanFocusMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetChildVisibleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetClipMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetCompositeNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetDeviceEnabledMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetDeviceEventsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetDirectionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetDoubleBufferedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetEventsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetFocusOnClickMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetFontMapMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetFontOptionsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetHalignMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetHasTooltipMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetHasWindowMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetHexpandMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetHexpandSetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetMappedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetMarginBottomMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetMarginEndMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetMarginLeftMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetMarginRightMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetMarginStartMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetMarginTopMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetNoShowAllMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetOpacityMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetParentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetParentWindowMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetRealizedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetReceivesDefaultMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetRedrawOnAllocateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetSensitiveMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetSizeRequestMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetStateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetStateFlagsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetStyleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetSupportMultideviceMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetTooltipMarkupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetTooltipTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetTooltipWindowMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetValignMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetVexpandMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetVexpandSetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetVisibleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetVisualMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSetWindowMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetShapeCombineRegionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetShowMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetShowAllMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetShowNowMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSizeAllocateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSizeAllocateWithBaselineMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetSizeRequestMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetStyleAttachMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetStyleGetPropertyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetThawChildNotifyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetTranslateCoordinatesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetTriggerTooltipQueryMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetUnmapMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetUnparentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetUnrealizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetUnregisterWindowMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WidgetUnsetStateFlagsMethodInfo
#endif
