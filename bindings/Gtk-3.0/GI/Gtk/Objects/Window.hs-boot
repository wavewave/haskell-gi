#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.Window where

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

newtype Window = Window (SP.ManagedPtr Window)
instance SP.ManagedPtrNewtype Window where
instance B.Types.TypedObject Window where
instance B.Types.GObject Window
class (SP.GObject o, O.IsDescendantOf Window o) => IsWindow o
instance (SP.GObject o, O.IsDescendantOf Window o) => IsWindow o
instance O.HasParentTypes Window
toWindow :: (MIO.MonadIO m, IsWindow o) => o -> m Window
instance B.GValue.IsGValue (Maybe Window) where
#if defined(ENABLE_OVERLOADING)
data WindowActivateDefaultSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowActivateFocusSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowEnableDebuggingSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowKeysChangedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetFocusSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowAcceptFocusPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowApplicationPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowAttachedToPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowDecoratedPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowDefaultHeightPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowDefaultWidthPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowDeletablePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowDestroyWithParentPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowFocusOnMapPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowFocusVisiblePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGravityPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowHasResizeGripPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowHasToplevelFocusPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowHideTitlebarWhenMaximizedPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowIconPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowIconNamePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowIsActivePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowIsMaximizedPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowMnemonicsVisiblePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowModalPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowResizablePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowResizeGripVisiblePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowRolePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowScreenPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSkipPagerHintPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSkipTaskbarHintPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowStartupIdPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowTitlePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowTransientForPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowTypePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowTypeHintPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowUrgencyHintPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowWindowPositionPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowActivateDefaultMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowActivateFocusMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowActivateKeyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowAddAccelGroupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowAddMnemonicMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowBeginMoveDragMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowBeginResizeDragMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowCloseMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowDeiconifyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowFullscreenMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowFullscreenOnMonitorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetAcceptFocusMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetApplicationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetAttachedToMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetDecoratedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetDefaultSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetDefaultWidgetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetDeletableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetDestroyWithParentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetFocusMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetFocusOnMapMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetFocusVisibleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetGravityMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetGroupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetHasResizeGripMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetHideTitlebarWhenMaximizedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetIconMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetIconListMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetIconNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetMnemonicModifierMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetMnemonicsVisibleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetModalMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetOpacityMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetPositionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetResizableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetResizeGripAreaMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetRoleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetScreenMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetSkipPagerHintMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetSkipTaskbarHintMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetTitleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetTitlebarMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetTransientForMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetTypeHintMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetUrgencyHintMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowGetWindowTypeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowHasGroupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowHasToplevelFocusMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowIconifyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowIsActiveMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowIsMaximizedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowMaximizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowMnemonicActivateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowMoveMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowParseGeometryMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowPresentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowPresentWithTimeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowPropagateKeyEventMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowRemoveAccelGroupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowRemoveMnemonicMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowReshowWithInitialSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowResizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowResizeGripIsVisibleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowResizeToGeometryMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetAcceptFocusMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetApplicationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetAttachedToMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetDecoratedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetDefaultMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetDefaultGeometryMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetDefaultSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetDeletableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetDestroyWithParentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetFocusMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetFocusOnMapMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetFocusVisibleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetGeometryHintsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetGravityMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetHasResizeGripMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetHasUserRefCountMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetHideTitlebarWhenMaximizedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetIconMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetIconFromFileMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetIconListMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetIconNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetKeepAboveMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetKeepBelowMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetMnemonicModifierMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetMnemonicsVisibleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetModalMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetOpacityMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetPositionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetResizableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetRoleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetScreenMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetSkipPagerHintMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetSkipTaskbarHintMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetStartupIdMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetTitleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetTitlebarMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetTransientForMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetTypeHintMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetUrgencyHintMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowSetWmclassMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowStickMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowUnfullscreenMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowUnmaximizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data WindowUnstickMethodInfo
#endif
