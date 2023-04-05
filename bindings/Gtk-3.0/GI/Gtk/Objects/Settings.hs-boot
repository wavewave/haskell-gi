#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.Settings where

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

newtype Settings = Settings (SP.ManagedPtr Settings)
instance SP.ManagedPtrNewtype Settings where
instance B.Types.TypedObject Settings where
instance B.Types.GObject Settings
class (SP.GObject o, O.IsDescendantOf Settings o) => IsSettings o
instance (SP.GObject o, O.IsDescendantOf Settings o) => IsSettings o
instance O.HasParentTypes Settings
toSettings :: (MIO.MonadIO m, IsSettings o) => o -> m Settings
instance B.GValue.IsGValue (Maybe Settings) where
#if defined(ENABLE_OVERLOADING)
data SettingsColorHashPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkAlternativeButtonOrderPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkAlternativeSortArrowsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkApplicationPreferDarkThemePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkAutoMnemonicsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkButtonImagesPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkCanChangeAccelsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkColorPalettePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkColorSchemePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkCursorAspectRatioPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkCursorBlinkPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkCursorBlinkTimePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkCursorBlinkTimeoutPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkCursorThemeNamePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkCursorThemeSizePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkDecorationLayoutPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkDialogsUseHeaderPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkDndDragThresholdPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkDoubleClickDistancePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkDoubleClickTimePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkEnableAccelsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkEnableAnimationsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkEnableEventSoundsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkEnableInputFeedbackSoundsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkEnableMnemonicsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkEnablePrimaryPastePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkEnableTooltipsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkEntryPasswordHintTimeoutPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkEntrySelectOnFocusPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkErrorBellPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkFallbackIconThemePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkFileChooserBackendPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkFontNamePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkFontconfigTimestampPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkIconSizesPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkIconThemeNamePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkImModulePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkImPreeditStylePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkImStatusStylePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkKeyThemeNamePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkKeynavCursorOnlyPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkKeynavUseCaretPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkKeynavWrapAroundPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkLabelSelectOnFocusPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkLongPressTimePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkMenuBarAccelPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkMenuBarPopupDelayPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkMenuImagesPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkMenuPopdownDelayPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkMenuPopupDelayPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkModulesPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkOverlayScrollingPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkPrimaryButtonWarpsSliderPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkPrintBackendsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkPrintPreviewCommandPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkRecentFilesEnabledPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkRecentFilesLimitPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkRecentFilesMaxAgePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkScrolledWindowPlacementPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkShellShowsAppMenuPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkShellShowsDesktopPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkShellShowsMenubarPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkShowInputMethodMenuPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkShowUnicodeMenuPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkSoundThemeNamePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkSplitCursorPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkThemeNamePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkTimeoutExpandPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkTimeoutInitialPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkTimeoutRepeatPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkTitlebarDoubleClickPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkTitlebarMiddleClickPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkTitlebarRightClickPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkToolbarIconSizePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkToolbarStylePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkTooltipBrowseModeTimeoutPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkTooltipBrowseTimeoutPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkTooltipTimeoutPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkTouchscreenModePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkVisibleFocusPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkXftAntialiasPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkXftDpiPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkXftHintingPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkXftHintstylePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsGtkXftRgbaPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsResetPropertyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsSetDoublePropertyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsSetLongPropertyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsSetPropertyValueMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SettingsSetStringPropertyMethodInfo
#endif
