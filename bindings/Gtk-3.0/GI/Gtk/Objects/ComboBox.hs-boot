#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.ComboBox where

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

newtype ComboBox = ComboBox (SP.ManagedPtr ComboBox)
instance SP.ManagedPtrNewtype ComboBox where
instance B.Types.TypedObject ComboBox where
instance B.Types.GObject ComboBox
class (SP.GObject o, O.IsDescendantOf ComboBox o) => IsComboBox o
instance (SP.GObject o, O.IsDescendantOf ComboBox o) => IsComboBox o
instance O.HasParentTypes ComboBox
toComboBox :: (MIO.MonadIO m, IsComboBox o) => o -> m ComboBox
instance B.GValue.IsGValue (Maybe ComboBox) where
#if defined(ENABLE_OVERLOADING)
data ComboBoxChangedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxFormatEntryTextSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxMoveActiveSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxPopdownSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxPopupSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxActivePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxActiveIdPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxAddTearoffsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxButtonSensitivityPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxCellAreaPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxColumnSpanColumnPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxEntryTextColumnPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxHasEntryPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxHasFramePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxIdColumnPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxModelPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxPopupFixedWidthPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxPopupShownPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxRowSpanColumnPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxTearoffTitlePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxWrapWidthPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxGetActiveMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxGetActiveIdMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxGetActiveIterMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxGetAddTearoffsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxGetButtonSensitivityMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxGetColumnSpanColumnMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxGetEntryTextColumnMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxGetFocusOnClickMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxGetHasEntryMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxGetIdColumnMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxGetModelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxGetPopupAccessibleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxGetPopupFixedWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxGetRowSpanColumnMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxGetTitleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxGetWrapWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxPopdownMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxPopupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxPopupForDeviceMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxSetActiveMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxSetActiveIdMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxSetActiveIterMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxSetAddTearoffsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxSetButtonSensitivityMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxSetColumnSpanColumnMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxSetEntryTextColumnMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxSetFocusOnClickMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxSetIdColumnMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxSetModelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxSetPopupFixedWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxSetRowSeparatorFuncMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxSetRowSpanColumnMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxSetTitleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ComboBoxSetWrapWidthMethodInfo
#endif
