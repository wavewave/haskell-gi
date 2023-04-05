#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.StatusIcon where

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

newtype StatusIcon = StatusIcon (SP.ManagedPtr StatusIcon)
instance SP.ManagedPtrNewtype StatusIcon where
instance B.Types.TypedObject StatusIcon where
instance B.Types.GObject StatusIcon
class (SP.GObject o, O.IsDescendantOf StatusIcon o) => IsStatusIcon o
instance (SP.GObject o, O.IsDescendantOf StatusIcon o) => IsStatusIcon o
instance O.HasParentTypes StatusIcon
toStatusIcon :: (MIO.MonadIO m, IsStatusIcon o) => o -> m StatusIcon
instance B.GValue.IsGValue (Maybe StatusIcon) where
#if defined(ENABLE_OVERLOADING)
data StatusIconActivateSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconButtonPressEventSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconButtonReleaseEventSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconPopupMenuSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconQueryTooltipSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconScrollEventSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconSizeChangedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconEmbeddedPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconFilePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconGiconPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconHasTooltipPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconIconNamePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconOrientationPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconPixbufPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconScreenPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconSizePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconStockPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconStorageTypePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconTitlePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconTooltipMarkupPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconTooltipTextPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconVisiblePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconGetGeometryMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconGetGiconMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconGetHasTooltipMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconGetIconNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconGetPixbufMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconGetScreenMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconGetSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconGetStockMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconGetStorageTypeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconGetTitleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconGetTooltipMarkupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconGetTooltipTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconGetVisibleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconGetX11WindowIdMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconIsEmbeddedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconSetFromFileMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconSetFromGiconMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconSetFromIconNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconSetFromPixbufMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconSetFromStockMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconSetHasTooltipMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconSetNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconSetScreenMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconSetTitleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconSetTooltipMarkupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconSetTooltipTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StatusIconSetVisibleMethodInfo
#endif
