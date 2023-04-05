#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.CellRenderer where

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

newtype CellRenderer = CellRenderer (SP.ManagedPtr CellRenderer)
instance SP.ManagedPtrNewtype CellRenderer where
instance B.Types.TypedObject CellRenderer where
instance B.Types.GObject CellRenderer
class (SP.GObject o, O.IsDescendantOf CellRenderer o) => IsCellRenderer o
instance (SP.GObject o, O.IsDescendantOf CellRenderer o) => IsCellRenderer o
instance O.HasParentTypes CellRenderer
toCellRenderer :: (MIO.MonadIO m, IsCellRenderer o) => o -> m CellRenderer
instance B.GValue.IsGValue (Maybe CellRenderer) where
#if defined(ENABLE_OVERLOADING)
data CellRendererEditingCanceledSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererEditingStartedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererCellBackgroundPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererCellBackgroundGdkPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererCellBackgroundRgbaPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererCellBackgroundSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererEditingPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererHeightPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererIsExpandedPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererIsExpanderPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererModePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererSensitivePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererVisiblePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererWidthPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererXalignPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererXpadPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererYalignPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererYpadPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererActivateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererGetAlignedAreaMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererGetAlignmentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererGetFixedSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererGetPaddingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererGetPreferredHeightMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererGetPreferredHeightForWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererGetPreferredSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererGetPreferredWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererGetPreferredWidthForHeightMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererGetRequestModeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererGetSensitiveMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererGetSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererGetStateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererGetVisibleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererIsActivatableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererRenderMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererSetAlignmentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererSetFixedSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererSetPaddingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererSetSensitiveMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererSetVisibleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererStartEditingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererStopEditingMethodInfo
#endif
