#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.ToolPalette where

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

newtype ToolPalette = ToolPalette (SP.ManagedPtr ToolPalette)
instance SP.ManagedPtrNewtype ToolPalette where
instance B.Types.TypedObject ToolPalette where
instance B.Types.GObject ToolPalette
class (SP.GObject o, O.IsDescendantOf ToolPalette o) => IsToolPalette o
instance (SP.GObject o, O.IsDescendantOf ToolPalette o) => IsToolPalette o
instance O.HasParentTypes ToolPalette
toToolPalette :: (MIO.MonadIO m, IsToolPalette o) => o -> m ToolPalette
instance B.GValue.IsGValue (Maybe ToolPalette) where
#if defined(ENABLE_OVERLOADING)
data ToolPaletteIconSizePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolPaletteIconSizeSetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolPaletteToolbarStylePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolPaletteAddDragDestMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolPaletteGetDragItemMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolPaletteGetDropGroupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolPaletteGetDropItemMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolPaletteGetExclusiveMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolPaletteGetExpandMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolPaletteGetGroupPositionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolPaletteGetHadjustmentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolPaletteGetIconSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolPaletteGetStyleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolPaletteGetVadjustmentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolPaletteSetDragSourceMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolPaletteSetExclusiveMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolPaletteSetExpandMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolPaletteSetGroupPositionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolPaletteSetIconSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolPaletteSetStyleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolPaletteUnsetIconSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ToolPaletteUnsetStyleMethodInfo
#endif
