#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.CellRendererToggle where

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

newtype CellRendererToggle = CellRendererToggle (SP.ManagedPtr CellRendererToggle)
instance SP.ManagedPtrNewtype CellRendererToggle where
instance B.Types.TypedObject CellRendererToggle where
instance B.Types.GObject CellRendererToggle
class (SP.GObject o, O.IsDescendantOf CellRendererToggle o) => IsCellRendererToggle o
instance (SP.GObject o, O.IsDescendantOf CellRendererToggle o) => IsCellRendererToggle o
instance O.HasParentTypes CellRendererToggle
toCellRendererToggle :: (MIO.MonadIO m, IsCellRendererToggle o) => o -> m CellRendererToggle
instance B.GValue.IsGValue (Maybe CellRendererToggle) where
#if defined(ENABLE_OVERLOADING)
data CellRendererToggleToggledSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererToggleActivatablePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererToggleActivePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererToggleInconsistentPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererToggleIndicatorSizePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererToggleRadioPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererToggleGetActivatableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererToggleGetActiveMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererToggleGetRadioMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererToggleSetActivatableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererToggleSetActiveMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data CellRendererToggleSetRadioMethodInfo
#endif
