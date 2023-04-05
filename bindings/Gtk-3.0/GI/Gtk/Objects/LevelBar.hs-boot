#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.LevelBar where

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

newtype LevelBar = LevelBar (SP.ManagedPtr LevelBar)
instance SP.ManagedPtrNewtype LevelBar where
instance B.Types.TypedObject LevelBar where
instance B.Types.GObject LevelBar
class (SP.GObject o, O.IsDescendantOf LevelBar o) => IsLevelBar o
instance (SP.GObject o, O.IsDescendantOf LevelBar o) => IsLevelBar o
instance O.HasParentTypes LevelBar
toLevelBar :: (MIO.MonadIO m, IsLevelBar o) => o -> m LevelBar
instance B.GValue.IsGValue (Maybe LevelBar) where
#if defined(ENABLE_OVERLOADING)
data LevelBarOffsetChangedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LevelBarInvertedPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LevelBarMaxValuePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LevelBarMinValuePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LevelBarModePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LevelBarValuePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LevelBarAddOffsetValueMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LevelBarGetInvertedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LevelBarGetMaxValueMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LevelBarGetMinValueMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LevelBarGetModeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LevelBarGetOffsetValueMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LevelBarGetValueMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LevelBarRemoveOffsetValueMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LevelBarSetInvertedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LevelBarSetMaxValueMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LevelBarSetMinValueMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LevelBarSetModeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LevelBarSetValueMethodInfo
#endif
