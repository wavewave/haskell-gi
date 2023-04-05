#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Interfaces.Buildable where

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

newtype Buildable = Buildable (SP.ManagedPtr Buildable)
instance SP.ManagedPtrNewtype Buildable where
instance B.Types.TypedObject Buildable where
instance B.Types.GObject Buildable
class (SP.GObject o, O.IsDescendantOf Buildable o) => IsBuildable o
instance (SP.GObject o, O.IsDescendantOf Buildable o) => IsBuildable o
instance O.HasParentTypes Buildable
toBuildable :: (MIO.MonadIO m, IsBuildable o) => o -> m Buildable
instance B.GValue.IsGValue (Maybe Buildable) where
#if defined(ENABLE_OVERLOADING)
data BuildableAddChildMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data BuildableConstructChildMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data BuildableCustomFinishedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data BuildableCustomTagEndMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data BuildableCustomTagStartMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data BuildableGetInternalChildMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data BuildableGetNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data BuildableParserFinishedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data BuildableSetBuildablePropertyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data BuildableSetNameMethodInfo
#endif
