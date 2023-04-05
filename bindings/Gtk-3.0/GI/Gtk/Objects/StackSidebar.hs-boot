#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.StackSidebar where

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

newtype StackSidebar = StackSidebar (SP.ManagedPtr StackSidebar)
instance SP.ManagedPtrNewtype StackSidebar where
instance B.Types.TypedObject StackSidebar where
instance B.Types.GObject StackSidebar
class (SP.GObject o, O.IsDescendantOf StackSidebar o) => IsStackSidebar o
instance (SP.GObject o, O.IsDescendantOf StackSidebar o) => IsStackSidebar o
instance O.HasParentTypes StackSidebar
toStackSidebar :: (MIO.MonadIO m, IsStackSidebar o) => o -> m StackSidebar
instance B.GValue.IsGValue (Maybe StackSidebar) where
#if defined(ENABLE_OVERLOADING)
data StackSidebarStackPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StackSidebarGetStackMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data StackSidebarSetStackMethodInfo
#endif
