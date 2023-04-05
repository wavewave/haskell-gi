#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.Revealer where

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

newtype Revealer = Revealer (SP.ManagedPtr Revealer)
instance SP.ManagedPtrNewtype Revealer where
instance B.Types.TypedObject Revealer where
instance B.Types.GObject Revealer
class (SP.GObject o, O.IsDescendantOf Revealer o) => IsRevealer o
instance (SP.GObject o, O.IsDescendantOf Revealer o) => IsRevealer o
instance O.HasParentTypes Revealer
toRevealer :: (MIO.MonadIO m, IsRevealer o) => o -> m Revealer
instance B.GValue.IsGValue (Maybe Revealer) where
#if defined(ENABLE_OVERLOADING)
data RevealerChildRevealedPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RevealerRevealChildPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RevealerTransitionDurationPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RevealerTransitionTypePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RevealerGetChildRevealedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RevealerGetRevealChildMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RevealerGetTransitionDurationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RevealerGetTransitionTypeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RevealerSetRevealChildMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RevealerSetTransitionDurationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RevealerSetTransitionTypeMethodInfo
#endif
