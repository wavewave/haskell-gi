#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.LinkButton where

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

newtype LinkButton = LinkButton (SP.ManagedPtr LinkButton)
instance SP.ManagedPtrNewtype LinkButton where
instance B.Types.TypedObject LinkButton where
instance B.Types.GObject LinkButton
class (SP.GObject o, O.IsDescendantOf LinkButton o) => IsLinkButton o
instance (SP.GObject o, O.IsDescendantOf LinkButton o) => IsLinkButton o
instance O.HasParentTypes LinkButton
toLinkButton :: (MIO.MonadIO m, IsLinkButton o) => o -> m LinkButton
instance B.GValue.IsGValue (Maybe LinkButton) where
#if defined(ENABLE_OVERLOADING)
data LinkButtonActivateLinkSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LinkButtonUriPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LinkButtonVisitedPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LinkButtonGetUriMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LinkButtonGetVisitedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LinkButtonSetUriMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data LinkButtonSetVisitedMethodInfo
#endif
