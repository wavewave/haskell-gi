#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.AccelGroup where

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

newtype AccelGroup = AccelGroup (SP.ManagedPtr AccelGroup)
instance SP.ManagedPtrNewtype AccelGroup where
instance B.Types.TypedObject AccelGroup where
instance B.Types.GObject AccelGroup
class (SP.GObject o, O.IsDescendantOf AccelGroup o) => IsAccelGroup o
instance (SP.GObject o, O.IsDescendantOf AccelGroup o) => IsAccelGroup o
instance O.HasParentTypes AccelGroup
toAccelGroup :: (MIO.MonadIO m, IsAccelGroup o) => o -> m AccelGroup
instance B.GValue.IsGValue (Maybe AccelGroup) where
#if defined(ENABLE_OVERLOADING)
data AccelGroupAccelActivateSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AccelGroupAccelChangedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AccelGroupIsLockedPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AccelGroupModifierMaskPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AccelGroupActivateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AccelGroupConnectMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AccelGroupConnectByPathMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AccelGroupDisconnectMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AccelGroupDisconnectKeyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AccelGroupFindMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AccelGroupGetIsLockedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AccelGroupGetModifierMaskMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AccelGroupLockMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AccelGroupQueryMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AccelGroupUnlockMethodInfo
#endif
