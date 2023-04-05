#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.InfoBar where

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

newtype InfoBar = InfoBar (SP.ManagedPtr InfoBar)
instance SP.ManagedPtrNewtype InfoBar where
instance B.Types.TypedObject InfoBar where
instance B.Types.GObject InfoBar
class (SP.GObject o, O.IsDescendantOf InfoBar o) => IsInfoBar o
instance (SP.GObject o, O.IsDescendantOf InfoBar o) => IsInfoBar o
instance O.HasParentTypes InfoBar
toInfoBar :: (MIO.MonadIO m, IsInfoBar o) => o -> m InfoBar
instance B.GValue.IsGValue (Maybe InfoBar) where
#if defined(ENABLE_OVERLOADING)
data InfoBarCloseSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data InfoBarResponseSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data InfoBarMessageTypePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data InfoBarRevealedPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data InfoBarShowCloseButtonPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data InfoBarAddActionWidgetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data InfoBarAddButtonMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data InfoBarGetActionAreaMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data InfoBarGetContentAreaMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data InfoBarGetMessageTypeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data InfoBarGetRevealedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data InfoBarGetShowCloseButtonMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data InfoBarResponseMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data InfoBarSetDefaultResponseMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data InfoBarSetMessageTypeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data InfoBarSetResponseSensitiveMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data InfoBarSetRevealedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data InfoBarSetShowCloseButtonMethodInfo
#endif
