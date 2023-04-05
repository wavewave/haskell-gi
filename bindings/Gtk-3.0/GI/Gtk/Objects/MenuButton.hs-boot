#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.MenuButton where

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

newtype MenuButton = MenuButton (SP.ManagedPtr MenuButton)
instance SP.ManagedPtrNewtype MenuButton where
instance B.Types.TypedObject MenuButton where
instance B.Types.GObject MenuButton
class (SP.GObject o, O.IsDescendantOf MenuButton o) => IsMenuButton o
instance (SP.GObject o, O.IsDescendantOf MenuButton o) => IsMenuButton o
instance O.HasParentTypes MenuButton
toMenuButton :: (MIO.MonadIO m, IsMenuButton o) => o -> m MenuButton
instance B.GValue.IsGValue (Maybe MenuButton) where
#if defined(ENABLE_OVERLOADING)
data MenuButtonAlignWidgetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuButtonDirectionPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuButtonMenuModelPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuButtonPopoverPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuButtonPopupPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuButtonUsePopoverPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuButtonGetAlignWidgetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuButtonGetDirectionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuButtonGetMenuModelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuButtonGetPopoverMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuButtonGetPopupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuButtonGetUsePopoverMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuButtonSetAlignWidgetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuButtonSetDirectionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuButtonSetMenuModelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuButtonSetPopoverMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuButtonSetPopupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data MenuButtonSetUsePopoverMethodInfo
#endif
