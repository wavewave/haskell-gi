#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.Popover where

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

newtype Popover = Popover (SP.ManagedPtr Popover)
instance SP.ManagedPtrNewtype Popover where
instance B.Types.TypedObject Popover where
instance B.Types.GObject Popover
class (SP.GObject o, O.IsDescendantOf Popover o) => IsPopover o
instance (SP.GObject o, O.IsDescendantOf Popover o) => IsPopover o
instance O.HasParentTypes Popover
toPopover :: (MIO.MonadIO m, IsPopover o) => o -> m Popover
instance B.GValue.IsGValue (Maybe Popover) where
#if defined(ENABLE_OVERLOADING)
data PopoverClosedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PopoverConstrainToPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PopoverModalPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PopoverPointingToPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PopoverPositionPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PopoverRelativeToPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PopoverTransitionsEnabledPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PopoverBindModelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PopoverGetConstrainToMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PopoverGetDefaultWidgetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PopoverGetModalMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PopoverGetPointingToMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PopoverGetPositionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PopoverGetRelativeToMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PopoverGetTransitionsEnabledMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PopoverPopdownMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PopoverPopupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PopoverSetConstrainToMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PopoverSetDefaultWidgetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PopoverSetModalMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PopoverSetPointingToMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PopoverSetPositionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PopoverSetRelativeToMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PopoverSetTransitionsEnabledMethodInfo
#endif
