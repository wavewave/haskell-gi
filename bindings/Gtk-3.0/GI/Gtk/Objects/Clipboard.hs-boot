#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.Clipboard where

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

newtype Clipboard = Clipboard (SP.ManagedPtr Clipboard)
instance SP.ManagedPtrNewtype Clipboard where
instance B.Types.TypedObject Clipboard where
instance B.Types.GObject Clipboard
class (SP.GObject o, O.IsDescendantOf Clipboard o) => IsClipboard o
instance (SP.GObject o, O.IsDescendantOf Clipboard o) => IsClipboard o
instance O.HasParentTypes Clipboard
toClipboard :: (MIO.MonadIO m, IsClipboard o) => o -> m Clipboard
instance B.GValue.IsGValue (Maybe Clipboard) where
#if defined(ENABLE_OVERLOADING)
data ClipboardOwnerChangeSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ClipboardClearMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ClipboardGetDisplayMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ClipboardGetOwnerMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ClipboardRequestContentsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ClipboardRequestImageMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ClipboardRequestRichTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ClipboardRequestTargetsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ClipboardRequestTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ClipboardRequestUrisMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ClipboardSetCanStoreMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ClipboardSetImageMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ClipboardSetTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ClipboardStoreMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ClipboardWaitForContentsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ClipboardWaitForImageMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ClipboardWaitForRichTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ClipboardWaitForTargetsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ClipboardWaitForTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ClipboardWaitForUrisMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ClipboardWaitIsImageAvailableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ClipboardWaitIsRichTextAvailableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ClipboardWaitIsTargetAvailableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ClipboardWaitIsTextAvailableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ClipboardWaitIsUrisAvailableMethodInfo
#endif
