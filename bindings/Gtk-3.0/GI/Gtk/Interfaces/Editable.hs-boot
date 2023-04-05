#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Interfaces.Editable where

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

newtype Editable = Editable (SP.ManagedPtr Editable)
instance SP.ManagedPtrNewtype Editable where
instance B.Types.TypedObject Editable where
instance B.Types.GObject Editable
class (SP.GObject o, O.IsDescendantOf Editable o) => IsEditable o
instance (SP.GObject o, O.IsDescendantOf Editable o) => IsEditable o
instance O.HasParentTypes Editable
toEditable :: (MIO.MonadIO m, IsEditable o) => o -> m Editable
instance B.GValue.IsGValue (Maybe Editable) where
#if defined(ENABLE_OVERLOADING)
data EditableCopyClipboardMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EditableCutClipboardMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EditableDeleteSelectionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EditableDeleteTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EditableGetCharsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EditableGetEditableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EditableGetPositionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EditableGetSelectionBoundsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EditableInsertTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EditablePasteClipboardMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EditableSelectRegionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EditableSetEditableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EditableSetPositionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EditableChangedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EditableDeleteTextSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data EditableInsertTextSignalInfo
#endif
