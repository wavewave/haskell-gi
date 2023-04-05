#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.FontSelection where

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

newtype FontSelection = FontSelection (SP.ManagedPtr FontSelection)
instance SP.ManagedPtrNewtype FontSelection where
instance B.Types.TypedObject FontSelection where
instance B.Types.GObject FontSelection
class (SP.GObject o, O.IsDescendantOf FontSelection o) => IsFontSelection o
instance (SP.GObject o, O.IsDescendantOf FontSelection o) => IsFontSelection o
instance O.HasParentTypes FontSelection
toFontSelection :: (MIO.MonadIO m, IsFontSelection o) => o -> m FontSelection
instance B.GValue.IsGValue (Maybe FontSelection) where
#if defined(ENABLE_OVERLOADING)
data FontSelectionFontNamePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontSelectionPreviewTextPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontSelectionGetFaceMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontSelectionGetFaceListMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontSelectionGetFamilyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontSelectionGetFamilyListMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontSelectionGetFontNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontSelectionGetPreviewEntryMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontSelectionGetPreviewTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontSelectionGetSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontSelectionGetSizeEntryMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontSelectionGetSizeListMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontSelectionSetFontNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontSelectionSetPreviewTextMethodInfo
#endif
