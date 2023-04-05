#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Interfaces.FontChooser where

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

newtype FontChooser = FontChooser (SP.ManagedPtr FontChooser)
instance SP.ManagedPtrNewtype FontChooser where
instance B.Types.TypedObject FontChooser where
instance B.Types.GObject FontChooser
class (SP.GObject o, O.IsDescendantOf FontChooser o) => IsFontChooser o
instance (SP.GObject o, O.IsDescendantOf FontChooser o) => IsFontChooser o
instance O.HasParentTypes FontChooser
toFontChooser :: (MIO.MonadIO m, IsFontChooser o) => o -> m FontChooser
instance B.GValue.IsGValue (Maybe FontChooser) where
#if defined(ENABLE_OVERLOADING)
data FontChooserFontPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontChooserFontDescPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontChooserFontFeaturesPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontChooserLanguagePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontChooserLevelPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontChooserPreviewTextPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontChooserShowPreviewEntryPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontChooserGetFontMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontChooserGetFontDescMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontChooserGetFontFaceMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontChooserGetFontFamilyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontChooserGetFontFeaturesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontChooserGetFontMapMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontChooserGetFontSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontChooserGetLanguageMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontChooserGetLevelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontChooserGetPreviewTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontChooserGetShowPreviewEntryMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontChooserSetFilterFuncMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontChooserSetFontMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontChooserSetFontDescMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontChooserSetFontMapMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontChooserSetLanguageMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontChooserSetLevelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontChooserSetPreviewTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontChooserSetShowPreviewEntryMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontChooserFontActivatedSignalInfo
#endif
