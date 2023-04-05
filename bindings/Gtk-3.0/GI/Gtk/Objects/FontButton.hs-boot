#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.FontButton where

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

newtype FontButton = FontButton (SP.ManagedPtr FontButton)
instance SP.ManagedPtrNewtype FontButton where
instance B.Types.TypedObject FontButton where
instance B.Types.GObject FontButton
class (SP.GObject o, O.IsDescendantOf FontButton o) => IsFontButton o
instance (SP.GObject o, O.IsDescendantOf FontButton o) => IsFontButton o
instance O.HasParentTypes FontButton
toFontButton :: (MIO.MonadIO m, IsFontButton o) => o -> m FontButton
instance B.GValue.IsGValue (Maybe FontButton) where
#if defined(ENABLE_OVERLOADING)
data FontButtonFontSetSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontButtonFontNamePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontButtonShowSizePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontButtonShowStylePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontButtonTitlePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontButtonUseFontPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontButtonUseSizePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontButtonGetFontNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontButtonGetShowSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontButtonGetShowStyleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontButtonGetTitleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontButtonGetUseFontMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontButtonGetUseSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontButtonSetFontNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontButtonSetShowSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontButtonSetShowStyleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontButtonSetTitleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontButtonSetUseFontMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FontButtonSetUseSizeMethodInfo
#endif
