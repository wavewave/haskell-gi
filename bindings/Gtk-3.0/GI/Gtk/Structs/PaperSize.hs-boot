#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Structs.PaperSize where

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

newtype PaperSize = PaperSize (SP.ManagedPtr PaperSize)
instance SP.ManagedPtrNewtype PaperSize where
instance O.HasParentTypes PaperSize
instance B.Types.TypedObject PaperSize where
instance B.Types.GBoxed PaperSize
instance B.GValue.IsGValue (Maybe PaperSize) where
#if defined(ENABLE_OVERLOADING)
data PaperSizeCopyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PaperSizeFreeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PaperSizeGetDefaultBottomMarginMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PaperSizeGetDefaultLeftMarginMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PaperSizeGetDefaultRightMarginMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PaperSizeGetDefaultTopMarginMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PaperSizeGetDisplayNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PaperSizeGetHeightMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PaperSizeGetNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PaperSizeGetPpdNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PaperSizeGetWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PaperSizeIsCustomMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PaperSizeIsEqualMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PaperSizeIsIppMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PaperSizeSetSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PaperSizeToGvariantMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PaperSizeToKeyFileMethodInfo
#endif
