#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.PageSetup where

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

newtype PageSetup = PageSetup (SP.ManagedPtr PageSetup)
instance SP.ManagedPtrNewtype PageSetup where
instance B.Types.TypedObject PageSetup where
instance B.Types.GObject PageSetup
class (SP.GObject o, O.IsDescendantOf PageSetup o) => IsPageSetup o
instance (SP.GObject o, O.IsDescendantOf PageSetup o) => IsPageSetup o
instance O.HasParentTypes PageSetup
toPageSetup :: (MIO.MonadIO m, IsPageSetup o) => o -> m PageSetup
instance B.GValue.IsGValue (Maybe PageSetup) where
#if defined(ENABLE_OVERLOADING)
data PageSetupCopyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PageSetupGetBottomMarginMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PageSetupGetLeftMarginMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PageSetupGetOrientationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PageSetupGetPageHeightMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PageSetupGetPageWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PageSetupGetPaperHeightMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PageSetupGetPaperSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PageSetupGetPaperWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PageSetupGetRightMarginMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PageSetupGetTopMarginMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PageSetupLoadFileMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PageSetupLoadKeyFileMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PageSetupSetBottomMarginMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PageSetupSetLeftMarginMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PageSetupSetOrientationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PageSetupSetPaperSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PageSetupSetPaperSizeAndDefaultMarginsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PageSetupSetRightMarginMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PageSetupSetTopMarginMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PageSetupToFileMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PageSetupToGvariantMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PageSetupToKeyFileMethodInfo
#endif
