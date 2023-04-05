#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.PrintSettings where

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

newtype PrintSettings = PrintSettings (SP.ManagedPtr PrintSettings)
instance SP.ManagedPtrNewtype PrintSettings where
instance B.Types.TypedObject PrintSettings where
instance B.Types.GObject PrintSettings
class (SP.GObject o, O.IsDescendantOf PrintSettings o) => IsPrintSettings o
instance (SP.GObject o, O.IsDescendantOf PrintSettings o) => IsPrintSettings o
instance O.HasParentTypes PrintSettings
toPrintSettings :: (MIO.MonadIO m, IsPrintSettings o) => o -> m PrintSettings
instance B.GValue.IsGValue (Maybe PrintSettings) where
#if defined(ENABLE_OVERLOADING)
data PrintSettingsCopyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsForeachMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetBoolMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetCollateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetDefaultSourceMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetDitherMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetDoubleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetDoubleWithDefaultMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetDuplexMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetFinishingsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetIntMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetIntWithDefaultMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetLengthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetMediaTypeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetNCopiesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetNumberUpMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetNumberUpLayoutMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetOrientationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetOutputBinMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetPageRangesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetPageSetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetPaperHeightMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetPaperSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetPaperWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetPrintPagesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetPrinterMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetPrinterLpiMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetQualityMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetResolutionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetResolutionXMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetResolutionYMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetReverseMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetScaleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsGetUseColorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsHasKeyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsLoadFileMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsLoadKeyFileMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetBoolMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetCollateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetDefaultSourceMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetDitherMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetDoubleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetDuplexMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetFinishingsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetIntMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetLengthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetMediaTypeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetNCopiesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetNumberUpMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetNumberUpLayoutMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetOrientationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetOutputBinMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetPageRangesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetPageSetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetPaperHeightMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetPaperSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetPaperWidthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetPrintPagesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetPrinterMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetPrinterLpiMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetQualityMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetResolutionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetResolutionXyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetReverseMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetScaleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsSetUseColorMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsToFileMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsToGvariantMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsToKeyFileMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data PrintSettingsUnsetMethodInfo
#endif
