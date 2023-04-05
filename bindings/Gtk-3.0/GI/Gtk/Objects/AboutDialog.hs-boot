#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.AboutDialog where

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

newtype AboutDialog = AboutDialog (SP.ManagedPtr AboutDialog)
instance SP.ManagedPtrNewtype AboutDialog where
instance B.Types.TypedObject AboutDialog where
instance B.Types.GObject AboutDialog
class (SP.GObject o, O.IsDescendantOf AboutDialog o) => IsAboutDialog o
instance (SP.GObject o, O.IsDescendantOf AboutDialog o) => IsAboutDialog o
instance O.HasParentTypes AboutDialog
toAboutDialog :: (MIO.MonadIO m, IsAboutDialog o) => o -> m AboutDialog
instance B.GValue.IsGValue (Maybe AboutDialog) where
#if defined(ENABLE_OVERLOADING)
data AboutDialogActivateLinkSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogArtistsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogAuthorsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogCommentsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogCopyrightPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogDocumentersPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogLicensePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogLicenseTypePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogLogoPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogLogoIconNamePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogProgramNamePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogTranslatorCreditsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogVersionPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogWebsitePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogWebsiteLabelPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogWrapLicensePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogAddCreditSectionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogGetArtistsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogGetAuthorsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogGetCommentsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogGetCopyrightMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogGetDocumentersMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogGetLicenseMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogGetLicenseTypeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogGetLogoMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogGetLogoIconNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogGetProgramNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogGetTranslatorCreditsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogGetVersionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogGetWebsiteMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogGetWebsiteLabelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogGetWrapLicenseMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogSetArtistsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogSetAuthorsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogSetCommentsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogSetCopyrightMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogSetDocumentersMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogSetLicenseMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogSetLicenseTypeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogSetLogoMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogSetLogoIconNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogSetProgramNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogSetTranslatorCreditsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogSetVersionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogSetWebsiteMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogSetWebsiteLabelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data AboutDialogSetWrapLicenseMethodInfo
#endif
