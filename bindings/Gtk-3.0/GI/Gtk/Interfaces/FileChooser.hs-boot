#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Interfaces.FileChooser where

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

newtype FileChooser = FileChooser (SP.ManagedPtr FileChooser)
instance SP.ManagedPtrNewtype FileChooser where
instance B.Types.TypedObject FileChooser where
instance B.Types.GObject FileChooser
class (SP.GObject o, O.IsDescendantOf FileChooser o) => IsFileChooser o
instance (SP.GObject o, O.IsDescendantOf FileChooser o) => IsFileChooser o
instance O.HasParentTypes FileChooser
toFileChooser :: (MIO.MonadIO m, IsFileChooser o) => o -> m FileChooser
instance B.GValue.IsGValue (Maybe FileChooser) where
#if defined(ENABLE_OVERLOADING)
data FileChooserActionPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserCreateFoldersPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserDoOverwriteConfirmationPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserExtraWidgetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserFilterPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserLocalOnlyPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserPreviewWidgetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserPreviewWidgetActivePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserSelectMultiplePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserShowHiddenPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserUsePreviewLabelPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserAddChoiceMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserAddFilterMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserAddShortcutFolderMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserAddShortcutFolderUriMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserGetActionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserGetChoiceMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserGetCreateFoldersMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserGetCurrentFolderMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserGetCurrentFolderFileMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserGetCurrentFolderUriMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserGetCurrentNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserGetDoOverwriteConfirmationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserGetExtraWidgetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserGetFileMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserGetFilenameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserGetFilenamesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserGetFilesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserGetFilterMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserGetLocalOnlyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserGetPreviewFileMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserGetPreviewFilenameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserGetPreviewUriMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserGetPreviewWidgetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserGetPreviewWidgetActiveMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserGetSelectMultipleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserGetShowHiddenMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserGetUriMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserGetUrisMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserGetUsePreviewLabelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserListFiltersMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserListShortcutFolderUrisMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserListShortcutFoldersMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserRemoveChoiceMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserRemoveFilterMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserRemoveShortcutFolderMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserRemoveShortcutFolderUriMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserSelectAllMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserSelectFileMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserSelectFilenameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserSelectUriMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserSetActionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserSetChoiceMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserSetCreateFoldersMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserSetCurrentFolderMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserSetCurrentFolderFileMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserSetCurrentFolderUriMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserSetCurrentNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserSetDoOverwriteConfirmationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserSetExtraWidgetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserSetFileMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserSetFilenameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserSetFilterMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserSetLocalOnlyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserSetPreviewWidgetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserSetPreviewWidgetActiveMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserSetSelectMultipleMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserSetShowHiddenMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserSetUriMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserSetUsePreviewLabelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserUnselectAllMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserUnselectFileMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserUnselectFilenameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserUnselectUriMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserConfirmOverwriteSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserCurrentFolderChangedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserFileActivatedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserSelectionChangedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data FileChooserUpdatePreviewSignalInfo
#endif
