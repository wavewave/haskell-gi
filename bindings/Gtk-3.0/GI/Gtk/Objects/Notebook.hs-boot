#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.Notebook where

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

newtype Notebook = Notebook (SP.ManagedPtr Notebook)
instance SP.ManagedPtrNewtype Notebook where
instance B.Types.TypedObject Notebook where
instance B.Types.GObject Notebook
class (SP.GObject o, O.IsDescendantOf Notebook o) => IsNotebook o
instance (SP.GObject o, O.IsDescendantOf Notebook o) => IsNotebook o
instance O.HasParentTypes Notebook
toNotebook :: (MIO.MonadIO m, IsNotebook o) => o -> m Notebook
instance B.GValue.IsGValue (Maybe Notebook) where
#if defined(ENABLE_OVERLOADING)
data NotebookChangeCurrentPageSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookCreateWindowSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookFocusTabSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookMoveFocusOutSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookPageAddedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookPageRemovedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookPageReorderedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookReorderTabSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookSelectPageSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookSwitchPageSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookEnablePopupPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookGroupNamePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookPagePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookScrollablePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookShowBorderPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookShowTabsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookTabPosPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookAppendPageMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookAppendPageMenuMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookDetachTabMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookGetActionWidgetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookGetCurrentPageMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookGetGroupNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookGetMenuLabelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookGetMenuLabelTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookGetNPagesMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookGetNthPageMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookGetScrollableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookGetShowBorderMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookGetShowTabsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookGetTabDetachableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookGetTabHborderMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookGetTabLabelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookGetTabLabelTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookGetTabPosMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookGetTabReorderableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookGetTabVborderMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookInsertPageMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookInsertPageMenuMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookNextPageMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookPageNumMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookPopupDisableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookPopupEnableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookPrependPageMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookPrependPageMenuMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookPrevPageMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookRemovePageMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookReorderChildMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookSetActionWidgetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookSetCurrentPageMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookSetGroupNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookSetMenuLabelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookSetMenuLabelTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookSetScrollableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookSetShowBorderMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookSetShowTabsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookSetTabDetachableMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookSetTabLabelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookSetTabLabelTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookSetTabPosMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data NotebookSetTabReorderableMethodInfo
#endif
