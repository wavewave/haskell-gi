#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.Expander where

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

newtype Expander = Expander (SP.ManagedPtr Expander)
instance SP.ManagedPtrNewtype Expander where
instance B.Types.TypedObject Expander where
instance B.Types.GObject Expander
class (SP.GObject o, O.IsDescendantOf Expander o) => IsExpander o
instance (SP.GObject o, O.IsDescendantOf Expander o) => IsExpander o
instance O.HasParentTypes Expander
toExpander :: (MIO.MonadIO m, IsExpander o) => o -> m Expander
instance B.GValue.IsGValue (Maybe Expander) where
#if defined(ENABLE_OVERLOADING)
data ExpanderActivateSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ExpanderExpandedPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ExpanderLabelPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ExpanderLabelFillPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ExpanderLabelWidgetPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ExpanderResizeToplevelPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ExpanderSpacingPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ExpanderUseMarkupPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ExpanderUseUnderlinePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ExpanderGetExpandedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ExpanderGetLabelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ExpanderGetLabelFillMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ExpanderGetLabelWidgetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ExpanderGetResizeToplevelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ExpanderGetSpacingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ExpanderGetUseMarkupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ExpanderGetUseUnderlineMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ExpanderSetExpandedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ExpanderSetLabelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ExpanderSetLabelFillMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ExpanderSetLabelWidgetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ExpanderSetResizeToplevelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ExpanderSetSpacingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ExpanderSetUseMarkupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ExpanderSetUseUnderlineMethodInfo
#endif
