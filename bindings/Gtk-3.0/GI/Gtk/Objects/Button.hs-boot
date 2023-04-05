#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.Button where

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

newtype Button = Button (SP.ManagedPtr Button)
instance SP.ManagedPtrNewtype Button where
instance B.Types.TypedObject Button where
instance B.Types.GObject Button
class (SP.GObject o, O.IsDescendantOf Button o) => IsButton o
instance (SP.GObject o, O.IsDescendantOf Button o) => IsButton o
instance O.HasParentTypes Button
toButton :: (MIO.MonadIO m, IsButton o) => o -> m Button
instance B.GValue.IsGValue (Maybe Button) where
#if defined(ENABLE_OVERLOADING)
data ButtonActivateSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonClickedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonEnterSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonLeaveSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonPressedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonReleasedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonAlwaysShowImagePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonImagePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonImagePositionPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonLabelPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonReliefPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonUseStockPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonUseUnderlinePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonXalignPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonYalignPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonClickedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonEnterMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonGetAlignmentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonGetAlwaysShowImageMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonGetEventWindowMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonGetFocusOnClickMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonGetImageMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonGetImagePositionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonGetLabelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonGetReliefMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonGetUseStockMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonGetUseUnderlineMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonLeaveMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonPressedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonReleasedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonSetAlignmentMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonSetAlwaysShowImageMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonSetFocusOnClickMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonSetImageMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonSetImagePositionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonSetLabelMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonSetReliefMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonSetUseStockMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data ButtonSetUseUnderlineMethodInfo
#endif
