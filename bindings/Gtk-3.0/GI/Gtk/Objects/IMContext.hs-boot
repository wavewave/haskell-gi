#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Objects.IMContext where

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

newtype IMContext = IMContext (SP.ManagedPtr IMContext)
instance SP.ManagedPtrNewtype IMContext where
instance B.Types.TypedObject IMContext where
instance B.Types.GObject IMContext
class (SP.GObject o, O.IsDescendantOf IMContext o) => IsIMContext o
instance (SP.GObject o, O.IsDescendantOf IMContext o) => IsIMContext o
instance O.HasParentTypes IMContext
toIMContext :: (MIO.MonadIO m, IsIMContext o) => o -> m IMContext
instance B.GValue.IsGValue (Maybe IMContext) where
#if defined(ENABLE_OVERLOADING)
data IMContextCommitSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IMContextDeleteSurroundingSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IMContextPreeditChangedSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IMContextPreeditEndSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IMContextPreeditStartSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IMContextRetrieveSurroundingSignalInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IMContextInputHintsPropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IMContextInputPurposePropertyInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IMContextDeleteSurroundingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IMContextFilterKeypressMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IMContextFocusInMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IMContextFocusOutMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IMContextGetPreeditStringMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IMContextGetSurroundingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IMContextResetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IMContextSetClientWindowMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IMContextSetCursorLocationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IMContextSetSurroundingMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IMContextSetUsePreeditMethodInfo
#endif
