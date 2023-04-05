#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Structs.IconSource where

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

newtype IconSource = IconSource (SP.ManagedPtr IconSource)
instance SP.ManagedPtrNewtype IconSource where
instance O.HasParentTypes IconSource
instance B.Types.TypedObject IconSource where
instance B.Types.GBoxed IconSource
instance B.GValue.IsGValue (Maybe IconSource) where
#if defined(ENABLE_OVERLOADING)
data IconSourceCopyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconSourceFreeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconSourceGetDirectionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconSourceGetDirectionWildcardedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconSourceGetFilenameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconSourceGetIconNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconSourceGetPixbufMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconSourceGetSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconSourceGetSizeWildcardedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconSourceGetStateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconSourceGetStateWildcardedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconSourceSetDirectionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconSourceSetDirectionWildcardedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconSourceSetFilenameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconSourceSetIconNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconSourceSetPixbufMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconSourceSetSizeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconSourceSetSizeWildcardedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconSourceSetStateMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data IconSourceSetStateWildcardedMethodInfo
#endif
