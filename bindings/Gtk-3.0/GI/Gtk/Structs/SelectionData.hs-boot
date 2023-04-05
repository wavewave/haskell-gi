#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Structs.SelectionData where

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

newtype SelectionData = SelectionData (SP.ManagedPtr SelectionData)
instance SP.ManagedPtrNewtype SelectionData where
instance O.HasParentTypes SelectionData
instance B.Types.TypedObject SelectionData where
instance B.Types.GBoxed SelectionData
instance B.GValue.IsGValue (Maybe SelectionData) where
#if defined(ENABLE_OVERLOADING)
data SelectionDataCopyMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SelectionDataFreeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SelectionDataGetDataTypeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SelectionDataGetDataMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SelectionDataGetDisplayMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SelectionDataGetFormatMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SelectionDataGetLengthMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SelectionDataGetPixbufMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SelectionDataGetSelectionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SelectionDataGetTargetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SelectionDataGetTargetsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SelectionDataGetTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SelectionDataGetUrisMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SelectionDataSetMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SelectionDataSetPixbufMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SelectionDataSetTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SelectionDataSetUrisMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SelectionDataTargetsIncludeImageMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SelectionDataTargetsIncludeRichTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SelectionDataTargetsIncludeTextMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data SelectionDataTargetsIncludeUriMethodInfo
#endif
