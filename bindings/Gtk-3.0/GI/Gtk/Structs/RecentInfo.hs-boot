#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif
module GI.Gtk.Structs.RecentInfo where

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

newtype RecentInfo = RecentInfo (SP.ManagedPtr RecentInfo)
instance SP.ManagedPtrNewtype RecentInfo where
instance O.HasParentTypes RecentInfo
instance B.Types.TypedObject RecentInfo where
instance B.Types.GBoxed RecentInfo
instance B.GValue.IsGValue (Maybe RecentInfo) where
#if defined(ENABLE_OVERLOADING)
data RecentInfoCreateAppInfoMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentInfoExistsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentInfoGetAddedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentInfoGetAgeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentInfoGetApplicationInfoMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentInfoGetApplicationsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentInfoGetDescriptionMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentInfoGetDisplayNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentInfoGetGiconMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentInfoGetGroupsMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentInfoGetIconMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentInfoGetMimeTypeMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentInfoGetModifiedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentInfoGetPrivateHintMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentInfoGetShortNameMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentInfoGetUriMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentInfoGetUriDisplayMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentInfoGetVisitedMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentInfoHasApplicationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentInfoHasGroupMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentInfoIsLocalMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentInfoLastApplicationMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentInfoMatchMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentInfoRefMethodInfo
#endif
#if defined(ENABLE_OVERLOADING)
data RecentInfoUnrefMethodInfo
#endif
