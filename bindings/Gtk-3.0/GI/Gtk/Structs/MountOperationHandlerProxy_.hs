{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- The t'GI.Gtk.Structs.MountOperationHandlerProxy_.MountOperationHandlerProxy_' structure contains only private data and should only be accessed using the provided API.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.MountOperationHandlerProxy_
    ( 

-- * Exported types
    MountOperationHandlerProxy_(..)         ,
    newZeroMountOperationHandlerProxy_      ,


 -- * Methods

#if defined(ENABLE_OVERLOADING)
    ResolveMountOperationHandlerProxy_Method,
#endif



    ) where

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


-- | Memory-managed wrapper type.
newtype MountOperationHandlerProxy_ = MountOperationHandlerProxy_ (SP.ManagedPtr MountOperationHandlerProxy_)
    deriving (Eq)

instance SP.ManagedPtrNewtype MountOperationHandlerProxy_ where
    toManagedPtr (MountOperationHandlerProxy_ p) = p

instance BoxedPtr MountOperationHandlerProxy_ where
    boxedPtrCopy = \p -> B.ManagedPtr.withManagedPtr p (copyBytes 40 >=> B.ManagedPtr.wrapPtr MountOperationHandlerProxy_)
    boxedPtrFree = \x -> SP.withManagedPtr x SP.freeMem
instance CallocPtr MountOperationHandlerProxy_ where
    boxedPtrCalloc = callocBytes 40


-- | Construct a `MountOperationHandlerProxy_` struct initialized to zero.
newZeroMountOperationHandlerProxy_ :: MonadIO m => m MountOperationHandlerProxy_
newZeroMountOperationHandlerProxy_ = liftIO $ boxedPtrCalloc >>= wrapPtr MountOperationHandlerProxy_

instance tag ~ 'AttrSet => Constructible MountOperationHandlerProxy_ tag where
    new _ attrs = do
        o <- newZeroMountOperationHandlerProxy_
        GI.Attributes.set o attrs
        return o



#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList MountOperationHandlerProxy_
type instance O.AttributeList MountOperationHandlerProxy_ = MountOperationHandlerProxy_AttributeList
type MountOperationHandlerProxy_AttributeList = ('[ ] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveMountOperationHandlerProxy_Method (t :: Symbol) (o :: *) :: * where
    ResolveMountOperationHandlerProxy_Method l o = O.MethodResolutionFailed l o

instance (info ~ ResolveMountOperationHandlerProxy_Method t MountOperationHandlerProxy_, O.OverloadedMethod info MountOperationHandlerProxy_ p) => OL.IsLabel t (MountOperationHandlerProxy_ -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveMountOperationHandlerProxy_Method t MountOperationHandlerProxy_, O.OverloadedMethod info MountOperationHandlerProxy_ p, R.HasField t MountOperationHandlerProxy_ p) => R.HasField t MountOperationHandlerProxy_ p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveMountOperationHandlerProxy_Method t MountOperationHandlerProxy_, O.OverloadedMethodInfo info MountOperationHandlerProxy_) => OL.IsLabel t (O.MethodProxy info MountOperationHandlerProxy_) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


