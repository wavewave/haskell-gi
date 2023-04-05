{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- Class structure for t'GI.Gtk.Structs.MountOperationHandlerProxy_.MountOperationHandlerProxy_'.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.MountOperationHandlerProxyClass_
    ( 

-- * Exported types
    MountOperationHandlerProxyClass_(..)    ,
    newZeroMountOperationHandlerProxyClass_ ,


 -- * Methods

#if defined(ENABLE_OVERLOADING)
    ResolveMountOperationHandlerProxyClass_Method,
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
newtype MountOperationHandlerProxyClass_ = MountOperationHandlerProxyClass_ (SP.ManagedPtr MountOperationHandlerProxyClass_)
    deriving (Eq)

instance SP.ManagedPtrNewtype MountOperationHandlerProxyClass_ where
    toManagedPtr (MountOperationHandlerProxyClass_ p) = p

instance BoxedPtr MountOperationHandlerProxyClass_ where
    boxedPtrCopy = \p -> B.ManagedPtr.withManagedPtr p (copyBytes 408 >=> B.ManagedPtr.wrapPtr MountOperationHandlerProxyClass_)
    boxedPtrFree = \x -> SP.withManagedPtr x SP.freeMem
instance CallocPtr MountOperationHandlerProxyClass_ where
    boxedPtrCalloc = callocBytes 408


-- | Construct a `MountOperationHandlerProxyClass_` struct initialized to zero.
newZeroMountOperationHandlerProxyClass_ :: MonadIO m => m MountOperationHandlerProxyClass_
newZeroMountOperationHandlerProxyClass_ = liftIO $ boxedPtrCalloc >>= wrapPtr MountOperationHandlerProxyClass_

instance tag ~ 'AttrSet => Constructible MountOperationHandlerProxyClass_ tag where
    new _ attrs = do
        o <- newZeroMountOperationHandlerProxyClass_
        GI.Attributes.set o attrs
        return o


-- XXX Skipped attribute for "MountOperationHandlerProxyClass_:parent_class"
-- Not implemented: Field type is an unsupported struct type

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList MountOperationHandlerProxyClass_
type instance O.AttributeList MountOperationHandlerProxyClass_ = MountOperationHandlerProxyClass_AttributeList
type MountOperationHandlerProxyClass_AttributeList = ('[ ] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveMountOperationHandlerProxyClass_Method (t :: Symbol) (o :: *) :: * where
    ResolveMountOperationHandlerProxyClass_Method l o = O.MethodResolutionFailed l o

instance (info ~ ResolveMountOperationHandlerProxyClass_Method t MountOperationHandlerProxyClass_, O.OverloadedMethod info MountOperationHandlerProxyClass_ p) => OL.IsLabel t (MountOperationHandlerProxyClass_ -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveMountOperationHandlerProxyClass_Method t MountOperationHandlerProxyClass_, O.OverloadedMethod info MountOperationHandlerProxyClass_ p, R.HasField t MountOperationHandlerProxyClass_ p) => R.HasField t MountOperationHandlerProxyClass_ p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveMountOperationHandlerProxyClass_Method t MountOperationHandlerProxyClass_, O.OverloadedMethodInfo info MountOperationHandlerProxyClass_) => OL.IsLabel t (O.MethodProxy info MountOperationHandlerProxyClass_) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


