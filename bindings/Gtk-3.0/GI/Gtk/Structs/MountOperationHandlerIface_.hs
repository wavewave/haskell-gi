{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- Virtual table for the D-Bus interface \<link linkend=\"gdbus-interface-org-Gtk-MountOperationHandler.top_of_page\">org.Gtk.MountOperationHandler\<\/link>.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.MountOperationHandlerIface_
    ( 

-- * Exported types
    MountOperationHandlerIface_(..)         ,
    newZeroMountOperationHandlerIface_      ,


 -- * Methods

#if defined(ENABLE_OVERLOADING)
    ResolveMountOperationHandlerIface_Method,
#endif



 -- * Properties


-- ** parentIface #attr:parentIface#
-- | The parent interface.

    getMountOperationHandlerIface_ParentIface,
#if defined(ENABLE_OVERLOADING)
    mountOperationHandlerIface__parentIface ,
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

import qualified GI.GObject.Structs.TypeInterface as GObject.TypeInterface

-- | Memory-managed wrapper type.
newtype MountOperationHandlerIface_ = MountOperationHandlerIface_ (SP.ManagedPtr MountOperationHandlerIface_)
    deriving (Eq)

instance SP.ManagedPtrNewtype MountOperationHandlerIface_ where
    toManagedPtr (MountOperationHandlerIface_ p) = p

instance BoxedPtr MountOperationHandlerIface_ where
    boxedPtrCopy = \p -> B.ManagedPtr.withManagedPtr p (copyBytes 48 >=> B.ManagedPtr.wrapPtr MountOperationHandlerIface_)
    boxedPtrFree = \x -> SP.withManagedPtr x SP.freeMem
instance CallocPtr MountOperationHandlerIface_ where
    boxedPtrCalloc = callocBytes 48


-- | Construct a `MountOperationHandlerIface_` struct initialized to zero.
newZeroMountOperationHandlerIface_ :: MonadIO m => m MountOperationHandlerIface_
newZeroMountOperationHandlerIface_ = liftIO $ boxedPtrCalloc >>= wrapPtr MountOperationHandlerIface_

instance tag ~ 'AttrSet => Constructible MountOperationHandlerIface_ tag where
    new _ attrs = do
        o <- newZeroMountOperationHandlerIface_
        GI.Attributes.set o attrs
        return o


-- | Get the value of the “@parent_iface@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' mountOperationHandlerIface_ #parentIface
-- @
getMountOperationHandlerIface_ParentIface :: MonadIO m => MountOperationHandlerIface_ -> m GObject.TypeInterface.TypeInterface
getMountOperationHandlerIface_ParentIface s = liftIO $ withManagedPtr s $ \ptr -> do
    let val = ptr `plusPtr` 0 :: (Ptr GObject.TypeInterface.TypeInterface)
    val' <- (newPtr GObject.TypeInterface.TypeInterface) val
    return val'

#if defined(ENABLE_OVERLOADING)
data MountOperationHandlerIface_ParentIfaceFieldInfo
instance AttrInfo MountOperationHandlerIface_ParentIfaceFieldInfo where
    type AttrBaseTypeConstraint MountOperationHandlerIface_ParentIfaceFieldInfo = (~) MountOperationHandlerIface_
    type AttrAllowedOps MountOperationHandlerIface_ParentIfaceFieldInfo = '[ 'AttrGet]
    type AttrSetTypeConstraint MountOperationHandlerIface_ParentIfaceFieldInfo = (~) (Ptr GObject.TypeInterface.TypeInterface)
    type AttrTransferTypeConstraint MountOperationHandlerIface_ParentIfaceFieldInfo = (~)(Ptr GObject.TypeInterface.TypeInterface)
    type AttrTransferType MountOperationHandlerIface_ParentIfaceFieldInfo = (Ptr GObject.TypeInterface.TypeInterface)
    type AttrGetType MountOperationHandlerIface_ParentIfaceFieldInfo = GObject.TypeInterface.TypeInterface
    type AttrLabel MountOperationHandlerIface_ParentIfaceFieldInfo = "parent_iface"
    type AttrOrigin MountOperationHandlerIface_ParentIfaceFieldInfo = MountOperationHandlerIface_
    attrGet = getMountOperationHandlerIface_ParentIface
    attrSet = undefined
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.MountOperationHandlerIface_.parentIface"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-MountOperationHandlerIface_.html#g:attr:parentIface"
        })

mountOperationHandlerIface__parentIface :: AttrLabelProxy "parentIface"
mountOperationHandlerIface__parentIface = AttrLabelProxy

#endif



#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList MountOperationHandlerIface_
type instance O.AttributeList MountOperationHandlerIface_ = MountOperationHandlerIface_AttributeList
type MountOperationHandlerIface_AttributeList = ('[ '("parentIface", MountOperationHandlerIface_ParentIfaceFieldInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveMountOperationHandlerIface_Method (t :: Symbol) (o :: *) :: * where
    ResolveMountOperationHandlerIface_Method l o = O.MethodResolutionFailed l o

instance (info ~ ResolveMountOperationHandlerIface_Method t MountOperationHandlerIface_, O.OverloadedMethod info MountOperationHandlerIface_ p) => OL.IsLabel t (MountOperationHandlerIface_ -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveMountOperationHandlerIface_Method t MountOperationHandlerIface_, O.OverloadedMethod info MountOperationHandlerIface_ p, R.HasField t MountOperationHandlerIface_ p) => R.HasField t MountOperationHandlerIface_ p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveMountOperationHandlerIface_Method t MountOperationHandlerIface_, O.OverloadedMethodInfo info MountOperationHandlerIface_) => OL.IsLabel t (O.MethodProxy info MountOperationHandlerIface_) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


