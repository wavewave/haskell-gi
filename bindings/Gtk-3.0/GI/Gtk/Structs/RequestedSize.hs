{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- Represents a request of a screen object in a given orientation. These
-- are primarily used in container implementations when allocating a natural
-- size for children calling. See 'GI.Gtk.Functions.distributeNaturalAllocation'.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.RequestedSize
    ( 

-- * Exported types
    RequestedSize(..)                       ,
    newZeroRequestedSize                    ,


 -- * Methods

#if defined(ENABLE_OVERLOADING)
    ResolveRequestedSizeMethod              ,
#endif



 -- * Properties


-- ** data #attr:data#
-- | A client pointer

    clearRequestedSizeData                  ,
    getRequestedSizeData                    ,
#if defined(ENABLE_OVERLOADING)
    requestedSize_data                      ,
#endif
    setRequestedSizeData                    ,


-- ** minimumSize #attr:minimumSize#
-- | The minimum size needed for allocation in a given orientation

    getRequestedSizeMinimumSize             ,
#if defined(ENABLE_OVERLOADING)
    requestedSize_minimumSize               ,
#endif
    setRequestedSizeMinimumSize             ,


-- ** naturalSize #attr:naturalSize#
-- | The natural size for allocation in a given orientation

    getRequestedSizeNaturalSize             ,
#if defined(ENABLE_OVERLOADING)
    requestedSize_naturalSize               ,
#endif
    setRequestedSizeNaturalSize             ,




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
newtype RequestedSize = RequestedSize (SP.ManagedPtr RequestedSize)
    deriving (Eq)

instance SP.ManagedPtrNewtype RequestedSize where
    toManagedPtr (RequestedSize p) = p

instance BoxedPtr RequestedSize where
    boxedPtrCopy = \p -> B.ManagedPtr.withManagedPtr p (copyBytes 16 >=> B.ManagedPtr.wrapPtr RequestedSize)
    boxedPtrFree = \x -> SP.withManagedPtr x SP.freeMem
instance CallocPtr RequestedSize where
    boxedPtrCalloc = callocBytes 16


-- | Construct a `RequestedSize` struct initialized to zero.
newZeroRequestedSize :: MonadIO m => m RequestedSize
newZeroRequestedSize = liftIO $ boxedPtrCalloc >>= wrapPtr RequestedSize

instance tag ~ 'AttrSet => Constructible RequestedSize tag where
    new _ attrs = do
        o <- newZeroRequestedSize
        GI.Attributes.set o attrs
        return o


-- | Get the value of the “@data@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' requestedSize #data
-- @
getRequestedSizeData :: MonadIO m => RequestedSize -> m (Ptr ())
getRequestedSizeData s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 0) :: IO (Ptr ())
    return val

-- | Set the value of the “@data@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' requestedSize [ #data 'Data.GI.Base.Attributes.:=' value ]
-- @
setRequestedSizeData :: MonadIO m => RequestedSize -> Ptr () -> m ()
setRequestedSizeData s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 0) (val :: Ptr ())

-- | Set the value of the “@data@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #data
-- @
clearRequestedSizeData :: MonadIO m => RequestedSize -> m ()
clearRequestedSizeData s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 0) (FP.nullPtr :: Ptr ())

#if defined(ENABLE_OVERLOADING)
data RequestedSizeDataFieldInfo
instance AttrInfo RequestedSizeDataFieldInfo where
    type AttrBaseTypeConstraint RequestedSizeDataFieldInfo = (~) RequestedSize
    type AttrAllowedOps RequestedSizeDataFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint RequestedSizeDataFieldInfo = (~) (Ptr ())
    type AttrTransferTypeConstraint RequestedSizeDataFieldInfo = (~)(Ptr ())
    type AttrTransferType RequestedSizeDataFieldInfo = (Ptr ())
    type AttrGetType RequestedSizeDataFieldInfo = Ptr ()
    type AttrLabel RequestedSizeDataFieldInfo = "data"
    type AttrOrigin RequestedSizeDataFieldInfo = RequestedSize
    attrGet = getRequestedSizeData
    attrSet = setRequestedSizeData
    attrConstruct = undefined
    attrClear = clearRequestedSizeData
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RequestedSize.data"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RequestedSize.html#g:attr:data"
        })

requestedSize_data :: AttrLabelProxy "data"
requestedSize_data = AttrLabelProxy

#endif


-- | Get the value of the “@minimum_size@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' requestedSize #minimumSize
-- @
getRequestedSizeMinimumSize :: MonadIO m => RequestedSize -> m Int32
getRequestedSizeMinimumSize s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 8) :: IO Int32
    return val

-- | Set the value of the “@minimum_size@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' requestedSize [ #minimumSize 'Data.GI.Base.Attributes.:=' value ]
-- @
setRequestedSizeMinimumSize :: MonadIO m => RequestedSize -> Int32 -> m ()
setRequestedSizeMinimumSize s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (val :: Int32)

#if defined(ENABLE_OVERLOADING)
data RequestedSizeMinimumSizeFieldInfo
instance AttrInfo RequestedSizeMinimumSizeFieldInfo where
    type AttrBaseTypeConstraint RequestedSizeMinimumSizeFieldInfo = (~) RequestedSize
    type AttrAllowedOps RequestedSizeMinimumSizeFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint RequestedSizeMinimumSizeFieldInfo = (~) Int32
    type AttrTransferTypeConstraint RequestedSizeMinimumSizeFieldInfo = (~)Int32
    type AttrTransferType RequestedSizeMinimumSizeFieldInfo = Int32
    type AttrGetType RequestedSizeMinimumSizeFieldInfo = Int32
    type AttrLabel RequestedSizeMinimumSizeFieldInfo = "minimum_size"
    type AttrOrigin RequestedSizeMinimumSizeFieldInfo = RequestedSize
    attrGet = getRequestedSizeMinimumSize
    attrSet = setRequestedSizeMinimumSize
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RequestedSize.minimumSize"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RequestedSize.html#g:attr:minimumSize"
        })

requestedSize_minimumSize :: AttrLabelProxy "minimumSize"
requestedSize_minimumSize = AttrLabelProxy

#endif


-- | Get the value of the “@natural_size@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' requestedSize #naturalSize
-- @
getRequestedSizeNaturalSize :: MonadIO m => RequestedSize -> m Int32
getRequestedSizeNaturalSize s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 12) :: IO Int32
    return val

-- | Set the value of the “@natural_size@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' requestedSize [ #naturalSize 'Data.GI.Base.Attributes.:=' value ]
-- @
setRequestedSizeNaturalSize :: MonadIO m => RequestedSize -> Int32 -> m ()
setRequestedSizeNaturalSize s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 12) (val :: Int32)

#if defined(ENABLE_OVERLOADING)
data RequestedSizeNaturalSizeFieldInfo
instance AttrInfo RequestedSizeNaturalSizeFieldInfo where
    type AttrBaseTypeConstraint RequestedSizeNaturalSizeFieldInfo = (~) RequestedSize
    type AttrAllowedOps RequestedSizeNaturalSizeFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint RequestedSizeNaturalSizeFieldInfo = (~) Int32
    type AttrTransferTypeConstraint RequestedSizeNaturalSizeFieldInfo = (~)Int32
    type AttrTransferType RequestedSizeNaturalSizeFieldInfo = Int32
    type AttrGetType RequestedSizeNaturalSizeFieldInfo = Int32
    type AttrLabel RequestedSizeNaturalSizeFieldInfo = "natural_size"
    type AttrOrigin RequestedSizeNaturalSizeFieldInfo = RequestedSize
    attrGet = getRequestedSizeNaturalSize
    attrSet = setRequestedSizeNaturalSize
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RequestedSize.naturalSize"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RequestedSize.html#g:attr:naturalSize"
        })

requestedSize_naturalSize :: AttrLabelProxy "naturalSize"
requestedSize_naturalSize = AttrLabelProxy

#endif



#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList RequestedSize
type instance O.AttributeList RequestedSize = RequestedSizeAttributeList
type RequestedSizeAttributeList = ('[ '("data", RequestedSizeDataFieldInfo), '("minimumSize", RequestedSizeMinimumSizeFieldInfo), '("naturalSize", RequestedSizeNaturalSizeFieldInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveRequestedSizeMethod (t :: Symbol) (o :: *) :: * where
    ResolveRequestedSizeMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveRequestedSizeMethod t RequestedSize, O.OverloadedMethod info RequestedSize p) => OL.IsLabel t (RequestedSize -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveRequestedSizeMethod t RequestedSize, O.OverloadedMethod info RequestedSize p, R.HasField t RequestedSize p) => R.HasField t RequestedSize p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveRequestedSizeMethod t RequestedSize, O.OverloadedMethodInfo info RequestedSize) => OL.IsLabel t (O.MethodProxy info RequestedSize) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


