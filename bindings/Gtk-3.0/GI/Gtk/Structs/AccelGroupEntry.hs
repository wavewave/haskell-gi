{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- /No description available in the introspection data./

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.AccelGroupEntry
    ( 

-- * Exported types
    AccelGroupEntry(..)                     ,
    newZeroAccelGroupEntry                  ,


 -- * Methods

#if defined(ENABLE_OVERLOADING)
    ResolveAccelGroupEntryMethod            ,
#endif



 -- * Properties


-- ** accelPathQuark #attr:accelPathQuark#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    accelGroupEntry_accelPathQuark          ,
#endif
    getAccelGroupEntryAccelPathQuark        ,
    setAccelGroupEntryAccelPathQuark        ,


-- ** closure #attr:closure#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    accelGroupEntry_closure                 ,
#endif
    clearAccelGroupEntryClosure             ,
    getAccelGroupEntryClosure               ,
    setAccelGroupEntryClosure               ,


-- ** key #attr:key#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    accelGroupEntry_key                     ,
#endif
    getAccelGroupEntryKey                   ,




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

import {-# SOURCE #-} qualified GI.Gtk.Structs.AccelKey as Gtk.AccelKey

-- | Memory-managed wrapper type.
newtype AccelGroupEntry = AccelGroupEntry (SP.ManagedPtr AccelGroupEntry)
    deriving (Eq)

instance SP.ManagedPtrNewtype AccelGroupEntry where
    toManagedPtr (AccelGroupEntry p) = p

instance BoxedPtr AccelGroupEntry where
    boxedPtrCopy = \p -> B.ManagedPtr.withManagedPtr p (copyBytes 32 >=> B.ManagedPtr.wrapPtr AccelGroupEntry)
    boxedPtrFree = \x -> SP.withManagedPtr x SP.freeMem
instance CallocPtr AccelGroupEntry where
    boxedPtrCalloc = callocBytes 32


-- | Construct a `AccelGroupEntry` struct initialized to zero.
newZeroAccelGroupEntry :: MonadIO m => m AccelGroupEntry
newZeroAccelGroupEntry = liftIO $ boxedPtrCalloc >>= wrapPtr AccelGroupEntry

instance tag ~ 'AttrSet => Constructible AccelGroupEntry tag where
    new _ attrs = do
        o <- newZeroAccelGroupEntry
        GI.Attributes.set o attrs
        return o


-- | Get the value of the “@key@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' accelGroupEntry #key
-- @
getAccelGroupEntryKey :: MonadIO m => AccelGroupEntry -> m Gtk.AccelKey.AccelKey
getAccelGroupEntryKey s = liftIO $ withManagedPtr s $ \ptr -> do
    let val = ptr `plusPtr` 0 :: (Ptr Gtk.AccelKey.AccelKey)
    val' <- (newPtr Gtk.AccelKey.AccelKey) val
    return val'

#if defined(ENABLE_OVERLOADING)
data AccelGroupEntryKeyFieldInfo
instance AttrInfo AccelGroupEntryKeyFieldInfo where
    type AttrBaseTypeConstraint AccelGroupEntryKeyFieldInfo = (~) AccelGroupEntry
    type AttrAllowedOps AccelGroupEntryKeyFieldInfo = '[ 'AttrGet]
    type AttrSetTypeConstraint AccelGroupEntryKeyFieldInfo = (~) (Ptr Gtk.AccelKey.AccelKey)
    type AttrTransferTypeConstraint AccelGroupEntryKeyFieldInfo = (~)(Ptr Gtk.AccelKey.AccelKey)
    type AttrTransferType AccelGroupEntryKeyFieldInfo = (Ptr Gtk.AccelKey.AccelKey)
    type AttrGetType AccelGroupEntryKeyFieldInfo = Gtk.AccelKey.AccelKey
    type AttrLabel AccelGroupEntryKeyFieldInfo = "key"
    type AttrOrigin AccelGroupEntryKeyFieldInfo = AccelGroupEntry
    attrGet = getAccelGroupEntryKey
    attrSet = undefined
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.AccelGroupEntry.key"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-AccelGroupEntry.html#g:attr:key"
        })

accelGroupEntry_key :: AttrLabelProxy "key"
accelGroupEntry_key = AttrLabelProxy

#endif


-- | Get the value of the “@closure@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' accelGroupEntry #closure
-- @
getAccelGroupEntryClosure :: MonadIO m => AccelGroupEntry -> m (Maybe (GClosure ()))
getAccelGroupEntryClosure s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 16) :: IO (Ptr (GClosure ()))
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- (B.GClosure.newGClosureFromPtr . FP.castPtr) val'
        return val''
    return result

-- | Set the value of the “@closure@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' accelGroupEntry [ #closure 'Data.GI.Base.Attributes.:=' value ]
-- @
setAccelGroupEntryClosure :: MonadIO m => AccelGroupEntry -> Ptr (GClosure ()) -> m ()
setAccelGroupEntryClosure s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 16) (val :: Ptr (GClosure ()))

-- | Set the value of the “@closure@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #closure
-- @
clearAccelGroupEntryClosure :: MonadIO m => AccelGroupEntry -> m ()
clearAccelGroupEntryClosure s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 16) (FP.nullPtr :: Ptr (GClosure ()))

#if defined(ENABLE_OVERLOADING)
data AccelGroupEntryClosureFieldInfo
instance AttrInfo AccelGroupEntryClosureFieldInfo where
    type AttrBaseTypeConstraint AccelGroupEntryClosureFieldInfo = (~) AccelGroupEntry
    type AttrAllowedOps AccelGroupEntryClosureFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint AccelGroupEntryClosureFieldInfo = (~) (Ptr (GClosure ()))
    type AttrTransferTypeConstraint AccelGroupEntryClosureFieldInfo = (~)(Ptr (GClosure ()))
    type AttrTransferType AccelGroupEntryClosureFieldInfo = (Ptr (GClosure ()))
    type AttrGetType AccelGroupEntryClosureFieldInfo = Maybe (GClosure ())
    type AttrLabel AccelGroupEntryClosureFieldInfo = "closure"
    type AttrOrigin AccelGroupEntryClosureFieldInfo = AccelGroupEntry
    attrGet = getAccelGroupEntryClosure
    attrSet = setAccelGroupEntryClosure
    attrConstruct = undefined
    attrClear = clearAccelGroupEntryClosure
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.AccelGroupEntry.closure"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-AccelGroupEntry.html#g:attr:closure"
        })

accelGroupEntry_closure :: AttrLabelProxy "closure"
accelGroupEntry_closure = AttrLabelProxy

#endif


-- | Get the value of the “@accel_path_quark@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' accelGroupEntry #accelPathQuark
-- @
getAccelGroupEntryAccelPathQuark :: MonadIO m => AccelGroupEntry -> m Word32
getAccelGroupEntryAccelPathQuark s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 24) :: IO Word32
    return val

-- | Set the value of the “@accel_path_quark@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' accelGroupEntry [ #accelPathQuark 'Data.GI.Base.Attributes.:=' value ]
-- @
setAccelGroupEntryAccelPathQuark :: MonadIO m => AccelGroupEntry -> Word32 -> m ()
setAccelGroupEntryAccelPathQuark s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 24) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data AccelGroupEntryAccelPathQuarkFieldInfo
instance AttrInfo AccelGroupEntryAccelPathQuarkFieldInfo where
    type AttrBaseTypeConstraint AccelGroupEntryAccelPathQuarkFieldInfo = (~) AccelGroupEntry
    type AttrAllowedOps AccelGroupEntryAccelPathQuarkFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint AccelGroupEntryAccelPathQuarkFieldInfo = (~) Word32
    type AttrTransferTypeConstraint AccelGroupEntryAccelPathQuarkFieldInfo = (~)Word32
    type AttrTransferType AccelGroupEntryAccelPathQuarkFieldInfo = Word32
    type AttrGetType AccelGroupEntryAccelPathQuarkFieldInfo = Word32
    type AttrLabel AccelGroupEntryAccelPathQuarkFieldInfo = "accel_path_quark"
    type AttrOrigin AccelGroupEntryAccelPathQuarkFieldInfo = AccelGroupEntry
    attrGet = getAccelGroupEntryAccelPathQuark
    attrSet = setAccelGroupEntryAccelPathQuark
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.AccelGroupEntry.accelPathQuark"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-AccelGroupEntry.html#g:attr:accelPathQuark"
        })

accelGroupEntry_accelPathQuark :: AttrLabelProxy "accelPathQuark"
accelGroupEntry_accelPathQuark = AttrLabelProxy

#endif



#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList AccelGroupEntry
type instance O.AttributeList AccelGroupEntry = AccelGroupEntryAttributeList
type AccelGroupEntryAttributeList = ('[ '("key", AccelGroupEntryKeyFieldInfo), '("closure", AccelGroupEntryClosureFieldInfo), '("accelPathQuark", AccelGroupEntryAccelPathQuarkFieldInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveAccelGroupEntryMethod (t :: Symbol) (o :: *) :: * where
    ResolveAccelGroupEntryMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveAccelGroupEntryMethod t AccelGroupEntry, O.OverloadedMethod info AccelGroupEntry p) => OL.IsLabel t (AccelGroupEntry -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveAccelGroupEntryMethod t AccelGroupEntry, O.OverloadedMethod info AccelGroupEntry p, R.HasField t AccelGroupEntry p) => R.HasField t AccelGroupEntry p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveAccelGroupEntryMethod t AccelGroupEntry, O.OverloadedMethodInfo info AccelGroupEntry) => OL.IsLabel t (O.MethodProxy info AccelGroupEntry) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


