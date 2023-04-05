{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A t'GI.Gtk.Structs.TargetPair.TargetPair' is used to represent the same
-- information as a table of t'GI.Gtk.Structs.TargetEntry.TargetEntry', but in
-- an efficient form.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.TargetPair
    ( 

-- * Exported types
    TargetPair(..)                          ,
    newZeroTargetPair                       ,


 -- * Methods

#if defined(ENABLE_OVERLOADING)
    ResolveTargetPairMethod                 ,
#endif



 -- * Properties


-- ** flags #attr:flags#
-- | t'GI.Gtk.Flags.TargetFlags' for DND

    getTargetPairFlags                      ,
    setTargetPairFlags                      ,
#if defined(ENABLE_OVERLOADING)
    targetPair_flags                        ,
#endif


-- ** info #attr:info#
-- | an application-assigned integer ID which will
--     get passed as a parameter to e.g the [Widget::selectionGet]("GI.Gtk.Objects.Widget#g:signal:selectionGet")
--     signal. It allows the application to identify the target
--     type without extensive string compares.

    getTargetPairInfo                       ,
    setTargetPairInfo                       ,
#if defined(ENABLE_OVERLOADING)
    targetPair_info                         ,
#endif


-- ** target #attr:target#
-- | t'GI.Gdk.Structs.Atom.Atom' representation of the target type

    getTargetPairTarget                     ,
#if defined(ENABLE_OVERLOADING)
    targetPair_target                       ,
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

import qualified GI.Gdk.Structs.Atom as Gdk.Atom

-- | Memory-managed wrapper type.
newtype TargetPair = TargetPair (SP.ManagedPtr TargetPair)
    deriving (Eq)

instance SP.ManagedPtrNewtype TargetPair where
    toManagedPtr (TargetPair p) = p

instance BoxedPtr TargetPair where
    boxedPtrCopy = \p -> B.ManagedPtr.withManagedPtr p (copyBytes 16 >=> B.ManagedPtr.wrapPtr TargetPair)
    boxedPtrFree = \x -> SP.withManagedPtr x SP.freeMem
instance CallocPtr TargetPair where
    boxedPtrCalloc = callocBytes 16


-- | Construct a `TargetPair` struct initialized to zero.
newZeroTargetPair :: MonadIO m => m TargetPair
newZeroTargetPair = liftIO $ boxedPtrCalloc >>= wrapPtr TargetPair

instance tag ~ 'AttrSet => Constructible TargetPair tag where
    new _ attrs = do
        o <- newZeroTargetPair
        GI.Attributes.set o attrs
        return o


-- | Get the value of the “@target@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' targetPair #target
-- @
getTargetPairTarget :: MonadIO m => TargetPair -> m Gdk.Atom.Atom
getTargetPairTarget s = liftIO $ withManagedPtr s $ \ptr -> do
    let val = ptr `plusPtr` 0 :: (Ptr Gdk.Atom.Atom)
    val' <- (newPtr Gdk.Atom.Atom) val
    return val'

#if defined(ENABLE_OVERLOADING)
data TargetPairTargetFieldInfo
instance AttrInfo TargetPairTargetFieldInfo where
    type AttrBaseTypeConstraint TargetPairTargetFieldInfo = (~) TargetPair
    type AttrAllowedOps TargetPairTargetFieldInfo = '[ 'AttrGet]
    type AttrSetTypeConstraint TargetPairTargetFieldInfo = (~) (Ptr Gdk.Atom.Atom)
    type AttrTransferTypeConstraint TargetPairTargetFieldInfo = (~)(Ptr Gdk.Atom.Atom)
    type AttrTransferType TargetPairTargetFieldInfo = (Ptr Gdk.Atom.Atom)
    type AttrGetType TargetPairTargetFieldInfo = Gdk.Atom.Atom
    type AttrLabel TargetPairTargetFieldInfo = "target"
    type AttrOrigin TargetPairTargetFieldInfo = TargetPair
    attrGet = getTargetPairTarget
    attrSet = undefined
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TargetPair.target"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TargetPair.html#g:attr:target"
        })

targetPair_target :: AttrLabelProxy "target"
targetPair_target = AttrLabelProxy

#endif


-- | Get the value of the “@flags@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' targetPair #flags
-- @
getTargetPairFlags :: MonadIO m => TargetPair -> m Word32
getTargetPairFlags s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 8) :: IO Word32
    return val

-- | Set the value of the “@flags@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' targetPair [ #flags 'Data.GI.Base.Attributes.:=' value ]
-- @
setTargetPairFlags :: MonadIO m => TargetPair -> Word32 -> m ()
setTargetPairFlags s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data TargetPairFlagsFieldInfo
instance AttrInfo TargetPairFlagsFieldInfo where
    type AttrBaseTypeConstraint TargetPairFlagsFieldInfo = (~) TargetPair
    type AttrAllowedOps TargetPairFlagsFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TargetPairFlagsFieldInfo = (~) Word32
    type AttrTransferTypeConstraint TargetPairFlagsFieldInfo = (~)Word32
    type AttrTransferType TargetPairFlagsFieldInfo = Word32
    type AttrGetType TargetPairFlagsFieldInfo = Word32
    type AttrLabel TargetPairFlagsFieldInfo = "flags"
    type AttrOrigin TargetPairFlagsFieldInfo = TargetPair
    attrGet = getTargetPairFlags
    attrSet = setTargetPairFlags
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TargetPair.flags"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TargetPair.html#g:attr:flags"
        })

targetPair_flags :: AttrLabelProxy "flags"
targetPair_flags = AttrLabelProxy

#endif


-- | Get the value of the “@info@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' targetPair #info
-- @
getTargetPairInfo :: MonadIO m => TargetPair -> m Word32
getTargetPairInfo s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 12) :: IO Word32
    return val

-- | Set the value of the “@info@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' targetPair [ #info 'Data.GI.Base.Attributes.:=' value ]
-- @
setTargetPairInfo :: MonadIO m => TargetPair -> Word32 -> m ()
setTargetPairInfo s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 12) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data TargetPairInfoFieldInfo
instance AttrInfo TargetPairInfoFieldInfo where
    type AttrBaseTypeConstraint TargetPairInfoFieldInfo = (~) TargetPair
    type AttrAllowedOps TargetPairInfoFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TargetPairInfoFieldInfo = (~) Word32
    type AttrTransferTypeConstraint TargetPairInfoFieldInfo = (~)Word32
    type AttrTransferType TargetPairInfoFieldInfo = Word32
    type AttrGetType TargetPairInfoFieldInfo = Word32
    type AttrLabel TargetPairInfoFieldInfo = "info"
    type AttrOrigin TargetPairInfoFieldInfo = TargetPair
    attrGet = getTargetPairInfo
    attrSet = setTargetPairInfo
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TargetPair.info"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TargetPair.html#g:attr:info"
        })

targetPair_info :: AttrLabelProxy "info"
targetPair_info = AttrLabelProxy

#endif



#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList TargetPair
type instance O.AttributeList TargetPair = TargetPairAttributeList
type TargetPairAttributeList = ('[ '("target", TargetPairTargetFieldInfo), '("flags", TargetPairFlagsFieldInfo), '("info", TargetPairInfoFieldInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveTargetPairMethod (t :: Symbol) (o :: *) :: * where
    ResolveTargetPairMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveTargetPairMethod t TargetPair, O.OverloadedMethod info TargetPair p) => OL.IsLabel t (TargetPair -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveTargetPairMethod t TargetPair, O.OverloadedMethod info TargetPair p, R.HasField t TargetPair p) => R.HasField t TargetPair p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveTargetPairMethod t TargetPair, O.OverloadedMethodInfo info TargetPair) => OL.IsLabel t (O.MethodProxy info TargetPair) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


