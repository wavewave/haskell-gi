{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A GtkBindingSignal stores the necessary information to
-- activate a widget in response to a key press via a signal
-- emission.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.BindingSignal
    ( 

-- * Exported types
    BindingSignal(..)                       ,
    newZeroBindingSignal                    ,


 -- * Methods

#if defined(ENABLE_OVERLOADING)
    ResolveBindingSignalMethod              ,
#endif



 -- * Properties


-- ** nArgs #attr:nArgs#
-- | number of arguments specified for the signal

#if defined(ENABLE_OVERLOADING)
    bindingSignal_nArgs                     ,
#endif
    getBindingSignalNArgs                   ,
    setBindingSignalNArgs                   ,


-- ** next #attr:next#
-- | implementation detail

#if defined(ENABLE_OVERLOADING)
    bindingSignal_next                      ,
#endif
    clearBindingSignalNext                  ,
    getBindingSignalNext                    ,
    setBindingSignalNext                    ,


-- ** signalName #attr:signalName#
-- | the action signal to be emitted

#if defined(ENABLE_OVERLOADING)
    bindingSignal_signalName                ,
#endif
    clearBindingSignalSignalName            ,
    getBindingSignalSignalName              ,
    setBindingSignalSignalName              ,




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
newtype BindingSignal = BindingSignal (SP.ManagedPtr BindingSignal)
    deriving (Eq)

instance SP.ManagedPtrNewtype BindingSignal where
    toManagedPtr (BindingSignal p) = p

instance BoxedPtr BindingSignal where
    boxedPtrCopy = \p -> B.ManagedPtr.withManagedPtr p (copyBytes 32 >=> B.ManagedPtr.wrapPtr BindingSignal)
    boxedPtrFree = \x -> SP.withManagedPtr x SP.freeMem
instance CallocPtr BindingSignal where
    boxedPtrCalloc = callocBytes 32


-- | Construct a `BindingSignal` struct initialized to zero.
newZeroBindingSignal :: MonadIO m => m BindingSignal
newZeroBindingSignal = liftIO $ boxedPtrCalloc >>= wrapPtr BindingSignal

instance tag ~ 'AttrSet => Constructible BindingSignal tag where
    new _ attrs = do
        o <- newZeroBindingSignal
        GI.Attributes.set o attrs
        return o


-- | Get the value of the “@next@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' bindingSignal #next
-- @
getBindingSignalNext :: MonadIO m => BindingSignal -> m (Maybe BindingSignal)
getBindingSignalNext s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 0) :: IO (Ptr BindingSignal)
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- (newPtr BindingSignal) val'
        return val''
    return result

-- | Set the value of the “@next@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' bindingSignal [ #next 'Data.GI.Base.Attributes.:=' value ]
-- @
setBindingSignalNext :: MonadIO m => BindingSignal -> Ptr BindingSignal -> m ()
setBindingSignalNext s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 0) (val :: Ptr BindingSignal)

-- | Set the value of the “@next@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #next
-- @
clearBindingSignalNext :: MonadIO m => BindingSignal -> m ()
clearBindingSignalNext s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 0) (FP.nullPtr :: Ptr BindingSignal)

#if defined(ENABLE_OVERLOADING)
data BindingSignalNextFieldInfo
instance AttrInfo BindingSignalNextFieldInfo where
    type AttrBaseTypeConstraint BindingSignalNextFieldInfo = (~) BindingSignal
    type AttrAllowedOps BindingSignalNextFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint BindingSignalNextFieldInfo = (~) (Ptr BindingSignal)
    type AttrTransferTypeConstraint BindingSignalNextFieldInfo = (~)(Ptr BindingSignal)
    type AttrTransferType BindingSignalNextFieldInfo = (Ptr BindingSignal)
    type AttrGetType BindingSignalNextFieldInfo = Maybe BindingSignal
    type AttrLabel BindingSignalNextFieldInfo = "next"
    type AttrOrigin BindingSignalNextFieldInfo = BindingSignal
    attrGet = getBindingSignalNext
    attrSet = setBindingSignalNext
    attrConstruct = undefined
    attrClear = clearBindingSignalNext
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.BindingSignal.next"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-BindingSignal.html#g:attr:next"
        })

bindingSignal_next :: AttrLabelProxy "next"
bindingSignal_next = AttrLabelProxy

#endif


-- | Get the value of the “@signal_name@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' bindingSignal #signalName
-- @
getBindingSignalSignalName :: MonadIO m => BindingSignal -> m (Maybe T.Text)
getBindingSignalSignalName s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 8) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@signal_name@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' bindingSignal [ #signalName 'Data.GI.Base.Attributes.:=' value ]
-- @
setBindingSignalSignalName :: MonadIO m => BindingSignal -> CString -> m ()
setBindingSignalSignalName s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (val :: CString)

-- | Set the value of the “@signal_name@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #signalName
-- @
clearBindingSignalSignalName :: MonadIO m => BindingSignal -> m ()
clearBindingSignalSignalName s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data BindingSignalSignalNameFieldInfo
instance AttrInfo BindingSignalSignalNameFieldInfo where
    type AttrBaseTypeConstraint BindingSignalSignalNameFieldInfo = (~) BindingSignal
    type AttrAllowedOps BindingSignalSignalNameFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint BindingSignalSignalNameFieldInfo = (~) CString
    type AttrTransferTypeConstraint BindingSignalSignalNameFieldInfo = (~)CString
    type AttrTransferType BindingSignalSignalNameFieldInfo = CString
    type AttrGetType BindingSignalSignalNameFieldInfo = Maybe T.Text
    type AttrLabel BindingSignalSignalNameFieldInfo = "signal_name"
    type AttrOrigin BindingSignalSignalNameFieldInfo = BindingSignal
    attrGet = getBindingSignalSignalName
    attrSet = setBindingSignalSignalName
    attrConstruct = undefined
    attrClear = clearBindingSignalSignalName
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.BindingSignal.signalName"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-BindingSignal.html#g:attr:signalName"
        })

bindingSignal_signalName :: AttrLabelProxy "signalName"
bindingSignal_signalName = AttrLabelProxy

#endif


-- | Get the value of the “@n_args@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' bindingSignal #nArgs
-- @
getBindingSignalNArgs :: MonadIO m => BindingSignal -> m Word32
getBindingSignalNArgs s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 16) :: IO Word32
    return val

-- | Set the value of the “@n_args@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' bindingSignal [ #nArgs 'Data.GI.Base.Attributes.:=' value ]
-- @
setBindingSignalNArgs :: MonadIO m => BindingSignal -> Word32 -> m ()
setBindingSignalNArgs s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 16) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data BindingSignalNArgsFieldInfo
instance AttrInfo BindingSignalNArgsFieldInfo where
    type AttrBaseTypeConstraint BindingSignalNArgsFieldInfo = (~) BindingSignal
    type AttrAllowedOps BindingSignalNArgsFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint BindingSignalNArgsFieldInfo = (~) Word32
    type AttrTransferTypeConstraint BindingSignalNArgsFieldInfo = (~)Word32
    type AttrTransferType BindingSignalNArgsFieldInfo = Word32
    type AttrGetType BindingSignalNArgsFieldInfo = Word32
    type AttrLabel BindingSignalNArgsFieldInfo = "n_args"
    type AttrOrigin BindingSignalNArgsFieldInfo = BindingSignal
    attrGet = getBindingSignalNArgs
    attrSet = setBindingSignalNArgs
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.BindingSignal.nArgs"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-BindingSignal.html#g:attr:nArgs"
        })

bindingSignal_nArgs :: AttrLabelProxy "nArgs"
bindingSignal_nArgs = AttrLabelProxy

#endif


-- XXX Skipped attribute for "BindingSignal:args"
-- Not implemented: Don't know how to unpack C array of type TCArray False (-1) 2 (TInterface (Name {namespace = "Gtk", name = "BindingArg"}))

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList BindingSignal
type instance O.AttributeList BindingSignal = BindingSignalAttributeList
type BindingSignalAttributeList = ('[ '("next", BindingSignalNextFieldInfo), '("signalName", BindingSignalSignalNameFieldInfo), '("nArgs", BindingSignalNArgsFieldInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveBindingSignalMethod (t :: Symbol) (o :: *) :: * where
    ResolveBindingSignalMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveBindingSignalMethod t BindingSignal, O.OverloadedMethod info BindingSignal p) => OL.IsLabel t (BindingSignal -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveBindingSignalMethod t BindingSignal, O.OverloadedMethod info BindingSignal p, R.HasField t BindingSignal p) => R.HasField t BindingSignal p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveBindingSignalMethod t BindingSignal, O.OverloadedMethodInfo info BindingSignal) => OL.IsLabel t (O.MethodProxy info BindingSignal) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


