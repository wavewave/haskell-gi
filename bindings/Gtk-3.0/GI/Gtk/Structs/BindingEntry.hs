{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- Each key binding element of a binding sets binding list is
-- represented by a GtkBindingEntry.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.BindingEntry
    ( 

-- * Exported types
    BindingEntry(..)                        ,
    newZeroBindingEntry                     ,


 -- * Methods

#if defined(ENABLE_OVERLOADING)
    ResolveBindingEntryMethod               ,
#endif

-- ** addSignalFromString #method:addSignalFromString#

    bindingEntryAddSignalFromString         ,


-- ** addSignall #method:addSignall#

    bindingEntryAddSignall                  ,


-- ** remove #method:remove#

    bindingEntryRemove                      ,


-- ** skip #method:skip#

    bindingEntrySkip                        ,




 -- * Properties


-- ** bindingSet #attr:bindingSet#
-- | binding set this entry belongs to

#if defined(ENABLE_OVERLOADING)
    bindingEntry_bindingSet                 ,
#endif
    clearBindingEntryBindingSet             ,
    getBindingEntryBindingSet               ,
    setBindingEntryBindingSet               ,


-- ** destroyed #attr:destroyed#
-- | implementation detail

#if defined(ENABLE_OVERLOADING)
    bindingEntry_destroyed                  ,
#endif
    getBindingEntryDestroyed                ,
    setBindingEntryDestroyed                ,


-- ** hashNext #attr:hashNext#
-- | implementation detail

#if defined(ENABLE_OVERLOADING)
    bindingEntry_hashNext                   ,
#endif
    clearBindingEntryHashNext               ,
    getBindingEntryHashNext                 ,
    setBindingEntryHashNext                 ,


-- ** inEmission #attr:inEmission#
-- | implementation detail

#if defined(ENABLE_OVERLOADING)
    bindingEntry_inEmission                 ,
#endif
    getBindingEntryInEmission               ,
    setBindingEntryInEmission               ,


-- ** keyval #attr:keyval#
-- | key value to match

#if defined(ENABLE_OVERLOADING)
    bindingEntry_keyval                     ,
#endif
    getBindingEntryKeyval                   ,
    setBindingEntryKeyval                   ,


-- ** marksUnbound #attr:marksUnbound#
-- | implementation detail

#if defined(ENABLE_OVERLOADING)
    bindingEntry_marksUnbound               ,
#endif
    getBindingEntryMarksUnbound             ,
    setBindingEntryMarksUnbound             ,


-- ** modifiers #attr:modifiers#
-- | key modifiers to match

#if defined(ENABLE_OVERLOADING)
    bindingEntry_modifiers                  ,
#endif
    getBindingEntryModifiers                ,
    setBindingEntryModifiers                ,


-- ** setNext #attr:setNext#
-- | linked list of entries maintained by binding set

#if defined(ENABLE_OVERLOADING)
    bindingEntry_setNext                    ,
#endif
    clearBindingEntrySetNext                ,
    getBindingEntrySetNext                  ,
    setBindingEntrySetNext                  ,


-- ** signals #attr:signals#
-- | action signals of this entry

#if defined(ENABLE_OVERLOADING)
    bindingEntry_signals                    ,
#endif
    clearBindingEntrySignals                ,
    getBindingEntrySignals                  ,
    setBindingEntrySignals                  ,




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

import qualified GI.GLib.Enums as GLib.Enums
import qualified GI.Gdk.Flags as Gdk.Flags
import {-# SOURCE #-} qualified GI.Gtk.Structs.BindingArg as Gtk.BindingArg
import {-# SOURCE #-} qualified GI.Gtk.Structs.BindingSet as Gtk.BindingSet
import {-# SOURCE #-} qualified GI.Gtk.Structs.BindingSignal as Gtk.BindingSignal

-- | Memory-managed wrapper type.
newtype BindingEntry = BindingEntry (SP.ManagedPtr BindingEntry)
    deriving (Eq)

instance SP.ManagedPtrNewtype BindingEntry where
    toManagedPtr (BindingEntry p) = p

instance BoxedPtr BindingEntry where
    boxedPtrCopy = \p -> B.ManagedPtr.withManagedPtr p (copyBytes 56 >=> B.ManagedPtr.wrapPtr BindingEntry)
    boxedPtrFree = \x -> SP.withManagedPtr x SP.freeMem
instance CallocPtr BindingEntry where
    boxedPtrCalloc = callocBytes 56


-- | Construct a `BindingEntry` struct initialized to zero.
newZeroBindingEntry :: MonadIO m => m BindingEntry
newZeroBindingEntry = liftIO $ boxedPtrCalloc >>= wrapPtr BindingEntry

instance tag ~ 'AttrSet => Constructible BindingEntry tag where
    new _ attrs = do
        o <- newZeroBindingEntry
        GI.Attributes.set o attrs
        return o


-- | Get the value of the “@keyval@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' bindingEntry #keyval
-- @
getBindingEntryKeyval :: MonadIO m => BindingEntry -> m Word32
getBindingEntryKeyval s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 0) :: IO Word32
    return val

-- | Set the value of the “@keyval@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' bindingEntry [ #keyval 'Data.GI.Base.Attributes.:=' value ]
-- @
setBindingEntryKeyval :: MonadIO m => BindingEntry -> Word32 -> m ()
setBindingEntryKeyval s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 0) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data BindingEntryKeyvalFieldInfo
instance AttrInfo BindingEntryKeyvalFieldInfo where
    type AttrBaseTypeConstraint BindingEntryKeyvalFieldInfo = (~) BindingEntry
    type AttrAllowedOps BindingEntryKeyvalFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint BindingEntryKeyvalFieldInfo = (~) Word32
    type AttrTransferTypeConstraint BindingEntryKeyvalFieldInfo = (~)Word32
    type AttrTransferType BindingEntryKeyvalFieldInfo = Word32
    type AttrGetType BindingEntryKeyvalFieldInfo = Word32
    type AttrLabel BindingEntryKeyvalFieldInfo = "keyval"
    type AttrOrigin BindingEntryKeyvalFieldInfo = BindingEntry
    attrGet = getBindingEntryKeyval
    attrSet = setBindingEntryKeyval
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.BindingEntry.keyval"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-BindingEntry.html#g:attr:keyval"
        })

bindingEntry_keyval :: AttrLabelProxy "keyval"
bindingEntry_keyval = AttrLabelProxy

#endif


-- | Get the value of the “@modifiers@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' bindingEntry #modifiers
-- @
getBindingEntryModifiers :: MonadIO m => BindingEntry -> m [Gdk.Flags.ModifierType]
getBindingEntryModifiers s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 4) :: IO CUInt
    let val' = wordToGFlags val
    return val'

-- | Set the value of the “@modifiers@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' bindingEntry [ #modifiers 'Data.GI.Base.Attributes.:=' value ]
-- @
setBindingEntryModifiers :: MonadIO m => BindingEntry -> [Gdk.Flags.ModifierType] -> m ()
setBindingEntryModifiers s val = liftIO $ withManagedPtr s $ \ptr -> do
    let val' = gflagsToWord val
    poke (ptr `plusPtr` 4) (val' :: CUInt)

#if defined(ENABLE_OVERLOADING)
data BindingEntryModifiersFieldInfo
instance AttrInfo BindingEntryModifiersFieldInfo where
    type AttrBaseTypeConstraint BindingEntryModifiersFieldInfo = (~) BindingEntry
    type AttrAllowedOps BindingEntryModifiersFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint BindingEntryModifiersFieldInfo = (~) [Gdk.Flags.ModifierType]
    type AttrTransferTypeConstraint BindingEntryModifiersFieldInfo = (~)[Gdk.Flags.ModifierType]
    type AttrTransferType BindingEntryModifiersFieldInfo = [Gdk.Flags.ModifierType]
    type AttrGetType BindingEntryModifiersFieldInfo = [Gdk.Flags.ModifierType]
    type AttrLabel BindingEntryModifiersFieldInfo = "modifiers"
    type AttrOrigin BindingEntryModifiersFieldInfo = BindingEntry
    attrGet = getBindingEntryModifiers
    attrSet = setBindingEntryModifiers
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.BindingEntry.modifiers"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-BindingEntry.html#g:attr:modifiers"
        })

bindingEntry_modifiers :: AttrLabelProxy "modifiers"
bindingEntry_modifiers = AttrLabelProxy

#endif


-- | Get the value of the “@binding_set@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' bindingEntry #bindingSet
-- @
getBindingEntryBindingSet :: MonadIO m => BindingEntry -> m (Maybe Gtk.BindingSet.BindingSet)
getBindingEntryBindingSet s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 8) :: IO (Ptr Gtk.BindingSet.BindingSet)
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- (newPtr Gtk.BindingSet.BindingSet) val'
        return val''
    return result

-- | Set the value of the “@binding_set@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' bindingEntry [ #bindingSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setBindingEntryBindingSet :: MonadIO m => BindingEntry -> Ptr Gtk.BindingSet.BindingSet -> m ()
setBindingEntryBindingSet s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (val :: Ptr Gtk.BindingSet.BindingSet)

-- | Set the value of the “@binding_set@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #bindingSet
-- @
clearBindingEntryBindingSet :: MonadIO m => BindingEntry -> m ()
clearBindingEntryBindingSet s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (FP.nullPtr :: Ptr Gtk.BindingSet.BindingSet)

#if defined(ENABLE_OVERLOADING)
data BindingEntryBindingSetFieldInfo
instance AttrInfo BindingEntryBindingSetFieldInfo where
    type AttrBaseTypeConstraint BindingEntryBindingSetFieldInfo = (~) BindingEntry
    type AttrAllowedOps BindingEntryBindingSetFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint BindingEntryBindingSetFieldInfo = (~) (Ptr Gtk.BindingSet.BindingSet)
    type AttrTransferTypeConstraint BindingEntryBindingSetFieldInfo = (~)(Ptr Gtk.BindingSet.BindingSet)
    type AttrTransferType BindingEntryBindingSetFieldInfo = (Ptr Gtk.BindingSet.BindingSet)
    type AttrGetType BindingEntryBindingSetFieldInfo = Maybe Gtk.BindingSet.BindingSet
    type AttrLabel BindingEntryBindingSetFieldInfo = "binding_set"
    type AttrOrigin BindingEntryBindingSetFieldInfo = BindingEntry
    attrGet = getBindingEntryBindingSet
    attrSet = setBindingEntryBindingSet
    attrConstruct = undefined
    attrClear = clearBindingEntryBindingSet
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.BindingEntry.bindingSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-BindingEntry.html#g:attr:bindingSet"
        })

bindingEntry_bindingSet :: AttrLabelProxy "bindingSet"
bindingEntry_bindingSet = AttrLabelProxy

#endif


-- | Get the value of the “@destroyed@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' bindingEntry #destroyed
-- @
getBindingEntryDestroyed :: MonadIO m => BindingEntry -> m Word32
getBindingEntryDestroyed s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 16) :: IO Word32
    return val

-- | Set the value of the “@destroyed@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' bindingEntry [ #destroyed 'Data.GI.Base.Attributes.:=' value ]
-- @
setBindingEntryDestroyed :: MonadIO m => BindingEntry -> Word32 -> m ()
setBindingEntryDestroyed s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 16) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data BindingEntryDestroyedFieldInfo
instance AttrInfo BindingEntryDestroyedFieldInfo where
    type AttrBaseTypeConstraint BindingEntryDestroyedFieldInfo = (~) BindingEntry
    type AttrAllowedOps BindingEntryDestroyedFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint BindingEntryDestroyedFieldInfo = (~) Word32
    type AttrTransferTypeConstraint BindingEntryDestroyedFieldInfo = (~)Word32
    type AttrTransferType BindingEntryDestroyedFieldInfo = Word32
    type AttrGetType BindingEntryDestroyedFieldInfo = Word32
    type AttrLabel BindingEntryDestroyedFieldInfo = "destroyed"
    type AttrOrigin BindingEntryDestroyedFieldInfo = BindingEntry
    attrGet = getBindingEntryDestroyed
    attrSet = setBindingEntryDestroyed
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.BindingEntry.destroyed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-BindingEntry.html#g:attr:destroyed"
        })

bindingEntry_destroyed :: AttrLabelProxy "destroyed"
bindingEntry_destroyed = AttrLabelProxy

#endif


-- | Get the value of the “@in_emission@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' bindingEntry #inEmission
-- @
getBindingEntryInEmission :: MonadIO m => BindingEntry -> m Word32
getBindingEntryInEmission s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 20) :: IO Word32
    return val

-- | Set the value of the “@in_emission@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' bindingEntry [ #inEmission 'Data.GI.Base.Attributes.:=' value ]
-- @
setBindingEntryInEmission :: MonadIO m => BindingEntry -> Word32 -> m ()
setBindingEntryInEmission s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 20) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data BindingEntryInEmissionFieldInfo
instance AttrInfo BindingEntryInEmissionFieldInfo where
    type AttrBaseTypeConstraint BindingEntryInEmissionFieldInfo = (~) BindingEntry
    type AttrAllowedOps BindingEntryInEmissionFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint BindingEntryInEmissionFieldInfo = (~) Word32
    type AttrTransferTypeConstraint BindingEntryInEmissionFieldInfo = (~)Word32
    type AttrTransferType BindingEntryInEmissionFieldInfo = Word32
    type AttrGetType BindingEntryInEmissionFieldInfo = Word32
    type AttrLabel BindingEntryInEmissionFieldInfo = "in_emission"
    type AttrOrigin BindingEntryInEmissionFieldInfo = BindingEntry
    attrGet = getBindingEntryInEmission
    attrSet = setBindingEntryInEmission
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.BindingEntry.inEmission"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-BindingEntry.html#g:attr:inEmission"
        })

bindingEntry_inEmission :: AttrLabelProxy "inEmission"
bindingEntry_inEmission = AttrLabelProxy

#endif


-- | Get the value of the “@marks_unbound@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' bindingEntry #marksUnbound
-- @
getBindingEntryMarksUnbound :: MonadIO m => BindingEntry -> m Word32
getBindingEntryMarksUnbound s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 24) :: IO Word32
    return val

-- | Set the value of the “@marks_unbound@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' bindingEntry [ #marksUnbound 'Data.GI.Base.Attributes.:=' value ]
-- @
setBindingEntryMarksUnbound :: MonadIO m => BindingEntry -> Word32 -> m ()
setBindingEntryMarksUnbound s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 24) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data BindingEntryMarksUnboundFieldInfo
instance AttrInfo BindingEntryMarksUnboundFieldInfo where
    type AttrBaseTypeConstraint BindingEntryMarksUnboundFieldInfo = (~) BindingEntry
    type AttrAllowedOps BindingEntryMarksUnboundFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint BindingEntryMarksUnboundFieldInfo = (~) Word32
    type AttrTransferTypeConstraint BindingEntryMarksUnboundFieldInfo = (~)Word32
    type AttrTransferType BindingEntryMarksUnboundFieldInfo = Word32
    type AttrGetType BindingEntryMarksUnboundFieldInfo = Word32
    type AttrLabel BindingEntryMarksUnboundFieldInfo = "marks_unbound"
    type AttrOrigin BindingEntryMarksUnboundFieldInfo = BindingEntry
    attrGet = getBindingEntryMarksUnbound
    attrSet = setBindingEntryMarksUnbound
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.BindingEntry.marksUnbound"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-BindingEntry.html#g:attr:marksUnbound"
        })

bindingEntry_marksUnbound :: AttrLabelProxy "marksUnbound"
bindingEntry_marksUnbound = AttrLabelProxy

#endif


-- | Get the value of the “@set_next@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' bindingEntry #setNext
-- @
getBindingEntrySetNext :: MonadIO m => BindingEntry -> m (Maybe BindingEntry)
getBindingEntrySetNext s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 32) :: IO (Ptr BindingEntry)
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- (newPtr BindingEntry) val'
        return val''
    return result

-- | Set the value of the “@set_next@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' bindingEntry [ #setNext 'Data.GI.Base.Attributes.:=' value ]
-- @
setBindingEntrySetNext :: MonadIO m => BindingEntry -> Ptr BindingEntry -> m ()
setBindingEntrySetNext s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 32) (val :: Ptr BindingEntry)

-- | Set the value of the “@set_next@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #setNext
-- @
clearBindingEntrySetNext :: MonadIO m => BindingEntry -> m ()
clearBindingEntrySetNext s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 32) (FP.nullPtr :: Ptr BindingEntry)

#if defined(ENABLE_OVERLOADING)
data BindingEntrySetNextFieldInfo
instance AttrInfo BindingEntrySetNextFieldInfo where
    type AttrBaseTypeConstraint BindingEntrySetNextFieldInfo = (~) BindingEntry
    type AttrAllowedOps BindingEntrySetNextFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint BindingEntrySetNextFieldInfo = (~) (Ptr BindingEntry)
    type AttrTransferTypeConstraint BindingEntrySetNextFieldInfo = (~)(Ptr BindingEntry)
    type AttrTransferType BindingEntrySetNextFieldInfo = (Ptr BindingEntry)
    type AttrGetType BindingEntrySetNextFieldInfo = Maybe BindingEntry
    type AttrLabel BindingEntrySetNextFieldInfo = "set_next"
    type AttrOrigin BindingEntrySetNextFieldInfo = BindingEntry
    attrGet = getBindingEntrySetNext
    attrSet = setBindingEntrySetNext
    attrConstruct = undefined
    attrClear = clearBindingEntrySetNext
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.BindingEntry.setNext"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-BindingEntry.html#g:attr:setNext"
        })

bindingEntry_setNext :: AttrLabelProxy "setNext"
bindingEntry_setNext = AttrLabelProxy

#endif


-- | Get the value of the “@hash_next@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' bindingEntry #hashNext
-- @
getBindingEntryHashNext :: MonadIO m => BindingEntry -> m (Maybe BindingEntry)
getBindingEntryHashNext s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 40) :: IO (Ptr BindingEntry)
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- (newPtr BindingEntry) val'
        return val''
    return result

-- | Set the value of the “@hash_next@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' bindingEntry [ #hashNext 'Data.GI.Base.Attributes.:=' value ]
-- @
setBindingEntryHashNext :: MonadIO m => BindingEntry -> Ptr BindingEntry -> m ()
setBindingEntryHashNext s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 40) (val :: Ptr BindingEntry)

-- | Set the value of the “@hash_next@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #hashNext
-- @
clearBindingEntryHashNext :: MonadIO m => BindingEntry -> m ()
clearBindingEntryHashNext s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 40) (FP.nullPtr :: Ptr BindingEntry)

#if defined(ENABLE_OVERLOADING)
data BindingEntryHashNextFieldInfo
instance AttrInfo BindingEntryHashNextFieldInfo where
    type AttrBaseTypeConstraint BindingEntryHashNextFieldInfo = (~) BindingEntry
    type AttrAllowedOps BindingEntryHashNextFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint BindingEntryHashNextFieldInfo = (~) (Ptr BindingEntry)
    type AttrTransferTypeConstraint BindingEntryHashNextFieldInfo = (~)(Ptr BindingEntry)
    type AttrTransferType BindingEntryHashNextFieldInfo = (Ptr BindingEntry)
    type AttrGetType BindingEntryHashNextFieldInfo = Maybe BindingEntry
    type AttrLabel BindingEntryHashNextFieldInfo = "hash_next"
    type AttrOrigin BindingEntryHashNextFieldInfo = BindingEntry
    attrGet = getBindingEntryHashNext
    attrSet = setBindingEntryHashNext
    attrConstruct = undefined
    attrClear = clearBindingEntryHashNext
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.BindingEntry.hashNext"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-BindingEntry.html#g:attr:hashNext"
        })

bindingEntry_hashNext :: AttrLabelProxy "hashNext"
bindingEntry_hashNext = AttrLabelProxy

#endif


-- | Get the value of the “@signals@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' bindingEntry #signals
-- @
getBindingEntrySignals :: MonadIO m => BindingEntry -> m (Maybe Gtk.BindingSignal.BindingSignal)
getBindingEntrySignals s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 48) :: IO (Ptr Gtk.BindingSignal.BindingSignal)
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- (newPtr Gtk.BindingSignal.BindingSignal) val'
        return val''
    return result

-- | Set the value of the “@signals@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' bindingEntry [ #signals 'Data.GI.Base.Attributes.:=' value ]
-- @
setBindingEntrySignals :: MonadIO m => BindingEntry -> Ptr Gtk.BindingSignal.BindingSignal -> m ()
setBindingEntrySignals s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 48) (val :: Ptr Gtk.BindingSignal.BindingSignal)

-- | Set the value of the “@signals@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #signals
-- @
clearBindingEntrySignals :: MonadIO m => BindingEntry -> m ()
clearBindingEntrySignals s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 48) (FP.nullPtr :: Ptr Gtk.BindingSignal.BindingSignal)

#if defined(ENABLE_OVERLOADING)
data BindingEntrySignalsFieldInfo
instance AttrInfo BindingEntrySignalsFieldInfo where
    type AttrBaseTypeConstraint BindingEntrySignalsFieldInfo = (~) BindingEntry
    type AttrAllowedOps BindingEntrySignalsFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint BindingEntrySignalsFieldInfo = (~) (Ptr Gtk.BindingSignal.BindingSignal)
    type AttrTransferTypeConstraint BindingEntrySignalsFieldInfo = (~)(Ptr Gtk.BindingSignal.BindingSignal)
    type AttrTransferType BindingEntrySignalsFieldInfo = (Ptr Gtk.BindingSignal.BindingSignal)
    type AttrGetType BindingEntrySignalsFieldInfo = Maybe Gtk.BindingSignal.BindingSignal
    type AttrLabel BindingEntrySignalsFieldInfo = "signals"
    type AttrOrigin BindingEntrySignalsFieldInfo = BindingEntry
    attrGet = getBindingEntrySignals
    attrSet = setBindingEntrySignals
    attrConstruct = undefined
    attrClear = clearBindingEntrySignals
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.BindingEntry.signals"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-BindingEntry.html#g:attr:signals"
        })

bindingEntry_signals :: AttrLabelProxy "signals"
bindingEntry_signals = AttrLabelProxy

#endif



#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList BindingEntry
type instance O.AttributeList BindingEntry = BindingEntryAttributeList
type BindingEntryAttributeList = ('[ '("keyval", BindingEntryKeyvalFieldInfo), '("modifiers", BindingEntryModifiersFieldInfo), '("bindingSet", BindingEntryBindingSetFieldInfo), '("destroyed", BindingEntryDestroyedFieldInfo), '("inEmission", BindingEntryInEmissionFieldInfo), '("marksUnbound", BindingEntryMarksUnboundFieldInfo), '("setNext", BindingEntrySetNextFieldInfo), '("hashNext", BindingEntryHashNextFieldInfo), '("signals", BindingEntrySignalsFieldInfo)] :: [(Symbol, *)])
#endif

-- method BindingEntry::add_signal_from_string
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "binding_set"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "BindingSet" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBindingSet" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "signal_desc"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a signal description"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "GLib" , name = "TokenType" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_binding_entry_add_signal_from_string" gtk_binding_entry_add_signal_from_string :: 
    Ptr Gtk.BindingSet.BindingSet ->        -- binding_set : TInterface (Name {namespace = "Gtk", name = "BindingSet"})
    CString ->                              -- signal_desc : TBasicType TUTF8
    IO CUInt

-- | Parses a signal description from /@signalDesc@/ and incorporates
-- it into /@bindingSet@/.
-- 
-- Signal descriptions may either bind a key combination to
-- one or more signals:
-- >
-- >  bind "key" {
-- >    "signalname" (param, ...)
-- >    ...
-- >  }
-- 
-- 
-- Or they may also unbind a key combination:
-- >
-- >  unbind "key"
-- 
-- 
-- Key combinations must be in a format that can be parsed by
-- 'GI.Gtk.Functions.acceleratorParse'.
-- 
-- /Since: 3.0/
bindingEntryAddSignalFromString ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Gtk.BindingSet.BindingSet
    -- ^ /@bindingSet@/: a t'GI.Gtk.Structs.BindingSet.BindingSet'
    -> T.Text
    -- ^ /@signalDesc@/: a signal description
    -> m GLib.Enums.TokenType
    -- ^ __Returns:__ 'GI.GLib.Enums.TokenTypeNone' if the signal was successfully parsed and added,
    --     the expected token otherwise
bindingEntryAddSignalFromString bindingSet signalDesc = liftIO $ do
    bindingSet' <- unsafeManagedPtrGetPtr bindingSet
    signalDesc' <- textToCString signalDesc
    result <- gtk_binding_entry_add_signal_from_string bindingSet' signalDesc'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr bindingSet
    freeMem signalDesc'
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method BindingEntry::add_signall
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "binding_set"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "BindingSet" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBindingSet to add a signal to"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "keyval"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "key value" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "modifiers"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "ModifierType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "key modifier" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "signal_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "signal name to be bound"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "binding_args"
--           , argType =
--               TGSList
--                 (TInterface Name { namespace = "Gtk" , name = "BindingArg" })
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "\n    list of #GtkBindingArg signal arguments"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_binding_entry_add_signall" gtk_binding_entry_add_signall :: 
    Ptr Gtk.BindingSet.BindingSet ->        -- binding_set : TInterface (Name {namespace = "Gtk", name = "BindingSet"})
    Word32 ->                               -- keyval : TBasicType TUInt
    CUInt ->                                -- modifiers : TInterface (Name {namespace = "Gdk", name = "ModifierType"})
    CString ->                              -- signal_name : TBasicType TUTF8
    Ptr (GSList (Ptr Gtk.BindingArg.BindingArg)) -> -- binding_args : TGSList (TInterface (Name {namespace = "Gtk", name = "BindingArg"}))
    IO ()

-- | Override or install a new key binding for /@keyval@/ with /@modifiers@/ on
-- /@bindingSet@/.
bindingEntryAddSignall ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Gtk.BindingSet.BindingSet
    -- ^ /@bindingSet@/: a t'GI.Gtk.Structs.BindingSet.BindingSet' to add a signal to
    -> Word32
    -- ^ /@keyval@/: key value
    -> [Gdk.Flags.ModifierType]
    -- ^ /@modifiers@/: key modifier
    -> T.Text
    -- ^ /@signalName@/: signal name to be bound
    -> [Gtk.BindingArg.BindingArg]
    -- ^ /@bindingArgs@/: 
    --     list of t'GI.Gtk.Structs.BindingArg.BindingArg' signal arguments
    -> m ()
bindingEntryAddSignall bindingSet keyval modifiers signalName bindingArgs = liftIO $ do
    bindingSet' <- unsafeManagedPtrGetPtr bindingSet
    let modifiers' = gflagsToWord modifiers
    signalName' <- textToCString signalName
    bindingArgs' <- mapM unsafeManagedPtrGetPtr bindingArgs
    bindingArgs'' <- packGSList bindingArgs'
    gtk_binding_entry_add_signall bindingSet' keyval modifiers' signalName' bindingArgs''
    touchManagedPtr bindingSet
    mapM_ touchManagedPtr bindingArgs
    freeMem signalName'
    g_slist_free bindingArgs''
    return ()

#if defined(ENABLE_OVERLOADING)
#endif

-- method BindingEntry::remove
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "binding_set"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "BindingSet" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBindingSet to remove an entry of"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "keyval"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "key value of binding to remove"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "modifiers"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "ModifierType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "key modifier of binding to remove"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_binding_entry_remove" gtk_binding_entry_remove :: 
    Ptr Gtk.BindingSet.BindingSet ->        -- binding_set : TInterface (Name {namespace = "Gtk", name = "BindingSet"})
    Word32 ->                               -- keyval : TBasicType TUInt
    CUInt ->                                -- modifiers : TInterface (Name {namespace = "Gdk", name = "ModifierType"})
    IO ()

-- | Remove a binding previously installed via
-- @/gtk_binding_entry_add_signal()/@ on /@bindingSet@/.
bindingEntryRemove ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Gtk.BindingSet.BindingSet
    -- ^ /@bindingSet@/: a t'GI.Gtk.Structs.BindingSet.BindingSet' to remove an entry of
    -> Word32
    -- ^ /@keyval@/: key value of binding to remove
    -> [Gdk.Flags.ModifierType]
    -- ^ /@modifiers@/: key modifier of binding to remove
    -> m ()
bindingEntryRemove bindingSet keyval modifiers = liftIO $ do
    bindingSet' <- unsafeManagedPtrGetPtr bindingSet
    let modifiers' = gflagsToWord modifiers
    gtk_binding_entry_remove bindingSet' keyval modifiers'
    touchManagedPtr bindingSet
    return ()

#if defined(ENABLE_OVERLOADING)
#endif

-- method BindingEntry::skip
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "binding_set"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "BindingSet" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBindingSet to skip an entry of"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "keyval"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "key value of binding to skip"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "modifiers"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "ModifierType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "key modifier of binding to skip"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_binding_entry_skip" gtk_binding_entry_skip :: 
    Ptr Gtk.BindingSet.BindingSet ->        -- binding_set : TInterface (Name {namespace = "Gtk", name = "BindingSet"})
    Word32 ->                               -- keyval : TBasicType TUInt
    CUInt ->                                -- modifiers : TInterface (Name {namespace = "Gdk", name = "ModifierType"})
    IO ()

-- | Install a binding on /@bindingSet@/ which causes key lookups
-- to be aborted, to prevent bindings from lower priority sets
-- to be activated.
-- 
-- /Since: 2.12/
bindingEntrySkip ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Gtk.BindingSet.BindingSet
    -- ^ /@bindingSet@/: a t'GI.Gtk.Structs.BindingSet.BindingSet' to skip an entry of
    -> Word32
    -- ^ /@keyval@/: key value of binding to skip
    -> [Gdk.Flags.ModifierType]
    -- ^ /@modifiers@/: key modifier of binding to skip
    -> m ()
bindingEntrySkip bindingSet keyval modifiers = liftIO $ do
    bindingSet' <- unsafeManagedPtrGetPtr bindingSet
    let modifiers' = gflagsToWord modifiers
    gtk_binding_entry_skip bindingSet' keyval modifiers'
    touchManagedPtr bindingSet
    return ()

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveBindingEntryMethod (t :: Symbol) (o :: *) :: * where
    ResolveBindingEntryMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveBindingEntryMethod t BindingEntry, O.OverloadedMethod info BindingEntry p) => OL.IsLabel t (BindingEntry -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveBindingEntryMethod t BindingEntry, O.OverloadedMethod info BindingEntry p, R.HasField t BindingEntry p) => R.HasField t BindingEntry p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveBindingEntryMethod t BindingEntry, O.OverloadedMethodInfo info BindingEntry) => OL.IsLabel t (O.MethodProxy info BindingEntry) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


