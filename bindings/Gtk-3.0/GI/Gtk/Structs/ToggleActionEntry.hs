{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Structs.ToggleActionEntry.ToggleActionEntry' structs are used with
-- @/gtk_action_group_add_toggle_actions()/@ to construct toggle actions.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.ToggleActionEntry
    ( 

-- * Exported types
    ToggleActionEntry(..)                   ,
    newZeroToggleActionEntry                ,


 -- * Methods

#if defined(ENABLE_OVERLOADING)
    ResolveToggleActionEntryMethod          ,
#endif



 -- * Properties


-- ** accelerator #attr:accelerator#
-- | The accelerator for the action, in the format understood by
--  'GI.Gtk.Functions.acceleratorParse'.

    clearToggleActionEntryAccelerator       ,
    getToggleActionEntryAccelerator         ,
    setToggleActionEntryAccelerator         ,
#if defined(ENABLE_OVERLOADING)
    toggleActionEntry_accelerator           ,
#endif


-- ** callback #attr:callback#
-- | The function to call when the action is activated.

    clearToggleActionEntryCallback          ,
    getToggleActionEntryCallback            ,
    setToggleActionEntryCallback            ,
#if defined(ENABLE_OVERLOADING)
    toggleActionEntry_callback              ,
#endif


-- ** isActive #attr:isActive#
-- | The initial state of the toggle action.

    getToggleActionEntryIsActive            ,
    setToggleActionEntryIsActive            ,
#if defined(ENABLE_OVERLOADING)
    toggleActionEntry_isActive              ,
#endif


-- ** label #attr:label#
-- | The label for the action. This field should typically be marked
--  for translation, see 'GI.Gtk.Objects.ActionGroup.actionGroupSetTranslationDomain'.

    clearToggleActionEntryLabel             ,
    getToggleActionEntryLabel               ,
    setToggleActionEntryLabel               ,
#if defined(ENABLE_OVERLOADING)
    toggleActionEntry_label                 ,
#endif


-- ** name #attr:name#
-- | The name of the action.

    clearToggleActionEntryName              ,
    getToggleActionEntryName                ,
    setToggleActionEntryName                ,
#if defined(ENABLE_OVERLOADING)
    toggleActionEntry_name                  ,
#endif


-- ** stockId #attr:stockId#
-- | The stock id for the action, or the name of an icon from the
--  icon theme.

    clearToggleActionEntryStockId           ,
    getToggleActionEntryStockId             ,
    setToggleActionEntryStockId             ,
#if defined(ENABLE_OVERLOADING)
    toggleActionEntry_stockId               ,
#endif


-- ** tooltip #attr:tooltip#
-- | The tooltip for the action. This field should typically be
--  marked for translation, see 'GI.Gtk.Objects.ActionGroup.actionGroupSetTranslationDomain'.

    clearToggleActionEntryTooltip           ,
    getToggleActionEntryTooltip             ,
    setToggleActionEntryTooltip             ,
#if defined(ENABLE_OVERLOADING)
    toggleActionEntry_tooltip               ,
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

import qualified GI.GObject.Callbacks as GObject.Callbacks

-- | Memory-managed wrapper type.
newtype ToggleActionEntry = ToggleActionEntry (SP.ManagedPtr ToggleActionEntry)
    deriving (Eq)

instance SP.ManagedPtrNewtype ToggleActionEntry where
    toManagedPtr (ToggleActionEntry p) = p

instance BoxedPtr ToggleActionEntry where
    boxedPtrCopy = \p -> B.ManagedPtr.withManagedPtr p (copyBytes 56 >=> B.ManagedPtr.wrapPtr ToggleActionEntry)
    boxedPtrFree = \x -> SP.withManagedPtr x SP.freeMem
instance CallocPtr ToggleActionEntry where
    boxedPtrCalloc = callocBytes 56


-- | Construct a `ToggleActionEntry` struct initialized to zero.
newZeroToggleActionEntry :: MonadIO m => m ToggleActionEntry
newZeroToggleActionEntry = liftIO $ boxedPtrCalloc >>= wrapPtr ToggleActionEntry

instance tag ~ 'AttrSet => Constructible ToggleActionEntry tag where
    new _ attrs = do
        o <- newZeroToggleActionEntry
        GI.Attributes.set o attrs
        return o


-- | Get the value of the “@name@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' toggleActionEntry #name
-- @
getToggleActionEntryName :: MonadIO m => ToggleActionEntry -> m (Maybe T.Text)
getToggleActionEntryName s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 0) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@name@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' toggleActionEntry [ #name 'Data.GI.Base.Attributes.:=' value ]
-- @
setToggleActionEntryName :: MonadIO m => ToggleActionEntry -> CString -> m ()
setToggleActionEntryName s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 0) (val :: CString)

-- | Set the value of the “@name@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #name
-- @
clearToggleActionEntryName :: MonadIO m => ToggleActionEntry -> m ()
clearToggleActionEntryName s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 0) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data ToggleActionEntryNameFieldInfo
instance AttrInfo ToggleActionEntryNameFieldInfo where
    type AttrBaseTypeConstraint ToggleActionEntryNameFieldInfo = (~) ToggleActionEntry
    type AttrAllowedOps ToggleActionEntryNameFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint ToggleActionEntryNameFieldInfo = (~) CString
    type AttrTransferTypeConstraint ToggleActionEntryNameFieldInfo = (~)CString
    type AttrTransferType ToggleActionEntryNameFieldInfo = CString
    type AttrGetType ToggleActionEntryNameFieldInfo = Maybe T.Text
    type AttrLabel ToggleActionEntryNameFieldInfo = "name"
    type AttrOrigin ToggleActionEntryNameFieldInfo = ToggleActionEntry
    attrGet = getToggleActionEntryName
    attrSet = setToggleActionEntryName
    attrConstruct = undefined
    attrClear = clearToggleActionEntryName
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.ToggleActionEntry.name"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-ToggleActionEntry.html#g:attr:name"
        })

toggleActionEntry_name :: AttrLabelProxy "name"
toggleActionEntry_name = AttrLabelProxy

#endif


-- | Get the value of the “@stock_id@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' toggleActionEntry #stockId
-- @
getToggleActionEntryStockId :: MonadIO m => ToggleActionEntry -> m (Maybe T.Text)
getToggleActionEntryStockId s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 8) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@stock_id@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' toggleActionEntry [ #stockId 'Data.GI.Base.Attributes.:=' value ]
-- @
setToggleActionEntryStockId :: MonadIO m => ToggleActionEntry -> CString -> m ()
setToggleActionEntryStockId s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (val :: CString)

-- | Set the value of the “@stock_id@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #stockId
-- @
clearToggleActionEntryStockId :: MonadIO m => ToggleActionEntry -> m ()
clearToggleActionEntryStockId s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data ToggleActionEntryStockIdFieldInfo
instance AttrInfo ToggleActionEntryStockIdFieldInfo where
    type AttrBaseTypeConstraint ToggleActionEntryStockIdFieldInfo = (~) ToggleActionEntry
    type AttrAllowedOps ToggleActionEntryStockIdFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint ToggleActionEntryStockIdFieldInfo = (~) CString
    type AttrTransferTypeConstraint ToggleActionEntryStockIdFieldInfo = (~)CString
    type AttrTransferType ToggleActionEntryStockIdFieldInfo = CString
    type AttrGetType ToggleActionEntryStockIdFieldInfo = Maybe T.Text
    type AttrLabel ToggleActionEntryStockIdFieldInfo = "stock_id"
    type AttrOrigin ToggleActionEntryStockIdFieldInfo = ToggleActionEntry
    attrGet = getToggleActionEntryStockId
    attrSet = setToggleActionEntryStockId
    attrConstruct = undefined
    attrClear = clearToggleActionEntryStockId
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.ToggleActionEntry.stockId"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-ToggleActionEntry.html#g:attr:stockId"
        })

toggleActionEntry_stockId :: AttrLabelProxy "stockId"
toggleActionEntry_stockId = AttrLabelProxy

#endif


-- | Get the value of the “@label@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' toggleActionEntry #label
-- @
getToggleActionEntryLabel :: MonadIO m => ToggleActionEntry -> m (Maybe T.Text)
getToggleActionEntryLabel s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 16) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@label@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' toggleActionEntry [ #label 'Data.GI.Base.Attributes.:=' value ]
-- @
setToggleActionEntryLabel :: MonadIO m => ToggleActionEntry -> CString -> m ()
setToggleActionEntryLabel s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 16) (val :: CString)

-- | Set the value of the “@label@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #label
-- @
clearToggleActionEntryLabel :: MonadIO m => ToggleActionEntry -> m ()
clearToggleActionEntryLabel s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 16) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data ToggleActionEntryLabelFieldInfo
instance AttrInfo ToggleActionEntryLabelFieldInfo where
    type AttrBaseTypeConstraint ToggleActionEntryLabelFieldInfo = (~) ToggleActionEntry
    type AttrAllowedOps ToggleActionEntryLabelFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint ToggleActionEntryLabelFieldInfo = (~) CString
    type AttrTransferTypeConstraint ToggleActionEntryLabelFieldInfo = (~)CString
    type AttrTransferType ToggleActionEntryLabelFieldInfo = CString
    type AttrGetType ToggleActionEntryLabelFieldInfo = Maybe T.Text
    type AttrLabel ToggleActionEntryLabelFieldInfo = "label"
    type AttrOrigin ToggleActionEntryLabelFieldInfo = ToggleActionEntry
    attrGet = getToggleActionEntryLabel
    attrSet = setToggleActionEntryLabel
    attrConstruct = undefined
    attrClear = clearToggleActionEntryLabel
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.ToggleActionEntry.label"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-ToggleActionEntry.html#g:attr:label"
        })

toggleActionEntry_label :: AttrLabelProxy "label"
toggleActionEntry_label = AttrLabelProxy

#endif


-- | Get the value of the “@accelerator@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' toggleActionEntry #accelerator
-- @
getToggleActionEntryAccelerator :: MonadIO m => ToggleActionEntry -> m (Maybe T.Text)
getToggleActionEntryAccelerator s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 24) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@accelerator@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' toggleActionEntry [ #accelerator 'Data.GI.Base.Attributes.:=' value ]
-- @
setToggleActionEntryAccelerator :: MonadIO m => ToggleActionEntry -> CString -> m ()
setToggleActionEntryAccelerator s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 24) (val :: CString)

-- | Set the value of the “@accelerator@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #accelerator
-- @
clearToggleActionEntryAccelerator :: MonadIO m => ToggleActionEntry -> m ()
clearToggleActionEntryAccelerator s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 24) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data ToggleActionEntryAcceleratorFieldInfo
instance AttrInfo ToggleActionEntryAcceleratorFieldInfo where
    type AttrBaseTypeConstraint ToggleActionEntryAcceleratorFieldInfo = (~) ToggleActionEntry
    type AttrAllowedOps ToggleActionEntryAcceleratorFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint ToggleActionEntryAcceleratorFieldInfo = (~) CString
    type AttrTransferTypeConstraint ToggleActionEntryAcceleratorFieldInfo = (~)CString
    type AttrTransferType ToggleActionEntryAcceleratorFieldInfo = CString
    type AttrGetType ToggleActionEntryAcceleratorFieldInfo = Maybe T.Text
    type AttrLabel ToggleActionEntryAcceleratorFieldInfo = "accelerator"
    type AttrOrigin ToggleActionEntryAcceleratorFieldInfo = ToggleActionEntry
    attrGet = getToggleActionEntryAccelerator
    attrSet = setToggleActionEntryAccelerator
    attrConstruct = undefined
    attrClear = clearToggleActionEntryAccelerator
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.ToggleActionEntry.accelerator"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-ToggleActionEntry.html#g:attr:accelerator"
        })

toggleActionEntry_accelerator :: AttrLabelProxy "accelerator"
toggleActionEntry_accelerator = AttrLabelProxy

#endif


-- | Get the value of the “@tooltip@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' toggleActionEntry #tooltip
-- @
getToggleActionEntryTooltip :: MonadIO m => ToggleActionEntry -> m (Maybe T.Text)
getToggleActionEntryTooltip s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 32) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@tooltip@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' toggleActionEntry [ #tooltip 'Data.GI.Base.Attributes.:=' value ]
-- @
setToggleActionEntryTooltip :: MonadIO m => ToggleActionEntry -> CString -> m ()
setToggleActionEntryTooltip s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 32) (val :: CString)

-- | Set the value of the “@tooltip@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #tooltip
-- @
clearToggleActionEntryTooltip :: MonadIO m => ToggleActionEntry -> m ()
clearToggleActionEntryTooltip s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 32) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data ToggleActionEntryTooltipFieldInfo
instance AttrInfo ToggleActionEntryTooltipFieldInfo where
    type AttrBaseTypeConstraint ToggleActionEntryTooltipFieldInfo = (~) ToggleActionEntry
    type AttrAllowedOps ToggleActionEntryTooltipFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint ToggleActionEntryTooltipFieldInfo = (~) CString
    type AttrTransferTypeConstraint ToggleActionEntryTooltipFieldInfo = (~)CString
    type AttrTransferType ToggleActionEntryTooltipFieldInfo = CString
    type AttrGetType ToggleActionEntryTooltipFieldInfo = Maybe T.Text
    type AttrLabel ToggleActionEntryTooltipFieldInfo = "tooltip"
    type AttrOrigin ToggleActionEntryTooltipFieldInfo = ToggleActionEntry
    attrGet = getToggleActionEntryTooltip
    attrSet = setToggleActionEntryTooltip
    attrConstruct = undefined
    attrClear = clearToggleActionEntryTooltip
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.ToggleActionEntry.tooltip"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-ToggleActionEntry.html#g:attr:tooltip"
        })

toggleActionEntry_tooltip :: AttrLabelProxy "tooltip"
toggleActionEntry_tooltip = AttrLabelProxy

#endif


-- | Get the value of the “@callback@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' toggleActionEntry #callback
-- @
getToggleActionEntryCallback :: MonadIO m => ToggleActionEntry -> m (Maybe GObject.Callbacks.Callback)
getToggleActionEntryCallback s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 40) :: IO (FunPtr GObject.Callbacks.C_Callback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = GObject.Callbacks.dynamic_Callback val'
        return val''
    return result

-- | Set the value of the “@callback@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' toggleActionEntry [ #callback 'Data.GI.Base.Attributes.:=' value ]
-- @
setToggleActionEntryCallback :: MonadIO m => ToggleActionEntry -> FunPtr GObject.Callbacks.C_Callback -> m ()
setToggleActionEntryCallback s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 40) (val :: FunPtr GObject.Callbacks.C_Callback)

-- | Set the value of the “@callback@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #callback
-- @
clearToggleActionEntryCallback :: MonadIO m => ToggleActionEntry -> m ()
clearToggleActionEntryCallback s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 40) (FP.nullFunPtr :: FunPtr GObject.Callbacks.C_Callback)

#if defined(ENABLE_OVERLOADING)
data ToggleActionEntryCallbackFieldInfo
instance AttrInfo ToggleActionEntryCallbackFieldInfo where
    type AttrBaseTypeConstraint ToggleActionEntryCallbackFieldInfo = (~) ToggleActionEntry
    type AttrAllowedOps ToggleActionEntryCallbackFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint ToggleActionEntryCallbackFieldInfo = (~) (FunPtr GObject.Callbacks.C_Callback)
    type AttrTransferTypeConstraint ToggleActionEntryCallbackFieldInfo = (~)GObject.Callbacks.Callback
    type AttrTransferType ToggleActionEntryCallbackFieldInfo = (FunPtr GObject.Callbacks.C_Callback)
    type AttrGetType ToggleActionEntryCallbackFieldInfo = Maybe GObject.Callbacks.Callback
    type AttrLabel ToggleActionEntryCallbackFieldInfo = "callback"
    type AttrOrigin ToggleActionEntryCallbackFieldInfo = ToggleActionEntry
    attrGet = getToggleActionEntryCallback
    attrSet = setToggleActionEntryCallback
    attrConstruct = undefined
    attrClear = clearToggleActionEntryCallback
    attrTransfer _ v = do
        GObject.Callbacks.mk_Callback (GObject.Callbacks.wrap_Callback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.ToggleActionEntry.callback"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-ToggleActionEntry.html#g:attr:callback"
        })

toggleActionEntry_callback :: AttrLabelProxy "callback"
toggleActionEntry_callback = AttrLabelProxy

#endif


-- | Get the value of the “@is_active@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' toggleActionEntry #isActive
-- @
getToggleActionEntryIsActive :: MonadIO m => ToggleActionEntry -> m Bool
getToggleActionEntryIsActive s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 48) :: IO CInt
    let val' = (/= 0) val
    return val'

-- | Set the value of the “@is_active@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' toggleActionEntry [ #isActive 'Data.GI.Base.Attributes.:=' value ]
-- @
setToggleActionEntryIsActive :: MonadIO m => ToggleActionEntry -> Bool -> m ()
setToggleActionEntryIsActive s val = liftIO $ withManagedPtr s $ \ptr -> do
    let val' = (fromIntegral . fromEnum) val
    poke (ptr `plusPtr` 48) (val' :: CInt)

#if defined(ENABLE_OVERLOADING)
data ToggleActionEntryIsActiveFieldInfo
instance AttrInfo ToggleActionEntryIsActiveFieldInfo where
    type AttrBaseTypeConstraint ToggleActionEntryIsActiveFieldInfo = (~) ToggleActionEntry
    type AttrAllowedOps ToggleActionEntryIsActiveFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint ToggleActionEntryIsActiveFieldInfo = (~) Bool
    type AttrTransferTypeConstraint ToggleActionEntryIsActiveFieldInfo = (~)Bool
    type AttrTransferType ToggleActionEntryIsActiveFieldInfo = Bool
    type AttrGetType ToggleActionEntryIsActiveFieldInfo = Bool
    type AttrLabel ToggleActionEntryIsActiveFieldInfo = "is_active"
    type AttrOrigin ToggleActionEntryIsActiveFieldInfo = ToggleActionEntry
    attrGet = getToggleActionEntryIsActive
    attrSet = setToggleActionEntryIsActive
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.ToggleActionEntry.isActive"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-ToggleActionEntry.html#g:attr:isActive"
        })

toggleActionEntry_isActive :: AttrLabelProxy "isActive"
toggleActionEntry_isActive = AttrLabelProxy

#endif



#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList ToggleActionEntry
type instance O.AttributeList ToggleActionEntry = ToggleActionEntryAttributeList
type ToggleActionEntryAttributeList = ('[ '("name", ToggleActionEntryNameFieldInfo), '("stockId", ToggleActionEntryStockIdFieldInfo), '("label", ToggleActionEntryLabelFieldInfo), '("accelerator", ToggleActionEntryAcceleratorFieldInfo), '("tooltip", ToggleActionEntryTooltipFieldInfo), '("callback", ToggleActionEntryCallbackFieldInfo), '("isActive", ToggleActionEntryIsActiveFieldInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveToggleActionEntryMethod (t :: Symbol) (o :: *) :: * where
    ResolveToggleActionEntryMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveToggleActionEntryMethod t ToggleActionEntry, O.OverloadedMethod info ToggleActionEntry p) => OL.IsLabel t (ToggleActionEntry -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveToggleActionEntryMethod t ToggleActionEntry, O.OverloadedMethod info ToggleActionEntry p, R.HasField t ToggleActionEntry p) => R.HasField t ToggleActionEntry p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveToggleActionEntryMethod t ToggleActionEntry, O.OverloadedMethodInfo info ToggleActionEntry) => OL.IsLabel t (O.MethodProxy info ToggleActionEntry) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


