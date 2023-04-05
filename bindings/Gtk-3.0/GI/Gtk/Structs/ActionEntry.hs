{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Structs.ActionEntry.ActionEntry' structs are used with @/gtk_action_group_add_actions()/@ to
-- construct actions.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.ActionEntry
    ( 

-- * Exported types
    ActionEntry(..)                         ,
    newZeroActionEntry                      ,


 -- * Methods

#if defined(ENABLE_OVERLOADING)
    ResolveActionEntryMethod                ,
#endif



 -- * Properties


-- ** accelerator #attr:accelerator#
-- | The accelerator for the action, in the format understood by
--  'GI.Gtk.Functions.acceleratorParse'.

#if defined(ENABLE_OVERLOADING)
    actionEntry_accelerator                 ,
#endif
    clearActionEntryAccelerator             ,
    getActionEntryAccelerator               ,
    setActionEntryAccelerator               ,


-- ** callback #attr:callback#
-- | The function to call when the action is activated.

#if defined(ENABLE_OVERLOADING)
    actionEntry_callback                    ,
#endif
    clearActionEntryCallback                ,
    getActionEntryCallback                  ,
    setActionEntryCallback                  ,


-- ** label #attr:label#
-- | The label for the action. This field should typically be marked
--  for translation, see 'GI.Gtk.Objects.ActionGroup.actionGroupSetTranslationDomain'. If
--  /@label@/ is 'P.Nothing', the label of the stock item with id /@stockId@/ is used.

#if defined(ENABLE_OVERLOADING)
    actionEntry_label                       ,
#endif
    clearActionEntryLabel                   ,
    getActionEntryLabel                     ,
    setActionEntryLabel                     ,


-- ** name #attr:name#
-- | The name of the action.

#if defined(ENABLE_OVERLOADING)
    actionEntry_name                        ,
#endif
    clearActionEntryName                    ,
    getActionEntryName                      ,
    setActionEntryName                      ,


-- ** stockId #attr:stockId#
-- | The stock id for the action, or the name of an icon from the
--  icon theme.

#if defined(ENABLE_OVERLOADING)
    actionEntry_stockId                     ,
#endif
    clearActionEntryStockId                 ,
    getActionEntryStockId                   ,
    setActionEntryStockId                   ,


-- ** tooltip #attr:tooltip#
-- | The tooltip for the action. This field should typically be
--  marked for translation, see 'GI.Gtk.Objects.ActionGroup.actionGroupSetTranslationDomain'.

#if defined(ENABLE_OVERLOADING)
    actionEntry_tooltip                     ,
#endif
    clearActionEntryTooltip                 ,
    getActionEntryTooltip                   ,
    setActionEntryTooltip                   ,




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
newtype ActionEntry = ActionEntry (SP.ManagedPtr ActionEntry)
    deriving (Eq)

instance SP.ManagedPtrNewtype ActionEntry where
    toManagedPtr (ActionEntry p) = p

instance BoxedPtr ActionEntry where
    boxedPtrCopy = \p -> B.ManagedPtr.withManagedPtr p (copyBytes 48 >=> B.ManagedPtr.wrapPtr ActionEntry)
    boxedPtrFree = \x -> SP.withManagedPtr x SP.freeMem
instance CallocPtr ActionEntry where
    boxedPtrCalloc = callocBytes 48


-- | Construct a `ActionEntry` struct initialized to zero.
newZeroActionEntry :: MonadIO m => m ActionEntry
newZeroActionEntry = liftIO $ boxedPtrCalloc >>= wrapPtr ActionEntry

instance tag ~ 'AttrSet => Constructible ActionEntry tag where
    new _ attrs = do
        o <- newZeroActionEntry
        GI.Attributes.set o attrs
        return o


-- | Get the value of the “@name@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' actionEntry #name
-- @
getActionEntryName :: MonadIO m => ActionEntry -> m (Maybe T.Text)
getActionEntryName s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 0) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@name@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' actionEntry [ #name 'Data.GI.Base.Attributes.:=' value ]
-- @
setActionEntryName :: MonadIO m => ActionEntry -> CString -> m ()
setActionEntryName s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 0) (val :: CString)

-- | Set the value of the “@name@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #name
-- @
clearActionEntryName :: MonadIO m => ActionEntry -> m ()
clearActionEntryName s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 0) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data ActionEntryNameFieldInfo
instance AttrInfo ActionEntryNameFieldInfo where
    type AttrBaseTypeConstraint ActionEntryNameFieldInfo = (~) ActionEntry
    type AttrAllowedOps ActionEntryNameFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint ActionEntryNameFieldInfo = (~) CString
    type AttrTransferTypeConstraint ActionEntryNameFieldInfo = (~)CString
    type AttrTransferType ActionEntryNameFieldInfo = CString
    type AttrGetType ActionEntryNameFieldInfo = Maybe T.Text
    type AttrLabel ActionEntryNameFieldInfo = "name"
    type AttrOrigin ActionEntryNameFieldInfo = ActionEntry
    attrGet = getActionEntryName
    attrSet = setActionEntryName
    attrConstruct = undefined
    attrClear = clearActionEntryName
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.ActionEntry.name"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-ActionEntry.html#g:attr:name"
        })

actionEntry_name :: AttrLabelProxy "name"
actionEntry_name = AttrLabelProxy

#endif


-- | Get the value of the “@stock_id@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' actionEntry #stockId
-- @
getActionEntryStockId :: MonadIO m => ActionEntry -> m (Maybe T.Text)
getActionEntryStockId s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 8) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@stock_id@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' actionEntry [ #stockId 'Data.GI.Base.Attributes.:=' value ]
-- @
setActionEntryStockId :: MonadIO m => ActionEntry -> CString -> m ()
setActionEntryStockId s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (val :: CString)

-- | Set the value of the “@stock_id@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #stockId
-- @
clearActionEntryStockId :: MonadIO m => ActionEntry -> m ()
clearActionEntryStockId s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data ActionEntryStockIdFieldInfo
instance AttrInfo ActionEntryStockIdFieldInfo where
    type AttrBaseTypeConstraint ActionEntryStockIdFieldInfo = (~) ActionEntry
    type AttrAllowedOps ActionEntryStockIdFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint ActionEntryStockIdFieldInfo = (~) CString
    type AttrTransferTypeConstraint ActionEntryStockIdFieldInfo = (~)CString
    type AttrTransferType ActionEntryStockIdFieldInfo = CString
    type AttrGetType ActionEntryStockIdFieldInfo = Maybe T.Text
    type AttrLabel ActionEntryStockIdFieldInfo = "stock_id"
    type AttrOrigin ActionEntryStockIdFieldInfo = ActionEntry
    attrGet = getActionEntryStockId
    attrSet = setActionEntryStockId
    attrConstruct = undefined
    attrClear = clearActionEntryStockId
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.ActionEntry.stockId"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-ActionEntry.html#g:attr:stockId"
        })

actionEntry_stockId :: AttrLabelProxy "stockId"
actionEntry_stockId = AttrLabelProxy

#endif


-- | Get the value of the “@label@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' actionEntry #label
-- @
getActionEntryLabel :: MonadIO m => ActionEntry -> m (Maybe T.Text)
getActionEntryLabel s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 16) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@label@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' actionEntry [ #label 'Data.GI.Base.Attributes.:=' value ]
-- @
setActionEntryLabel :: MonadIO m => ActionEntry -> CString -> m ()
setActionEntryLabel s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 16) (val :: CString)

-- | Set the value of the “@label@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #label
-- @
clearActionEntryLabel :: MonadIO m => ActionEntry -> m ()
clearActionEntryLabel s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 16) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data ActionEntryLabelFieldInfo
instance AttrInfo ActionEntryLabelFieldInfo where
    type AttrBaseTypeConstraint ActionEntryLabelFieldInfo = (~) ActionEntry
    type AttrAllowedOps ActionEntryLabelFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint ActionEntryLabelFieldInfo = (~) CString
    type AttrTransferTypeConstraint ActionEntryLabelFieldInfo = (~)CString
    type AttrTransferType ActionEntryLabelFieldInfo = CString
    type AttrGetType ActionEntryLabelFieldInfo = Maybe T.Text
    type AttrLabel ActionEntryLabelFieldInfo = "label"
    type AttrOrigin ActionEntryLabelFieldInfo = ActionEntry
    attrGet = getActionEntryLabel
    attrSet = setActionEntryLabel
    attrConstruct = undefined
    attrClear = clearActionEntryLabel
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.ActionEntry.label"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-ActionEntry.html#g:attr:label"
        })

actionEntry_label :: AttrLabelProxy "label"
actionEntry_label = AttrLabelProxy

#endif


-- | Get the value of the “@accelerator@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' actionEntry #accelerator
-- @
getActionEntryAccelerator :: MonadIO m => ActionEntry -> m (Maybe T.Text)
getActionEntryAccelerator s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 24) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@accelerator@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' actionEntry [ #accelerator 'Data.GI.Base.Attributes.:=' value ]
-- @
setActionEntryAccelerator :: MonadIO m => ActionEntry -> CString -> m ()
setActionEntryAccelerator s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 24) (val :: CString)

-- | Set the value of the “@accelerator@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #accelerator
-- @
clearActionEntryAccelerator :: MonadIO m => ActionEntry -> m ()
clearActionEntryAccelerator s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 24) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data ActionEntryAcceleratorFieldInfo
instance AttrInfo ActionEntryAcceleratorFieldInfo where
    type AttrBaseTypeConstraint ActionEntryAcceleratorFieldInfo = (~) ActionEntry
    type AttrAllowedOps ActionEntryAcceleratorFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint ActionEntryAcceleratorFieldInfo = (~) CString
    type AttrTransferTypeConstraint ActionEntryAcceleratorFieldInfo = (~)CString
    type AttrTransferType ActionEntryAcceleratorFieldInfo = CString
    type AttrGetType ActionEntryAcceleratorFieldInfo = Maybe T.Text
    type AttrLabel ActionEntryAcceleratorFieldInfo = "accelerator"
    type AttrOrigin ActionEntryAcceleratorFieldInfo = ActionEntry
    attrGet = getActionEntryAccelerator
    attrSet = setActionEntryAccelerator
    attrConstruct = undefined
    attrClear = clearActionEntryAccelerator
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.ActionEntry.accelerator"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-ActionEntry.html#g:attr:accelerator"
        })

actionEntry_accelerator :: AttrLabelProxy "accelerator"
actionEntry_accelerator = AttrLabelProxy

#endif


-- | Get the value of the “@tooltip@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' actionEntry #tooltip
-- @
getActionEntryTooltip :: MonadIO m => ActionEntry -> m (Maybe T.Text)
getActionEntryTooltip s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 32) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@tooltip@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' actionEntry [ #tooltip 'Data.GI.Base.Attributes.:=' value ]
-- @
setActionEntryTooltip :: MonadIO m => ActionEntry -> CString -> m ()
setActionEntryTooltip s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 32) (val :: CString)

-- | Set the value of the “@tooltip@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #tooltip
-- @
clearActionEntryTooltip :: MonadIO m => ActionEntry -> m ()
clearActionEntryTooltip s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 32) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data ActionEntryTooltipFieldInfo
instance AttrInfo ActionEntryTooltipFieldInfo where
    type AttrBaseTypeConstraint ActionEntryTooltipFieldInfo = (~) ActionEntry
    type AttrAllowedOps ActionEntryTooltipFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint ActionEntryTooltipFieldInfo = (~) CString
    type AttrTransferTypeConstraint ActionEntryTooltipFieldInfo = (~)CString
    type AttrTransferType ActionEntryTooltipFieldInfo = CString
    type AttrGetType ActionEntryTooltipFieldInfo = Maybe T.Text
    type AttrLabel ActionEntryTooltipFieldInfo = "tooltip"
    type AttrOrigin ActionEntryTooltipFieldInfo = ActionEntry
    attrGet = getActionEntryTooltip
    attrSet = setActionEntryTooltip
    attrConstruct = undefined
    attrClear = clearActionEntryTooltip
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.ActionEntry.tooltip"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-ActionEntry.html#g:attr:tooltip"
        })

actionEntry_tooltip :: AttrLabelProxy "tooltip"
actionEntry_tooltip = AttrLabelProxy

#endif


-- | Get the value of the “@callback@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' actionEntry #callback
-- @
getActionEntryCallback :: MonadIO m => ActionEntry -> m (Maybe GObject.Callbacks.Callback)
getActionEntryCallback s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 40) :: IO (FunPtr GObject.Callbacks.C_Callback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = GObject.Callbacks.dynamic_Callback val'
        return val''
    return result

-- | Set the value of the “@callback@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' actionEntry [ #callback 'Data.GI.Base.Attributes.:=' value ]
-- @
setActionEntryCallback :: MonadIO m => ActionEntry -> FunPtr GObject.Callbacks.C_Callback -> m ()
setActionEntryCallback s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 40) (val :: FunPtr GObject.Callbacks.C_Callback)

-- | Set the value of the “@callback@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #callback
-- @
clearActionEntryCallback :: MonadIO m => ActionEntry -> m ()
clearActionEntryCallback s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 40) (FP.nullFunPtr :: FunPtr GObject.Callbacks.C_Callback)

#if defined(ENABLE_OVERLOADING)
data ActionEntryCallbackFieldInfo
instance AttrInfo ActionEntryCallbackFieldInfo where
    type AttrBaseTypeConstraint ActionEntryCallbackFieldInfo = (~) ActionEntry
    type AttrAllowedOps ActionEntryCallbackFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint ActionEntryCallbackFieldInfo = (~) (FunPtr GObject.Callbacks.C_Callback)
    type AttrTransferTypeConstraint ActionEntryCallbackFieldInfo = (~)GObject.Callbacks.Callback
    type AttrTransferType ActionEntryCallbackFieldInfo = (FunPtr GObject.Callbacks.C_Callback)
    type AttrGetType ActionEntryCallbackFieldInfo = Maybe GObject.Callbacks.Callback
    type AttrLabel ActionEntryCallbackFieldInfo = "callback"
    type AttrOrigin ActionEntryCallbackFieldInfo = ActionEntry
    attrGet = getActionEntryCallback
    attrSet = setActionEntryCallback
    attrConstruct = undefined
    attrClear = clearActionEntryCallback
    attrTransfer _ v = do
        GObject.Callbacks.mk_Callback (GObject.Callbacks.wrap_Callback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.ActionEntry.callback"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-ActionEntry.html#g:attr:callback"
        })

actionEntry_callback :: AttrLabelProxy "callback"
actionEntry_callback = AttrLabelProxy

#endif



#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList ActionEntry
type instance O.AttributeList ActionEntry = ActionEntryAttributeList
type ActionEntryAttributeList = ('[ '("name", ActionEntryNameFieldInfo), '("stockId", ActionEntryStockIdFieldInfo), '("label", ActionEntryLabelFieldInfo), '("accelerator", ActionEntryAcceleratorFieldInfo), '("tooltip", ActionEntryTooltipFieldInfo), '("callback", ActionEntryCallbackFieldInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveActionEntryMethod (t :: Symbol) (o :: *) :: * where
    ResolveActionEntryMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveActionEntryMethod t ActionEntry, O.OverloadedMethod info ActionEntry p) => OL.IsLabel t (ActionEntry -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveActionEntryMethod t ActionEntry, O.OverloadedMethod info ActionEntry p, R.HasField t ActionEntry p) => R.HasField t ActionEntry p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveActionEntryMethod t ActionEntry, O.OverloadedMethodInfo info ActionEntry) => OL.IsLabel t (O.MethodProxy info ActionEntry) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


