{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Structs.RadioActionEntry.RadioActionEntry' structs are used with
-- @/gtk_action_group_add_radio_actions()/@ to construct groups of radio actions.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.RadioActionEntry
    ( 

-- * Exported types
    RadioActionEntry(..)                    ,
    newZeroRadioActionEntry                 ,


 -- * Methods

#if defined(ENABLE_OVERLOADING)
    ResolveRadioActionEntryMethod           ,
#endif



 -- * Properties


-- ** accelerator #attr:accelerator#
-- | The accelerator for the action, in the format understood by
--  'GI.Gtk.Functions.acceleratorParse'.

    clearRadioActionEntryAccelerator        ,
    getRadioActionEntryAccelerator          ,
#if defined(ENABLE_OVERLOADING)
    radioActionEntry_accelerator            ,
#endif
    setRadioActionEntryAccelerator          ,


-- ** label #attr:label#
-- | The label for the action. This field should typically be marked
--  for translation, see 'GI.Gtk.Objects.ActionGroup.actionGroupSetTranslationDomain'.

    clearRadioActionEntryLabel              ,
    getRadioActionEntryLabel                ,
#if defined(ENABLE_OVERLOADING)
    radioActionEntry_label                  ,
#endif
    setRadioActionEntryLabel                ,


-- ** name #attr:name#
-- | The name of the action.

    clearRadioActionEntryName               ,
    getRadioActionEntryName                 ,
#if defined(ENABLE_OVERLOADING)
    radioActionEntry_name                   ,
#endif
    setRadioActionEntryName                 ,


-- ** stockId #attr:stockId#
-- | The stock id for the action, or the name of an icon from the
--  icon theme.

    clearRadioActionEntryStockId            ,
    getRadioActionEntryStockId              ,
#if defined(ENABLE_OVERLOADING)
    radioActionEntry_stockId                ,
#endif
    setRadioActionEntryStockId              ,


-- ** tooltip #attr:tooltip#
-- | The tooltip for the action. This field should typically be
--  marked for translation, see 'GI.Gtk.Objects.ActionGroup.actionGroupSetTranslationDomain'.

    clearRadioActionEntryTooltip            ,
    getRadioActionEntryTooltip              ,
#if defined(ENABLE_OVERLOADING)
    radioActionEntry_tooltip                ,
#endif
    setRadioActionEntryTooltip              ,


-- ** value #attr:value#
-- | The value to set on the radio action. See
--  'GI.Gtk.Objects.RadioAction.radioActionGetCurrentValue'.

    getRadioActionEntryValue                ,
#if defined(ENABLE_OVERLOADING)
    radioActionEntry_value                  ,
#endif
    setRadioActionEntryValue                ,




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
newtype RadioActionEntry = RadioActionEntry (SP.ManagedPtr RadioActionEntry)
    deriving (Eq)

instance SP.ManagedPtrNewtype RadioActionEntry where
    toManagedPtr (RadioActionEntry p) = p

instance BoxedPtr RadioActionEntry where
    boxedPtrCopy = \p -> B.ManagedPtr.withManagedPtr p (copyBytes 48 >=> B.ManagedPtr.wrapPtr RadioActionEntry)
    boxedPtrFree = \x -> SP.withManagedPtr x SP.freeMem
instance CallocPtr RadioActionEntry where
    boxedPtrCalloc = callocBytes 48


-- | Construct a `RadioActionEntry` struct initialized to zero.
newZeroRadioActionEntry :: MonadIO m => m RadioActionEntry
newZeroRadioActionEntry = liftIO $ boxedPtrCalloc >>= wrapPtr RadioActionEntry

instance tag ~ 'AttrSet => Constructible RadioActionEntry tag where
    new _ attrs = do
        o <- newZeroRadioActionEntry
        GI.Attributes.set o attrs
        return o


-- | Get the value of the “@name@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' radioActionEntry #name
-- @
getRadioActionEntryName :: MonadIO m => RadioActionEntry -> m (Maybe T.Text)
getRadioActionEntryName s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 0) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@name@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' radioActionEntry [ #name 'Data.GI.Base.Attributes.:=' value ]
-- @
setRadioActionEntryName :: MonadIO m => RadioActionEntry -> CString -> m ()
setRadioActionEntryName s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 0) (val :: CString)

-- | Set the value of the “@name@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #name
-- @
clearRadioActionEntryName :: MonadIO m => RadioActionEntry -> m ()
clearRadioActionEntryName s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 0) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data RadioActionEntryNameFieldInfo
instance AttrInfo RadioActionEntryNameFieldInfo where
    type AttrBaseTypeConstraint RadioActionEntryNameFieldInfo = (~) RadioActionEntry
    type AttrAllowedOps RadioActionEntryNameFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint RadioActionEntryNameFieldInfo = (~) CString
    type AttrTransferTypeConstraint RadioActionEntryNameFieldInfo = (~)CString
    type AttrTransferType RadioActionEntryNameFieldInfo = CString
    type AttrGetType RadioActionEntryNameFieldInfo = Maybe T.Text
    type AttrLabel RadioActionEntryNameFieldInfo = "name"
    type AttrOrigin RadioActionEntryNameFieldInfo = RadioActionEntry
    attrGet = getRadioActionEntryName
    attrSet = setRadioActionEntryName
    attrConstruct = undefined
    attrClear = clearRadioActionEntryName
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RadioActionEntry.name"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RadioActionEntry.html#g:attr:name"
        })

radioActionEntry_name :: AttrLabelProxy "name"
radioActionEntry_name = AttrLabelProxy

#endif


-- | Get the value of the “@stock_id@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' radioActionEntry #stockId
-- @
getRadioActionEntryStockId :: MonadIO m => RadioActionEntry -> m (Maybe T.Text)
getRadioActionEntryStockId s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 8) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@stock_id@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' radioActionEntry [ #stockId 'Data.GI.Base.Attributes.:=' value ]
-- @
setRadioActionEntryStockId :: MonadIO m => RadioActionEntry -> CString -> m ()
setRadioActionEntryStockId s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (val :: CString)

-- | Set the value of the “@stock_id@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #stockId
-- @
clearRadioActionEntryStockId :: MonadIO m => RadioActionEntry -> m ()
clearRadioActionEntryStockId s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data RadioActionEntryStockIdFieldInfo
instance AttrInfo RadioActionEntryStockIdFieldInfo where
    type AttrBaseTypeConstraint RadioActionEntryStockIdFieldInfo = (~) RadioActionEntry
    type AttrAllowedOps RadioActionEntryStockIdFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint RadioActionEntryStockIdFieldInfo = (~) CString
    type AttrTransferTypeConstraint RadioActionEntryStockIdFieldInfo = (~)CString
    type AttrTransferType RadioActionEntryStockIdFieldInfo = CString
    type AttrGetType RadioActionEntryStockIdFieldInfo = Maybe T.Text
    type AttrLabel RadioActionEntryStockIdFieldInfo = "stock_id"
    type AttrOrigin RadioActionEntryStockIdFieldInfo = RadioActionEntry
    attrGet = getRadioActionEntryStockId
    attrSet = setRadioActionEntryStockId
    attrConstruct = undefined
    attrClear = clearRadioActionEntryStockId
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RadioActionEntry.stockId"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RadioActionEntry.html#g:attr:stockId"
        })

radioActionEntry_stockId :: AttrLabelProxy "stockId"
radioActionEntry_stockId = AttrLabelProxy

#endif


-- | Get the value of the “@label@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' radioActionEntry #label
-- @
getRadioActionEntryLabel :: MonadIO m => RadioActionEntry -> m (Maybe T.Text)
getRadioActionEntryLabel s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 16) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@label@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' radioActionEntry [ #label 'Data.GI.Base.Attributes.:=' value ]
-- @
setRadioActionEntryLabel :: MonadIO m => RadioActionEntry -> CString -> m ()
setRadioActionEntryLabel s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 16) (val :: CString)

-- | Set the value of the “@label@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #label
-- @
clearRadioActionEntryLabel :: MonadIO m => RadioActionEntry -> m ()
clearRadioActionEntryLabel s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 16) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data RadioActionEntryLabelFieldInfo
instance AttrInfo RadioActionEntryLabelFieldInfo where
    type AttrBaseTypeConstraint RadioActionEntryLabelFieldInfo = (~) RadioActionEntry
    type AttrAllowedOps RadioActionEntryLabelFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint RadioActionEntryLabelFieldInfo = (~) CString
    type AttrTransferTypeConstraint RadioActionEntryLabelFieldInfo = (~)CString
    type AttrTransferType RadioActionEntryLabelFieldInfo = CString
    type AttrGetType RadioActionEntryLabelFieldInfo = Maybe T.Text
    type AttrLabel RadioActionEntryLabelFieldInfo = "label"
    type AttrOrigin RadioActionEntryLabelFieldInfo = RadioActionEntry
    attrGet = getRadioActionEntryLabel
    attrSet = setRadioActionEntryLabel
    attrConstruct = undefined
    attrClear = clearRadioActionEntryLabel
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RadioActionEntry.label"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RadioActionEntry.html#g:attr:label"
        })

radioActionEntry_label :: AttrLabelProxy "label"
radioActionEntry_label = AttrLabelProxy

#endif


-- | Get the value of the “@accelerator@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' radioActionEntry #accelerator
-- @
getRadioActionEntryAccelerator :: MonadIO m => RadioActionEntry -> m (Maybe T.Text)
getRadioActionEntryAccelerator s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 24) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@accelerator@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' radioActionEntry [ #accelerator 'Data.GI.Base.Attributes.:=' value ]
-- @
setRadioActionEntryAccelerator :: MonadIO m => RadioActionEntry -> CString -> m ()
setRadioActionEntryAccelerator s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 24) (val :: CString)

-- | Set the value of the “@accelerator@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #accelerator
-- @
clearRadioActionEntryAccelerator :: MonadIO m => RadioActionEntry -> m ()
clearRadioActionEntryAccelerator s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 24) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data RadioActionEntryAcceleratorFieldInfo
instance AttrInfo RadioActionEntryAcceleratorFieldInfo where
    type AttrBaseTypeConstraint RadioActionEntryAcceleratorFieldInfo = (~) RadioActionEntry
    type AttrAllowedOps RadioActionEntryAcceleratorFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint RadioActionEntryAcceleratorFieldInfo = (~) CString
    type AttrTransferTypeConstraint RadioActionEntryAcceleratorFieldInfo = (~)CString
    type AttrTransferType RadioActionEntryAcceleratorFieldInfo = CString
    type AttrGetType RadioActionEntryAcceleratorFieldInfo = Maybe T.Text
    type AttrLabel RadioActionEntryAcceleratorFieldInfo = "accelerator"
    type AttrOrigin RadioActionEntryAcceleratorFieldInfo = RadioActionEntry
    attrGet = getRadioActionEntryAccelerator
    attrSet = setRadioActionEntryAccelerator
    attrConstruct = undefined
    attrClear = clearRadioActionEntryAccelerator
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RadioActionEntry.accelerator"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RadioActionEntry.html#g:attr:accelerator"
        })

radioActionEntry_accelerator :: AttrLabelProxy "accelerator"
radioActionEntry_accelerator = AttrLabelProxy

#endif


-- | Get the value of the “@tooltip@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' radioActionEntry #tooltip
-- @
getRadioActionEntryTooltip :: MonadIO m => RadioActionEntry -> m (Maybe T.Text)
getRadioActionEntryTooltip s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 32) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@tooltip@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' radioActionEntry [ #tooltip 'Data.GI.Base.Attributes.:=' value ]
-- @
setRadioActionEntryTooltip :: MonadIO m => RadioActionEntry -> CString -> m ()
setRadioActionEntryTooltip s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 32) (val :: CString)

-- | Set the value of the “@tooltip@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #tooltip
-- @
clearRadioActionEntryTooltip :: MonadIO m => RadioActionEntry -> m ()
clearRadioActionEntryTooltip s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 32) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data RadioActionEntryTooltipFieldInfo
instance AttrInfo RadioActionEntryTooltipFieldInfo where
    type AttrBaseTypeConstraint RadioActionEntryTooltipFieldInfo = (~) RadioActionEntry
    type AttrAllowedOps RadioActionEntryTooltipFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint RadioActionEntryTooltipFieldInfo = (~) CString
    type AttrTransferTypeConstraint RadioActionEntryTooltipFieldInfo = (~)CString
    type AttrTransferType RadioActionEntryTooltipFieldInfo = CString
    type AttrGetType RadioActionEntryTooltipFieldInfo = Maybe T.Text
    type AttrLabel RadioActionEntryTooltipFieldInfo = "tooltip"
    type AttrOrigin RadioActionEntryTooltipFieldInfo = RadioActionEntry
    attrGet = getRadioActionEntryTooltip
    attrSet = setRadioActionEntryTooltip
    attrConstruct = undefined
    attrClear = clearRadioActionEntryTooltip
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RadioActionEntry.tooltip"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RadioActionEntry.html#g:attr:tooltip"
        })

radioActionEntry_tooltip :: AttrLabelProxy "tooltip"
radioActionEntry_tooltip = AttrLabelProxy

#endif


-- | Get the value of the “@value@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' radioActionEntry #value
-- @
getRadioActionEntryValue :: MonadIO m => RadioActionEntry -> m Int32
getRadioActionEntryValue s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 40) :: IO Int32
    return val

-- | Set the value of the “@value@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' radioActionEntry [ #value 'Data.GI.Base.Attributes.:=' value ]
-- @
setRadioActionEntryValue :: MonadIO m => RadioActionEntry -> Int32 -> m ()
setRadioActionEntryValue s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 40) (val :: Int32)

#if defined(ENABLE_OVERLOADING)
data RadioActionEntryValueFieldInfo
instance AttrInfo RadioActionEntryValueFieldInfo where
    type AttrBaseTypeConstraint RadioActionEntryValueFieldInfo = (~) RadioActionEntry
    type AttrAllowedOps RadioActionEntryValueFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint RadioActionEntryValueFieldInfo = (~) Int32
    type AttrTransferTypeConstraint RadioActionEntryValueFieldInfo = (~)Int32
    type AttrTransferType RadioActionEntryValueFieldInfo = Int32
    type AttrGetType RadioActionEntryValueFieldInfo = Int32
    type AttrLabel RadioActionEntryValueFieldInfo = "value"
    type AttrOrigin RadioActionEntryValueFieldInfo = RadioActionEntry
    attrGet = getRadioActionEntryValue
    attrSet = setRadioActionEntryValue
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RadioActionEntry.value"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RadioActionEntry.html#g:attr:value"
        })

radioActionEntry_value :: AttrLabelProxy "value"
radioActionEntry_value = AttrLabelProxy

#endif



#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList RadioActionEntry
type instance O.AttributeList RadioActionEntry = RadioActionEntryAttributeList
type RadioActionEntryAttributeList = ('[ '("name", RadioActionEntryNameFieldInfo), '("stockId", RadioActionEntryStockIdFieldInfo), '("label", RadioActionEntryLabelFieldInfo), '("accelerator", RadioActionEntryAcceleratorFieldInfo), '("tooltip", RadioActionEntryTooltipFieldInfo), '("value", RadioActionEntryValueFieldInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveRadioActionEntryMethod (t :: Symbol) (o :: *) :: * where
    ResolveRadioActionEntryMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveRadioActionEntryMethod t RadioActionEntry, O.OverloadedMethod info RadioActionEntry p) => OL.IsLabel t (RadioActionEntry -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveRadioActionEntryMethod t RadioActionEntry, O.OverloadedMethod info RadioActionEntry p, R.HasField t RadioActionEntry p) => R.HasField t RadioActionEntry p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveRadioActionEntryMethod t RadioActionEntry, O.OverloadedMethodInfo info RadioActionEntry) => OL.IsLabel t (O.MethodProxy info RadioActionEntry) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


