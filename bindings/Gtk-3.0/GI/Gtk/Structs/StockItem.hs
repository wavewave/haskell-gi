{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- /No description available in the introspection data./

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.StockItem
    ( 

-- * Exported types
    StockItem(..)                           ,
    newZeroStockItem                        ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [free]("GI.Gtk.Structs.StockItem#g:method:free").
-- 
-- ==== Getters
-- /None/.
-- 
-- ==== Setters
-- /None/.

#if defined(ENABLE_OVERLOADING)
    ResolveStockItemMethod                  ,
#endif

-- ** free #method:free#

#if defined(ENABLE_OVERLOADING)
    StockItemFreeMethodInfo                 ,
#endif
    stockItemFree                           ,




 -- * Properties


-- ** keyval #attr:keyval#
-- | Keyboard accelerator

    getStockItemKeyval                      ,
    setStockItemKeyval                      ,
#if defined(ENABLE_OVERLOADING)
    stockItem_keyval                        ,
#endif


-- ** label #attr:label#
-- | User visible label.

    clearStockItemLabel                     ,
    getStockItemLabel                       ,
    setStockItemLabel                       ,
#if defined(ENABLE_OVERLOADING)
    stockItem_label                         ,
#endif


-- ** modifier #attr:modifier#
-- | Modifier type for keyboard accelerator

    getStockItemModifier                    ,
    setStockItemModifier                    ,
#if defined(ENABLE_OVERLOADING)
    stockItem_modifier                      ,
#endif


-- ** stockId #attr:stockId#
-- | Identifier.

    clearStockItemStockId                   ,
    getStockItemStockId                     ,
    setStockItemStockId                     ,
#if defined(ENABLE_OVERLOADING)
    stockItem_stockId                       ,
#endif


-- ** translationDomain #attr:translationDomain#
-- | Translation domain of the menu or toolbar item

    clearStockItemTranslationDomain         ,
    getStockItemTranslationDomain           ,
    setStockItemTranslationDomain           ,
#if defined(ENABLE_OVERLOADING)
    stockItem_translationDomain             ,
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

import qualified GI.Gdk.Flags as Gdk.Flags

-- | Memory-managed wrapper type.
newtype StockItem = StockItem (SP.ManagedPtr StockItem)
    deriving (Eq)

instance SP.ManagedPtrNewtype StockItem where
    toManagedPtr (StockItem p) = p

instance BoxedPtr StockItem where
    boxedPtrCopy = \p -> B.ManagedPtr.withManagedPtr p (copyBytes 32 >=> B.ManagedPtr.wrapPtr StockItem)
    boxedPtrFree = \x -> SP.withManagedPtr x SP.freeMem
instance CallocPtr StockItem where
    boxedPtrCalloc = callocBytes 32


-- | Construct a `StockItem` struct initialized to zero.
newZeroStockItem :: MonadIO m => m StockItem
newZeroStockItem = liftIO $ boxedPtrCalloc >>= wrapPtr StockItem

instance tag ~ 'AttrSet => Constructible StockItem tag where
    new _ attrs = do
        o <- newZeroStockItem
        GI.Attributes.set o attrs
        return o


-- | Get the value of the “@stock_id@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' stockItem #stockId
-- @
getStockItemStockId :: MonadIO m => StockItem -> m (Maybe T.Text)
getStockItemStockId s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 0) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@stock_id@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' stockItem [ #stockId 'Data.GI.Base.Attributes.:=' value ]
-- @
setStockItemStockId :: MonadIO m => StockItem -> CString -> m ()
setStockItemStockId s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 0) (val :: CString)

-- | Set the value of the “@stock_id@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #stockId
-- @
clearStockItemStockId :: MonadIO m => StockItem -> m ()
clearStockItemStockId s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 0) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data StockItemStockIdFieldInfo
instance AttrInfo StockItemStockIdFieldInfo where
    type AttrBaseTypeConstraint StockItemStockIdFieldInfo = (~) StockItem
    type AttrAllowedOps StockItemStockIdFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint StockItemStockIdFieldInfo = (~) CString
    type AttrTransferTypeConstraint StockItemStockIdFieldInfo = (~)CString
    type AttrTransferType StockItemStockIdFieldInfo = CString
    type AttrGetType StockItemStockIdFieldInfo = Maybe T.Text
    type AttrLabel StockItemStockIdFieldInfo = "stock_id"
    type AttrOrigin StockItemStockIdFieldInfo = StockItem
    attrGet = getStockItemStockId
    attrSet = setStockItemStockId
    attrConstruct = undefined
    attrClear = clearStockItemStockId
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.StockItem.stockId"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-StockItem.html#g:attr:stockId"
        })

stockItem_stockId :: AttrLabelProxy "stockId"
stockItem_stockId = AttrLabelProxy

#endif


-- | Get the value of the “@label@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' stockItem #label
-- @
getStockItemLabel :: MonadIO m => StockItem -> m (Maybe T.Text)
getStockItemLabel s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 8) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@label@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' stockItem [ #label 'Data.GI.Base.Attributes.:=' value ]
-- @
setStockItemLabel :: MonadIO m => StockItem -> CString -> m ()
setStockItemLabel s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (val :: CString)

-- | Set the value of the “@label@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #label
-- @
clearStockItemLabel :: MonadIO m => StockItem -> m ()
clearStockItemLabel s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data StockItemLabelFieldInfo
instance AttrInfo StockItemLabelFieldInfo where
    type AttrBaseTypeConstraint StockItemLabelFieldInfo = (~) StockItem
    type AttrAllowedOps StockItemLabelFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint StockItemLabelFieldInfo = (~) CString
    type AttrTransferTypeConstraint StockItemLabelFieldInfo = (~)CString
    type AttrTransferType StockItemLabelFieldInfo = CString
    type AttrGetType StockItemLabelFieldInfo = Maybe T.Text
    type AttrLabel StockItemLabelFieldInfo = "label"
    type AttrOrigin StockItemLabelFieldInfo = StockItem
    attrGet = getStockItemLabel
    attrSet = setStockItemLabel
    attrConstruct = undefined
    attrClear = clearStockItemLabel
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.StockItem.label"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-StockItem.html#g:attr:label"
        })

stockItem_label :: AttrLabelProxy "label"
stockItem_label = AttrLabelProxy

#endif


-- | Get the value of the “@modifier@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' stockItem #modifier
-- @
getStockItemModifier :: MonadIO m => StockItem -> m [Gdk.Flags.ModifierType]
getStockItemModifier s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 16) :: IO CUInt
    let val' = wordToGFlags val
    return val'

-- | Set the value of the “@modifier@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' stockItem [ #modifier 'Data.GI.Base.Attributes.:=' value ]
-- @
setStockItemModifier :: MonadIO m => StockItem -> [Gdk.Flags.ModifierType] -> m ()
setStockItemModifier s val = liftIO $ withManagedPtr s $ \ptr -> do
    let val' = gflagsToWord val
    poke (ptr `plusPtr` 16) (val' :: CUInt)

#if defined(ENABLE_OVERLOADING)
data StockItemModifierFieldInfo
instance AttrInfo StockItemModifierFieldInfo where
    type AttrBaseTypeConstraint StockItemModifierFieldInfo = (~) StockItem
    type AttrAllowedOps StockItemModifierFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint StockItemModifierFieldInfo = (~) [Gdk.Flags.ModifierType]
    type AttrTransferTypeConstraint StockItemModifierFieldInfo = (~)[Gdk.Flags.ModifierType]
    type AttrTransferType StockItemModifierFieldInfo = [Gdk.Flags.ModifierType]
    type AttrGetType StockItemModifierFieldInfo = [Gdk.Flags.ModifierType]
    type AttrLabel StockItemModifierFieldInfo = "modifier"
    type AttrOrigin StockItemModifierFieldInfo = StockItem
    attrGet = getStockItemModifier
    attrSet = setStockItemModifier
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.StockItem.modifier"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-StockItem.html#g:attr:modifier"
        })

stockItem_modifier :: AttrLabelProxy "modifier"
stockItem_modifier = AttrLabelProxy

#endif


-- | Get the value of the “@keyval@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' stockItem #keyval
-- @
getStockItemKeyval :: MonadIO m => StockItem -> m Word32
getStockItemKeyval s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 20) :: IO Word32
    return val

-- | Set the value of the “@keyval@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' stockItem [ #keyval 'Data.GI.Base.Attributes.:=' value ]
-- @
setStockItemKeyval :: MonadIO m => StockItem -> Word32 -> m ()
setStockItemKeyval s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 20) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data StockItemKeyvalFieldInfo
instance AttrInfo StockItemKeyvalFieldInfo where
    type AttrBaseTypeConstraint StockItemKeyvalFieldInfo = (~) StockItem
    type AttrAllowedOps StockItemKeyvalFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint StockItemKeyvalFieldInfo = (~) Word32
    type AttrTransferTypeConstraint StockItemKeyvalFieldInfo = (~)Word32
    type AttrTransferType StockItemKeyvalFieldInfo = Word32
    type AttrGetType StockItemKeyvalFieldInfo = Word32
    type AttrLabel StockItemKeyvalFieldInfo = "keyval"
    type AttrOrigin StockItemKeyvalFieldInfo = StockItem
    attrGet = getStockItemKeyval
    attrSet = setStockItemKeyval
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.StockItem.keyval"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-StockItem.html#g:attr:keyval"
        })

stockItem_keyval :: AttrLabelProxy "keyval"
stockItem_keyval = AttrLabelProxy

#endif


-- | Get the value of the “@translation_domain@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' stockItem #translationDomain
-- @
getStockItemTranslationDomain :: MonadIO m => StockItem -> m (Maybe T.Text)
getStockItemTranslationDomain s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 24) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@translation_domain@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' stockItem [ #translationDomain 'Data.GI.Base.Attributes.:=' value ]
-- @
setStockItemTranslationDomain :: MonadIO m => StockItem -> CString -> m ()
setStockItemTranslationDomain s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 24) (val :: CString)

-- | Set the value of the “@translation_domain@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #translationDomain
-- @
clearStockItemTranslationDomain :: MonadIO m => StockItem -> m ()
clearStockItemTranslationDomain s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 24) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data StockItemTranslationDomainFieldInfo
instance AttrInfo StockItemTranslationDomainFieldInfo where
    type AttrBaseTypeConstraint StockItemTranslationDomainFieldInfo = (~) StockItem
    type AttrAllowedOps StockItemTranslationDomainFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint StockItemTranslationDomainFieldInfo = (~) CString
    type AttrTransferTypeConstraint StockItemTranslationDomainFieldInfo = (~)CString
    type AttrTransferType StockItemTranslationDomainFieldInfo = CString
    type AttrGetType StockItemTranslationDomainFieldInfo = Maybe T.Text
    type AttrLabel StockItemTranslationDomainFieldInfo = "translation_domain"
    type AttrOrigin StockItemTranslationDomainFieldInfo = StockItem
    attrGet = getStockItemTranslationDomain
    attrSet = setStockItemTranslationDomain
    attrConstruct = undefined
    attrClear = clearStockItemTranslationDomain
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.StockItem.translationDomain"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-StockItem.html#g:attr:translationDomain"
        })

stockItem_translationDomain :: AttrLabelProxy "translationDomain"
stockItem_translationDomain = AttrLabelProxy

#endif



#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList StockItem
type instance O.AttributeList StockItem = StockItemAttributeList
type StockItemAttributeList = ('[ '("stockId", StockItemStockIdFieldInfo), '("label", StockItemLabelFieldInfo), '("modifier", StockItemModifierFieldInfo), '("keyval", StockItemKeyvalFieldInfo), '("translationDomain", StockItemTranslationDomainFieldInfo)] :: [(Symbol, *)])
#endif

-- method StockItem::free
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "item"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StockItem" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStockItem" , sinceVersion = Nothing }
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

foreign import ccall "gtk_stock_item_free" gtk_stock_item_free :: 
    Ptr StockItem ->                        -- item : TInterface (Name {namespace = "Gtk", name = "StockItem"})
    IO ()

{-# DEPRECATED stockItemFree ["(Since version 3.10)"] #-}
-- | Frees a stock item allocated on the heap, such as one returned by
-- @/gtk_stock_item_copy()/@. Also frees the fields inside the stock item,
-- if they are not 'P.Nothing'.
stockItemFree ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    StockItem
    -- ^ /@item@/: a t'GI.Gtk.Structs.StockItem.StockItem'
    -> m ()
stockItemFree item = liftIO $ do
    item' <- unsafeManagedPtrGetPtr item
    gtk_stock_item_free item'
    touchManagedPtr item
    return ()

#if defined(ENABLE_OVERLOADING)
data StockItemFreeMethodInfo
instance (signature ~ (m ()), MonadIO m) => O.OverloadedMethod StockItemFreeMethodInfo StockItem signature where
    overloadedMethod = stockItemFree

instance O.OverloadedMethodInfo StockItemFreeMethodInfo StockItem where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.StockItem.stockItemFree",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-StockItem.html#v:stockItemFree"
        })


#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveStockItemMethod (t :: Symbol) (o :: *) :: * where
    ResolveStockItemMethod "free" o = StockItemFreeMethodInfo
    ResolveStockItemMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveStockItemMethod t StockItem, O.OverloadedMethod info StockItem p) => OL.IsLabel t (StockItem -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveStockItemMethod t StockItem, O.OverloadedMethod info StockItem p, R.HasField t StockItem p) => R.HasField t StockItem p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveStockItemMethod t StockItem, O.OverloadedMethodInfo info StockItem) => OL.IsLabel t (O.MethodProxy info StockItem) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


