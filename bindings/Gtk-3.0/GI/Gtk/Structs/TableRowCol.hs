{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- /No description available in the introspection data./

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.TableRowCol
    ( 

-- * Exported types
    TableRowCol(..)                         ,
    newZeroTableRowCol                      ,


 -- * Methods

#if defined(ENABLE_OVERLOADING)
    ResolveTableRowColMethod                ,
#endif



 -- * Properties


-- ** allocation #attr:allocation#
-- | /No description available in the introspection data./

    getTableRowColAllocation                ,
    setTableRowColAllocation                ,
#if defined(ENABLE_OVERLOADING)
    tableRowCol_allocation                  ,
#endif


-- ** empty #attr:empty#
-- | /No description available in the introspection data./

    getTableRowColEmpty                     ,
    setTableRowColEmpty                     ,
#if defined(ENABLE_OVERLOADING)
    tableRowCol_empty                       ,
#endif


-- ** expand #attr:expand#
-- | /No description available in the introspection data./

    getTableRowColExpand                    ,
    setTableRowColExpand                    ,
#if defined(ENABLE_OVERLOADING)
    tableRowCol_expand                      ,
#endif


-- ** needExpand #attr:needExpand#
-- | /No description available in the introspection data./

    getTableRowColNeedExpand                ,
    setTableRowColNeedExpand                ,
#if defined(ENABLE_OVERLOADING)
    tableRowCol_needExpand                  ,
#endif


-- ** needShrink #attr:needShrink#
-- | /No description available in the introspection data./

    getTableRowColNeedShrink                ,
    setTableRowColNeedShrink                ,
#if defined(ENABLE_OVERLOADING)
    tableRowCol_needShrink                  ,
#endif


-- ** requisition #attr:requisition#
-- | /No description available in the introspection data./

    getTableRowColRequisition               ,
    setTableRowColRequisition               ,
#if defined(ENABLE_OVERLOADING)
    tableRowCol_requisition                 ,
#endif


-- ** shrink #attr:shrink#
-- | /No description available in the introspection data./

    getTableRowColShrink                    ,
    setTableRowColShrink                    ,
#if defined(ENABLE_OVERLOADING)
    tableRowCol_shrink                      ,
#endif


-- ** spacing #attr:spacing#
-- | /No description available in the introspection data./

    getTableRowColSpacing                   ,
    setTableRowColSpacing                   ,
#if defined(ENABLE_OVERLOADING)
    tableRowCol_spacing                     ,
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


-- | Memory-managed wrapper type.
newtype TableRowCol = TableRowCol (SP.ManagedPtr TableRowCol)
    deriving (Eq)

instance SP.ManagedPtrNewtype TableRowCol where
    toManagedPtr (TableRowCol p) = p

instance BoxedPtr TableRowCol where
    boxedPtrCopy = \p -> B.ManagedPtr.withManagedPtr p (copyBytes 28 >=> B.ManagedPtr.wrapPtr TableRowCol)
    boxedPtrFree = \x -> SP.withManagedPtr x SP.freeMem
instance CallocPtr TableRowCol where
    boxedPtrCalloc = callocBytes 28


-- | Construct a `TableRowCol` struct initialized to zero.
newZeroTableRowCol :: MonadIO m => m TableRowCol
newZeroTableRowCol = liftIO $ boxedPtrCalloc >>= wrapPtr TableRowCol

instance tag ~ 'AttrSet => Constructible TableRowCol tag where
    new _ attrs = do
        o <- newZeroTableRowCol
        GI.Attributes.set o attrs
        return o


-- | Get the value of the “@requisition@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' tableRowCol #requisition
-- @
getTableRowColRequisition :: MonadIO m => TableRowCol -> m Word16
getTableRowColRequisition s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 0) :: IO Word16
    return val

-- | Set the value of the “@requisition@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' tableRowCol [ #requisition 'Data.GI.Base.Attributes.:=' value ]
-- @
setTableRowColRequisition :: MonadIO m => TableRowCol -> Word16 -> m ()
setTableRowColRequisition s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 0) (val :: Word16)

#if defined(ENABLE_OVERLOADING)
data TableRowColRequisitionFieldInfo
instance AttrInfo TableRowColRequisitionFieldInfo where
    type AttrBaseTypeConstraint TableRowColRequisitionFieldInfo = (~) TableRowCol
    type AttrAllowedOps TableRowColRequisitionFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TableRowColRequisitionFieldInfo = (~) Word16
    type AttrTransferTypeConstraint TableRowColRequisitionFieldInfo = (~)Word16
    type AttrTransferType TableRowColRequisitionFieldInfo = Word16
    type AttrGetType TableRowColRequisitionFieldInfo = Word16
    type AttrLabel TableRowColRequisitionFieldInfo = "requisition"
    type AttrOrigin TableRowColRequisitionFieldInfo = TableRowCol
    attrGet = getTableRowColRequisition
    attrSet = setTableRowColRequisition
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TableRowCol.requisition"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TableRowCol.html#g:attr:requisition"
        })

tableRowCol_requisition :: AttrLabelProxy "requisition"
tableRowCol_requisition = AttrLabelProxy

#endif


-- | Get the value of the “@allocation@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' tableRowCol #allocation
-- @
getTableRowColAllocation :: MonadIO m => TableRowCol -> m Word16
getTableRowColAllocation s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 2) :: IO Word16
    return val

-- | Set the value of the “@allocation@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' tableRowCol [ #allocation 'Data.GI.Base.Attributes.:=' value ]
-- @
setTableRowColAllocation :: MonadIO m => TableRowCol -> Word16 -> m ()
setTableRowColAllocation s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 2) (val :: Word16)

#if defined(ENABLE_OVERLOADING)
data TableRowColAllocationFieldInfo
instance AttrInfo TableRowColAllocationFieldInfo where
    type AttrBaseTypeConstraint TableRowColAllocationFieldInfo = (~) TableRowCol
    type AttrAllowedOps TableRowColAllocationFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TableRowColAllocationFieldInfo = (~) Word16
    type AttrTransferTypeConstraint TableRowColAllocationFieldInfo = (~)Word16
    type AttrTransferType TableRowColAllocationFieldInfo = Word16
    type AttrGetType TableRowColAllocationFieldInfo = Word16
    type AttrLabel TableRowColAllocationFieldInfo = "allocation"
    type AttrOrigin TableRowColAllocationFieldInfo = TableRowCol
    attrGet = getTableRowColAllocation
    attrSet = setTableRowColAllocation
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TableRowCol.allocation"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TableRowCol.html#g:attr:allocation"
        })

tableRowCol_allocation :: AttrLabelProxy "allocation"
tableRowCol_allocation = AttrLabelProxy

#endif


-- | Get the value of the “@spacing@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' tableRowCol #spacing
-- @
getTableRowColSpacing :: MonadIO m => TableRowCol -> m Word16
getTableRowColSpacing s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 4) :: IO Word16
    return val

-- | Set the value of the “@spacing@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' tableRowCol [ #spacing 'Data.GI.Base.Attributes.:=' value ]
-- @
setTableRowColSpacing :: MonadIO m => TableRowCol -> Word16 -> m ()
setTableRowColSpacing s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 4) (val :: Word16)

#if defined(ENABLE_OVERLOADING)
data TableRowColSpacingFieldInfo
instance AttrInfo TableRowColSpacingFieldInfo where
    type AttrBaseTypeConstraint TableRowColSpacingFieldInfo = (~) TableRowCol
    type AttrAllowedOps TableRowColSpacingFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TableRowColSpacingFieldInfo = (~) Word16
    type AttrTransferTypeConstraint TableRowColSpacingFieldInfo = (~)Word16
    type AttrTransferType TableRowColSpacingFieldInfo = Word16
    type AttrGetType TableRowColSpacingFieldInfo = Word16
    type AttrLabel TableRowColSpacingFieldInfo = "spacing"
    type AttrOrigin TableRowColSpacingFieldInfo = TableRowCol
    attrGet = getTableRowColSpacing
    attrSet = setTableRowColSpacing
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TableRowCol.spacing"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TableRowCol.html#g:attr:spacing"
        })

tableRowCol_spacing :: AttrLabelProxy "spacing"
tableRowCol_spacing = AttrLabelProxy

#endif


-- | Get the value of the “@need_expand@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' tableRowCol #needExpand
-- @
getTableRowColNeedExpand :: MonadIO m => TableRowCol -> m Word32
getTableRowColNeedExpand s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 8) :: IO Word32
    return val

-- | Set the value of the “@need_expand@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' tableRowCol [ #needExpand 'Data.GI.Base.Attributes.:=' value ]
-- @
setTableRowColNeedExpand :: MonadIO m => TableRowCol -> Word32 -> m ()
setTableRowColNeedExpand s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data TableRowColNeedExpandFieldInfo
instance AttrInfo TableRowColNeedExpandFieldInfo where
    type AttrBaseTypeConstraint TableRowColNeedExpandFieldInfo = (~) TableRowCol
    type AttrAllowedOps TableRowColNeedExpandFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TableRowColNeedExpandFieldInfo = (~) Word32
    type AttrTransferTypeConstraint TableRowColNeedExpandFieldInfo = (~)Word32
    type AttrTransferType TableRowColNeedExpandFieldInfo = Word32
    type AttrGetType TableRowColNeedExpandFieldInfo = Word32
    type AttrLabel TableRowColNeedExpandFieldInfo = "need_expand"
    type AttrOrigin TableRowColNeedExpandFieldInfo = TableRowCol
    attrGet = getTableRowColNeedExpand
    attrSet = setTableRowColNeedExpand
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TableRowCol.needExpand"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TableRowCol.html#g:attr:needExpand"
        })

tableRowCol_needExpand :: AttrLabelProxy "needExpand"
tableRowCol_needExpand = AttrLabelProxy

#endif


-- | Get the value of the “@need_shrink@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' tableRowCol #needShrink
-- @
getTableRowColNeedShrink :: MonadIO m => TableRowCol -> m Word32
getTableRowColNeedShrink s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 12) :: IO Word32
    return val

-- | Set the value of the “@need_shrink@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' tableRowCol [ #needShrink 'Data.GI.Base.Attributes.:=' value ]
-- @
setTableRowColNeedShrink :: MonadIO m => TableRowCol -> Word32 -> m ()
setTableRowColNeedShrink s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 12) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data TableRowColNeedShrinkFieldInfo
instance AttrInfo TableRowColNeedShrinkFieldInfo where
    type AttrBaseTypeConstraint TableRowColNeedShrinkFieldInfo = (~) TableRowCol
    type AttrAllowedOps TableRowColNeedShrinkFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TableRowColNeedShrinkFieldInfo = (~) Word32
    type AttrTransferTypeConstraint TableRowColNeedShrinkFieldInfo = (~)Word32
    type AttrTransferType TableRowColNeedShrinkFieldInfo = Word32
    type AttrGetType TableRowColNeedShrinkFieldInfo = Word32
    type AttrLabel TableRowColNeedShrinkFieldInfo = "need_shrink"
    type AttrOrigin TableRowColNeedShrinkFieldInfo = TableRowCol
    attrGet = getTableRowColNeedShrink
    attrSet = setTableRowColNeedShrink
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TableRowCol.needShrink"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TableRowCol.html#g:attr:needShrink"
        })

tableRowCol_needShrink :: AttrLabelProxy "needShrink"
tableRowCol_needShrink = AttrLabelProxy

#endif


-- | Get the value of the “@expand@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' tableRowCol #expand
-- @
getTableRowColExpand :: MonadIO m => TableRowCol -> m Word32
getTableRowColExpand s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 16) :: IO Word32
    return val

-- | Set the value of the “@expand@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' tableRowCol [ #expand 'Data.GI.Base.Attributes.:=' value ]
-- @
setTableRowColExpand :: MonadIO m => TableRowCol -> Word32 -> m ()
setTableRowColExpand s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 16) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data TableRowColExpandFieldInfo
instance AttrInfo TableRowColExpandFieldInfo where
    type AttrBaseTypeConstraint TableRowColExpandFieldInfo = (~) TableRowCol
    type AttrAllowedOps TableRowColExpandFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TableRowColExpandFieldInfo = (~) Word32
    type AttrTransferTypeConstraint TableRowColExpandFieldInfo = (~)Word32
    type AttrTransferType TableRowColExpandFieldInfo = Word32
    type AttrGetType TableRowColExpandFieldInfo = Word32
    type AttrLabel TableRowColExpandFieldInfo = "expand"
    type AttrOrigin TableRowColExpandFieldInfo = TableRowCol
    attrGet = getTableRowColExpand
    attrSet = setTableRowColExpand
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TableRowCol.expand"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TableRowCol.html#g:attr:expand"
        })

tableRowCol_expand :: AttrLabelProxy "expand"
tableRowCol_expand = AttrLabelProxy

#endif


-- | Get the value of the “@shrink@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' tableRowCol #shrink
-- @
getTableRowColShrink :: MonadIO m => TableRowCol -> m Word32
getTableRowColShrink s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 20) :: IO Word32
    return val

-- | Set the value of the “@shrink@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' tableRowCol [ #shrink 'Data.GI.Base.Attributes.:=' value ]
-- @
setTableRowColShrink :: MonadIO m => TableRowCol -> Word32 -> m ()
setTableRowColShrink s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 20) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data TableRowColShrinkFieldInfo
instance AttrInfo TableRowColShrinkFieldInfo where
    type AttrBaseTypeConstraint TableRowColShrinkFieldInfo = (~) TableRowCol
    type AttrAllowedOps TableRowColShrinkFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TableRowColShrinkFieldInfo = (~) Word32
    type AttrTransferTypeConstraint TableRowColShrinkFieldInfo = (~)Word32
    type AttrTransferType TableRowColShrinkFieldInfo = Word32
    type AttrGetType TableRowColShrinkFieldInfo = Word32
    type AttrLabel TableRowColShrinkFieldInfo = "shrink"
    type AttrOrigin TableRowColShrinkFieldInfo = TableRowCol
    attrGet = getTableRowColShrink
    attrSet = setTableRowColShrink
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TableRowCol.shrink"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TableRowCol.html#g:attr:shrink"
        })

tableRowCol_shrink :: AttrLabelProxy "shrink"
tableRowCol_shrink = AttrLabelProxy

#endif


-- | Get the value of the “@empty@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' tableRowCol #empty
-- @
getTableRowColEmpty :: MonadIO m => TableRowCol -> m Word32
getTableRowColEmpty s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 24) :: IO Word32
    return val

-- | Set the value of the “@empty@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' tableRowCol [ #empty 'Data.GI.Base.Attributes.:=' value ]
-- @
setTableRowColEmpty :: MonadIO m => TableRowCol -> Word32 -> m ()
setTableRowColEmpty s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 24) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data TableRowColEmptyFieldInfo
instance AttrInfo TableRowColEmptyFieldInfo where
    type AttrBaseTypeConstraint TableRowColEmptyFieldInfo = (~) TableRowCol
    type AttrAllowedOps TableRowColEmptyFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TableRowColEmptyFieldInfo = (~) Word32
    type AttrTransferTypeConstraint TableRowColEmptyFieldInfo = (~)Word32
    type AttrTransferType TableRowColEmptyFieldInfo = Word32
    type AttrGetType TableRowColEmptyFieldInfo = Word32
    type AttrLabel TableRowColEmptyFieldInfo = "empty"
    type AttrOrigin TableRowColEmptyFieldInfo = TableRowCol
    attrGet = getTableRowColEmpty
    attrSet = setTableRowColEmpty
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TableRowCol.empty"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TableRowCol.html#g:attr:empty"
        })

tableRowCol_empty :: AttrLabelProxy "empty"
tableRowCol_empty = AttrLabelProxy

#endif



#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList TableRowCol
type instance O.AttributeList TableRowCol = TableRowColAttributeList
type TableRowColAttributeList = ('[ '("requisition", TableRowColRequisitionFieldInfo), '("allocation", TableRowColAllocationFieldInfo), '("spacing", TableRowColSpacingFieldInfo), '("needExpand", TableRowColNeedExpandFieldInfo), '("needShrink", TableRowColNeedShrinkFieldInfo), '("expand", TableRowColExpandFieldInfo), '("shrink", TableRowColShrinkFieldInfo), '("empty", TableRowColEmptyFieldInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveTableRowColMethod (t :: Symbol) (o :: *) :: * where
    ResolveTableRowColMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveTableRowColMethod t TableRowCol, O.OverloadedMethod info TableRowCol p) => OL.IsLabel t (TableRowCol -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveTableRowColMethod t TableRowCol, O.OverloadedMethod info TableRowCol p, R.HasField t TableRowCol p) => R.HasField t TableRowCol p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveTableRowColMethod t TableRowCol, O.OverloadedMethodInfo info TableRowCol) => OL.IsLabel t (O.MethodProxy info TableRowCol) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


