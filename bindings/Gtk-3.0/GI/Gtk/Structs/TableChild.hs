{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- /No description available in the introspection data./

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.TableChild
    ( 

-- * Exported types
    TableChild(..)                          ,
    newZeroTableChild                       ,


 -- * Methods

#if defined(ENABLE_OVERLOADING)
    ResolveTableChildMethod                 ,
#endif



 -- * Properties


-- ** bottomAttach #attr:bottomAttach#
-- | /No description available in the introspection data./

    getTableChildBottomAttach               ,
    setTableChildBottomAttach               ,
#if defined(ENABLE_OVERLOADING)
    tableChild_bottomAttach                 ,
#endif


-- ** leftAttach #attr:leftAttach#
-- | /No description available in the introspection data./

    getTableChildLeftAttach                 ,
    setTableChildLeftAttach                 ,
#if defined(ENABLE_OVERLOADING)
    tableChild_leftAttach                   ,
#endif


-- ** rightAttach #attr:rightAttach#
-- | /No description available in the introspection data./

    getTableChildRightAttach                ,
    setTableChildRightAttach                ,
#if defined(ENABLE_OVERLOADING)
    tableChild_rightAttach                  ,
#endif


-- ** topAttach #attr:topAttach#
-- | /No description available in the introspection data./

    getTableChildTopAttach                  ,
    setTableChildTopAttach                  ,
#if defined(ENABLE_OVERLOADING)
    tableChild_topAttach                    ,
#endif


-- ** widget #attr:widget#
-- | /No description available in the introspection data./

    clearTableChildWidget                   ,
    getTableChildWidget                     ,
    setTableChildWidget                     ,
#if defined(ENABLE_OVERLOADING)
    tableChild_widget                       ,
#endif


-- ** xexpand #attr:xexpand#
-- | /No description available in the introspection data./

    getTableChildXexpand                    ,
    setTableChildXexpand                    ,
#if defined(ENABLE_OVERLOADING)
    tableChild_xexpand                      ,
#endif


-- ** xfill #attr:xfill#
-- | /No description available in the introspection data./

    getTableChildXfill                      ,
    setTableChildXfill                      ,
#if defined(ENABLE_OVERLOADING)
    tableChild_xfill                        ,
#endif


-- ** xpadding #attr:xpadding#
-- | /No description available in the introspection data./

    getTableChildXpadding                   ,
    setTableChildXpadding                   ,
#if defined(ENABLE_OVERLOADING)
    tableChild_xpadding                     ,
#endif


-- ** xshrink #attr:xshrink#
-- | /No description available in the introspection data./

    getTableChildXshrink                    ,
    setTableChildXshrink                    ,
#if defined(ENABLE_OVERLOADING)
    tableChild_xshrink                      ,
#endif


-- ** yexpand #attr:yexpand#
-- | /No description available in the introspection data./

    getTableChildYexpand                    ,
    setTableChildYexpand                    ,
#if defined(ENABLE_OVERLOADING)
    tableChild_yexpand                      ,
#endif


-- ** yfill #attr:yfill#
-- | /No description available in the introspection data./

    getTableChildYfill                      ,
    setTableChildYfill                      ,
#if defined(ENABLE_OVERLOADING)
    tableChild_yfill                        ,
#endif


-- ** ypadding #attr:ypadding#
-- | /No description available in the introspection data./

    getTableChildYpadding                   ,
    setTableChildYpadding                   ,
#if defined(ENABLE_OVERLOADING)
    tableChild_ypadding                     ,
#endif


-- ** yshrink #attr:yshrink#
-- | /No description available in the introspection data./

    getTableChildYshrink                    ,
    setTableChildYshrink                    ,
#if defined(ENABLE_OVERLOADING)
    tableChild_yshrink                      ,
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

import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype TableChild = TableChild (SP.ManagedPtr TableChild)
    deriving (Eq)

instance SP.ManagedPtrNewtype TableChild where
    toManagedPtr (TableChild p) = p

instance BoxedPtr TableChild where
    boxedPtrCopy = \p -> B.ManagedPtr.withManagedPtr p (copyBytes 48 >=> B.ManagedPtr.wrapPtr TableChild)
    boxedPtrFree = \x -> SP.withManagedPtr x SP.freeMem
instance CallocPtr TableChild where
    boxedPtrCalloc = callocBytes 48


-- | Construct a `TableChild` struct initialized to zero.
newZeroTableChild :: MonadIO m => m TableChild
newZeroTableChild = liftIO $ boxedPtrCalloc >>= wrapPtr TableChild

instance tag ~ 'AttrSet => Constructible TableChild tag where
    new _ attrs = do
        o <- newZeroTableChild
        GI.Attributes.set o attrs
        return o


-- | Get the value of the “@widget@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' tableChild #widget
-- @
getTableChildWidget :: MonadIO m => TableChild -> m (Maybe Gtk.Widget.Widget)
getTableChildWidget s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 0) :: IO (Ptr Gtk.Widget.Widget)
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- (newObject Gtk.Widget.Widget) val'
        return val''
    return result

-- | Set the value of the “@widget@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' tableChild [ #widget 'Data.GI.Base.Attributes.:=' value ]
-- @
setTableChildWidget :: MonadIO m => TableChild -> Ptr Gtk.Widget.Widget -> m ()
setTableChildWidget s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 0) (val :: Ptr Gtk.Widget.Widget)

-- | Set the value of the “@widget@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #widget
-- @
clearTableChildWidget :: MonadIO m => TableChild -> m ()
clearTableChildWidget s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 0) (FP.nullPtr :: Ptr Gtk.Widget.Widget)

#if defined(ENABLE_OVERLOADING)
data TableChildWidgetFieldInfo
instance AttrInfo TableChildWidgetFieldInfo where
    type AttrBaseTypeConstraint TableChildWidgetFieldInfo = (~) TableChild
    type AttrAllowedOps TableChildWidgetFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint TableChildWidgetFieldInfo = (~) (Ptr Gtk.Widget.Widget)
    type AttrTransferTypeConstraint TableChildWidgetFieldInfo = (~)(Ptr Gtk.Widget.Widget)
    type AttrTransferType TableChildWidgetFieldInfo = (Ptr Gtk.Widget.Widget)
    type AttrGetType TableChildWidgetFieldInfo = Maybe Gtk.Widget.Widget
    type AttrLabel TableChildWidgetFieldInfo = "widget"
    type AttrOrigin TableChildWidgetFieldInfo = TableChild
    attrGet = getTableChildWidget
    attrSet = setTableChildWidget
    attrConstruct = undefined
    attrClear = clearTableChildWidget
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TableChild.widget"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TableChild.html#g:attr:widget"
        })

tableChild_widget :: AttrLabelProxy "widget"
tableChild_widget = AttrLabelProxy

#endif


-- | Get the value of the “@left_attach@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' tableChild #leftAttach
-- @
getTableChildLeftAttach :: MonadIO m => TableChild -> m Word16
getTableChildLeftAttach s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 8) :: IO Word16
    return val

-- | Set the value of the “@left_attach@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' tableChild [ #leftAttach 'Data.GI.Base.Attributes.:=' value ]
-- @
setTableChildLeftAttach :: MonadIO m => TableChild -> Word16 -> m ()
setTableChildLeftAttach s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (val :: Word16)

#if defined(ENABLE_OVERLOADING)
data TableChildLeftAttachFieldInfo
instance AttrInfo TableChildLeftAttachFieldInfo where
    type AttrBaseTypeConstraint TableChildLeftAttachFieldInfo = (~) TableChild
    type AttrAllowedOps TableChildLeftAttachFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TableChildLeftAttachFieldInfo = (~) Word16
    type AttrTransferTypeConstraint TableChildLeftAttachFieldInfo = (~)Word16
    type AttrTransferType TableChildLeftAttachFieldInfo = Word16
    type AttrGetType TableChildLeftAttachFieldInfo = Word16
    type AttrLabel TableChildLeftAttachFieldInfo = "left_attach"
    type AttrOrigin TableChildLeftAttachFieldInfo = TableChild
    attrGet = getTableChildLeftAttach
    attrSet = setTableChildLeftAttach
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TableChild.leftAttach"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TableChild.html#g:attr:leftAttach"
        })

tableChild_leftAttach :: AttrLabelProxy "leftAttach"
tableChild_leftAttach = AttrLabelProxy

#endif


-- | Get the value of the “@right_attach@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' tableChild #rightAttach
-- @
getTableChildRightAttach :: MonadIO m => TableChild -> m Word16
getTableChildRightAttach s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 10) :: IO Word16
    return val

-- | Set the value of the “@right_attach@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' tableChild [ #rightAttach 'Data.GI.Base.Attributes.:=' value ]
-- @
setTableChildRightAttach :: MonadIO m => TableChild -> Word16 -> m ()
setTableChildRightAttach s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 10) (val :: Word16)

#if defined(ENABLE_OVERLOADING)
data TableChildRightAttachFieldInfo
instance AttrInfo TableChildRightAttachFieldInfo where
    type AttrBaseTypeConstraint TableChildRightAttachFieldInfo = (~) TableChild
    type AttrAllowedOps TableChildRightAttachFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TableChildRightAttachFieldInfo = (~) Word16
    type AttrTransferTypeConstraint TableChildRightAttachFieldInfo = (~)Word16
    type AttrTransferType TableChildRightAttachFieldInfo = Word16
    type AttrGetType TableChildRightAttachFieldInfo = Word16
    type AttrLabel TableChildRightAttachFieldInfo = "right_attach"
    type AttrOrigin TableChildRightAttachFieldInfo = TableChild
    attrGet = getTableChildRightAttach
    attrSet = setTableChildRightAttach
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TableChild.rightAttach"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TableChild.html#g:attr:rightAttach"
        })

tableChild_rightAttach :: AttrLabelProxy "rightAttach"
tableChild_rightAttach = AttrLabelProxy

#endif


-- | Get the value of the “@top_attach@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' tableChild #topAttach
-- @
getTableChildTopAttach :: MonadIO m => TableChild -> m Word16
getTableChildTopAttach s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 12) :: IO Word16
    return val

-- | Set the value of the “@top_attach@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' tableChild [ #topAttach 'Data.GI.Base.Attributes.:=' value ]
-- @
setTableChildTopAttach :: MonadIO m => TableChild -> Word16 -> m ()
setTableChildTopAttach s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 12) (val :: Word16)

#if defined(ENABLE_OVERLOADING)
data TableChildTopAttachFieldInfo
instance AttrInfo TableChildTopAttachFieldInfo where
    type AttrBaseTypeConstraint TableChildTopAttachFieldInfo = (~) TableChild
    type AttrAllowedOps TableChildTopAttachFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TableChildTopAttachFieldInfo = (~) Word16
    type AttrTransferTypeConstraint TableChildTopAttachFieldInfo = (~)Word16
    type AttrTransferType TableChildTopAttachFieldInfo = Word16
    type AttrGetType TableChildTopAttachFieldInfo = Word16
    type AttrLabel TableChildTopAttachFieldInfo = "top_attach"
    type AttrOrigin TableChildTopAttachFieldInfo = TableChild
    attrGet = getTableChildTopAttach
    attrSet = setTableChildTopAttach
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TableChild.topAttach"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TableChild.html#g:attr:topAttach"
        })

tableChild_topAttach :: AttrLabelProxy "topAttach"
tableChild_topAttach = AttrLabelProxy

#endif


-- | Get the value of the “@bottom_attach@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' tableChild #bottomAttach
-- @
getTableChildBottomAttach :: MonadIO m => TableChild -> m Word16
getTableChildBottomAttach s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 14) :: IO Word16
    return val

-- | Set the value of the “@bottom_attach@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' tableChild [ #bottomAttach 'Data.GI.Base.Attributes.:=' value ]
-- @
setTableChildBottomAttach :: MonadIO m => TableChild -> Word16 -> m ()
setTableChildBottomAttach s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 14) (val :: Word16)

#if defined(ENABLE_OVERLOADING)
data TableChildBottomAttachFieldInfo
instance AttrInfo TableChildBottomAttachFieldInfo where
    type AttrBaseTypeConstraint TableChildBottomAttachFieldInfo = (~) TableChild
    type AttrAllowedOps TableChildBottomAttachFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TableChildBottomAttachFieldInfo = (~) Word16
    type AttrTransferTypeConstraint TableChildBottomAttachFieldInfo = (~)Word16
    type AttrTransferType TableChildBottomAttachFieldInfo = Word16
    type AttrGetType TableChildBottomAttachFieldInfo = Word16
    type AttrLabel TableChildBottomAttachFieldInfo = "bottom_attach"
    type AttrOrigin TableChildBottomAttachFieldInfo = TableChild
    attrGet = getTableChildBottomAttach
    attrSet = setTableChildBottomAttach
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TableChild.bottomAttach"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TableChild.html#g:attr:bottomAttach"
        })

tableChild_bottomAttach :: AttrLabelProxy "bottomAttach"
tableChild_bottomAttach = AttrLabelProxy

#endif


-- | Get the value of the “@xpadding@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' tableChild #xpadding
-- @
getTableChildXpadding :: MonadIO m => TableChild -> m Word16
getTableChildXpadding s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 16) :: IO Word16
    return val

-- | Set the value of the “@xpadding@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' tableChild [ #xpadding 'Data.GI.Base.Attributes.:=' value ]
-- @
setTableChildXpadding :: MonadIO m => TableChild -> Word16 -> m ()
setTableChildXpadding s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 16) (val :: Word16)

#if defined(ENABLE_OVERLOADING)
data TableChildXpaddingFieldInfo
instance AttrInfo TableChildXpaddingFieldInfo where
    type AttrBaseTypeConstraint TableChildXpaddingFieldInfo = (~) TableChild
    type AttrAllowedOps TableChildXpaddingFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TableChildXpaddingFieldInfo = (~) Word16
    type AttrTransferTypeConstraint TableChildXpaddingFieldInfo = (~)Word16
    type AttrTransferType TableChildXpaddingFieldInfo = Word16
    type AttrGetType TableChildXpaddingFieldInfo = Word16
    type AttrLabel TableChildXpaddingFieldInfo = "xpadding"
    type AttrOrigin TableChildXpaddingFieldInfo = TableChild
    attrGet = getTableChildXpadding
    attrSet = setTableChildXpadding
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TableChild.xpadding"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TableChild.html#g:attr:xpadding"
        })

tableChild_xpadding :: AttrLabelProxy "xpadding"
tableChild_xpadding = AttrLabelProxy

#endif


-- | Get the value of the “@ypadding@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' tableChild #ypadding
-- @
getTableChildYpadding :: MonadIO m => TableChild -> m Word16
getTableChildYpadding s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 18) :: IO Word16
    return val

-- | Set the value of the “@ypadding@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' tableChild [ #ypadding 'Data.GI.Base.Attributes.:=' value ]
-- @
setTableChildYpadding :: MonadIO m => TableChild -> Word16 -> m ()
setTableChildYpadding s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 18) (val :: Word16)

#if defined(ENABLE_OVERLOADING)
data TableChildYpaddingFieldInfo
instance AttrInfo TableChildYpaddingFieldInfo where
    type AttrBaseTypeConstraint TableChildYpaddingFieldInfo = (~) TableChild
    type AttrAllowedOps TableChildYpaddingFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TableChildYpaddingFieldInfo = (~) Word16
    type AttrTransferTypeConstraint TableChildYpaddingFieldInfo = (~)Word16
    type AttrTransferType TableChildYpaddingFieldInfo = Word16
    type AttrGetType TableChildYpaddingFieldInfo = Word16
    type AttrLabel TableChildYpaddingFieldInfo = "ypadding"
    type AttrOrigin TableChildYpaddingFieldInfo = TableChild
    attrGet = getTableChildYpadding
    attrSet = setTableChildYpadding
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TableChild.ypadding"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TableChild.html#g:attr:ypadding"
        })

tableChild_ypadding :: AttrLabelProxy "ypadding"
tableChild_ypadding = AttrLabelProxy

#endif


-- | Get the value of the “@xexpand@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' tableChild #xexpand
-- @
getTableChildXexpand :: MonadIO m => TableChild -> m Word32
getTableChildXexpand s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 20) :: IO Word32
    return val

-- | Set the value of the “@xexpand@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' tableChild [ #xexpand 'Data.GI.Base.Attributes.:=' value ]
-- @
setTableChildXexpand :: MonadIO m => TableChild -> Word32 -> m ()
setTableChildXexpand s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 20) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data TableChildXexpandFieldInfo
instance AttrInfo TableChildXexpandFieldInfo where
    type AttrBaseTypeConstraint TableChildXexpandFieldInfo = (~) TableChild
    type AttrAllowedOps TableChildXexpandFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TableChildXexpandFieldInfo = (~) Word32
    type AttrTransferTypeConstraint TableChildXexpandFieldInfo = (~)Word32
    type AttrTransferType TableChildXexpandFieldInfo = Word32
    type AttrGetType TableChildXexpandFieldInfo = Word32
    type AttrLabel TableChildXexpandFieldInfo = "xexpand"
    type AttrOrigin TableChildXexpandFieldInfo = TableChild
    attrGet = getTableChildXexpand
    attrSet = setTableChildXexpand
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TableChild.xexpand"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TableChild.html#g:attr:xexpand"
        })

tableChild_xexpand :: AttrLabelProxy "xexpand"
tableChild_xexpand = AttrLabelProxy

#endif


-- | Get the value of the “@yexpand@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' tableChild #yexpand
-- @
getTableChildYexpand :: MonadIO m => TableChild -> m Word32
getTableChildYexpand s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 24) :: IO Word32
    return val

-- | Set the value of the “@yexpand@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' tableChild [ #yexpand 'Data.GI.Base.Attributes.:=' value ]
-- @
setTableChildYexpand :: MonadIO m => TableChild -> Word32 -> m ()
setTableChildYexpand s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 24) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data TableChildYexpandFieldInfo
instance AttrInfo TableChildYexpandFieldInfo where
    type AttrBaseTypeConstraint TableChildYexpandFieldInfo = (~) TableChild
    type AttrAllowedOps TableChildYexpandFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TableChildYexpandFieldInfo = (~) Word32
    type AttrTransferTypeConstraint TableChildYexpandFieldInfo = (~)Word32
    type AttrTransferType TableChildYexpandFieldInfo = Word32
    type AttrGetType TableChildYexpandFieldInfo = Word32
    type AttrLabel TableChildYexpandFieldInfo = "yexpand"
    type AttrOrigin TableChildYexpandFieldInfo = TableChild
    attrGet = getTableChildYexpand
    attrSet = setTableChildYexpand
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TableChild.yexpand"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TableChild.html#g:attr:yexpand"
        })

tableChild_yexpand :: AttrLabelProxy "yexpand"
tableChild_yexpand = AttrLabelProxy

#endif


-- | Get the value of the “@xshrink@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' tableChild #xshrink
-- @
getTableChildXshrink :: MonadIO m => TableChild -> m Word32
getTableChildXshrink s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 28) :: IO Word32
    return val

-- | Set the value of the “@xshrink@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' tableChild [ #xshrink 'Data.GI.Base.Attributes.:=' value ]
-- @
setTableChildXshrink :: MonadIO m => TableChild -> Word32 -> m ()
setTableChildXshrink s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 28) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data TableChildXshrinkFieldInfo
instance AttrInfo TableChildXshrinkFieldInfo where
    type AttrBaseTypeConstraint TableChildXshrinkFieldInfo = (~) TableChild
    type AttrAllowedOps TableChildXshrinkFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TableChildXshrinkFieldInfo = (~) Word32
    type AttrTransferTypeConstraint TableChildXshrinkFieldInfo = (~)Word32
    type AttrTransferType TableChildXshrinkFieldInfo = Word32
    type AttrGetType TableChildXshrinkFieldInfo = Word32
    type AttrLabel TableChildXshrinkFieldInfo = "xshrink"
    type AttrOrigin TableChildXshrinkFieldInfo = TableChild
    attrGet = getTableChildXshrink
    attrSet = setTableChildXshrink
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TableChild.xshrink"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TableChild.html#g:attr:xshrink"
        })

tableChild_xshrink :: AttrLabelProxy "xshrink"
tableChild_xshrink = AttrLabelProxy

#endif


-- | Get the value of the “@yshrink@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' tableChild #yshrink
-- @
getTableChildYshrink :: MonadIO m => TableChild -> m Word32
getTableChildYshrink s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 32) :: IO Word32
    return val

-- | Set the value of the “@yshrink@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' tableChild [ #yshrink 'Data.GI.Base.Attributes.:=' value ]
-- @
setTableChildYshrink :: MonadIO m => TableChild -> Word32 -> m ()
setTableChildYshrink s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 32) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data TableChildYshrinkFieldInfo
instance AttrInfo TableChildYshrinkFieldInfo where
    type AttrBaseTypeConstraint TableChildYshrinkFieldInfo = (~) TableChild
    type AttrAllowedOps TableChildYshrinkFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TableChildYshrinkFieldInfo = (~) Word32
    type AttrTransferTypeConstraint TableChildYshrinkFieldInfo = (~)Word32
    type AttrTransferType TableChildYshrinkFieldInfo = Word32
    type AttrGetType TableChildYshrinkFieldInfo = Word32
    type AttrLabel TableChildYshrinkFieldInfo = "yshrink"
    type AttrOrigin TableChildYshrinkFieldInfo = TableChild
    attrGet = getTableChildYshrink
    attrSet = setTableChildYshrink
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TableChild.yshrink"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TableChild.html#g:attr:yshrink"
        })

tableChild_yshrink :: AttrLabelProxy "yshrink"
tableChild_yshrink = AttrLabelProxy

#endif


-- | Get the value of the “@xfill@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' tableChild #xfill
-- @
getTableChildXfill :: MonadIO m => TableChild -> m Word32
getTableChildXfill s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 36) :: IO Word32
    return val

-- | Set the value of the “@xfill@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' tableChild [ #xfill 'Data.GI.Base.Attributes.:=' value ]
-- @
setTableChildXfill :: MonadIO m => TableChild -> Word32 -> m ()
setTableChildXfill s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 36) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data TableChildXfillFieldInfo
instance AttrInfo TableChildXfillFieldInfo where
    type AttrBaseTypeConstraint TableChildXfillFieldInfo = (~) TableChild
    type AttrAllowedOps TableChildXfillFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TableChildXfillFieldInfo = (~) Word32
    type AttrTransferTypeConstraint TableChildXfillFieldInfo = (~)Word32
    type AttrTransferType TableChildXfillFieldInfo = Word32
    type AttrGetType TableChildXfillFieldInfo = Word32
    type AttrLabel TableChildXfillFieldInfo = "xfill"
    type AttrOrigin TableChildXfillFieldInfo = TableChild
    attrGet = getTableChildXfill
    attrSet = setTableChildXfill
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TableChild.xfill"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TableChild.html#g:attr:xfill"
        })

tableChild_xfill :: AttrLabelProxy "xfill"
tableChild_xfill = AttrLabelProxy

#endif


-- | Get the value of the “@yfill@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' tableChild #yfill
-- @
getTableChildYfill :: MonadIO m => TableChild -> m Word32
getTableChildYfill s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 40) :: IO Word32
    return val

-- | Set the value of the “@yfill@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' tableChild [ #yfill 'Data.GI.Base.Attributes.:=' value ]
-- @
setTableChildYfill :: MonadIO m => TableChild -> Word32 -> m ()
setTableChildYfill s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 40) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data TableChildYfillFieldInfo
instance AttrInfo TableChildYfillFieldInfo where
    type AttrBaseTypeConstraint TableChildYfillFieldInfo = (~) TableChild
    type AttrAllowedOps TableChildYfillFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TableChildYfillFieldInfo = (~) Word32
    type AttrTransferTypeConstraint TableChildYfillFieldInfo = (~)Word32
    type AttrTransferType TableChildYfillFieldInfo = Word32
    type AttrGetType TableChildYfillFieldInfo = Word32
    type AttrLabel TableChildYfillFieldInfo = "yfill"
    type AttrOrigin TableChildYfillFieldInfo = TableChild
    attrGet = getTableChildYfill
    attrSet = setTableChildYfill
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TableChild.yfill"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TableChild.html#g:attr:yfill"
        })

tableChild_yfill :: AttrLabelProxy "yfill"
tableChild_yfill = AttrLabelProxy

#endif



#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList TableChild
type instance O.AttributeList TableChild = TableChildAttributeList
type TableChildAttributeList = ('[ '("widget", TableChildWidgetFieldInfo), '("leftAttach", TableChildLeftAttachFieldInfo), '("rightAttach", TableChildRightAttachFieldInfo), '("topAttach", TableChildTopAttachFieldInfo), '("bottomAttach", TableChildBottomAttachFieldInfo), '("xpadding", TableChildXpaddingFieldInfo), '("ypadding", TableChildYpaddingFieldInfo), '("xexpand", TableChildXexpandFieldInfo), '("yexpand", TableChildYexpandFieldInfo), '("xshrink", TableChildXshrinkFieldInfo), '("yshrink", TableChildYshrinkFieldInfo), '("xfill", TableChildXfillFieldInfo), '("yfill", TableChildYfillFieldInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveTableChildMethod (t :: Symbol) (o :: *) :: * where
    ResolveTableChildMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveTableChildMethod t TableChild, O.OverloadedMethod info TableChild p) => OL.IsLabel t (TableChild -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveTableChildMethod t TableChild, O.OverloadedMethod info TableChild p, R.HasField t TableChild p) => R.HasField t TableChild p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveTableChildMethod t TableChild, O.OverloadedMethodInfo info TableChild) => OL.IsLabel t (O.MethodProxy info TableChild) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


