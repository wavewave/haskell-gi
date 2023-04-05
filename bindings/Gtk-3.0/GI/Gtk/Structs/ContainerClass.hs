{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- Base class for containers.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.ContainerClass
    ( 

-- * Exported types
    ContainerClass(..)                      ,
    newZeroContainerClass                   ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [findChildProperty]("GI.Gtk.Structs.ContainerClass#g:method:findChildProperty"), [handleBorderWidth]("GI.Gtk.Structs.ContainerClass#g:method:handleBorderWidth"), [installChildProperty]("GI.Gtk.Structs.ContainerClass#g:method:installChildProperty").
-- 
-- ==== Getters
-- /None/.
-- 
-- ==== Setters
-- /None/.

#if defined(ENABLE_OVERLOADING)
    ResolveContainerClassMethod             ,
#endif

-- ** findChildProperty #method:findChildProperty#

#if defined(ENABLE_OVERLOADING)
    ContainerClassFindChildPropertyMethodInfo,
#endif
    containerClassFindChildProperty         ,


-- ** handleBorderWidth #method:handleBorderWidth#

#if defined(ENABLE_OVERLOADING)
    ContainerClassHandleBorderWidthMethodInfo,
#endif
    containerClassHandleBorderWidth         ,


-- ** installChildProperty #method:installChildProperty#

#if defined(ENABLE_OVERLOADING)
    ContainerClassInstallChildPropertyMethodInfo,
#endif
    containerClassInstallChildProperty      ,




 -- * Properties


-- ** add #attr:add#
-- | /No description available in the introspection data./

    clearContainerClassAdd                  ,
#if defined(ENABLE_OVERLOADING)
    containerClass_add                      ,
#endif
    getContainerClassAdd                    ,
    setContainerClassAdd                    ,


-- ** checkResize #attr:checkResize#
-- | /No description available in the introspection data./

    clearContainerClassCheckResize          ,
#if defined(ENABLE_OVERLOADING)
    containerClass_checkResize              ,
#endif
    getContainerClassCheckResize            ,
    setContainerClassCheckResize            ,


-- ** childType #attr:childType#
-- | /No description available in the introspection data./

    clearContainerClassChildType            ,
#if defined(ENABLE_OVERLOADING)
    containerClass_childType                ,
#endif
    getContainerClassChildType              ,
    setContainerClassChildType              ,


-- ** compositeName #attr:compositeName#
-- | /No description available in the introspection data./

    clearContainerClassCompositeName        ,
#if defined(ENABLE_OVERLOADING)
    containerClass_compositeName            ,
#endif
    getContainerClassCompositeName          ,
    setContainerClassCompositeName          ,


-- ** forall #attr:forall#
-- | /No description available in the introspection data./

    clearContainerClassForall               ,
#if defined(ENABLE_OVERLOADING)
    containerClass_forall                   ,
#endif
    getContainerClassForall                 ,
    setContainerClassForall                 ,


-- ** getChildProperty #attr:getChildProperty#
-- | /No description available in the introspection data./

    clearContainerClassGetChildProperty     ,
#if defined(ENABLE_OVERLOADING)
    containerClass_getChildProperty         ,
#endif
    getContainerClassGetChildProperty       ,
    setContainerClassGetChildProperty       ,


-- ** getPathForChild #attr:getPathForChild#
-- | /No description available in the introspection data./

    clearContainerClassGetPathForChild      ,
#if defined(ENABLE_OVERLOADING)
    containerClass_getPathForChild          ,
#endif
    getContainerClassGetPathForChild        ,
    setContainerClassGetPathForChild        ,


-- ** parentClass #attr:parentClass#
-- | The parent class.

#if defined(ENABLE_OVERLOADING)
    containerClass_parentClass              ,
#endif
    getContainerClassParentClass            ,


-- ** remove #attr:remove#
-- | /No description available in the introspection data./

    clearContainerClassRemove               ,
#if defined(ENABLE_OVERLOADING)
    containerClass_remove                   ,
#endif
    getContainerClassRemove                 ,
    setContainerClassRemove                 ,


-- ** setChildProperty #attr:setChildProperty#
-- | /No description available in the introspection data./

    clearContainerClassSetChildProperty     ,
#if defined(ENABLE_OVERLOADING)
    containerClass_setChildProperty         ,
#endif
    getContainerClassSetChildProperty       ,
    setContainerClassSetChildProperty       ,


-- ** setFocusChild #attr:setFocusChild#
-- | /No description available in the introspection data./

    clearContainerClassSetFocusChild        ,
#if defined(ENABLE_OVERLOADING)
    containerClass_setFocusChild            ,
#endif
    getContainerClassSetFocusChild          ,
    setContainerClassSetFocusChild          ,




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

import qualified GI.Gtk.Callbacks as Gtk.Callbacks
import {-# SOURCE #-} qualified GI.Gtk.Structs.WidgetClass as Gtk.WidgetClass

-- | Memory-managed wrapper type.
newtype ContainerClass = ContainerClass (SP.ManagedPtr ContainerClass)
    deriving (Eq)

instance SP.ManagedPtrNewtype ContainerClass where
    toManagedPtr (ContainerClass p) = p

instance BoxedPtr ContainerClass where
    boxedPtrCopy = \p -> B.ManagedPtr.withManagedPtr p (copyBytes 976 >=> B.ManagedPtr.wrapPtr ContainerClass)
    boxedPtrFree = \x -> SP.withManagedPtr x SP.freeMem
instance CallocPtr ContainerClass where
    boxedPtrCalloc = callocBytes 976


-- | Construct a `ContainerClass` struct initialized to zero.
newZeroContainerClass :: MonadIO m => m ContainerClass
newZeroContainerClass = liftIO $ boxedPtrCalloc >>= wrapPtr ContainerClass

instance tag ~ 'AttrSet => Constructible ContainerClass tag where
    new _ attrs = do
        o <- newZeroContainerClass
        GI.Attributes.set o attrs
        return o


-- | Get the value of the “@parent_class@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' containerClass #parentClass
-- @
getContainerClassParentClass :: MonadIO m => ContainerClass -> m Gtk.WidgetClass.WidgetClass
getContainerClassParentClass s = liftIO $ withManagedPtr s $ \ptr -> do
    let val = ptr `plusPtr` 0 :: (Ptr Gtk.WidgetClass.WidgetClass)
    val' <- (newPtr Gtk.WidgetClass.WidgetClass) val
    return val'

#if defined(ENABLE_OVERLOADING)
data ContainerClassParentClassFieldInfo
instance AttrInfo ContainerClassParentClassFieldInfo where
    type AttrBaseTypeConstraint ContainerClassParentClassFieldInfo = (~) ContainerClass
    type AttrAllowedOps ContainerClassParentClassFieldInfo = '[ 'AttrGet]
    type AttrSetTypeConstraint ContainerClassParentClassFieldInfo = (~) (Ptr Gtk.WidgetClass.WidgetClass)
    type AttrTransferTypeConstraint ContainerClassParentClassFieldInfo = (~)(Ptr Gtk.WidgetClass.WidgetClass)
    type AttrTransferType ContainerClassParentClassFieldInfo = (Ptr Gtk.WidgetClass.WidgetClass)
    type AttrGetType ContainerClassParentClassFieldInfo = Gtk.WidgetClass.WidgetClass
    type AttrLabel ContainerClassParentClassFieldInfo = "parent_class"
    type AttrOrigin ContainerClassParentClassFieldInfo = ContainerClass
    attrGet = getContainerClassParentClass
    attrSet = undefined
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.ContainerClass.parentClass"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-ContainerClass.html#g:attr:parentClass"
        })

containerClass_parentClass :: AttrLabelProxy "parentClass"
containerClass_parentClass = AttrLabelProxy

#endif


-- | Get the value of the “@add@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' containerClass #add
-- @
getContainerClassAdd :: MonadIO m => ContainerClass -> m (Maybe Gtk.Callbacks.ContainerClassAddFieldCallback)
getContainerClassAdd s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 824) :: IO (FunPtr Gtk.Callbacks.C_ContainerClassAddFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_ContainerClassAddFieldCallback val'
        return val''
    return result

-- | Set the value of the “@add@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' containerClass [ #add 'Data.GI.Base.Attributes.:=' value ]
-- @
setContainerClassAdd :: MonadIO m => ContainerClass -> FunPtr Gtk.Callbacks.C_ContainerClassAddFieldCallback -> m ()
setContainerClassAdd s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 824) (val :: FunPtr Gtk.Callbacks.C_ContainerClassAddFieldCallback)

-- | Set the value of the “@add@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #add
-- @
clearContainerClassAdd :: MonadIO m => ContainerClass -> m ()
clearContainerClassAdd s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 824) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_ContainerClassAddFieldCallback)

#if defined(ENABLE_OVERLOADING)
data ContainerClassAddFieldInfo
instance AttrInfo ContainerClassAddFieldInfo where
    type AttrBaseTypeConstraint ContainerClassAddFieldInfo = (~) ContainerClass
    type AttrAllowedOps ContainerClassAddFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint ContainerClassAddFieldInfo = (~) (FunPtr Gtk.Callbacks.C_ContainerClassAddFieldCallback)
    type AttrTransferTypeConstraint ContainerClassAddFieldInfo = (~)Gtk.Callbacks.ContainerClassAddFieldCallback
    type AttrTransferType ContainerClassAddFieldInfo = (FunPtr Gtk.Callbacks.C_ContainerClassAddFieldCallback)
    type AttrGetType ContainerClassAddFieldInfo = Maybe Gtk.Callbacks.ContainerClassAddFieldCallback
    type AttrLabel ContainerClassAddFieldInfo = "add"
    type AttrOrigin ContainerClassAddFieldInfo = ContainerClass
    attrGet = getContainerClassAdd
    attrSet = setContainerClassAdd
    attrConstruct = undefined
    attrClear = clearContainerClassAdd
    attrTransfer _ v = do
        Gtk.Callbacks.mk_ContainerClassAddFieldCallback (Gtk.Callbacks.wrap_ContainerClassAddFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.ContainerClass.add"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-ContainerClass.html#g:attr:add"
        })

containerClass_add :: AttrLabelProxy "add"
containerClass_add = AttrLabelProxy

#endif


-- | Get the value of the “@remove@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' containerClass #remove
-- @
getContainerClassRemove :: MonadIO m => ContainerClass -> m (Maybe Gtk.Callbacks.ContainerClassRemoveFieldCallback)
getContainerClassRemove s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 832) :: IO (FunPtr Gtk.Callbacks.C_ContainerClassRemoveFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_ContainerClassRemoveFieldCallback val'
        return val''
    return result

-- | Set the value of the “@remove@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' containerClass [ #remove 'Data.GI.Base.Attributes.:=' value ]
-- @
setContainerClassRemove :: MonadIO m => ContainerClass -> FunPtr Gtk.Callbacks.C_ContainerClassRemoveFieldCallback -> m ()
setContainerClassRemove s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 832) (val :: FunPtr Gtk.Callbacks.C_ContainerClassRemoveFieldCallback)

-- | Set the value of the “@remove@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #remove
-- @
clearContainerClassRemove :: MonadIO m => ContainerClass -> m ()
clearContainerClassRemove s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 832) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_ContainerClassRemoveFieldCallback)

#if defined(ENABLE_OVERLOADING)
data ContainerClassRemoveFieldInfo
instance AttrInfo ContainerClassRemoveFieldInfo where
    type AttrBaseTypeConstraint ContainerClassRemoveFieldInfo = (~) ContainerClass
    type AttrAllowedOps ContainerClassRemoveFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint ContainerClassRemoveFieldInfo = (~) (FunPtr Gtk.Callbacks.C_ContainerClassRemoveFieldCallback)
    type AttrTransferTypeConstraint ContainerClassRemoveFieldInfo = (~)Gtk.Callbacks.ContainerClassRemoveFieldCallback
    type AttrTransferType ContainerClassRemoveFieldInfo = (FunPtr Gtk.Callbacks.C_ContainerClassRemoveFieldCallback)
    type AttrGetType ContainerClassRemoveFieldInfo = Maybe Gtk.Callbacks.ContainerClassRemoveFieldCallback
    type AttrLabel ContainerClassRemoveFieldInfo = "remove"
    type AttrOrigin ContainerClassRemoveFieldInfo = ContainerClass
    attrGet = getContainerClassRemove
    attrSet = setContainerClassRemove
    attrConstruct = undefined
    attrClear = clearContainerClassRemove
    attrTransfer _ v = do
        Gtk.Callbacks.mk_ContainerClassRemoveFieldCallback (Gtk.Callbacks.wrap_ContainerClassRemoveFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.ContainerClass.remove"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-ContainerClass.html#g:attr:remove"
        })

containerClass_remove :: AttrLabelProxy "remove"
containerClass_remove = AttrLabelProxy

#endif


-- | Get the value of the “@check_resize@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' containerClass #checkResize
-- @
getContainerClassCheckResize :: MonadIO m => ContainerClass -> m (Maybe Gtk.Callbacks.ContainerClassCheckResizeFieldCallback)
getContainerClassCheckResize s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 840) :: IO (FunPtr Gtk.Callbacks.C_ContainerClassCheckResizeFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_ContainerClassCheckResizeFieldCallback val'
        return val''
    return result

-- | Set the value of the “@check_resize@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' containerClass [ #checkResize 'Data.GI.Base.Attributes.:=' value ]
-- @
setContainerClassCheckResize :: MonadIO m => ContainerClass -> FunPtr Gtk.Callbacks.C_ContainerClassCheckResizeFieldCallback -> m ()
setContainerClassCheckResize s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 840) (val :: FunPtr Gtk.Callbacks.C_ContainerClassCheckResizeFieldCallback)

-- | Set the value of the “@check_resize@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #checkResize
-- @
clearContainerClassCheckResize :: MonadIO m => ContainerClass -> m ()
clearContainerClassCheckResize s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 840) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_ContainerClassCheckResizeFieldCallback)

#if defined(ENABLE_OVERLOADING)
data ContainerClassCheckResizeFieldInfo
instance AttrInfo ContainerClassCheckResizeFieldInfo where
    type AttrBaseTypeConstraint ContainerClassCheckResizeFieldInfo = (~) ContainerClass
    type AttrAllowedOps ContainerClassCheckResizeFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint ContainerClassCheckResizeFieldInfo = (~) (FunPtr Gtk.Callbacks.C_ContainerClassCheckResizeFieldCallback)
    type AttrTransferTypeConstraint ContainerClassCheckResizeFieldInfo = (~)Gtk.Callbacks.ContainerClassCheckResizeFieldCallback
    type AttrTransferType ContainerClassCheckResizeFieldInfo = (FunPtr Gtk.Callbacks.C_ContainerClassCheckResizeFieldCallback)
    type AttrGetType ContainerClassCheckResizeFieldInfo = Maybe Gtk.Callbacks.ContainerClassCheckResizeFieldCallback
    type AttrLabel ContainerClassCheckResizeFieldInfo = "check_resize"
    type AttrOrigin ContainerClassCheckResizeFieldInfo = ContainerClass
    attrGet = getContainerClassCheckResize
    attrSet = setContainerClassCheckResize
    attrConstruct = undefined
    attrClear = clearContainerClassCheckResize
    attrTransfer _ v = do
        Gtk.Callbacks.mk_ContainerClassCheckResizeFieldCallback (Gtk.Callbacks.wrap_ContainerClassCheckResizeFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.ContainerClass.checkResize"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-ContainerClass.html#g:attr:checkResize"
        })

containerClass_checkResize :: AttrLabelProxy "checkResize"
containerClass_checkResize = AttrLabelProxy

#endif


-- | Get the value of the “@forall@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' containerClass #forall
-- @
getContainerClassForall :: MonadIO m => ContainerClass -> m (Maybe Gtk.Callbacks.ContainerClassForallFieldCallback_WithClosures)
getContainerClassForall s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 848) :: IO (FunPtr Gtk.Callbacks.C_ContainerClassForallFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_ContainerClassForallFieldCallback val'
        return val''
    return result

-- | Set the value of the “@forall@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' containerClass [ #forall 'Data.GI.Base.Attributes.:=' value ]
-- @
setContainerClassForall :: MonadIO m => ContainerClass -> FunPtr Gtk.Callbacks.C_ContainerClassForallFieldCallback -> m ()
setContainerClassForall s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 848) (val :: FunPtr Gtk.Callbacks.C_ContainerClassForallFieldCallback)

-- | Set the value of the “@forall@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #forall
-- @
clearContainerClassForall :: MonadIO m => ContainerClass -> m ()
clearContainerClassForall s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 848) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_ContainerClassForallFieldCallback)

#if defined(ENABLE_OVERLOADING)
data ContainerClassForallFieldInfo
instance AttrInfo ContainerClassForallFieldInfo where
    type AttrBaseTypeConstraint ContainerClassForallFieldInfo = (~) ContainerClass
    type AttrAllowedOps ContainerClassForallFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint ContainerClassForallFieldInfo = (~) (FunPtr Gtk.Callbacks.C_ContainerClassForallFieldCallback)
    type AttrTransferTypeConstraint ContainerClassForallFieldInfo = (~)Gtk.Callbacks.ContainerClassForallFieldCallback_WithClosures
    type AttrTransferType ContainerClassForallFieldInfo = (FunPtr Gtk.Callbacks.C_ContainerClassForallFieldCallback)
    type AttrGetType ContainerClassForallFieldInfo = Maybe Gtk.Callbacks.ContainerClassForallFieldCallback_WithClosures
    type AttrLabel ContainerClassForallFieldInfo = "forall"
    type AttrOrigin ContainerClassForallFieldInfo = ContainerClass
    attrGet = getContainerClassForall
    attrSet = setContainerClassForall
    attrConstruct = undefined
    attrClear = clearContainerClassForall
    attrTransfer _ v = do
        Gtk.Callbacks.mk_ContainerClassForallFieldCallback (Gtk.Callbacks.wrap_ContainerClassForallFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.ContainerClass.forall"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-ContainerClass.html#g:attr:forall"
        })

containerClass_forall :: AttrLabelProxy "forall"
containerClass_forall = AttrLabelProxy

#endif


-- | Get the value of the “@set_focus_child@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' containerClass #setFocusChild
-- @
getContainerClassSetFocusChild :: MonadIO m => ContainerClass -> m (Maybe Gtk.Callbacks.ContainerClassSetFocusChildFieldCallback)
getContainerClassSetFocusChild s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 856) :: IO (FunPtr Gtk.Callbacks.C_ContainerClassSetFocusChildFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_ContainerClassSetFocusChildFieldCallback val'
        return val''
    return result

-- | Set the value of the “@set_focus_child@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' containerClass [ #setFocusChild 'Data.GI.Base.Attributes.:=' value ]
-- @
setContainerClassSetFocusChild :: MonadIO m => ContainerClass -> FunPtr Gtk.Callbacks.C_ContainerClassSetFocusChildFieldCallback -> m ()
setContainerClassSetFocusChild s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 856) (val :: FunPtr Gtk.Callbacks.C_ContainerClassSetFocusChildFieldCallback)

-- | Set the value of the “@set_focus_child@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #setFocusChild
-- @
clearContainerClassSetFocusChild :: MonadIO m => ContainerClass -> m ()
clearContainerClassSetFocusChild s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 856) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_ContainerClassSetFocusChildFieldCallback)

#if defined(ENABLE_OVERLOADING)
data ContainerClassSetFocusChildFieldInfo
instance AttrInfo ContainerClassSetFocusChildFieldInfo where
    type AttrBaseTypeConstraint ContainerClassSetFocusChildFieldInfo = (~) ContainerClass
    type AttrAllowedOps ContainerClassSetFocusChildFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint ContainerClassSetFocusChildFieldInfo = (~) (FunPtr Gtk.Callbacks.C_ContainerClassSetFocusChildFieldCallback)
    type AttrTransferTypeConstraint ContainerClassSetFocusChildFieldInfo = (~)Gtk.Callbacks.ContainerClassSetFocusChildFieldCallback
    type AttrTransferType ContainerClassSetFocusChildFieldInfo = (FunPtr Gtk.Callbacks.C_ContainerClassSetFocusChildFieldCallback)
    type AttrGetType ContainerClassSetFocusChildFieldInfo = Maybe Gtk.Callbacks.ContainerClassSetFocusChildFieldCallback
    type AttrLabel ContainerClassSetFocusChildFieldInfo = "set_focus_child"
    type AttrOrigin ContainerClassSetFocusChildFieldInfo = ContainerClass
    attrGet = getContainerClassSetFocusChild
    attrSet = setContainerClassSetFocusChild
    attrConstruct = undefined
    attrClear = clearContainerClassSetFocusChild
    attrTransfer _ v = do
        Gtk.Callbacks.mk_ContainerClassSetFocusChildFieldCallback (Gtk.Callbacks.wrap_ContainerClassSetFocusChildFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.ContainerClass.setFocusChild"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-ContainerClass.html#g:attr:setFocusChild"
        })

containerClass_setFocusChild :: AttrLabelProxy "setFocusChild"
containerClass_setFocusChild = AttrLabelProxy

#endif


-- | Get the value of the “@child_type@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' containerClass #childType
-- @
getContainerClassChildType :: MonadIO m => ContainerClass -> m (Maybe Gtk.Callbacks.ContainerClassChildTypeFieldCallback)
getContainerClassChildType s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 864) :: IO (FunPtr Gtk.Callbacks.C_ContainerClassChildTypeFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_ContainerClassChildTypeFieldCallback val'
        return val''
    return result

-- | Set the value of the “@child_type@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' containerClass [ #childType 'Data.GI.Base.Attributes.:=' value ]
-- @
setContainerClassChildType :: MonadIO m => ContainerClass -> FunPtr Gtk.Callbacks.C_ContainerClassChildTypeFieldCallback -> m ()
setContainerClassChildType s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 864) (val :: FunPtr Gtk.Callbacks.C_ContainerClassChildTypeFieldCallback)

-- | Set the value of the “@child_type@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #childType
-- @
clearContainerClassChildType :: MonadIO m => ContainerClass -> m ()
clearContainerClassChildType s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 864) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_ContainerClassChildTypeFieldCallback)

#if defined(ENABLE_OVERLOADING)
data ContainerClassChildTypeFieldInfo
instance AttrInfo ContainerClassChildTypeFieldInfo where
    type AttrBaseTypeConstraint ContainerClassChildTypeFieldInfo = (~) ContainerClass
    type AttrAllowedOps ContainerClassChildTypeFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint ContainerClassChildTypeFieldInfo = (~) (FunPtr Gtk.Callbacks.C_ContainerClassChildTypeFieldCallback)
    type AttrTransferTypeConstraint ContainerClassChildTypeFieldInfo = (~)Gtk.Callbacks.ContainerClassChildTypeFieldCallback
    type AttrTransferType ContainerClassChildTypeFieldInfo = (FunPtr Gtk.Callbacks.C_ContainerClassChildTypeFieldCallback)
    type AttrGetType ContainerClassChildTypeFieldInfo = Maybe Gtk.Callbacks.ContainerClassChildTypeFieldCallback
    type AttrLabel ContainerClassChildTypeFieldInfo = "child_type"
    type AttrOrigin ContainerClassChildTypeFieldInfo = ContainerClass
    attrGet = getContainerClassChildType
    attrSet = setContainerClassChildType
    attrConstruct = undefined
    attrClear = clearContainerClassChildType
    attrTransfer _ v = do
        Gtk.Callbacks.mk_ContainerClassChildTypeFieldCallback (Gtk.Callbacks.wrap_ContainerClassChildTypeFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.ContainerClass.childType"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-ContainerClass.html#g:attr:childType"
        })

containerClass_childType :: AttrLabelProxy "childType"
containerClass_childType = AttrLabelProxy

#endif


-- | Get the value of the “@composite_name@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' containerClass #compositeName
-- @
getContainerClassCompositeName :: MonadIO m => ContainerClass -> m (Maybe Gtk.Callbacks.ContainerClassCompositeNameFieldCallback)
getContainerClassCompositeName s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 872) :: IO (FunPtr Gtk.Callbacks.C_ContainerClassCompositeNameFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_ContainerClassCompositeNameFieldCallback val'
        return val''
    return result

-- | Set the value of the “@composite_name@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' containerClass [ #compositeName 'Data.GI.Base.Attributes.:=' value ]
-- @
setContainerClassCompositeName :: MonadIO m => ContainerClass -> FunPtr Gtk.Callbacks.C_ContainerClassCompositeNameFieldCallback -> m ()
setContainerClassCompositeName s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 872) (val :: FunPtr Gtk.Callbacks.C_ContainerClassCompositeNameFieldCallback)

-- | Set the value of the “@composite_name@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #compositeName
-- @
clearContainerClassCompositeName :: MonadIO m => ContainerClass -> m ()
clearContainerClassCompositeName s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 872) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_ContainerClassCompositeNameFieldCallback)

#if defined(ENABLE_OVERLOADING)
data ContainerClassCompositeNameFieldInfo
instance AttrInfo ContainerClassCompositeNameFieldInfo where
    type AttrBaseTypeConstraint ContainerClassCompositeNameFieldInfo = (~) ContainerClass
    type AttrAllowedOps ContainerClassCompositeNameFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint ContainerClassCompositeNameFieldInfo = (~) (FunPtr Gtk.Callbacks.C_ContainerClassCompositeNameFieldCallback)
    type AttrTransferTypeConstraint ContainerClassCompositeNameFieldInfo = (~)Gtk.Callbacks.ContainerClassCompositeNameFieldCallback
    type AttrTransferType ContainerClassCompositeNameFieldInfo = (FunPtr Gtk.Callbacks.C_ContainerClassCompositeNameFieldCallback)
    type AttrGetType ContainerClassCompositeNameFieldInfo = Maybe Gtk.Callbacks.ContainerClassCompositeNameFieldCallback
    type AttrLabel ContainerClassCompositeNameFieldInfo = "composite_name"
    type AttrOrigin ContainerClassCompositeNameFieldInfo = ContainerClass
    attrGet = getContainerClassCompositeName
    attrSet = setContainerClassCompositeName
    attrConstruct = undefined
    attrClear = clearContainerClassCompositeName
    attrTransfer _ v = do
        Gtk.Callbacks.mk_ContainerClassCompositeNameFieldCallback (Gtk.Callbacks.wrap_ContainerClassCompositeNameFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.ContainerClass.compositeName"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-ContainerClass.html#g:attr:compositeName"
        })

containerClass_compositeName :: AttrLabelProxy "compositeName"
containerClass_compositeName = AttrLabelProxy

#endif


-- | Get the value of the “@set_child_property@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' containerClass #setChildProperty
-- @
getContainerClassSetChildProperty :: MonadIO m => ContainerClass -> m (Maybe Gtk.Callbacks.ContainerClassSetChildPropertyFieldCallback)
getContainerClassSetChildProperty s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 880) :: IO (FunPtr Gtk.Callbacks.C_ContainerClassSetChildPropertyFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_ContainerClassSetChildPropertyFieldCallback val'
        return val''
    return result

-- | Set the value of the “@set_child_property@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' containerClass [ #setChildProperty 'Data.GI.Base.Attributes.:=' value ]
-- @
setContainerClassSetChildProperty :: MonadIO m => ContainerClass -> FunPtr Gtk.Callbacks.C_ContainerClassSetChildPropertyFieldCallback -> m ()
setContainerClassSetChildProperty s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 880) (val :: FunPtr Gtk.Callbacks.C_ContainerClassSetChildPropertyFieldCallback)

-- | Set the value of the “@set_child_property@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #setChildProperty
-- @
clearContainerClassSetChildProperty :: MonadIO m => ContainerClass -> m ()
clearContainerClassSetChildProperty s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 880) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_ContainerClassSetChildPropertyFieldCallback)

#if defined(ENABLE_OVERLOADING)
data ContainerClassSetChildPropertyFieldInfo
instance AttrInfo ContainerClassSetChildPropertyFieldInfo where
    type AttrBaseTypeConstraint ContainerClassSetChildPropertyFieldInfo = (~) ContainerClass
    type AttrAllowedOps ContainerClassSetChildPropertyFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint ContainerClassSetChildPropertyFieldInfo = (~) (FunPtr Gtk.Callbacks.C_ContainerClassSetChildPropertyFieldCallback)
    type AttrTransferTypeConstraint ContainerClassSetChildPropertyFieldInfo = (~)Gtk.Callbacks.ContainerClassSetChildPropertyFieldCallback
    type AttrTransferType ContainerClassSetChildPropertyFieldInfo = (FunPtr Gtk.Callbacks.C_ContainerClassSetChildPropertyFieldCallback)
    type AttrGetType ContainerClassSetChildPropertyFieldInfo = Maybe Gtk.Callbacks.ContainerClassSetChildPropertyFieldCallback
    type AttrLabel ContainerClassSetChildPropertyFieldInfo = "set_child_property"
    type AttrOrigin ContainerClassSetChildPropertyFieldInfo = ContainerClass
    attrGet = getContainerClassSetChildProperty
    attrSet = setContainerClassSetChildProperty
    attrConstruct = undefined
    attrClear = clearContainerClassSetChildProperty
    attrTransfer _ v = do
        Gtk.Callbacks.mk_ContainerClassSetChildPropertyFieldCallback (Gtk.Callbacks.wrap_ContainerClassSetChildPropertyFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.ContainerClass.setChildProperty"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-ContainerClass.html#g:attr:setChildProperty"
        })

containerClass_setChildProperty :: AttrLabelProxy "setChildProperty"
containerClass_setChildProperty = AttrLabelProxy

#endif


-- | Get the value of the “@get_child_property@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' containerClass #getChildProperty
-- @
getContainerClassGetChildProperty :: MonadIO m => ContainerClass -> m (Maybe Gtk.Callbacks.ContainerClassGetChildPropertyFieldCallback)
getContainerClassGetChildProperty s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 888) :: IO (FunPtr Gtk.Callbacks.C_ContainerClassGetChildPropertyFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_ContainerClassGetChildPropertyFieldCallback val'
        return val''
    return result

-- | Set the value of the “@get_child_property@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' containerClass [ #getChildProperty 'Data.GI.Base.Attributes.:=' value ]
-- @
setContainerClassGetChildProperty :: MonadIO m => ContainerClass -> FunPtr Gtk.Callbacks.C_ContainerClassGetChildPropertyFieldCallback -> m ()
setContainerClassGetChildProperty s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 888) (val :: FunPtr Gtk.Callbacks.C_ContainerClassGetChildPropertyFieldCallback)

-- | Set the value of the “@get_child_property@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #getChildProperty
-- @
clearContainerClassGetChildProperty :: MonadIO m => ContainerClass -> m ()
clearContainerClassGetChildProperty s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 888) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_ContainerClassGetChildPropertyFieldCallback)

#if defined(ENABLE_OVERLOADING)
data ContainerClassGetChildPropertyFieldInfo
instance AttrInfo ContainerClassGetChildPropertyFieldInfo where
    type AttrBaseTypeConstraint ContainerClassGetChildPropertyFieldInfo = (~) ContainerClass
    type AttrAllowedOps ContainerClassGetChildPropertyFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint ContainerClassGetChildPropertyFieldInfo = (~) (FunPtr Gtk.Callbacks.C_ContainerClassGetChildPropertyFieldCallback)
    type AttrTransferTypeConstraint ContainerClassGetChildPropertyFieldInfo = (~)Gtk.Callbacks.ContainerClassGetChildPropertyFieldCallback
    type AttrTransferType ContainerClassGetChildPropertyFieldInfo = (FunPtr Gtk.Callbacks.C_ContainerClassGetChildPropertyFieldCallback)
    type AttrGetType ContainerClassGetChildPropertyFieldInfo = Maybe Gtk.Callbacks.ContainerClassGetChildPropertyFieldCallback
    type AttrLabel ContainerClassGetChildPropertyFieldInfo = "get_child_property"
    type AttrOrigin ContainerClassGetChildPropertyFieldInfo = ContainerClass
    attrGet = getContainerClassGetChildProperty
    attrSet = setContainerClassGetChildProperty
    attrConstruct = undefined
    attrClear = clearContainerClassGetChildProperty
    attrTransfer _ v = do
        Gtk.Callbacks.mk_ContainerClassGetChildPropertyFieldCallback (Gtk.Callbacks.wrap_ContainerClassGetChildPropertyFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.ContainerClass.getChildProperty"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-ContainerClass.html#g:attr:getChildProperty"
        })

containerClass_getChildProperty :: AttrLabelProxy "getChildProperty"
containerClass_getChildProperty = AttrLabelProxy

#endif


-- | Get the value of the “@get_path_for_child@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' containerClass #getPathForChild
-- @
getContainerClassGetPathForChild :: MonadIO m => ContainerClass -> m (Maybe Gtk.Callbacks.ContainerClassGetPathForChildFieldCallback)
getContainerClassGetPathForChild s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 896) :: IO (FunPtr Gtk.Callbacks.C_ContainerClassGetPathForChildFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_ContainerClassGetPathForChildFieldCallback val'
        return val''
    return result

-- | Set the value of the “@get_path_for_child@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' containerClass [ #getPathForChild 'Data.GI.Base.Attributes.:=' value ]
-- @
setContainerClassGetPathForChild :: MonadIO m => ContainerClass -> FunPtr Gtk.Callbacks.C_ContainerClassGetPathForChildFieldCallback -> m ()
setContainerClassGetPathForChild s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 896) (val :: FunPtr Gtk.Callbacks.C_ContainerClassGetPathForChildFieldCallback)

-- | Set the value of the “@get_path_for_child@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #getPathForChild
-- @
clearContainerClassGetPathForChild :: MonadIO m => ContainerClass -> m ()
clearContainerClassGetPathForChild s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 896) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_ContainerClassGetPathForChildFieldCallback)

#if defined(ENABLE_OVERLOADING)
data ContainerClassGetPathForChildFieldInfo
instance AttrInfo ContainerClassGetPathForChildFieldInfo where
    type AttrBaseTypeConstraint ContainerClassGetPathForChildFieldInfo = (~) ContainerClass
    type AttrAllowedOps ContainerClassGetPathForChildFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint ContainerClassGetPathForChildFieldInfo = (~) (FunPtr Gtk.Callbacks.C_ContainerClassGetPathForChildFieldCallback)
    type AttrTransferTypeConstraint ContainerClassGetPathForChildFieldInfo = (~)Gtk.Callbacks.ContainerClassGetPathForChildFieldCallback
    type AttrTransferType ContainerClassGetPathForChildFieldInfo = (FunPtr Gtk.Callbacks.C_ContainerClassGetPathForChildFieldCallback)
    type AttrGetType ContainerClassGetPathForChildFieldInfo = Maybe Gtk.Callbacks.ContainerClassGetPathForChildFieldCallback
    type AttrLabel ContainerClassGetPathForChildFieldInfo = "get_path_for_child"
    type AttrOrigin ContainerClassGetPathForChildFieldInfo = ContainerClass
    attrGet = getContainerClassGetPathForChild
    attrSet = setContainerClassGetPathForChild
    attrConstruct = undefined
    attrClear = clearContainerClassGetPathForChild
    attrTransfer _ v = do
        Gtk.Callbacks.mk_ContainerClassGetPathForChildFieldCallback (Gtk.Callbacks.wrap_ContainerClassGetPathForChildFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.ContainerClass.getPathForChild"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-ContainerClass.html#g:attr:getPathForChild"
        })

containerClass_getPathForChild :: AttrLabelProxy "getPathForChild"
containerClass_getPathForChild = AttrLabelProxy

#endif



#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList ContainerClass
type instance O.AttributeList ContainerClass = ContainerClassAttributeList
type ContainerClassAttributeList = ('[ '("parentClass", ContainerClassParentClassFieldInfo), '("add", ContainerClassAddFieldInfo), '("remove", ContainerClassRemoveFieldInfo), '("checkResize", ContainerClassCheckResizeFieldInfo), '("forall", ContainerClassForallFieldInfo), '("setFocusChild", ContainerClassSetFocusChildFieldInfo), '("childType", ContainerClassChildTypeFieldInfo), '("compositeName", ContainerClassCompositeNameFieldInfo), '("setChildProperty", ContainerClassSetChildPropertyFieldInfo), '("getChildProperty", ContainerClassGetChildPropertyFieldInfo), '("getPathForChild", ContainerClassGetPathForChildFieldInfo)] :: [(Symbol, *)])
#endif

-- method ContainerClass::find_child_property
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cclass"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ContainerClass" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkContainerClass"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "property_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the name of the child property to find"
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
-- returnType: Just TParamSpec
-- throws : False
-- Skip return : False

foreign import ccall "gtk_container_class_find_child_property" gtk_container_class_find_child_property :: 
    Ptr ContainerClass ->                   -- cclass : TInterface (Name {namespace = "Gtk", name = "ContainerClass"})
    CString ->                              -- property_name : TBasicType TUTF8
    IO (Ptr GParamSpec)

-- | Finds a child property of a container class by name.
containerClassFindChildProperty ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    ContainerClass
    -- ^ /@cclass@/: a t'GI.Gtk.Structs.ContainerClass.ContainerClass'
    -> T.Text
    -- ^ /@propertyName@/: the name of the child property to find
    -> m (Maybe GParamSpec)
    -- ^ __Returns:__ the t'GI.GObject.Objects.ParamSpec.ParamSpec' of the child
    --     property or 'P.Nothing' if /@class@/ has no child property with that
    --     name.
containerClassFindChildProperty cclass propertyName = liftIO $ do
    cclass' <- unsafeManagedPtrGetPtr cclass
    propertyName' <- textToCString propertyName
    result <- gtk_container_class_find_child_property cclass' propertyName'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- B.GParamSpec.newGParamSpecFromPtr result'
        return result''
    touchManagedPtr cclass
    freeMem propertyName'
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data ContainerClassFindChildPropertyMethodInfo
instance (signature ~ (T.Text -> m (Maybe GParamSpec)), MonadIO m) => O.OverloadedMethod ContainerClassFindChildPropertyMethodInfo ContainerClass signature where
    overloadedMethod = containerClassFindChildProperty

instance O.OverloadedMethodInfo ContainerClassFindChildPropertyMethodInfo ContainerClass where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.ContainerClass.containerClassFindChildProperty",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-ContainerClass.html#v:containerClassFindChildProperty"
        })


#endif

-- method ContainerClass::handle_border_width
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "klass"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ContainerClass" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the class struct of a #GtkContainer subclass"
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

foreign import ccall "gtk_container_class_handle_border_width" gtk_container_class_handle_border_width :: 
    Ptr ContainerClass ->                   -- klass : TInterface (Name {namespace = "Gtk", name = "ContainerClass"})
    IO ()

-- | Modifies a subclass of t'GI.Gtk.Structs.ContainerClass.ContainerClass' to automatically add and
-- remove the border-width setting on GtkContainer.  This allows the
-- subclass to ignore the border width in its size request and
-- allocate methods. The intent is for a subclass to invoke this
-- in its class_init function.
-- 
-- 'GI.Gtk.Structs.ContainerClass.containerClassHandleBorderWidth' is necessary because it
-- would break API too badly to make this behavior the default. So
-- subclasses must “opt in” to the parent class handling border_width
-- for them.
containerClassHandleBorderWidth ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    ContainerClass
    -- ^ /@klass@/: the class struct of a t'GI.Gtk.Objects.Container.Container' subclass
    -> m ()
containerClassHandleBorderWidth klass = liftIO $ do
    klass' <- unsafeManagedPtrGetPtr klass
    gtk_container_class_handle_border_width klass'
    touchManagedPtr klass
    return ()

#if defined(ENABLE_OVERLOADING)
data ContainerClassHandleBorderWidthMethodInfo
instance (signature ~ (m ()), MonadIO m) => O.OverloadedMethod ContainerClassHandleBorderWidthMethodInfo ContainerClass signature where
    overloadedMethod = containerClassHandleBorderWidth

instance O.OverloadedMethodInfo ContainerClassHandleBorderWidthMethodInfo ContainerClass where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.ContainerClass.containerClassHandleBorderWidth",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-ContainerClass.html#v:containerClassHandleBorderWidth"
        })


#endif

-- XXX Could not generate method ContainerClass::install_child_properties
-- Not implemented: Don't know how to pack C array of type TCArray False (-1) 1 TParamSpec
-- method ContainerClass::install_child_property
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cclass"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ContainerClass" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkContainerClass"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "property_id"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the id for the property"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pspec"
--           , argType = TParamSpec
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GParamSpec for the property"
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

foreign import ccall "gtk_container_class_install_child_property" gtk_container_class_install_child_property :: 
    Ptr ContainerClass ->                   -- cclass : TInterface (Name {namespace = "Gtk", name = "ContainerClass"})
    Word32 ->                               -- property_id : TBasicType TUInt
    Ptr GParamSpec ->                       -- pspec : TParamSpec
    IO ()

-- | Installs a child property on a container class.
containerClassInstallChildProperty ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    ContainerClass
    -- ^ /@cclass@/: a t'GI.Gtk.Structs.ContainerClass.ContainerClass'
    -> Word32
    -- ^ /@propertyId@/: the id for the property
    -> GParamSpec
    -- ^ /@pspec@/: the t'GI.GObject.Objects.ParamSpec.ParamSpec' for the property
    -> m ()
containerClassInstallChildProperty cclass propertyId pspec = liftIO $ do
    cclass' <- unsafeManagedPtrGetPtr cclass
    pspec' <- unsafeManagedPtrGetPtr pspec
    gtk_container_class_install_child_property cclass' propertyId pspec'
    touchManagedPtr cclass
    touchManagedPtr pspec
    return ()

#if defined(ENABLE_OVERLOADING)
data ContainerClassInstallChildPropertyMethodInfo
instance (signature ~ (Word32 -> GParamSpec -> m ()), MonadIO m) => O.OverloadedMethod ContainerClassInstallChildPropertyMethodInfo ContainerClass signature where
    overloadedMethod = containerClassInstallChildProperty

instance O.OverloadedMethodInfo ContainerClassInstallChildPropertyMethodInfo ContainerClass where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.ContainerClass.containerClassInstallChildProperty",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-ContainerClass.html#v:containerClassInstallChildProperty"
        })


#endif

-- XXX Could not generate method ContainerClass::list_child_properties
-- Not implemented: unpackCArray : Don't know how to unpack C Array of type TParamSpec
#if defined(ENABLE_OVERLOADING)
type family ResolveContainerClassMethod (t :: Symbol) (o :: *) :: * where
    ResolveContainerClassMethod "findChildProperty" o = ContainerClassFindChildPropertyMethodInfo
    ResolveContainerClassMethod "handleBorderWidth" o = ContainerClassHandleBorderWidthMethodInfo
    ResolveContainerClassMethod "installChildProperty" o = ContainerClassInstallChildPropertyMethodInfo
    ResolveContainerClassMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveContainerClassMethod t ContainerClass, O.OverloadedMethod info ContainerClass p) => OL.IsLabel t (ContainerClass -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveContainerClassMethod t ContainerClass, O.OverloadedMethod info ContainerClass p, R.HasField t ContainerClass p) => R.HasField t ContainerClass p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveContainerClassMethod t ContainerClass, O.OverloadedMethodInfo info ContainerClass) => OL.IsLabel t (O.MethodProxy info ContainerClass) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


