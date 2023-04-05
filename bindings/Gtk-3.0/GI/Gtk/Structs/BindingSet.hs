{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A binding set maintains a list of activatable key bindings.
-- A single binding set can match multiple types of widgets.
-- Similar to style contexts, can be matched by any information contained
-- in a widgets t'GI.Gtk.Structs.WidgetPath.WidgetPath'. When a binding within a set is matched upon
-- activation, an action signal is emitted on the target widget to carry out
-- the actual activation.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.BindingSet
    ( 

-- * Exported types
    BindingSet(..)                          ,
    newZeroBindingSet                       ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Structs.BindingSet#g:method:activate"), [addPath]("GI.Gtk.Structs.BindingSet#g:method:addPath").
-- 
-- ==== Getters
-- /None/.
-- 
-- ==== Setters
-- /None/.

#if defined(ENABLE_OVERLOADING)
    ResolveBindingSetMethod                 ,
#endif

-- ** activate #method:activate#

#if defined(ENABLE_OVERLOADING)
    BindingSetActivateMethodInfo            ,
#endif
    bindingSetActivate                      ,


-- ** addPath #method:addPath#

#if defined(ENABLE_OVERLOADING)
    BindingSetAddPathMethodInfo             ,
#endif
    bindingSetAddPath                       ,


-- ** find #method:find#

    bindingSetFind                          ,




 -- * Properties


-- ** classBranchPspecs #attr:classBranchPspecs#
-- | unused

#if defined(ENABLE_OVERLOADING)
    bindingSet_classBranchPspecs            ,
#endif
    clearBindingSetClassBranchPspecs        ,
    getBindingSetClassBranchPspecs          ,
    setBindingSetClassBranchPspecs          ,


-- ** current #attr:current#
-- | implementation detail

#if defined(ENABLE_OVERLOADING)
    bindingSet_current                      ,
#endif
    clearBindingSetCurrent                  ,
    getBindingSetCurrent                    ,
    setBindingSetCurrent                    ,


-- ** entries #attr:entries#
-- | the key binding entries in this binding set

#if defined(ENABLE_OVERLOADING)
    bindingSet_entries                      ,
#endif
    clearBindingSetEntries                  ,
    getBindingSetEntries                    ,
    setBindingSetEntries                    ,


-- ** parsed #attr:parsed#
-- | whether this binding set stems from a CSS file and is reset upon theme changes

#if defined(ENABLE_OVERLOADING)
    bindingSet_parsed                       ,
#endif
    getBindingSetParsed                     ,
    setBindingSetParsed                     ,


-- ** priority #attr:priority#
-- | unused

#if defined(ENABLE_OVERLOADING)
    bindingSet_priority                     ,
#endif
    getBindingSetPriority                   ,
    setBindingSetPriority                   ,


-- ** setName #attr:setName#
-- | unique name of this binding set

#if defined(ENABLE_OVERLOADING)
    bindingSet_setName                      ,
#endif
    clearBindingSetSetName                  ,
    getBindingSetSetName                    ,
    setBindingSetSetName                    ,


-- ** widgetClassPspecs #attr:widgetClassPspecs#
-- | unused

#if defined(ENABLE_OVERLOADING)
    bindingSet_widgetClassPspecs            ,
#endif
    clearBindingSetWidgetClassPspecs        ,
    getBindingSetWidgetClassPspecs          ,
    setBindingSetWidgetClassPspecs          ,


-- ** widgetPathPspecs #attr:widgetPathPspecs#
-- | unused

#if defined(ENABLE_OVERLOADING)
    bindingSet_widgetPathPspecs             ,
#endif
    clearBindingSetWidgetPathPspecs         ,
    getBindingSetWidgetPathPspecs           ,
    setBindingSetWidgetPathPspecs           ,




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

import qualified GI.GObject.Objects.Object as GObject.Object
import qualified GI.Gdk.Flags as Gdk.Flags
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Structs.BindingEntry as Gtk.BindingEntry

-- | Memory-managed wrapper type.
newtype BindingSet = BindingSet (SP.ManagedPtr BindingSet)
    deriving (Eq)

instance SP.ManagedPtrNewtype BindingSet where
    toManagedPtr (BindingSet p) = p

instance BoxedPtr BindingSet where
    boxedPtrCopy = \p -> B.ManagedPtr.withManagedPtr p (copyBytes 64 >=> B.ManagedPtr.wrapPtr BindingSet)
    boxedPtrFree = \x -> SP.withManagedPtr x SP.freeMem
instance CallocPtr BindingSet where
    boxedPtrCalloc = callocBytes 64


-- | Construct a `BindingSet` struct initialized to zero.
newZeroBindingSet :: MonadIO m => m BindingSet
newZeroBindingSet = liftIO $ boxedPtrCalloc >>= wrapPtr BindingSet

instance tag ~ 'AttrSet => Constructible BindingSet tag where
    new _ attrs = do
        o <- newZeroBindingSet
        GI.Attributes.set o attrs
        return o


-- | Get the value of the “@set_name@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' bindingSet #setName
-- @
getBindingSetSetName :: MonadIO m => BindingSet -> m (Maybe T.Text)
getBindingSetSetName s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 0) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@set_name@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' bindingSet [ #setName 'Data.GI.Base.Attributes.:=' value ]
-- @
setBindingSetSetName :: MonadIO m => BindingSet -> CString -> m ()
setBindingSetSetName s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 0) (val :: CString)

-- | Set the value of the “@set_name@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #setName
-- @
clearBindingSetSetName :: MonadIO m => BindingSet -> m ()
clearBindingSetSetName s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 0) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data BindingSetSetNameFieldInfo
instance AttrInfo BindingSetSetNameFieldInfo where
    type AttrBaseTypeConstraint BindingSetSetNameFieldInfo = (~) BindingSet
    type AttrAllowedOps BindingSetSetNameFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint BindingSetSetNameFieldInfo = (~) CString
    type AttrTransferTypeConstraint BindingSetSetNameFieldInfo = (~)CString
    type AttrTransferType BindingSetSetNameFieldInfo = CString
    type AttrGetType BindingSetSetNameFieldInfo = Maybe T.Text
    type AttrLabel BindingSetSetNameFieldInfo = "set_name"
    type AttrOrigin BindingSetSetNameFieldInfo = BindingSet
    attrGet = getBindingSetSetName
    attrSet = setBindingSetSetName
    attrConstruct = undefined
    attrClear = clearBindingSetSetName
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.BindingSet.setName"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-BindingSet.html#g:attr:setName"
        })

bindingSet_setName :: AttrLabelProxy "setName"
bindingSet_setName = AttrLabelProxy

#endif


-- | Get the value of the “@priority@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' bindingSet #priority
-- @
getBindingSetPriority :: MonadIO m => BindingSet -> m Int32
getBindingSetPriority s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 8) :: IO Int32
    return val

-- | Set the value of the “@priority@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' bindingSet [ #priority 'Data.GI.Base.Attributes.:=' value ]
-- @
setBindingSetPriority :: MonadIO m => BindingSet -> Int32 -> m ()
setBindingSetPriority s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (val :: Int32)

#if defined(ENABLE_OVERLOADING)
data BindingSetPriorityFieldInfo
instance AttrInfo BindingSetPriorityFieldInfo where
    type AttrBaseTypeConstraint BindingSetPriorityFieldInfo = (~) BindingSet
    type AttrAllowedOps BindingSetPriorityFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint BindingSetPriorityFieldInfo = (~) Int32
    type AttrTransferTypeConstraint BindingSetPriorityFieldInfo = (~)Int32
    type AttrTransferType BindingSetPriorityFieldInfo = Int32
    type AttrGetType BindingSetPriorityFieldInfo = Int32
    type AttrLabel BindingSetPriorityFieldInfo = "priority"
    type AttrOrigin BindingSetPriorityFieldInfo = BindingSet
    attrGet = getBindingSetPriority
    attrSet = setBindingSetPriority
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.BindingSet.priority"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-BindingSet.html#g:attr:priority"
        })

bindingSet_priority :: AttrLabelProxy "priority"
bindingSet_priority = AttrLabelProxy

#endif


-- | Get the value of the “@widget_path_pspecs@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' bindingSet #widgetPathPspecs
-- @
getBindingSetWidgetPathPspecs :: MonadIO m => BindingSet -> m ([Ptr ()])
getBindingSetWidgetPathPspecs s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 16) :: IO (Ptr (GSList (Ptr ())))
    val' <- unpackGSList val
    return val'

-- | Set the value of the “@widget_path_pspecs@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' bindingSet [ #widgetPathPspecs 'Data.GI.Base.Attributes.:=' value ]
-- @
setBindingSetWidgetPathPspecs :: MonadIO m => BindingSet -> Ptr (GSList (Ptr ())) -> m ()
setBindingSetWidgetPathPspecs s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 16) (val :: Ptr (GSList (Ptr ())))

-- | Set the value of the “@widget_path_pspecs@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #widgetPathPspecs
-- @
clearBindingSetWidgetPathPspecs :: MonadIO m => BindingSet -> m ()
clearBindingSetWidgetPathPspecs s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 16) (FP.nullPtr :: Ptr (GSList (Ptr ())))

#if defined(ENABLE_OVERLOADING)
data BindingSetWidgetPathPspecsFieldInfo
instance AttrInfo BindingSetWidgetPathPspecsFieldInfo where
    type AttrBaseTypeConstraint BindingSetWidgetPathPspecsFieldInfo = (~) BindingSet
    type AttrAllowedOps BindingSetWidgetPathPspecsFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint BindingSetWidgetPathPspecsFieldInfo = (~) (Ptr (GSList (Ptr ())))
    type AttrTransferTypeConstraint BindingSetWidgetPathPspecsFieldInfo = (~)(Ptr (GSList (Ptr ())))
    type AttrTransferType BindingSetWidgetPathPspecsFieldInfo = (Ptr (GSList (Ptr ())))
    type AttrGetType BindingSetWidgetPathPspecsFieldInfo = [Ptr ()]
    type AttrLabel BindingSetWidgetPathPspecsFieldInfo = "widget_path_pspecs"
    type AttrOrigin BindingSetWidgetPathPspecsFieldInfo = BindingSet
    attrGet = getBindingSetWidgetPathPspecs
    attrSet = setBindingSetWidgetPathPspecs
    attrConstruct = undefined
    attrClear = clearBindingSetWidgetPathPspecs
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.BindingSet.widgetPathPspecs"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-BindingSet.html#g:attr:widgetPathPspecs"
        })

bindingSet_widgetPathPspecs :: AttrLabelProxy "widgetPathPspecs"
bindingSet_widgetPathPspecs = AttrLabelProxy

#endif


-- | Get the value of the “@widget_class_pspecs@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' bindingSet #widgetClassPspecs
-- @
getBindingSetWidgetClassPspecs :: MonadIO m => BindingSet -> m ([Ptr ()])
getBindingSetWidgetClassPspecs s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 24) :: IO (Ptr (GSList (Ptr ())))
    val' <- unpackGSList val
    return val'

-- | Set the value of the “@widget_class_pspecs@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' bindingSet [ #widgetClassPspecs 'Data.GI.Base.Attributes.:=' value ]
-- @
setBindingSetWidgetClassPspecs :: MonadIO m => BindingSet -> Ptr (GSList (Ptr ())) -> m ()
setBindingSetWidgetClassPspecs s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 24) (val :: Ptr (GSList (Ptr ())))

-- | Set the value of the “@widget_class_pspecs@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #widgetClassPspecs
-- @
clearBindingSetWidgetClassPspecs :: MonadIO m => BindingSet -> m ()
clearBindingSetWidgetClassPspecs s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 24) (FP.nullPtr :: Ptr (GSList (Ptr ())))

#if defined(ENABLE_OVERLOADING)
data BindingSetWidgetClassPspecsFieldInfo
instance AttrInfo BindingSetWidgetClassPspecsFieldInfo where
    type AttrBaseTypeConstraint BindingSetWidgetClassPspecsFieldInfo = (~) BindingSet
    type AttrAllowedOps BindingSetWidgetClassPspecsFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint BindingSetWidgetClassPspecsFieldInfo = (~) (Ptr (GSList (Ptr ())))
    type AttrTransferTypeConstraint BindingSetWidgetClassPspecsFieldInfo = (~)(Ptr (GSList (Ptr ())))
    type AttrTransferType BindingSetWidgetClassPspecsFieldInfo = (Ptr (GSList (Ptr ())))
    type AttrGetType BindingSetWidgetClassPspecsFieldInfo = [Ptr ()]
    type AttrLabel BindingSetWidgetClassPspecsFieldInfo = "widget_class_pspecs"
    type AttrOrigin BindingSetWidgetClassPspecsFieldInfo = BindingSet
    attrGet = getBindingSetWidgetClassPspecs
    attrSet = setBindingSetWidgetClassPspecs
    attrConstruct = undefined
    attrClear = clearBindingSetWidgetClassPspecs
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.BindingSet.widgetClassPspecs"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-BindingSet.html#g:attr:widgetClassPspecs"
        })

bindingSet_widgetClassPspecs :: AttrLabelProxy "widgetClassPspecs"
bindingSet_widgetClassPspecs = AttrLabelProxy

#endif


-- | Get the value of the “@class_branch_pspecs@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' bindingSet #classBranchPspecs
-- @
getBindingSetClassBranchPspecs :: MonadIO m => BindingSet -> m ([Ptr ()])
getBindingSetClassBranchPspecs s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 32) :: IO (Ptr (GSList (Ptr ())))
    val' <- unpackGSList val
    return val'

-- | Set the value of the “@class_branch_pspecs@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' bindingSet [ #classBranchPspecs 'Data.GI.Base.Attributes.:=' value ]
-- @
setBindingSetClassBranchPspecs :: MonadIO m => BindingSet -> Ptr (GSList (Ptr ())) -> m ()
setBindingSetClassBranchPspecs s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 32) (val :: Ptr (GSList (Ptr ())))

-- | Set the value of the “@class_branch_pspecs@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #classBranchPspecs
-- @
clearBindingSetClassBranchPspecs :: MonadIO m => BindingSet -> m ()
clearBindingSetClassBranchPspecs s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 32) (FP.nullPtr :: Ptr (GSList (Ptr ())))

#if defined(ENABLE_OVERLOADING)
data BindingSetClassBranchPspecsFieldInfo
instance AttrInfo BindingSetClassBranchPspecsFieldInfo where
    type AttrBaseTypeConstraint BindingSetClassBranchPspecsFieldInfo = (~) BindingSet
    type AttrAllowedOps BindingSetClassBranchPspecsFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint BindingSetClassBranchPspecsFieldInfo = (~) (Ptr (GSList (Ptr ())))
    type AttrTransferTypeConstraint BindingSetClassBranchPspecsFieldInfo = (~)(Ptr (GSList (Ptr ())))
    type AttrTransferType BindingSetClassBranchPspecsFieldInfo = (Ptr (GSList (Ptr ())))
    type AttrGetType BindingSetClassBranchPspecsFieldInfo = [Ptr ()]
    type AttrLabel BindingSetClassBranchPspecsFieldInfo = "class_branch_pspecs"
    type AttrOrigin BindingSetClassBranchPspecsFieldInfo = BindingSet
    attrGet = getBindingSetClassBranchPspecs
    attrSet = setBindingSetClassBranchPspecs
    attrConstruct = undefined
    attrClear = clearBindingSetClassBranchPspecs
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.BindingSet.classBranchPspecs"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-BindingSet.html#g:attr:classBranchPspecs"
        })

bindingSet_classBranchPspecs :: AttrLabelProxy "classBranchPspecs"
bindingSet_classBranchPspecs = AttrLabelProxy

#endif


-- | Get the value of the “@entries@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' bindingSet #entries
-- @
getBindingSetEntries :: MonadIO m => BindingSet -> m (Maybe Gtk.BindingEntry.BindingEntry)
getBindingSetEntries s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 40) :: IO (Ptr Gtk.BindingEntry.BindingEntry)
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- (newPtr Gtk.BindingEntry.BindingEntry) val'
        return val''
    return result

-- | Set the value of the “@entries@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' bindingSet [ #entries 'Data.GI.Base.Attributes.:=' value ]
-- @
setBindingSetEntries :: MonadIO m => BindingSet -> Ptr Gtk.BindingEntry.BindingEntry -> m ()
setBindingSetEntries s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 40) (val :: Ptr Gtk.BindingEntry.BindingEntry)

-- | Set the value of the “@entries@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #entries
-- @
clearBindingSetEntries :: MonadIO m => BindingSet -> m ()
clearBindingSetEntries s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 40) (FP.nullPtr :: Ptr Gtk.BindingEntry.BindingEntry)

#if defined(ENABLE_OVERLOADING)
data BindingSetEntriesFieldInfo
instance AttrInfo BindingSetEntriesFieldInfo where
    type AttrBaseTypeConstraint BindingSetEntriesFieldInfo = (~) BindingSet
    type AttrAllowedOps BindingSetEntriesFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint BindingSetEntriesFieldInfo = (~) (Ptr Gtk.BindingEntry.BindingEntry)
    type AttrTransferTypeConstraint BindingSetEntriesFieldInfo = (~)(Ptr Gtk.BindingEntry.BindingEntry)
    type AttrTransferType BindingSetEntriesFieldInfo = (Ptr Gtk.BindingEntry.BindingEntry)
    type AttrGetType BindingSetEntriesFieldInfo = Maybe Gtk.BindingEntry.BindingEntry
    type AttrLabel BindingSetEntriesFieldInfo = "entries"
    type AttrOrigin BindingSetEntriesFieldInfo = BindingSet
    attrGet = getBindingSetEntries
    attrSet = setBindingSetEntries
    attrConstruct = undefined
    attrClear = clearBindingSetEntries
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.BindingSet.entries"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-BindingSet.html#g:attr:entries"
        })

bindingSet_entries :: AttrLabelProxy "entries"
bindingSet_entries = AttrLabelProxy

#endif


-- | Get the value of the “@current@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' bindingSet #current
-- @
getBindingSetCurrent :: MonadIO m => BindingSet -> m (Maybe Gtk.BindingEntry.BindingEntry)
getBindingSetCurrent s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 48) :: IO (Ptr Gtk.BindingEntry.BindingEntry)
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- (newPtr Gtk.BindingEntry.BindingEntry) val'
        return val''
    return result

-- | Set the value of the “@current@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' bindingSet [ #current 'Data.GI.Base.Attributes.:=' value ]
-- @
setBindingSetCurrent :: MonadIO m => BindingSet -> Ptr Gtk.BindingEntry.BindingEntry -> m ()
setBindingSetCurrent s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 48) (val :: Ptr Gtk.BindingEntry.BindingEntry)

-- | Set the value of the “@current@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #current
-- @
clearBindingSetCurrent :: MonadIO m => BindingSet -> m ()
clearBindingSetCurrent s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 48) (FP.nullPtr :: Ptr Gtk.BindingEntry.BindingEntry)

#if defined(ENABLE_OVERLOADING)
data BindingSetCurrentFieldInfo
instance AttrInfo BindingSetCurrentFieldInfo where
    type AttrBaseTypeConstraint BindingSetCurrentFieldInfo = (~) BindingSet
    type AttrAllowedOps BindingSetCurrentFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint BindingSetCurrentFieldInfo = (~) (Ptr Gtk.BindingEntry.BindingEntry)
    type AttrTransferTypeConstraint BindingSetCurrentFieldInfo = (~)(Ptr Gtk.BindingEntry.BindingEntry)
    type AttrTransferType BindingSetCurrentFieldInfo = (Ptr Gtk.BindingEntry.BindingEntry)
    type AttrGetType BindingSetCurrentFieldInfo = Maybe Gtk.BindingEntry.BindingEntry
    type AttrLabel BindingSetCurrentFieldInfo = "current"
    type AttrOrigin BindingSetCurrentFieldInfo = BindingSet
    attrGet = getBindingSetCurrent
    attrSet = setBindingSetCurrent
    attrConstruct = undefined
    attrClear = clearBindingSetCurrent
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.BindingSet.current"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-BindingSet.html#g:attr:current"
        })

bindingSet_current :: AttrLabelProxy "current"
bindingSet_current = AttrLabelProxy

#endif


-- | Get the value of the “@parsed@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' bindingSet #parsed
-- @
getBindingSetParsed :: MonadIO m => BindingSet -> m Word32
getBindingSetParsed s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 56) :: IO Word32
    return val

-- | Set the value of the “@parsed@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' bindingSet [ #parsed 'Data.GI.Base.Attributes.:=' value ]
-- @
setBindingSetParsed :: MonadIO m => BindingSet -> Word32 -> m ()
setBindingSetParsed s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 56) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data BindingSetParsedFieldInfo
instance AttrInfo BindingSetParsedFieldInfo where
    type AttrBaseTypeConstraint BindingSetParsedFieldInfo = (~) BindingSet
    type AttrAllowedOps BindingSetParsedFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint BindingSetParsedFieldInfo = (~) Word32
    type AttrTransferTypeConstraint BindingSetParsedFieldInfo = (~)Word32
    type AttrTransferType BindingSetParsedFieldInfo = Word32
    type AttrGetType BindingSetParsedFieldInfo = Word32
    type AttrLabel BindingSetParsedFieldInfo = "parsed"
    type AttrOrigin BindingSetParsedFieldInfo = BindingSet
    attrGet = getBindingSetParsed
    attrSet = setBindingSetParsed
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.BindingSet.parsed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-BindingSet.html#g:attr:parsed"
        })

bindingSet_parsed :: AttrLabelProxy "parsed"
bindingSet_parsed = AttrLabelProxy

#endif



#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList BindingSet
type instance O.AttributeList BindingSet = BindingSetAttributeList
type BindingSetAttributeList = ('[ '("setName", BindingSetSetNameFieldInfo), '("priority", BindingSetPriorityFieldInfo), '("widgetPathPspecs", BindingSetWidgetPathPspecsFieldInfo), '("widgetClassPspecs", BindingSetWidgetClassPspecsFieldInfo), '("classBranchPspecs", BindingSetClassBranchPspecsFieldInfo), '("entries", BindingSetEntriesFieldInfo), '("current", BindingSetCurrentFieldInfo), '("parsed", BindingSetParsedFieldInfo)] :: [(Symbol, *)])
#endif

-- method BindingSet::activate
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "binding_set"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "BindingSet" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBindingSet set to activate"
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
--                 { rawDocText = Just "key value of the binding"
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
--                 { rawDocText = Just "key modifier of the binding"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "object"
--           , argType =
--               TInterface Name { namespace = "GObject" , name = "Object" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "object to activate when binding found"
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
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_binding_set_activate" gtk_binding_set_activate :: 
    Ptr BindingSet ->                       -- binding_set : TInterface (Name {namespace = "Gtk", name = "BindingSet"})
    Word32 ->                               -- keyval : TBasicType TUInt
    CUInt ->                                -- modifiers : TInterface (Name {namespace = "Gdk", name = "ModifierType"})
    Ptr GObject.Object.Object ->            -- object : TInterface (Name {namespace = "GObject", name = "Object"})
    IO CInt

-- | Find a key binding matching /@keyval@/ and /@modifiers@/ within
-- /@bindingSet@/ and activate the binding on /@object@/.
bindingSetActivate ::
    (B.CallStack.HasCallStack, MonadIO m, GObject.Object.IsObject a) =>
    BindingSet
    -- ^ /@bindingSet@/: a t'GI.Gtk.Structs.BindingSet.BindingSet' set to activate
    -> Word32
    -- ^ /@keyval@/: key value of the binding
    -> [Gdk.Flags.ModifierType]
    -- ^ /@modifiers@/: key modifier of the binding
    -> a
    -- ^ /@object@/: object to activate when binding found
    -> m Bool
    -- ^ __Returns:__ 'P.True' if a binding was found and activated
bindingSetActivate bindingSet keyval modifiers object = liftIO $ do
    bindingSet' <- unsafeManagedPtrGetPtr bindingSet
    let modifiers' = gflagsToWord modifiers
    object' <- unsafeManagedPtrCastPtr object
    result <- gtk_binding_set_activate bindingSet' keyval modifiers' object'
    let result' = (/= 0) result
    touchManagedPtr bindingSet
    touchManagedPtr object
    return result'

#if defined(ENABLE_OVERLOADING)
data BindingSetActivateMethodInfo
instance (signature ~ (Word32 -> [Gdk.Flags.ModifierType] -> a -> m Bool), MonadIO m, GObject.Object.IsObject a) => O.OverloadedMethod BindingSetActivateMethodInfo BindingSet signature where
    overloadedMethod = bindingSetActivate

instance O.OverloadedMethodInfo BindingSetActivateMethodInfo BindingSet where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.BindingSet.bindingSetActivate",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-BindingSet.html#v:bindingSetActivate"
        })


#endif

-- method BindingSet::add_path
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "binding_set"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "BindingSet" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBindingSet to add a path to"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PathType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "path type the pattern applies to"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path_pattern"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the actual match pattern"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "priority"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PathPriorityType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "binding priority" , sinceVersion = Nothing }
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

foreign import ccall "gtk_binding_set_add_path" gtk_binding_set_add_path :: 
    Ptr BindingSet ->                       -- binding_set : TInterface (Name {namespace = "Gtk", name = "BindingSet"})
    CUInt ->                                -- path_type : TInterface (Name {namespace = "Gtk", name = "PathType"})
    CString ->                              -- path_pattern : TBasicType TUTF8
    CUInt ->                                -- priority : TInterface (Name {namespace = "Gtk", name = "PathPriorityType"})
    IO ()

{-# DEPRECATED bindingSetAddPath ["(Since version 3.0)"] #-}
-- | This function was used internally by the GtkRC parsing mechanism
-- to assign match patterns to t'GI.Gtk.Structs.BindingSet.BindingSet' structures.
-- 
-- In GTK+ 3, these match patterns are unused.
bindingSetAddPath ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    BindingSet
    -- ^ /@bindingSet@/: a t'GI.Gtk.Structs.BindingSet.BindingSet' to add a path to
    -> Gtk.Enums.PathType
    -- ^ /@pathType@/: path type the pattern applies to
    -> T.Text
    -- ^ /@pathPattern@/: the actual match pattern
    -> Gtk.Enums.PathPriorityType
    -- ^ /@priority@/: binding priority
    -> m ()
bindingSetAddPath bindingSet pathType pathPattern priority = liftIO $ do
    bindingSet' <- unsafeManagedPtrGetPtr bindingSet
    let pathType' = (fromIntegral . fromEnum) pathType
    pathPattern' <- textToCString pathPattern
    let priority' = (fromIntegral . fromEnum) priority
    gtk_binding_set_add_path bindingSet' pathType' pathPattern' priority'
    touchManagedPtr bindingSet
    freeMem pathPattern'
    return ()

#if defined(ENABLE_OVERLOADING)
data BindingSetAddPathMethodInfo
instance (signature ~ (Gtk.Enums.PathType -> T.Text -> Gtk.Enums.PathPriorityType -> m ()), MonadIO m) => O.OverloadedMethod BindingSetAddPathMethodInfo BindingSet signature where
    overloadedMethod = bindingSetAddPath

instance O.OverloadedMethodInfo BindingSetAddPathMethodInfo BindingSet where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.BindingSet.bindingSetAddPath",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-BindingSet.html#v:bindingSetAddPath"
        })


#endif

-- method BindingSet::find
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "set_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "unique binding set name"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "BindingSet" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_binding_set_find" gtk_binding_set_find :: 
    CString ->                              -- set_name : TBasicType TUTF8
    IO (Ptr BindingSet)

-- | Find a binding set by its globally unique name.
-- 
-- The /@setName@/ can either be a name used for @/gtk_binding_set_new()/@
-- or the type name of a class used in @/gtk_binding_set_by_class()/@.
bindingSetFind ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@setName@/: unique binding set name
    -> m (Maybe BindingSet)
    -- ^ __Returns:__ 'P.Nothing' or the specified binding set
bindingSetFind setName = liftIO $ do
    setName' <- textToCString setName
    result <- gtk_binding_set_find setName'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newPtr BindingSet) result'
        return result''
    freeMem setName'
    return maybeResult

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveBindingSetMethod (t :: Symbol) (o :: *) :: * where
    ResolveBindingSetMethod "activate" o = BindingSetActivateMethodInfo
    ResolveBindingSetMethod "addPath" o = BindingSetAddPathMethodInfo
    ResolveBindingSetMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveBindingSetMethod t BindingSet, O.OverloadedMethod info BindingSet p) => OL.IsLabel t (BindingSet -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveBindingSetMethod t BindingSet, O.OverloadedMethod info BindingSet p, R.HasField t BindingSet p) => R.HasField t BindingSet p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveBindingSetMethod t BindingSet, O.OverloadedMethodInfo info BindingSet) => OL.IsLabel t (O.MethodProxy info BindingSet) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


