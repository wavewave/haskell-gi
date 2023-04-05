{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- This should not be accessed directly. Use the accessor functions below.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.MountOperation
    ( 

-- * Exported types
    MountOperation(..)                      ,
    IsMountOperation                        ,
    toMountOperation                        ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isShowing]("GI.Gtk.Objects.MountOperation#g:method:isShowing"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [reply]("GI.Gio.Objects.MountOperation#g:method:reply"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAnonymous]("GI.Gio.Objects.MountOperation#g:method:getAnonymous"), [getChoice]("GI.Gio.Objects.MountOperation#g:method:getChoice"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDomain]("GI.Gio.Objects.MountOperation#g:method:getDomain"), [getIsTcryptHiddenVolume]("GI.Gio.Objects.MountOperation#g:method:getIsTcryptHiddenVolume"), [getIsTcryptSystemVolume]("GI.Gio.Objects.MountOperation#g:method:getIsTcryptSystemVolume"), [getParent]("GI.Gtk.Objects.MountOperation#g:method:getParent"), [getPassword]("GI.Gio.Objects.MountOperation#g:method:getPassword"), [getPasswordSave]("GI.Gio.Objects.MountOperation#g:method:getPasswordSave"), [getPim]("GI.Gio.Objects.MountOperation#g:method:getPim"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getScreen]("GI.Gtk.Objects.MountOperation#g:method:getScreen"), [getUsername]("GI.Gio.Objects.MountOperation#g:method:getUsername").
-- 
-- ==== Setters
-- [setAnonymous]("GI.Gio.Objects.MountOperation#g:method:setAnonymous"), [setChoice]("GI.Gio.Objects.MountOperation#g:method:setChoice"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDomain]("GI.Gio.Objects.MountOperation#g:method:setDomain"), [setIsTcryptHiddenVolume]("GI.Gio.Objects.MountOperation#g:method:setIsTcryptHiddenVolume"), [setIsTcryptSystemVolume]("GI.Gio.Objects.MountOperation#g:method:setIsTcryptSystemVolume"), [setParent]("GI.Gtk.Objects.MountOperation#g:method:setParent"), [setPassword]("GI.Gio.Objects.MountOperation#g:method:setPassword"), [setPasswordSave]("GI.Gio.Objects.MountOperation#g:method:setPasswordSave"), [setPim]("GI.Gio.Objects.MountOperation#g:method:setPim"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setScreen]("GI.Gtk.Objects.MountOperation#g:method:setScreen"), [setUsername]("GI.Gio.Objects.MountOperation#g:method:setUsername").

#if defined(ENABLE_OVERLOADING)
    ResolveMountOperationMethod             ,
#endif

-- ** getParent #method:getParent#

#if defined(ENABLE_OVERLOADING)
    MountOperationGetParentMethodInfo       ,
#endif
    mountOperationGetParent                 ,


-- ** getScreen #method:getScreen#

#if defined(ENABLE_OVERLOADING)
    MountOperationGetScreenMethodInfo       ,
#endif
    mountOperationGetScreen                 ,


-- ** isShowing #method:isShowing#

#if defined(ENABLE_OVERLOADING)
    MountOperationIsShowingMethodInfo       ,
#endif
    mountOperationIsShowing                 ,


-- ** new #method:new#

    mountOperationNew                       ,


-- ** setParent #method:setParent#

#if defined(ENABLE_OVERLOADING)
    MountOperationSetParentMethodInfo       ,
#endif
    mountOperationSetParent                 ,


-- ** setScreen #method:setScreen#

#if defined(ENABLE_OVERLOADING)
    MountOperationSetScreenMethodInfo       ,
#endif
    mountOperationSetScreen                 ,




 -- * Properties


-- ** isShowing #attr:isShowing#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    MountOperationIsShowingPropertyInfo     ,
#endif
    getMountOperationIsShowing              ,


-- ** parent #attr:parent#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    MountOperationParentPropertyInfo        ,
#endif
    clearMountOperationParent               ,
    constructMountOperationParent           ,
    getMountOperationParent                 ,
#if defined(ENABLE_OVERLOADING)
    mountOperationParent                    ,
#endif
    setMountOperationParent                 ,


-- ** screen #attr:screen#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    MountOperationScreenPropertyInfo        ,
#endif
    constructMountOperationScreen           ,
    getMountOperationScreen                 ,
#if defined(ENABLE_OVERLOADING)
    mountOperationScreen                    ,
#endif
    setMountOperationScreen                 ,




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
import qualified GI.Gdk.Objects.Screen as Gdk.Screen
import qualified GI.Gio.Objects.MountOperation as Gio.MountOperation
import {-# SOURCE #-} qualified GI.Gtk.Objects.Window as Gtk.Window

-- | Memory-managed wrapper type.
newtype MountOperation = MountOperation (SP.ManagedPtr MountOperation)
    deriving (Eq)

instance SP.ManagedPtrNewtype MountOperation where
    toManagedPtr (MountOperation p) = p

foreign import ccall "gtk_mount_operation_get_type"
    c_gtk_mount_operation_get_type :: IO B.Types.GType

instance B.Types.TypedObject MountOperation where
    glibType = c_gtk_mount_operation_get_type

instance B.Types.GObject MountOperation

-- | Type class for types which can be safely cast to `MountOperation`, for instance with `toMountOperation`.
class (SP.GObject o, O.IsDescendantOf MountOperation o) => IsMountOperation o
instance (SP.GObject o, O.IsDescendantOf MountOperation o) => IsMountOperation o

instance O.HasParentTypes MountOperation
type instance O.ParentTypes MountOperation = '[Gio.MountOperation.MountOperation, GObject.Object.Object]

-- | Cast to `MountOperation`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toMountOperation :: (MIO.MonadIO m, IsMountOperation o) => o -> m MountOperation
toMountOperation = MIO.liftIO . B.ManagedPtr.unsafeCastTo MountOperation

-- | Convert 'MountOperation' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe MountOperation) where
    gvalueGType_ = c_gtk_mount_operation_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr MountOperation)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr MountOperation)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject MountOperation ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveMountOperationMethod (t :: Symbol) (o :: *) :: * where
    ResolveMountOperationMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveMountOperationMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveMountOperationMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveMountOperationMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveMountOperationMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveMountOperationMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveMountOperationMethod "isShowing" o = MountOperationIsShowingMethodInfo
    ResolveMountOperationMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveMountOperationMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveMountOperationMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveMountOperationMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveMountOperationMethod "reply" o = Gio.MountOperation.MountOperationReplyMethodInfo
    ResolveMountOperationMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveMountOperationMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveMountOperationMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveMountOperationMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveMountOperationMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveMountOperationMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveMountOperationMethod "getAnonymous" o = Gio.MountOperation.MountOperationGetAnonymousMethodInfo
    ResolveMountOperationMethod "getChoice" o = Gio.MountOperation.MountOperationGetChoiceMethodInfo
    ResolveMountOperationMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveMountOperationMethod "getDomain" o = Gio.MountOperation.MountOperationGetDomainMethodInfo
    ResolveMountOperationMethod "getIsTcryptHiddenVolume" o = Gio.MountOperation.MountOperationGetIsTcryptHiddenVolumeMethodInfo
    ResolveMountOperationMethod "getIsTcryptSystemVolume" o = Gio.MountOperation.MountOperationGetIsTcryptSystemVolumeMethodInfo
    ResolveMountOperationMethod "getParent" o = MountOperationGetParentMethodInfo
    ResolveMountOperationMethod "getPassword" o = Gio.MountOperation.MountOperationGetPasswordMethodInfo
    ResolveMountOperationMethod "getPasswordSave" o = Gio.MountOperation.MountOperationGetPasswordSaveMethodInfo
    ResolveMountOperationMethod "getPim" o = Gio.MountOperation.MountOperationGetPimMethodInfo
    ResolveMountOperationMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveMountOperationMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveMountOperationMethod "getScreen" o = MountOperationGetScreenMethodInfo
    ResolveMountOperationMethod "getUsername" o = Gio.MountOperation.MountOperationGetUsernameMethodInfo
    ResolveMountOperationMethod "setAnonymous" o = Gio.MountOperation.MountOperationSetAnonymousMethodInfo
    ResolveMountOperationMethod "setChoice" o = Gio.MountOperation.MountOperationSetChoiceMethodInfo
    ResolveMountOperationMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveMountOperationMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveMountOperationMethod "setDomain" o = Gio.MountOperation.MountOperationSetDomainMethodInfo
    ResolveMountOperationMethod "setIsTcryptHiddenVolume" o = Gio.MountOperation.MountOperationSetIsTcryptHiddenVolumeMethodInfo
    ResolveMountOperationMethod "setIsTcryptSystemVolume" o = Gio.MountOperation.MountOperationSetIsTcryptSystemVolumeMethodInfo
    ResolveMountOperationMethod "setParent" o = MountOperationSetParentMethodInfo
    ResolveMountOperationMethod "setPassword" o = Gio.MountOperation.MountOperationSetPasswordMethodInfo
    ResolveMountOperationMethod "setPasswordSave" o = Gio.MountOperation.MountOperationSetPasswordSaveMethodInfo
    ResolveMountOperationMethod "setPim" o = Gio.MountOperation.MountOperationSetPimMethodInfo
    ResolveMountOperationMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveMountOperationMethod "setScreen" o = MountOperationSetScreenMethodInfo
    ResolveMountOperationMethod "setUsername" o = Gio.MountOperation.MountOperationSetUsernameMethodInfo
    ResolveMountOperationMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveMountOperationMethod t MountOperation, O.OverloadedMethod info MountOperation p) => OL.IsLabel t (MountOperation -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveMountOperationMethod t MountOperation, O.OverloadedMethod info MountOperation p, R.HasField t MountOperation p) => R.HasField t MountOperation p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveMountOperationMethod t MountOperation, O.OverloadedMethodInfo info MountOperation) => OL.IsLabel t (O.MethodProxy info MountOperation) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- VVV Prop "is-showing"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@is-showing@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' mountOperation #isShowing
-- @
getMountOperationIsShowing :: (MonadIO m, IsMountOperation o) => o -> m Bool
getMountOperationIsShowing obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "is-showing"

#if defined(ENABLE_OVERLOADING)
data MountOperationIsShowingPropertyInfo
instance AttrInfo MountOperationIsShowingPropertyInfo where
    type AttrAllowedOps MountOperationIsShowingPropertyInfo = '[ 'AttrGet]
    type AttrBaseTypeConstraint MountOperationIsShowingPropertyInfo = IsMountOperation
    type AttrSetTypeConstraint MountOperationIsShowingPropertyInfo = (~) ()
    type AttrTransferTypeConstraint MountOperationIsShowingPropertyInfo = (~) ()
    type AttrTransferType MountOperationIsShowingPropertyInfo = ()
    type AttrGetType MountOperationIsShowingPropertyInfo = Bool
    type AttrLabel MountOperationIsShowingPropertyInfo = "is-showing"
    type AttrOrigin MountOperationIsShowingPropertyInfo = MountOperation
    attrGet = getMountOperationIsShowing
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MountOperation.isShowing"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MountOperation.html#g:attr:isShowing"
        })
#endif

-- VVV Prop "parent"
   -- Type: TInterface (Name {namespace = "Gtk", name = "Window"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just True)

-- | Get the value of the “@parent@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' mountOperation #parent
-- @
getMountOperationParent :: (MonadIO m, IsMountOperation o) => o -> m Gtk.Window.Window
getMountOperationParent obj = MIO.liftIO $ checkUnexpectedNothing "getMountOperationParent" $ B.Properties.getObjectPropertyObject obj "parent" Gtk.Window.Window

-- | Set the value of the “@parent@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' mountOperation [ #parent 'Data.GI.Base.Attributes.:=' value ]
-- @
setMountOperationParent :: (MonadIO m, IsMountOperation o, Gtk.Window.IsWindow a) => o -> a -> m ()
setMountOperationParent obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "parent" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@parent@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructMountOperationParent :: (IsMountOperation o, MIO.MonadIO m, Gtk.Window.IsWindow a) => a -> m (GValueConstruct o)
constructMountOperationParent val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "parent" (P.Just val)

-- | Set the value of the “@parent@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #parent
-- @
clearMountOperationParent :: (MonadIO m, IsMountOperation o) => o -> m ()
clearMountOperationParent obj = liftIO $ B.Properties.setObjectPropertyObject obj "parent" (Nothing :: Maybe Gtk.Window.Window)

#if defined(ENABLE_OVERLOADING)
data MountOperationParentPropertyInfo
instance AttrInfo MountOperationParentPropertyInfo where
    type AttrAllowedOps MountOperationParentPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint MountOperationParentPropertyInfo = IsMountOperation
    type AttrSetTypeConstraint MountOperationParentPropertyInfo = Gtk.Window.IsWindow
    type AttrTransferTypeConstraint MountOperationParentPropertyInfo = Gtk.Window.IsWindow
    type AttrTransferType MountOperationParentPropertyInfo = Gtk.Window.Window
    type AttrGetType MountOperationParentPropertyInfo = Gtk.Window.Window
    type AttrLabel MountOperationParentPropertyInfo = "parent"
    type AttrOrigin MountOperationParentPropertyInfo = MountOperation
    attrGet = getMountOperationParent
    attrSet = setMountOperationParent
    attrTransfer _ v = do
        unsafeCastTo Gtk.Window.Window v
    attrConstruct = constructMountOperationParent
    attrClear = clearMountOperationParent
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MountOperation.parent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MountOperation.html#g:attr:parent"
        })
#endif

-- VVV Prop "screen"
   -- Type: TInterface (Name {namespace = "Gdk", name = "Screen"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@screen@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' mountOperation #screen
-- @
getMountOperationScreen :: (MonadIO m, IsMountOperation o) => o -> m Gdk.Screen.Screen
getMountOperationScreen obj = MIO.liftIO $ checkUnexpectedNothing "getMountOperationScreen" $ B.Properties.getObjectPropertyObject obj "screen" Gdk.Screen.Screen

-- | Set the value of the “@screen@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' mountOperation [ #screen 'Data.GI.Base.Attributes.:=' value ]
-- @
setMountOperationScreen :: (MonadIO m, IsMountOperation o, Gdk.Screen.IsScreen a) => o -> a -> m ()
setMountOperationScreen obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "screen" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@screen@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructMountOperationScreen :: (IsMountOperation o, MIO.MonadIO m, Gdk.Screen.IsScreen a) => a -> m (GValueConstruct o)
constructMountOperationScreen val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "screen" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data MountOperationScreenPropertyInfo
instance AttrInfo MountOperationScreenPropertyInfo where
    type AttrAllowedOps MountOperationScreenPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint MountOperationScreenPropertyInfo = IsMountOperation
    type AttrSetTypeConstraint MountOperationScreenPropertyInfo = Gdk.Screen.IsScreen
    type AttrTransferTypeConstraint MountOperationScreenPropertyInfo = Gdk.Screen.IsScreen
    type AttrTransferType MountOperationScreenPropertyInfo = Gdk.Screen.Screen
    type AttrGetType MountOperationScreenPropertyInfo = Gdk.Screen.Screen
    type AttrLabel MountOperationScreenPropertyInfo = "screen"
    type AttrOrigin MountOperationScreenPropertyInfo = MountOperation
    attrGet = getMountOperationScreen
    attrSet = setMountOperationScreen
    attrTransfer _ v = do
        unsafeCastTo Gdk.Screen.Screen v
    attrConstruct = constructMountOperationScreen
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MountOperation.screen"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MountOperation.html#g:attr:screen"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList MountOperation
type instance O.AttributeList MountOperation = MountOperationAttributeList
type MountOperationAttributeList = ('[ '("anonymous", Gio.MountOperation.MountOperationAnonymousPropertyInfo), '("choice", Gio.MountOperation.MountOperationChoicePropertyInfo), '("domain", Gio.MountOperation.MountOperationDomainPropertyInfo), '("isShowing", MountOperationIsShowingPropertyInfo), '("isTcryptHiddenVolume", Gio.MountOperation.MountOperationIsTcryptHiddenVolumePropertyInfo), '("isTcryptSystemVolume", Gio.MountOperation.MountOperationIsTcryptSystemVolumePropertyInfo), '("parent", MountOperationParentPropertyInfo), '("password", Gio.MountOperation.MountOperationPasswordPropertyInfo), '("passwordSave", Gio.MountOperation.MountOperationPasswordSavePropertyInfo), '("pim", Gio.MountOperation.MountOperationPimPropertyInfo), '("screen", MountOperationScreenPropertyInfo), '("username", Gio.MountOperation.MountOperationUsernamePropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
mountOperationParent :: AttrLabelProxy "parent"
mountOperationParent = AttrLabelProxy

mountOperationScreen :: AttrLabelProxy "screen"
mountOperationScreen = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList MountOperation = MountOperationSignalList
type MountOperationSignalList = ('[ '("aborted", Gio.MountOperation.MountOperationAbortedSignalInfo), '("askPassword", Gio.MountOperation.MountOperationAskPasswordSignalInfo), '("askQuestion", Gio.MountOperation.MountOperationAskQuestionSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("reply", Gio.MountOperation.MountOperationReplySignalInfo), '("showProcesses", Gio.MountOperation.MountOperationShowProcessesSignalInfo), '("showUnmountProgress", Gio.MountOperation.MountOperationShowUnmountProgressSignalInfo)] :: [(Symbol, *)])

#endif

-- method MountOperation::new
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "parent"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "transient parent of the window, or %NULL"
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
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "MountOperation" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_mount_operation_new" gtk_mount_operation_new :: 
    Ptr Gtk.Window.Window ->                -- parent : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO (Ptr MountOperation)

-- | Creates a new t'GI.Gtk.Objects.MountOperation.MountOperation'
-- 
-- /Since: 2.14/
mountOperationNew ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Window.IsWindow a) =>
    Maybe (a)
    -- ^ /@parent@/: transient parent of the window, or 'P.Nothing'
    -> m MountOperation
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.MountOperation.MountOperation'
mountOperationNew parent = liftIO $ do
    maybeParent <- case parent of
        Nothing -> return nullPtr
        Just jParent -> do
            jParent' <- unsafeManagedPtrCastPtr jParent
            return jParent'
    result <- gtk_mount_operation_new maybeParent
    checkUnexpectedReturnNULL "mountOperationNew" result
    result' <- (wrapObject MountOperation) result
    whenJust parent touchManagedPtr
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method MountOperation::get_parent
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "op"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MountOperation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMountOperation"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Window" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_mount_operation_get_parent" gtk_mount_operation_get_parent :: 
    Ptr MountOperation ->                   -- op : TInterface (Name {namespace = "Gtk", name = "MountOperation"})
    IO (Ptr Gtk.Window.Window)

-- | Gets the transient parent used by the t'GI.Gtk.Objects.MountOperation.MountOperation'
-- 
-- /Since: 2.14/
mountOperationGetParent ::
    (B.CallStack.HasCallStack, MonadIO m, IsMountOperation a) =>
    a
    -- ^ /@op@/: a t'GI.Gtk.Objects.MountOperation.MountOperation'
    -> m Gtk.Window.Window
    -- ^ __Returns:__ the transient parent for windows shown by /@op@/
mountOperationGetParent op = liftIO $ do
    op' <- unsafeManagedPtrCastPtr op
    result <- gtk_mount_operation_get_parent op'
    checkUnexpectedReturnNULL "mountOperationGetParent" result
    result' <- (newObject Gtk.Window.Window) result
    touchManagedPtr op
    return result'

#if defined(ENABLE_OVERLOADING)
data MountOperationGetParentMethodInfo
instance (signature ~ (m Gtk.Window.Window), MonadIO m, IsMountOperation a) => O.OverloadedMethod MountOperationGetParentMethodInfo a signature where
    overloadedMethod = mountOperationGetParent

instance O.OverloadedMethodInfo MountOperationGetParentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MountOperation.mountOperationGetParent",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MountOperation.html#v:mountOperationGetParent"
        })


#endif

-- method MountOperation::get_screen
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "op"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MountOperation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMountOperation"
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
-- returnType: Just (TInterface Name { namespace = "Gdk" , name = "Screen" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_mount_operation_get_screen" gtk_mount_operation_get_screen :: 
    Ptr MountOperation ->                   -- op : TInterface (Name {namespace = "Gtk", name = "MountOperation"})
    IO (Ptr Gdk.Screen.Screen)

-- | Gets the screen on which windows of the t'GI.Gtk.Objects.MountOperation.MountOperation'
-- will be shown.
-- 
-- /Since: 2.14/
mountOperationGetScreen ::
    (B.CallStack.HasCallStack, MonadIO m, IsMountOperation a) =>
    a
    -- ^ /@op@/: a t'GI.Gtk.Objects.MountOperation.MountOperation'
    -> m Gdk.Screen.Screen
    -- ^ __Returns:__ the screen on which windows of /@op@/ are shown
mountOperationGetScreen op = liftIO $ do
    op' <- unsafeManagedPtrCastPtr op
    result <- gtk_mount_operation_get_screen op'
    checkUnexpectedReturnNULL "mountOperationGetScreen" result
    result' <- (newObject Gdk.Screen.Screen) result
    touchManagedPtr op
    return result'

#if defined(ENABLE_OVERLOADING)
data MountOperationGetScreenMethodInfo
instance (signature ~ (m Gdk.Screen.Screen), MonadIO m, IsMountOperation a) => O.OverloadedMethod MountOperationGetScreenMethodInfo a signature where
    overloadedMethod = mountOperationGetScreen

instance O.OverloadedMethodInfo MountOperationGetScreenMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MountOperation.mountOperationGetScreen",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MountOperation.html#v:mountOperationGetScreen"
        })


#endif

-- method MountOperation::is_showing
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "op"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MountOperation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMountOperation"
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

foreign import ccall "gtk_mount_operation_is_showing" gtk_mount_operation_is_showing :: 
    Ptr MountOperation ->                   -- op : TInterface (Name {namespace = "Gtk", name = "MountOperation"})
    IO CInt

-- | Returns whether the t'GI.Gtk.Objects.MountOperation.MountOperation' is currently displaying
-- a window.
-- 
-- /Since: 2.14/
mountOperationIsShowing ::
    (B.CallStack.HasCallStack, MonadIO m, IsMountOperation a) =>
    a
    -- ^ /@op@/: a t'GI.Gtk.Objects.MountOperation.MountOperation'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@op@/ is currently displaying a window
mountOperationIsShowing op = liftIO $ do
    op' <- unsafeManagedPtrCastPtr op
    result <- gtk_mount_operation_is_showing op'
    let result' = (/= 0) result
    touchManagedPtr op
    return result'

#if defined(ENABLE_OVERLOADING)
data MountOperationIsShowingMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsMountOperation a) => O.OverloadedMethod MountOperationIsShowingMethodInfo a signature where
    overloadedMethod = mountOperationIsShowing

instance O.OverloadedMethodInfo MountOperationIsShowingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MountOperation.mountOperationIsShowing",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MountOperation.html#v:mountOperationIsShowing"
        })


#endif

-- method MountOperation::set_parent
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "op"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MountOperation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMountOperation"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "parent"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "transient parent of the window, or %NULL"
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

foreign import ccall "gtk_mount_operation_set_parent" gtk_mount_operation_set_parent :: 
    Ptr MountOperation ->                   -- op : TInterface (Name {namespace = "Gtk", name = "MountOperation"})
    Ptr Gtk.Window.Window ->                -- parent : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO ()

-- | Sets the transient parent for windows shown by the
-- t'GI.Gtk.Objects.MountOperation.MountOperation'.
-- 
-- /Since: 2.14/
mountOperationSetParent ::
    (B.CallStack.HasCallStack, MonadIO m, IsMountOperation a, Gtk.Window.IsWindow b) =>
    a
    -- ^ /@op@/: a t'GI.Gtk.Objects.MountOperation.MountOperation'
    -> Maybe (b)
    -- ^ /@parent@/: transient parent of the window, or 'P.Nothing'
    -> m ()
mountOperationSetParent op parent = liftIO $ do
    op' <- unsafeManagedPtrCastPtr op
    maybeParent <- case parent of
        Nothing -> return nullPtr
        Just jParent -> do
            jParent' <- unsafeManagedPtrCastPtr jParent
            return jParent'
    gtk_mount_operation_set_parent op' maybeParent
    touchManagedPtr op
    whenJust parent touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data MountOperationSetParentMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsMountOperation a, Gtk.Window.IsWindow b) => O.OverloadedMethod MountOperationSetParentMethodInfo a signature where
    overloadedMethod = mountOperationSetParent

instance O.OverloadedMethodInfo MountOperationSetParentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MountOperation.mountOperationSetParent",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MountOperation.html#v:mountOperationSetParent"
        })


#endif

-- method MountOperation::set_screen
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "op"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MountOperation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMountOperation"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "screen"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Screen" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkScreen" , sinceVersion = Nothing }
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

foreign import ccall "gtk_mount_operation_set_screen" gtk_mount_operation_set_screen :: 
    Ptr MountOperation ->                   -- op : TInterface (Name {namespace = "Gtk", name = "MountOperation"})
    Ptr Gdk.Screen.Screen ->                -- screen : TInterface (Name {namespace = "Gdk", name = "Screen"})
    IO ()

-- | Sets the screen to show windows of the t'GI.Gtk.Objects.MountOperation.MountOperation' on.
-- 
-- /Since: 2.14/
mountOperationSetScreen ::
    (B.CallStack.HasCallStack, MonadIO m, IsMountOperation a, Gdk.Screen.IsScreen b) =>
    a
    -- ^ /@op@/: a t'GI.Gtk.Objects.MountOperation.MountOperation'
    -> b
    -- ^ /@screen@/: a t'GI.Gdk.Objects.Screen.Screen'
    -> m ()
mountOperationSetScreen op screen = liftIO $ do
    op' <- unsafeManagedPtrCastPtr op
    screen' <- unsafeManagedPtrCastPtr screen
    gtk_mount_operation_set_screen op' screen'
    touchManagedPtr op
    touchManagedPtr screen
    return ()

#if defined(ENABLE_OVERLOADING)
data MountOperationSetScreenMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsMountOperation a, Gdk.Screen.IsScreen b) => O.OverloadedMethod MountOperationSetScreenMethodInfo a signature where
    overloadedMethod = mountOperationSetScreen

instance O.OverloadedMethodInfo MountOperationSetScreenMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MountOperation.mountOperationSetScreen",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MountOperation.html#v:mountOperationSetScreen"
        })


#endif


