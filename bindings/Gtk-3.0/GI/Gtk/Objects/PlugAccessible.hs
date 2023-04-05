{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- /No description available in the introspection data./
-- 
-- /Since: 3.24.30/

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.PlugAccessible
    ( 

-- * Exported types
    PlugAccessible(..)                      ,
    IsPlugAccessible                        ,
    toPlugAccessible                        ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [addRelationship]("GI.Atk.Objects.Object#g:method:addRelationship"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [connectWidgetDestroyed]("GI.Gtk.Objects.Accessible#g:method:connectWidgetDestroyed"), [contains]("GI.Atk.Interfaces.Component#g:method:contains"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabFocus]("GI.Atk.Interfaces.Component#g:method:grabFocus"), [initialize]("GI.Atk.Objects.Object#g:method:initialize"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [notifyStateChange]("GI.Atk.Objects.Object#g:method:notifyStateChange"), [peekParent]("GI.Atk.Objects.Object#g:method:peekParent"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refAccessibleAtPoint]("GI.Atk.Interfaces.Component#g:method:refAccessibleAtPoint"), [refAccessibleChild]("GI.Atk.Objects.Object#g:method:refAccessibleChild"), [refRelationSet]("GI.Atk.Objects.Object#g:method:refRelationSet"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [refStateSet]("GI.Atk.Objects.Object#g:method:refStateSet"), [removeFocusHandler]("GI.Atk.Interfaces.Component#g:method:removeFocusHandler"), [removePropertyChangeHandler]("GI.Atk.Objects.Object#g:method:removePropertyChangeHandler"), [removeRelationship]("GI.Atk.Objects.Object#g:method:removeRelationship"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [scrollTo]("GI.Atk.Interfaces.Component#g:method:scrollTo"), [scrollToPoint]("GI.Atk.Interfaces.Component#g:method:scrollToPoint"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessibleId]("GI.Atk.Objects.Object#g:method:getAccessibleId"), [getAlpha]("GI.Atk.Interfaces.Component#g:method:getAlpha"), [getAttributes]("GI.Atk.Objects.Object#g:method:getAttributes"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDescription]("GI.Atk.Objects.Object#g:method:getDescription"), [getExtents]("GI.Atk.Interfaces.Component#g:method:getExtents"), [getId]("GI.Gtk.Objects.PlugAccessible#g:method:getId"), [getIndexInParent]("GI.Atk.Objects.Object#g:method:getIndexInParent"), [getLayer]("GI.Atk.Objects.Object#g:method:getLayer"), [getMdiZorder]("GI.Atk.Objects.Object#g:method:getMdiZorder"), [getNAccessibleChildren]("GI.Atk.Objects.Object#g:method:getNAccessibleChildren"), [getName]("GI.Atk.Objects.Object#g:method:getName"), [getObjectLocale]("GI.Atk.Objects.Object#g:method:getObjectLocale"), [getParent]("GI.Atk.Objects.Object#g:method:getParent"), [getPosition]("GI.Atk.Interfaces.Component#g:method:getPosition"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRole]("GI.Atk.Objects.Object#g:method:getRole"), [getSize]("GI.Atk.Interfaces.Component#g:method:getSize"), [getWidget]("GI.Gtk.Objects.Accessible#g:method:getWidget").
-- 
-- ==== Setters
-- [setAccessibleId]("GI.Atk.Objects.Object#g:method:setAccessibleId"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDescription]("GI.Atk.Objects.Object#g:method:setDescription"), [setExtents]("GI.Atk.Interfaces.Component#g:method:setExtents"), [setName]("GI.Atk.Objects.Object#g:method:setName"), [setParent]("GI.Atk.Objects.Object#g:method:setParent"), [setPosition]("GI.Atk.Interfaces.Component#g:method:setPosition"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRole]("GI.Atk.Objects.Object#g:method:setRole"), [setSize]("GI.Atk.Interfaces.Component#g:method:setSize"), [setWidget]("GI.Gtk.Objects.Accessible#g:method:setWidget").

#if defined(ENABLE_OVERLOADING)
    ResolvePlugAccessibleMethod             ,
#endif

-- ** getId #method:getId#

#if defined(ENABLE_OVERLOADING)
    PlugAccessibleGetIdMethodInfo           ,
#endif
    plugAccessibleGetId                     ,




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

import qualified GI.Atk.Interfaces.Component as Atk.Component
import qualified GI.Atk.Interfaces.Window as Atk.Window
import qualified GI.Atk.Objects.Object as Atk.Object
import qualified GI.GObject.Objects.Object as GObject.Object
import {-# SOURCE #-} qualified GI.Gtk.Objects.Accessible as Gtk.Accessible
import {-# SOURCE #-} qualified GI.Gtk.Objects.ContainerAccessible as Gtk.ContainerAccessible
import {-# SOURCE #-} qualified GI.Gtk.Objects.WidgetAccessible as Gtk.WidgetAccessible
import {-# SOURCE #-} qualified GI.Gtk.Objects.WindowAccessible as Gtk.WindowAccessible

-- | Memory-managed wrapper type.
newtype PlugAccessible = PlugAccessible (SP.ManagedPtr PlugAccessible)
    deriving (Eq)

instance SP.ManagedPtrNewtype PlugAccessible where
    toManagedPtr (PlugAccessible p) = p

foreign import ccall "gtk_plug_accessible_get_type"
    c_gtk_plug_accessible_get_type :: IO B.Types.GType

instance B.Types.TypedObject PlugAccessible where
    glibType = c_gtk_plug_accessible_get_type

instance B.Types.GObject PlugAccessible

-- | Type class for types which can be safely cast to `PlugAccessible`, for instance with `toPlugAccessible`.
class (SP.GObject o, O.IsDescendantOf PlugAccessible o) => IsPlugAccessible o
instance (SP.GObject o, O.IsDescendantOf PlugAccessible o) => IsPlugAccessible o

instance O.HasParentTypes PlugAccessible
type instance O.ParentTypes PlugAccessible = '[Gtk.WindowAccessible.WindowAccessible, Gtk.ContainerAccessible.ContainerAccessible, Gtk.WidgetAccessible.WidgetAccessible, Gtk.Accessible.Accessible, Atk.Object.Object, GObject.Object.Object, Atk.Component.Component, Atk.Window.Window]

-- | Cast to `PlugAccessible`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toPlugAccessible :: (MIO.MonadIO m, IsPlugAccessible o) => o -> m PlugAccessible
toPlugAccessible = MIO.liftIO . B.ManagedPtr.unsafeCastTo PlugAccessible

-- | Convert 'PlugAccessible' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe PlugAccessible) where
    gvalueGType_ = c_gtk_plug_accessible_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr PlugAccessible)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr PlugAccessible)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject PlugAccessible ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolvePlugAccessibleMethod (t :: Symbol) (o :: *) :: * where
    ResolvePlugAccessibleMethod "addRelationship" o = Atk.Object.ObjectAddRelationshipMethodInfo
    ResolvePlugAccessibleMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolvePlugAccessibleMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolvePlugAccessibleMethod "connectWidgetDestroyed" o = Gtk.Accessible.AccessibleConnectWidgetDestroyedMethodInfo
    ResolvePlugAccessibleMethod "contains" o = Atk.Component.ComponentContainsMethodInfo
    ResolvePlugAccessibleMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolvePlugAccessibleMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolvePlugAccessibleMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolvePlugAccessibleMethod "grabFocus" o = Atk.Component.ComponentGrabFocusMethodInfo
    ResolvePlugAccessibleMethod "initialize" o = Atk.Object.ObjectInitializeMethodInfo
    ResolvePlugAccessibleMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolvePlugAccessibleMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolvePlugAccessibleMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolvePlugAccessibleMethod "notifyStateChange" o = Atk.Object.ObjectNotifyStateChangeMethodInfo
    ResolvePlugAccessibleMethod "peekParent" o = Atk.Object.ObjectPeekParentMethodInfo
    ResolvePlugAccessibleMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolvePlugAccessibleMethod "refAccessibleAtPoint" o = Atk.Component.ComponentRefAccessibleAtPointMethodInfo
    ResolvePlugAccessibleMethod "refAccessibleChild" o = Atk.Object.ObjectRefAccessibleChildMethodInfo
    ResolvePlugAccessibleMethod "refRelationSet" o = Atk.Object.ObjectRefRelationSetMethodInfo
    ResolvePlugAccessibleMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolvePlugAccessibleMethod "refStateSet" o = Atk.Object.ObjectRefStateSetMethodInfo
    ResolvePlugAccessibleMethod "removeFocusHandler" o = Atk.Component.ComponentRemoveFocusHandlerMethodInfo
    ResolvePlugAccessibleMethod "removePropertyChangeHandler" o = Atk.Object.ObjectRemovePropertyChangeHandlerMethodInfo
    ResolvePlugAccessibleMethod "removeRelationship" o = Atk.Object.ObjectRemoveRelationshipMethodInfo
    ResolvePlugAccessibleMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolvePlugAccessibleMethod "scrollTo" o = Atk.Component.ComponentScrollToMethodInfo
    ResolvePlugAccessibleMethod "scrollToPoint" o = Atk.Component.ComponentScrollToPointMethodInfo
    ResolvePlugAccessibleMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolvePlugAccessibleMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolvePlugAccessibleMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolvePlugAccessibleMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolvePlugAccessibleMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolvePlugAccessibleMethod "getAccessibleId" o = Atk.Object.ObjectGetAccessibleIdMethodInfo
    ResolvePlugAccessibleMethod "getAlpha" o = Atk.Component.ComponentGetAlphaMethodInfo
    ResolvePlugAccessibleMethod "getAttributes" o = Atk.Object.ObjectGetAttributesMethodInfo
    ResolvePlugAccessibleMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolvePlugAccessibleMethod "getDescription" o = Atk.Object.ObjectGetDescriptionMethodInfo
    ResolvePlugAccessibleMethod "getExtents" o = Atk.Component.ComponentGetExtentsMethodInfo
    ResolvePlugAccessibleMethod "getId" o = PlugAccessibleGetIdMethodInfo
    ResolvePlugAccessibleMethod "getIndexInParent" o = Atk.Object.ObjectGetIndexInParentMethodInfo
    ResolvePlugAccessibleMethod "getLayer" o = Atk.Object.ObjectGetLayerMethodInfo
    ResolvePlugAccessibleMethod "getMdiZorder" o = Atk.Object.ObjectGetMdiZorderMethodInfo
    ResolvePlugAccessibleMethod "getNAccessibleChildren" o = Atk.Object.ObjectGetNAccessibleChildrenMethodInfo
    ResolvePlugAccessibleMethod "getName" o = Atk.Object.ObjectGetNameMethodInfo
    ResolvePlugAccessibleMethod "getObjectLocale" o = Atk.Object.ObjectGetObjectLocaleMethodInfo
    ResolvePlugAccessibleMethod "getParent" o = Atk.Object.ObjectGetParentMethodInfo
    ResolvePlugAccessibleMethod "getPosition" o = Atk.Component.ComponentGetPositionMethodInfo
    ResolvePlugAccessibleMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolvePlugAccessibleMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolvePlugAccessibleMethod "getRole" o = Atk.Object.ObjectGetRoleMethodInfo
    ResolvePlugAccessibleMethod "getSize" o = Atk.Component.ComponentGetSizeMethodInfo
    ResolvePlugAccessibleMethod "getWidget" o = Gtk.Accessible.AccessibleGetWidgetMethodInfo
    ResolvePlugAccessibleMethod "setAccessibleId" o = Atk.Object.ObjectSetAccessibleIdMethodInfo
    ResolvePlugAccessibleMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolvePlugAccessibleMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolvePlugAccessibleMethod "setDescription" o = Atk.Object.ObjectSetDescriptionMethodInfo
    ResolvePlugAccessibleMethod "setExtents" o = Atk.Component.ComponentSetExtentsMethodInfo
    ResolvePlugAccessibleMethod "setName" o = Atk.Object.ObjectSetNameMethodInfo
    ResolvePlugAccessibleMethod "setParent" o = Atk.Object.ObjectSetParentMethodInfo
    ResolvePlugAccessibleMethod "setPosition" o = Atk.Component.ComponentSetPositionMethodInfo
    ResolvePlugAccessibleMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolvePlugAccessibleMethod "setRole" o = Atk.Object.ObjectSetRoleMethodInfo
    ResolvePlugAccessibleMethod "setSize" o = Atk.Component.ComponentSetSizeMethodInfo
    ResolvePlugAccessibleMethod "setWidget" o = Gtk.Accessible.AccessibleSetWidgetMethodInfo
    ResolvePlugAccessibleMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolvePlugAccessibleMethod t PlugAccessible, O.OverloadedMethod info PlugAccessible p) => OL.IsLabel t (PlugAccessible -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolvePlugAccessibleMethod t PlugAccessible, O.OverloadedMethod info PlugAccessible p, R.HasField t PlugAccessible p) => R.HasField t PlugAccessible p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolvePlugAccessibleMethod t PlugAccessible, O.OverloadedMethodInfo info PlugAccessible) => OL.IsLabel t (O.MethodProxy info PlugAccessible) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList PlugAccessible
type instance O.AttributeList PlugAccessible = PlugAccessibleAttributeList
type PlugAccessibleAttributeList = ('[ '("accessibleComponentLayer", Atk.Object.ObjectAccessibleComponentLayerPropertyInfo), '("accessibleComponentMdiZorder", Atk.Object.ObjectAccessibleComponentMdiZorderPropertyInfo), '("accessibleDescription", Atk.Object.ObjectAccessibleDescriptionPropertyInfo), '("accessibleHypertextNlinks", Atk.Object.ObjectAccessibleHypertextNlinksPropertyInfo), '("accessibleName", Atk.Object.ObjectAccessibleNamePropertyInfo), '("accessibleParent", Atk.Object.ObjectAccessibleParentPropertyInfo), '("accessibleRole", Atk.Object.ObjectAccessibleRolePropertyInfo), '("accessibleTableCaption", Atk.Object.ObjectAccessibleTableCaptionPropertyInfo), '("accessibleTableCaptionObject", Atk.Object.ObjectAccessibleTableCaptionObjectPropertyInfo), '("accessibleTableColumnDescription", Atk.Object.ObjectAccessibleTableColumnDescriptionPropertyInfo), '("accessibleTableColumnHeader", Atk.Object.ObjectAccessibleTableColumnHeaderPropertyInfo), '("accessibleTableRowDescription", Atk.Object.ObjectAccessibleTableRowDescriptionPropertyInfo), '("accessibleTableRowHeader", Atk.Object.ObjectAccessibleTableRowHeaderPropertyInfo), '("accessibleTableSummary", Atk.Object.ObjectAccessibleTableSummaryPropertyInfo), '("accessibleValue", Atk.Object.ObjectAccessibleValuePropertyInfo), '("widget", Gtk.Accessible.AccessibleWidgetPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList PlugAccessible = PlugAccessibleSignalList
type PlugAccessibleSignalList = ('[ '("activate", Atk.Window.WindowActivateSignalInfo), '("activeDescendantChanged", Atk.Object.ObjectActiveDescendantChangedSignalInfo), '("announcement", Atk.Object.ObjectAnnouncementSignalInfo), '("boundsChanged", Atk.Component.ComponentBoundsChangedSignalInfo), '("childrenChanged", Atk.Object.ObjectChildrenChangedSignalInfo), '("create", Atk.Window.WindowCreateSignalInfo), '("deactivate", Atk.Window.WindowDeactivateSignalInfo), '("destroy", Atk.Window.WindowDestroySignalInfo), '("focusEvent", Atk.Object.ObjectFocusEventSignalInfo), '("maximize", Atk.Window.WindowMaximizeSignalInfo), '("minimize", Atk.Window.WindowMinimizeSignalInfo), '("move", Atk.Window.WindowMoveSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("propertyChange", Atk.Object.ObjectPropertyChangeSignalInfo), '("resize", Atk.Window.WindowResizeSignalInfo), '("restore", Atk.Window.WindowRestoreSignalInfo), '("stateChange", Atk.Object.ObjectStateChangeSignalInfo), '("visibleDataChanged", Atk.Object.ObjectVisibleDataChangedSignalInfo)] :: [(Symbol, *)])

#endif

-- method PlugAccessible::get_id
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "plug"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PlugAccessible" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation { rawDocText = Nothing , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TUTF8)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_plug_accessible_get_id" gtk_plug_accessible_get_id :: 
    Ptr PlugAccessible ->                   -- plug : TInterface (Name {namespace = "Gtk", name = "PlugAccessible"})
    IO CString

-- | /No description available in the introspection data./
plugAccessibleGetId ::
    (B.CallStack.HasCallStack, MonadIO m, IsPlugAccessible a) =>
    a
    -> m T.Text
plugAccessibleGetId plug = liftIO $ do
    plug' <- unsafeManagedPtrCastPtr plug
    result <- gtk_plug_accessible_get_id plug'
    checkUnexpectedReturnNULL "plugAccessibleGetId" result
    result' <- cstringToText result
    freeMem result
    touchManagedPtr plug
    return result'

#if defined(ENABLE_OVERLOADING)
data PlugAccessibleGetIdMethodInfo
instance (signature ~ (m T.Text), MonadIO m, IsPlugAccessible a) => O.OverloadedMethod PlugAccessibleGetIdMethodInfo a signature where
    overloadedMethod = plugAccessibleGetId

instance O.OverloadedMethodInfo PlugAccessibleGetIdMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PlugAccessible.plugAccessibleGetId",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PlugAccessible.html#v:plugAccessibleGetId"
        })


#endif


