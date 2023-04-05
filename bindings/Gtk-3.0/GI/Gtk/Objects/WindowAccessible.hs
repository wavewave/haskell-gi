{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- /No description available in the introspection data./

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.WindowAccessible
    ( 

-- * Exported types
    WindowAccessible(..)                    ,
    IsWindowAccessible                      ,
    toWindowAccessible                      ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [addRelationship]("GI.Atk.Objects.Object#g:method:addRelationship"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [connectWidgetDestroyed]("GI.Gtk.Objects.Accessible#g:method:connectWidgetDestroyed"), [contains]("GI.Atk.Interfaces.Component#g:method:contains"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabFocus]("GI.Atk.Interfaces.Component#g:method:grabFocus"), [initialize]("GI.Atk.Objects.Object#g:method:initialize"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [notifyStateChange]("GI.Atk.Objects.Object#g:method:notifyStateChange"), [peekParent]("GI.Atk.Objects.Object#g:method:peekParent"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refAccessibleAtPoint]("GI.Atk.Interfaces.Component#g:method:refAccessibleAtPoint"), [refAccessibleChild]("GI.Atk.Objects.Object#g:method:refAccessibleChild"), [refRelationSet]("GI.Atk.Objects.Object#g:method:refRelationSet"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [refStateSet]("GI.Atk.Objects.Object#g:method:refStateSet"), [removeFocusHandler]("GI.Atk.Interfaces.Component#g:method:removeFocusHandler"), [removePropertyChangeHandler]("GI.Atk.Objects.Object#g:method:removePropertyChangeHandler"), [removeRelationship]("GI.Atk.Objects.Object#g:method:removeRelationship"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [scrollTo]("GI.Atk.Interfaces.Component#g:method:scrollTo"), [scrollToPoint]("GI.Atk.Interfaces.Component#g:method:scrollToPoint"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessibleId]("GI.Atk.Objects.Object#g:method:getAccessibleId"), [getAlpha]("GI.Atk.Interfaces.Component#g:method:getAlpha"), [getAttributes]("GI.Atk.Objects.Object#g:method:getAttributes"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDescription]("GI.Atk.Objects.Object#g:method:getDescription"), [getExtents]("GI.Atk.Interfaces.Component#g:method:getExtents"), [getIndexInParent]("GI.Atk.Objects.Object#g:method:getIndexInParent"), [getLayer]("GI.Atk.Objects.Object#g:method:getLayer"), [getMdiZorder]("GI.Atk.Objects.Object#g:method:getMdiZorder"), [getNAccessibleChildren]("GI.Atk.Objects.Object#g:method:getNAccessibleChildren"), [getName]("GI.Atk.Objects.Object#g:method:getName"), [getObjectLocale]("GI.Atk.Objects.Object#g:method:getObjectLocale"), [getParent]("GI.Atk.Objects.Object#g:method:getParent"), [getPosition]("GI.Atk.Interfaces.Component#g:method:getPosition"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRole]("GI.Atk.Objects.Object#g:method:getRole"), [getSize]("GI.Atk.Interfaces.Component#g:method:getSize"), [getWidget]("GI.Gtk.Objects.Accessible#g:method:getWidget").
-- 
-- ==== Setters
-- [setAccessibleId]("GI.Atk.Objects.Object#g:method:setAccessibleId"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDescription]("GI.Atk.Objects.Object#g:method:setDescription"), [setExtents]("GI.Atk.Interfaces.Component#g:method:setExtents"), [setName]("GI.Atk.Objects.Object#g:method:setName"), [setParent]("GI.Atk.Objects.Object#g:method:setParent"), [setPosition]("GI.Atk.Interfaces.Component#g:method:setPosition"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRole]("GI.Atk.Objects.Object#g:method:setRole"), [setSize]("GI.Atk.Interfaces.Component#g:method:setSize"), [setWidget]("GI.Gtk.Objects.Accessible#g:method:setWidget").

#if defined(ENABLE_OVERLOADING)
    ResolveWindowAccessibleMethod           ,
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

import qualified GI.Atk.Interfaces.Component as Atk.Component
import qualified GI.Atk.Interfaces.Window as Atk.Window
import qualified GI.Atk.Objects.Object as Atk.Object
import qualified GI.GObject.Objects.Object as GObject.Object
import {-# SOURCE #-} qualified GI.Gtk.Objects.Accessible as Gtk.Accessible
import {-# SOURCE #-} qualified GI.Gtk.Objects.ContainerAccessible as Gtk.ContainerAccessible
import {-# SOURCE #-} qualified GI.Gtk.Objects.WidgetAccessible as Gtk.WidgetAccessible

-- | Memory-managed wrapper type.
newtype WindowAccessible = WindowAccessible (SP.ManagedPtr WindowAccessible)
    deriving (Eq)

instance SP.ManagedPtrNewtype WindowAccessible where
    toManagedPtr (WindowAccessible p) = p

foreign import ccall "gtk_window_accessible_get_type"
    c_gtk_window_accessible_get_type :: IO B.Types.GType

instance B.Types.TypedObject WindowAccessible where
    glibType = c_gtk_window_accessible_get_type

instance B.Types.GObject WindowAccessible

-- | Type class for types which can be safely cast to `WindowAccessible`, for instance with `toWindowAccessible`.
class (SP.GObject o, O.IsDescendantOf WindowAccessible o) => IsWindowAccessible o
instance (SP.GObject o, O.IsDescendantOf WindowAccessible o) => IsWindowAccessible o

instance O.HasParentTypes WindowAccessible
type instance O.ParentTypes WindowAccessible = '[Gtk.ContainerAccessible.ContainerAccessible, Gtk.WidgetAccessible.WidgetAccessible, Gtk.Accessible.Accessible, Atk.Object.Object, GObject.Object.Object, Atk.Component.Component, Atk.Window.Window]

-- | Cast to `WindowAccessible`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toWindowAccessible :: (MIO.MonadIO m, IsWindowAccessible o) => o -> m WindowAccessible
toWindowAccessible = MIO.liftIO . B.ManagedPtr.unsafeCastTo WindowAccessible

-- | Convert 'WindowAccessible' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe WindowAccessible) where
    gvalueGType_ = c_gtk_window_accessible_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr WindowAccessible)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr WindowAccessible)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject WindowAccessible ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveWindowAccessibleMethod (t :: Symbol) (o :: *) :: * where
    ResolveWindowAccessibleMethod "addRelationship" o = Atk.Object.ObjectAddRelationshipMethodInfo
    ResolveWindowAccessibleMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveWindowAccessibleMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveWindowAccessibleMethod "connectWidgetDestroyed" o = Gtk.Accessible.AccessibleConnectWidgetDestroyedMethodInfo
    ResolveWindowAccessibleMethod "contains" o = Atk.Component.ComponentContainsMethodInfo
    ResolveWindowAccessibleMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveWindowAccessibleMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveWindowAccessibleMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveWindowAccessibleMethod "grabFocus" o = Atk.Component.ComponentGrabFocusMethodInfo
    ResolveWindowAccessibleMethod "initialize" o = Atk.Object.ObjectInitializeMethodInfo
    ResolveWindowAccessibleMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveWindowAccessibleMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveWindowAccessibleMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveWindowAccessibleMethod "notifyStateChange" o = Atk.Object.ObjectNotifyStateChangeMethodInfo
    ResolveWindowAccessibleMethod "peekParent" o = Atk.Object.ObjectPeekParentMethodInfo
    ResolveWindowAccessibleMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveWindowAccessibleMethod "refAccessibleAtPoint" o = Atk.Component.ComponentRefAccessibleAtPointMethodInfo
    ResolveWindowAccessibleMethod "refAccessibleChild" o = Atk.Object.ObjectRefAccessibleChildMethodInfo
    ResolveWindowAccessibleMethod "refRelationSet" o = Atk.Object.ObjectRefRelationSetMethodInfo
    ResolveWindowAccessibleMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveWindowAccessibleMethod "refStateSet" o = Atk.Object.ObjectRefStateSetMethodInfo
    ResolveWindowAccessibleMethod "removeFocusHandler" o = Atk.Component.ComponentRemoveFocusHandlerMethodInfo
    ResolveWindowAccessibleMethod "removePropertyChangeHandler" o = Atk.Object.ObjectRemovePropertyChangeHandlerMethodInfo
    ResolveWindowAccessibleMethod "removeRelationship" o = Atk.Object.ObjectRemoveRelationshipMethodInfo
    ResolveWindowAccessibleMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveWindowAccessibleMethod "scrollTo" o = Atk.Component.ComponentScrollToMethodInfo
    ResolveWindowAccessibleMethod "scrollToPoint" o = Atk.Component.ComponentScrollToPointMethodInfo
    ResolveWindowAccessibleMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveWindowAccessibleMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveWindowAccessibleMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveWindowAccessibleMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveWindowAccessibleMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveWindowAccessibleMethod "getAccessibleId" o = Atk.Object.ObjectGetAccessibleIdMethodInfo
    ResolveWindowAccessibleMethod "getAlpha" o = Atk.Component.ComponentGetAlphaMethodInfo
    ResolveWindowAccessibleMethod "getAttributes" o = Atk.Object.ObjectGetAttributesMethodInfo
    ResolveWindowAccessibleMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveWindowAccessibleMethod "getDescription" o = Atk.Object.ObjectGetDescriptionMethodInfo
    ResolveWindowAccessibleMethod "getExtents" o = Atk.Component.ComponentGetExtentsMethodInfo
    ResolveWindowAccessibleMethod "getIndexInParent" o = Atk.Object.ObjectGetIndexInParentMethodInfo
    ResolveWindowAccessibleMethod "getLayer" o = Atk.Object.ObjectGetLayerMethodInfo
    ResolveWindowAccessibleMethod "getMdiZorder" o = Atk.Object.ObjectGetMdiZorderMethodInfo
    ResolveWindowAccessibleMethod "getNAccessibleChildren" o = Atk.Object.ObjectGetNAccessibleChildrenMethodInfo
    ResolveWindowAccessibleMethod "getName" o = Atk.Object.ObjectGetNameMethodInfo
    ResolveWindowAccessibleMethod "getObjectLocale" o = Atk.Object.ObjectGetObjectLocaleMethodInfo
    ResolveWindowAccessibleMethod "getParent" o = Atk.Object.ObjectGetParentMethodInfo
    ResolveWindowAccessibleMethod "getPosition" o = Atk.Component.ComponentGetPositionMethodInfo
    ResolveWindowAccessibleMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveWindowAccessibleMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveWindowAccessibleMethod "getRole" o = Atk.Object.ObjectGetRoleMethodInfo
    ResolveWindowAccessibleMethod "getSize" o = Atk.Component.ComponentGetSizeMethodInfo
    ResolveWindowAccessibleMethod "getWidget" o = Gtk.Accessible.AccessibleGetWidgetMethodInfo
    ResolveWindowAccessibleMethod "setAccessibleId" o = Atk.Object.ObjectSetAccessibleIdMethodInfo
    ResolveWindowAccessibleMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveWindowAccessibleMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveWindowAccessibleMethod "setDescription" o = Atk.Object.ObjectSetDescriptionMethodInfo
    ResolveWindowAccessibleMethod "setExtents" o = Atk.Component.ComponentSetExtentsMethodInfo
    ResolveWindowAccessibleMethod "setName" o = Atk.Object.ObjectSetNameMethodInfo
    ResolveWindowAccessibleMethod "setParent" o = Atk.Object.ObjectSetParentMethodInfo
    ResolveWindowAccessibleMethod "setPosition" o = Atk.Component.ComponentSetPositionMethodInfo
    ResolveWindowAccessibleMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveWindowAccessibleMethod "setRole" o = Atk.Object.ObjectSetRoleMethodInfo
    ResolveWindowAccessibleMethod "setSize" o = Atk.Component.ComponentSetSizeMethodInfo
    ResolveWindowAccessibleMethod "setWidget" o = Gtk.Accessible.AccessibleSetWidgetMethodInfo
    ResolveWindowAccessibleMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveWindowAccessibleMethod t WindowAccessible, O.OverloadedMethod info WindowAccessible p) => OL.IsLabel t (WindowAccessible -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveWindowAccessibleMethod t WindowAccessible, O.OverloadedMethod info WindowAccessible p, R.HasField t WindowAccessible p) => R.HasField t WindowAccessible p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveWindowAccessibleMethod t WindowAccessible, O.OverloadedMethodInfo info WindowAccessible) => OL.IsLabel t (O.MethodProxy info WindowAccessible) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList WindowAccessible
type instance O.AttributeList WindowAccessible = WindowAccessibleAttributeList
type WindowAccessibleAttributeList = ('[ '("accessibleComponentLayer", Atk.Object.ObjectAccessibleComponentLayerPropertyInfo), '("accessibleComponentMdiZorder", Atk.Object.ObjectAccessibleComponentMdiZorderPropertyInfo), '("accessibleDescription", Atk.Object.ObjectAccessibleDescriptionPropertyInfo), '("accessibleHypertextNlinks", Atk.Object.ObjectAccessibleHypertextNlinksPropertyInfo), '("accessibleName", Atk.Object.ObjectAccessibleNamePropertyInfo), '("accessibleParent", Atk.Object.ObjectAccessibleParentPropertyInfo), '("accessibleRole", Atk.Object.ObjectAccessibleRolePropertyInfo), '("accessibleTableCaption", Atk.Object.ObjectAccessibleTableCaptionPropertyInfo), '("accessibleTableCaptionObject", Atk.Object.ObjectAccessibleTableCaptionObjectPropertyInfo), '("accessibleTableColumnDescription", Atk.Object.ObjectAccessibleTableColumnDescriptionPropertyInfo), '("accessibleTableColumnHeader", Atk.Object.ObjectAccessibleTableColumnHeaderPropertyInfo), '("accessibleTableRowDescription", Atk.Object.ObjectAccessibleTableRowDescriptionPropertyInfo), '("accessibleTableRowHeader", Atk.Object.ObjectAccessibleTableRowHeaderPropertyInfo), '("accessibleTableSummary", Atk.Object.ObjectAccessibleTableSummaryPropertyInfo), '("accessibleValue", Atk.Object.ObjectAccessibleValuePropertyInfo), '("widget", Gtk.Accessible.AccessibleWidgetPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList WindowAccessible = WindowAccessibleSignalList
type WindowAccessibleSignalList = ('[ '("activate", Atk.Window.WindowActivateSignalInfo), '("activeDescendantChanged", Atk.Object.ObjectActiveDescendantChangedSignalInfo), '("announcement", Atk.Object.ObjectAnnouncementSignalInfo), '("boundsChanged", Atk.Component.ComponentBoundsChangedSignalInfo), '("childrenChanged", Atk.Object.ObjectChildrenChangedSignalInfo), '("create", Atk.Window.WindowCreateSignalInfo), '("deactivate", Atk.Window.WindowDeactivateSignalInfo), '("destroy", Atk.Window.WindowDestroySignalInfo), '("focusEvent", Atk.Object.ObjectFocusEventSignalInfo), '("maximize", Atk.Window.WindowMaximizeSignalInfo), '("minimize", Atk.Window.WindowMinimizeSignalInfo), '("move", Atk.Window.WindowMoveSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("propertyChange", Atk.Object.ObjectPropertyChangeSignalInfo), '("resize", Atk.Window.WindowResizeSignalInfo), '("restore", Atk.Window.WindowRestoreSignalInfo), '("stateChange", Atk.Object.ObjectStateChangeSignalInfo), '("visibleDataChanged", Atk.Object.ObjectVisibleDataChangedSignalInfo)] :: [(Symbol, *)])

#endif


