{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- /No description available in the introspection data./

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.ContainerAccessible
    ( 

-- * Exported types
    ContainerAccessible(..)                 ,
    IsContainerAccessible                   ,
    toContainerAccessible                   ,


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
    ResolveContainerAccessibleMethod        ,
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
import qualified GI.Atk.Objects.Object as Atk.Object
import qualified GI.GObject.Objects.Object as GObject.Object
import {-# SOURCE #-} qualified GI.Gtk.Objects.Accessible as Gtk.Accessible
import {-# SOURCE #-} qualified GI.Gtk.Objects.WidgetAccessible as Gtk.WidgetAccessible

-- | Memory-managed wrapper type.
newtype ContainerAccessible = ContainerAccessible (SP.ManagedPtr ContainerAccessible)
    deriving (Eq)

instance SP.ManagedPtrNewtype ContainerAccessible where
    toManagedPtr (ContainerAccessible p) = p

foreign import ccall "gtk_container_accessible_get_type"
    c_gtk_container_accessible_get_type :: IO B.Types.GType

instance B.Types.TypedObject ContainerAccessible where
    glibType = c_gtk_container_accessible_get_type

instance B.Types.GObject ContainerAccessible

-- | Type class for types which can be safely cast to `ContainerAccessible`, for instance with `toContainerAccessible`.
class (SP.GObject o, O.IsDescendantOf ContainerAccessible o) => IsContainerAccessible o
instance (SP.GObject o, O.IsDescendantOf ContainerAccessible o) => IsContainerAccessible o

instance O.HasParentTypes ContainerAccessible
type instance O.ParentTypes ContainerAccessible = '[Gtk.WidgetAccessible.WidgetAccessible, Gtk.Accessible.Accessible, Atk.Object.Object, GObject.Object.Object, Atk.Component.Component]

-- | Cast to `ContainerAccessible`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toContainerAccessible :: (MIO.MonadIO m, IsContainerAccessible o) => o -> m ContainerAccessible
toContainerAccessible = MIO.liftIO . B.ManagedPtr.unsafeCastTo ContainerAccessible

-- | Convert 'ContainerAccessible' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe ContainerAccessible) where
    gvalueGType_ = c_gtk_container_accessible_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr ContainerAccessible)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr ContainerAccessible)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject ContainerAccessible ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveContainerAccessibleMethod (t :: Symbol) (o :: *) :: * where
    ResolveContainerAccessibleMethod "addRelationship" o = Atk.Object.ObjectAddRelationshipMethodInfo
    ResolveContainerAccessibleMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveContainerAccessibleMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveContainerAccessibleMethod "connectWidgetDestroyed" o = Gtk.Accessible.AccessibleConnectWidgetDestroyedMethodInfo
    ResolveContainerAccessibleMethod "contains" o = Atk.Component.ComponentContainsMethodInfo
    ResolveContainerAccessibleMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveContainerAccessibleMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveContainerAccessibleMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveContainerAccessibleMethod "grabFocus" o = Atk.Component.ComponentGrabFocusMethodInfo
    ResolveContainerAccessibleMethod "initialize" o = Atk.Object.ObjectInitializeMethodInfo
    ResolveContainerAccessibleMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveContainerAccessibleMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveContainerAccessibleMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveContainerAccessibleMethod "notifyStateChange" o = Atk.Object.ObjectNotifyStateChangeMethodInfo
    ResolveContainerAccessibleMethod "peekParent" o = Atk.Object.ObjectPeekParentMethodInfo
    ResolveContainerAccessibleMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveContainerAccessibleMethod "refAccessibleAtPoint" o = Atk.Component.ComponentRefAccessibleAtPointMethodInfo
    ResolveContainerAccessibleMethod "refAccessibleChild" o = Atk.Object.ObjectRefAccessibleChildMethodInfo
    ResolveContainerAccessibleMethod "refRelationSet" o = Atk.Object.ObjectRefRelationSetMethodInfo
    ResolveContainerAccessibleMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveContainerAccessibleMethod "refStateSet" o = Atk.Object.ObjectRefStateSetMethodInfo
    ResolveContainerAccessibleMethod "removeFocusHandler" o = Atk.Component.ComponentRemoveFocusHandlerMethodInfo
    ResolveContainerAccessibleMethod "removePropertyChangeHandler" o = Atk.Object.ObjectRemovePropertyChangeHandlerMethodInfo
    ResolveContainerAccessibleMethod "removeRelationship" o = Atk.Object.ObjectRemoveRelationshipMethodInfo
    ResolveContainerAccessibleMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveContainerAccessibleMethod "scrollTo" o = Atk.Component.ComponentScrollToMethodInfo
    ResolveContainerAccessibleMethod "scrollToPoint" o = Atk.Component.ComponentScrollToPointMethodInfo
    ResolveContainerAccessibleMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveContainerAccessibleMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveContainerAccessibleMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveContainerAccessibleMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveContainerAccessibleMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveContainerAccessibleMethod "getAccessibleId" o = Atk.Object.ObjectGetAccessibleIdMethodInfo
    ResolveContainerAccessibleMethod "getAlpha" o = Atk.Component.ComponentGetAlphaMethodInfo
    ResolveContainerAccessibleMethod "getAttributes" o = Atk.Object.ObjectGetAttributesMethodInfo
    ResolveContainerAccessibleMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveContainerAccessibleMethod "getDescription" o = Atk.Object.ObjectGetDescriptionMethodInfo
    ResolveContainerAccessibleMethod "getExtents" o = Atk.Component.ComponentGetExtentsMethodInfo
    ResolveContainerAccessibleMethod "getIndexInParent" o = Atk.Object.ObjectGetIndexInParentMethodInfo
    ResolveContainerAccessibleMethod "getLayer" o = Atk.Object.ObjectGetLayerMethodInfo
    ResolveContainerAccessibleMethod "getMdiZorder" o = Atk.Object.ObjectGetMdiZorderMethodInfo
    ResolveContainerAccessibleMethod "getNAccessibleChildren" o = Atk.Object.ObjectGetNAccessibleChildrenMethodInfo
    ResolveContainerAccessibleMethod "getName" o = Atk.Object.ObjectGetNameMethodInfo
    ResolveContainerAccessibleMethod "getObjectLocale" o = Atk.Object.ObjectGetObjectLocaleMethodInfo
    ResolveContainerAccessibleMethod "getParent" o = Atk.Object.ObjectGetParentMethodInfo
    ResolveContainerAccessibleMethod "getPosition" o = Atk.Component.ComponentGetPositionMethodInfo
    ResolveContainerAccessibleMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveContainerAccessibleMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveContainerAccessibleMethod "getRole" o = Atk.Object.ObjectGetRoleMethodInfo
    ResolveContainerAccessibleMethod "getSize" o = Atk.Component.ComponentGetSizeMethodInfo
    ResolveContainerAccessibleMethod "getWidget" o = Gtk.Accessible.AccessibleGetWidgetMethodInfo
    ResolveContainerAccessibleMethod "setAccessibleId" o = Atk.Object.ObjectSetAccessibleIdMethodInfo
    ResolveContainerAccessibleMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveContainerAccessibleMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveContainerAccessibleMethod "setDescription" o = Atk.Object.ObjectSetDescriptionMethodInfo
    ResolveContainerAccessibleMethod "setExtents" o = Atk.Component.ComponentSetExtentsMethodInfo
    ResolveContainerAccessibleMethod "setName" o = Atk.Object.ObjectSetNameMethodInfo
    ResolveContainerAccessibleMethod "setParent" o = Atk.Object.ObjectSetParentMethodInfo
    ResolveContainerAccessibleMethod "setPosition" o = Atk.Component.ComponentSetPositionMethodInfo
    ResolveContainerAccessibleMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveContainerAccessibleMethod "setRole" o = Atk.Object.ObjectSetRoleMethodInfo
    ResolveContainerAccessibleMethod "setSize" o = Atk.Component.ComponentSetSizeMethodInfo
    ResolveContainerAccessibleMethod "setWidget" o = Gtk.Accessible.AccessibleSetWidgetMethodInfo
    ResolveContainerAccessibleMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveContainerAccessibleMethod t ContainerAccessible, O.OverloadedMethod info ContainerAccessible p) => OL.IsLabel t (ContainerAccessible -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveContainerAccessibleMethod t ContainerAccessible, O.OverloadedMethod info ContainerAccessible p, R.HasField t ContainerAccessible p) => R.HasField t ContainerAccessible p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveContainerAccessibleMethod t ContainerAccessible, O.OverloadedMethodInfo info ContainerAccessible) => OL.IsLabel t (O.MethodProxy info ContainerAccessible) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList ContainerAccessible
type instance O.AttributeList ContainerAccessible = ContainerAccessibleAttributeList
type ContainerAccessibleAttributeList = ('[ '("accessibleComponentLayer", Atk.Object.ObjectAccessibleComponentLayerPropertyInfo), '("accessibleComponentMdiZorder", Atk.Object.ObjectAccessibleComponentMdiZorderPropertyInfo), '("accessibleDescription", Atk.Object.ObjectAccessibleDescriptionPropertyInfo), '("accessibleHypertextNlinks", Atk.Object.ObjectAccessibleHypertextNlinksPropertyInfo), '("accessibleName", Atk.Object.ObjectAccessibleNamePropertyInfo), '("accessibleParent", Atk.Object.ObjectAccessibleParentPropertyInfo), '("accessibleRole", Atk.Object.ObjectAccessibleRolePropertyInfo), '("accessibleTableCaption", Atk.Object.ObjectAccessibleTableCaptionPropertyInfo), '("accessibleTableCaptionObject", Atk.Object.ObjectAccessibleTableCaptionObjectPropertyInfo), '("accessibleTableColumnDescription", Atk.Object.ObjectAccessibleTableColumnDescriptionPropertyInfo), '("accessibleTableColumnHeader", Atk.Object.ObjectAccessibleTableColumnHeaderPropertyInfo), '("accessibleTableRowDescription", Atk.Object.ObjectAccessibleTableRowDescriptionPropertyInfo), '("accessibleTableRowHeader", Atk.Object.ObjectAccessibleTableRowHeaderPropertyInfo), '("accessibleTableSummary", Atk.Object.ObjectAccessibleTableSummaryPropertyInfo), '("accessibleValue", Atk.Object.ObjectAccessibleValuePropertyInfo), '("widget", Gtk.Accessible.AccessibleWidgetPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList ContainerAccessible = ContainerAccessibleSignalList
type ContainerAccessibleSignalList = ('[ '("activeDescendantChanged", Atk.Object.ObjectActiveDescendantChangedSignalInfo), '("announcement", Atk.Object.ObjectAnnouncementSignalInfo), '("boundsChanged", Atk.Component.ComponentBoundsChangedSignalInfo), '("childrenChanged", Atk.Object.ObjectChildrenChangedSignalInfo), '("focusEvent", Atk.Object.ObjectFocusEventSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("propertyChange", Atk.Object.ObjectPropertyChangeSignalInfo), '("stateChange", Atk.Object.ObjectStateChangeSignalInfo), '("visibleDataChanged", Atk.Object.ObjectVisibleDataChangedSignalInfo)] :: [(Symbol, *)])

#endif


