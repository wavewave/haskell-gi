{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- /No description available in the introspection data./

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.PanedAccessible
    ( 

-- * Exported types
    PanedAccessible(..)                     ,
    IsPanedAccessible                       ,
    toPanedAccessible                       ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [addRelationship]("GI.Atk.Objects.Object#g:method:addRelationship"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [connectWidgetDestroyed]("GI.Gtk.Objects.Accessible#g:method:connectWidgetDestroyed"), [contains]("GI.Atk.Interfaces.Component#g:method:contains"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabFocus]("GI.Atk.Interfaces.Component#g:method:grabFocus"), [initialize]("GI.Atk.Objects.Object#g:method:initialize"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [notifyStateChange]("GI.Atk.Objects.Object#g:method:notifyStateChange"), [peekParent]("GI.Atk.Objects.Object#g:method:peekParent"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refAccessibleAtPoint]("GI.Atk.Interfaces.Component#g:method:refAccessibleAtPoint"), [refAccessibleChild]("GI.Atk.Objects.Object#g:method:refAccessibleChild"), [refRelationSet]("GI.Atk.Objects.Object#g:method:refRelationSet"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [refStateSet]("GI.Atk.Objects.Object#g:method:refStateSet"), [removeFocusHandler]("GI.Atk.Interfaces.Component#g:method:removeFocusHandler"), [removePropertyChangeHandler]("GI.Atk.Objects.Object#g:method:removePropertyChangeHandler"), [removeRelationship]("GI.Atk.Objects.Object#g:method:removeRelationship"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [scrollTo]("GI.Atk.Interfaces.Component#g:method:scrollTo"), [scrollToPoint]("GI.Atk.Interfaces.Component#g:method:scrollToPoint"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessibleId]("GI.Atk.Objects.Object#g:method:getAccessibleId"), [getAlpha]("GI.Atk.Interfaces.Component#g:method:getAlpha"), [getAttributes]("GI.Atk.Objects.Object#g:method:getAttributes"), [getCurrentValue]("GI.Atk.Interfaces.Value#g:method:getCurrentValue"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDescription]("GI.Atk.Objects.Object#g:method:getDescription"), [getExtents]("GI.Atk.Interfaces.Component#g:method:getExtents"), [getIncrement]("GI.Atk.Interfaces.Value#g:method:getIncrement"), [getIndexInParent]("GI.Atk.Objects.Object#g:method:getIndexInParent"), [getLayer]("GI.Atk.Objects.Object#g:method:getLayer"), [getMaximumValue]("GI.Atk.Interfaces.Value#g:method:getMaximumValue"), [getMdiZorder]("GI.Atk.Objects.Object#g:method:getMdiZorder"), [getMinimumIncrement]("GI.Atk.Interfaces.Value#g:method:getMinimumIncrement"), [getMinimumValue]("GI.Atk.Interfaces.Value#g:method:getMinimumValue"), [getNAccessibleChildren]("GI.Atk.Objects.Object#g:method:getNAccessibleChildren"), [getName]("GI.Atk.Objects.Object#g:method:getName"), [getObjectLocale]("GI.Atk.Objects.Object#g:method:getObjectLocale"), [getParent]("GI.Atk.Objects.Object#g:method:getParent"), [getPosition]("GI.Atk.Interfaces.Component#g:method:getPosition"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRange]("GI.Atk.Interfaces.Value#g:method:getRange"), [getRole]("GI.Atk.Objects.Object#g:method:getRole"), [getSize]("GI.Atk.Interfaces.Component#g:method:getSize"), [getSubRanges]("GI.Atk.Interfaces.Value#g:method:getSubRanges"), [getValueAndText]("GI.Atk.Interfaces.Value#g:method:getValueAndText"), [getWidget]("GI.Gtk.Objects.Accessible#g:method:getWidget").
-- 
-- ==== Setters
-- [setAccessibleId]("GI.Atk.Objects.Object#g:method:setAccessibleId"), [setCurrentValue]("GI.Atk.Interfaces.Value#g:method:setCurrentValue"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDescription]("GI.Atk.Objects.Object#g:method:setDescription"), [setExtents]("GI.Atk.Interfaces.Component#g:method:setExtents"), [setName]("GI.Atk.Objects.Object#g:method:setName"), [setParent]("GI.Atk.Objects.Object#g:method:setParent"), [setPosition]("GI.Atk.Interfaces.Component#g:method:setPosition"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRole]("GI.Atk.Objects.Object#g:method:setRole"), [setSize]("GI.Atk.Interfaces.Component#g:method:setSize"), [setValue]("GI.Atk.Interfaces.Value#g:method:setValue"), [setWidget]("GI.Gtk.Objects.Accessible#g:method:setWidget").

#if defined(ENABLE_OVERLOADING)
    ResolvePanedAccessibleMethod            ,
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
import qualified GI.Atk.Interfaces.Value as Atk.Value
import qualified GI.Atk.Objects.Object as Atk.Object
import qualified GI.GObject.Objects.Object as GObject.Object
import {-# SOURCE #-} qualified GI.Gtk.Objects.Accessible as Gtk.Accessible
import {-# SOURCE #-} qualified GI.Gtk.Objects.ContainerAccessible as Gtk.ContainerAccessible
import {-# SOURCE #-} qualified GI.Gtk.Objects.WidgetAccessible as Gtk.WidgetAccessible

-- | Memory-managed wrapper type.
newtype PanedAccessible = PanedAccessible (SP.ManagedPtr PanedAccessible)
    deriving (Eq)

instance SP.ManagedPtrNewtype PanedAccessible where
    toManagedPtr (PanedAccessible p) = p

foreign import ccall "gtk_paned_accessible_get_type"
    c_gtk_paned_accessible_get_type :: IO B.Types.GType

instance B.Types.TypedObject PanedAccessible where
    glibType = c_gtk_paned_accessible_get_type

instance B.Types.GObject PanedAccessible

-- | Type class for types which can be safely cast to `PanedAccessible`, for instance with `toPanedAccessible`.
class (SP.GObject o, O.IsDescendantOf PanedAccessible o) => IsPanedAccessible o
instance (SP.GObject o, O.IsDescendantOf PanedAccessible o) => IsPanedAccessible o

instance O.HasParentTypes PanedAccessible
type instance O.ParentTypes PanedAccessible = '[Gtk.ContainerAccessible.ContainerAccessible, Gtk.WidgetAccessible.WidgetAccessible, Gtk.Accessible.Accessible, Atk.Object.Object, GObject.Object.Object, Atk.Component.Component, Atk.Value.Value]

-- | Cast to `PanedAccessible`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toPanedAccessible :: (MIO.MonadIO m, IsPanedAccessible o) => o -> m PanedAccessible
toPanedAccessible = MIO.liftIO . B.ManagedPtr.unsafeCastTo PanedAccessible

-- | Convert 'PanedAccessible' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe PanedAccessible) where
    gvalueGType_ = c_gtk_paned_accessible_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr PanedAccessible)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr PanedAccessible)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject PanedAccessible ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolvePanedAccessibleMethod (t :: Symbol) (o :: *) :: * where
    ResolvePanedAccessibleMethod "addRelationship" o = Atk.Object.ObjectAddRelationshipMethodInfo
    ResolvePanedAccessibleMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolvePanedAccessibleMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolvePanedAccessibleMethod "connectWidgetDestroyed" o = Gtk.Accessible.AccessibleConnectWidgetDestroyedMethodInfo
    ResolvePanedAccessibleMethod "contains" o = Atk.Component.ComponentContainsMethodInfo
    ResolvePanedAccessibleMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolvePanedAccessibleMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolvePanedAccessibleMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolvePanedAccessibleMethod "grabFocus" o = Atk.Component.ComponentGrabFocusMethodInfo
    ResolvePanedAccessibleMethod "initialize" o = Atk.Object.ObjectInitializeMethodInfo
    ResolvePanedAccessibleMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolvePanedAccessibleMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolvePanedAccessibleMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolvePanedAccessibleMethod "notifyStateChange" o = Atk.Object.ObjectNotifyStateChangeMethodInfo
    ResolvePanedAccessibleMethod "peekParent" o = Atk.Object.ObjectPeekParentMethodInfo
    ResolvePanedAccessibleMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolvePanedAccessibleMethod "refAccessibleAtPoint" o = Atk.Component.ComponentRefAccessibleAtPointMethodInfo
    ResolvePanedAccessibleMethod "refAccessibleChild" o = Atk.Object.ObjectRefAccessibleChildMethodInfo
    ResolvePanedAccessibleMethod "refRelationSet" o = Atk.Object.ObjectRefRelationSetMethodInfo
    ResolvePanedAccessibleMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolvePanedAccessibleMethod "refStateSet" o = Atk.Object.ObjectRefStateSetMethodInfo
    ResolvePanedAccessibleMethod "removeFocusHandler" o = Atk.Component.ComponentRemoveFocusHandlerMethodInfo
    ResolvePanedAccessibleMethod "removePropertyChangeHandler" o = Atk.Object.ObjectRemovePropertyChangeHandlerMethodInfo
    ResolvePanedAccessibleMethod "removeRelationship" o = Atk.Object.ObjectRemoveRelationshipMethodInfo
    ResolvePanedAccessibleMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolvePanedAccessibleMethod "scrollTo" o = Atk.Component.ComponentScrollToMethodInfo
    ResolvePanedAccessibleMethod "scrollToPoint" o = Atk.Component.ComponentScrollToPointMethodInfo
    ResolvePanedAccessibleMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolvePanedAccessibleMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolvePanedAccessibleMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolvePanedAccessibleMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolvePanedAccessibleMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolvePanedAccessibleMethod "getAccessibleId" o = Atk.Object.ObjectGetAccessibleIdMethodInfo
    ResolvePanedAccessibleMethod "getAlpha" o = Atk.Component.ComponentGetAlphaMethodInfo
    ResolvePanedAccessibleMethod "getAttributes" o = Atk.Object.ObjectGetAttributesMethodInfo
    ResolvePanedAccessibleMethod "getCurrentValue" o = Atk.Value.ValueGetCurrentValueMethodInfo
    ResolvePanedAccessibleMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolvePanedAccessibleMethod "getDescription" o = Atk.Object.ObjectGetDescriptionMethodInfo
    ResolvePanedAccessibleMethod "getExtents" o = Atk.Component.ComponentGetExtentsMethodInfo
    ResolvePanedAccessibleMethod "getIncrement" o = Atk.Value.ValueGetIncrementMethodInfo
    ResolvePanedAccessibleMethod "getIndexInParent" o = Atk.Object.ObjectGetIndexInParentMethodInfo
    ResolvePanedAccessibleMethod "getLayer" o = Atk.Object.ObjectGetLayerMethodInfo
    ResolvePanedAccessibleMethod "getMaximumValue" o = Atk.Value.ValueGetMaximumValueMethodInfo
    ResolvePanedAccessibleMethod "getMdiZorder" o = Atk.Object.ObjectGetMdiZorderMethodInfo
    ResolvePanedAccessibleMethod "getMinimumIncrement" o = Atk.Value.ValueGetMinimumIncrementMethodInfo
    ResolvePanedAccessibleMethod "getMinimumValue" o = Atk.Value.ValueGetMinimumValueMethodInfo
    ResolvePanedAccessibleMethod "getNAccessibleChildren" o = Atk.Object.ObjectGetNAccessibleChildrenMethodInfo
    ResolvePanedAccessibleMethod "getName" o = Atk.Object.ObjectGetNameMethodInfo
    ResolvePanedAccessibleMethod "getObjectLocale" o = Atk.Object.ObjectGetObjectLocaleMethodInfo
    ResolvePanedAccessibleMethod "getParent" o = Atk.Object.ObjectGetParentMethodInfo
    ResolvePanedAccessibleMethod "getPosition" o = Atk.Component.ComponentGetPositionMethodInfo
    ResolvePanedAccessibleMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolvePanedAccessibleMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolvePanedAccessibleMethod "getRange" o = Atk.Value.ValueGetRangeMethodInfo
    ResolvePanedAccessibleMethod "getRole" o = Atk.Object.ObjectGetRoleMethodInfo
    ResolvePanedAccessibleMethod "getSize" o = Atk.Component.ComponentGetSizeMethodInfo
    ResolvePanedAccessibleMethod "getSubRanges" o = Atk.Value.ValueGetSubRangesMethodInfo
    ResolvePanedAccessibleMethod "getValueAndText" o = Atk.Value.ValueGetValueAndTextMethodInfo
    ResolvePanedAccessibleMethod "getWidget" o = Gtk.Accessible.AccessibleGetWidgetMethodInfo
    ResolvePanedAccessibleMethod "setAccessibleId" o = Atk.Object.ObjectSetAccessibleIdMethodInfo
    ResolvePanedAccessibleMethod "setCurrentValue" o = Atk.Value.ValueSetCurrentValueMethodInfo
    ResolvePanedAccessibleMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolvePanedAccessibleMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolvePanedAccessibleMethod "setDescription" o = Atk.Object.ObjectSetDescriptionMethodInfo
    ResolvePanedAccessibleMethod "setExtents" o = Atk.Component.ComponentSetExtentsMethodInfo
    ResolvePanedAccessibleMethod "setName" o = Atk.Object.ObjectSetNameMethodInfo
    ResolvePanedAccessibleMethod "setParent" o = Atk.Object.ObjectSetParentMethodInfo
    ResolvePanedAccessibleMethod "setPosition" o = Atk.Component.ComponentSetPositionMethodInfo
    ResolvePanedAccessibleMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolvePanedAccessibleMethod "setRole" o = Atk.Object.ObjectSetRoleMethodInfo
    ResolvePanedAccessibleMethod "setSize" o = Atk.Component.ComponentSetSizeMethodInfo
    ResolvePanedAccessibleMethod "setValue" o = Atk.Value.ValueSetValueMethodInfo
    ResolvePanedAccessibleMethod "setWidget" o = Gtk.Accessible.AccessibleSetWidgetMethodInfo
    ResolvePanedAccessibleMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolvePanedAccessibleMethod t PanedAccessible, O.OverloadedMethod info PanedAccessible p) => OL.IsLabel t (PanedAccessible -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolvePanedAccessibleMethod t PanedAccessible, O.OverloadedMethod info PanedAccessible p, R.HasField t PanedAccessible p) => R.HasField t PanedAccessible p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolvePanedAccessibleMethod t PanedAccessible, O.OverloadedMethodInfo info PanedAccessible) => OL.IsLabel t (O.MethodProxy info PanedAccessible) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList PanedAccessible
type instance O.AttributeList PanedAccessible = PanedAccessibleAttributeList
type PanedAccessibleAttributeList = ('[ '("accessibleComponentLayer", Atk.Object.ObjectAccessibleComponentLayerPropertyInfo), '("accessibleComponentMdiZorder", Atk.Object.ObjectAccessibleComponentMdiZorderPropertyInfo), '("accessibleDescription", Atk.Object.ObjectAccessibleDescriptionPropertyInfo), '("accessibleHypertextNlinks", Atk.Object.ObjectAccessibleHypertextNlinksPropertyInfo), '("accessibleName", Atk.Object.ObjectAccessibleNamePropertyInfo), '("accessibleParent", Atk.Object.ObjectAccessibleParentPropertyInfo), '("accessibleRole", Atk.Object.ObjectAccessibleRolePropertyInfo), '("accessibleTableCaption", Atk.Object.ObjectAccessibleTableCaptionPropertyInfo), '("accessibleTableCaptionObject", Atk.Object.ObjectAccessibleTableCaptionObjectPropertyInfo), '("accessibleTableColumnDescription", Atk.Object.ObjectAccessibleTableColumnDescriptionPropertyInfo), '("accessibleTableColumnHeader", Atk.Object.ObjectAccessibleTableColumnHeaderPropertyInfo), '("accessibleTableRowDescription", Atk.Object.ObjectAccessibleTableRowDescriptionPropertyInfo), '("accessibleTableRowHeader", Atk.Object.ObjectAccessibleTableRowHeaderPropertyInfo), '("accessibleTableSummary", Atk.Object.ObjectAccessibleTableSummaryPropertyInfo), '("accessibleValue", Atk.Object.ObjectAccessibleValuePropertyInfo), '("widget", Gtk.Accessible.AccessibleWidgetPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList PanedAccessible = PanedAccessibleSignalList
type PanedAccessibleSignalList = ('[ '("activeDescendantChanged", Atk.Object.ObjectActiveDescendantChangedSignalInfo), '("announcement", Atk.Object.ObjectAnnouncementSignalInfo), '("boundsChanged", Atk.Component.ComponentBoundsChangedSignalInfo), '("childrenChanged", Atk.Object.ObjectChildrenChangedSignalInfo), '("focusEvent", Atk.Object.ObjectFocusEventSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("propertyChange", Atk.Object.ObjectPropertyChangeSignalInfo), '("stateChange", Atk.Object.ObjectStateChangeSignalInfo), '("valueChanged", Atk.Value.ValueValueChangedSignalInfo), '("visibleDataChanged", Atk.Object.ObjectVisibleDataChangedSignalInfo)] :: [(Symbol, *)])

#endif


