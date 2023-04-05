{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- /No description available in the introspection data./

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.LevelBarAccessible
    ( 

-- * Exported types
    LevelBarAccessible(..)                  ,
    IsLevelBarAccessible                    ,
    toLevelBarAccessible                    ,


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
    ResolveLevelBarAccessibleMethod         ,
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
import {-# SOURCE #-} qualified GI.Gtk.Objects.WidgetAccessible as Gtk.WidgetAccessible

-- | Memory-managed wrapper type.
newtype LevelBarAccessible = LevelBarAccessible (SP.ManagedPtr LevelBarAccessible)
    deriving (Eq)

instance SP.ManagedPtrNewtype LevelBarAccessible where
    toManagedPtr (LevelBarAccessible p) = p

foreign import ccall "gtk_level_bar_accessible_get_type"
    c_gtk_level_bar_accessible_get_type :: IO B.Types.GType

instance B.Types.TypedObject LevelBarAccessible where
    glibType = c_gtk_level_bar_accessible_get_type

instance B.Types.GObject LevelBarAccessible

-- | Type class for types which can be safely cast to `LevelBarAccessible`, for instance with `toLevelBarAccessible`.
class (SP.GObject o, O.IsDescendantOf LevelBarAccessible o) => IsLevelBarAccessible o
instance (SP.GObject o, O.IsDescendantOf LevelBarAccessible o) => IsLevelBarAccessible o

instance O.HasParentTypes LevelBarAccessible
type instance O.ParentTypes LevelBarAccessible = '[Gtk.WidgetAccessible.WidgetAccessible, Gtk.Accessible.Accessible, Atk.Object.Object, GObject.Object.Object, Atk.Component.Component, Atk.Value.Value]

-- | Cast to `LevelBarAccessible`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toLevelBarAccessible :: (MIO.MonadIO m, IsLevelBarAccessible o) => o -> m LevelBarAccessible
toLevelBarAccessible = MIO.liftIO . B.ManagedPtr.unsafeCastTo LevelBarAccessible

-- | Convert 'LevelBarAccessible' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe LevelBarAccessible) where
    gvalueGType_ = c_gtk_level_bar_accessible_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr LevelBarAccessible)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr LevelBarAccessible)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject LevelBarAccessible ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveLevelBarAccessibleMethod (t :: Symbol) (o :: *) :: * where
    ResolveLevelBarAccessibleMethod "addRelationship" o = Atk.Object.ObjectAddRelationshipMethodInfo
    ResolveLevelBarAccessibleMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveLevelBarAccessibleMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveLevelBarAccessibleMethod "connectWidgetDestroyed" o = Gtk.Accessible.AccessibleConnectWidgetDestroyedMethodInfo
    ResolveLevelBarAccessibleMethod "contains" o = Atk.Component.ComponentContainsMethodInfo
    ResolveLevelBarAccessibleMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveLevelBarAccessibleMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveLevelBarAccessibleMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveLevelBarAccessibleMethod "grabFocus" o = Atk.Component.ComponentGrabFocusMethodInfo
    ResolveLevelBarAccessibleMethod "initialize" o = Atk.Object.ObjectInitializeMethodInfo
    ResolveLevelBarAccessibleMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveLevelBarAccessibleMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveLevelBarAccessibleMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveLevelBarAccessibleMethod "notifyStateChange" o = Atk.Object.ObjectNotifyStateChangeMethodInfo
    ResolveLevelBarAccessibleMethod "peekParent" o = Atk.Object.ObjectPeekParentMethodInfo
    ResolveLevelBarAccessibleMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveLevelBarAccessibleMethod "refAccessibleAtPoint" o = Atk.Component.ComponentRefAccessibleAtPointMethodInfo
    ResolveLevelBarAccessibleMethod "refAccessibleChild" o = Atk.Object.ObjectRefAccessibleChildMethodInfo
    ResolveLevelBarAccessibleMethod "refRelationSet" o = Atk.Object.ObjectRefRelationSetMethodInfo
    ResolveLevelBarAccessibleMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveLevelBarAccessibleMethod "refStateSet" o = Atk.Object.ObjectRefStateSetMethodInfo
    ResolveLevelBarAccessibleMethod "removeFocusHandler" o = Atk.Component.ComponentRemoveFocusHandlerMethodInfo
    ResolveLevelBarAccessibleMethod "removePropertyChangeHandler" o = Atk.Object.ObjectRemovePropertyChangeHandlerMethodInfo
    ResolveLevelBarAccessibleMethod "removeRelationship" o = Atk.Object.ObjectRemoveRelationshipMethodInfo
    ResolveLevelBarAccessibleMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveLevelBarAccessibleMethod "scrollTo" o = Atk.Component.ComponentScrollToMethodInfo
    ResolveLevelBarAccessibleMethod "scrollToPoint" o = Atk.Component.ComponentScrollToPointMethodInfo
    ResolveLevelBarAccessibleMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveLevelBarAccessibleMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveLevelBarAccessibleMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveLevelBarAccessibleMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveLevelBarAccessibleMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveLevelBarAccessibleMethod "getAccessibleId" o = Atk.Object.ObjectGetAccessibleIdMethodInfo
    ResolveLevelBarAccessibleMethod "getAlpha" o = Atk.Component.ComponentGetAlphaMethodInfo
    ResolveLevelBarAccessibleMethod "getAttributes" o = Atk.Object.ObjectGetAttributesMethodInfo
    ResolveLevelBarAccessibleMethod "getCurrentValue" o = Atk.Value.ValueGetCurrentValueMethodInfo
    ResolveLevelBarAccessibleMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveLevelBarAccessibleMethod "getDescription" o = Atk.Object.ObjectGetDescriptionMethodInfo
    ResolveLevelBarAccessibleMethod "getExtents" o = Atk.Component.ComponentGetExtentsMethodInfo
    ResolveLevelBarAccessibleMethod "getIncrement" o = Atk.Value.ValueGetIncrementMethodInfo
    ResolveLevelBarAccessibleMethod "getIndexInParent" o = Atk.Object.ObjectGetIndexInParentMethodInfo
    ResolveLevelBarAccessibleMethod "getLayer" o = Atk.Object.ObjectGetLayerMethodInfo
    ResolveLevelBarAccessibleMethod "getMaximumValue" o = Atk.Value.ValueGetMaximumValueMethodInfo
    ResolveLevelBarAccessibleMethod "getMdiZorder" o = Atk.Object.ObjectGetMdiZorderMethodInfo
    ResolveLevelBarAccessibleMethod "getMinimumIncrement" o = Atk.Value.ValueGetMinimumIncrementMethodInfo
    ResolveLevelBarAccessibleMethod "getMinimumValue" o = Atk.Value.ValueGetMinimumValueMethodInfo
    ResolveLevelBarAccessibleMethod "getNAccessibleChildren" o = Atk.Object.ObjectGetNAccessibleChildrenMethodInfo
    ResolveLevelBarAccessibleMethod "getName" o = Atk.Object.ObjectGetNameMethodInfo
    ResolveLevelBarAccessibleMethod "getObjectLocale" o = Atk.Object.ObjectGetObjectLocaleMethodInfo
    ResolveLevelBarAccessibleMethod "getParent" o = Atk.Object.ObjectGetParentMethodInfo
    ResolveLevelBarAccessibleMethod "getPosition" o = Atk.Component.ComponentGetPositionMethodInfo
    ResolveLevelBarAccessibleMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveLevelBarAccessibleMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveLevelBarAccessibleMethod "getRange" o = Atk.Value.ValueGetRangeMethodInfo
    ResolveLevelBarAccessibleMethod "getRole" o = Atk.Object.ObjectGetRoleMethodInfo
    ResolveLevelBarAccessibleMethod "getSize" o = Atk.Component.ComponentGetSizeMethodInfo
    ResolveLevelBarAccessibleMethod "getSubRanges" o = Atk.Value.ValueGetSubRangesMethodInfo
    ResolveLevelBarAccessibleMethod "getValueAndText" o = Atk.Value.ValueGetValueAndTextMethodInfo
    ResolveLevelBarAccessibleMethod "getWidget" o = Gtk.Accessible.AccessibleGetWidgetMethodInfo
    ResolveLevelBarAccessibleMethod "setAccessibleId" o = Atk.Object.ObjectSetAccessibleIdMethodInfo
    ResolveLevelBarAccessibleMethod "setCurrentValue" o = Atk.Value.ValueSetCurrentValueMethodInfo
    ResolveLevelBarAccessibleMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveLevelBarAccessibleMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveLevelBarAccessibleMethod "setDescription" o = Atk.Object.ObjectSetDescriptionMethodInfo
    ResolveLevelBarAccessibleMethod "setExtents" o = Atk.Component.ComponentSetExtentsMethodInfo
    ResolveLevelBarAccessibleMethod "setName" o = Atk.Object.ObjectSetNameMethodInfo
    ResolveLevelBarAccessibleMethod "setParent" o = Atk.Object.ObjectSetParentMethodInfo
    ResolveLevelBarAccessibleMethod "setPosition" o = Atk.Component.ComponentSetPositionMethodInfo
    ResolveLevelBarAccessibleMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveLevelBarAccessibleMethod "setRole" o = Atk.Object.ObjectSetRoleMethodInfo
    ResolveLevelBarAccessibleMethod "setSize" o = Atk.Component.ComponentSetSizeMethodInfo
    ResolveLevelBarAccessibleMethod "setValue" o = Atk.Value.ValueSetValueMethodInfo
    ResolveLevelBarAccessibleMethod "setWidget" o = Gtk.Accessible.AccessibleSetWidgetMethodInfo
    ResolveLevelBarAccessibleMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveLevelBarAccessibleMethod t LevelBarAccessible, O.OverloadedMethod info LevelBarAccessible p) => OL.IsLabel t (LevelBarAccessible -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveLevelBarAccessibleMethod t LevelBarAccessible, O.OverloadedMethod info LevelBarAccessible p, R.HasField t LevelBarAccessible p) => R.HasField t LevelBarAccessible p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveLevelBarAccessibleMethod t LevelBarAccessible, O.OverloadedMethodInfo info LevelBarAccessible) => OL.IsLabel t (O.MethodProxy info LevelBarAccessible) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList LevelBarAccessible
type instance O.AttributeList LevelBarAccessible = LevelBarAccessibleAttributeList
type LevelBarAccessibleAttributeList = ('[ '("accessibleComponentLayer", Atk.Object.ObjectAccessibleComponentLayerPropertyInfo), '("accessibleComponentMdiZorder", Atk.Object.ObjectAccessibleComponentMdiZorderPropertyInfo), '("accessibleDescription", Atk.Object.ObjectAccessibleDescriptionPropertyInfo), '("accessibleHypertextNlinks", Atk.Object.ObjectAccessibleHypertextNlinksPropertyInfo), '("accessibleName", Atk.Object.ObjectAccessibleNamePropertyInfo), '("accessibleParent", Atk.Object.ObjectAccessibleParentPropertyInfo), '("accessibleRole", Atk.Object.ObjectAccessibleRolePropertyInfo), '("accessibleTableCaption", Atk.Object.ObjectAccessibleTableCaptionPropertyInfo), '("accessibleTableCaptionObject", Atk.Object.ObjectAccessibleTableCaptionObjectPropertyInfo), '("accessibleTableColumnDescription", Atk.Object.ObjectAccessibleTableColumnDescriptionPropertyInfo), '("accessibleTableColumnHeader", Atk.Object.ObjectAccessibleTableColumnHeaderPropertyInfo), '("accessibleTableRowDescription", Atk.Object.ObjectAccessibleTableRowDescriptionPropertyInfo), '("accessibleTableRowHeader", Atk.Object.ObjectAccessibleTableRowHeaderPropertyInfo), '("accessibleTableSummary", Atk.Object.ObjectAccessibleTableSummaryPropertyInfo), '("accessibleValue", Atk.Object.ObjectAccessibleValuePropertyInfo), '("widget", Gtk.Accessible.AccessibleWidgetPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList LevelBarAccessible = LevelBarAccessibleSignalList
type LevelBarAccessibleSignalList = ('[ '("activeDescendantChanged", Atk.Object.ObjectActiveDescendantChangedSignalInfo), '("announcement", Atk.Object.ObjectAnnouncementSignalInfo), '("boundsChanged", Atk.Component.ComponentBoundsChangedSignalInfo), '("childrenChanged", Atk.Object.ObjectChildrenChangedSignalInfo), '("focusEvent", Atk.Object.ObjectFocusEventSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("propertyChange", Atk.Object.ObjectPropertyChangeSignalInfo), '("stateChange", Atk.Object.ObjectStateChangeSignalInfo), '("valueChanged", Atk.Value.ValueValueChangedSignalInfo), '("visibleDataChanged", Atk.Object.ObjectVisibleDataChangedSignalInfo)] :: [(Symbol, *)])

#endif


