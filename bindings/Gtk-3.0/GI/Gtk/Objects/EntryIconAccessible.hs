{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- /No description available in the introspection data./

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.EntryIconAccessible
    ( 

-- * Exported types
    EntryIconAccessible(..)                 ,
    IsEntryIconAccessible                   ,
    toEntryIconAccessible                   ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [addRelationship]("GI.Atk.Objects.Object#g:method:addRelationship"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [contains]("GI.Atk.Interfaces.Component#g:method:contains"), [doAction]("GI.Atk.Interfaces.Action#g:method:doAction"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabFocus]("GI.Atk.Interfaces.Component#g:method:grabFocus"), [initialize]("GI.Atk.Objects.Object#g:method:initialize"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [notifyStateChange]("GI.Atk.Objects.Object#g:method:notifyStateChange"), [peekParent]("GI.Atk.Objects.Object#g:method:peekParent"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refAccessibleAtPoint]("GI.Atk.Interfaces.Component#g:method:refAccessibleAtPoint"), [refAccessibleChild]("GI.Atk.Objects.Object#g:method:refAccessibleChild"), [refRelationSet]("GI.Atk.Objects.Object#g:method:refRelationSet"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [refStateSet]("GI.Atk.Objects.Object#g:method:refStateSet"), [removeFocusHandler]("GI.Atk.Interfaces.Component#g:method:removeFocusHandler"), [removePropertyChangeHandler]("GI.Atk.Objects.Object#g:method:removePropertyChangeHandler"), [removeRelationship]("GI.Atk.Objects.Object#g:method:removeRelationship"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [scrollTo]("GI.Atk.Interfaces.Component#g:method:scrollTo"), [scrollToPoint]("GI.Atk.Interfaces.Component#g:method:scrollToPoint"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessibleId]("GI.Atk.Objects.Object#g:method:getAccessibleId"), [getAlpha]("GI.Atk.Interfaces.Component#g:method:getAlpha"), [getAttributes]("GI.Atk.Objects.Object#g:method:getAttributes"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDescription]("GI.Atk.Objects.Object#g:method:getDescription"), [getExtents]("GI.Atk.Interfaces.Component#g:method:getExtents"), [getIndexInParent]("GI.Atk.Objects.Object#g:method:getIndexInParent"), [getKeybinding]("GI.Atk.Interfaces.Action#g:method:getKeybinding"), [getLayer]("GI.Atk.Objects.Object#g:method:getLayer"), [getLocalizedName]("GI.Atk.Interfaces.Action#g:method:getLocalizedName"), [getMdiZorder]("GI.Atk.Objects.Object#g:method:getMdiZorder"), [getNAccessibleChildren]("GI.Atk.Objects.Object#g:method:getNAccessibleChildren"), [getNActions]("GI.Atk.Interfaces.Action#g:method:getNActions"), [getName]("GI.Atk.Objects.Object#g:method:getName"), [getObjectLocale]("GI.Atk.Objects.Object#g:method:getObjectLocale"), [getParent]("GI.Atk.Objects.Object#g:method:getParent"), [getPosition]("GI.Atk.Interfaces.Component#g:method:getPosition"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRole]("GI.Atk.Objects.Object#g:method:getRole"), [getSize]("GI.Atk.Interfaces.Component#g:method:getSize").
-- 
-- ==== Setters
-- [setAccessibleId]("GI.Atk.Objects.Object#g:method:setAccessibleId"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDescription]("GI.Atk.Objects.Object#g:method:setDescription"), [setExtents]("GI.Atk.Interfaces.Component#g:method:setExtents"), [setName]("GI.Atk.Objects.Object#g:method:setName"), [setParent]("GI.Atk.Objects.Object#g:method:setParent"), [setPosition]("GI.Atk.Interfaces.Component#g:method:setPosition"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRole]("GI.Atk.Objects.Object#g:method:setRole"), [setSize]("GI.Atk.Interfaces.Component#g:method:setSize").

#if defined(ENABLE_OVERLOADING)
    ResolveEntryIconAccessibleMethod        ,
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

import qualified GI.Atk.Interfaces.Action as Atk.Action
import qualified GI.Atk.Interfaces.Component as Atk.Component
import qualified GI.Atk.Objects.Object as Atk.Object
import qualified GI.GObject.Objects.Object as GObject.Object

-- | Memory-managed wrapper type.
newtype EntryIconAccessible = EntryIconAccessible (SP.ManagedPtr EntryIconAccessible)
    deriving (Eq)

instance SP.ManagedPtrNewtype EntryIconAccessible where
    toManagedPtr (EntryIconAccessible p) = p

foreign import ccall "gtk_entry_icon_accessible_get_type"
    c_gtk_entry_icon_accessible_get_type :: IO B.Types.GType

instance B.Types.TypedObject EntryIconAccessible where
    glibType = c_gtk_entry_icon_accessible_get_type

instance B.Types.GObject EntryIconAccessible

-- | Type class for types which can be safely cast to `EntryIconAccessible`, for instance with `toEntryIconAccessible`.
class (SP.GObject o, O.IsDescendantOf EntryIconAccessible o) => IsEntryIconAccessible o
instance (SP.GObject o, O.IsDescendantOf EntryIconAccessible o) => IsEntryIconAccessible o

instance O.HasParentTypes EntryIconAccessible
type instance O.ParentTypes EntryIconAccessible = '[Atk.Object.Object, GObject.Object.Object, Atk.Action.Action, Atk.Component.Component]

-- | Cast to `EntryIconAccessible`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toEntryIconAccessible :: (MIO.MonadIO m, IsEntryIconAccessible o) => o -> m EntryIconAccessible
toEntryIconAccessible = MIO.liftIO . B.ManagedPtr.unsafeCastTo EntryIconAccessible

-- | Convert 'EntryIconAccessible' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe EntryIconAccessible) where
    gvalueGType_ = c_gtk_entry_icon_accessible_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr EntryIconAccessible)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr EntryIconAccessible)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject EntryIconAccessible ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveEntryIconAccessibleMethod (t :: Symbol) (o :: *) :: * where
    ResolveEntryIconAccessibleMethod "addRelationship" o = Atk.Object.ObjectAddRelationshipMethodInfo
    ResolveEntryIconAccessibleMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveEntryIconAccessibleMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveEntryIconAccessibleMethod "contains" o = Atk.Component.ComponentContainsMethodInfo
    ResolveEntryIconAccessibleMethod "doAction" o = Atk.Action.ActionDoActionMethodInfo
    ResolveEntryIconAccessibleMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveEntryIconAccessibleMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveEntryIconAccessibleMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveEntryIconAccessibleMethod "grabFocus" o = Atk.Component.ComponentGrabFocusMethodInfo
    ResolveEntryIconAccessibleMethod "initialize" o = Atk.Object.ObjectInitializeMethodInfo
    ResolveEntryIconAccessibleMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveEntryIconAccessibleMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveEntryIconAccessibleMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveEntryIconAccessibleMethod "notifyStateChange" o = Atk.Object.ObjectNotifyStateChangeMethodInfo
    ResolveEntryIconAccessibleMethod "peekParent" o = Atk.Object.ObjectPeekParentMethodInfo
    ResolveEntryIconAccessibleMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveEntryIconAccessibleMethod "refAccessibleAtPoint" o = Atk.Component.ComponentRefAccessibleAtPointMethodInfo
    ResolveEntryIconAccessibleMethod "refAccessibleChild" o = Atk.Object.ObjectRefAccessibleChildMethodInfo
    ResolveEntryIconAccessibleMethod "refRelationSet" o = Atk.Object.ObjectRefRelationSetMethodInfo
    ResolveEntryIconAccessibleMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveEntryIconAccessibleMethod "refStateSet" o = Atk.Object.ObjectRefStateSetMethodInfo
    ResolveEntryIconAccessibleMethod "removeFocusHandler" o = Atk.Component.ComponentRemoveFocusHandlerMethodInfo
    ResolveEntryIconAccessibleMethod "removePropertyChangeHandler" o = Atk.Object.ObjectRemovePropertyChangeHandlerMethodInfo
    ResolveEntryIconAccessibleMethod "removeRelationship" o = Atk.Object.ObjectRemoveRelationshipMethodInfo
    ResolveEntryIconAccessibleMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveEntryIconAccessibleMethod "scrollTo" o = Atk.Component.ComponentScrollToMethodInfo
    ResolveEntryIconAccessibleMethod "scrollToPoint" o = Atk.Component.ComponentScrollToPointMethodInfo
    ResolveEntryIconAccessibleMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveEntryIconAccessibleMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveEntryIconAccessibleMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveEntryIconAccessibleMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveEntryIconAccessibleMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveEntryIconAccessibleMethod "getAccessibleId" o = Atk.Object.ObjectGetAccessibleIdMethodInfo
    ResolveEntryIconAccessibleMethod "getAlpha" o = Atk.Component.ComponentGetAlphaMethodInfo
    ResolveEntryIconAccessibleMethod "getAttributes" o = Atk.Object.ObjectGetAttributesMethodInfo
    ResolveEntryIconAccessibleMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveEntryIconAccessibleMethod "getDescription" o = Atk.Object.ObjectGetDescriptionMethodInfo
    ResolveEntryIconAccessibleMethod "getExtents" o = Atk.Component.ComponentGetExtentsMethodInfo
    ResolveEntryIconAccessibleMethod "getIndexInParent" o = Atk.Object.ObjectGetIndexInParentMethodInfo
    ResolveEntryIconAccessibleMethod "getKeybinding" o = Atk.Action.ActionGetKeybindingMethodInfo
    ResolveEntryIconAccessibleMethod "getLayer" o = Atk.Object.ObjectGetLayerMethodInfo
    ResolveEntryIconAccessibleMethod "getLocalizedName" o = Atk.Action.ActionGetLocalizedNameMethodInfo
    ResolveEntryIconAccessibleMethod "getMdiZorder" o = Atk.Object.ObjectGetMdiZorderMethodInfo
    ResolveEntryIconAccessibleMethod "getNAccessibleChildren" o = Atk.Object.ObjectGetNAccessibleChildrenMethodInfo
    ResolveEntryIconAccessibleMethod "getNActions" o = Atk.Action.ActionGetNActionsMethodInfo
    ResolveEntryIconAccessibleMethod "getName" o = Atk.Object.ObjectGetNameMethodInfo
    ResolveEntryIconAccessibleMethod "getObjectLocale" o = Atk.Object.ObjectGetObjectLocaleMethodInfo
    ResolveEntryIconAccessibleMethod "getParent" o = Atk.Object.ObjectGetParentMethodInfo
    ResolveEntryIconAccessibleMethod "getPosition" o = Atk.Component.ComponentGetPositionMethodInfo
    ResolveEntryIconAccessibleMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveEntryIconAccessibleMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveEntryIconAccessibleMethod "getRole" o = Atk.Object.ObjectGetRoleMethodInfo
    ResolveEntryIconAccessibleMethod "getSize" o = Atk.Component.ComponentGetSizeMethodInfo
    ResolveEntryIconAccessibleMethod "setAccessibleId" o = Atk.Object.ObjectSetAccessibleIdMethodInfo
    ResolveEntryIconAccessibleMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveEntryIconAccessibleMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveEntryIconAccessibleMethod "setDescription" o = Atk.Object.ObjectSetDescriptionMethodInfo
    ResolveEntryIconAccessibleMethod "setExtents" o = Atk.Component.ComponentSetExtentsMethodInfo
    ResolveEntryIconAccessibleMethod "setName" o = Atk.Object.ObjectSetNameMethodInfo
    ResolveEntryIconAccessibleMethod "setParent" o = Atk.Object.ObjectSetParentMethodInfo
    ResolveEntryIconAccessibleMethod "setPosition" o = Atk.Component.ComponentSetPositionMethodInfo
    ResolveEntryIconAccessibleMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveEntryIconAccessibleMethod "setRole" o = Atk.Object.ObjectSetRoleMethodInfo
    ResolveEntryIconAccessibleMethod "setSize" o = Atk.Component.ComponentSetSizeMethodInfo
    ResolveEntryIconAccessibleMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveEntryIconAccessibleMethod t EntryIconAccessible, O.OverloadedMethod info EntryIconAccessible p) => OL.IsLabel t (EntryIconAccessible -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveEntryIconAccessibleMethod t EntryIconAccessible, O.OverloadedMethod info EntryIconAccessible p, R.HasField t EntryIconAccessible p) => R.HasField t EntryIconAccessible p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveEntryIconAccessibleMethod t EntryIconAccessible, O.OverloadedMethodInfo info EntryIconAccessible) => OL.IsLabel t (O.MethodProxy info EntryIconAccessible) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList EntryIconAccessible
type instance O.AttributeList EntryIconAccessible = EntryIconAccessibleAttributeList
type EntryIconAccessibleAttributeList = ('[ '("accessibleComponentLayer", Atk.Object.ObjectAccessibleComponentLayerPropertyInfo), '("accessibleComponentMdiZorder", Atk.Object.ObjectAccessibleComponentMdiZorderPropertyInfo), '("accessibleDescription", Atk.Object.ObjectAccessibleDescriptionPropertyInfo), '("accessibleHypertextNlinks", Atk.Object.ObjectAccessibleHypertextNlinksPropertyInfo), '("accessibleName", Atk.Object.ObjectAccessibleNamePropertyInfo), '("accessibleParent", Atk.Object.ObjectAccessibleParentPropertyInfo), '("accessibleRole", Atk.Object.ObjectAccessibleRolePropertyInfo), '("accessibleTableCaption", Atk.Object.ObjectAccessibleTableCaptionPropertyInfo), '("accessibleTableCaptionObject", Atk.Object.ObjectAccessibleTableCaptionObjectPropertyInfo), '("accessibleTableColumnDescription", Atk.Object.ObjectAccessibleTableColumnDescriptionPropertyInfo), '("accessibleTableColumnHeader", Atk.Object.ObjectAccessibleTableColumnHeaderPropertyInfo), '("accessibleTableRowDescription", Atk.Object.ObjectAccessibleTableRowDescriptionPropertyInfo), '("accessibleTableRowHeader", Atk.Object.ObjectAccessibleTableRowHeaderPropertyInfo), '("accessibleTableSummary", Atk.Object.ObjectAccessibleTableSummaryPropertyInfo), '("accessibleValue", Atk.Object.ObjectAccessibleValuePropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList EntryIconAccessible = EntryIconAccessibleSignalList
type EntryIconAccessibleSignalList = ('[ '("activeDescendantChanged", Atk.Object.ObjectActiveDescendantChangedSignalInfo), '("announcement", Atk.Object.ObjectAnnouncementSignalInfo), '("boundsChanged", Atk.Component.ComponentBoundsChangedSignalInfo), '("childrenChanged", Atk.Object.ObjectChildrenChangedSignalInfo), '("focusEvent", Atk.Object.ObjectFocusEventSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("propertyChange", Atk.Object.ObjectPropertyChangeSignalInfo), '("stateChange", Atk.Object.ObjectStateChangeSignalInfo), '("visibleDataChanged", Atk.Object.ObjectVisibleDataChangedSignalInfo)] :: [(Symbol, *)])

#endif


