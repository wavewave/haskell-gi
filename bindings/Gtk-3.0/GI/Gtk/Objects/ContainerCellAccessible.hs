{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- /No description available in the introspection data./

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.ContainerCellAccessible
    ( 

-- * Exported types
    ContainerCellAccessible(..)             ,
    IsContainerCellAccessible               ,
    toContainerCellAccessible               ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [addChild]("GI.Gtk.Objects.ContainerCellAccessible#g:method:addChild"), [addRelationship]("GI.Atk.Objects.Object#g:method:addRelationship"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [connectWidgetDestroyed]("GI.Gtk.Objects.Accessible#g:method:connectWidgetDestroyed"), [contains]("GI.Atk.Interfaces.Component#g:method:contains"), [doAction]("GI.Atk.Interfaces.Action#g:method:doAction"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabFocus]("GI.Atk.Interfaces.Component#g:method:grabFocus"), [initialize]("GI.Atk.Objects.Object#g:method:initialize"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [notifyStateChange]("GI.Atk.Objects.Object#g:method:notifyStateChange"), [peekParent]("GI.Atk.Objects.Object#g:method:peekParent"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refAccessibleAtPoint]("GI.Atk.Interfaces.Component#g:method:refAccessibleAtPoint"), [refAccessibleChild]("GI.Atk.Objects.Object#g:method:refAccessibleChild"), [refRelationSet]("GI.Atk.Objects.Object#g:method:refRelationSet"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [refStateSet]("GI.Atk.Objects.Object#g:method:refStateSet"), [removeChild]("GI.Gtk.Objects.ContainerCellAccessible#g:method:removeChild"), [removeFocusHandler]("GI.Atk.Interfaces.Component#g:method:removeFocusHandler"), [removePropertyChangeHandler]("GI.Atk.Objects.Object#g:method:removePropertyChangeHandler"), [removeRelationship]("GI.Atk.Objects.Object#g:method:removeRelationship"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [scrollTo]("GI.Atk.Interfaces.Component#g:method:scrollTo"), [scrollToPoint]("GI.Atk.Interfaces.Component#g:method:scrollToPoint"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessibleId]("GI.Atk.Objects.Object#g:method:getAccessibleId"), [getAlpha]("GI.Atk.Interfaces.Component#g:method:getAlpha"), [getAttributes]("GI.Atk.Objects.Object#g:method:getAttributes"), [getChildren]("GI.Gtk.Objects.ContainerCellAccessible#g:method:getChildren"), [getColumnHeaderCells]("GI.Atk.Interfaces.TableCell#g:method:getColumnHeaderCells"), [getColumnSpan]("GI.Atk.Interfaces.TableCell#g:method:getColumnSpan"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDescription]("GI.Atk.Objects.Object#g:method:getDescription"), [getExtents]("GI.Atk.Interfaces.Component#g:method:getExtents"), [getIndexInParent]("GI.Atk.Objects.Object#g:method:getIndexInParent"), [getKeybinding]("GI.Atk.Interfaces.Action#g:method:getKeybinding"), [getLayer]("GI.Atk.Objects.Object#g:method:getLayer"), [getLocalizedName]("GI.Atk.Interfaces.Action#g:method:getLocalizedName"), [getMdiZorder]("GI.Atk.Objects.Object#g:method:getMdiZorder"), [getNAccessibleChildren]("GI.Atk.Objects.Object#g:method:getNAccessibleChildren"), [getNActions]("GI.Atk.Interfaces.Action#g:method:getNActions"), [getName]("GI.Atk.Objects.Object#g:method:getName"), [getObjectLocale]("GI.Atk.Objects.Object#g:method:getObjectLocale"), [getParent]("GI.Atk.Objects.Object#g:method:getParent"), [getPosition]("GI.Atk.Interfaces.Component#g:method:getPosition"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRole]("GI.Atk.Objects.Object#g:method:getRole"), [getRowColumnSpan]("GI.Atk.Interfaces.TableCell#g:method:getRowColumnSpan"), [getRowHeaderCells]("GI.Atk.Interfaces.TableCell#g:method:getRowHeaderCells"), [getRowSpan]("GI.Atk.Interfaces.TableCell#g:method:getRowSpan"), [getSize]("GI.Atk.Interfaces.Component#g:method:getSize"), [getTable]("GI.Atk.Interfaces.TableCell#g:method:getTable"), [getWidget]("GI.Gtk.Objects.Accessible#g:method:getWidget").
-- 
-- ==== Setters
-- [setAccessibleId]("GI.Atk.Objects.Object#g:method:setAccessibleId"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDescription]("GI.Atk.Objects.Object#g:method:setDescription"), [setExtents]("GI.Atk.Interfaces.Component#g:method:setExtents"), [setName]("GI.Atk.Objects.Object#g:method:setName"), [setParent]("GI.Atk.Objects.Object#g:method:setParent"), [setPosition]("GI.Atk.Interfaces.Component#g:method:setPosition"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRole]("GI.Atk.Objects.Object#g:method:setRole"), [setSize]("GI.Atk.Interfaces.Component#g:method:setSize"), [setWidget]("GI.Gtk.Objects.Accessible#g:method:setWidget").

#if defined(ENABLE_OVERLOADING)
    ResolveContainerCellAccessibleMethod    ,
#endif

-- ** addChild #method:addChild#

#if defined(ENABLE_OVERLOADING)
    ContainerCellAccessibleAddChildMethodInfo,
#endif
    containerCellAccessibleAddChild         ,


-- ** getChildren #method:getChildren#

#if defined(ENABLE_OVERLOADING)
    ContainerCellAccessibleGetChildrenMethodInfo,
#endif
    containerCellAccessibleGetChildren      ,


-- ** new #method:new#

    containerCellAccessibleNew              ,


-- ** removeChild #method:removeChild#

#if defined(ENABLE_OVERLOADING)
    ContainerCellAccessibleRemoveChildMethodInfo,
#endif
    containerCellAccessibleRemoveChild      ,




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
import qualified GI.Atk.Interfaces.TableCell as Atk.TableCell
import qualified GI.Atk.Objects.Object as Atk.Object
import qualified GI.GObject.Objects.Object as GObject.Object
import {-# SOURCE #-} qualified GI.Gtk.Objects.Accessible as Gtk.Accessible
import {-# SOURCE #-} qualified GI.Gtk.Objects.CellAccessible as Gtk.CellAccessible

-- | Memory-managed wrapper type.
newtype ContainerCellAccessible = ContainerCellAccessible (SP.ManagedPtr ContainerCellAccessible)
    deriving (Eq)

instance SP.ManagedPtrNewtype ContainerCellAccessible where
    toManagedPtr (ContainerCellAccessible p) = p

foreign import ccall "gtk_container_cell_accessible_get_type"
    c_gtk_container_cell_accessible_get_type :: IO B.Types.GType

instance B.Types.TypedObject ContainerCellAccessible where
    glibType = c_gtk_container_cell_accessible_get_type

instance B.Types.GObject ContainerCellAccessible

-- | Type class for types which can be safely cast to `ContainerCellAccessible`, for instance with `toContainerCellAccessible`.
class (SP.GObject o, O.IsDescendantOf ContainerCellAccessible o) => IsContainerCellAccessible o
instance (SP.GObject o, O.IsDescendantOf ContainerCellAccessible o) => IsContainerCellAccessible o

instance O.HasParentTypes ContainerCellAccessible
type instance O.ParentTypes ContainerCellAccessible = '[Gtk.CellAccessible.CellAccessible, Gtk.Accessible.Accessible, Atk.Object.Object, GObject.Object.Object, Atk.Action.Action, Atk.Component.Component, Atk.TableCell.TableCell]

-- | Cast to `ContainerCellAccessible`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toContainerCellAccessible :: (MIO.MonadIO m, IsContainerCellAccessible o) => o -> m ContainerCellAccessible
toContainerCellAccessible = MIO.liftIO . B.ManagedPtr.unsafeCastTo ContainerCellAccessible

-- | Convert 'ContainerCellAccessible' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe ContainerCellAccessible) where
    gvalueGType_ = c_gtk_container_cell_accessible_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr ContainerCellAccessible)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr ContainerCellAccessible)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject ContainerCellAccessible ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveContainerCellAccessibleMethod (t :: Symbol) (o :: *) :: * where
    ResolveContainerCellAccessibleMethod "addChild" o = ContainerCellAccessibleAddChildMethodInfo
    ResolveContainerCellAccessibleMethod "addRelationship" o = Atk.Object.ObjectAddRelationshipMethodInfo
    ResolveContainerCellAccessibleMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveContainerCellAccessibleMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveContainerCellAccessibleMethod "connectWidgetDestroyed" o = Gtk.Accessible.AccessibleConnectWidgetDestroyedMethodInfo
    ResolveContainerCellAccessibleMethod "contains" o = Atk.Component.ComponentContainsMethodInfo
    ResolveContainerCellAccessibleMethod "doAction" o = Atk.Action.ActionDoActionMethodInfo
    ResolveContainerCellAccessibleMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveContainerCellAccessibleMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveContainerCellAccessibleMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveContainerCellAccessibleMethod "grabFocus" o = Atk.Component.ComponentGrabFocusMethodInfo
    ResolveContainerCellAccessibleMethod "initialize" o = Atk.Object.ObjectInitializeMethodInfo
    ResolveContainerCellAccessibleMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveContainerCellAccessibleMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveContainerCellAccessibleMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveContainerCellAccessibleMethod "notifyStateChange" o = Atk.Object.ObjectNotifyStateChangeMethodInfo
    ResolveContainerCellAccessibleMethod "peekParent" o = Atk.Object.ObjectPeekParentMethodInfo
    ResolveContainerCellAccessibleMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveContainerCellAccessibleMethod "refAccessibleAtPoint" o = Atk.Component.ComponentRefAccessibleAtPointMethodInfo
    ResolveContainerCellAccessibleMethod "refAccessibleChild" o = Atk.Object.ObjectRefAccessibleChildMethodInfo
    ResolveContainerCellAccessibleMethod "refRelationSet" o = Atk.Object.ObjectRefRelationSetMethodInfo
    ResolveContainerCellAccessibleMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveContainerCellAccessibleMethod "refStateSet" o = Atk.Object.ObjectRefStateSetMethodInfo
    ResolveContainerCellAccessibleMethod "removeChild" o = ContainerCellAccessibleRemoveChildMethodInfo
    ResolveContainerCellAccessibleMethod "removeFocusHandler" o = Atk.Component.ComponentRemoveFocusHandlerMethodInfo
    ResolveContainerCellAccessibleMethod "removePropertyChangeHandler" o = Atk.Object.ObjectRemovePropertyChangeHandlerMethodInfo
    ResolveContainerCellAccessibleMethod "removeRelationship" o = Atk.Object.ObjectRemoveRelationshipMethodInfo
    ResolveContainerCellAccessibleMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveContainerCellAccessibleMethod "scrollTo" o = Atk.Component.ComponentScrollToMethodInfo
    ResolveContainerCellAccessibleMethod "scrollToPoint" o = Atk.Component.ComponentScrollToPointMethodInfo
    ResolveContainerCellAccessibleMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveContainerCellAccessibleMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveContainerCellAccessibleMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveContainerCellAccessibleMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveContainerCellAccessibleMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveContainerCellAccessibleMethod "getAccessibleId" o = Atk.Object.ObjectGetAccessibleIdMethodInfo
    ResolveContainerCellAccessibleMethod "getAlpha" o = Atk.Component.ComponentGetAlphaMethodInfo
    ResolveContainerCellAccessibleMethod "getAttributes" o = Atk.Object.ObjectGetAttributesMethodInfo
    ResolveContainerCellAccessibleMethod "getChildren" o = ContainerCellAccessibleGetChildrenMethodInfo
    ResolveContainerCellAccessibleMethod "getColumnHeaderCells" o = Atk.TableCell.TableCellGetColumnHeaderCellsMethodInfo
    ResolveContainerCellAccessibleMethod "getColumnSpan" o = Atk.TableCell.TableCellGetColumnSpanMethodInfo
    ResolveContainerCellAccessibleMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveContainerCellAccessibleMethod "getDescription" o = Atk.Object.ObjectGetDescriptionMethodInfo
    ResolveContainerCellAccessibleMethod "getExtents" o = Atk.Component.ComponentGetExtentsMethodInfo
    ResolveContainerCellAccessibleMethod "getIndexInParent" o = Atk.Object.ObjectGetIndexInParentMethodInfo
    ResolveContainerCellAccessibleMethod "getKeybinding" o = Atk.Action.ActionGetKeybindingMethodInfo
    ResolveContainerCellAccessibleMethod "getLayer" o = Atk.Object.ObjectGetLayerMethodInfo
    ResolveContainerCellAccessibleMethod "getLocalizedName" o = Atk.Action.ActionGetLocalizedNameMethodInfo
    ResolveContainerCellAccessibleMethod "getMdiZorder" o = Atk.Object.ObjectGetMdiZorderMethodInfo
    ResolveContainerCellAccessibleMethod "getNAccessibleChildren" o = Atk.Object.ObjectGetNAccessibleChildrenMethodInfo
    ResolveContainerCellAccessibleMethod "getNActions" o = Atk.Action.ActionGetNActionsMethodInfo
    ResolveContainerCellAccessibleMethod "getName" o = Atk.Object.ObjectGetNameMethodInfo
    ResolveContainerCellAccessibleMethod "getObjectLocale" o = Atk.Object.ObjectGetObjectLocaleMethodInfo
    ResolveContainerCellAccessibleMethod "getParent" o = Atk.Object.ObjectGetParentMethodInfo
    ResolveContainerCellAccessibleMethod "getPosition" o = Atk.Component.ComponentGetPositionMethodInfo
    ResolveContainerCellAccessibleMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveContainerCellAccessibleMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveContainerCellAccessibleMethod "getRole" o = Atk.Object.ObjectGetRoleMethodInfo
    ResolveContainerCellAccessibleMethod "getRowColumnSpan" o = Atk.TableCell.TableCellGetRowColumnSpanMethodInfo
    ResolveContainerCellAccessibleMethod "getRowHeaderCells" o = Atk.TableCell.TableCellGetRowHeaderCellsMethodInfo
    ResolveContainerCellAccessibleMethod "getRowSpan" o = Atk.TableCell.TableCellGetRowSpanMethodInfo
    ResolveContainerCellAccessibleMethod "getSize" o = Atk.Component.ComponentGetSizeMethodInfo
    ResolveContainerCellAccessibleMethod "getTable" o = Atk.TableCell.TableCellGetTableMethodInfo
    ResolveContainerCellAccessibleMethod "getWidget" o = Gtk.Accessible.AccessibleGetWidgetMethodInfo
    ResolveContainerCellAccessibleMethod "setAccessibleId" o = Atk.Object.ObjectSetAccessibleIdMethodInfo
    ResolveContainerCellAccessibleMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveContainerCellAccessibleMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveContainerCellAccessibleMethod "setDescription" o = Atk.Object.ObjectSetDescriptionMethodInfo
    ResolveContainerCellAccessibleMethod "setExtents" o = Atk.Component.ComponentSetExtentsMethodInfo
    ResolveContainerCellAccessibleMethod "setName" o = Atk.Object.ObjectSetNameMethodInfo
    ResolveContainerCellAccessibleMethod "setParent" o = Atk.Object.ObjectSetParentMethodInfo
    ResolveContainerCellAccessibleMethod "setPosition" o = Atk.Component.ComponentSetPositionMethodInfo
    ResolveContainerCellAccessibleMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveContainerCellAccessibleMethod "setRole" o = Atk.Object.ObjectSetRoleMethodInfo
    ResolveContainerCellAccessibleMethod "setSize" o = Atk.Component.ComponentSetSizeMethodInfo
    ResolveContainerCellAccessibleMethod "setWidget" o = Gtk.Accessible.AccessibleSetWidgetMethodInfo
    ResolveContainerCellAccessibleMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveContainerCellAccessibleMethod t ContainerCellAccessible, O.OverloadedMethod info ContainerCellAccessible p) => OL.IsLabel t (ContainerCellAccessible -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveContainerCellAccessibleMethod t ContainerCellAccessible, O.OverloadedMethod info ContainerCellAccessible p, R.HasField t ContainerCellAccessible p) => R.HasField t ContainerCellAccessible p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveContainerCellAccessibleMethod t ContainerCellAccessible, O.OverloadedMethodInfo info ContainerCellAccessible) => OL.IsLabel t (O.MethodProxy info ContainerCellAccessible) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList ContainerCellAccessible
type instance O.AttributeList ContainerCellAccessible = ContainerCellAccessibleAttributeList
type ContainerCellAccessibleAttributeList = ('[ '("accessibleComponentLayer", Atk.Object.ObjectAccessibleComponentLayerPropertyInfo), '("accessibleComponentMdiZorder", Atk.Object.ObjectAccessibleComponentMdiZorderPropertyInfo), '("accessibleDescription", Atk.Object.ObjectAccessibleDescriptionPropertyInfo), '("accessibleHypertextNlinks", Atk.Object.ObjectAccessibleHypertextNlinksPropertyInfo), '("accessibleName", Atk.Object.ObjectAccessibleNamePropertyInfo), '("accessibleParent", Atk.Object.ObjectAccessibleParentPropertyInfo), '("accessibleRole", Atk.Object.ObjectAccessibleRolePropertyInfo), '("accessibleTableCaption", Atk.Object.ObjectAccessibleTableCaptionPropertyInfo), '("accessibleTableCaptionObject", Atk.Object.ObjectAccessibleTableCaptionObjectPropertyInfo), '("accessibleTableColumnDescription", Atk.Object.ObjectAccessibleTableColumnDescriptionPropertyInfo), '("accessibleTableColumnHeader", Atk.Object.ObjectAccessibleTableColumnHeaderPropertyInfo), '("accessibleTableRowDescription", Atk.Object.ObjectAccessibleTableRowDescriptionPropertyInfo), '("accessibleTableRowHeader", Atk.Object.ObjectAccessibleTableRowHeaderPropertyInfo), '("accessibleTableSummary", Atk.Object.ObjectAccessibleTableSummaryPropertyInfo), '("accessibleValue", Atk.Object.ObjectAccessibleValuePropertyInfo), '("widget", Gtk.Accessible.AccessibleWidgetPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList ContainerCellAccessible = ContainerCellAccessibleSignalList
type ContainerCellAccessibleSignalList = ('[ '("activeDescendantChanged", Atk.Object.ObjectActiveDescendantChangedSignalInfo), '("announcement", Atk.Object.ObjectAnnouncementSignalInfo), '("boundsChanged", Atk.Component.ComponentBoundsChangedSignalInfo), '("childrenChanged", Atk.Object.ObjectChildrenChangedSignalInfo), '("focusEvent", Atk.Object.ObjectFocusEventSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("propertyChange", Atk.Object.ObjectPropertyChangeSignalInfo), '("stateChange", Atk.Object.ObjectStateChangeSignalInfo), '("visibleDataChanged", Atk.Object.ObjectVisibleDataChangedSignalInfo)] :: [(Symbol, *)])

#endif

-- method ContainerCellAccessible::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just
--               (TInterface
--                  Name { namespace = "Gtk" , name = "ContainerCellAccessible" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_container_cell_accessible_new" gtk_container_cell_accessible_new :: 
    IO (Ptr ContainerCellAccessible)

-- | /No description available in the introspection data./
containerCellAccessibleNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m ContainerCellAccessible
containerCellAccessibleNew  = liftIO $ do
    result <- gtk_container_cell_accessible_new
    checkUnexpectedReturnNULL "containerCellAccessibleNew" result
    result' <- (wrapObject ContainerCellAccessible) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method ContainerCellAccessible::add_child
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "container"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "ContainerCellAccessible" }
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
--       , Arg
--           { argCName = "child"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellAccessible" }
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
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_container_cell_accessible_add_child" gtk_container_cell_accessible_add_child :: 
    Ptr ContainerCellAccessible ->          -- container : TInterface (Name {namespace = "Gtk", name = "ContainerCellAccessible"})
    Ptr Gtk.CellAccessible.CellAccessible -> -- child : TInterface (Name {namespace = "Gtk", name = "CellAccessible"})
    IO ()

-- | /No description available in the introspection data./
containerCellAccessibleAddChild ::
    (B.CallStack.HasCallStack, MonadIO m, IsContainerCellAccessible a, Gtk.CellAccessible.IsCellAccessible b) =>
    a
    -> b
    -> m ()
containerCellAccessibleAddChild container child = liftIO $ do
    container' <- unsafeManagedPtrCastPtr container
    child' <- unsafeManagedPtrCastPtr child
    gtk_container_cell_accessible_add_child container' child'
    touchManagedPtr container
    touchManagedPtr child
    return ()

#if defined(ENABLE_OVERLOADING)
data ContainerCellAccessibleAddChildMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsContainerCellAccessible a, Gtk.CellAccessible.IsCellAccessible b) => O.OverloadedMethod ContainerCellAccessibleAddChildMethodInfo a signature where
    overloadedMethod = containerCellAccessibleAddChild

instance O.OverloadedMethodInfo ContainerCellAccessibleAddChildMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ContainerCellAccessible.containerCellAccessibleAddChild",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ContainerCellAccessible.html#v:containerCellAccessibleAddChild"
        })


#endif

-- method ContainerCellAccessible::get_children
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "container"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "ContainerCellAccessible" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the container" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TGList
--                  (TInterface Name { namespace = "Gtk" , name = "CellAccessible" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_container_cell_accessible_get_children" gtk_container_cell_accessible_get_children :: 
    Ptr ContainerCellAccessible ->          -- container : TInterface (Name {namespace = "Gtk", name = "ContainerCellAccessible"})
    IO (Ptr (GList (Ptr Gtk.CellAccessible.CellAccessible)))

-- | Get a list of children.
containerCellAccessibleGetChildren ::
    (B.CallStack.HasCallStack, MonadIO m, IsContainerCellAccessible a) =>
    a
    -- ^ /@container@/: the container
    -> m [Gtk.CellAccessible.CellAccessible]
containerCellAccessibleGetChildren container = liftIO $ do
    container' <- unsafeManagedPtrCastPtr container
    result <- gtk_container_cell_accessible_get_children container'
    result' <- unpackGList result
    result'' <- mapM (newObject Gtk.CellAccessible.CellAccessible) result'
    touchManagedPtr container
    return result''

#if defined(ENABLE_OVERLOADING)
data ContainerCellAccessibleGetChildrenMethodInfo
instance (signature ~ (m [Gtk.CellAccessible.CellAccessible]), MonadIO m, IsContainerCellAccessible a) => O.OverloadedMethod ContainerCellAccessibleGetChildrenMethodInfo a signature where
    overloadedMethod = containerCellAccessibleGetChildren

instance O.OverloadedMethodInfo ContainerCellAccessibleGetChildrenMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ContainerCellAccessible.containerCellAccessibleGetChildren",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ContainerCellAccessible.html#v:containerCellAccessibleGetChildren"
        })


#endif

-- method ContainerCellAccessible::remove_child
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "container"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "ContainerCellAccessible" }
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
--       , Arg
--           { argCName = "child"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellAccessible" }
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
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_container_cell_accessible_remove_child" gtk_container_cell_accessible_remove_child :: 
    Ptr ContainerCellAccessible ->          -- container : TInterface (Name {namespace = "Gtk", name = "ContainerCellAccessible"})
    Ptr Gtk.CellAccessible.CellAccessible -> -- child : TInterface (Name {namespace = "Gtk", name = "CellAccessible"})
    IO ()

-- | /No description available in the introspection data./
containerCellAccessibleRemoveChild ::
    (B.CallStack.HasCallStack, MonadIO m, IsContainerCellAccessible a, Gtk.CellAccessible.IsCellAccessible b) =>
    a
    -> b
    -> m ()
containerCellAccessibleRemoveChild container child = liftIO $ do
    container' <- unsafeManagedPtrCastPtr container
    child' <- unsafeManagedPtrCastPtr child
    gtk_container_cell_accessible_remove_child container' child'
    touchManagedPtr container
    touchManagedPtr child
    return ()

#if defined(ENABLE_OVERLOADING)
data ContainerCellAccessibleRemoveChildMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsContainerCellAccessible a, Gtk.CellAccessible.IsCellAccessible b) => O.OverloadedMethod ContainerCellAccessibleRemoveChildMethodInfo a signature where
    overloadedMethod = containerCellAccessibleRemoveChild

instance O.OverloadedMethodInfo ContainerCellAccessibleRemoveChildMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ContainerCellAccessible.containerCellAccessibleRemoveChild",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ContainerCellAccessible.html#v:containerCellAccessibleRemoveChild"
        })


#endif


