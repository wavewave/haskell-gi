{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- /No description available in the introspection data./

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.TreeViewAccessible
    ( 

-- * Exported types
    TreeViewAccessible(..)                  ,
    IsTreeViewAccessible                    ,
    toTreeViewAccessible                    ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Interfaces.CellAccessibleParent#g:method:activate"), [addColumnSelection]("GI.Atk.Interfaces.Table#g:method:addColumnSelection"), [addRelationship]("GI.Atk.Objects.Object#g:method:addRelationship"), [addRowSelection]("GI.Atk.Interfaces.Table#g:method:addRowSelection"), [addSelection]("GI.Atk.Interfaces.Selection#g:method:addSelection"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [clearSelection]("GI.Atk.Interfaces.Selection#g:method:clearSelection"), [connectWidgetDestroyed]("GI.Gtk.Objects.Accessible#g:method:connectWidgetDestroyed"), [contains]("GI.Atk.Interfaces.Component#g:method:contains"), [edit]("GI.Gtk.Interfaces.CellAccessibleParent#g:method:edit"), [expandCollapse]("GI.Gtk.Interfaces.CellAccessibleParent#g:method:expandCollapse"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabFocus]("GI.Atk.Interfaces.Component#g:method:grabFocus"), [initialize]("GI.Atk.Objects.Object#g:method:initialize"), [isChildSelected]("GI.Atk.Interfaces.Selection#g:method:isChildSelected"), [isColumnSelected]("GI.Atk.Interfaces.Table#g:method:isColumnSelected"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isRowSelected]("GI.Atk.Interfaces.Table#g:method:isRowSelected"), [isSelected]("GI.Atk.Interfaces.Table#g:method:isSelected"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [notifyStateChange]("GI.Atk.Objects.Object#g:method:notifyStateChange"), [peekParent]("GI.Atk.Objects.Object#g:method:peekParent"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refAccessibleAtPoint]("GI.Atk.Interfaces.Component#g:method:refAccessibleAtPoint"), [refAccessibleChild]("GI.Atk.Objects.Object#g:method:refAccessibleChild"), [refAt]("GI.Atk.Interfaces.Table#g:method:refAt"), [refRelationSet]("GI.Atk.Objects.Object#g:method:refRelationSet"), [refSelection]("GI.Atk.Interfaces.Selection#g:method:refSelection"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [refStateSet]("GI.Atk.Objects.Object#g:method:refStateSet"), [removeColumnSelection]("GI.Atk.Interfaces.Table#g:method:removeColumnSelection"), [removeFocusHandler]("GI.Atk.Interfaces.Component#g:method:removeFocusHandler"), [removePropertyChangeHandler]("GI.Atk.Objects.Object#g:method:removePropertyChangeHandler"), [removeRelationship]("GI.Atk.Objects.Object#g:method:removeRelationship"), [removeRowSelection]("GI.Atk.Interfaces.Table#g:method:removeRowSelection"), [removeSelection]("GI.Atk.Interfaces.Selection#g:method:removeSelection"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [scrollTo]("GI.Atk.Interfaces.Component#g:method:scrollTo"), [scrollToPoint]("GI.Atk.Interfaces.Component#g:method:scrollToPoint"), [selectAllSelection]("GI.Atk.Interfaces.Selection#g:method:selectAllSelection"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [updateRelationset]("GI.Gtk.Interfaces.CellAccessibleParent#g:method:updateRelationset"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessibleId]("GI.Atk.Objects.Object#g:method:getAccessibleId"), [getAlpha]("GI.Atk.Interfaces.Component#g:method:getAlpha"), [getAttributes]("GI.Atk.Objects.Object#g:method:getAttributes"), [getCaption]("GI.Atk.Interfaces.Table#g:method:getCaption"), [getCellArea]("GI.Gtk.Interfaces.CellAccessibleParent#g:method:getCellArea"), [getCellExtents]("GI.Gtk.Interfaces.CellAccessibleParent#g:method:getCellExtents"), [getCellPosition]("GI.Gtk.Interfaces.CellAccessibleParent#g:method:getCellPosition"), [getChildIndex]("GI.Gtk.Interfaces.CellAccessibleParent#g:method:getChildIndex"), [getColumnAtIndex]("GI.Atk.Interfaces.Table#g:method:getColumnAtIndex"), [getColumnDescription]("GI.Atk.Interfaces.Table#g:method:getColumnDescription"), [getColumnExtentAt]("GI.Atk.Interfaces.Table#g:method:getColumnExtentAt"), [getColumnHeader]("GI.Atk.Interfaces.Table#g:method:getColumnHeader"), [getColumnHeaderCells]("GI.Gtk.Interfaces.CellAccessibleParent#g:method:getColumnHeaderCells"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDescription]("GI.Atk.Objects.Object#g:method:getDescription"), [getExtents]("GI.Atk.Interfaces.Component#g:method:getExtents"), [getIndexAt]("GI.Atk.Interfaces.Table#g:method:getIndexAt"), [getIndexInParent]("GI.Atk.Objects.Object#g:method:getIndexInParent"), [getLayer]("GI.Atk.Objects.Object#g:method:getLayer"), [getMdiZorder]("GI.Atk.Objects.Object#g:method:getMdiZorder"), [getNAccessibleChildren]("GI.Atk.Objects.Object#g:method:getNAccessibleChildren"), [getNColumns]("GI.Atk.Interfaces.Table#g:method:getNColumns"), [getNRows]("GI.Atk.Interfaces.Table#g:method:getNRows"), [getName]("GI.Atk.Objects.Object#g:method:getName"), [getObjectLocale]("GI.Atk.Objects.Object#g:method:getObjectLocale"), [getParent]("GI.Atk.Objects.Object#g:method:getParent"), [getPosition]("GI.Atk.Interfaces.Component#g:method:getPosition"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRendererState]("GI.Gtk.Interfaces.CellAccessibleParent#g:method:getRendererState"), [getRole]("GI.Atk.Objects.Object#g:method:getRole"), [getRowAtIndex]("GI.Atk.Interfaces.Table#g:method:getRowAtIndex"), [getRowDescription]("GI.Atk.Interfaces.Table#g:method:getRowDescription"), [getRowExtentAt]("GI.Atk.Interfaces.Table#g:method:getRowExtentAt"), [getRowHeader]("GI.Atk.Interfaces.Table#g:method:getRowHeader"), [getRowHeaderCells]("GI.Gtk.Interfaces.CellAccessibleParent#g:method:getRowHeaderCells"), [getSelectedColumns]("GI.Atk.Interfaces.Table#g:method:getSelectedColumns"), [getSelectedRows]("GI.Atk.Interfaces.Table#g:method:getSelectedRows"), [getSelectionCount]("GI.Atk.Interfaces.Selection#g:method:getSelectionCount"), [getSize]("GI.Atk.Interfaces.Component#g:method:getSize"), [getSummary]("GI.Atk.Interfaces.Table#g:method:getSummary"), [getWidget]("GI.Gtk.Objects.Accessible#g:method:getWidget").
-- 
-- ==== Setters
-- [setAccessibleId]("GI.Atk.Objects.Object#g:method:setAccessibleId"), [setCaption]("GI.Atk.Interfaces.Table#g:method:setCaption"), [setColumnDescription]("GI.Atk.Interfaces.Table#g:method:setColumnDescription"), [setColumnHeader]("GI.Atk.Interfaces.Table#g:method:setColumnHeader"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDescription]("GI.Atk.Objects.Object#g:method:setDescription"), [setExtents]("GI.Atk.Interfaces.Component#g:method:setExtents"), [setName]("GI.Atk.Objects.Object#g:method:setName"), [setParent]("GI.Atk.Objects.Object#g:method:setParent"), [setPosition]("GI.Atk.Interfaces.Component#g:method:setPosition"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRole]("GI.Atk.Objects.Object#g:method:setRole"), [setRowDescription]("GI.Atk.Interfaces.Table#g:method:setRowDescription"), [setRowHeader]("GI.Atk.Interfaces.Table#g:method:setRowHeader"), [setSize]("GI.Atk.Interfaces.Component#g:method:setSize"), [setSummary]("GI.Atk.Interfaces.Table#g:method:setSummary"), [setWidget]("GI.Gtk.Objects.Accessible#g:method:setWidget").

#if defined(ENABLE_OVERLOADING)
    ResolveTreeViewAccessibleMethod         ,
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
import qualified GI.Atk.Interfaces.Selection as Atk.Selection
import qualified GI.Atk.Interfaces.Table as Atk.Table
import qualified GI.Atk.Objects.Object as Atk.Object
import qualified GI.GObject.Objects.Object as GObject.Object
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.CellAccessibleParent as Gtk.CellAccessibleParent
import {-# SOURCE #-} qualified GI.Gtk.Objects.Accessible as Gtk.Accessible
import {-# SOURCE #-} qualified GI.Gtk.Objects.ContainerAccessible as Gtk.ContainerAccessible
import {-# SOURCE #-} qualified GI.Gtk.Objects.WidgetAccessible as Gtk.WidgetAccessible

-- | Memory-managed wrapper type.
newtype TreeViewAccessible = TreeViewAccessible (SP.ManagedPtr TreeViewAccessible)
    deriving (Eq)

instance SP.ManagedPtrNewtype TreeViewAccessible where
    toManagedPtr (TreeViewAccessible p) = p

foreign import ccall "gtk_tree_view_accessible_get_type"
    c_gtk_tree_view_accessible_get_type :: IO B.Types.GType

instance B.Types.TypedObject TreeViewAccessible where
    glibType = c_gtk_tree_view_accessible_get_type

instance B.Types.GObject TreeViewAccessible

-- | Type class for types which can be safely cast to `TreeViewAccessible`, for instance with `toTreeViewAccessible`.
class (SP.GObject o, O.IsDescendantOf TreeViewAccessible o) => IsTreeViewAccessible o
instance (SP.GObject o, O.IsDescendantOf TreeViewAccessible o) => IsTreeViewAccessible o

instance O.HasParentTypes TreeViewAccessible
type instance O.ParentTypes TreeViewAccessible = '[Gtk.ContainerAccessible.ContainerAccessible, Gtk.WidgetAccessible.WidgetAccessible, Gtk.Accessible.Accessible, Atk.Object.Object, GObject.Object.Object, Atk.Component.Component, Atk.Selection.Selection, Atk.Table.Table, Gtk.CellAccessibleParent.CellAccessibleParent]

-- | Cast to `TreeViewAccessible`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toTreeViewAccessible :: (MIO.MonadIO m, IsTreeViewAccessible o) => o -> m TreeViewAccessible
toTreeViewAccessible = MIO.liftIO . B.ManagedPtr.unsafeCastTo TreeViewAccessible

-- | Convert 'TreeViewAccessible' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe TreeViewAccessible) where
    gvalueGType_ = c_gtk_tree_view_accessible_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr TreeViewAccessible)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr TreeViewAccessible)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject TreeViewAccessible ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveTreeViewAccessibleMethod (t :: Symbol) (o :: *) :: * where
    ResolveTreeViewAccessibleMethod "activate" o = Gtk.CellAccessibleParent.CellAccessibleParentActivateMethodInfo
    ResolveTreeViewAccessibleMethod "addColumnSelection" o = Atk.Table.TableAddColumnSelectionMethodInfo
    ResolveTreeViewAccessibleMethod "addRelationship" o = Atk.Object.ObjectAddRelationshipMethodInfo
    ResolveTreeViewAccessibleMethod "addRowSelection" o = Atk.Table.TableAddRowSelectionMethodInfo
    ResolveTreeViewAccessibleMethod "addSelection" o = Atk.Selection.SelectionAddSelectionMethodInfo
    ResolveTreeViewAccessibleMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveTreeViewAccessibleMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveTreeViewAccessibleMethod "clearSelection" o = Atk.Selection.SelectionClearSelectionMethodInfo
    ResolveTreeViewAccessibleMethod "connectWidgetDestroyed" o = Gtk.Accessible.AccessibleConnectWidgetDestroyedMethodInfo
    ResolveTreeViewAccessibleMethod "contains" o = Atk.Component.ComponentContainsMethodInfo
    ResolveTreeViewAccessibleMethod "edit" o = Gtk.CellAccessibleParent.CellAccessibleParentEditMethodInfo
    ResolveTreeViewAccessibleMethod "expandCollapse" o = Gtk.CellAccessibleParent.CellAccessibleParentExpandCollapseMethodInfo
    ResolveTreeViewAccessibleMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveTreeViewAccessibleMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveTreeViewAccessibleMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveTreeViewAccessibleMethod "grabFocus" o = Atk.Component.ComponentGrabFocusMethodInfo
    ResolveTreeViewAccessibleMethod "initialize" o = Atk.Object.ObjectInitializeMethodInfo
    ResolveTreeViewAccessibleMethod "isChildSelected" o = Atk.Selection.SelectionIsChildSelectedMethodInfo
    ResolveTreeViewAccessibleMethod "isColumnSelected" o = Atk.Table.TableIsColumnSelectedMethodInfo
    ResolveTreeViewAccessibleMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveTreeViewAccessibleMethod "isRowSelected" o = Atk.Table.TableIsRowSelectedMethodInfo
    ResolveTreeViewAccessibleMethod "isSelected" o = Atk.Table.TableIsSelectedMethodInfo
    ResolveTreeViewAccessibleMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveTreeViewAccessibleMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveTreeViewAccessibleMethod "notifyStateChange" o = Atk.Object.ObjectNotifyStateChangeMethodInfo
    ResolveTreeViewAccessibleMethod "peekParent" o = Atk.Object.ObjectPeekParentMethodInfo
    ResolveTreeViewAccessibleMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveTreeViewAccessibleMethod "refAccessibleAtPoint" o = Atk.Component.ComponentRefAccessibleAtPointMethodInfo
    ResolveTreeViewAccessibleMethod "refAccessibleChild" o = Atk.Object.ObjectRefAccessibleChildMethodInfo
    ResolveTreeViewAccessibleMethod "refAt" o = Atk.Table.TableRefAtMethodInfo
    ResolveTreeViewAccessibleMethod "refRelationSet" o = Atk.Object.ObjectRefRelationSetMethodInfo
    ResolveTreeViewAccessibleMethod "refSelection" o = Atk.Selection.SelectionRefSelectionMethodInfo
    ResolveTreeViewAccessibleMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveTreeViewAccessibleMethod "refStateSet" o = Atk.Object.ObjectRefStateSetMethodInfo
    ResolveTreeViewAccessibleMethod "removeColumnSelection" o = Atk.Table.TableRemoveColumnSelectionMethodInfo
    ResolveTreeViewAccessibleMethod "removeFocusHandler" o = Atk.Component.ComponentRemoveFocusHandlerMethodInfo
    ResolveTreeViewAccessibleMethod "removePropertyChangeHandler" o = Atk.Object.ObjectRemovePropertyChangeHandlerMethodInfo
    ResolveTreeViewAccessibleMethod "removeRelationship" o = Atk.Object.ObjectRemoveRelationshipMethodInfo
    ResolveTreeViewAccessibleMethod "removeRowSelection" o = Atk.Table.TableRemoveRowSelectionMethodInfo
    ResolveTreeViewAccessibleMethod "removeSelection" o = Atk.Selection.SelectionRemoveSelectionMethodInfo
    ResolveTreeViewAccessibleMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveTreeViewAccessibleMethod "scrollTo" o = Atk.Component.ComponentScrollToMethodInfo
    ResolveTreeViewAccessibleMethod "scrollToPoint" o = Atk.Component.ComponentScrollToPointMethodInfo
    ResolveTreeViewAccessibleMethod "selectAllSelection" o = Atk.Selection.SelectionSelectAllSelectionMethodInfo
    ResolveTreeViewAccessibleMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveTreeViewAccessibleMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveTreeViewAccessibleMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveTreeViewAccessibleMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveTreeViewAccessibleMethod "updateRelationset" o = Gtk.CellAccessibleParent.CellAccessibleParentUpdateRelationsetMethodInfo
    ResolveTreeViewAccessibleMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveTreeViewAccessibleMethod "getAccessibleId" o = Atk.Object.ObjectGetAccessibleIdMethodInfo
    ResolveTreeViewAccessibleMethod "getAlpha" o = Atk.Component.ComponentGetAlphaMethodInfo
    ResolveTreeViewAccessibleMethod "getAttributes" o = Atk.Object.ObjectGetAttributesMethodInfo
    ResolveTreeViewAccessibleMethod "getCaption" o = Atk.Table.TableGetCaptionMethodInfo
    ResolveTreeViewAccessibleMethod "getCellArea" o = Gtk.CellAccessibleParent.CellAccessibleParentGetCellAreaMethodInfo
    ResolveTreeViewAccessibleMethod "getCellExtents" o = Gtk.CellAccessibleParent.CellAccessibleParentGetCellExtentsMethodInfo
    ResolveTreeViewAccessibleMethod "getCellPosition" o = Gtk.CellAccessibleParent.CellAccessibleParentGetCellPositionMethodInfo
    ResolveTreeViewAccessibleMethod "getChildIndex" o = Gtk.CellAccessibleParent.CellAccessibleParentGetChildIndexMethodInfo
    ResolveTreeViewAccessibleMethod "getColumnAtIndex" o = Atk.Table.TableGetColumnAtIndexMethodInfo
    ResolveTreeViewAccessibleMethod "getColumnDescription" o = Atk.Table.TableGetColumnDescriptionMethodInfo
    ResolveTreeViewAccessibleMethod "getColumnExtentAt" o = Atk.Table.TableGetColumnExtentAtMethodInfo
    ResolveTreeViewAccessibleMethod "getColumnHeader" o = Atk.Table.TableGetColumnHeaderMethodInfo
    ResolveTreeViewAccessibleMethod "getColumnHeaderCells" o = Gtk.CellAccessibleParent.CellAccessibleParentGetColumnHeaderCellsMethodInfo
    ResolveTreeViewAccessibleMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveTreeViewAccessibleMethod "getDescription" o = Atk.Object.ObjectGetDescriptionMethodInfo
    ResolveTreeViewAccessibleMethod "getExtents" o = Atk.Component.ComponentGetExtentsMethodInfo
    ResolveTreeViewAccessibleMethod "getIndexAt" o = Atk.Table.TableGetIndexAtMethodInfo
    ResolveTreeViewAccessibleMethod "getIndexInParent" o = Atk.Object.ObjectGetIndexInParentMethodInfo
    ResolveTreeViewAccessibleMethod "getLayer" o = Atk.Object.ObjectGetLayerMethodInfo
    ResolveTreeViewAccessibleMethod "getMdiZorder" o = Atk.Object.ObjectGetMdiZorderMethodInfo
    ResolveTreeViewAccessibleMethod "getNAccessibleChildren" o = Atk.Object.ObjectGetNAccessibleChildrenMethodInfo
    ResolveTreeViewAccessibleMethod "getNColumns" o = Atk.Table.TableGetNColumnsMethodInfo
    ResolveTreeViewAccessibleMethod "getNRows" o = Atk.Table.TableGetNRowsMethodInfo
    ResolveTreeViewAccessibleMethod "getName" o = Atk.Object.ObjectGetNameMethodInfo
    ResolveTreeViewAccessibleMethod "getObjectLocale" o = Atk.Object.ObjectGetObjectLocaleMethodInfo
    ResolveTreeViewAccessibleMethod "getParent" o = Atk.Object.ObjectGetParentMethodInfo
    ResolveTreeViewAccessibleMethod "getPosition" o = Atk.Component.ComponentGetPositionMethodInfo
    ResolveTreeViewAccessibleMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveTreeViewAccessibleMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveTreeViewAccessibleMethod "getRendererState" o = Gtk.CellAccessibleParent.CellAccessibleParentGetRendererStateMethodInfo
    ResolveTreeViewAccessibleMethod "getRole" o = Atk.Object.ObjectGetRoleMethodInfo
    ResolveTreeViewAccessibleMethod "getRowAtIndex" o = Atk.Table.TableGetRowAtIndexMethodInfo
    ResolveTreeViewAccessibleMethod "getRowDescription" o = Atk.Table.TableGetRowDescriptionMethodInfo
    ResolveTreeViewAccessibleMethod "getRowExtentAt" o = Atk.Table.TableGetRowExtentAtMethodInfo
    ResolveTreeViewAccessibleMethod "getRowHeader" o = Atk.Table.TableGetRowHeaderMethodInfo
    ResolveTreeViewAccessibleMethod "getRowHeaderCells" o = Gtk.CellAccessibleParent.CellAccessibleParentGetRowHeaderCellsMethodInfo
    ResolveTreeViewAccessibleMethod "getSelectedColumns" o = Atk.Table.TableGetSelectedColumnsMethodInfo
    ResolveTreeViewAccessibleMethod "getSelectedRows" o = Atk.Table.TableGetSelectedRowsMethodInfo
    ResolveTreeViewAccessibleMethod "getSelectionCount" o = Atk.Selection.SelectionGetSelectionCountMethodInfo
    ResolveTreeViewAccessibleMethod "getSize" o = Atk.Component.ComponentGetSizeMethodInfo
    ResolveTreeViewAccessibleMethod "getSummary" o = Atk.Table.TableGetSummaryMethodInfo
    ResolveTreeViewAccessibleMethod "getWidget" o = Gtk.Accessible.AccessibleGetWidgetMethodInfo
    ResolveTreeViewAccessibleMethod "setAccessibleId" o = Atk.Object.ObjectSetAccessibleIdMethodInfo
    ResolveTreeViewAccessibleMethod "setCaption" o = Atk.Table.TableSetCaptionMethodInfo
    ResolveTreeViewAccessibleMethod "setColumnDescription" o = Atk.Table.TableSetColumnDescriptionMethodInfo
    ResolveTreeViewAccessibleMethod "setColumnHeader" o = Atk.Table.TableSetColumnHeaderMethodInfo
    ResolveTreeViewAccessibleMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveTreeViewAccessibleMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveTreeViewAccessibleMethod "setDescription" o = Atk.Object.ObjectSetDescriptionMethodInfo
    ResolveTreeViewAccessibleMethod "setExtents" o = Atk.Component.ComponentSetExtentsMethodInfo
    ResolveTreeViewAccessibleMethod "setName" o = Atk.Object.ObjectSetNameMethodInfo
    ResolveTreeViewAccessibleMethod "setParent" o = Atk.Object.ObjectSetParentMethodInfo
    ResolveTreeViewAccessibleMethod "setPosition" o = Atk.Component.ComponentSetPositionMethodInfo
    ResolveTreeViewAccessibleMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveTreeViewAccessibleMethod "setRole" o = Atk.Object.ObjectSetRoleMethodInfo
    ResolveTreeViewAccessibleMethod "setRowDescription" o = Atk.Table.TableSetRowDescriptionMethodInfo
    ResolveTreeViewAccessibleMethod "setRowHeader" o = Atk.Table.TableSetRowHeaderMethodInfo
    ResolveTreeViewAccessibleMethod "setSize" o = Atk.Component.ComponentSetSizeMethodInfo
    ResolveTreeViewAccessibleMethod "setSummary" o = Atk.Table.TableSetSummaryMethodInfo
    ResolveTreeViewAccessibleMethod "setWidget" o = Gtk.Accessible.AccessibleSetWidgetMethodInfo
    ResolveTreeViewAccessibleMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveTreeViewAccessibleMethod t TreeViewAccessible, O.OverloadedMethod info TreeViewAccessible p) => OL.IsLabel t (TreeViewAccessible -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveTreeViewAccessibleMethod t TreeViewAccessible, O.OverloadedMethod info TreeViewAccessible p, R.HasField t TreeViewAccessible p) => R.HasField t TreeViewAccessible p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveTreeViewAccessibleMethod t TreeViewAccessible, O.OverloadedMethodInfo info TreeViewAccessible) => OL.IsLabel t (O.MethodProxy info TreeViewAccessible) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList TreeViewAccessible
type instance O.AttributeList TreeViewAccessible = TreeViewAccessibleAttributeList
type TreeViewAccessibleAttributeList = ('[ '("accessibleComponentLayer", Atk.Object.ObjectAccessibleComponentLayerPropertyInfo), '("accessibleComponentMdiZorder", Atk.Object.ObjectAccessibleComponentMdiZorderPropertyInfo), '("accessibleDescription", Atk.Object.ObjectAccessibleDescriptionPropertyInfo), '("accessibleHypertextNlinks", Atk.Object.ObjectAccessibleHypertextNlinksPropertyInfo), '("accessibleName", Atk.Object.ObjectAccessibleNamePropertyInfo), '("accessibleParent", Atk.Object.ObjectAccessibleParentPropertyInfo), '("accessibleRole", Atk.Object.ObjectAccessibleRolePropertyInfo), '("accessibleTableCaption", Atk.Object.ObjectAccessibleTableCaptionPropertyInfo), '("accessibleTableCaptionObject", Atk.Object.ObjectAccessibleTableCaptionObjectPropertyInfo), '("accessibleTableColumnDescription", Atk.Object.ObjectAccessibleTableColumnDescriptionPropertyInfo), '("accessibleTableColumnHeader", Atk.Object.ObjectAccessibleTableColumnHeaderPropertyInfo), '("accessibleTableRowDescription", Atk.Object.ObjectAccessibleTableRowDescriptionPropertyInfo), '("accessibleTableRowHeader", Atk.Object.ObjectAccessibleTableRowHeaderPropertyInfo), '("accessibleTableSummary", Atk.Object.ObjectAccessibleTableSummaryPropertyInfo), '("accessibleValue", Atk.Object.ObjectAccessibleValuePropertyInfo), '("widget", Gtk.Accessible.AccessibleWidgetPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList TreeViewAccessible = TreeViewAccessibleSignalList
type TreeViewAccessibleSignalList = ('[ '("activeDescendantChanged", Atk.Object.ObjectActiveDescendantChangedSignalInfo), '("announcement", Atk.Object.ObjectAnnouncementSignalInfo), '("boundsChanged", Atk.Component.ComponentBoundsChangedSignalInfo), '("childrenChanged", Atk.Object.ObjectChildrenChangedSignalInfo), '("columnDeleted", Atk.Table.TableColumnDeletedSignalInfo), '("columnInserted", Atk.Table.TableColumnInsertedSignalInfo), '("columnReordered", Atk.Table.TableColumnReorderedSignalInfo), '("focusEvent", Atk.Object.ObjectFocusEventSignalInfo), '("modelChanged", Atk.Table.TableModelChangedSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("propertyChange", Atk.Object.ObjectPropertyChangeSignalInfo), '("rowDeleted", Atk.Table.TableRowDeletedSignalInfo), '("rowInserted", Atk.Table.TableRowInsertedSignalInfo), '("rowReordered", Atk.Table.TableRowReorderedSignalInfo), '("selectionChanged", Atk.Selection.SelectionSelectionChangedSignalInfo), '("stateChange", Atk.Object.ObjectStateChangeSignalInfo), '("visibleDataChanged", Atk.Object.ObjectVisibleDataChangedSignalInfo)] :: [(Symbol, *)])

#endif


