{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- The GtkTreeViewColumn object represents a visible column in a t'GI.Gtk.Objects.TreeView.TreeView' widget.
-- It allows to set properties of the column header, and functions as a holding pen for
-- the cell renderers which determine how the data in the column is displayed.
-- 
-- Please refer to the [tree widget conceptual overview][TreeWidget]
-- for an overview of all the objects and data types related to the tree widget and how
-- they work together.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.TreeViewColumn
    ( 

-- * Exported types
    TreeViewColumn(..)                      ,
    IsTreeViewColumn                        ,
    toTreeViewColumn                        ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [addAttribute]("GI.Gtk.Objects.TreeViewColumn#g:method:addAttribute"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [cellGetPosition]("GI.Gtk.Objects.TreeViewColumn#g:method:cellGetPosition"), [cellGetSize]("GI.Gtk.Objects.TreeViewColumn#g:method:cellGetSize"), [cellIsVisible]("GI.Gtk.Objects.TreeViewColumn#g:method:cellIsVisible"), [cellSetCellData]("GI.Gtk.Objects.TreeViewColumn#g:method:cellSetCellData"), [clear]("GI.Gtk.Objects.TreeViewColumn#g:method:clear"), [clearAttributes]("GI.Gtk.Objects.TreeViewColumn#g:method:clearAttributes"), [clicked]("GI.Gtk.Objects.TreeViewColumn#g:method:clicked"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [focusCell]("GI.Gtk.Objects.TreeViewColumn#g:method:focusCell"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [packEnd]("GI.Gtk.Objects.TreeViewColumn#g:method:packEnd"), [packStart]("GI.Gtk.Objects.TreeViewColumn#g:method:packStart"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [queueResize]("GI.Gtk.Objects.TreeViewColumn#g:method:queueResize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [reorder]("GI.Gtk.Interfaces.CellLayout#g:method:reorder"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAlignment]("GI.Gtk.Objects.TreeViewColumn#g:method:getAlignment"), [getArea]("GI.Gtk.Interfaces.CellLayout#g:method:getArea"), [getButton]("GI.Gtk.Objects.TreeViewColumn#g:method:getButton"), [getCells]("GI.Gtk.Interfaces.CellLayout#g:method:getCells"), [getClickable]("GI.Gtk.Objects.TreeViewColumn#g:method:getClickable"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getExpand]("GI.Gtk.Objects.TreeViewColumn#g:method:getExpand"), [getFixedWidth]("GI.Gtk.Objects.TreeViewColumn#g:method:getFixedWidth"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getMaxWidth]("GI.Gtk.Objects.TreeViewColumn#g:method:getMaxWidth"), [getMinWidth]("GI.Gtk.Objects.TreeViewColumn#g:method:getMinWidth"), [getName]("GI.Gtk.Interfaces.Buildable#g:method:getName"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getReorderable]("GI.Gtk.Objects.TreeViewColumn#g:method:getReorderable"), [getResizable]("GI.Gtk.Objects.TreeViewColumn#g:method:getResizable"), [getSizing]("GI.Gtk.Objects.TreeViewColumn#g:method:getSizing"), [getSortColumnId]("GI.Gtk.Objects.TreeViewColumn#g:method:getSortColumnId"), [getSortIndicator]("GI.Gtk.Objects.TreeViewColumn#g:method:getSortIndicator"), [getSortOrder]("GI.Gtk.Objects.TreeViewColumn#g:method:getSortOrder"), [getSpacing]("GI.Gtk.Objects.TreeViewColumn#g:method:getSpacing"), [getTitle]("GI.Gtk.Objects.TreeViewColumn#g:method:getTitle"), [getTreeView]("GI.Gtk.Objects.TreeViewColumn#g:method:getTreeView"), [getVisible]("GI.Gtk.Objects.TreeViewColumn#g:method:getVisible"), [getWidget]("GI.Gtk.Objects.TreeViewColumn#g:method:getWidget"), [getWidth]("GI.Gtk.Objects.TreeViewColumn#g:method:getWidth"), [getXOffset]("GI.Gtk.Objects.TreeViewColumn#g:method:getXOffset").
-- 
-- ==== Setters
-- [setAlignment]("GI.Gtk.Objects.TreeViewColumn#g:method:setAlignment"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCellDataFunc]("GI.Gtk.Objects.TreeViewColumn#g:method:setCellDataFunc"), [setClickable]("GI.Gtk.Objects.TreeViewColumn#g:method:setClickable"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setExpand]("GI.Gtk.Objects.TreeViewColumn#g:method:setExpand"), [setFixedWidth]("GI.Gtk.Objects.TreeViewColumn#g:method:setFixedWidth"), [setMaxWidth]("GI.Gtk.Objects.TreeViewColumn#g:method:setMaxWidth"), [setMinWidth]("GI.Gtk.Objects.TreeViewColumn#g:method:setMinWidth"), [setName]("GI.Gtk.Interfaces.Buildable#g:method:setName"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setReorderable]("GI.Gtk.Objects.TreeViewColumn#g:method:setReorderable"), [setResizable]("GI.Gtk.Objects.TreeViewColumn#g:method:setResizable"), [setSizing]("GI.Gtk.Objects.TreeViewColumn#g:method:setSizing"), [setSortColumnId]("GI.Gtk.Objects.TreeViewColumn#g:method:setSortColumnId"), [setSortIndicator]("GI.Gtk.Objects.TreeViewColumn#g:method:setSortIndicator"), [setSortOrder]("GI.Gtk.Objects.TreeViewColumn#g:method:setSortOrder"), [setSpacing]("GI.Gtk.Objects.TreeViewColumn#g:method:setSpacing"), [setTitle]("GI.Gtk.Objects.TreeViewColumn#g:method:setTitle"), [setVisible]("GI.Gtk.Objects.TreeViewColumn#g:method:setVisible"), [setWidget]("GI.Gtk.Objects.TreeViewColumn#g:method:setWidget").

#if defined(ENABLE_OVERLOADING)
    ResolveTreeViewColumnMethod             ,
#endif

-- ** addAttribute #method:addAttribute#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnAddAttributeMethodInfo    ,
#endif
    treeViewColumnAddAttribute              ,


-- ** cellGetPosition #method:cellGetPosition#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnCellGetPositionMethodInfo ,
#endif
    treeViewColumnCellGetPosition           ,


-- ** cellGetSize #method:cellGetSize#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnCellGetSizeMethodInfo     ,
#endif
    treeViewColumnCellGetSize               ,


-- ** cellIsVisible #method:cellIsVisible#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnCellIsVisibleMethodInfo   ,
#endif
    treeViewColumnCellIsVisible             ,


-- ** cellSetCellData #method:cellSetCellData#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnCellSetCellDataMethodInfo ,
#endif
    treeViewColumnCellSetCellData           ,


-- ** clear #method:clear#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnClearMethodInfo           ,
#endif
    treeViewColumnClear                     ,


-- ** clearAttributes #method:clearAttributes#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnClearAttributesMethodInfo ,
#endif
    treeViewColumnClearAttributes           ,


-- ** clicked #method:clicked#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnClickedMethodInfo         ,
#endif
    treeViewColumnClicked                   ,


-- ** focusCell #method:focusCell#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnFocusCellMethodInfo       ,
#endif
    treeViewColumnFocusCell                 ,


-- ** getAlignment #method:getAlignment#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnGetAlignmentMethodInfo    ,
#endif
    treeViewColumnGetAlignment              ,


-- ** getButton #method:getButton#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnGetButtonMethodInfo       ,
#endif
    treeViewColumnGetButton                 ,


-- ** getClickable #method:getClickable#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnGetClickableMethodInfo    ,
#endif
    treeViewColumnGetClickable              ,


-- ** getExpand #method:getExpand#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnGetExpandMethodInfo       ,
#endif
    treeViewColumnGetExpand                 ,


-- ** getFixedWidth #method:getFixedWidth#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnGetFixedWidthMethodInfo   ,
#endif
    treeViewColumnGetFixedWidth             ,


-- ** getMaxWidth #method:getMaxWidth#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnGetMaxWidthMethodInfo     ,
#endif
    treeViewColumnGetMaxWidth               ,


-- ** getMinWidth #method:getMinWidth#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnGetMinWidthMethodInfo     ,
#endif
    treeViewColumnGetMinWidth               ,


-- ** getReorderable #method:getReorderable#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnGetReorderableMethodInfo  ,
#endif
    treeViewColumnGetReorderable            ,


-- ** getResizable #method:getResizable#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnGetResizableMethodInfo    ,
#endif
    treeViewColumnGetResizable              ,


-- ** getSizing #method:getSizing#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnGetSizingMethodInfo       ,
#endif
    treeViewColumnGetSizing                 ,


-- ** getSortColumnId #method:getSortColumnId#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnGetSortColumnIdMethodInfo ,
#endif
    treeViewColumnGetSortColumnId           ,


-- ** getSortIndicator #method:getSortIndicator#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnGetSortIndicatorMethodInfo,
#endif
    treeViewColumnGetSortIndicator          ,


-- ** getSortOrder #method:getSortOrder#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnGetSortOrderMethodInfo    ,
#endif
    treeViewColumnGetSortOrder              ,


-- ** getSpacing #method:getSpacing#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnGetSpacingMethodInfo      ,
#endif
    treeViewColumnGetSpacing                ,


-- ** getTitle #method:getTitle#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnGetTitleMethodInfo        ,
#endif
    treeViewColumnGetTitle                  ,


-- ** getTreeView #method:getTreeView#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnGetTreeViewMethodInfo     ,
#endif
    treeViewColumnGetTreeView               ,


-- ** getVisible #method:getVisible#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnGetVisibleMethodInfo      ,
#endif
    treeViewColumnGetVisible                ,


-- ** getWidget #method:getWidget#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnGetWidgetMethodInfo       ,
#endif
    treeViewColumnGetWidget                 ,


-- ** getWidth #method:getWidth#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnGetWidthMethodInfo        ,
#endif
    treeViewColumnGetWidth                  ,


-- ** getXOffset #method:getXOffset#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnGetXOffsetMethodInfo      ,
#endif
    treeViewColumnGetXOffset                ,


-- ** new #method:new#

    treeViewColumnNew                       ,


-- ** newWithArea #method:newWithArea#

    treeViewColumnNewWithArea               ,


-- ** packEnd #method:packEnd#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnPackEndMethodInfo         ,
#endif
    treeViewColumnPackEnd                   ,


-- ** packStart #method:packStart#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnPackStartMethodInfo       ,
#endif
    treeViewColumnPackStart                 ,


-- ** queueResize #method:queueResize#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnQueueResizeMethodInfo     ,
#endif
    treeViewColumnQueueResize               ,


-- ** setAlignment #method:setAlignment#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnSetAlignmentMethodInfo    ,
#endif
    treeViewColumnSetAlignment              ,


-- ** setCellDataFunc #method:setCellDataFunc#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnSetCellDataFuncMethodInfo ,
#endif
    treeViewColumnSetCellDataFunc           ,


-- ** setClickable #method:setClickable#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnSetClickableMethodInfo    ,
#endif
    treeViewColumnSetClickable              ,


-- ** setExpand #method:setExpand#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnSetExpandMethodInfo       ,
#endif
    treeViewColumnSetExpand                 ,


-- ** setFixedWidth #method:setFixedWidth#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnSetFixedWidthMethodInfo   ,
#endif
    treeViewColumnSetFixedWidth             ,


-- ** setMaxWidth #method:setMaxWidth#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnSetMaxWidthMethodInfo     ,
#endif
    treeViewColumnSetMaxWidth               ,


-- ** setMinWidth #method:setMinWidth#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnSetMinWidthMethodInfo     ,
#endif
    treeViewColumnSetMinWidth               ,


-- ** setReorderable #method:setReorderable#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnSetReorderableMethodInfo  ,
#endif
    treeViewColumnSetReorderable            ,


-- ** setResizable #method:setResizable#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnSetResizableMethodInfo    ,
#endif
    treeViewColumnSetResizable              ,


-- ** setSizing #method:setSizing#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnSetSizingMethodInfo       ,
#endif
    treeViewColumnSetSizing                 ,


-- ** setSortColumnId #method:setSortColumnId#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnSetSortColumnIdMethodInfo ,
#endif
    treeViewColumnSetSortColumnId           ,


-- ** setSortIndicator #method:setSortIndicator#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnSetSortIndicatorMethodInfo,
#endif
    treeViewColumnSetSortIndicator          ,


-- ** setSortOrder #method:setSortOrder#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnSetSortOrderMethodInfo    ,
#endif
    treeViewColumnSetSortOrder              ,


-- ** setSpacing #method:setSpacing#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnSetSpacingMethodInfo      ,
#endif
    treeViewColumnSetSpacing                ,


-- ** setTitle #method:setTitle#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnSetTitleMethodInfo        ,
#endif
    treeViewColumnSetTitle                  ,


-- ** setVisible #method:setVisible#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnSetVisibleMethodInfo      ,
#endif
    treeViewColumnSetVisible                ,


-- ** setWidget #method:setWidget#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnSetWidgetMethodInfo       ,
#endif
    treeViewColumnSetWidget                 ,




 -- * Properties


-- ** alignment #attr:alignment#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnAlignmentPropertyInfo     ,
#endif
    constructTreeViewColumnAlignment        ,
    getTreeViewColumnAlignment              ,
    setTreeViewColumnAlignment              ,
#if defined(ENABLE_OVERLOADING)
    treeViewColumnAlignment                 ,
#endif


-- ** cellArea #attr:cellArea#
-- | The t'GI.Gtk.Objects.CellArea.CellArea' used to layout cell renderers for this column.
-- 
-- If no area is specified when creating the tree view column with 'GI.Gtk.Objects.TreeViewColumn.treeViewColumnNewWithArea'
-- a horizontally oriented t'GI.Gtk.Objects.CellAreaBox.CellAreaBox' will be used.
-- 
-- /Since: 3.0/

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnCellAreaPropertyInfo      ,
#endif
    constructTreeViewColumnCellArea         ,
    getTreeViewColumnCellArea               ,
#if defined(ENABLE_OVERLOADING)
    treeViewColumnCellArea                  ,
#endif


-- ** clickable #attr:clickable#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnClickablePropertyInfo     ,
#endif
    constructTreeViewColumnClickable        ,
    getTreeViewColumnClickable              ,
    setTreeViewColumnClickable              ,
#if defined(ENABLE_OVERLOADING)
    treeViewColumnClickable                 ,
#endif


-- ** expand #attr:expand#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnExpandPropertyInfo        ,
#endif
    constructTreeViewColumnExpand           ,
    getTreeViewColumnExpand                 ,
    setTreeViewColumnExpand                 ,
#if defined(ENABLE_OVERLOADING)
    treeViewColumnExpand                    ,
#endif


-- ** fixedWidth #attr:fixedWidth#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnFixedWidthPropertyInfo    ,
#endif
    constructTreeViewColumnFixedWidth       ,
    getTreeViewColumnFixedWidth             ,
    setTreeViewColumnFixedWidth             ,
#if defined(ENABLE_OVERLOADING)
    treeViewColumnFixedWidth                ,
#endif


-- ** maxWidth #attr:maxWidth#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnMaxWidthPropertyInfo      ,
#endif
    constructTreeViewColumnMaxWidth         ,
    getTreeViewColumnMaxWidth               ,
    setTreeViewColumnMaxWidth               ,
#if defined(ENABLE_OVERLOADING)
    treeViewColumnMaxWidth                  ,
#endif


-- ** minWidth #attr:minWidth#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnMinWidthPropertyInfo      ,
#endif
    constructTreeViewColumnMinWidth         ,
    getTreeViewColumnMinWidth               ,
    setTreeViewColumnMinWidth               ,
#if defined(ENABLE_OVERLOADING)
    treeViewColumnMinWidth                  ,
#endif


-- ** reorderable #attr:reorderable#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnReorderablePropertyInfo   ,
#endif
    constructTreeViewColumnReorderable      ,
    getTreeViewColumnReorderable            ,
    setTreeViewColumnReorderable            ,
#if defined(ENABLE_OVERLOADING)
    treeViewColumnReorderable               ,
#endif


-- ** resizable #attr:resizable#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnResizablePropertyInfo     ,
#endif
    constructTreeViewColumnResizable        ,
    getTreeViewColumnResizable              ,
    setTreeViewColumnResizable              ,
#if defined(ENABLE_OVERLOADING)
    treeViewColumnResizable                 ,
#endif


-- ** sizing #attr:sizing#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnSizingPropertyInfo        ,
#endif
    constructTreeViewColumnSizing           ,
    getTreeViewColumnSizing                 ,
    setTreeViewColumnSizing                 ,
#if defined(ENABLE_OVERLOADING)
    treeViewColumnSizing                    ,
#endif


-- ** sortColumnId #attr:sortColumnId#
-- | Logical sort column ID this column sorts on when selected for sorting. Setting the sort column ID makes the column header
-- clickable. Set to -1 to make the column unsortable.
-- 
-- /Since: 2.18/

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnSortColumnIdPropertyInfo  ,
#endif
    constructTreeViewColumnSortColumnId     ,
    getTreeViewColumnSortColumnId           ,
    setTreeViewColumnSortColumnId           ,
#if defined(ENABLE_OVERLOADING)
    treeViewColumnSortColumnId              ,
#endif


-- ** sortIndicator #attr:sortIndicator#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnSortIndicatorPropertyInfo ,
#endif
    constructTreeViewColumnSortIndicator    ,
    getTreeViewColumnSortIndicator          ,
    setTreeViewColumnSortIndicator          ,
#if defined(ENABLE_OVERLOADING)
    treeViewColumnSortIndicator             ,
#endif


-- ** sortOrder #attr:sortOrder#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnSortOrderPropertyInfo     ,
#endif
    constructTreeViewColumnSortOrder        ,
    getTreeViewColumnSortOrder              ,
    setTreeViewColumnSortOrder              ,
#if defined(ENABLE_OVERLOADING)
    treeViewColumnSortOrder                 ,
#endif


-- ** spacing #attr:spacing#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnSpacingPropertyInfo       ,
#endif
    constructTreeViewColumnSpacing          ,
    getTreeViewColumnSpacing                ,
    setTreeViewColumnSpacing                ,
#if defined(ENABLE_OVERLOADING)
    treeViewColumnSpacing                   ,
#endif


-- ** title #attr:title#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnTitlePropertyInfo         ,
#endif
    constructTreeViewColumnTitle            ,
    getTreeViewColumnTitle                  ,
    setTreeViewColumnTitle                  ,
#if defined(ENABLE_OVERLOADING)
    treeViewColumnTitle                     ,
#endif


-- ** visible #attr:visible#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnVisiblePropertyInfo       ,
#endif
    constructTreeViewColumnVisible          ,
    getTreeViewColumnVisible                ,
    setTreeViewColumnVisible                ,
#if defined(ENABLE_OVERLOADING)
    treeViewColumnVisible                   ,
#endif


-- ** widget #attr:widget#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnWidgetPropertyInfo        ,
#endif
    clearTreeViewColumnWidget               ,
    constructTreeViewColumnWidget           ,
    getTreeViewColumnWidget                 ,
    setTreeViewColumnWidget                 ,
#if defined(ENABLE_OVERLOADING)
    treeViewColumnWidget                    ,
#endif


-- ** width #attr:width#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnWidthPropertyInfo         ,
#endif
    getTreeViewColumnWidth                  ,
#if defined(ENABLE_OVERLOADING)
    treeViewColumnWidth                     ,
#endif


-- ** xOffset #attr:xOffset#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnXOffsetPropertyInfo       ,
#endif
    getTreeViewColumnXOffset                ,
#if defined(ENABLE_OVERLOADING)
    treeViewColumnXOffset                   ,
#endif




 -- * Signals


-- ** clicked #signal:clicked#

    TreeViewColumnClickedCallback           ,
#if defined(ENABLE_OVERLOADING)
    TreeViewColumnClickedSignalInfo         ,
#endif
    afterTreeViewColumnClicked              ,
    onTreeViewColumnClicked                 ,




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

import qualified GI.GLib.Callbacks as GLib.Callbacks
import qualified GI.GObject.Objects.Object as GObject.Object
import qualified GI.Gdk.Structs.Rectangle as Gdk.Rectangle
import qualified GI.Gtk.Callbacks as Gtk.Callbacks
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.CellLayout as Gtk.CellLayout
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.TreeModel as Gtk.TreeModel
import {-# SOURCE #-} qualified GI.Gtk.Objects.CellArea as Gtk.CellArea
import {-# SOURCE #-} qualified GI.Gtk.Objects.CellRenderer as Gtk.CellRenderer
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget
import {-# SOURCE #-} qualified GI.Gtk.Structs.TreeIter as Gtk.TreeIter

-- | Memory-managed wrapper type.
newtype TreeViewColumn = TreeViewColumn (SP.ManagedPtr TreeViewColumn)
    deriving (Eq)

instance SP.ManagedPtrNewtype TreeViewColumn where
    toManagedPtr (TreeViewColumn p) = p

foreign import ccall "gtk_tree_view_column_get_type"
    c_gtk_tree_view_column_get_type :: IO B.Types.GType

instance B.Types.TypedObject TreeViewColumn where
    glibType = c_gtk_tree_view_column_get_type

instance B.Types.GObject TreeViewColumn

-- | Type class for types which can be safely cast to `TreeViewColumn`, for instance with `toTreeViewColumn`.
class (SP.GObject o, O.IsDescendantOf TreeViewColumn o) => IsTreeViewColumn o
instance (SP.GObject o, O.IsDescendantOf TreeViewColumn o) => IsTreeViewColumn o

instance O.HasParentTypes TreeViewColumn
type instance O.ParentTypes TreeViewColumn = '[GObject.Object.Object, Gtk.Buildable.Buildable, Gtk.CellLayout.CellLayout]

-- | Cast to `TreeViewColumn`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toTreeViewColumn :: (MIO.MonadIO m, IsTreeViewColumn o) => o -> m TreeViewColumn
toTreeViewColumn = MIO.liftIO . B.ManagedPtr.unsafeCastTo TreeViewColumn

-- | Convert 'TreeViewColumn' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe TreeViewColumn) where
    gvalueGType_ = c_gtk_tree_view_column_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr TreeViewColumn)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr TreeViewColumn)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject TreeViewColumn ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveTreeViewColumnMethod (t :: Symbol) (o :: *) :: * where
    ResolveTreeViewColumnMethod "addAttribute" o = TreeViewColumnAddAttributeMethodInfo
    ResolveTreeViewColumnMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveTreeViewColumnMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveTreeViewColumnMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveTreeViewColumnMethod "cellGetPosition" o = TreeViewColumnCellGetPositionMethodInfo
    ResolveTreeViewColumnMethod "cellGetSize" o = TreeViewColumnCellGetSizeMethodInfo
    ResolveTreeViewColumnMethod "cellIsVisible" o = TreeViewColumnCellIsVisibleMethodInfo
    ResolveTreeViewColumnMethod "cellSetCellData" o = TreeViewColumnCellSetCellDataMethodInfo
    ResolveTreeViewColumnMethod "clear" o = TreeViewColumnClearMethodInfo
    ResolveTreeViewColumnMethod "clearAttributes" o = TreeViewColumnClearAttributesMethodInfo
    ResolveTreeViewColumnMethod "clicked" o = TreeViewColumnClickedMethodInfo
    ResolveTreeViewColumnMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveTreeViewColumnMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveTreeViewColumnMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveTreeViewColumnMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveTreeViewColumnMethod "focusCell" o = TreeViewColumnFocusCellMethodInfo
    ResolveTreeViewColumnMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveTreeViewColumnMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveTreeViewColumnMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveTreeViewColumnMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveTreeViewColumnMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveTreeViewColumnMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveTreeViewColumnMethod "packEnd" o = TreeViewColumnPackEndMethodInfo
    ResolveTreeViewColumnMethod "packStart" o = TreeViewColumnPackStartMethodInfo
    ResolveTreeViewColumnMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveTreeViewColumnMethod "queueResize" o = TreeViewColumnQueueResizeMethodInfo
    ResolveTreeViewColumnMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveTreeViewColumnMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveTreeViewColumnMethod "reorder" o = Gtk.CellLayout.CellLayoutReorderMethodInfo
    ResolveTreeViewColumnMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveTreeViewColumnMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveTreeViewColumnMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveTreeViewColumnMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveTreeViewColumnMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveTreeViewColumnMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveTreeViewColumnMethod "getAlignment" o = TreeViewColumnGetAlignmentMethodInfo
    ResolveTreeViewColumnMethod "getArea" o = Gtk.CellLayout.CellLayoutGetAreaMethodInfo
    ResolveTreeViewColumnMethod "getButton" o = TreeViewColumnGetButtonMethodInfo
    ResolveTreeViewColumnMethod "getCells" o = Gtk.CellLayout.CellLayoutGetCellsMethodInfo
    ResolveTreeViewColumnMethod "getClickable" o = TreeViewColumnGetClickableMethodInfo
    ResolveTreeViewColumnMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveTreeViewColumnMethod "getExpand" o = TreeViewColumnGetExpandMethodInfo
    ResolveTreeViewColumnMethod "getFixedWidth" o = TreeViewColumnGetFixedWidthMethodInfo
    ResolveTreeViewColumnMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveTreeViewColumnMethod "getMaxWidth" o = TreeViewColumnGetMaxWidthMethodInfo
    ResolveTreeViewColumnMethod "getMinWidth" o = TreeViewColumnGetMinWidthMethodInfo
    ResolveTreeViewColumnMethod "getName" o = Gtk.Buildable.BuildableGetNameMethodInfo
    ResolveTreeViewColumnMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveTreeViewColumnMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveTreeViewColumnMethod "getReorderable" o = TreeViewColumnGetReorderableMethodInfo
    ResolveTreeViewColumnMethod "getResizable" o = TreeViewColumnGetResizableMethodInfo
    ResolveTreeViewColumnMethod "getSizing" o = TreeViewColumnGetSizingMethodInfo
    ResolveTreeViewColumnMethod "getSortColumnId" o = TreeViewColumnGetSortColumnIdMethodInfo
    ResolveTreeViewColumnMethod "getSortIndicator" o = TreeViewColumnGetSortIndicatorMethodInfo
    ResolveTreeViewColumnMethod "getSortOrder" o = TreeViewColumnGetSortOrderMethodInfo
    ResolveTreeViewColumnMethod "getSpacing" o = TreeViewColumnGetSpacingMethodInfo
    ResolveTreeViewColumnMethod "getTitle" o = TreeViewColumnGetTitleMethodInfo
    ResolveTreeViewColumnMethod "getTreeView" o = TreeViewColumnGetTreeViewMethodInfo
    ResolveTreeViewColumnMethod "getVisible" o = TreeViewColumnGetVisibleMethodInfo
    ResolveTreeViewColumnMethod "getWidget" o = TreeViewColumnGetWidgetMethodInfo
    ResolveTreeViewColumnMethod "getWidth" o = TreeViewColumnGetWidthMethodInfo
    ResolveTreeViewColumnMethod "getXOffset" o = TreeViewColumnGetXOffsetMethodInfo
    ResolveTreeViewColumnMethod "setAlignment" o = TreeViewColumnSetAlignmentMethodInfo
    ResolveTreeViewColumnMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveTreeViewColumnMethod "setCellDataFunc" o = TreeViewColumnSetCellDataFuncMethodInfo
    ResolveTreeViewColumnMethod "setClickable" o = TreeViewColumnSetClickableMethodInfo
    ResolveTreeViewColumnMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveTreeViewColumnMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveTreeViewColumnMethod "setExpand" o = TreeViewColumnSetExpandMethodInfo
    ResolveTreeViewColumnMethod "setFixedWidth" o = TreeViewColumnSetFixedWidthMethodInfo
    ResolveTreeViewColumnMethod "setMaxWidth" o = TreeViewColumnSetMaxWidthMethodInfo
    ResolveTreeViewColumnMethod "setMinWidth" o = TreeViewColumnSetMinWidthMethodInfo
    ResolveTreeViewColumnMethod "setName" o = Gtk.Buildable.BuildableSetNameMethodInfo
    ResolveTreeViewColumnMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveTreeViewColumnMethod "setReorderable" o = TreeViewColumnSetReorderableMethodInfo
    ResolveTreeViewColumnMethod "setResizable" o = TreeViewColumnSetResizableMethodInfo
    ResolveTreeViewColumnMethod "setSizing" o = TreeViewColumnSetSizingMethodInfo
    ResolveTreeViewColumnMethod "setSortColumnId" o = TreeViewColumnSetSortColumnIdMethodInfo
    ResolveTreeViewColumnMethod "setSortIndicator" o = TreeViewColumnSetSortIndicatorMethodInfo
    ResolveTreeViewColumnMethod "setSortOrder" o = TreeViewColumnSetSortOrderMethodInfo
    ResolveTreeViewColumnMethod "setSpacing" o = TreeViewColumnSetSpacingMethodInfo
    ResolveTreeViewColumnMethod "setTitle" o = TreeViewColumnSetTitleMethodInfo
    ResolveTreeViewColumnMethod "setVisible" o = TreeViewColumnSetVisibleMethodInfo
    ResolveTreeViewColumnMethod "setWidget" o = TreeViewColumnSetWidgetMethodInfo
    ResolveTreeViewColumnMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveTreeViewColumnMethod t TreeViewColumn, O.OverloadedMethod info TreeViewColumn p) => OL.IsLabel t (TreeViewColumn -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveTreeViewColumnMethod t TreeViewColumn, O.OverloadedMethod info TreeViewColumn p, R.HasField t TreeViewColumn p) => R.HasField t TreeViewColumn p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveTreeViewColumnMethod t TreeViewColumn, O.OverloadedMethodInfo info TreeViewColumn) => OL.IsLabel t (O.MethodProxy info TreeViewColumn) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal TreeViewColumn::clicked
-- | /No description available in the introspection data./
type TreeViewColumnClickedCallback =
    IO ()

type C_TreeViewColumnClickedCallback =
    Ptr TreeViewColumn ->                   -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TreeViewColumnClickedCallback`.
foreign import ccall "wrapper"
    mk_TreeViewColumnClickedCallback :: C_TreeViewColumnClickedCallback -> IO (FunPtr C_TreeViewColumnClickedCallback)

wrap_TreeViewColumnClickedCallback :: 
    GObject a => (a -> TreeViewColumnClickedCallback) ->
    C_TreeViewColumnClickedCallback
wrap_TreeViewColumnClickedCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [clicked](#signal:clicked) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' treeViewColumn #clicked callback
-- @
-- 
-- 
onTreeViewColumnClicked :: (IsTreeViewColumn a, MonadIO m) => a -> ((?self :: a) => TreeViewColumnClickedCallback) -> m SignalHandlerId
onTreeViewColumnClicked obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeViewColumnClickedCallback wrapped
    wrapped'' <- mk_TreeViewColumnClickedCallback wrapped'
    connectSignalFunPtr obj "clicked" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [clicked](#signal:clicked) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' treeViewColumn #clicked callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTreeViewColumnClicked :: (IsTreeViewColumn a, MonadIO m) => a -> ((?self :: a) => TreeViewColumnClickedCallback) -> m SignalHandlerId
afterTreeViewColumnClicked obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeViewColumnClickedCallback wrapped
    wrapped'' <- mk_TreeViewColumnClickedCallback wrapped'
    connectSignalFunPtr obj "clicked" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TreeViewColumnClickedSignalInfo
instance SignalInfo TreeViewColumnClickedSignalInfo where
    type HaskellCallbackType TreeViewColumnClickedSignalInfo = TreeViewColumnClickedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TreeViewColumnClickedCallback cb
        cb'' <- mk_TreeViewColumnClickedCallback cb'
        connectSignalFunPtr obj "clicked" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn::clicked"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#g:signal:clicked"})

#endif

-- VVV Prop "alignment"
   -- Type: TBasicType TFloat
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@alignment@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeViewColumn #alignment
-- @
getTreeViewColumnAlignment :: (MonadIO m, IsTreeViewColumn o) => o -> m Float
getTreeViewColumnAlignment obj = MIO.liftIO $ B.Properties.getObjectPropertyFloat obj "alignment"

-- | Set the value of the “@alignment@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeViewColumn [ #alignment 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeViewColumnAlignment :: (MonadIO m, IsTreeViewColumn o) => o -> Float -> m ()
setTreeViewColumnAlignment obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyFloat obj "alignment" val

-- | Construct a `GValueConstruct` with valid value for the “@alignment@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewColumnAlignment :: (IsTreeViewColumn o, MIO.MonadIO m) => Float -> m (GValueConstruct o)
constructTreeViewColumnAlignment val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyFloat "alignment" val

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnAlignmentPropertyInfo
instance AttrInfo TreeViewColumnAlignmentPropertyInfo where
    type AttrAllowedOps TreeViewColumnAlignmentPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TreeViewColumnAlignmentPropertyInfo = IsTreeViewColumn
    type AttrSetTypeConstraint TreeViewColumnAlignmentPropertyInfo = (~) Float
    type AttrTransferTypeConstraint TreeViewColumnAlignmentPropertyInfo = (~) Float
    type AttrTransferType TreeViewColumnAlignmentPropertyInfo = Float
    type AttrGetType TreeViewColumnAlignmentPropertyInfo = Float
    type AttrLabel TreeViewColumnAlignmentPropertyInfo = "alignment"
    type AttrOrigin TreeViewColumnAlignmentPropertyInfo = TreeViewColumn
    attrGet = getTreeViewColumnAlignment
    attrSet = setTreeViewColumnAlignment
    attrTransfer _ v = do
        return v
    attrConstruct = constructTreeViewColumnAlignment
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.alignment"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#g:attr:alignment"
        })
#endif

-- VVV Prop "cell-area"
   -- Type: TInterface (Name {namespace = "Gtk", name = "CellArea"})
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstructOnly]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@cell-area@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeViewColumn #cellArea
-- @
getTreeViewColumnCellArea :: (MonadIO m, IsTreeViewColumn o) => o -> m (Maybe Gtk.CellArea.CellArea)
getTreeViewColumnCellArea obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "cell-area" Gtk.CellArea.CellArea

-- | Construct a `GValueConstruct` with valid value for the “@cell-area@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewColumnCellArea :: (IsTreeViewColumn o, MIO.MonadIO m, Gtk.CellArea.IsCellArea a) => a -> m (GValueConstruct o)
constructTreeViewColumnCellArea val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "cell-area" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnCellAreaPropertyInfo
instance AttrInfo TreeViewColumnCellAreaPropertyInfo where
    type AttrAllowedOps TreeViewColumnCellAreaPropertyInfo = '[ 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint TreeViewColumnCellAreaPropertyInfo = IsTreeViewColumn
    type AttrSetTypeConstraint TreeViewColumnCellAreaPropertyInfo = Gtk.CellArea.IsCellArea
    type AttrTransferTypeConstraint TreeViewColumnCellAreaPropertyInfo = Gtk.CellArea.IsCellArea
    type AttrTransferType TreeViewColumnCellAreaPropertyInfo = Gtk.CellArea.CellArea
    type AttrGetType TreeViewColumnCellAreaPropertyInfo = (Maybe Gtk.CellArea.CellArea)
    type AttrLabel TreeViewColumnCellAreaPropertyInfo = "cell-area"
    type AttrOrigin TreeViewColumnCellAreaPropertyInfo = TreeViewColumn
    attrGet = getTreeViewColumnCellArea
    attrSet = undefined
    attrTransfer _ v = do
        unsafeCastTo Gtk.CellArea.CellArea v
    attrConstruct = constructTreeViewColumnCellArea
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.cellArea"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#g:attr:cellArea"
        })
#endif

-- VVV Prop "clickable"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@clickable@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeViewColumn #clickable
-- @
getTreeViewColumnClickable :: (MonadIO m, IsTreeViewColumn o) => o -> m Bool
getTreeViewColumnClickable obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "clickable"

-- | Set the value of the “@clickable@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeViewColumn [ #clickable 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeViewColumnClickable :: (MonadIO m, IsTreeViewColumn o) => o -> Bool -> m ()
setTreeViewColumnClickable obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "clickable" val

-- | Construct a `GValueConstruct` with valid value for the “@clickable@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewColumnClickable :: (IsTreeViewColumn o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTreeViewColumnClickable val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "clickable" val

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnClickablePropertyInfo
instance AttrInfo TreeViewColumnClickablePropertyInfo where
    type AttrAllowedOps TreeViewColumnClickablePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TreeViewColumnClickablePropertyInfo = IsTreeViewColumn
    type AttrSetTypeConstraint TreeViewColumnClickablePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TreeViewColumnClickablePropertyInfo = (~) Bool
    type AttrTransferType TreeViewColumnClickablePropertyInfo = Bool
    type AttrGetType TreeViewColumnClickablePropertyInfo = Bool
    type AttrLabel TreeViewColumnClickablePropertyInfo = "clickable"
    type AttrOrigin TreeViewColumnClickablePropertyInfo = TreeViewColumn
    attrGet = getTreeViewColumnClickable
    attrSet = setTreeViewColumnClickable
    attrTransfer _ v = do
        return v
    attrConstruct = constructTreeViewColumnClickable
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.clickable"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#g:attr:clickable"
        })
#endif

-- VVV Prop "expand"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@expand@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeViewColumn #expand
-- @
getTreeViewColumnExpand :: (MonadIO m, IsTreeViewColumn o) => o -> m Bool
getTreeViewColumnExpand obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "expand"

-- | Set the value of the “@expand@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeViewColumn [ #expand 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeViewColumnExpand :: (MonadIO m, IsTreeViewColumn o) => o -> Bool -> m ()
setTreeViewColumnExpand obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "expand" val

-- | Construct a `GValueConstruct` with valid value for the “@expand@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewColumnExpand :: (IsTreeViewColumn o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTreeViewColumnExpand val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "expand" val

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnExpandPropertyInfo
instance AttrInfo TreeViewColumnExpandPropertyInfo where
    type AttrAllowedOps TreeViewColumnExpandPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TreeViewColumnExpandPropertyInfo = IsTreeViewColumn
    type AttrSetTypeConstraint TreeViewColumnExpandPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TreeViewColumnExpandPropertyInfo = (~) Bool
    type AttrTransferType TreeViewColumnExpandPropertyInfo = Bool
    type AttrGetType TreeViewColumnExpandPropertyInfo = Bool
    type AttrLabel TreeViewColumnExpandPropertyInfo = "expand"
    type AttrOrigin TreeViewColumnExpandPropertyInfo = TreeViewColumn
    attrGet = getTreeViewColumnExpand
    attrSet = setTreeViewColumnExpand
    attrTransfer _ v = do
        return v
    attrConstruct = constructTreeViewColumnExpand
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.expand"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#g:attr:expand"
        })
#endif

-- VVV Prop "fixed-width"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@fixed-width@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeViewColumn #fixedWidth
-- @
getTreeViewColumnFixedWidth :: (MonadIO m, IsTreeViewColumn o) => o -> m Int32
getTreeViewColumnFixedWidth obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "fixed-width"

-- | Set the value of the “@fixed-width@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeViewColumn [ #fixedWidth 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeViewColumnFixedWidth :: (MonadIO m, IsTreeViewColumn o) => o -> Int32 -> m ()
setTreeViewColumnFixedWidth obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "fixed-width" val

-- | Construct a `GValueConstruct` with valid value for the “@fixed-width@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewColumnFixedWidth :: (IsTreeViewColumn o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructTreeViewColumnFixedWidth val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "fixed-width" val

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnFixedWidthPropertyInfo
instance AttrInfo TreeViewColumnFixedWidthPropertyInfo where
    type AttrAllowedOps TreeViewColumnFixedWidthPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TreeViewColumnFixedWidthPropertyInfo = IsTreeViewColumn
    type AttrSetTypeConstraint TreeViewColumnFixedWidthPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint TreeViewColumnFixedWidthPropertyInfo = (~) Int32
    type AttrTransferType TreeViewColumnFixedWidthPropertyInfo = Int32
    type AttrGetType TreeViewColumnFixedWidthPropertyInfo = Int32
    type AttrLabel TreeViewColumnFixedWidthPropertyInfo = "fixed-width"
    type AttrOrigin TreeViewColumnFixedWidthPropertyInfo = TreeViewColumn
    attrGet = getTreeViewColumnFixedWidth
    attrSet = setTreeViewColumnFixedWidth
    attrTransfer _ v = do
        return v
    attrConstruct = constructTreeViewColumnFixedWidth
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.fixedWidth"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#g:attr:fixedWidth"
        })
#endif

-- VVV Prop "max-width"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@max-width@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeViewColumn #maxWidth
-- @
getTreeViewColumnMaxWidth :: (MonadIO m, IsTreeViewColumn o) => o -> m Int32
getTreeViewColumnMaxWidth obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "max-width"

-- | Set the value of the “@max-width@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeViewColumn [ #maxWidth 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeViewColumnMaxWidth :: (MonadIO m, IsTreeViewColumn o) => o -> Int32 -> m ()
setTreeViewColumnMaxWidth obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "max-width" val

-- | Construct a `GValueConstruct` with valid value for the “@max-width@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewColumnMaxWidth :: (IsTreeViewColumn o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructTreeViewColumnMaxWidth val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "max-width" val

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnMaxWidthPropertyInfo
instance AttrInfo TreeViewColumnMaxWidthPropertyInfo where
    type AttrAllowedOps TreeViewColumnMaxWidthPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TreeViewColumnMaxWidthPropertyInfo = IsTreeViewColumn
    type AttrSetTypeConstraint TreeViewColumnMaxWidthPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint TreeViewColumnMaxWidthPropertyInfo = (~) Int32
    type AttrTransferType TreeViewColumnMaxWidthPropertyInfo = Int32
    type AttrGetType TreeViewColumnMaxWidthPropertyInfo = Int32
    type AttrLabel TreeViewColumnMaxWidthPropertyInfo = "max-width"
    type AttrOrigin TreeViewColumnMaxWidthPropertyInfo = TreeViewColumn
    attrGet = getTreeViewColumnMaxWidth
    attrSet = setTreeViewColumnMaxWidth
    attrTransfer _ v = do
        return v
    attrConstruct = constructTreeViewColumnMaxWidth
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.maxWidth"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#g:attr:maxWidth"
        })
#endif

-- VVV Prop "min-width"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@min-width@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeViewColumn #minWidth
-- @
getTreeViewColumnMinWidth :: (MonadIO m, IsTreeViewColumn o) => o -> m Int32
getTreeViewColumnMinWidth obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "min-width"

-- | Set the value of the “@min-width@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeViewColumn [ #minWidth 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeViewColumnMinWidth :: (MonadIO m, IsTreeViewColumn o) => o -> Int32 -> m ()
setTreeViewColumnMinWidth obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "min-width" val

-- | Construct a `GValueConstruct` with valid value for the “@min-width@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewColumnMinWidth :: (IsTreeViewColumn o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructTreeViewColumnMinWidth val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "min-width" val

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnMinWidthPropertyInfo
instance AttrInfo TreeViewColumnMinWidthPropertyInfo where
    type AttrAllowedOps TreeViewColumnMinWidthPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TreeViewColumnMinWidthPropertyInfo = IsTreeViewColumn
    type AttrSetTypeConstraint TreeViewColumnMinWidthPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint TreeViewColumnMinWidthPropertyInfo = (~) Int32
    type AttrTransferType TreeViewColumnMinWidthPropertyInfo = Int32
    type AttrGetType TreeViewColumnMinWidthPropertyInfo = Int32
    type AttrLabel TreeViewColumnMinWidthPropertyInfo = "min-width"
    type AttrOrigin TreeViewColumnMinWidthPropertyInfo = TreeViewColumn
    attrGet = getTreeViewColumnMinWidth
    attrSet = setTreeViewColumnMinWidth
    attrTransfer _ v = do
        return v
    attrConstruct = constructTreeViewColumnMinWidth
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.minWidth"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#g:attr:minWidth"
        })
#endif

-- VVV Prop "reorderable"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@reorderable@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeViewColumn #reorderable
-- @
getTreeViewColumnReorderable :: (MonadIO m, IsTreeViewColumn o) => o -> m Bool
getTreeViewColumnReorderable obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "reorderable"

-- | Set the value of the “@reorderable@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeViewColumn [ #reorderable 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeViewColumnReorderable :: (MonadIO m, IsTreeViewColumn o) => o -> Bool -> m ()
setTreeViewColumnReorderable obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "reorderable" val

-- | Construct a `GValueConstruct` with valid value for the “@reorderable@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewColumnReorderable :: (IsTreeViewColumn o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTreeViewColumnReorderable val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "reorderable" val

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnReorderablePropertyInfo
instance AttrInfo TreeViewColumnReorderablePropertyInfo where
    type AttrAllowedOps TreeViewColumnReorderablePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TreeViewColumnReorderablePropertyInfo = IsTreeViewColumn
    type AttrSetTypeConstraint TreeViewColumnReorderablePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TreeViewColumnReorderablePropertyInfo = (~) Bool
    type AttrTransferType TreeViewColumnReorderablePropertyInfo = Bool
    type AttrGetType TreeViewColumnReorderablePropertyInfo = Bool
    type AttrLabel TreeViewColumnReorderablePropertyInfo = "reorderable"
    type AttrOrigin TreeViewColumnReorderablePropertyInfo = TreeViewColumn
    attrGet = getTreeViewColumnReorderable
    attrSet = setTreeViewColumnReorderable
    attrTransfer _ v = do
        return v
    attrConstruct = constructTreeViewColumnReorderable
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.reorderable"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#g:attr:reorderable"
        })
#endif

-- VVV Prop "resizable"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@resizable@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeViewColumn #resizable
-- @
getTreeViewColumnResizable :: (MonadIO m, IsTreeViewColumn o) => o -> m Bool
getTreeViewColumnResizable obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "resizable"

-- | Set the value of the “@resizable@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeViewColumn [ #resizable 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeViewColumnResizable :: (MonadIO m, IsTreeViewColumn o) => o -> Bool -> m ()
setTreeViewColumnResizable obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "resizable" val

-- | Construct a `GValueConstruct` with valid value for the “@resizable@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewColumnResizable :: (IsTreeViewColumn o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTreeViewColumnResizable val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "resizable" val

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnResizablePropertyInfo
instance AttrInfo TreeViewColumnResizablePropertyInfo where
    type AttrAllowedOps TreeViewColumnResizablePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TreeViewColumnResizablePropertyInfo = IsTreeViewColumn
    type AttrSetTypeConstraint TreeViewColumnResizablePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TreeViewColumnResizablePropertyInfo = (~) Bool
    type AttrTransferType TreeViewColumnResizablePropertyInfo = Bool
    type AttrGetType TreeViewColumnResizablePropertyInfo = Bool
    type AttrLabel TreeViewColumnResizablePropertyInfo = "resizable"
    type AttrOrigin TreeViewColumnResizablePropertyInfo = TreeViewColumn
    attrGet = getTreeViewColumnResizable
    attrSet = setTreeViewColumnResizable
    attrTransfer _ v = do
        return v
    attrConstruct = constructTreeViewColumnResizable
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.resizable"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#g:attr:resizable"
        })
#endif

-- VVV Prop "sizing"
   -- Type: TInterface (Name {namespace = "Gtk", name = "TreeViewColumnSizing"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@sizing@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeViewColumn #sizing
-- @
getTreeViewColumnSizing :: (MonadIO m, IsTreeViewColumn o) => o -> m Gtk.Enums.TreeViewColumnSizing
getTreeViewColumnSizing obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "sizing"

-- | Set the value of the “@sizing@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeViewColumn [ #sizing 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeViewColumnSizing :: (MonadIO m, IsTreeViewColumn o) => o -> Gtk.Enums.TreeViewColumnSizing -> m ()
setTreeViewColumnSizing obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "sizing" val

-- | Construct a `GValueConstruct` with valid value for the “@sizing@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewColumnSizing :: (IsTreeViewColumn o, MIO.MonadIO m) => Gtk.Enums.TreeViewColumnSizing -> m (GValueConstruct o)
constructTreeViewColumnSizing val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "sizing" val

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSizingPropertyInfo
instance AttrInfo TreeViewColumnSizingPropertyInfo where
    type AttrAllowedOps TreeViewColumnSizingPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TreeViewColumnSizingPropertyInfo = IsTreeViewColumn
    type AttrSetTypeConstraint TreeViewColumnSizingPropertyInfo = (~) Gtk.Enums.TreeViewColumnSizing
    type AttrTransferTypeConstraint TreeViewColumnSizingPropertyInfo = (~) Gtk.Enums.TreeViewColumnSizing
    type AttrTransferType TreeViewColumnSizingPropertyInfo = Gtk.Enums.TreeViewColumnSizing
    type AttrGetType TreeViewColumnSizingPropertyInfo = Gtk.Enums.TreeViewColumnSizing
    type AttrLabel TreeViewColumnSizingPropertyInfo = "sizing"
    type AttrOrigin TreeViewColumnSizingPropertyInfo = TreeViewColumn
    attrGet = getTreeViewColumnSizing
    attrSet = setTreeViewColumnSizing
    attrTransfer _ v = do
        return v
    attrConstruct = constructTreeViewColumnSizing
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.sizing"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#g:attr:sizing"
        })
#endif

-- VVV Prop "sort-column-id"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@sort-column-id@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeViewColumn #sortColumnId
-- @
getTreeViewColumnSortColumnId :: (MonadIO m, IsTreeViewColumn o) => o -> m Int32
getTreeViewColumnSortColumnId obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "sort-column-id"

-- | Set the value of the “@sort-column-id@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeViewColumn [ #sortColumnId 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeViewColumnSortColumnId :: (MonadIO m, IsTreeViewColumn o) => o -> Int32 -> m ()
setTreeViewColumnSortColumnId obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "sort-column-id" val

-- | Construct a `GValueConstruct` with valid value for the “@sort-column-id@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewColumnSortColumnId :: (IsTreeViewColumn o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructTreeViewColumnSortColumnId val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "sort-column-id" val

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSortColumnIdPropertyInfo
instance AttrInfo TreeViewColumnSortColumnIdPropertyInfo where
    type AttrAllowedOps TreeViewColumnSortColumnIdPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TreeViewColumnSortColumnIdPropertyInfo = IsTreeViewColumn
    type AttrSetTypeConstraint TreeViewColumnSortColumnIdPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint TreeViewColumnSortColumnIdPropertyInfo = (~) Int32
    type AttrTransferType TreeViewColumnSortColumnIdPropertyInfo = Int32
    type AttrGetType TreeViewColumnSortColumnIdPropertyInfo = Int32
    type AttrLabel TreeViewColumnSortColumnIdPropertyInfo = "sort-column-id"
    type AttrOrigin TreeViewColumnSortColumnIdPropertyInfo = TreeViewColumn
    attrGet = getTreeViewColumnSortColumnId
    attrSet = setTreeViewColumnSortColumnId
    attrTransfer _ v = do
        return v
    attrConstruct = constructTreeViewColumnSortColumnId
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.sortColumnId"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#g:attr:sortColumnId"
        })
#endif

-- VVV Prop "sort-indicator"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@sort-indicator@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeViewColumn #sortIndicator
-- @
getTreeViewColumnSortIndicator :: (MonadIO m, IsTreeViewColumn o) => o -> m Bool
getTreeViewColumnSortIndicator obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "sort-indicator"

-- | Set the value of the “@sort-indicator@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeViewColumn [ #sortIndicator 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeViewColumnSortIndicator :: (MonadIO m, IsTreeViewColumn o) => o -> Bool -> m ()
setTreeViewColumnSortIndicator obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "sort-indicator" val

-- | Construct a `GValueConstruct` with valid value for the “@sort-indicator@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewColumnSortIndicator :: (IsTreeViewColumn o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTreeViewColumnSortIndicator val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "sort-indicator" val

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSortIndicatorPropertyInfo
instance AttrInfo TreeViewColumnSortIndicatorPropertyInfo where
    type AttrAllowedOps TreeViewColumnSortIndicatorPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TreeViewColumnSortIndicatorPropertyInfo = IsTreeViewColumn
    type AttrSetTypeConstraint TreeViewColumnSortIndicatorPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TreeViewColumnSortIndicatorPropertyInfo = (~) Bool
    type AttrTransferType TreeViewColumnSortIndicatorPropertyInfo = Bool
    type AttrGetType TreeViewColumnSortIndicatorPropertyInfo = Bool
    type AttrLabel TreeViewColumnSortIndicatorPropertyInfo = "sort-indicator"
    type AttrOrigin TreeViewColumnSortIndicatorPropertyInfo = TreeViewColumn
    attrGet = getTreeViewColumnSortIndicator
    attrSet = setTreeViewColumnSortIndicator
    attrTransfer _ v = do
        return v
    attrConstruct = constructTreeViewColumnSortIndicator
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.sortIndicator"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#g:attr:sortIndicator"
        })
#endif

-- VVV Prop "sort-order"
   -- Type: TInterface (Name {namespace = "Gtk", name = "SortType"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@sort-order@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeViewColumn #sortOrder
-- @
getTreeViewColumnSortOrder :: (MonadIO m, IsTreeViewColumn o) => o -> m Gtk.Enums.SortType
getTreeViewColumnSortOrder obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "sort-order"

-- | Set the value of the “@sort-order@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeViewColumn [ #sortOrder 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeViewColumnSortOrder :: (MonadIO m, IsTreeViewColumn o) => o -> Gtk.Enums.SortType -> m ()
setTreeViewColumnSortOrder obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "sort-order" val

-- | Construct a `GValueConstruct` with valid value for the “@sort-order@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewColumnSortOrder :: (IsTreeViewColumn o, MIO.MonadIO m) => Gtk.Enums.SortType -> m (GValueConstruct o)
constructTreeViewColumnSortOrder val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "sort-order" val

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSortOrderPropertyInfo
instance AttrInfo TreeViewColumnSortOrderPropertyInfo where
    type AttrAllowedOps TreeViewColumnSortOrderPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TreeViewColumnSortOrderPropertyInfo = IsTreeViewColumn
    type AttrSetTypeConstraint TreeViewColumnSortOrderPropertyInfo = (~) Gtk.Enums.SortType
    type AttrTransferTypeConstraint TreeViewColumnSortOrderPropertyInfo = (~) Gtk.Enums.SortType
    type AttrTransferType TreeViewColumnSortOrderPropertyInfo = Gtk.Enums.SortType
    type AttrGetType TreeViewColumnSortOrderPropertyInfo = Gtk.Enums.SortType
    type AttrLabel TreeViewColumnSortOrderPropertyInfo = "sort-order"
    type AttrOrigin TreeViewColumnSortOrderPropertyInfo = TreeViewColumn
    attrGet = getTreeViewColumnSortOrder
    attrSet = setTreeViewColumnSortOrder
    attrTransfer _ v = do
        return v
    attrConstruct = constructTreeViewColumnSortOrder
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.sortOrder"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#g:attr:sortOrder"
        })
#endif

-- VVV Prop "spacing"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@spacing@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeViewColumn #spacing
-- @
getTreeViewColumnSpacing :: (MonadIO m, IsTreeViewColumn o) => o -> m Int32
getTreeViewColumnSpacing obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "spacing"

-- | Set the value of the “@spacing@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeViewColumn [ #spacing 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeViewColumnSpacing :: (MonadIO m, IsTreeViewColumn o) => o -> Int32 -> m ()
setTreeViewColumnSpacing obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "spacing" val

-- | Construct a `GValueConstruct` with valid value for the “@spacing@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewColumnSpacing :: (IsTreeViewColumn o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructTreeViewColumnSpacing val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "spacing" val

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSpacingPropertyInfo
instance AttrInfo TreeViewColumnSpacingPropertyInfo where
    type AttrAllowedOps TreeViewColumnSpacingPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TreeViewColumnSpacingPropertyInfo = IsTreeViewColumn
    type AttrSetTypeConstraint TreeViewColumnSpacingPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint TreeViewColumnSpacingPropertyInfo = (~) Int32
    type AttrTransferType TreeViewColumnSpacingPropertyInfo = Int32
    type AttrGetType TreeViewColumnSpacingPropertyInfo = Int32
    type AttrLabel TreeViewColumnSpacingPropertyInfo = "spacing"
    type AttrOrigin TreeViewColumnSpacingPropertyInfo = TreeViewColumn
    attrGet = getTreeViewColumnSpacing
    attrSet = setTreeViewColumnSpacing
    attrTransfer _ v = do
        return v
    attrConstruct = constructTreeViewColumnSpacing
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.spacing"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#g:attr:spacing"
        })
#endif

-- VVV Prop "title"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@title@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeViewColumn #title
-- @
getTreeViewColumnTitle :: (MonadIO m, IsTreeViewColumn o) => o -> m T.Text
getTreeViewColumnTitle obj = MIO.liftIO $ checkUnexpectedNothing "getTreeViewColumnTitle" $ B.Properties.getObjectPropertyString obj "title"

-- | Set the value of the “@title@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeViewColumn [ #title 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeViewColumnTitle :: (MonadIO m, IsTreeViewColumn o) => o -> T.Text -> m ()
setTreeViewColumnTitle obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "title" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@title@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewColumnTitle :: (IsTreeViewColumn o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructTreeViewColumnTitle val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "title" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnTitlePropertyInfo
instance AttrInfo TreeViewColumnTitlePropertyInfo where
    type AttrAllowedOps TreeViewColumnTitlePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TreeViewColumnTitlePropertyInfo = IsTreeViewColumn
    type AttrSetTypeConstraint TreeViewColumnTitlePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint TreeViewColumnTitlePropertyInfo = (~) T.Text
    type AttrTransferType TreeViewColumnTitlePropertyInfo = T.Text
    type AttrGetType TreeViewColumnTitlePropertyInfo = T.Text
    type AttrLabel TreeViewColumnTitlePropertyInfo = "title"
    type AttrOrigin TreeViewColumnTitlePropertyInfo = TreeViewColumn
    attrGet = getTreeViewColumnTitle
    attrSet = setTreeViewColumnTitle
    attrTransfer _ v = do
        return v
    attrConstruct = constructTreeViewColumnTitle
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.title"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#g:attr:title"
        })
#endif

-- VVV Prop "visible"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@visible@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeViewColumn #visible
-- @
getTreeViewColumnVisible :: (MonadIO m, IsTreeViewColumn o) => o -> m Bool
getTreeViewColumnVisible obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "visible"

-- | Set the value of the “@visible@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeViewColumn [ #visible 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeViewColumnVisible :: (MonadIO m, IsTreeViewColumn o) => o -> Bool -> m ()
setTreeViewColumnVisible obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "visible" val

-- | Construct a `GValueConstruct` with valid value for the “@visible@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewColumnVisible :: (IsTreeViewColumn o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTreeViewColumnVisible val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "visible" val

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnVisiblePropertyInfo
instance AttrInfo TreeViewColumnVisiblePropertyInfo where
    type AttrAllowedOps TreeViewColumnVisiblePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TreeViewColumnVisiblePropertyInfo = IsTreeViewColumn
    type AttrSetTypeConstraint TreeViewColumnVisiblePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TreeViewColumnVisiblePropertyInfo = (~) Bool
    type AttrTransferType TreeViewColumnVisiblePropertyInfo = Bool
    type AttrGetType TreeViewColumnVisiblePropertyInfo = Bool
    type AttrLabel TreeViewColumnVisiblePropertyInfo = "visible"
    type AttrOrigin TreeViewColumnVisiblePropertyInfo = TreeViewColumn
    attrGet = getTreeViewColumnVisible
    attrSet = setTreeViewColumnVisible
    attrTransfer _ v = do
        return v
    attrConstruct = constructTreeViewColumnVisible
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.visible"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#g:attr:visible"
        })
#endif

-- VVV Prop "widget"
   -- Type: TInterface (Name {namespace = "Gtk", name = "Widget"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just True)

-- | Get the value of the “@widget@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeViewColumn #widget
-- @
getTreeViewColumnWidget :: (MonadIO m, IsTreeViewColumn o) => o -> m (Maybe Gtk.Widget.Widget)
getTreeViewColumnWidget obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "widget" Gtk.Widget.Widget

-- | Set the value of the “@widget@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeViewColumn [ #widget 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeViewColumnWidget :: (MonadIO m, IsTreeViewColumn o, Gtk.Widget.IsWidget a) => o -> a -> m ()
setTreeViewColumnWidget obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "widget" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@widget@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewColumnWidget :: (IsTreeViewColumn o, MIO.MonadIO m, Gtk.Widget.IsWidget a) => a -> m (GValueConstruct o)
constructTreeViewColumnWidget val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "widget" (P.Just val)

-- | Set the value of the “@widget@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #widget
-- @
clearTreeViewColumnWidget :: (MonadIO m, IsTreeViewColumn o) => o -> m ()
clearTreeViewColumnWidget obj = liftIO $ B.Properties.setObjectPropertyObject obj "widget" (Nothing :: Maybe Gtk.Widget.Widget)

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnWidgetPropertyInfo
instance AttrInfo TreeViewColumnWidgetPropertyInfo where
    type AttrAllowedOps TreeViewColumnWidgetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint TreeViewColumnWidgetPropertyInfo = IsTreeViewColumn
    type AttrSetTypeConstraint TreeViewColumnWidgetPropertyInfo = Gtk.Widget.IsWidget
    type AttrTransferTypeConstraint TreeViewColumnWidgetPropertyInfo = Gtk.Widget.IsWidget
    type AttrTransferType TreeViewColumnWidgetPropertyInfo = Gtk.Widget.Widget
    type AttrGetType TreeViewColumnWidgetPropertyInfo = (Maybe Gtk.Widget.Widget)
    type AttrLabel TreeViewColumnWidgetPropertyInfo = "widget"
    type AttrOrigin TreeViewColumnWidgetPropertyInfo = TreeViewColumn
    attrGet = getTreeViewColumnWidget
    attrSet = setTreeViewColumnWidget
    attrTransfer _ v = do
        unsafeCastTo Gtk.Widget.Widget v
    attrConstruct = constructTreeViewColumnWidget
    attrClear = clearTreeViewColumnWidget
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.widget"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#g:attr:widget"
        })
#endif

-- VVV Prop "width"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable]
   -- Nullable: (Just False,Nothing)

-- | Get the value of the “@width@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeViewColumn #width
-- @
getTreeViewColumnWidth :: (MonadIO m, IsTreeViewColumn o) => o -> m Int32
getTreeViewColumnWidth obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "width"

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnWidthPropertyInfo
instance AttrInfo TreeViewColumnWidthPropertyInfo where
    type AttrAllowedOps TreeViewColumnWidthPropertyInfo = '[ 'AttrGet]
    type AttrBaseTypeConstraint TreeViewColumnWidthPropertyInfo = IsTreeViewColumn
    type AttrSetTypeConstraint TreeViewColumnWidthPropertyInfo = (~) ()
    type AttrTransferTypeConstraint TreeViewColumnWidthPropertyInfo = (~) ()
    type AttrTransferType TreeViewColumnWidthPropertyInfo = ()
    type AttrGetType TreeViewColumnWidthPropertyInfo = Int32
    type AttrLabel TreeViewColumnWidthPropertyInfo = "width"
    type AttrOrigin TreeViewColumnWidthPropertyInfo = TreeViewColumn
    attrGet = getTreeViewColumnWidth
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.width"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#g:attr:width"
        })
#endif

-- VVV Prop "x-offset"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable]
   -- Nullable: (Just False,Nothing)

-- | Get the value of the “@x-offset@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeViewColumn #xOffset
-- @
getTreeViewColumnXOffset :: (MonadIO m, IsTreeViewColumn o) => o -> m Int32
getTreeViewColumnXOffset obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "x-offset"

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnXOffsetPropertyInfo
instance AttrInfo TreeViewColumnXOffsetPropertyInfo where
    type AttrAllowedOps TreeViewColumnXOffsetPropertyInfo = '[ 'AttrGet]
    type AttrBaseTypeConstraint TreeViewColumnXOffsetPropertyInfo = IsTreeViewColumn
    type AttrSetTypeConstraint TreeViewColumnXOffsetPropertyInfo = (~) ()
    type AttrTransferTypeConstraint TreeViewColumnXOffsetPropertyInfo = (~) ()
    type AttrTransferType TreeViewColumnXOffsetPropertyInfo = ()
    type AttrGetType TreeViewColumnXOffsetPropertyInfo = Int32
    type AttrLabel TreeViewColumnXOffsetPropertyInfo = "x-offset"
    type AttrOrigin TreeViewColumnXOffsetPropertyInfo = TreeViewColumn
    attrGet = getTreeViewColumnXOffset
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.xOffset"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#g:attr:xOffset"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList TreeViewColumn
type instance O.AttributeList TreeViewColumn = TreeViewColumnAttributeList
type TreeViewColumnAttributeList = ('[ '("alignment", TreeViewColumnAlignmentPropertyInfo), '("cellArea", TreeViewColumnCellAreaPropertyInfo), '("clickable", TreeViewColumnClickablePropertyInfo), '("expand", TreeViewColumnExpandPropertyInfo), '("fixedWidth", TreeViewColumnFixedWidthPropertyInfo), '("maxWidth", TreeViewColumnMaxWidthPropertyInfo), '("minWidth", TreeViewColumnMinWidthPropertyInfo), '("reorderable", TreeViewColumnReorderablePropertyInfo), '("resizable", TreeViewColumnResizablePropertyInfo), '("sizing", TreeViewColumnSizingPropertyInfo), '("sortColumnId", TreeViewColumnSortColumnIdPropertyInfo), '("sortIndicator", TreeViewColumnSortIndicatorPropertyInfo), '("sortOrder", TreeViewColumnSortOrderPropertyInfo), '("spacing", TreeViewColumnSpacingPropertyInfo), '("title", TreeViewColumnTitlePropertyInfo), '("visible", TreeViewColumnVisiblePropertyInfo), '("widget", TreeViewColumnWidgetPropertyInfo), '("width", TreeViewColumnWidthPropertyInfo), '("xOffset", TreeViewColumnXOffsetPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
treeViewColumnAlignment :: AttrLabelProxy "alignment"
treeViewColumnAlignment = AttrLabelProxy

treeViewColumnCellArea :: AttrLabelProxy "cellArea"
treeViewColumnCellArea = AttrLabelProxy

treeViewColumnClickable :: AttrLabelProxy "clickable"
treeViewColumnClickable = AttrLabelProxy

treeViewColumnExpand :: AttrLabelProxy "expand"
treeViewColumnExpand = AttrLabelProxy

treeViewColumnFixedWidth :: AttrLabelProxy "fixedWidth"
treeViewColumnFixedWidth = AttrLabelProxy

treeViewColumnMaxWidth :: AttrLabelProxy "maxWidth"
treeViewColumnMaxWidth = AttrLabelProxy

treeViewColumnMinWidth :: AttrLabelProxy "minWidth"
treeViewColumnMinWidth = AttrLabelProxy

treeViewColumnReorderable :: AttrLabelProxy "reorderable"
treeViewColumnReorderable = AttrLabelProxy

treeViewColumnResizable :: AttrLabelProxy "resizable"
treeViewColumnResizable = AttrLabelProxy

treeViewColumnSizing :: AttrLabelProxy "sizing"
treeViewColumnSizing = AttrLabelProxy

treeViewColumnSortColumnId :: AttrLabelProxy "sortColumnId"
treeViewColumnSortColumnId = AttrLabelProxy

treeViewColumnSortIndicator :: AttrLabelProxy "sortIndicator"
treeViewColumnSortIndicator = AttrLabelProxy

treeViewColumnSortOrder :: AttrLabelProxy "sortOrder"
treeViewColumnSortOrder = AttrLabelProxy

treeViewColumnSpacing :: AttrLabelProxy "spacing"
treeViewColumnSpacing = AttrLabelProxy

treeViewColumnTitle :: AttrLabelProxy "title"
treeViewColumnTitle = AttrLabelProxy

treeViewColumnVisible :: AttrLabelProxy "visible"
treeViewColumnVisible = AttrLabelProxy

treeViewColumnWidget :: AttrLabelProxy "widget"
treeViewColumnWidget = AttrLabelProxy

treeViewColumnWidth :: AttrLabelProxy "width"
treeViewColumnWidth = AttrLabelProxy

treeViewColumnXOffset :: AttrLabelProxy "xOffset"
treeViewColumnXOffset = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList TreeViewColumn = TreeViewColumnSignalList
type TreeViewColumnSignalList = ('[ '("clicked", TreeViewColumnClickedSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif

-- method TreeViewColumn::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_column_new" gtk_tree_view_column_new :: 
    IO (Ptr TreeViewColumn)

-- | Creates a new t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'.
treeViewColumnNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m TreeViewColumn
    -- ^ __Returns:__ A newly created t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'.
treeViewColumnNew  = liftIO $ do
    result <- gtk_tree_view_column_new
    checkUnexpectedReturnNULL "treeViewColumnNew" result
    result' <- (newObject TreeViewColumn) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method TreeViewColumn::new_with_area
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the #GtkCellArea that the newly created column should use to layout cells."
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
--               (TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_column_new_with_area" gtk_tree_view_column_new_with_area :: 
    Ptr Gtk.CellArea.CellArea ->            -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    IO (Ptr TreeViewColumn)

-- | Creates a new t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn' using /@area@/ to render its cells.
-- 
-- /Since: 3.0/
treeViewColumnNewWithArea ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.CellArea.IsCellArea a) =>
    a
    -- ^ /@area@/: the t'GI.Gtk.Objects.CellArea.CellArea' that the newly created column should use to layout cells.
    -> m TreeViewColumn
    -- ^ __Returns:__ A newly created t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'.
treeViewColumnNewWithArea area = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    result <- gtk_tree_view_column_new_with_area area'
    checkUnexpectedReturnNULL "treeViewColumnNewWithArea" result
    result' <- (newObject TreeViewColumn) result
    touchManagedPtr area
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method TreeViewColumn::add_attribute
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cell_renderer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkCellRenderer to set attributes on"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "attribute"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "An attribute on the renderer"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "column"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The column position on the model to get the attribute from."
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

foreign import ccall "gtk_tree_view_column_add_attribute" gtk_tree_view_column_add_attribute :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- cell_renderer : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    CString ->                              -- attribute : TBasicType TUTF8
    Int32 ->                                -- column : TBasicType TInt
    IO ()

-- | Adds an attribute mapping to the list in /@treeColumn@/.  The /@column@/ is the
-- column of the model to get a value from, and the /@attribute@/ is the
-- parameter on /@cellRenderer@/ to be set from the value. So for example
-- if column 2 of the model contains strings, you could have the
-- “text” attribute of a t'GI.Gtk.Objects.CellRendererText.CellRendererText' get its values from
-- column 2.
treeViewColumnAddAttribute ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a, Gtk.CellRenderer.IsCellRenderer b) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'.
    -> b
    -- ^ /@cellRenderer@/: the t'GI.Gtk.Objects.CellRenderer.CellRenderer' to set attributes on
    -> T.Text
    -- ^ /@attribute@/: An attribute on the renderer
    -> Int32
    -- ^ /@column@/: The column position on the model to get the attribute from.
    -> m ()
treeViewColumnAddAttribute treeColumn cellRenderer attribute column = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    cellRenderer' <- unsafeManagedPtrCastPtr cellRenderer
    attribute' <- textToCString attribute
    gtk_tree_view_column_add_attribute treeColumn' cellRenderer' attribute' column
    touchManagedPtr treeColumn
    touchManagedPtr cellRenderer
    freeMem attribute'
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnAddAttributeMethodInfo
instance (signature ~ (b -> T.Text -> Int32 -> m ()), MonadIO m, IsTreeViewColumn a, Gtk.CellRenderer.IsCellRenderer b) => O.OverloadedMethod TreeViewColumnAddAttributeMethodInfo a signature where
    overloadedMethod = treeViewColumnAddAttribute

instance O.OverloadedMethodInfo TreeViewColumnAddAttributeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnAddAttribute",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnAddAttribute"
        })


#endif

-- method TreeViewColumn::cell_get_position
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeViewColumn"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cell_renderer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellRenderer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x_offset"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "return location for the horizontal\n           position of @cell within @tree_column, may be %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "width"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "return location for the width of @cell,\n        may be %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_column_cell_get_position" gtk_tree_view_column_cell_get_position :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- cell_renderer : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    Ptr Int32 ->                            -- x_offset : TBasicType TInt
    Ptr Int32 ->                            -- width : TBasicType TInt
    IO CInt

-- | Obtains the horizontal position and size of a cell in a column. If the
-- cell is not found in the column, /@startPos@/ and /@width@/ are not changed and
-- 'P.False' is returned.
treeViewColumnCellGetPosition ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a, Gtk.CellRenderer.IsCellRenderer b) =>
    a
    -- ^ /@treeColumn@/: a t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'
    -> b
    -- ^ /@cellRenderer@/: a t'GI.Gtk.Objects.CellRenderer.CellRenderer'
    -> m ((Bool, Int32, Int32))
    -- ^ __Returns:__ 'P.True' if /@cell@/ belongs to /@treeColumn@/.
treeViewColumnCellGetPosition treeColumn cellRenderer = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    cellRenderer' <- unsafeManagedPtrCastPtr cellRenderer
    xOffset <- allocMem :: IO (Ptr Int32)
    width <- allocMem :: IO (Ptr Int32)
    result <- gtk_tree_view_column_cell_get_position treeColumn' cellRenderer' xOffset width
    let result' = (/= 0) result
    xOffset' <- peek xOffset
    width' <- peek width
    touchManagedPtr treeColumn
    touchManagedPtr cellRenderer
    freeMem xOffset
    freeMem width
    return (result', xOffset', width')

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnCellGetPositionMethodInfo
instance (signature ~ (b -> m ((Bool, Int32, Int32))), MonadIO m, IsTreeViewColumn a, Gtk.CellRenderer.IsCellRenderer b) => O.OverloadedMethod TreeViewColumnCellGetPositionMethodInfo a signature where
    overloadedMethod = treeViewColumnCellGetPosition

instance O.OverloadedMethodInfo TreeViewColumnCellGetPositionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnCellGetPosition",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnCellGetPosition"
        })


#endif

-- method TreeViewColumn::cell_get_size
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cell_area"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The area a cell in the column will be allocated, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x_offset"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "location to return x offset of a cell relative to @cell_area, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "y_offset"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "location to return y offset of a cell relative to @cell_area, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "width"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location to return width needed to render a cell, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "height"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location to return height needed to render a cell, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       ]
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_column_cell_get_size" gtk_tree_view_column_cell_get_size :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    Ptr Gdk.Rectangle.Rectangle ->          -- cell_area : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    Ptr Int32 ->                            -- x_offset : TBasicType TInt
    Ptr Int32 ->                            -- y_offset : TBasicType TInt
    Ptr Int32 ->                            -- width : TBasicType TInt
    Ptr Int32 ->                            -- height : TBasicType TInt
    IO ()

-- | Obtains the width and height needed to render the column.  This is used
-- primarily by the t'GI.Gtk.Objects.TreeView.TreeView'.
treeViewColumnCellGetSize ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'.
    -> Maybe (Gdk.Rectangle.Rectangle)
    -- ^ /@cellArea@/: The area a cell in the column will be allocated, or 'P.Nothing'
    -> m ((Int32, Int32, Int32, Int32))
treeViewColumnCellGetSize treeColumn cellArea = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    maybeCellArea <- case cellArea of
        Nothing -> return nullPtr
        Just jCellArea -> do
            jCellArea' <- unsafeManagedPtrGetPtr jCellArea
            return jCellArea'
    xOffset <- allocMem :: IO (Ptr Int32)
    yOffset <- allocMem :: IO (Ptr Int32)
    width <- allocMem :: IO (Ptr Int32)
    height <- allocMem :: IO (Ptr Int32)
    gtk_tree_view_column_cell_get_size treeColumn' maybeCellArea xOffset yOffset width height
    xOffset' <- peek xOffset
    yOffset' <- peek yOffset
    width' <- peek width
    height' <- peek height
    touchManagedPtr treeColumn
    whenJust cellArea touchManagedPtr
    freeMem xOffset
    freeMem yOffset
    freeMem width
    freeMem height
    return (xOffset', yOffset', width', height')

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnCellGetSizeMethodInfo
instance (signature ~ (Maybe (Gdk.Rectangle.Rectangle) -> m ((Int32, Int32, Int32, Int32))), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnCellGetSizeMethodInfo a signature where
    overloadedMethod = treeViewColumnCellGetSize

instance O.OverloadedMethodInfo TreeViewColumnCellGetSizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnCellGetSize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnCellGetSize"
        })


#endif

-- method TreeViewColumn::cell_is_visible
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn"
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

foreign import ccall "gtk_tree_view_column_cell_is_visible" gtk_tree_view_column_cell_is_visible :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    IO CInt

-- | Returns 'P.True' if any of the cells packed into the /@treeColumn@/ are visible.
-- For this to be meaningful, you must first initialize the cells with
-- 'GI.Gtk.Objects.TreeViewColumn.treeViewColumnCellSetCellData'
treeViewColumnCellIsVisible ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'
    -> m Bool
    -- ^ __Returns:__ 'P.True', if any of the cells packed into the /@treeColumn@/ are currently visible
treeViewColumnCellIsVisible treeColumn = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    result <- gtk_tree_view_column_cell_is_visible treeColumn'
    let result' = (/= 0) result
    touchManagedPtr treeColumn
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnCellIsVisibleMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnCellIsVisibleMethodInfo a signature where
    overloadedMethod = treeViewColumnCellIsVisible

instance O.OverloadedMethodInfo TreeViewColumnCellIsVisibleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnCellIsVisible",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnCellIsVisible"
        })


#endif

-- method TreeViewColumn::cell_set_cell_data
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "tree_model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "The #GtkTreeModel to to get the cell renderers attributes from."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "The #GtkTreeIter to to get the cell renderer\8217s attributes from."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "is_expander"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE, if the row has children"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "is_expanded"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE, if the row has visible children"
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

foreign import ccall "gtk_tree_view_column_cell_set_cell_data" gtk_tree_view_column_cell_set_cell_data :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    Ptr Gtk.TreeModel.TreeModel ->          -- tree_model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    CInt ->                                 -- is_expander : TBasicType TBoolean
    CInt ->                                 -- is_expanded : TBasicType TBoolean
    IO ()

-- | Sets the cell renderer based on the /@treeModel@/ and /@iter@/.  That is, for
-- every attribute mapping in /@treeColumn@/, it will get a value from the set
-- column on the /@iter@/, and use that value to set the attribute on the cell
-- renderer.  This is used primarily by the t'GI.Gtk.Objects.TreeView.TreeView'.
treeViewColumnCellSetCellData ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a, Gtk.TreeModel.IsTreeModel b) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'.
    -> b
    -- ^ /@treeModel@/: The t'GI.Gtk.Interfaces.TreeModel.TreeModel' to to get the cell renderers attributes from.
    -> Gtk.TreeIter.TreeIter
    -- ^ /@iter@/: The t'GI.Gtk.Structs.TreeIter.TreeIter' to to get the cell renderer’s attributes from.
    -> Bool
    -- ^ /@isExpander@/: 'P.True', if the row has children
    -> Bool
    -- ^ /@isExpanded@/: 'P.True', if the row has visible children
    -> m ()
treeViewColumnCellSetCellData treeColumn treeModel iter isExpander isExpanded = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    treeModel' <- unsafeManagedPtrCastPtr treeModel
    iter' <- unsafeManagedPtrGetPtr iter
    let isExpander' = (fromIntegral . fromEnum) isExpander
    let isExpanded' = (fromIntegral . fromEnum) isExpanded
    gtk_tree_view_column_cell_set_cell_data treeColumn' treeModel' iter' isExpander' isExpanded'
    touchManagedPtr treeColumn
    touchManagedPtr treeModel
    touchManagedPtr iter
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnCellSetCellDataMethodInfo
instance (signature ~ (b -> Gtk.TreeIter.TreeIter -> Bool -> Bool -> m ()), MonadIO m, IsTreeViewColumn a, Gtk.TreeModel.IsTreeModel b) => O.OverloadedMethod TreeViewColumnCellSetCellDataMethodInfo a signature where
    overloadedMethod = treeViewColumnCellSetCellData

instance O.OverloadedMethodInfo TreeViewColumnCellSetCellDataMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnCellSetCellData",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnCellSetCellData"
        })


#endif

-- method TreeViewColumn::clear
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn"
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

foreign import ccall "gtk_tree_view_column_clear" gtk_tree_view_column_clear :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    IO ()

-- | Unsets all the mappings on all renderers on the /@treeColumn@/.
treeViewColumnClear ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'
    -> m ()
treeViewColumnClear treeColumn = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    gtk_tree_view_column_clear treeColumn'
    touchManagedPtr treeColumn
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnClearMethodInfo
instance (signature ~ (m ()), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnClearMethodInfo a signature where
    overloadedMethod = treeViewColumnClear

instance O.OverloadedMethodInfo TreeViewColumnClearMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnClear",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnClear"
        })


#endif

-- method TreeViewColumn::clear_attributes
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeViewColumn"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cell_renderer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GtkCellRenderer to clear the attribute mapping on."
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

foreign import ccall "gtk_tree_view_column_clear_attributes" gtk_tree_view_column_clear_attributes :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- cell_renderer : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    IO ()

-- | Clears all existing attributes previously set with
-- @/gtk_tree_view_column_set_attributes()/@.
treeViewColumnClearAttributes ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a, Gtk.CellRenderer.IsCellRenderer b) =>
    a
    -- ^ /@treeColumn@/: a t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'
    -> b
    -- ^ /@cellRenderer@/: a t'GI.Gtk.Objects.CellRenderer.CellRenderer' to clear the attribute mapping on.
    -> m ()
treeViewColumnClearAttributes treeColumn cellRenderer = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    cellRenderer' <- unsafeManagedPtrCastPtr cellRenderer
    gtk_tree_view_column_clear_attributes treeColumn' cellRenderer'
    touchManagedPtr treeColumn
    touchManagedPtr cellRenderer
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnClearAttributesMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsTreeViewColumn a, Gtk.CellRenderer.IsCellRenderer b) => O.OverloadedMethod TreeViewColumnClearAttributesMethodInfo a signature where
    overloadedMethod = treeViewColumnClearAttributes

instance O.OverloadedMethodInfo TreeViewColumnClearAttributesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnClearAttributes",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnClearAttributes"
        })


#endif

-- method TreeViewColumn::clicked
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeViewColumn"
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

foreign import ccall "gtk_tree_view_column_clicked" gtk_tree_view_column_clicked :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    IO ()

-- | Emits the “clicked” signal on the column.  This function will only work if
-- /@treeColumn@/ is clickable.
treeViewColumnClicked ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: a t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'
    -> m ()
treeViewColumnClicked treeColumn = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    gtk_tree_view_column_clicked treeColumn'
    touchManagedPtr treeColumn
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnClickedMethodInfo
instance (signature ~ (m ()), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnClickedMethodInfo a signature where
    overloadedMethod = treeViewColumnClicked

instance O.OverloadedMethodInfo TreeViewColumnClickedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnClicked",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnClicked"
        })


#endif

-- method TreeViewColumn::focus_cell
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkCellRenderer" , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_view_column_focus_cell" gtk_tree_view_column_focus_cell :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    IO ()

-- | Sets the current keyboard focus to be at /@cell@/, if the column contains
-- 2 or more editable and activatable cells.
-- 
-- /Since: 2.2/
treeViewColumnFocusCell ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a, Gtk.CellRenderer.IsCellRenderer b) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'
    -> b
    -- ^ /@cell@/: A t'GI.Gtk.Objects.CellRenderer.CellRenderer'
    -> m ()
treeViewColumnFocusCell treeColumn cell = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    cell' <- unsafeManagedPtrCastPtr cell
    gtk_tree_view_column_focus_cell treeColumn' cell'
    touchManagedPtr treeColumn
    touchManagedPtr cell
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnFocusCellMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsTreeViewColumn a, Gtk.CellRenderer.IsCellRenderer b) => O.OverloadedMethod TreeViewColumnFocusCellMethodInfo a signature where
    overloadedMethod = treeViewColumnFocusCell

instance O.OverloadedMethodInfo TreeViewColumnFocusCellMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnFocusCell",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnFocusCell"
        })


#endif

-- method TreeViewColumn::get_alignment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn."
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
-- returnType: Just (TBasicType TFloat)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_column_get_alignment" gtk_tree_view_column_get_alignment :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    IO CFloat

-- | Returns the current x alignment of /@treeColumn@/.  This value can range
-- between 0.0 and 1.0.
treeViewColumnGetAlignment ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'.
    -> m Float
    -- ^ __Returns:__ The current alignent of /@treeColumn@/.
treeViewColumnGetAlignment treeColumn = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    result <- gtk_tree_view_column_get_alignment treeColumn'
    let result' = realToFrac result
    touchManagedPtr treeColumn
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetAlignmentMethodInfo
instance (signature ~ (m Float), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnGetAlignmentMethodInfo a signature where
    overloadedMethod = treeViewColumnGetAlignment

instance O.OverloadedMethodInfo TreeViewColumnGetAlignmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnGetAlignment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnGetAlignment"
        })


#endif

-- method TreeViewColumn::get_button
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Widget" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_column_get_button" gtk_tree_view_column_get_button :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    IO (Ptr Gtk.Widget.Widget)

-- | Returns the button used in the treeview column header
-- 
-- /Since: 3.0/
treeViewColumnGetButton ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'
    -> m Gtk.Widget.Widget
    -- ^ __Returns:__ The button for the column header.
treeViewColumnGetButton treeColumn = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    result <- gtk_tree_view_column_get_button treeColumn'
    checkUnexpectedReturnNULL "treeViewColumnGetButton" result
    result' <- (newObject Gtk.Widget.Widget) result
    touchManagedPtr treeColumn
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetButtonMethodInfo
instance (signature ~ (m Gtk.Widget.Widget), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnGetButtonMethodInfo a signature where
    overloadedMethod = treeViewColumnGetButton

instance O.OverloadedMethodInfo TreeViewColumnGetButtonMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnGetButton",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnGetButton"
        })


#endif

-- method TreeViewColumn::get_clickable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeViewColumn"
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

foreign import ccall "gtk_tree_view_column_get_clickable" gtk_tree_view_column_get_clickable :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    IO CInt

-- | Returns 'P.True' if the user can click on the header for the column.
treeViewColumnGetClickable ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: a t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if user can click the column header.
treeViewColumnGetClickable treeColumn = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    result <- gtk_tree_view_column_get_clickable treeColumn'
    let result' = (/= 0) result
    touchManagedPtr treeColumn
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetClickableMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnGetClickableMethodInfo a signature where
    overloadedMethod = treeViewColumnGetClickable

instance O.OverloadedMethodInfo TreeViewColumnGetClickableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnGetClickable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnGetClickable"
        })


#endif

-- method TreeViewColumn::get_expand
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn."
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

foreign import ccall "gtk_tree_view_column_get_expand" gtk_tree_view_column_get_expand :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    IO CInt

-- | Returns 'P.True' if the column expands to fill available space.
-- 
-- /Since: 2.4/
treeViewColumnGetExpand ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'.
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the column expands to fill available space.
treeViewColumnGetExpand treeColumn = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    result <- gtk_tree_view_column_get_expand treeColumn'
    let result' = (/= 0) result
    touchManagedPtr treeColumn
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetExpandMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnGetExpandMethodInfo a signature where
    overloadedMethod = treeViewColumnGetExpand

instance O.OverloadedMethodInfo TreeViewColumnGetExpandMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnGetExpand",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnGetExpand"
        })


#endif

-- method TreeViewColumn::get_fixed_width
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn."
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
-- returnType: Just (TBasicType TInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_column_get_fixed_width" gtk_tree_view_column_get_fixed_width :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    IO Int32

-- | Gets the fixed width of the column.  This may not be the actual displayed
-- width of the column; for that, use 'GI.Gtk.Objects.TreeViewColumn.treeViewColumnGetWidth'.
treeViewColumnGetFixedWidth ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'.
    -> m Int32
    -- ^ __Returns:__ The fixed width of the column.
treeViewColumnGetFixedWidth treeColumn = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    result <- gtk_tree_view_column_get_fixed_width treeColumn'
    touchManagedPtr treeColumn
    return result

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetFixedWidthMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnGetFixedWidthMethodInfo a signature where
    overloadedMethod = treeViewColumnGetFixedWidth

instance O.OverloadedMethodInfo TreeViewColumnGetFixedWidthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnGetFixedWidth",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnGetFixedWidth"
        })


#endif

-- method TreeViewColumn::get_max_width
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn."
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
-- returnType: Just (TBasicType TInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_column_get_max_width" gtk_tree_view_column_get_max_width :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    IO Int32

-- | Returns the maximum width in pixels of the /@treeColumn@/, or -1 if no maximum
-- width is set.
treeViewColumnGetMaxWidth ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'.
    -> m Int32
    -- ^ __Returns:__ The maximum width of the /@treeColumn@/.
treeViewColumnGetMaxWidth treeColumn = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    result <- gtk_tree_view_column_get_max_width treeColumn'
    touchManagedPtr treeColumn
    return result

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetMaxWidthMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnGetMaxWidthMethodInfo a signature where
    overloadedMethod = treeViewColumnGetMaxWidth

instance O.OverloadedMethodInfo TreeViewColumnGetMaxWidthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnGetMaxWidth",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnGetMaxWidth"
        })


#endif

-- method TreeViewColumn::get_min_width
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn."
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
-- returnType: Just (TBasicType TInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_column_get_min_width" gtk_tree_view_column_get_min_width :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    IO Int32

-- | Returns the minimum width in pixels of the /@treeColumn@/, or -1 if no minimum
-- width is set.
treeViewColumnGetMinWidth ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'.
    -> m Int32
    -- ^ __Returns:__ The minimum width of the /@treeColumn@/.
treeViewColumnGetMinWidth treeColumn = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    result <- gtk_tree_view_column_get_min_width treeColumn'
    touchManagedPtr treeColumn
    return result

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetMinWidthMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnGetMinWidthMethodInfo a signature where
    overloadedMethod = treeViewColumnGetMinWidth

instance O.OverloadedMethodInfo TreeViewColumnGetMinWidthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnGetMinWidth",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnGetMinWidth"
        })


#endif

-- method TreeViewColumn::get_reorderable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn"
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

foreign import ccall "gtk_tree_view_column_get_reorderable" gtk_tree_view_column_get_reorderable :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    IO CInt

-- | Returns 'P.True' if the /@treeColumn@/ can be reordered by the user.
treeViewColumnGetReorderable ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the /@treeColumn@/ can be reordered by the user.
treeViewColumnGetReorderable treeColumn = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    result <- gtk_tree_view_column_get_reorderable treeColumn'
    let result' = (/= 0) result
    touchManagedPtr treeColumn
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetReorderableMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnGetReorderableMethodInfo a signature where
    overloadedMethod = treeViewColumnGetReorderable

instance O.OverloadedMethodInfo TreeViewColumnGetReorderableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnGetReorderable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnGetReorderable"
        })


#endif

-- method TreeViewColumn::get_resizable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn"
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

foreign import ccall "gtk_tree_view_column_get_resizable" gtk_tree_view_column_get_resizable :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    IO CInt

-- | Returns 'P.True' if the /@treeColumn@/ can be resized by the end user.
treeViewColumnGetResizable ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'
    -> m Bool
    -- ^ __Returns:__ 'P.True', if the /@treeColumn@/ can be resized.
treeViewColumnGetResizable treeColumn = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    result <- gtk_tree_view_column_get_resizable treeColumn'
    let result' = (/= 0) result
    touchManagedPtr treeColumn
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetResizableMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnGetResizableMethodInfo a signature where
    overloadedMethod = treeViewColumnGetResizable

instance O.OverloadedMethodInfo TreeViewColumnGetResizableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnGetResizable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnGetResizable"
        })


#endif

-- method TreeViewColumn::get_sizing
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn."
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
--               (TInterface
--                  Name { namespace = "Gtk" , name = "TreeViewColumnSizing" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_column_get_sizing" gtk_tree_view_column_get_sizing :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    IO CUInt

-- | Returns the current type of /@treeColumn@/.
treeViewColumnGetSizing ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'.
    -> m Gtk.Enums.TreeViewColumnSizing
    -- ^ __Returns:__ The type of /@treeColumn@/.
treeViewColumnGetSizing treeColumn = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    result <- gtk_tree_view_column_get_sizing treeColumn'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr treeColumn
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetSizingMethodInfo
instance (signature ~ (m Gtk.Enums.TreeViewColumnSizing), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnGetSizingMethodInfo a signature where
    overloadedMethod = treeViewColumnGetSizing

instance O.OverloadedMethodInfo TreeViewColumnGetSizingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnGetSizing",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnGetSizing"
        })


#endif

-- method TreeViewColumn::get_sort_column_id
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeViewColumn"
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
-- returnType: Just (TBasicType TInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_column_get_sort_column_id" gtk_tree_view_column_get_sort_column_id :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    IO Int32

-- | Gets the logical /@sortColumnId@/ that the model sorts on when this
-- column is selected for sorting.
-- See 'GI.Gtk.Objects.TreeViewColumn.treeViewColumnSetSortColumnId'.
treeViewColumnGetSortColumnId ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: a t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'
    -> m Int32
    -- ^ __Returns:__ the current /@sortColumnId@/ for this column, or -1 if
    --               this column can’t be used for sorting.
treeViewColumnGetSortColumnId treeColumn = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    result <- gtk_tree_view_column_get_sort_column_id treeColumn'
    touchManagedPtr treeColumn
    return result

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetSortColumnIdMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnGetSortColumnIdMethodInfo a signature where
    overloadedMethod = treeViewColumnGetSortColumnId

instance O.OverloadedMethodInfo TreeViewColumnGetSortColumnIdMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnGetSortColumnId",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnGetSortColumnId"
        })


#endif

-- method TreeViewColumn::get_sort_indicator
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeViewColumn"
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

foreign import ccall "gtk_tree_view_column_get_sort_indicator" gtk_tree_view_column_get_sort_indicator :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    IO CInt

-- | Gets the value set by 'GI.Gtk.Objects.TreeViewColumn.treeViewColumnSetSortIndicator'.
treeViewColumnGetSortIndicator ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: a t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'
    -> m Bool
    -- ^ __Returns:__ whether the sort indicator arrow is displayed
treeViewColumnGetSortIndicator treeColumn = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    result <- gtk_tree_view_column_get_sort_indicator treeColumn'
    let result' = (/= 0) result
    touchManagedPtr treeColumn
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetSortIndicatorMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnGetSortIndicatorMethodInfo a signature where
    overloadedMethod = treeViewColumnGetSortIndicator

instance O.OverloadedMethodInfo TreeViewColumnGetSortIndicatorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnGetSortIndicator",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnGetSortIndicator"
        })


#endif

-- method TreeViewColumn::get_sort_order
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeViewColumn"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "SortType" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_column_get_sort_order" gtk_tree_view_column_get_sort_order :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    IO CUInt

-- | Gets the value set by 'GI.Gtk.Objects.TreeViewColumn.treeViewColumnSetSortOrder'.
treeViewColumnGetSortOrder ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: a t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'
    -> m Gtk.Enums.SortType
    -- ^ __Returns:__ the sort order the sort indicator is indicating
treeViewColumnGetSortOrder treeColumn = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    result <- gtk_tree_view_column_get_sort_order treeColumn'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr treeColumn
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetSortOrderMethodInfo
instance (signature ~ (m Gtk.Enums.SortType), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnGetSortOrderMethodInfo a signature where
    overloadedMethod = treeViewColumnGetSortOrder

instance O.OverloadedMethodInfo TreeViewColumnGetSortOrderMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnGetSortOrder",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnGetSortOrder"
        })


#endif

-- method TreeViewColumn::get_spacing
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn."
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
-- returnType: Just (TBasicType TInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_column_get_spacing" gtk_tree_view_column_get_spacing :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    IO Int32

-- | Returns the spacing of /@treeColumn@/.
treeViewColumnGetSpacing ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'.
    -> m Int32
    -- ^ __Returns:__ the spacing of /@treeColumn@/.
treeViewColumnGetSpacing treeColumn = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    result <- gtk_tree_view_column_get_spacing treeColumn'
    touchManagedPtr treeColumn
    return result

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetSpacingMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnGetSpacingMethodInfo a signature where
    overloadedMethod = treeViewColumnGetSpacing

instance O.OverloadedMethodInfo TreeViewColumnGetSpacingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnGetSpacing",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnGetSpacing"
        })


#endif

-- method TreeViewColumn::get_title
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn."
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
-- returnType: Just (TBasicType TUTF8)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_column_get_title" gtk_tree_view_column_get_title :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    IO CString

-- | Returns the title of the widget.
treeViewColumnGetTitle ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'.
    -> m T.Text
    -- ^ __Returns:__ the title of the column. This string should not be
    -- modified or freed.
treeViewColumnGetTitle treeColumn = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    result <- gtk_tree_view_column_get_title treeColumn'
    checkUnexpectedReturnNULL "treeViewColumnGetTitle" result
    result' <- cstringToText result
    touchManagedPtr treeColumn
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetTitleMethodInfo
instance (signature ~ (m T.Text), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnGetTitleMethodInfo a signature where
    overloadedMethod = treeViewColumnGetTitle

instance O.OverloadedMethodInfo TreeViewColumnGetTitleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnGetTitle",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnGetTitle"
        })


#endif

-- method TreeViewColumn::get_tree_view
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Widget" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_column_get_tree_view" gtk_tree_view_column_get_tree_view :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    IO (Ptr Gtk.Widget.Widget)

-- | Returns the t'GI.Gtk.Objects.TreeView.TreeView' wherein /@treeColumn@/ has been inserted.
-- If /@column@/ is currently not inserted in any tree view, 'P.Nothing' is
-- returned.
-- 
-- /Since: 2.12/
treeViewColumnGetTreeView ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'
    -> m (Maybe Gtk.Widget.Widget)
    -- ^ __Returns:__ The tree view wherein /@column@/ has
    --     been inserted if any, 'P.Nothing' otherwise.
treeViewColumnGetTreeView treeColumn = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    result <- gtk_tree_view_column_get_tree_view treeColumn'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Widget.Widget) result'
        return result''
    touchManagedPtr treeColumn
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetTreeViewMethodInfo
instance (signature ~ (m (Maybe Gtk.Widget.Widget)), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnGetTreeViewMethodInfo a signature where
    overloadedMethod = treeViewColumnGetTreeView

instance O.OverloadedMethodInfo TreeViewColumnGetTreeViewMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnGetTreeView",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnGetTreeView"
        })


#endif

-- method TreeViewColumn::get_visible
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn."
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

foreign import ccall "gtk_tree_view_column_get_visible" gtk_tree_view_column_get_visible :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    IO CInt

-- | Returns 'P.True' if /@treeColumn@/ is visible.
treeViewColumnGetVisible ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'.
    -> m Bool
    -- ^ __Returns:__ whether the column is visible or not.  If it is visible, then
    -- the tree will show the column.
treeViewColumnGetVisible treeColumn = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    result <- gtk_tree_view_column_get_visible treeColumn'
    let result' = (/= 0) result
    touchManagedPtr treeColumn
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetVisibleMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnGetVisibleMethodInfo a signature where
    overloadedMethod = treeViewColumnGetVisible

instance O.OverloadedMethodInfo TreeViewColumnGetVisibleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnGetVisible",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnGetVisible"
        })


#endif

-- method TreeViewColumn::get_widget
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn."
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Widget" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_column_get_widget" gtk_tree_view_column_get_widget :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    IO (Ptr Gtk.Widget.Widget)

-- | Returns the t'GI.Gtk.Objects.Widget.Widget' in the button on the column header.
-- If a custom widget has not been set then 'P.Nothing' is returned.
treeViewColumnGetWidget ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'.
    -> m (Maybe Gtk.Widget.Widget)
    -- ^ __Returns:__ The t'GI.Gtk.Objects.Widget.Widget' in the column
    --     header, or 'P.Nothing'
treeViewColumnGetWidget treeColumn = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    result <- gtk_tree_view_column_get_widget treeColumn'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Widget.Widget) result'
        return result''
    touchManagedPtr treeColumn
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetWidgetMethodInfo
instance (signature ~ (m (Maybe Gtk.Widget.Widget)), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnGetWidgetMethodInfo a signature where
    overloadedMethod = treeViewColumnGetWidget

instance O.OverloadedMethodInfo TreeViewColumnGetWidgetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnGetWidget",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnGetWidget"
        })


#endif

-- method TreeViewColumn::get_width
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn."
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
-- returnType: Just (TBasicType TInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_column_get_width" gtk_tree_view_column_get_width :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    IO Int32

-- | Returns the current size of /@treeColumn@/ in pixels.
treeViewColumnGetWidth ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'.
    -> m Int32
    -- ^ __Returns:__ The current width of /@treeColumn@/.
treeViewColumnGetWidth treeColumn = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    result <- gtk_tree_view_column_get_width treeColumn'
    touchManagedPtr treeColumn
    return result

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetWidthMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnGetWidthMethodInfo a signature where
    overloadedMethod = treeViewColumnGetWidth

instance O.OverloadedMethodInfo TreeViewColumnGetWidthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnGetWidth",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnGetWidth"
        })


#endif

-- method TreeViewColumn::get_x_offset
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn."
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
-- returnType: Just (TBasicType TInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_column_get_x_offset" gtk_tree_view_column_get_x_offset :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    IO Int32

-- | Returns the current X offset of /@treeColumn@/ in pixels.
-- 
-- /Since: 3.2/
treeViewColumnGetXOffset ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'.
    -> m Int32
    -- ^ __Returns:__ The current X offset of /@treeColumn@/.
treeViewColumnGetXOffset treeColumn = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    result <- gtk_tree_view_column_get_x_offset treeColumn'
    touchManagedPtr treeColumn
    return result

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnGetXOffsetMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnGetXOffsetMethodInfo a signature where
    overloadedMethod = treeViewColumnGetXOffset

instance O.OverloadedMethodInfo TreeViewColumnGetXOffsetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnGetXOffset",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnGetXOffset"
        })


#endif

-- method TreeViewColumn::pack_end
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The #GtkCellRenderer."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "expand"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "%TRUE if @cell is to be given extra space allocated to @tree_column."
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

foreign import ccall "gtk_tree_view_column_pack_end" gtk_tree_view_column_pack_end :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    CInt ->                                 -- expand : TBasicType TBoolean
    IO ()

-- | Adds the /@cell@/ to end of the column. If /@expand@/ is 'P.False', then the /@cell@/
-- is allocated no more space than it needs. Any unused space is divided
-- evenly between cells for which /@expand@/ is 'P.True'.
treeViewColumnPackEnd ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a, Gtk.CellRenderer.IsCellRenderer b) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'.
    -> b
    -- ^ /@cell@/: The t'GI.Gtk.Objects.CellRenderer.CellRenderer'.
    -> Bool
    -- ^ /@expand@/: 'P.True' if /@cell@/ is to be given extra space allocated to /@treeColumn@/.
    -> m ()
treeViewColumnPackEnd treeColumn cell expand = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    cell' <- unsafeManagedPtrCastPtr cell
    let expand' = (fromIntegral . fromEnum) expand
    gtk_tree_view_column_pack_end treeColumn' cell' expand'
    touchManagedPtr treeColumn
    touchManagedPtr cell
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnPackEndMethodInfo
instance (signature ~ (b -> Bool -> m ()), MonadIO m, IsTreeViewColumn a, Gtk.CellRenderer.IsCellRenderer b) => O.OverloadedMethod TreeViewColumnPackEndMethodInfo a signature where
    overloadedMethod = treeViewColumnPackEnd

instance O.OverloadedMethodInfo TreeViewColumnPackEndMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnPackEnd",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnPackEnd"
        })


#endif

-- method TreeViewColumn::pack_start
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The #GtkCellRenderer."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "expand"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "%TRUE if @cell is to be given extra space allocated to @tree_column."
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

foreign import ccall "gtk_tree_view_column_pack_start" gtk_tree_view_column_pack_start :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    CInt ->                                 -- expand : TBasicType TBoolean
    IO ()

-- | Packs the /@cell@/ into the beginning of the column. If /@expand@/ is 'P.False', then
-- the /@cell@/ is allocated no more space than it needs. Any unused space is divided
-- evenly between cells for which /@expand@/ is 'P.True'.
treeViewColumnPackStart ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a, Gtk.CellRenderer.IsCellRenderer b) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'.
    -> b
    -- ^ /@cell@/: The t'GI.Gtk.Objects.CellRenderer.CellRenderer'.
    -> Bool
    -- ^ /@expand@/: 'P.True' if /@cell@/ is to be given extra space allocated to /@treeColumn@/.
    -> m ()
treeViewColumnPackStart treeColumn cell expand = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    cell' <- unsafeManagedPtrCastPtr cell
    let expand' = (fromIntegral . fromEnum) expand
    gtk_tree_view_column_pack_start treeColumn' cell' expand'
    touchManagedPtr treeColumn
    touchManagedPtr cell
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnPackStartMethodInfo
instance (signature ~ (b -> Bool -> m ()), MonadIO m, IsTreeViewColumn a, Gtk.CellRenderer.IsCellRenderer b) => O.OverloadedMethod TreeViewColumnPackStartMethodInfo a signature where
    overloadedMethod = treeViewColumnPackStart

instance O.OverloadedMethodInfo TreeViewColumnPackStartMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnPackStart",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnPackStart"
        })


#endif

-- method TreeViewColumn::queue_resize
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn"
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

foreign import ccall "gtk_tree_view_column_queue_resize" gtk_tree_view_column_queue_resize :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    IO ()

-- | Flags the column, and the cell renderers added to this column, to have
-- their sizes renegotiated.
-- 
-- /Since: 2.8/
treeViewColumnQueueResize ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'
    -> m ()
treeViewColumnQueueResize treeColumn = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    gtk_tree_view_column_queue_resize treeColumn'
    touchManagedPtr treeColumn
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnQueueResizeMethodInfo
instance (signature ~ (m ()), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnQueueResizeMethodInfo a signature where
    overloadedMethod = treeViewColumnQueueResize

instance O.OverloadedMethodInfo TreeViewColumnQueueResizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnQueueResize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnQueueResize"
        })


#endif

-- method TreeViewColumn::set_alignment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "xalign"
--           , argType = TBasicType TFloat
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The alignment, which is between [0.0 and 1.0] inclusive."
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

foreign import ccall "gtk_tree_view_column_set_alignment" gtk_tree_view_column_set_alignment :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    CFloat ->                               -- xalign : TBasicType TFloat
    IO ()

-- | Sets the alignment of the title or custom widget inside the column header.
-- The alignment determines its location inside the button -- 0.0 for left, 0.5
-- for center, 1.0 for right.
treeViewColumnSetAlignment ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'.
    -> Float
    -- ^ /@xalign@/: The alignment, which is between [0.0 and 1.0] inclusive.
    -> m ()
treeViewColumnSetAlignment treeColumn xalign = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    let xalign' = realToFrac xalign
    gtk_tree_view_column_set_alignment treeColumn' xalign'
    touchManagedPtr treeColumn
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSetAlignmentMethodInfo
instance (signature ~ (Float -> m ()), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnSetAlignmentMethodInfo a signature where
    overloadedMethod = treeViewColumnSetAlignment

instance O.OverloadedMethodInfo TreeViewColumnSetAlignmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnSetAlignment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnSetAlignment"
        })


#endif

-- method TreeViewColumn::set_cell_data_func
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cell_renderer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkCellRenderer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "func"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeCellDataFunc" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The #GtkTreeCellDataFunc to use."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeNotified
--           , argClosure = 3
--           , argDestroy = 4
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "func_data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The user data for @func."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "destroy"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "DestroyNotify" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The destroy notification for @func_data"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeAsync
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

foreign import ccall "gtk_tree_view_column_set_cell_data_func" gtk_tree_view_column_set_cell_data_func :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- cell_renderer : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    FunPtr Gtk.Callbacks.C_TreeCellDataFunc -> -- func : TInterface (Name {namespace = "Gtk", name = "TreeCellDataFunc"})
    Ptr () ->                               -- func_data : TBasicType TPtr
    FunPtr GLib.Callbacks.C_DestroyNotify -> -- destroy : TInterface (Name {namespace = "GLib", name = "DestroyNotify"})
    IO ()

-- | Sets the t'GI.Gtk.Callbacks.TreeCellDataFunc' to use for the column.  This
-- function is used instead of the standard attributes mapping for
-- setting the column value, and should set the value of /@treeColumn@/\'s
-- cell renderer as appropriate.  /@func@/ may be 'P.Nothing' to remove an
-- older one.
treeViewColumnSetCellDataFunc ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a, Gtk.CellRenderer.IsCellRenderer b) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'
    -> b
    -- ^ /@cellRenderer@/: A t'GI.Gtk.Objects.CellRenderer.CellRenderer'
    -> Maybe (Gtk.Callbacks.TreeCellDataFunc)
    -- ^ /@func@/: The t'GI.Gtk.Callbacks.TreeCellDataFunc' to use.
    -> m ()
treeViewColumnSetCellDataFunc treeColumn cellRenderer func = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    cellRenderer' <- unsafeManagedPtrCastPtr cellRenderer
    maybeFunc <- case func of
        Nothing -> return (castPtrToFunPtr nullPtr)
        Just jFunc -> do
            jFunc' <- Gtk.Callbacks.mk_TreeCellDataFunc (Gtk.Callbacks.wrap_TreeCellDataFunc Nothing (Gtk.Callbacks.drop_closures_TreeCellDataFunc jFunc))
            return jFunc'
    let funcData = castFunPtrToPtr maybeFunc
    let destroy = SP.safeFreeFunPtrPtr
    gtk_tree_view_column_set_cell_data_func treeColumn' cellRenderer' maybeFunc funcData destroy
    touchManagedPtr treeColumn
    touchManagedPtr cellRenderer
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSetCellDataFuncMethodInfo
instance (signature ~ (b -> Maybe (Gtk.Callbacks.TreeCellDataFunc) -> m ()), MonadIO m, IsTreeViewColumn a, Gtk.CellRenderer.IsCellRenderer b) => O.OverloadedMethod TreeViewColumnSetCellDataFuncMethodInfo a signature where
    overloadedMethod = treeViewColumnSetCellDataFunc

instance O.OverloadedMethodInfo TreeViewColumnSetCellDataFuncMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnSetCellDataFunc",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnSetCellDataFunc"
        })


#endif

-- method TreeViewColumn::set_clickable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "clickable"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE if the header is active."
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

foreign import ccall "gtk_tree_view_column_set_clickable" gtk_tree_view_column_set_clickable :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    CInt ->                                 -- clickable : TBasicType TBoolean
    IO ()

-- | Sets the header to be active if /@clickable@/ is 'P.True'.  When the header is
-- active, then it can take keyboard focus, and can be clicked.
treeViewColumnSetClickable ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'.
    -> Bool
    -- ^ /@clickable@/: 'P.True' if the header is active.
    -> m ()
treeViewColumnSetClickable treeColumn clickable = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    let clickable' = (fromIntegral . fromEnum) clickable
    gtk_tree_view_column_set_clickable treeColumn' clickable'
    touchManagedPtr treeColumn
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSetClickableMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnSetClickableMethodInfo a signature where
    overloadedMethod = treeViewColumnSetClickable

instance O.OverloadedMethodInfo TreeViewColumnSetClickableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnSetClickable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnSetClickable"
        })


#endif

-- method TreeViewColumn::set_expand
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "expand"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "%TRUE if the column should expand to fill available space."
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

foreign import ccall "gtk_tree_view_column_set_expand" gtk_tree_view_column_set_expand :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    CInt ->                                 -- expand : TBasicType TBoolean
    IO ()

-- | Sets the column to take available extra space.  This space is shared equally
-- amongst all columns that have the expand set to 'P.True'.  If no column has this
-- option set, then the last column gets all extra space.  By default, every
-- column is created with this 'P.False'.
-- 
-- Along with “fixed-width”, the “expand” property changes when the column is
-- resized by the user.
-- 
-- /Since: 2.4/
treeViewColumnSetExpand ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'.
    -> Bool
    -- ^ /@expand@/: 'P.True' if the column should expand to fill available space.
    -> m ()
treeViewColumnSetExpand treeColumn expand = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    let expand' = (fromIntegral . fromEnum) expand
    gtk_tree_view_column_set_expand treeColumn' expand'
    touchManagedPtr treeColumn
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSetExpandMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnSetExpandMethodInfo a signature where
    overloadedMethod = treeViewColumnSetExpand

instance O.OverloadedMethodInfo TreeViewColumnSetExpandMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnSetExpand",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnSetExpand"
        })


#endif

-- method TreeViewColumn::set_fixed_width
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "fixed_width"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The new fixed width, in pixels, or -1."
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

foreign import ccall "gtk_tree_view_column_set_fixed_width" gtk_tree_view_column_set_fixed_width :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    Int32 ->                                -- fixed_width : TBasicType TInt
    IO ()

-- | If /@fixedWidth@/ is not -1, sets the fixed width of /@treeColumn@/; otherwise
-- unsets it.  The effective value of /@fixedWidth@/ is clamped between the
-- minimum and maximum width of the column; however, the value stored in the
-- “fixed-width” property is not clamped.  If the column sizing is
-- @/GTK_TREE_VIEW_COLUMN_GROW_ONLY/@ or @/GTK_TREE_VIEW_COLUMN_AUTOSIZE/@, setting
-- a fixed width overrides the automatically calculated width.  Note that
-- /@fixedWidth@/ is only a hint to GTK+; the width actually allocated to the
-- column may be greater or less than requested.
-- 
-- Along with “expand”, the “fixed-width” property changes when the column is
-- resized by the user.
treeViewColumnSetFixedWidth ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'.
    -> Int32
    -- ^ /@fixedWidth@/: The new fixed width, in pixels, or -1.
    -> m ()
treeViewColumnSetFixedWidth treeColumn fixedWidth = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    gtk_tree_view_column_set_fixed_width treeColumn' fixedWidth
    touchManagedPtr treeColumn
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSetFixedWidthMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnSetFixedWidthMethodInfo a signature where
    overloadedMethod = treeViewColumnSetFixedWidth

instance O.OverloadedMethodInfo TreeViewColumnSetFixedWidthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnSetFixedWidth",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnSetFixedWidth"
        })


#endif

-- method TreeViewColumn::set_max_width
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "max_width"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The maximum width of the column in pixels, or -1."
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

foreign import ccall "gtk_tree_view_column_set_max_width" gtk_tree_view_column_set_max_width :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    Int32 ->                                -- max_width : TBasicType TInt
    IO ()

-- | Sets the maximum width of the /@treeColumn@/.  If /@maxWidth@/ is -1, then the
-- maximum width is unset.  Note, the column can actually be wider than max
-- width if it’s the last column in a view.  In this case, the column expands to
-- fill any extra space.
treeViewColumnSetMaxWidth ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'.
    -> Int32
    -- ^ /@maxWidth@/: The maximum width of the column in pixels, or -1.
    -> m ()
treeViewColumnSetMaxWidth treeColumn maxWidth = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    gtk_tree_view_column_set_max_width treeColumn' maxWidth
    touchManagedPtr treeColumn
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSetMaxWidthMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnSetMaxWidthMethodInfo a signature where
    overloadedMethod = treeViewColumnSetMaxWidth

instance O.OverloadedMethodInfo TreeViewColumnSetMaxWidthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnSetMaxWidth",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnSetMaxWidth"
        })


#endif

-- method TreeViewColumn::set_min_width
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "min_width"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The minimum width of the column in pixels, or -1."
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

foreign import ccall "gtk_tree_view_column_set_min_width" gtk_tree_view_column_set_min_width :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    Int32 ->                                -- min_width : TBasicType TInt
    IO ()

-- | Sets the minimum width of the /@treeColumn@/.  If /@minWidth@/ is -1, then the
-- minimum width is unset.
treeViewColumnSetMinWidth ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'.
    -> Int32
    -- ^ /@minWidth@/: The minimum width of the column in pixels, or -1.
    -> m ()
treeViewColumnSetMinWidth treeColumn minWidth = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    gtk_tree_view_column_set_min_width treeColumn' minWidth
    touchManagedPtr treeColumn
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSetMinWidthMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnSetMinWidthMethodInfo a signature where
    overloadedMethod = treeViewColumnSetMinWidth

instance O.OverloadedMethodInfo TreeViewColumnSetMinWidthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnSetMinWidth",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnSetMinWidth"
        })


#endif

-- method TreeViewColumn::set_reorderable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "reorderable"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE, if the column can be reordered."
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

foreign import ccall "gtk_tree_view_column_set_reorderable" gtk_tree_view_column_set_reorderable :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    CInt ->                                 -- reorderable : TBasicType TBoolean
    IO ()

-- | If /@reorderable@/ is 'P.True', then the column can be reordered by the end user
-- dragging the header.
treeViewColumnSetReorderable ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'
    -> Bool
    -- ^ /@reorderable@/: 'P.True', if the column can be reordered.
    -> m ()
treeViewColumnSetReorderable treeColumn reorderable = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    let reorderable' = (fromIntegral . fromEnum) reorderable
    gtk_tree_view_column_set_reorderable treeColumn' reorderable'
    touchManagedPtr treeColumn
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSetReorderableMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnSetReorderableMethodInfo a signature where
    overloadedMethod = treeViewColumnSetReorderable

instance O.OverloadedMethodInfo TreeViewColumnSetReorderableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnSetReorderable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnSetReorderable"
        })


#endif

-- method TreeViewColumn::set_resizable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "resizable"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE, if the column can be resized"
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

foreign import ccall "gtk_tree_view_column_set_resizable" gtk_tree_view_column_set_resizable :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    CInt ->                                 -- resizable : TBasicType TBoolean
    IO ()

-- | If /@resizable@/ is 'P.True', then the user can explicitly resize the column by
-- grabbing the outer edge of the column button.  If resizable is 'P.True' and
-- sizing mode of the column is @/GTK_TREE_VIEW_COLUMN_AUTOSIZE/@, then the sizing
-- mode is changed to @/GTK_TREE_VIEW_COLUMN_GROW_ONLY/@.
treeViewColumnSetResizable ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'
    -> Bool
    -- ^ /@resizable@/: 'P.True', if the column can be resized
    -> m ()
treeViewColumnSetResizable treeColumn resizable = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    let resizable' = (fromIntegral . fromEnum) resizable
    gtk_tree_view_column_set_resizable treeColumn' resizable'
    touchManagedPtr treeColumn
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSetResizableMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnSetResizableMethodInfo a signature where
    overloadedMethod = treeViewColumnSetResizable

instance O.OverloadedMethodInfo TreeViewColumnSetResizableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnSetResizable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnSetResizable"
        })


#endif

-- method TreeViewColumn::set_sizing
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "type"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "TreeViewColumnSizing" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The #GtkTreeViewColumnSizing."
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

foreign import ccall "gtk_tree_view_column_set_sizing" gtk_tree_view_column_set_sizing :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    CUInt ->                                -- type : TInterface (Name {namespace = "Gtk", name = "TreeViewColumnSizing"})
    IO ()

-- | Sets the growth behavior of /@treeColumn@/ to /@type@/.
treeViewColumnSetSizing ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'.
    -> Gtk.Enums.TreeViewColumnSizing
    -- ^ /@type@/: The t'GI.Gtk.Enums.TreeViewColumnSizing'.
    -> m ()
treeViewColumnSetSizing treeColumn type_ = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    let type_' = (fromIntegral . fromEnum) type_
    gtk_tree_view_column_set_sizing treeColumn' type_'
    touchManagedPtr treeColumn
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSetSizingMethodInfo
instance (signature ~ (Gtk.Enums.TreeViewColumnSizing -> m ()), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnSetSizingMethodInfo a signature where
    overloadedMethod = treeViewColumnSetSizing

instance O.OverloadedMethodInfo TreeViewColumnSetSizingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnSetSizing",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnSetSizing"
        })


#endif

-- method TreeViewColumn::set_sort_column_id
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeViewColumn"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "sort_column_id"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The @sort_column_id of the model to sort on."
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

foreign import ccall "gtk_tree_view_column_set_sort_column_id" gtk_tree_view_column_set_sort_column_id :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    Int32 ->                                -- sort_column_id : TBasicType TInt
    IO ()

-- | Sets the logical /@sortColumnId@/ that this column sorts on when this column
-- is selected for sorting.  Doing so makes the column header clickable.
treeViewColumnSetSortColumnId ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: a t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'
    -> Int32
    -- ^ /@sortColumnId@/: The /@sortColumnId@/ of the model to sort on.
    -> m ()
treeViewColumnSetSortColumnId treeColumn sortColumnId = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    gtk_tree_view_column_set_sort_column_id treeColumn' sortColumnId
    touchManagedPtr treeColumn
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSetSortColumnIdMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnSetSortColumnIdMethodInfo a signature where
    overloadedMethod = treeViewColumnSetSortColumnId

instance O.OverloadedMethodInfo TreeViewColumnSetSortColumnIdMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnSetSortColumnId",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnSetSortColumnId"
        })


#endif

-- method TreeViewColumn::set_sort_indicator
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeViewColumn"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "setting"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "%TRUE to display an indicator that the column is sorted"
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

foreign import ccall "gtk_tree_view_column_set_sort_indicator" gtk_tree_view_column_set_sort_indicator :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | Call this function with a /@setting@/ of 'P.True' to display an arrow in
-- the header button indicating the column is sorted. Call
-- 'GI.Gtk.Objects.TreeViewColumn.treeViewColumnSetSortOrder' to change the direction of
-- the arrow.
treeViewColumnSetSortIndicator ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: a t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'
    -> Bool
    -- ^ /@setting@/: 'P.True' to display an indicator that the column is sorted
    -> m ()
treeViewColumnSetSortIndicator treeColumn setting = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    let setting' = (fromIntegral . fromEnum) setting
    gtk_tree_view_column_set_sort_indicator treeColumn' setting'
    touchManagedPtr treeColumn
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSetSortIndicatorMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnSetSortIndicatorMethodInfo a signature where
    overloadedMethod = treeViewColumnSetSortIndicator

instance O.OverloadedMethodInfo TreeViewColumnSetSortIndicatorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnSetSortIndicator",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnSetSortIndicator"
        })


#endif

-- method TreeViewColumn::set_sort_order
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeViewColumn"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "order"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SortType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "sort order that the sort indicator should indicate"
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

foreign import ccall "gtk_tree_view_column_set_sort_order" gtk_tree_view_column_set_sort_order :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    CUInt ->                                -- order : TInterface (Name {namespace = "Gtk", name = "SortType"})
    IO ()

-- | Changes the appearance of the sort indicator.
-- 
-- This does not actually sort the model.  Use
-- 'GI.Gtk.Objects.TreeViewColumn.treeViewColumnSetSortColumnId' if you want automatic sorting
-- support.  This function is primarily for custom sorting behavior, and should
-- be used in conjunction with 'GI.Gtk.Interfaces.TreeSortable.treeSortableSetSortColumnId' to do
-- that. For custom models, the mechanism will vary.
-- 
-- The sort indicator changes direction to indicate normal sort or reverse sort.
-- Note that you must have the sort indicator enabled to see anything when
-- calling this function; see 'GI.Gtk.Objects.TreeViewColumn.treeViewColumnSetSortIndicator'.
treeViewColumnSetSortOrder ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: a t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'
    -> Gtk.Enums.SortType
    -- ^ /@order@/: sort order that the sort indicator should indicate
    -> m ()
treeViewColumnSetSortOrder treeColumn order = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    let order' = (fromIntegral . fromEnum) order
    gtk_tree_view_column_set_sort_order treeColumn' order'
    touchManagedPtr treeColumn
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSetSortOrderMethodInfo
instance (signature ~ (Gtk.Enums.SortType -> m ()), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnSetSortOrderMethodInfo a signature where
    overloadedMethod = treeViewColumnSetSortOrder

instance O.OverloadedMethodInfo TreeViewColumnSetSortOrderMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnSetSortOrder",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnSetSortOrder"
        })


#endif

-- method TreeViewColumn::set_spacing
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "spacing"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "distance between cell renderers in pixels."
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

foreign import ccall "gtk_tree_view_column_set_spacing" gtk_tree_view_column_set_spacing :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    Int32 ->                                -- spacing : TBasicType TInt
    IO ()

-- | Sets the spacing field of /@treeColumn@/, which is the number of pixels to
-- place between cell renderers packed into it.
treeViewColumnSetSpacing ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'.
    -> Int32
    -- ^ /@spacing@/: distance between cell renderers in pixels.
    -> m ()
treeViewColumnSetSpacing treeColumn spacing = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    gtk_tree_view_column_set_spacing treeColumn' spacing
    touchManagedPtr treeColumn
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSetSpacingMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnSetSpacingMethodInfo a signature where
    overloadedMethod = treeViewColumnSetSpacing

instance O.OverloadedMethodInfo TreeViewColumnSetSpacingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnSetSpacing",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnSetSpacing"
        })


#endif

-- method TreeViewColumn::set_title
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "title"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The title of the @tree_column."
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

foreign import ccall "gtk_tree_view_column_set_title" gtk_tree_view_column_set_title :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    CString ->                              -- title : TBasicType TUTF8
    IO ()

-- | Sets the title of the /@treeColumn@/.  If a custom widget has been set, then
-- this value is ignored.
treeViewColumnSetTitle ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'.
    -> T.Text
    -- ^ /@title@/: The title of the /@treeColumn@/.
    -> m ()
treeViewColumnSetTitle treeColumn title = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    title' <- textToCString title
    gtk_tree_view_column_set_title treeColumn' title'
    touchManagedPtr treeColumn
    freeMem title'
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSetTitleMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnSetTitleMethodInfo a signature where
    overloadedMethod = treeViewColumnSetTitle

instance O.OverloadedMethodInfo TreeViewColumnSetTitleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnSetTitle",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnSetTitle"
        })


#endif

-- method TreeViewColumn::set_visible
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "visible"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE if the @tree_column is visible."
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

foreign import ccall "gtk_tree_view_column_set_visible" gtk_tree_view_column_set_visible :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    CInt ->                                 -- visible : TBasicType TBoolean
    IO ()

-- | Sets the visibility of /@treeColumn@/.
treeViewColumnSetVisible ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'.
    -> Bool
    -- ^ /@visible@/: 'P.True' if the /@treeColumn@/ is visible.
    -> m ()
treeViewColumnSetVisible treeColumn visible = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    let visible' = (fromIntegral . fromEnum) visible
    gtk_tree_view_column_set_visible treeColumn' visible'
    touchManagedPtr treeColumn
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSetVisibleMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsTreeViewColumn a) => O.OverloadedMethod TreeViewColumnSetVisibleMethodInfo a signature where
    overloadedMethod = treeViewColumnSetVisible

instance O.OverloadedMethodInfo TreeViewColumnSetVisibleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnSetVisible",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnSetVisible"
        })


#endif

-- method TreeViewColumn::set_widget
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A child #GtkWidget, or %NULL."
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

foreign import ccall "gtk_tree_view_column_set_widget" gtk_tree_view_column_set_widget :: 
    Ptr TreeViewColumn ->                   -- tree_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | Sets the widget in the header to be /@widget@/.  If widget is 'P.Nothing', then the
-- header button is set with a t'GI.Gtk.Objects.Label.Label' set to the title of /@treeColumn@/.
treeViewColumnSetWidget ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeViewColumn a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@treeColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn'.
    -> Maybe (b)
    -- ^ /@widget@/: A child t'GI.Gtk.Objects.Widget.Widget', or 'P.Nothing'.
    -> m ()
treeViewColumnSetWidget treeColumn widget = liftIO $ do
    treeColumn' <- unsafeManagedPtrCastPtr treeColumn
    maybeWidget <- case widget of
        Nothing -> return nullPtr
        Just jWidget -> do
            jWidget' <- unsafeManagedPtrCastPtr jWidget
            return jWidget'
    gtk_tree_view_column_set_widget treeColumn' maybeWidget
    touchManagedPtr treeColumn
    whenJust widget touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnSetWidgetMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsTreeViewColumn a, Gtk.Widget.IsWidget b) => O.OverloadedMethod TreeViewColumnSetWidgetMethodInfo a signature where
    overloadedMethod = treeViewColumnSetWidget

instance O.OverloadedMethodInfo TreeViewColumnSetWidgetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeViewColumn.treeViewColumnSetWidget",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeViewColumn.html#v:treeViewColumnSetWidget"
        })


#endif


