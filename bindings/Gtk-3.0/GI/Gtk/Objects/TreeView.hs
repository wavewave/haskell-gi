{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- Widget that displays any object that implements the t'GI.Gtk.Interfaces.TreeModel.TreeModel' interface.
-- 
-- Please refer to the
-- [tree widget conceptual overview][TreeWidget]
-- for an overview of all the objects and data types related
-- to the tree widget and how they work together.
-- 
-- Several different coordinate systems are exposed in the GtkTreeView API.
-- These are:
-- 
-- <<https://developer.gnome.org/gtk3/stable/tree-view-coordinates.png>>
-- 
-- Coordinate systems in GtkTreeView API:
-- 
-- * Widget coordinates: Coordinates relative to the widget (usually @widget->window@).
-- * Bin window coordinates: Coordinates relative to the window that GtkTreeView renders to.
-- * Tree coordinates: Coordinates relative to the entire scrollable area of GtkTreeView. These
-- coordinates start at (0, 0) for row 0 of the tree.
-- 
-- 
-- Several functions are available for converting between the different
-- coordinate systems.  The most common translations are between widget and bin
-- window coordinates and between bin window and tree coordinates. For the
-- former you can use 'GI.Gtk.Objects.TreeView.treeViewConvertWidgetToBinWindowCoords'
-- (and vice versa), for the latter 'GI.Gtk.Objects.TreeView.treeViewConvertBinWindowToTreeCoords'
-- (and vice versa).
-- 
-- = GtkTreeView as GtkBuildable
-- 
-- The GtkTreeView implementation of the GtkBuildable interface accepts
-- t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn' objects as @\<child>@ elements and exposes the internal
-- t'GI.Gtk.Objects.TreeSelection.TreeSelection' in UI definitions.
-- 
-- An example of a UI definition fragment with GtkTreeView:
-- 
-- 
-- === /xml code/
-- >
-- ><object class="GtkTreeView" id="treeview">
-- >  <property name="model">liststore1</property>
-- >  <child>
-- >    <object class="GtkTreeViewColumn" id="test-column">
-- >      <property name="title">Test</property>
-- >      <child>
-- >        <object class="GtkCellRendererText" id="test-renderer"/>
-- >        <attributes>
-- >          <attribute name="text">1</attribute>
-- >        </attributes>
-- >      </child>
-- >    </object>
-- >  </child>
-- >  <child internal-child="selection">
-- >    <object class="GtkTreeSelection" id="selection">
-- >      <signal name="changed" handler="on_treeview_selection_changed"/>
-- >    </object>
-- >  </child>
-- ></object>
-- 
-- 
-- = CSS nodes
-- 
-- 
-- === /plain code/
-- >
-- >treeview.view
-- >├── header
-- >│   ├── <column header>
-- >┊   ┊
-- >│   ╰── <column header>
-- >│
-- >╰── [rubberband]
-- 
-- 
-- GtkTreeView has a main CSS node with name treeview and style class .view.
-- It has a subnode with name header, which is the parent for all the column
-- header widgets\' CSS nodes.
-- For rubberband selection, a subnode with name rubberband is used.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.TreeView
    ( 

-- * Exported types
    TreeView(..)                            ,
    IsTreeView                              ,
    toTreeView                              ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [appendColumn]("GI.Gtk.Objects.TreeView#g:method:appendColumn"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [collapseAll]("GI.Gtk.Objects.TreeView#g:method:collapseAll"), [collapseRow]("GI.Gtk.Objects.TreeView#g:method:collapseRow"), [columnsAutosize]("GI.Gtk.Objects.TreeView#g:method:columnsAutosize"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [convertBinWindowToTreeCoords]("GI.Gtk.Objects.TreeView#g:method:convertBinWindowToTreeCoords"), [convertBinWindowToWidgetCoords]("GI.Gtk.Objects.TreeView#g:method:convertBinWindowToWidgetCoords"), [convertTreeToBinWindowCoords]("GI.Gtk.Objects.TreeView#g:method:convertTreeToBinWindowCoords"), [convertTreeToWidgetCoords]("GI.Gtk.Objects.TreeView#g:method:convertTreeToWidgetCoords"), [convertWidgetToBinWindowCoords]("GI.Gtk.Objects.TreeView#g:method:convertWidgetToBinWindowCoords"), [convertWidgetToTreeCoords]("GI.Gtk.Objects.TreeView#g:method:convertWidgetToTreeCoords"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [createRowDragIcon]("GI.Gtk.Objects.TreeView#g:method:createRowDragIcon"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [enableModelDragDest]("GI.Gtk.Objects.TreeView#g:method:enableModelDragDest"), [enableModelDragSource]("GI.Gtk.Objects.TreeView#g:method:enableModelDragSource"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [expandAll]("GI.Gtk.Objects.TreeView#g:method:expandAll"), [expandRow]("GI.Gtk.Objects.TreeView#g:method:expandRow"), [expandToPath]("GI.Gtk.Objects.TreeView#g:method:expandToPath"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [insertColumn]("GI.Gtk.Objects.TreeView#g:method:insertColumn"), [insertColumnWithDataFunc]("GI.Gtk.Objects.TreeView#g:method:insertColumnWithDataFunc"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isBlankAtPos]("GI.Gtk.Objects.TreeView#g:method:isBlankAtPos"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isRubberBandingActive]("GI.Gtk.Objects.TreeView#g:method:isRubberBandingActive"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mapExpandedRows]("GI.Gtk.Objects.TreeView#g:method:mapExpandedRows"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [moveColumnAfter]("GI.Gtk.Objects.TreeView#g:method:moveColumnAfter"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeColumn]("GI.Gtk.Objects.TreeView#g:method:removeColumn"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [rowActivated]("GI.Gtk.Objects.TreeView#g:method:rowActivated"), [rowExpanded]("GI.Gtk.Objects.TreeView#g:method:rowExpanded"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [scrollToCell]("GI.Gtk.Objects.TreeView#g:method:scrollToCell"), [scrollToPoint]("GI.Gtk.Objects.TreeView#g:method:scrollToPoint"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetRowsDragDest]("GI.Gtk.Objects.TreeView#g:method:unsetRowsDragDest"), [unsetRowsDragSource]("GI.Gtk.Objects.TreeView#g:method:unsetRowsDragSource"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getActivateOnSingleClick]("GI.Gtk.Objects.TreeView#g:method:getActivateOnSingleClick"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getBackgroundArea]("GI.Gtk.Objects.TreeView#g:method:getBackgroundArea"), [getBinWindow]("GI.Gtk.Objects.TreeView#g:method:getBinWindow"), [getBorder]("GI.Gtk.Interfaces.Scrollable#g:method:getBorder"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getCellArea]("GI.Gtk.Objects.TreeView#g:method:getCellArea"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getColumn]("GI.Gtk.Objects.TreeView#g:method:getColumn"), [getColumns]("GI.Gtk.Objects.TreeView#g:method:getColumns"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getCursor]("GI.Gtk.Objects.TreeView#g:method:getCursor"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDestRowAtPos]("GI.Gtk.Objects.TreeView#g:method:getDestRowAtPos"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getDragDestRow]("GI.Gtk.Objects.TreeView#g:method:getDragDestRow"), [getEnableSearch]("GI.Gtk.Objects.TreeView#g:method:getEnableSearch"), [getEnableTreeLines]("GI.Gtk.Objects.TreeView#g:method:getEnableTreeLines"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getExpanderColumn]("GI.Gtk.Objects.TreeView#g:method:getExpanderColumn"), [getFixedHeightMode]("GI.Gtk.Objects.TreeView#g:method:getFixedHeightMode"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getGridLines]("GI.Gtk.Objects.TreeView#g:method:getGridLines"), [getHadjustment]("GI.Gtk.Objects.TreeView#g:method:getHadjustment"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHeadersClickable]("GI.Gtk.Objects.TreeView#g:method:getHeadersClickable"), [getHeadersVisible]("GI.Gtk.Objects.TreeView#g:method:getHeadersVisible"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getHoverExpand]("GI.Gtk.Objects.TreeView#g:method:getHoverExpand"), [getHoverSelection]("GI.Gtk.Objects.TreeView#g:method:getHoverSelection"), [getHscrollPolicy]("GI.Gtk.Interfaces.Scrollable#g:method:getHscrollPolicy"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getLevelIndentation]("GI.Gtk.Objects.TreeView#g:method:getLevelIndentation"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getModel]("GI.Gtk.Objects.TreeView#g:method:getModel"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getNColumns]("GI.Gtk.Objects.TreeView#g:method:getNColumns"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathAtPos]("GI.Gtk.Objects.TreeView#g:method:getPathAtPos"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getReorderable]("GI.Gtk.Objects.TreeView#g:method:getReorderable"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getRubberBanding]("GI.Gtk.Objects.TreeView#g:method:getRubberBanding"), [getRulesHint]("GI.Gtk.Objects.TreeView#g:method:getRulesHint"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSearchColumn]("GI.Gtk.Objects.TreeView#g:method:getSearchColumn"), [getSearchEntry]("GI.Gtk.Objects.TreeView#g:method:getSearchEntry"), [getSelection]("GI.Gtk.Objects.TreeView#g:method:getSelection"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getShowExpanders]("GI.Gtk.Objects.TreeView#g:method:getShowExpanders"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipColumn]("GI.Gtk.Objects.TreeView#g:method:getTooltipColumn"), [getTooltipContext]("GI.Gtk.Objects.TreeView#g:method:getTooltipContext"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getVadjustment]("GI.Gtk.Objects.TreeView#g:method:getVadjustment"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisibleRange]("GI.Gtk.Objects.TreeView#g:method:getVisibleRange"), [getVisibleRect]("GI.Gtk.Objects.TreeView#g:method:getVisibleRect"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getVscrollPolicy]("GI.Gtk.Interfaces.Scrollable#g:method:getVscrollPolicy"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setActivateOnSingleClick]("GI.Gtk.Objects.TreeView#g:method:setActivateOnSingleClick"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setColumnDragFunction]("GI.Gtk.Objects.TreeView#g:method:setColumnDragFunction"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setCursor]("GI.Gtk.Objects.TreeView#g:method:setCursor"), [setCursorOnCell]("GI.Gtk.Objects.TreeView#g:method:setCursorOnCell"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDestroyCountFunc]("GI.Gtk.Objects.TreeView#g:method:setDestroyCountFunc"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setDragDestRow]("GI.Gtk.Objects.TreeView#g:method:setDragDestRow"), [setEnableSearch]("GI.Gtk.Objects.TreeView#g:method:setEnableSearch"), [setEnableTreeLines]("GI.Gtk.Objects.TreeView#g:method:setEnableTreeLines"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setExpanderColumn]("GI.Gtk.Objects.TreeView#g:method:setExpanderColumn"), [setFixedHeightMode]("GI.Gtk.Objects.TreeView#g:method:setFixedHeightMode"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setGridLines]("GI.Gtk.Objects.TreeView#g:method:setGridLines"), [setHadjustment]("GI.Gtk.Objects.TreeView#g:method:setHadjustment"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHeadersClickable]("GI.Gtk.Objects.TreeView#g:method:setHeadersClickable"), [setHeadersVisible]("GI.Gtk.Objects.TreeView#g:method:setHeadersVisible"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setHoverExpand]("GI.Gtk.Objects.TreeView#g:method:setHoverExpand"), [setHoverSelection]("GI.Gtk.Objects.TreeView#g:method:setHoverSelection"), [setHscrollPolicy]("GI.Gtk.Interfaces.Scrollable#g:method:setHscrollPolicy"), [setLevelIndentation]("GI.Gtk.Objects.TreeView#g:method:setLevelIndentation"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setModel]("GI.Gtk.Objects.TreeView#g:method:setModel"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setReorderable]("GI.Gtk.Objects.TreeView#g:method:setReorderable"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setRowSeparatorFunc]("GI.Gtk.Objects.TreeView#g:method:setRowSeparatorFunc"), [setRubberBanding]("GI.Gtk.Objects.TreeView#g:method:setRubberBanding"), [setRulesHint]("GI.Gtk.Objects.TreeView#g:method:setRulesHint"), [setSearchColumn]("GI.Gtk.Objects.TreeView#g:method:setSearchColumn"), [setSearchEntry]("GI.Gtk.Objects.TreeView#g:method:setSearchEntry"), [setSearchEqualFunc]("GI.Gtk.Objects.TreeView#g:method:setSearchEqualFunc"), [setSearchPositionFunc]("GI.Gtk.Objects.TreeView#g:method:setSearchPositionFunc"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setShowExpanders]("GI.Gtk.Objects.TreeView#g:method:setShowExpanders"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTooltipCell]("GI.Gtk.Objects.TreeView#g:method:setTooltipCell"), [setTooltipColumn]("GI.Gtk.Objects.TreeView#g:method:setTooltipColumn"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipRow]("GI.Gtk.Objects.TreeView#g:method:setTooltipRow"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setVadjustment]("GI.Gtk.Objects.TreeView#g:method:setVadjustment"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setVscrollPolicy]("GI.Gtk.Interfaces.Scrollable#g:method:setVscrollPolicy"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveTreeViewMethod                   ,
#endif

-- ** appendColumn #method:appendColumn#

#if defined(ENABLE_OVERLOADING)
    TreeViewAppendColumnMethodInfo          ,
#endif
    treeViewAppendColumn                    ,


-- ** collapseAll #method:collapseAll#

#if defined(ENABLE_OVERLOADING)
    TreeViewCollapseAllMethodInfo           ,
#endif
    treeViewCollapseAll                     ,


-- ** collapseRow #method:collapseRow#

#if defined(ENABLE_OVERLOADING)
    TreeViewCollapseRowMethodInfo           ,
#endif
    treeViewCollapseRow                     ,


-- ** columnsAutosize #method:columnsAutosize#

#if defined(ENABLE_OVERLOADING)
    TreeViewColumnsAutosizeMethodInfo       ,
#endif
    treeViewColumnsAutosize                 ,


-- ** convertBinWindowToTreeCoords #method:convertBinWindowToTreeCoords#

#if defined(ENABLE_OVERLOADING)
    TreeViewConvertBinWindowToTreeCoordsMethodInfo,
#endif
    treeViewConvertBinWindowToTreeCoords    ,


-- ** convertBinWindowToWidgetCoords #method:convertBinWindowToWidgetCoords#

#if defined(ENABLE_OVERLOADING)
    TreeViewConvertBinWindowToWidgetCoordsMethodInfo,
#endif
    treeViewConvertBinWindowToWidgetCoords  ,


-- ** convertTreeToBinWindowCoords #method:convertTreeToBinWindowCoords#

#if defined(ENABLE_OVERLOADING)
    TreeViewConvertTreeToBinWindowCoordsMethodInfo,
#endif
    treeViewConvertTreeToBinWindowCoords    ,


-- ** convertTreeToWidgetCoords #method:convertTreeToWidgetCoords#

#if defined(ENABLE_OVERLOADING)
    TreeViewConvertTreeToWidgetCoordsMethodInfo,
#endif
    treeViewConvertTreeToWidgetCoords       ,


-- ** convertWidgetToBinWindowCoords #method:convertWidgetToBinWindowCoords#

#if defined(ENABLE_OVERLOADING)
    TreeViewConvertWidgetToBinWindowCoordsMethodInfo,
#endif
    treeViewConvertWidgetToBinWindowCoords  ,


-- ** convertWidgetToTreeCoords #method:convertWidgetToTreeCoords#

#if defined(ENABLE_OVERLOADING)
    TreeViewConvertWidgetToTreeCoordsMethodInfo,
#endif
    treeViewConvertWidgetToTreeCoords       ,


-- ** createRowDragIcon #method:createRowDragIcon#

#if defined(ENABLE_OVERLOADING)
    TreeViewCreateRowDragIconMethodInfo     ,
#endif
    treeViewCreateRowDragIcon               ,


-- ** enableModelDragDest #method:enableModelDragDest#

#if defined(ENABLE_OVERLOADING)
    TreeViewEnableModelDragDestMethodInfo   ,
#endif
    treeViewEnableModelDragDest             ,


-- ** enableModelDragSource #method:enableModelDragSource#

#if defined(ENABLE_OVERLOADING)
    TreeViewEnableModelDragSourceMethodInfo ,
#endif
    treeViewEnableModelDragSource           ,


-- ** expandAll #method:expandAll#

#if defined(ENABLE_OVERLOADING)
    TreeViewExpandAllMethodInfo             ,
#endif
    treeViewExpandAll                       ,


-- ** expandRow #method:expandRow#

#if defined(ENABLE_OVERLOADING)
    TreeViewExpandRowMethodInfo             ,
#endif
    treeViewExpandRow                       ,


-- ** expandToPath #method:expandToPath#

#if defined(ENABLE_OVERLOADING)
    TreeViewExpandToPathMethodInfo          ,
#endif
    treeViewExpandToPath                    ,


-- ** getActivateOnSingleClick #method:getActivateOnSingleClick#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetActivateOnSingleClickMethodInfo,
#endif
    treeViewGetActivateOnSingleClick        ,


-- ** getBackgroundArea #method:getBackgroundArea#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetBackgroundAreaMethodInfo     ,
#endif
    treeViewGetBackgroundArea               ,


-- ** getBinWindow #method:getBinWindow#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetBinWindowMethodInfo          ,
#endif
    treeViewGetBinWindow                    ,


-- ** getCellArea #method:getCellArea#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetCellAreaMethodInfo           ,
#endif
    treeViewGetCellArea                     ,


-- ** getColumn #method:getColumn#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetColumnMethodInfo             ,
#endif
    treeViewGetColumn                       ,


-- ** getColumns #method:getColumns#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetColumnsMethodInfo            ,
#endif
    treeViewGetColumns                      ,


-- ** getCursor #method:getCursor#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetCursorMethodInfo             ,
#endif
    treeViewGetCursor                       ,


-- ** getDestRowAtPos #method:getDestRowAtPos#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetDestRowAtPosMethodInfo       ,
#endif
    treeViewGetDestRowAtPos                 ,


-- ** getDragDestRow #method:getDragDestRow#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetDragDestRowMethodInfo        ,
#endif
    treeViewGetDragDestRow                  ,


-- ** getEnableSearch #method:getEnableSearch#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetEnableSearchMethodInfo       ,
#endif
    treeViewGetEnableSearch                 ,


-- ** getEnableTreeLines #method:getEnableTreeLines#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetEnableTreeLinesMethodInfo    ,
#endif
    treeViewGetEnableTreeLines              ,


-- ** getExpanderColumn #method:getExpanderColumn#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetExpanderColumnMethodInfo     ,
#endif
    treeViewGetExpanderColumn               ,


-- ** getFixedHeightMode #method:getFixedHeightMode#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetFixedHeightModeMethodInfo    ,
#endif
    treeViewGetFixedHeightMode              ,


-- ** getGridLines #method:getGridLines#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetGridLinesMethodInfo          ,
#endif
    treeViewGetGridLines                    ,


-- ** getHadjustment #method:getHadjustment#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetHadjustmentMethodInfo        ,
#endif
    treeViewGetHadjustment                  ,


-- ** getHeadersClickable #method:getHeadersClickable#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetHeadersClickableMethodInfo   ,
#endif
    treeViewGetHeadersClickable             ,


-- ** getHeadersVisible #method:getHeadersVisible#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetHeadersVisibleMethodInfo     ,
#endif
    treeViewGetHeadersVisible               ,


-- ** getHoverExpand #method:getHoverExpand#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetHoverExpandMethodInfo        ,
#endif
    treeViewGetHoverExpand                  ,


-- ** getHoverSelection #method:getHoverSelection#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetHoverSelectionMethodInfo     ,
#endif
    treeViewGetHoverSelection               ,


-- ** getLevelIndentation #method:getLevelIndentation#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetLevelIndentationMethodInfo   ,
#endif
    treeViewGetLevelIndentation             ,


-- ** getModel #method:getModel#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetModelMethodInfo              ,
#endif
    treeViewGetModel                        ,


-- ** getNColumns #method:getNColumns#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetNColumnsMethodInfo           ,
#endif
    treeViewGetNColumns                     ,


-- ** getPathAtPos #method:getPathAtPos#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetPathAtPosMethodInfo          ,
#endif
    treeViewGetPathAtPos                    ,


-- ** getReorderable #method:getReorderable#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetReorderableMethodInfo        ,
#endif
    treeViewGetReorderable                  ,


-- ** getRubberBanding #method:getRubberBanding#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetRubberBandingMethodInfo      ,
#endif
    treeViewGetRubberBanding                ,


-- ** getRulesHint #method:getRulesHint#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetRulesHintMethodInfo          ,
#endif
    treeViewGetRulesHint                    ,


-- ** getSearchColumn #method:getSearchColumn#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetSearchColumnMethodInfo       ,
#endif
    treeViewGetSearchColumn                 ,


-- ** getSearchEntry #method:getSearchEntry#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetSearchEntryMethodInfo        ,
#endif
    treeViewGetSearchEntry                  ,


-- ** getSelection #method:getSelection#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetSelectionMethodInfo          ,
#endif
    treeViewGetSelection                    ,


-- ** getShowExpanders #method:getShowExpanders#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetShowExpandersMethodInfo      ,
#endif
    treeViewGetShowExpanders                ,


-- ** getTooltipColumn #method:getTooltipColumn#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetTooltipColumnMethodInfo      ,
#endif
    treeViewGetTooltipColumn                ,


-- ** getTooltipContext #method:getTooltipContext#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetTooltipContextMethodInfo     ,
#endif
    treeViewGetTooltipContext               ,


-- ** getVadjustment #method:getVadjustment#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetVadjustmentMethodInfo        ,
#endif
    treeViewGetVadjustment                  ,


-- ** getVisibleRange #method:getVisibleRange#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetVisibleRangeMethodInfo       ,
#endif
    treeViewGetVisibleRange                 ,


-- ** getVisibleRect #method:getVisibleRect#

#if defined(ENABLE_OVERLOADING)
    TreeViewGetVisibleRectMethodInfo        ,
#endif
    treeViewGetVisibleRect                  ,


-- ** insertColumn #method:insertColumn#

#if defined(ENABLE_OVERLOADING)
    TreeViewInsertColumnMethodInfo          ,
#endif
    treeViewInsertColumn                    ,


-- ** insertColumnWithDataFunc #method:insertColumnWithDataFunc#

#if defined(ENABLE_OVERLOADING)
    TreeViewInsertColumnWithDataFuncMethodInfo,
#endif
    treeViewInsertColumnWithDataFunc        ,


-- ** isBlankAtPos #method:isBlankAtPos#

#if defined(ENABLE_OVERLOADING)
    TreeViewIsBlankAtPosMethodInfo          ,
#endif
    treeViewIsBlankAtPos                    ,


-- ** isRubberBandingActive #method:isRubberBandingActive#

#if defined(ENABLE_OVERLOADING)
    TreeViewIsRubberBandingActiveMethodInfo ,
#endif
    treeViewIsRubberBandingActive           ,


-- ** mapExpandedRows #method:mapExpandedRows#

#if defined(ENABLE_OVERLOADING)
    TreeViewMapExpandedRowsMethodInfo       ,
#endif
    treeViewMapExpandedRows                 ,


-- ** moveColumnAfter #method:moveColumnAfter#

#if defined(ENABLE_OVERLOADING)
    TreeViewMoveColumnAfterMethodInfo       ,
#endif
    treeViewMoveColumnAfter                 ,


-- ** new #method:new#

    treeViewNew                             ,


-- ** newWithModel #method:newWithModel#

    treeViewNewWithModel                    ,


-- ** removeColumn #method:removeColumn#

#if defined(ENABLE_OVERLOADING)
    TreeViewRemoveColumnMethodInfo          ,
#endif
    treeViewRemoveColumn                    ,


-- ** rowActivated #method:rowActivated#

#if defined(ENABLE_OVERLOADING)
    TreeViewRowActivatedMethodInfo          ,
#endif
    treeViewRowActivated                    ,


-- ** rowExpanded #method:rowExpanded#

#if defined(ENABLE_OVERLOADING)
    TreeViewRowExpandedMethodInfo           ,
#endif
    treeViewRowExpanded                     ,


-- ** scrollToCell #method:scrollToCell#

#if defined(ENABLE_OVERLOADING)
    TreeViewScrollToCellMethodInfo          ,
#endif
    treeViewScrollToCell                    ,


-- ** scrollToPoint #method:scrollToPoint#

#if defined(ENABLE_OVERLOADING)
    TreeViewScrollToPointMethodInfo         ,
#endif
    treeViewScrollToPoint                   ,


-- ** setActivateOnSingleClick #method:setActivateOnSingleClick#

#if defined(ENABLE_OVERLOADING)
    TreeViewSetActivateOnSingleClickMethodInfo,
#endif
    treeViewSetActivateOnSingleClick        ,


-- ** setColumnDragFunction #method:setColumnDragFunction#

#if defined(ENABLE_OVERLOADING)
    TreeViewSetColumnDragFunctionMethodInfo ,
#endif
    treeViewSetColumnDragFunction           ,


-- ** setCursor #method:setCursor#

#if defined(ENABLE_OVERLOADING)
    TreeViewSetCursorMethodInfo             ,
#endif
    treeViewSetCursor                       ,


-- ** setCursorOnCell #method:setCursorOnCell#

#if defined(ENABLE_OVERLOADING)
    TreeViewSetCursorOnCellMethodInfo       ,
#endif
    treeViewSetCursorOnCell                 ,


-- ** setDestroyCountFunc #method:setDestroyCountFunc#

#if defined(ENABLE_OVERLOADING)
    TreeViewSetDestroyCountFuncMethodInfo   ,
#endif
    treeViewSetDestroyCountFunc             ,


-- ** setDragDestRow #method:setDragDestRow#

#if defined(ENABLE_OVERLOADING)
    TreeViewSetDragDestRowMethodInfo        ,
#endif
    treeViewSetDragDestRow                  ,


-- ** setEnableSearch #method:setEnableSearch#

#if defined(ENABLE_OVERLOADING)
    TreeViewSetEnableSearchMethodInfo       ,
#endif
    treeViewSetEnableSearch                 ,


-- ** setEnableTreeLines #method:setEnableTreeLines#

#if defined(ENABLE_OVERLOADING)
    TreeViewSetEnableTreeLinesMethodInfo    ,
#endif
    treeViewSetEnableTreeLines              ,


-- ** setExpanderColumn #method:setExpanderColumn#

#if defined(ENABLE_OVERLOADING)
    TreeViewSetExpanderColumnMethodInfo     ,
#endif
    treeViewSetExpanderColumn               ,


-- ** setFixedHeightMode #method:setFixedHeightMode#

#if defined(ENABLE_OVERLOADING)
    TreeViewSetFixedHeightModeMethodInfo    ,
#endif
    treeViewSetFixedHeightMode              ,


-- ** setGridLines #method:setGridLines#

#if defined(ENABLE_OVERLOADING)
    TreeViewSetGridLinesMethodInfo          ,
#endif
    treeViewSetGridLines                    ,


-- ** setHadjustment #method:setHadjustment#

#if defined(ENABLE_OVERLOADING)
    TreeViewSetHadjustmentMethodInfo        ,
#endif
    treeViewSetHadjustment                  ,


-- ** setHeadersClickable #method:setHeadersClickable#

#if defined(ENABLE_OVERLOADING)
    TreeViewSetHeadersClickableMethodInfo   ,
#endif
    treeViewSetHeadersClickable             ,


-- ** setHeadersVisible #method:setHeadersVisible#

#if defined(ENABLE_OVERLOADING)
    TreeViewSetHeadersVisibleMethodInfo     ,
#endif
    treeViewSetHeadersVisible               ,


-- ** setHoverExpand #method:setHoverExpand#

#if defined(ENABLE_OVERLOADING)
    TreeViewSetHoverExpandMethodInfo        ,
#endif
    treeViewSetHoverExpand                  ,


-- ** setHoverSelection #method:setHoverSelection#

#if defined(ENABLE_OVERLOADING)
    TreeViewSetHoverSelectionMethodInfo     ,
#endif
    treeViewSetHoverSelection               ,


-- ** setLevelIndentation #method:setLevelIndentation#

#if defined(ENABLE_OVERLOADING)
    TreeViewSetLevelIndentationMethodInfo   ,
#endif
    treeViewSetLevelIndentation             ,


-- ** setModel #method:setModel#

#if defined(ENABLE_OVERLOADING)
    TreeViewSetModelMethodInfo              ,
#endif
    treeViewSetModel                        ,


-- ** setReorderable #method:setReorderable#

#if defined(ENABLE_OVERLOADING)
    TreeViewSetReorderableMethodInfo        ,
#endif
    treeViewSetReorderable                  ,


-- ** setRowSeparatorFunc #method:setRowSeparatorFunc#

#if defined(ENABLE_OVERLOADING)
    TreeViewSetRowSeparatorFuncMethodInfo   ,
#endif
    treeViewSetRowSeparatorFunc             ,


-- ** setRubberBanding #method:setRubberBanding#

#if defined(ENABLE_OVERLOADING)
    TreeViewSetRubberBandingMethodInfo      ,
#endif
    treeViewSetRubberBanding                ,


-- ** setRulesHint #method:setRulesHint#

#if defined(ENABLE_OVERLOADING)
    TreeViewSetRulesHintMethodInfo          ,
#endif
    treeViewSetRulesHint                    ,


-- ** setSearchColumn #method:setSearchColumn#

#if defined(ENABLE_OVERLOADING)
    TreeViewSetSearchColumnMethodInfo       ,
#endif
    treeViewSetSearchColumn                 ,


-- ** setSearchEntry #method:setSearchEntry#

#if defined(ENABLE_OVERLOADING)
    TreeViewSetSearchEntryMethodInfo        ,
#endif
    treeViewSetSearchEntry                  ,


-- ** setSearchEqualFunc #method:setSearchEqualFunc#

#if defined(ENABLE_OVERLOADING)
    TreeViewSetSearchEqualFuncMethodInfo    ,
#endif
    treeViewSetSearchEqualFunc              ,


-- ** setSearchPositionFunc #method:setSearchPositionFunc#

#if defined(ENABLE_OVERLOADING)
    TreeViewSetSearchPositionFuncMethodInfo ,
#endif
    treeViewSetSearchPositionFunc           ,


-- ** setShowExpanders #method:setShowExpanders#

#if defined(ENABLE_OVERLOADING)
    TreeViewSetShowExpandersMethodInfo      ,
#endif
    treeViewSetShowExpanders                ,


-- ** setTooltipCell #method:setTooltipCell#

#if defined(ENABLE_OVERLOADING)
    TreeViewSetTooltipCellMethodInfo        ,
#endif
    treeViewSetTooltipCell                  ,


-- ** setTooltipColumn #method:setTooltipColumn#

#if defined(ENABLE_OVERLOADING)
    TreeViewSetTooltipColumnMethodInfo      ,
#endif
    treeViewSetTooltipColumn                ,


-- ** setTooltipRow #method:setTooltipRow#

#if defined(ENABLE_OVERLOADING)
    TreeViewSetTooltipRowMethodInfo         ,
#endif
    treeViewSetTooltipRow                   ,


-- ** setVadjustment #method:setVadjustment#

#if defined(ENABLE_OVERLOADING)
    TreeViewSetVadjustmentMethodInfo        ,
#endif
    treeViewSetVadjustment                  ,


-- ** unsetRowsDragDest #method:unsetRowsDragDest#

#if defined(ENABLE_OVERLOADING)
    TreeViewUnsetRowsDragDestMethodInfo     ,
#endif
    treeViewUnsetRowsDragDest               ,


-- ** unsetRowsDragSource #method:unsetRowsDragSource#

#if defined(ENABLE_OVERLOADING)
    TreeViewUnsetRowsDragSourceMethodInfo   ,
#endif
    treeViewUnsetRowsDragSource             ,




 -- * Properties


-- ** activateOnSingleClick #attr:activateOnSingleClick#
-- | The activate-on-single-click property specifies whether the \"row-activated\" signal
-- will be emitted after a single click.
-- 
-- /Since: 3.8/

#if defined(ENABLE_OVERLOADING)
    TreeViewActivateOnSingleClickPropertyInfo,
#endif
    constructTreeViewActivateOnSingleClick  ,
    getTreeViewActivateOnSingleClick        ,
    setTreeViewActivateOnSingleClick        ,
#if defined(ENABLE_OVERLOADING)
    treeViewActivateOnSingleClick           ,
#endif


-- ** enableGridLines #attr:enableGridLines#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TreeViewEnableGridLinesPropertyInfo     ,
#endif
    constructTreeViewEnableGridLines        ,
    getTreeViewEnableGridLines              ,
    setTreeViewEnableGridLines              ,
#if defined(ENABLE_OVERLOADING)
    treeViewEnableGridLines                 ,
#endif


-- ** enableSearch #attr:enableSearch#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TreeViewEnableSearchPropertyInfo        ,
#endif
    constructTreeViewEnableSearch           ,
    getTreeViewEnableSearch                 ,
    setTreeViewEnableSearch                 ,
#if defined(ENABLE_OVERLOADING)
    treeViewEnableSearch                    ,
#endif


-- ** enableTreeLines #attr:enableTreeLines#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TreeViewEnableTreeLinesPropertyInfo     ,
#endif
    constructTreeViewEnableTreeLines        ,
    getTreeViewEnableTreeLines              ,
    setTreeViewEnableTreeLines              ,
#if defined(ENABLE_OVERLOADING)
    treeViewEnableTreeLines                 ,
#endif


-- ** expanderColumn #attr:expanderColumn#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TreeViewExpanderColumnPropertyInfo      ,
#endif
    clearTreeViewExpanderColumn             ,
    constructTreeViewExpanderColumn         ,
    getTreeViewExpanderColumn               ,
    setTreeViewExpanderColumn               ,
#if defined(ENABLE_OVERLOADING)
    treeViewExpanderColumn                  ,
#endif


-- ** fixedHeightMode #attr:fixedHeightMode#
-- | Setting the [fixedHeightMode](#g:signal:fixedHeightMode) property to 'P.True' speeds up
-- t'GI.Gtk.Objects.TreeView.TreeView' by assuming that all rows have the same height.
-- Only enable this option if all rows are the same height.
-- Please see 'GI.Gtk.Objects.TreeView.treeViewSetFixedHeightMode' for more
-- information on this option.
-- 
-- /Since: 2.4/

#if defined(ENABLE_OVERLOADING)
    TreeViewFixedHeightModePropertyInfo     ,
#endif
    constructTreeViewFixedHeightMode        ,
    getTreeViewFixedHeightMode              ,
    setTreeViewFixedHeightMode              ,
#if defined(ENABLE_OVERLOADING)
    treeViewFixedHeightMode                 ,
#endif


-- ** headersClickable #attr:headersClickable#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TreeViewHeadersClickablePropertyInfo    ,
#endif
    constructTreeViewHeadersClickable       ,
    getTreeViewHeadersClickable             ,
    setTreeViewHeadersClickable             ,
#if defined(ENABLE_OVERLOADING)
    treeViewHeadersClickable                ,
#endif


-- ** headersVisible #attr:headersVisible#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TreeViewHeadersVisiblePropertyInfo      ,
#endif
    constructTreeViewHeadersVisible         ,
    getTreeViewHeadersVisible               ,
    setTreeViewHeadersVisible               ,
#if defined(ENABLE_OVERLOADING)
    treeViewHeadersVisible                  ,
#endif


-- ** hoverExpand #attr:hoverExpand#
-- | Enables or disables the hover expansion mode of /@treeView@/.
-- Hover expansion makes rows expand or collapse if the pointer moves
-- over them.
-- 
-- This mode is primarily intended for treeviews in popups, e.g.
-- in t'GI.Gtk.Objects.ComboBox.ComboBox' or t'GI.Gtk.Objects.EntryCompletion.EntryCompletion'.
-- 
-- /Since: 2.6/

#if defined(ENABLE_OVERLOADING)
    TreeViewHoverExpandPropertyInfo         ,
#endif
    constructTreeViewHoverExpand            ,
    getTreeViewHoverExpand                  ,
    setTreeViewHoverExpand                  ,
#if defined(ENABLE_OVERLOADING)
    treeViewHoverExpand                     ,
#endif


-- ** hoverSelection #attr:hoverSelection#
-- | Enables or disables the hover selection mode of /@treeView@/.
-- Hover selection makes the selected row follow the pointer.
-- Currently, this works only for the selection modes
-- 'GI.Gtk.Enums.SelectionModeSingle' and 'GI.Gtk.Enums.SelectionModeBrowse'.
-- 
-- This mode is primarily intended for treeviews in popups, e.g.
-- in t'GI.Gtk.Objects.ComboBox.ComboBox' or t'GI.Gtk.Objects.EntryCompletion.EntryCompletion'.
-- 
-- /Since: 2.6/

#if defined(ENABLE_OVERLOADING)
    TreeViewHoverSelectionPropertyInfo      ,
#endif
    constructTreeViewHoverSelection         ,
    getTreeViewHoverSelection               ,
    setTreeViewHoverSelection               ,
#if defined(ENABLE_OVERLOADING)
    treeViewHoverSelection                  ,
#endif


-- ** levelIndentation #attr:levelIndentation#
-- | Extra indentation for each level.
-- 
-- /Since: 2.12/

#if defined(ENABLE_OVERLOADING)
    TreeViewLevelIndentationPropertyInfo    ,
#endif
    constructTreeViewLevelIndentation       ,
    getTreeViewLevelIndentation             ,
    setTreeViewLevelIndentation             ,
#if defined(ENABLE_OVERLOADING)
    treeViewLevelIndentation                ,
#endif


-- ** model #attr:model#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TreeViewModelPropertyInfo               ,
#endif
    clearTreeViewModel                      ,
    constructTreeViewModel                  ,
    getTreeViewModel                        ,
    setTreeViewModel                        ,
#if defined(ENABLE_OVERLOADING)
    treeViewModel                           ,
#endif


-- ** reorderable #attr:reorderable#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TreeViewReorderablePropertyInfo         ,
#endif
    constructTreeViewReorderable            ,
    getTreeViewReorderable                  ,
    setTreeViewReorderable                  ,
#if defined(ENABLE_OVERLOADING)
    treeViewReorderable                     ,
#endif


-- ** rubberBanding #attr:rubberBanding#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TreeViewRubberBandingPropertyInfo       ,
#endif
    constructTreeViewRubberBanding          ,
    getTreeViewRubberBanding                ,
    setTreeViewRubberBanding                ,
#if defined(ENABLE_OVERLOADING)
    treeViewRubberBanding                   ,
#endif


-- ** rulesHint #attr:rulesHint#
-- | Sets a hint to the theme to draw rows in alternating colors.

#if defined(ENABLE_OVERLOADING)
    TreeViewRulesHintPropertyInfo           ,
#endif
    constructTreeViewRulesHint              ,
    getTreeViewRulesHint                    ,
    setTreeViewRulesHint                    ,
#if defined(ENABLE_OVERLOADING)
    treeViewRulesHint                       ,
#endif


-- ** searchColumn #attr:searchColumn#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TreeViewSearchColumnPropertyInfo        ,
#endif
    constructTreeViewSearchColumn           ,
    getTreeViewSearchColumn                 ,
    setTreeViewSearchColumn                 ,
#if defined(ENABLE_OVERLOADING)
    treeViewSearchColumn                    ,
#endif


-- ** showExpanders #attr:showExpanders#
-- | 'P.True' if the view has expanders.
-- 
-- /Since: 2.12/

#if defined(ENABLE_OVERLOADING)
    TreeViewShowExpandersPropertyInfo       ,
#endif
    constructTreeViewShowExpanders          ,
    getTreeViewShowExpanders                ,
    setTreeViewShowExpanders                ,
#if defined(ENABLE_OVERLOADING)
    treeViewShowExpanders                   ,
#endif


-- ** tooltipColumn #attr:tooltipColumn#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TreeViewTooltipColumnPropertyInfo       ,
#endif
    constructTreeViewTooltipColumn          ,
    getTreeViewTooltipColumn                ,
    setTreeViewTooltipColumn                ,
#if defined(ENABLE_OVERLOADING)
    treeViewTooltipColumn                   ,
#endif




 -- * Signals


-- ** columnsChanged #signal:columnsChanged#

    TreeViewColumnsChangedCallback          ,
#if defined(ENABLE_OVERLOADING)
    TreeViewColumnsChangedSignalInfo        ,
#endif
    afterTreeViewColumnsChanged             ,
    onTreeViewColumnsChanged                ,


-- ** cursorChanged #signal:cursorChanged#

    TreeViewCursorChangedCallback           ,
#if defined(ENABLE_OVERLOADING)
    TreeViewCursorChangedSignalInfo         ,
#endif
    afterTreeViewCursorChanged              ,
    onTreeViewCursorChanged                 ,


-- ** expandCollapseCursorRow #signal:expandCollapseCursorRow#

    TreeViewExpandCollapseCursorRowCallback ,
#if defined(ENABLE_OVERLOADING)
    TreeViewExpandCollapseCursorRowSignalInfo,
#endif
    afterTreeViewExpandCollapseCursorRow    ,
    onTreeViewExpandCollapseCursorRow       ,


-- ** moveCursor #signal:moveCursor#

    TreeViewMoveCursorCallback              ,
#if defined(ENABLE_OVERLOADING)
    TreeViewMoveCursorSignalInfo            ,
#endif
    afterTreeViewMoveCursor                 ,
    onTreeViewMoveCursor                    ,


-- ** rowActivated #signal:rowActivated#

    TreeViewRowActivatedCallback            ,
#if defined(ENABLE_OVERLOADING)
    TreeViewRowActivatedSignalInfo          ,
#endif
    afterTreeViewRowActivated               ,
    onTreeViewRowActivated                  ,


-- ** rowCollapsed #signal:rowCollapsed#

    TreeViewRowCollapsedCallback            ,
#if defined(ENABLE_OVERLOADING)
    TreeViewRowCollapsedSignalInfo          ,
#endif
    afterTreeViewRowCollapsed               ,
    onTreeViewRowCollapsed                  ,


-- ** rowExpanded #signal:rowExpanded#

    TreeViewRowExpandedCallback             ,
#if defined(ENABLE_OVERLOADING)
    TreeViewRowExpandedSignalInfo           ,
#endif
    afterTreeViewRowExpanded                ,
    onTreeViewRowExpanded                   ,


-- ** selectAll #signal:selectAll#

    TreeViewSelectAllCallback               ,
#if defined(ENABLE_OVERLOADING)
    TreeViewSelectAllSignalInfo             ,
#endif
    afterTreeViewSelectAll                  ,
    onTreeViewSelectAll                     ,


-- ** selectCursorParent #signal:selectCursorParent#

    TreeViewSelectCursorParentCallback      ,
#if defined(ENABLE_OVERLOADING)
    TreeViewSelectCursorParentSignalInfo    ,
#endif
    afterTreeViewSelectCursorParent         ,
    onTreeViewSelectCursorParent            ,


-- ** selectCursorRow #signal:selectCursorRow#

    TreeViewSelectCursorRowCallback         ,
#if defined(ENABLE_OVERLOADING)
    TreeViewSelectCursorRowSignalInfo       ,
#endif
    afterTreeViewSelectCursorRow            ,
    onTreeViewSelectCursorRow               ,


-- ** startInteractiveSearch #signal:startInteractiveSearch#

    TreeViewStartInteractiveSearchCallback  ,
#if defined(ENABLE_OVERLOADING)
    TreeViewStartInteractiveSearchSignalInfo,
#endif
    afterTreeViewStartInteractiveSearch     ,
    onTreeViewStartInteractiveSearch        ,


-- ** testCollapseRow #signal:testCollapseRow#

    TreeViewTestCollapseRowCallback         ,
#if defined(ENABLE_OVERLOADING)
    TreeViewTestCollapseRowSignalInfo       ,
#endif
    afterTreeViewTestCollapseRow            ,
    onTreeViewTestCollapseRow               ,


-- ** testExpandRow #signal:testExpandRow#

    TreeViewTestExpandRowCallback           ,
#if defined(ENABLE_OVERLOADING)
    TreeViewTestExpandRowSignalInfo         ,
#endif
    afterTreeViewTestExpandRow              ,
    onTreeViewTestExpandRow                 ,


-- ** toggleCursorRow #signal:toggleCursorRow#

    TreeViewToggleCursorRowCallback         ,
#if defined(ENABLE_OVERLOADING)
    TreeViewToggleCursorRowSignalInfo       ,
#endif
    afterTreeViewToggleCursorRow            ,
    onTreeViewToggleCursorRow               ,


-- ** unselectAll #signal:unselectAll#

    TreeViewUnselectAllCallback             ,
#if defined(ENABLE_OVERLOADING)
    TreeViewUnselectAllSignalInfo           ,
#endif
    afterTreeViewUnselectAll                ,
    onTreeViewUnselectAll                   ,




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

import qualified GI.Atk.Interfaces.ImplementorIface as Atk.ImplementorIface
import qualified GI.Cairo.Structs.Surface as Cairo.Surface
import qualified GI.GLib.Callbacks as GLib.Callbacks
import qualified GI.GObject.Objects.Object as GObject.Object
import qualified GI.Gdk.Flags as Gdk.Flags
import qualified GI.Gdk.Objects.Window as Gdk.Window
import qualified GI.Gdk.Structs.Rectangle as Gdk.Rectangle
import qualified GI.Gtk.Callbacks as Gtk.Callbacks
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Scrollable as Gtk.Scrollable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.TreeModel as Gtk.TreeModel
import {-# SOURCE #-} qualified GI.Gtk.Objects.Adjustment as Gtk.Adjustment
import {-# SOURCE #-} qualified GI.Gtk.Objects.CellRenderer as Gtk.CellRenderer
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.Entry as Gtk.Entry
import {-# SOURCE #-} qualified GI.Gtk.Objects.Tooltip as Gtk.Tooltip
import {-# SOURCE #-} qualified GI.Gtk.Objects.TreeSelection as Gtk.TreeSelection
import {-# SOURCE #-} qualified GI.Gtk.Objects.TreeViewColumn as Gtk.TreeViewColumn
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget
import {-# SOURCE #-} qualified GI.Gtk.Structs.TargetEntry as Gtk.TargetEntry
import {-# SOURCE #-} qualified GI.Gtk.Structs.TreeIter as Gtk.TreeIter
import {-# SOURCE #-} qualified GI.Gtk.Structs.TreePath as Gtk.TreePath

-- | Memory-managed wrapper type.
newtype TreeView = TreeView (SP.ManagedPtr TreeView)
    deriving (Eq)

instance SP.ManagedPtrNewtype TreeView where
    toManagedPtr (TreeView p) = p

foreign import ccall "gtk_tree_view_get_type"
    c_gtk_tree_view_get_type :: IO B.Types.GType

instance B.Types.TypedObject TreeView where
    glibType = c_gtk_tree_view_get_type

instance B.Types.GObject TreeView

-- | Type class for types which can be safely cast to `TreeView`, for instance with `toTreeView`.
class (SP.GObject o, O.IsDescendantOf TreeView o) => IsTreeView o
instance (SP.GObject o, O.IsDescendantOf TreeView o) => IsTreeView o

instance O.HasParentTypes TreeView
type instance O.ParentTypes TreeView = '[Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable, Gtk.Scrollable.Scrollable]

-- | Cast to `TreeView`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toTreeView :: (MIO.MonadIO m, IsTreeView o) => o -> m TreeView
toTreeView = MIO.liftIO . B.ManagedPtr.unsafeCastTo TreeView

-- | Convert 'TreeView' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe TreeView) where
    gvalueGType_ = c_gtk_tree_view_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr TreeView)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr TreeView)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject TreeView ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveTreeViewMethod (t :: Symbol) (o :: *) :: * where
    ResolveTreeViewMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveTreeViewMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveTreeViewMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveTreeViewMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveTreeViewMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveTreeViewMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveTreeViewMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveTreeViewMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveTreeViewMethod "appendColumn" o = TreeViewAppendColumnMethodInfo
    ResolveTreeViewMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveTreeViewMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveTreeViewMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveTreeViewMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveTreeViewMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveTreeViewMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveTreeViewMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveTreeViewMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveTreeViewMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveTreeViewMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveTreeViewMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveTreeViewMethod "collapseAll" o = TreeViewCollapseAllMethodInfo
    ResolveTreeViewMethod "collapseRow" o = TreeViewCollapseRowMethodInfo
    ResolveTreeViewMethod "columnsAutosize" o = TreeViewColumnsAutosizeMethodInfo
    ResolveTreeViewMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveTreeViewMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveTreeViewMethod "convertBinWindowToTreeCoords" o = TreeViewConvertBinWindowToTreeCoordsMethodInfo
    ResolveTreeViewMethod "convertBinWindowToWidgetCoords" o = TreeViewConvertBinWindowToWidgetCoordsMethodInfo
    ResolveTreeViewMethod "convertTreeToBinWindowCoords" o = TreeViewConvertTreeToBinWindowCoordsMethodInfo
    ResolveTreeViewMethod "convertTreeToWidgetCoords" o = TreeViewConvertTreeToWidgetCoordsMethodInfo
    ResolveTreeViewMethod "convertWidgetToBinWindowCoords" o = TreeViewConvertWidgetToBinWindowCoordsMethodInfo
    ResolveTreeViewMethod "convertWidgetToTreeCoords" o = TreeViewConvertWidgetToTreeCoordsMethodInfo
    ResolveTreeViewMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveTreeViewMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveTreeViewMethod "createRowDragIcon" o = TreeViewCreateRowDragIconMethodInfo
    ResolveTreeViewMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveTreeViewMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveTreeViewMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveTreeViewMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveTreeViewMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveTreeViewMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveTreeViewMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveTreeViewMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveTreeViewMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveTreeViewMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveTreeViewMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveTreeViewMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveTreeViewMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveTreeViewMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveTreeViewMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveTreeViewMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveTreeViewMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveTreeViewMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveTreeViewMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveTreeViewMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveTreeViewMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveTreeViewMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveTreeViewMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveTreeViewMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveTreeViewMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveTreeViewMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveTreeViewMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveTreeViewMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveTreeViewMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveTreeViewMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveTreeViewMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveTreeViewMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveTreeViewMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveTreeViewMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveTreeViewMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveTreeViewMethod "enableModelDragDest" o = TreeViewEnableModelDragDestMethodInfo
    ResolveTreeViewMethod "enableModelDragSource" o = TreeViewEnableModelDragSourceMethodInfo
    ResolveTreeViewMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveTreeViewMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveTreeViewMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveTreeViewMethod "expandAll" o = TreeViewExpandAllMethodInfo
    ResolveTreeViewMethod "expandRow" o = TreeViewExpandRowMethodInfo
    ResolveTreeViewMethod "expandToPath" o = TreeViewExpandToPathMethodInfo
    ResolveTreeViewMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveTreeViewMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveTreeViewMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveTreeViewMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveTreeViewMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveTreeViewMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveTreeViewMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveTreeViewMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveTreeViewMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveTreeViewMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveTreeViewMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveTreeViewMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveTreeViewMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveTreeViewMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveTreeViewMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveTreeViewMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveTreeViewMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveTreeViewMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveTreeViewMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveTreeViewMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveTreeViewMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveTreeViewMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveTreeViewMethod "insertColumn" o = TreeViewInsertColumnMethodInfo
    ResolveTreeViewMethod "insertColumnWithDataFunc" o = TreeViewInsertColumnWithDataFuncMethodInfo
    ResolveTreeViewMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveTreeViewMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveTreeViewMethod "isBlankAtPos" o = TreeViewIsBlankAtPosMethodInfo
    ResolveTreeViewMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveTreeViewMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveTreeViewMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveTreeViewMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveTreeViewMethod "isRubberBandingActive" o = TreeViewIsRubberBandingActiveMethodInfo
    ResolveTreeViewMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveTreeViewMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveTreeViewMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveTreeViewMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveTreeViewMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveTreeViewMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveTreeViewMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveTreeViewMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveTreeViewMethod "mapExpandedRows" o = TreeViewMapExpandedRowsMethodInfo
    ResolveTreeViewMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveTreeViewMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveTreeViewMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveTreeViewMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveTreeViewMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveTreeViewMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveTreeViewMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveTreeViewMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveTreeViewMethod "moveColumnAfter" o = TreeViewMoveColumnAfterMethodInfo
    ResolveTreeViewMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveTreeViewMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveTreeViewMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveTreeViewMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveTreeViewMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveTreeViewMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveTreeViewMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveTreeViewMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveTreeViewMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveTreeViewMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveTreeViewMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveTreeViewMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveTreeViewMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveTreeViewMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveTreeViewMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveTreeViewMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveTreeViewMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveTreeViewMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveTreeViewMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveTreeViewMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveTreeViewMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveTreeViewMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveTreeViewMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolveTreeViewMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveTreeViewMethod "removeColumn" o = TreeViewRemoveColumnMethodInfo
    ResolveTreeViewMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveTreeViewMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveTreeViewMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveTreeViewMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveTreeViewMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveTreeViewMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveTreeViewMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveTreeViewMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveTreeViewMethod "rowActivated" o = TreeViewRowActivatedMethodInfo
    ResolveTreeViewMethod "rowExpanded" o = TreeViewRowExpandedMethodInfo
    ResolveTreeViewMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveTreeViewMethod "scrollToCell" o = TreeViewScrollToCellMethodInfo
    ResolveTreeViewMethod "scrollToPoint" o = TreeViewScrollToPointMethodInfo
    ResolveTreeViewMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveTreeViewMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveTreeViewMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveTreeViewMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveTreeViewMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveTreeViewMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveTreeViewMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveTreeViewMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveTreeViewMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveTreeViewMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveTreeViewMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveTreeViewMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveTreeViewMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveTreeViewMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveTreeViewMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveTreeViewMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveTreeViewMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveTreeViewMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveTreeViewMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveTreeViewMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveTreeViewMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveTreeViewMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveTreeViewMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveTreeViewMethod "unsetRowsDragDest" o = TreeViewUnsetRowsDragDestMethodInfo
    ResolveTreeViewMethod "unsetRowsDragSource" o = TreeViewUnsetRowsDragSourceMethodInfo
    ResolveTreeViewMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveTreeViewMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveTreeViewMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveTreeViewMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveTreeViewMethod "getActivateOnSingleClick" o = TreeViewGetActivateOnSingleClickMethodInfo
    ResolveTreeViewMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveTreeViewMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveTreeViewMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveTreeViewMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveTreeViewMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveTreeViewMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveTreeViewMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveTreeViewMethod "getBackgroundArea" o = TreeViewGetBackgroundAreaMethodInfo
    ResolveTreeViewMethod "getBinWindow" o = TreeViewGetBinWindowMethodInfo
    ResolveTreeViewMethod "getBorder" o = Gtk.Scrollable.ScrollableGetBorderMethodInfo
    ResolveTreeViewMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveTreeViewMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveTreeViewMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveTreeViewMethod "getCellArea" o = TreeViewGetCellAreaMethodInfo
    ResolveTreeViewMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveTreeViewMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveTreeViewMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveTreeViewMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveTreeViewMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveTreeViewMethod "getColumn" o = TreeViewGetColumnMethodInfo
    ResolveTreeViewMethod "getColumns" o = TreeViewGetColumnsMethodInfo
    ResolveTreeViewMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveTreeViewMethod "getCursor" o = TreeViewGetCursorMethodInfo
    ResolveTreeViewMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveTreeViewMethod "getDestRowAtPos" o = TreeViewGetDestRowAtPosMethodInfo
    ResolveTreeViewMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveTreeViewMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveTreeViewMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveTreeViewMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveTreeViewMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveTreeViewMethod "getDragDestRow" o = TreeViewGetDragDestRowMethodInfo
    ResolveTreeViewMethod "getEnableSearch" o = TreeViewGetEnableSearchMethodInfo
    ResolveTreeViewMethod "getEnableTreeLines" o = TreeViewGetEnableTreeLinesMethodInfo
    ResolveTreeViewMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveTreeViewMethod "getExpanderColumn" o = TreeViewGetExpanderColumnMethodInfo
    ResolveTreeViewMethod "getFixedHeightMode" o = TreeViewGetFixedHeightModeMethodInfo
    ResolveTreeViewMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveTreeViewMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveTreeViewMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveTreeViewMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveTreeViewMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveTreeViewMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveTreeViewMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveTreeViewMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveTreeViewMethod "getGridLines" o = TreeViewGetGridLinesMethodInfo
    ResolveTreeViewMethod "getHadjustment" o = TreeViewGetHadjustmentMethodInfo
    ResolveTreeViewMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveTreeViewMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveTreeViewMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveTreeViewMethod "getHeadersClickable" o = TreeViewGetHeadersClickableMethodInfo
    ResolveTreeViewMethod "getHeadersVisible" o = TreeViewGetHeadersVisibleMethodInfo
    ResolveTreeViewMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveTreeViewMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveTreeViewMethod "getHoverExpand" o = TreeViewGetHoverExpandMethodInfo
    ResolveTreeViewMethod "getHoverSelection" o = TreeViewGetHoverSelectionMethodInfo
    ResolveTreeViewMethod "getHscrollPolicy" o = Gtk.Scrollable.ScrollableGetHscrollPolicyMethodInfo
    ResolveTreeViewMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveTreeViewMethod "getLevelIndentation" o = TreeViewGetLevelIndentationMethodInfo
    ResolveTreeViewMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveTreeViewMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveTreeViewMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveTreeViewMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveTreeViewMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveTreeViewMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveTreeViewMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveTreeViewMethod "getModel" o = TreeViewGetModelMethodInfo
    ResolveTreeViewMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveTreeViewMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveTreeViewMethod "getNColumns" o = TreeViewGetNColumnsMethodInfo
    ResolveTreeViewMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveTreeViewMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveTreeViewMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveTreeViewMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveTreeViewMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveTreeViewMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveTreeViewMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveTreeViewMethod "getPathAtPos" o = TreeViewGetPathAtPosMethodInfo
    ResolveTreeViewMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveTreeViewMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveTreeViewMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveTreeViewMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveTreeViewMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveTreeViewMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveTreeViewMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveTreeViewMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveTreeViewMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveTreeViewMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveTreeViewMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveTreeViewMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveTreeViewMethod "getReorderable" o = TreeViewGetReorderableMethodInfo
    ResolveTreeViewMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveTreeViewMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveTreeViewMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveTreeViewMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveTreeViewMethod "getRubberBanding" o = TreeViewGetRubberBandingMethodInfo
    ResolveTreeViewMethod "getRulesHint" o = TreeViewGetRulesHintMethodInfo
    ResolveTreeViewMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveTreeViewMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveTreeViewMethod "getSearchColumn" o = TreeViewGetSearchColumnMethodInfo
    ResolveTreeViewMethod "getSearchEntry" o = TreeViewGetSearchEntryMethodInfo
    ResolveTreeViewMethod "getSelection" o = TreeViewGetSelectionMethodInfo
    ResolveTreeViewMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveTreeViewMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveTreeViewMethod "getShowExpanders" o = TreeViewGetShowExpandersMethodInfo
    ResolveTreeViewMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveTreeViewMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveTreeViewMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveTreeViewMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveTreeViewMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveTreeViewMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveTreeViewMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveTreeViewMethod "getTooltipColumn" o = TreeViewGetTooltipColumnMethodInfo
    ResolveTreeViewMethod "getTooltipContext" o = TreeViewGetTooltipContextMethodInfo
    ResolveTreeViewMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveTreeViewMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveTreeViewMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveTreeViewMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveTreeViewMethod "getVadjustment" o = TreeViewGetVadjustmentMethodInfo
    ResolveTreeViewMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveTreeViewMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveTreeViewMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveTreeViewMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveTreeViewMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveTreeViewMethod "getVisibleRange" o = TreeViewGetVisibleRangeMethodInfo
    ResolveTreeViewMethod "getVisibleRect" o = TreeViewGetVisibleRectMethodInfo
    ResolveTreeViewMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveTreeViewMethod "getVscrollPolicy" o = Gtk.Scrollable.ScrollableGetVscrollPolicyMethodInfo
    ResolveTreeViewMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveTreeViewMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveTreeViewMethod "setActivateOnSingleClick" o = TreeViewSetActivateOnSingleClickMethodInfo
    ResolveTreeViewMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveTreeViewMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveTreeViewMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveTreeViewMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveTreeViewMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveTreeViewMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveTreeViewMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveTreeViewMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveTreeViewMethod "setColumnDragFunction" o = TreeViewSetColumnDragFunctionMethodInfo
    ResolveTreeViewMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveTreeViewMethod "setCursor" o = TreeViewSetCursorMethodInfo
    ResolveTreeViewMethod "setCursorOnCell" o = TreeViewSetCursorOnCellMethodInfo
    ResolveTreeViewMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveTreeViewMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveTreeViewMethod "setDestroyCountFunc" o = TreeViewSetDestroyCountFuncMethodInfo
    ResolveTreeViewMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveTreeViewMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveTreeViewMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveTreeViewMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveTreeViewMethod "setDragDestRow" o = TreeViewSetDragDestRowMethodInfo
    ResolveTreeViewMethod "setEnableSearch" o = TreeViewSetEnableSearchMethodInfo
    ResolveTreeViewMethod "setEnableTreeLines" o = TreeViewSetEnableTreeLinesMethodInfo
    ResolveTreeViewMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveTreeViewMethod "setExpanderColumn" o = TreeViewSetExpanderColumnMethodInfo
    ResolveTreeViewMethod "setFixedHeightMode" o = TreeViewSetFixedHeightModeMethodInfo
    ResolveTreeViewMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveTreeViewMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveTreeViewMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveTreeViewMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveTreeViewMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveTreeViewMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveTreeViewMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveTreeViewMethod "setGridLines" o = TreeViewSetGridLinesMethodInfo
    ResolveTreeViewMethod "setHadjustment" o = TreeViewSetHadjustmentMethodInfo
    ResolveTreeViewMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveTreeViewMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveTreeViewMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveTreeViewMethod "setHeadersClickable" o = TreeViewSetHeadersClickableMethodInfo
    ResolveTreeViewMethod "setHeadersVisible" o = TreeViewSetHeadersVisibleMethodInfo
    ResolveTreeViewMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveTreeViewMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveTreeViewMethod "setHoverExpand" o = TreeViewSetHoverExpandMethodInfo
    ResolveTreeViewMethod "setHoverSelection" o = TreeViewSetHoverSelectionMethodInfo
    ResolveTreeViewMethod "setHscrollPolicy" o = Gtk.Scrollable.ScrollableSetHscrollPolicyMethodInfo
    ResolveTreeViewMethod "setLevelIndentation" o = TreeViewSetLevelIndentationMethodInfo
    ResolveTreeViewMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveTreeViewMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveTreeViewMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveTreeViewMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveTreeViewMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveTreeViewMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveTreeViewMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveTreeViewMethod "setModel" o = TreeViewSetModelMethodInfo
    ResolveTreeViewMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveTreeViewMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveTreeViewMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveTreeViewMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveTreeViewMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveTreeViewMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveTreeViewMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveTreeViewMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveTreeViewMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveTreeViewMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveTreeViewMethod "setReorderable" o = TreeViewSetReorderableMethodInfo
    ResolveTreeViewMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveTreeViewMethod "setRowSeparatorFunc" o = TreeViewSetRowSeparatorFuncMethodInfo
    ResolveTreeViewMethod "setRubberBanding" o = TreeViewSetRubberBandingMethodInfo
    ResolveTreeViewMethod "setRulesHint" o = TreeViewSetRulesHintMethodInfo
    ResolveTreeViewMethod "setSearchColumn" o = TreeViewSetSearchColumnMethodInfo
    ResolveTreeViewMethod "setSearchEntry" o = TreeViewSetSearchEntryMethodInfo
    ResolveTreeViewMethod "setSearchEqualFunc" o = TreeViewSetSearchEqualFuncMethodInfo
    ResolveTreeViewMethod "setSearchPositionFunc" o = TreeViewSetSearchPositionFuncMethodInfo
    ResolveTreeViewMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveTreeViewMethod "setShowExpanders" o = TreeViewSetShowExpandersMethodInfo
    ResolveTreeViewMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveTreeViewMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveTreeViewMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveTreeViewMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveTreeViewMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveTreeViewMethod "setTooltipCell" o = TreeViewSetTooltipCellMethodInfo
    ResolveTreeViewMethod "setTooltipColumn" o = TreeViewSetTooltipColumnMethodInfo
    ResolveTreeViewMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveTreeViewMethod "setTooltipRow" o = TreeViewSetTooltipRowMethodInfo
    ResolveTreeViewMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveTreeViewMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveTreeViewMethod "setVadjustment" o = TreeViewSetVadjustmentMethodInfo
    ResolveTreeViewMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveTreeViewMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveTreeViewMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveTreeViewMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveTreeViewMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveTreeViewMethod "setVscrollPolicy" o = Gtk.Scrollable.ScrollableSetVscrollPolicyMethodInfo
    ResolveTreeViewMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveTreeViewMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveTreeViewMethod t TreeView, O.OverloadedMethod info TreeView p) => OL.IsLabel t (TreeView -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveTreeViewMethod t TreeView, O.OverloadedMethod info TreeView p, R.HasField t TreeView p) => R.HasField t TreeView p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveTreeViewMethod t TreeView, O.OverloadedMethodInfo info TreeView) => OL.IsLabel t (O.MethodProxy info TreeView) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal TreeView::columns-changed
-- | The number of columns of the treeview has changed.
type TreeViewColumnsChangedCallback =
    IO ()

type C_TreeViewColumnsChangedCallback =
    Ptr TreeView ->                         -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TreeViewColumnsChangedCallback`.
foreign import ccall "wrapper"
    mk_TreeViewColumnsChangedCallback :: C_TreeViewColumnsChangedCallback -> IO (FunPtr C_TreeViewColumnsChangedCallback)

wrap_TreeViewColumnsChangedCallback :: 
    GObject a => (a -> TreeViewColumnsChangedCallback) ->
    C_TreeViewColumnsChangedCallback
wrap_TreeViewColumnsChangedCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [columnsChanged](#signal:columnsChanged) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' treeView #columnsChanged callback
-- @
-- 
-- 
onTreeViewColumnsChanged :: (IsTreeView a, MonadIO m) => a -> ((?self :: a) => TreeViewColumnsChangedCallback) -> m SignalHandlerId
onTreeViewColumnsChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeViewColumnsChangedCallback wrapped
    wrapped'' <- mk_TreeViewColumnsChangedCallback wrapped'
    connectSignalFunPtr obj "columns-changed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [columnsChanged](#signal:columnsChanged) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' treeView #columnsChanged callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTreeViewColumnsChanged :: (IsTreeView a, MonadIO m) => a -> ((?self :: a) => TreeViewColumnsChangedCallback) -> m SignalHandlerId
afterTreeViewColumnsChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeViewColumnsChangedCallback wrapped
    wrapped'' <- mk_TreeViewColumnsChangedCallback wrapped'
    connectSignalFunPtr obj "columns-changed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TreeViewColumnsChangedSignalInfo
instance SignalInfo TreeViewColumnsChangedSignalInfo where
    type HaskellCallbackType TreeViewColumnsChangedSignalInfo = TreeViewColumnsChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TreeViewColumnsChangedCallback cb
        cb'' <- mk_TreeViewColumnsChangedCallback cb'
        connectSignalFunPtr obj "columns-changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView::columns-changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#g:signal:columnsChanged"})

#endif

-- signal TreeView::cursor-changed
-- | The position of the cursor (focused cell) has changed.
type TreeViewCursorChangedCallback =
    IO ()

type C_TreeViewCursorChangedCallback =
    Ptr TreeView ->                         -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TreeViewCursorChangedCallback`.
foreign import ccall "wrapper"
    mk_TreeViewCursorChangedCallback :: C_TreeViewCursorChangedCallback -> IO (FunPtr C_TreeViewCursorChangedCallback)

wrap_TreeViewCursorChangedCallback :: 
    GObject a => (a -> TreeViewCursorChangedCallback) ->
    C_TreeViewCursorChangedCallback
wrap_TreeViewCursorChangedCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [cursorChanged](#signal:cursorChanged) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' treeView #cursorChanged callback
-- @
-- 
-- 
onTreeViewCursorChanged :: (IsTreeView a, MonadIO m) => a -> ((?self :: a) => TreeViewCursorChangedCallback) -> m SignalHandlerId
onTreeViewCursorChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeViewCursorChangedCallback wrapped
    wrapped'' <- mk_TreeViewCursorChangedCallback wrapped'
    connectSignalFunPtr obj "cursor-changed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [cursorChanged](#signal:cursorChanged) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' treeView #cursorChanged callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTreeViewCursorChanged :: (IsTreeView a, MonadIO m) => a -> ((?self :: a) => TreeViewCursorChangedCallback) -> m SignalHandlerId
afterTreeViewCursorChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeViewCursorChangedCallback wrapped
    wrapped'' <- mk_TreeViewCursorChangedCallback wrapped'
    connectSignalFunPtr obj "cursor-changed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TreeViewCursorChangedSignalInfo
instance SignalInfo TreeViewCursorChangedSignalInfo where
    type HaskellCallbackType TreeViewCursorChangedSignalInfo = TreeViewCursorChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TreeViewCursorChangedCallback cb
        cb'' <- mk_TreeViewCursorChangedCallback cb'
        connectSignalFunPtr obj "cursor-changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView::cursor-changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#g:signal:cursorChanged"})

#endif

-- signal TreeView::expand-collapse-cursor-row
-- | /No description available in the introspection data./
type TreeViewExpandCollapseCursorRowCallback =
    Bool
    -> Bool
    -> Bool
    -> IO Bool

type C_TreeViewExpandCollapseCursorRowCallback =
    Ptr TreeView ->                         -- object
    CInt ->
    CInt ->
    CInt ->
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_TreeViewExpandCollapseCursorRowCallback`.
foreign import ccall "wrapper"
    mk_TreeViewExpandCollapseCursorRowCallback :: C_TreeViewExpandCollapseCursorRowCallback -> IO (FunPtr C_TreeViewExpandCollapseCursorRowCallback)

wrap_TreeViewExpandCollapseCursorRowCallback :: 
    GObject a => (a -> TreeViewExpandCollapseCursorRowCallback) ->
    C_TreeViewExpandCollapseCursorRowCallback
wrap_TreeViewExpandCollapseCursorRowCallback gi'cb gi'selfPtr object p0 p1 _ = do
    let object' = (/= 0) object
    let p0' = (/= 0) p0
    let p1' = (/= 0) p1
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  object' p0' p1'
    let result' = (fromIntegral . fromEnum) result
    return result'


-- | Connect a signal handler for the [expandCollapseCursorRow](#signal:expandCollapseCursorRow) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' treeView #expandCollapseCursorRow callback
-- @
-- 
-- 
onTreeViewExpandCollapseCursorRow :: (IsTreeView a, MonadIO m) => a -> ((?self :: a) => TreeViewExpandCollapseCursorRowCallback) -> m SignalHandlerId
onTreeViewExpandCollapseCursorRow obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeViewExpandCollapseCursorRowCallback wrapped
    wrapped'' <- mk_TreeViewExpandCollapseCursorRowCallback wrapped'
    connectSignalFunPtr obj "expand-collapse-cursor-row" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [expandCollapseCursorRow](#signal:expandCollapseCursorRow) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' treeView #expandCollapseCursorRow callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTreeViewExpandCollapseCursorRow :: (IsTreeView a, MonadIO m) => a -> ((?self :: a) => TreeViewExpandCollapseCursorRowCallback) -> m SignalHandlerId
afterTreeViewExpandCollapseCursorRow obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeViewExpandCollapseCursorRowCallback wrapped
    wrapped'' <- mk_TreeViewExpandCollapseCursorRowCallback wrapped'
    connectSignalFunPtr obj "expand-collapse-cursor-row" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TreeViewExpandCollapseCursorRowSignalInfo
instance SignalInfo TreeViewExpandCollapseCursorRowSignalInfo where
    type HaskellCallbackType TreeViewExpandCollapseCursorRowSignalInfo = TreeViewExpandCollapseCursorRowCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TreeViewExpandCollapseCursorRowCallback cb
        cb'' <- mk_TreeViewExpandCollapseCursorRowCallback cb'
        connectSignalFunPtr obj "expand-collapse-cursor-row" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView::expand-collapse-cursor-row"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#g:signal:expandCollapseCursorRow"})

#endif

-- signal TreeView::move-cursor
-- | The [TreeView::moveCursor]("GI.Gtk.Objects.TreeView#g:signal:moveCursor") signal is a [keybinding
-- signal][GtkBindingSignal] which gets emitted when the user
-- presses one of the cursor keys.
-- 
-- Applications should not connect to it, but may emit it with
-- @/g_signal_emit_by_name()/@ if they need to control the cursor
-- programmatically. In contrast to 'GI.Gtk.Objects.TreeView.treeViewSetCursor' and
-- 'GI.Gtk.Objects.TreeView.treeViewSetCursorOnCell' when moving horizontally
-- [TreeView::moveCursor]("GI.Gtk.Objects.TreeView#g:signal:moveCursor") does not reset the current selection.
type TreeViewMoveCursorCallback =
    Gtk.Enums.MovementStep
    -- ^ /@step@/: the granularity of the move, as a
    -- t'GI.Gtk.Enums.MovementStep'. 'GI.Gtk.Enums.MovementStepLogicalPositions',
    -- 'GI.Gtk.Enums.MovementStepVisualPositions', 'GI.Gtk.Enums.MovementStepDisplayLines',
    -- 'GI.Gtk.Enums.MovementStepPages' and 'GI.Gtk.Enums.MovementStepBufferEnds' are
    -- supported. 'GI.Gtk.Enums.MovementStepLogicalPositions' and
    -- 'GI.Gtk.Enums.MovementStepVisualPositions' are treated identically.
    -> Int32
    -- ^ /@direction@/: the direction to move: +1 to move forwards;
    -- -1 to move backwards. The resulting movement is
    -- undefined for all other values.
    -> IO Bool
    -- ^ __Returns:__ 'P.True' if /@step@/ is supported, 'P.False' otherwise.

type C_TreeViewMoveCursorCallback =
    Ptr TreeView ->                         -- object
    CUInt ->
    Int32 ->
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_TreeViewMoveCursorCallback`.
foreign import ccall "wrapper"
    mk_TreeViewMoveCursorCallback :: C_TreeViewMoveCursorCallback -> IO (FunPtr C_TreeViewMoveCursorCallback)

wrap_TreeViewMoveCursorCallback :: 
    GObject a => (a -> TreeViewMoveCursorCallback) ->
    C_TreeViewMoveCursorCallback
wrap_TreeViewMoveCursorCallback gi'cb gi'selfPtr step direction _ = do
    let step' = (toEnum . fromIntegral) step
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  step' direction
    let result' = (fromIntegral . fromEnum) result
    return result'


-- | Connect a signal handler for the [moveCursor](#signal:moveCursor) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' treeView #moveCursor callback
-- @
-- 
-- 
onTreeViewMoveCursor :: (IsTreeView a, MonadIO m) => a -> ((?self :: a) => TreeViewMoveCursorCallback) -> m SignalHandlerId
onTreeViewMoveCursor obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeViewMoveCursorCallback wrapped
    wrapped'' <- mk_TreeViewMoveCursorCallback wrapped'
    connectSignalFunPtr obj "move-cursor" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [moveCursor](#signal:moveCursor) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' treeView #moveCursor callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTreeViewMoveCursor :: (IsTreeView a, MonadIO m) => a -> ((?self :: a) => TreeViewMoveCursorCallback) -> m SignalHandlerId
afterTreeViewMoveCursor obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeViewMoveCursorCallback wrapped
    wrapped'' <- mk_TreeViewMoveCursorCallback wrapped'
    connectSignalFunPtr obj "move-cursor" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TreeViewMoveCursorSignalInfo
instance SignalInfo TreeViewMoveCursorSignalInfo where
    type HaskellCallbackType TreeViewMoveCursorSignalInfo = TreeViewMoveCursorCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TreeViewMoveCursorCallback cb
        cb'' <- mk_TreeViewMoveCursorCallback cb'
        connectSignalFunPtr obj "move-cursor" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView::move-cursor"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#g:signal:moveCursor"})

#endif

-- signal TreeView::row-activated
-- | The \"row-activated\" signal is emitted when the method
-- 'GI.Gtk.Objects.TreeView.treeViewRowActivated' is called, when the user double
-- clicks a treeview row with the \"activate-on-single-click\"
-- property set to 'P.False', or when the user single clicks a row when
-- the \"activate-on-single-click\" property set to 'P.True'. It is also
-- emitted when a non-editable row is selected and one of the keys:
-- Space, Shift+Space, Return or Enter is pressed.
-- 
-- For selection handling refer to the
-- [tree widget conceptual overview][TreeWidget]
-- as well as t'GI.Gtk.Objects.TreeSelection.TreeSelection'.
type TreeViewRowActivatedCallback =
    Gtk.TreePath.TreePath
    -- ^ /@path@/: the t'GI.Gtk.Structs.TreePath.TreePath' for the activated row
    -> Gtk.TreeViewColumn.TreeViewColumn
    -- ^ /@column@/: the t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn' in which the activation occurred
    -> IO ()

type C_TreeViewRowActivatedCallback =
    Ptr TreeView ->                         -- object
    Ptr Gtk.TreePath.TreePath ->
    Ptr Gtk.TreeViewColumn.TreeViewColumn ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TreeViewRowActivatedCallback`.
foreign import ccall "wrapper"
    mk_TreeViewRowActivatedCallback :: C_TreeViewRowActivatedCallback -> IO (FunPtr C_TreeViewRowActivatedCallback)

wrap_TreeViewRowActivatedCallback :: 
    GObject a => (a -> TreeViewRowActivatedCallback) ->
    C_TreeViewRowActivatedCallback
wrap_TreeViewRowActivatedCallback gi'cb gi'selfPtr path column _ = do
    B.ManagedPtr.withTransient  path $ \path' -> do
        column' <- (newObject Gtk.TreeViewColumn.TreeViewColumn) column
        B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  path' column'


-- | Connect a signal handler for the [rowActivated](#signal:rowActivated) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' treeView #rowActivated callback
-- @
-- 
-- 
onTreeViewRowActivated :: (IsTreeView a, MonadIO m) => a -> ((?self :: a) => TreeViewRowActivatedCallback) -> m SignalHandlerId
onTreeViewRowActivated obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeViewRowActivatedCallback wrapped
    wrapped'' <- mk_TreeViewRowActivatedCallback wrapped'
    connectSignalFunPtr obj "row-activated" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [rowActivated](#signal:rowActivated) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' treeView #rowActivated callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTreeViewRowActivated :: (IsTreeView a, MonadIO m) => a -> ((?self :: a) => TreeViewRowActivatedCallback) -> m SignalHandlerId
afterTreeViewRowActivated obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeViewRowActivatedCallback wrapped
    wrapped'' <- mk_TreeViewRowActivatedCallback wrapped'
    connectSignalFunPtr obj "row-activated" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TreeViewRowActivatedSignalInfo
instance SignalInfo TreeViewRowActivatedSignalInfo where
    type HaskellCallbackType TreeViewRowActivatedSignalInfo = TreeViewRowActivatedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TreeViewRowActivatedCallback cb
        cb'' <- mk_TreeViewRowActivatedCallback cb'
        connectSignalFunPtr obj "row-activated" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView::row-activated"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#g:signal:rowActivated"})

#endif

-- signal TreeView::row-collapsed
-- | The given row has been collapsed (child nodes are hidden).
type TreeViewRowCollapsedCallback =
    Gtk.TreeIter.TreeIter
    -- ^ /@iter@/: the tree iter of the collapsed row
    -> Gtk.TreePath.TreePath
    -- ^ /@path@/: a tree path that points to the row
    -> IO ()

type C_TreeViewRowCollapsedCallback =
    Ptr TreeView ->                         -- object
    Ptr Gtk.TreeIter.TreeIter ->
    Ptr Gtk.TreePath.TreePath ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TreeViewRowCollapsedCallback`.
foreign import ccall "wrapper"
    mk_TreeViewRowCollapsedCallback :: C_TreeViewRowCollapsedCallback -> IO (FunPtr C_TreeViewRowCollapsedCallback)

wrap_TreeViewRowCollapsedCallback :: 
    GObject a => (a -> TreeViewRowCollapsedCallback) ->
    C_TreeViewRowCollapsedCallback
wrap_TreeViewRowCollapsedCallback gi'cb gi'selfPtr iter path _ = do
    B.ManagedPtr.withTransient  iter $ \iter' -> do
        B.ManagedPtr.withTransient  path $ \path' -> do
            B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  iter' path'


-- | Connect a signal handler for the [rowCollapsed](#signal:rowCollapsed) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' treeView #rowCollapsed callback
-- @
-- 
-- 
onTreeViewRowCollapsed :: (IsTreeView a, MonadIO m) => a -> ((?self :: a) => TreeViewRowCollapsedCallback) -> m SignalHandlerId
onTreeViewRowCollapsed obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeViewRowCollapsedCallback wrapped
    wrapped'' <- mk_TreeViewRowCollapsedCallback wrapped'
    connectSignalFunPtr obj "row-collapsed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [rowCollapsed](#signal:rowCollapsed) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' treeView #rowCollapsed callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTreeViewRowCollapsed :: (IsTreeView a, MonadIO m) => a -> ((?self :: a) => TreeViewRowCollapsedCallback) -> m SignalHandlerId
afterTreeViewRowCollapsed obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeViewRowCollapsedCallback wrapped
    wrapped'' <- mk_TreeViewRowCollapsedCallback wrapped'
    connectSignalFunPtr obj "row-collapsed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TreeViewRowCollapsedSignalInfo
instance SignalInfo TreeViewRowCollapsedSignalInfo where
    type HaskellCallbackType TreeViewRowCollapsedSignalInfo = TreeViewRowCollapsedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TreeViewRowCollapsedCallback cb
        cb'' <- mk_TreeViewRowCollapsedCallback cb'
        connectSignalFunPtr obj "row-collapsed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView::row-collapsed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#g:signal:rowCollapsed"})

#endif

-- signal TreeView::row-expanded
-- | The given row has been expanded (child nodes are shown).
type TreeViewRowExpandedCallback =
    Gtk.TreeIter.TreeIter
    -- ^ /@iter@/: the tree iter of the expanded row
    -> Gtk.TreePath.TreePath
    -- ^ /@path@/: a tree path that points to the row
    -> IO ()

type C_TreeViewRowExpandedCallback =
    Ptr TreeView ->                         -- object
    Ptr Gtk.TreeIter.TreeIter ->
    Ptr Gtk.TreePath.TreePath ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TreeViewRowExpandedCallback`.
foreign import ccall "wrapper"
    mk_TreeViewRowExpandedCallback :: C_TreeViewRowExpandedCallback -> IO (FunPtr C_TreeViewRowExpandedCallback)

wrap_TreeViewRowExpandedCallback :: 
    GObject a => (a -> TreeViewRowExpandedCallback) ->
    C_TreeViewRowExpandedCallback
wrap_TreeViewRowExpandedCallback gi'cb gi'selfPtr iter path _ = do
    B.ManagedPtr.withTransient  iter $ \iter' -> do
        B.ManagedPtr.withTransient  path $ \path' -> do
            B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  iter' path'


-- | Connect a signal handler for the [rowExpanded](#signal:rowExpanded) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' treeView #rowExpanded callback
-- @
-- 
-- 
onTreeViewRowExpanded :: (IsTreeView a, MonadIO m) => a -> ((?self :: a) => TreeViewRowExpandedCallback) -> m SignalHandlerId
onTreeViewRowExpanded obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeViewRowExpandedCallback wrapped
    wrapped'' <- mk_TreeViewRowExpandedCallback wrapped'
    connectSignalFunPtr obj "row-expanded" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [rowExpanded](#signal:rowExpanded) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' treeView #rowExpanded callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTreeViewRowExpanded :: (IsTreeView a, MonadIO m) => a -> ((?self :: a) => TreeViewRowExpandedCallback) -> m SignalHandlerId
afterTreeViewRowExpanded obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeViewRowExpandedCallback wrapped
    wrapped'' <- mk_TreeViewRowExpandedCallback wrapped'
    connectSignalFunPtr obj "row-expanded" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TreeViewRowExpandedSignalInfo
instance SignalInfo TreeViewRowExpandedSignalInfo where
    type HaskellCallbackType TreeViewRowExpandedSignalInfo = TreeViewRowExpandedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TreeViewRowExpandedCallback cb
        cb'' <- mk_TreeViewRowExpandedCallback cb'
        connectSignalFunPtr obj "row-expanded" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView::row-expanded"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#g:signal:rowExpanded"})

#endif

-- signal TreeView::select-all
-- | /No description available in the introspection data./
type TreeViewSelectAllCallback =
    IO Bool

type C_TreeViewSelectAllCallback =
    Ptr TreeView ->                         -- object
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_TreeViewSelectAllCallback`.
foreign import ccall "wrapper"
    mk_TreeViewSelectAllCallback :: C_TreeViewSelectAllCallback -> IO (FunPtr C_TreeViewSelectAllCallback)

wrap_TreeViewSelectAllCallback :: 
    GObject a => (a -> TreeViewSelectAllCallback) ->
    C_TreeViewSelectAllCallback
wrap_TreeViewSelectAllCallback gi'cb gi'selfPtr _ = do
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 
    let result' = (fromIntegral . fromEnum) result
    return result'


-- | Connect a signal handler for the [selectAll](#signal:selectAll) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' treeView #selectAll callback
-- @
-- 
-- 
onTreeViewSelectAll :: (IsTreeView a, MonadIO m) => a -> ((?self :: a) => TreeViewSelectAllCallback) -> m SignalHandlerId
onTreeViewSelectAll obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeViewSelectAllCallback wrapped
    wrapped'' <- mk_TreeViewSelectAllCallback wrapped'
    connectSignalFunPtr obj "select-all" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [selectAll](#signal:selectAll) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' treeView #selectAll callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTreeViewSelectAll :: (IsTreeView a, MonadIO m) => a -> ((?self :: a) => TreeViewSelectAllCallback) -> m SignalHandlerId
afterTreeViewSelectAll obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeViewSelectAllCallback wrapped
    wrapped'' <- mk_TreeViewSelectAllCallback wrapped'
    connectSignalFunPtr obj "select-all" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TreeViewSelectAllSignalInfo
instance SignalInfo TreeViewSelectAllSignalInfo where
    type HaskellCallbackType TreeViewSelectAllSignalInfo = TreeViewSelectAllCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TreeViewSelectAllCallback cb
        cb'' <- mk_TreeViewSelectAllCallback cb'
        connectSignalFunPtr obj "select-all" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView::select-all"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#g:signal:selectAll"})

#endif

-- signal TreeView::select-cursor-parent
-- | /No description available in the introspection data./
type TreeViewSelectCursorParentCallback =
    IO Bool

type C_TreeViewSelectCursorParentCallback =
    Ptr TreeView ->                         -- object
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_TreeViewSelectCursorParentCallback`.
foreign import ccall "wrapper"
    mk_TreeViewSelectCursorParentCallback :: C_TreeViewSelectCursorParentCallback -> IO (FunPtr C_TreeViewSelectCursorParentCallback)

wrap_TreeViewSelectCursorParentCallback :: 
    GObject a => (a -> TreeViewSelectCursorParentCallback) ->
    C_TreeViewSelectCursorParentCallback
wrap_TreeViewSelectCursorParentCallback gi'cb gi'selfPtr _ = do
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 
    let result' = (fromIntegral . fromEnum) result
    return result'


-- | Connect a signal handler for the [selectCursorParent](#signal:selectCursorParent) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' treeView #selectCursorParent callback
-- @
-- 
-- 
onTreeViewSelectCursorParent :: (IsTreeView a, MonadIO m) => a -> ((?self :: a) => TreeViewSelectCursorParentCallback) -> m SignalHandlerId
onTreeViewSelectCursorParent obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeViewSelectCursorParentCallback wrapped
    wrapped'' <- mk_TreeViewSelectCursorParentCallback wrapped'
    connectSignalFunPtr obj "select-cursor-parent" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [selectCursorParent](#signal:selectCursorParent) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' treeView #selectCursorParent callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTreeViewSelectCursorParent :: (IsTreeView a, MonadIO m) => a -> ((?self :: a) => TreeViewSelectCursorParentCallback) -> m SignalHandlerId
afterTreeViewSelectCursorParent obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeViewSelectCursorParentCallback wrapped
    wrapped'' <- mk_TreeViewSelectCursorParentCallback wrapped'
    connectSignalFunPtr obj "select-cursor-parent" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TreeViewSelectCursorParentSignalInfo
instance SignalInfo TreeViewSelectCursorParentSignalInfo where
    type HaskellCallbackType TreeViewSelectCursorParentSignalInfo = TreeViewSelectCursorParentCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TreeViewSelectCursorParentCallback cb
        cb'' <- mk_TreeViewSelectCursorParentCallback cb'
        connectSignalFunPtr obj "select-cursor-parent" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView::select-cursor-parent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#g:signal:selectCursorParent"})

#endif

-- signal TreeView::select-cursor-row
-- | /No description available in the introspection data./
type TreeViewSelectCursorRowCallback =
    Bool
    -> IO Bool

type C_TreeViewSelectCursorRowCallback =
    Ptr TreeView ->                         -- object
    CInt ->
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_TreeViewSelectCursorRowCallback`.
foreign import ccall "wrapper"
    mk_TreeViewSelectCursorRowCallback :: C_TreeViewSelectCursorRowCallback -> IO (FunPtr C_TreeViewSelectCursorRowCallback)

wrap_TreeViewSelectCursorRowCallback :: 
    GObject a => (a -> TreeViewSelectCursorRowCallback) ->
    C_TreeViewSelectCursorRowCallback
wrap_TreeViewSelectCursorRowCallback gi'cb gi'selfPtr object _ = do
    let object' = (/= 0) object
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  object'
    let result' = (fromIntegral . fromEnum) result
    return result'


-- | Connect a signal handler for the [selectCursorRow](#signal:selectCursorRow) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' treeView #selectCursorRow callback
-- @
-- 
-- 
onTreeViewSelectCursorRow :: (IsTreeView a, MonadIO m) => a -> ((?self :: a) => TreeViewSelectCursorRowCallback) -> m SignalHandlerId
onTreeViewSelectCursorRow obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeViewSelectCursorRowCallback wrapped
    wrapped'' <- mk_TreeViewSelectCursorRowCallback wrapped'
    connectSignalFunPtr obj "select-cursor-row" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [selectCursorRow](#signal:selectCursorRow) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' treeView #selectCursorRow callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTreeViewSelectCursorRow :: (IsTreeView a, MonadIO m) => a -> ((?self :: a) => TreeViewSelectCursorRowCallback) -> m SignalHandlerId
afterTreeViewSelectCursorRow obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeViewSelectCursorRowCallback wrapped
    wrapped'' <- mk_TreeViewSelectCursorRowCallback wrapped'
    connectSignalFunPtr obj "select-cursor-row" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TreeViewSelectCursorRowSignalInfo
instance SignalInfo TreeViewSelectCursorRowSignalInfo where
    type HaskellCallbackType TreeViewSelectCursorRowSignalInfo = TreeViewSelectCursorRowCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TreeViewSelectCursorRowCallback cb
        cb'' <- mk_TreeViewSelectCursorRowCallback cb'
        connectSignalFunPtr obj "select-cursor-row" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView::select-cursor-row"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#g:signal:selectCursorRow"})

#endif

-- signal TreeView::start-interactive-search
-- | /No description available in the introspection data./
type TreeViewStartInteractiveSearchCallback =
    IO Bool

type C_TreeViewStartInteractiveSearchCallback =
    Ptr TreeView ->                         -- object
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_TreeViewStartInteractiveSearchCallback`.
foreign import ccall "wrapper"
    mk_TreeViewStartInteractiveSearchCallback :: C_TreeViewStartInteractiveSearchCallback -> IO (FunPtr C_TreeViewStartInteractiveSearchCallback)

wrap_TreeViewStartInteractiveSearchCallback :: 
    GObject a => (a -> TreeViewStartInteractiveSearchCallback) ->
    C_TreeViewStartInteractiveSearchCallback
wrap_TreeViewStartInteractiveSearchCallback gi'cb gi'selfPtr _ = do
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 
    let result' = (fromIntegral . fromEnum) result
    return result'


-- | Connect a signal handler for the [startInteractiveSearch](#signal:startInteractiveSearch) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' treeView #startInteractiveSearch callback
-- @
-- 
-- 
onTreeViewStartInteractiveSearch :: (IsTreeView a, MonadIO m) => a -> ((?self :: a) => TreeViewStartInteractiveSearchCallback) -> m SignalHandlerId
onTreeViewStartInteractiveSearch obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeViewStartInteractiveSearchCallback wrapped
    wrapped'' <- mk_TreeViewStartInteractiveSearchCallback wrapped'
    connectSignalFunPtr obj "start-interactive-search" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [startInteractiveSearch](#signal:startInteractiveSearch) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' treeView #startInteractiveSearch callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTreeViewStartInteractiveSearch :: (IsTreeView a, MonadIO m) => a -> ((?self :: a) => TreeViewStartInteractiveSearchCallback) -> m SignalHandlerId
afterTreeViewStartInteractiveSearch obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeViewStartInteractiveSearchCallback wrapped
    wrapped'' <- mk_TreeViewStartInteractiveSearchCallback wrapped'
    connectSignalFunPtr obj "start-interactive-search" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TreeViewStartInteractiveSearchSignalInfo
instance SignalInfo TreeViewStartInteractiveSearchSignalInfo where
    type HaskellCallbackType TreeViewStartInteractiveSearchSignalInfo = TreeViewStartInteractiveSearchCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TreeViewStartInteractiveSearchCallback cb
        cb'' <- mk_TreeViewStartInteractiveSearchCallback cb'
        connectSignalFunPtr obj "start-interactive-search" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView::start-interactive-search"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#g:signal:startInteractiveSearch"})

#endif

-- signal TreeView::test-collapse-row
-- | The given row is about to be collapsed (hide its children nodes). Use this
-- signal if you need to control the collapsibility of individual rows.
type TreeViewTestCollapseRowCallback =
    Gtk.TreeIter.TreeIter
    -- ^ /@iter@/: the tree iter of the row to collapse
    -> Gtk.TreePath.TreePath
    -- ^ /@path@/: a tree path that points to the row
    -> IO Bool
    -- ^ __Returns:__ 'P.False' to allow collapsing, 'P.True' to reject

type C_TreeViewTestCollapseRowCallback =
    Ptr TreeView ->                         -- object
    Ptr Gtk.TreeIter.TreeIter ->
    Ptr Gtk.TreePath.TreePath ->
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_TreeViewTestCollapseRowCallback`.
foreign import ccall "wrapper"
    mk_TreeViewTestCollapseRowCallback :: C_TreeViewTestCollapseRowCallback -> IO (FunPtr C_TreeViewTestCollapseRowCallback)

wrap_TreeViewTestCollapseRowCallback :: 
    GObject a => (a -> TreeViewTestCollapseRowCallback) ->
    C_TreeViewTestCollapseRowCallback
wrap_TreeViewTestCollapseRowCallback gi'cb gi'selfPtr iter path _ = do
    B.ManagedPtr.withTransient  iter $ \iter' -> do
        B.ManagedPtr.withTransient  path $ \path' -> do
            result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  iter' path'
            let result' = (fromIntegral . fromEnum) result
            return result'


-- | Connect a signal handler for the [testCollapseRow](#signal:testCollapseRow) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' treeView #testCollapseRow callback
-- @
-- 
-- 
onTreeViewTestCollapseRow :: (IsTreeView a, MonadIO m) => a -> ((?self :: a) => TreeViewTestCollapseRowCallback) -> m SignalHandlerId
onTreeViewTestCollapseRow obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeViewTestCollapseRowCallback wrapped
    wrapped'' <- mk_TreeViewTestCollapseRowCallback wrapped'
    connectSignalFunPtr obj "test-collapse-row" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [testCollapseRow](#signal:testCollapseRow) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' treeView #testCollapseRow callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTreeViewTestCollapseRow :: (IsTreeView a, MonadIO m) => a -> ((?self :: a) => TreeViewTestCollapseRowCallback) -> m SignalHandlerId
afterTreeViewTestCollapseRow obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeViewTestCollapseRowCallback wrapped
    wrapped'' <- mk_TreeViewTestCollapseRowCallback wrapped'
    connectSignalFunPtr obj "test-collapse-row" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TreeViewTestCollapseRowSignalInfo
instance SignalInfo TreeViewTestCollapseRowSignalInfo where
    type HaskellCallbackType TreeViewTestCollapseRowSignalInfo = TreeViewTestCollapseRowCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TreeViewTestCollapseRowCallback cb
        cb'' <- mk_TreeViewTestCollapseRowCallback cb'
        connectSignalFunPtr obj "test-collapse-row" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView::test-collapse-row"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#g:signal:testCollapseRow"})

#endif

-- signal TreeView::test-expand-row
-- | The given row is about to be expanded (show its children nodes). Use this
-- signal if you need to control the expandability of individual rows.
type TreeViewTestExpandRowCallback =
    Gtk.TreeIter.TreeIter
    -- ^ /@iter@/: the tree iter of the row to expand
    -> Gtk.TreePath.TreePath
    -- ^ /@path@/: a tree path that points to the row
    -> IO Bool
    -- ^ __Returns:__ 'P.False' to allow expansion, 'P.True' to reject

type C_TreeViewTestExpandRowCallback =
    Ptr TreeView ->                         -- object
    Ptr Gtk.TreeIter.TreeIter ->
    Ptr Gtk.TreePath.TreePath ->
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_TreeViewTestExpandRowCallback`.
foreign import ccall "wrapper"
    mk_TreeViewTestExpandRowCallback :: C_TreeViewTestExpandRowCallback -> IO (FunPtr C_TreeViewTestExpandRowCallback)

wrap_TreeViewTestExpandRowCallback :: 
    GObject a => (a -> TreeViewTestExpandRowCallback) ->
    C_TreeViewTestExpandRowCallback
wrap_TreeViewTestExpandRowCallback gi'cb gi'selfPtr iter path _ = do
    B.ManagedPtr.withTransient  iter $ \iter' -> do
        B.ManagedPtr.withTransient  path $ \path' -> do
            result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  iter' path'
            let result' = (fromIntegral . fromEnum) result
            return result'


-- | Connect a signal handler for the [testExpandRow](#signal:testExpandRow) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' treeView #testExpandRow callback
-- @
-- 
-- 
onTreeViewTestExpandRow :: (IsTreeView a, MonadIO m) => a -> ((?self :: a) => TreeViewTestExpandRowCallback) -> m SignalHandlerId
onTreeViewTestExpandRow obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeViewTestExpandRowCallback wrapped
    wrapped'' <- mk_TreeViewTestExpandRowCallback wrapped'
    connectSignalFunPtr obj "test-expand-row" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [testExpandRow](#signal:testExpandRow) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' treeView #testExpandRow callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTreeViewTestExpandRow :: (IsTreeView a, MonadIO m) => a -> ((?self :: a) => TreeViewTestExpandRowCallback) -> m SignalHandlerId
afterTreeViewTestExpandRow obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeViewTestExpandRowCallback wrapped
    wrapped'' <- mk_TreeViewTestExpandRowCallback wrapped'
    connectSignalFunPtr obj "test-expand-row" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TreeViewTestExpandRowSignalInfo
instance SignalInfo TreeViewTestExpandRowSignalInfo where
    type HaskellCallbackType TreeViewTestExpandRowSignalInfo = TreeViewTestExpandRowCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TreeViewTestExpandRowCallback cb
        cb'' <- mk_TreeViewTestExpandRowCallback cb'
        connectSignalFunPtr obj "test-expand-row" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView::test-expand-row"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#g:signal:testExpandRow"})

#endif

-- signal TreeView::toggle-cursor-row
-- | /No description available in the introspection data./
type TreeViewToggleCursorRowCallback =
    IO Bool

type C_TreeViewToggleCursorRowCallback =
    Ptr TreeView ->                         -- object
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_TreeViewToggleCursorRowCallback`.
foreign import ccall "wrapper"
    mk_TreeViewToggleCursorRowCallback :: C_TreeViewToggleCursorRowCallback -> IO (FunPtr C_TreeViewToggleCursorRowCallback)

wrap_TreeViewToggleCursorRowCallback :: 
    GObject a => (a -> TreeViewToggleCursorRowCallback) ->
    C_TreeViewToggleCursorRowCallback
wrap_TreeViewToggleCursorRowCallback gi'cb gi'selfPtr _ = do
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 
    let result' = (fromIntegral . fromEnum) result
    return result'


-- | Connect a signal handler for the [toggleCursorRow](#signal:toggleCursorRow) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' treeView #toggleCursorRow callback
-- @
-- 
-- 
onTreeViewToggleCursorRow :: (IsTreeView a, MonadIO m) => a -> ((?self :: a) => TreeViewToggleCursorRowCallback) -> m SignalHandlerId
onTreeViewToggleCursorRow obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeViewToggleCursorRowCallback wrapped
    wrapped'' <- mk_TreeViewToggleCursorRowCallback wrapped'
    connectSignalFunPtr obj "toggle-cursor-row" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [toggleCursorRow](#signal:toggleCursorRow) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' treeView #toggleCursorRow callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTreeViewToggleCursorRow :: (IsTreeView a, MonadIO m) => a -> ((?self :: a) => TreeViewToggleCursorRowCallback) -> m SignalHandlerId
afterTreeViewToggleCursorRow obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeViewToggleCursorRowCallback wrapped
    wrapped'' <- mk_TreeViewToggleCursorRowCallback wrapped'
    connectSignalFunPtr obj "toggle-cursor-row" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TreeViewToggleCursorRowSignalInfo
instance SignalInfo TreeViewToggleCursorRowSignalInfo where
    type HaskellCallbackType TreeViewToggleCursorRowSignalInfo = TreeViewToggleCursorRowCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TreeViewToggleCursorRowCallback cb
        cb'' <- mk_TreeViewToggleCursorRowCallback cb'
        connectSignalFunPtr obj "toggle-cursor-row" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView::toggle-cursor-row"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#g:signal:toggleCursorRow"})

#endif

-- signal TreeView::unselect-all
-- | /No description available in the introspection data./
type TreeViewUnselectAllCallback =
    IO Bool

type C_TreeViewUnselectAllCallback =
    Ptr TreeView ->                         -- object
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_TreeViewUnselectAllCallback`.
foreign import ccall "wrapper"
    mk_TreeViewUnselectAllCallback :: C_TreeViewUnselectAllCallback -> IO (FunPtr C_TreeViewUnselectAllCallback)

wrap_TreeViewUnselectAllCallback :: 
    GObject a => (a -> TreeViewUnselectAllCallback) ->
    C_TreeViewUnselectAllCallback
wrap_TreeViewUnselectAllCallback gi'cb gi'selfPtr _ = do
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 
    let result' = (fromIntegral . fromEnum) result
    return result'


-- | Connect a signal handler for the [unselectAll](#signal:unselectAll) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' treeView #unselectAll callback
-- @
-- 
-- 
onTreeViewUnselectAll :: (IsTreeView a, MonadIO m) => a -> ((?self :: a) => TreeViewUnselectAllCallback) -> m SignalHandlerId
onTreeViewUnselectAll obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeViewUnselectAllCallback wrapped
    wrapped'' <- mk_TreeViewUnselectAllCallback wrapped'
    connectSignalFunPtr obj "unselect-all" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [unselectAll](#signal:unselectAll) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' treeView #unselectAll callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTreeViewUnselectAll :: (IsTreeView a, MonadIO m) => a -> ((?self :: a) => TreeViewUnselectAllCallback) -> m SignalHandlerId
afterTreeViewUnselectAll obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeViewUnselectAllCallback wrapped
    wrapped'' <- mk_TreeViewUnselectAllCallback wrapped'
    connectSignalFunPtr obj "unselect-all" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TreeViewUnselectAllSignalInfo
instance SignalInfo TreeViewUnselectAllSignalInfo where
    type HaskellCallbackType TreeViewUnselectAllSignalInfo = TreeViewUnselectAllCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TreeViewUnselectAllCallback cb
        cb'' <- mk_TreeViewUnselectAllCallback cb'
        connectSignalFunPtr obj "unselect-all" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView::unselect-all"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#g:signal:unselectAll"})

#endif

-- VVV Prop "activate-on-single-click"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@activate-on-single-click@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeView #activateOnSingleClick
-- @
getTreeViewActivateOnSingleClick :: (MonadIO m, IsTreeView o) => o -> m Bool
getTreeViewActivateOnSingleClick obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "activate-on-single-click"

-- | Set the value of the “@activate-on-single-click@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeView [ #activateOnSingleClick 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeViewActivateOnSingleClick :: (MonadIO m, IsTreeView o) => o -> Bool -> m ()
setTreeViewActivateOnSingleClick obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "activate-on-single-click" val

-- | Construct a `GValueConstruct` with valid value for the “@activate-on-single-click@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewActivateOnSingleClick :: (IsTreeView o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTreeViewActivateOnSingleClick val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "activate-on-single-click" val

#if defined(ENABLE_OVERLOADING)
data TreeViewActivateOnSingleClickPropertyInfo
instance AttrInfo TreeViewActivateOnSingleClickPropertyInfo where
    type AttrAllowedOps TreeViewActivateOnSingleClickPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TreeViewActivateOnSingleClickPropertyInfo = IsTreeView
    type AttrSetTypeConstraint TreeViewActivateOnSingleClickPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TreeViewActivateOnSingleClickPropertyInfo = (~) Bool
    type AttrTransferType TreeViewActivateOnSingleClickPropertyInfo = Bool
    type AttrGetType TreeViewActivateOnSingleClickPropertyInfo = Bool
    type AttrLabel TreeViewActivateOnSingleClickPropertyInfo = "activate-on-single-click"
    type AttrOrigin TreeViewActivateOnSingleClickPropertyInfo = TreeView
    attrGet = getTreeViewActivateOnSingleClick
    attrSet = setTreeViewActivateOnSingleClick
    attrTransfer _ v = do
        return v
    attrConstruct = constructTreeViewActivateOnSingleClick
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.activateOnSingleClick"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#g:attr:activateOnSingleClick"
        })
#endif

-- VVV Prop "enable-grid-lines"
   -- Type: TInterface (Name {namespace = "Gtk", name = "TreeViewGridLines"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@enable-grid-lines@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeView #enableGridLines
-- @
getTreeViewEnableGridLines :: (MonadIO m, IsTreeView o) => o -> m Gtk.Enums.TreeViewGridLines
getTreeViewEnableGridLines obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "enable-grid-lines"

-- | Set the value of the “@enable-grid-lines@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeView [ #enableGridLines 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeViewEnableGridLines :: (MonadIO m, IsTreeView o) => o -> Gtk.Enums.TreeViewGridLines -> m ()
setTreeViewEnableGridLines obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "enable-grid-lines" val

-- | Construct a `GValueConstruct` with valid value for the “@enable-grid-lines@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewEnableGridLines :: (IsTreeView o, MIO.MonadIO m) => Gtk.Enums.TreeViewGridLines -> m (GValueConstruct o)
constructTreeViewEnableGridLines val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "enable-grid-lines" val

#if defined(ENABLE_OVERLOADING)
data TreeViewEnableGridLinesPropertyInfo
instance AttrInfo TreeViewEnableGridLinesPropertyInfo where
    type AttrAllowedOps TreeViewEnableGridLinesPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TreeViewEnableGridLinesPropertyInfo = IsTreeView
    type AttrSetTypeConstraint TreeViewEnableGridLinesPropertyInfo = (~) Gtk.Enums.TreeViewGridLines
    type AttrTransferTypeConstraint TreeViewEnableGridLinesPropertyInfo = (~) Gtk.Enums.TreeViewGridLines
    type AttrTransferType TreeViewEnableGridLinesPropertyInfo = Gtk.Enums.TreeViewGridLines
    type AttrGetType TreeViewEnableGridLinesPropertyInfo = Gtk.Enums.TreeViewGridLines
    type AttrLabel TreeViewEnableGridLinesPropertyInfo = "enable-grid-lines"
    type AttrOrigin TreeViewEnableGridLinesPropertyInfo = TreeView
    attrGet = getTreeViewEnableGridLines
    attrSet = setTreeViewEnableGridLines
    attrTransfer _ v = do
        return v
    attrConstruct = constructTreeViewEnableGridLines
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.enableGridLines"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#g:attr:enableGridLines"
        })
#endif

-- VVV Prop "enable-search"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@enable-search@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeView #enableSearch
-- @
getTreeViewEnableSearch :: (MonadIO m, IsTreeView o) => o -> m Bool
getTreeViewEnableSearch obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "enable-search"

-- | Set the value of the “@enable-search@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeView [ #enableSearch 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeViewEnableSearch :: (MonadIO m, IsTreeView o) => o -> Bool -> m ()
setTreeViewEnableSearch obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "enable-search" val

-- | Construct a `GValueConstruct` with valid value for the “@enable-search@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewEnableSearch :: (IsTreeView o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTreeViewEnableSearch val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "enable-search" val

#if defined(ENABLE_OVERLOADING)
data TreeViewEnableSearchPropertyInfo
instance AttrInfo TreeViewEnableSearchPropertyInfo where
    type AttrAllowedOps TreeViewEnableSearchPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TreeViewEnableSearchPropertyInfo = IsTreeView
    type AttrSetTypeConstraint TreeViewEnableSearchPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TreeViewEnableSearchPropertyInfo = (~) Bool
    type AttrTransferType TreeViewEnableSearchPropertyInfo = Bool
    type AttrGetType TreeViewEnableSearchPropertyInfo = Bool
    type AttrLabel TreeViewEnableSearchPropertyInfo = "enable-search"
    type AttrOrigin TreeViewEnableSearchPropertyInfo = TreeView
    attrGet = getTreeViewEnableSearch
    attrSet = setTreeViewEnableSearch
    attrTransfer _ v = do
        return v
    attrConstruct = constructTreeViewEnableSearch
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.enableSearch"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#g:attr:enableSearch"
        })
#endif

-- VVV Prop "enable-tree-lines"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@enable-tree-lines@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeView #enableTreeLines
-- @
getTreeViewEnableTreeLines :: (MonadIO m, IsTreeView o) => o -> m Bool
getTreeViewEnableTreeLines obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "enable-tree-lines"

-- | Set the value of the “@enable-tree-lines@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeView [ #enableTreeLines 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeViewEnableTreeLines :: (MonadIO m, IsTreeView o) => o -> Bool -> m ()
setTreeViewEnableTreeLines obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "enable-tree-lines" val

-- | Construct a `GValueConstruct` with valid value for the “@enable-tree-lines@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewEnableTreeLines :: (IsTreeView o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTreeViewEnableTreeLines val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "enable-tree-lines" val

#if defined(ENABLE_OVERLOADING)
data TreeViewEnableTreeLinesPropertyInfo
instance AttrInfo TreeViewEnableTreeLinesPropertyInfo where
    type AttrAllowedOps TreeViewEnableTreeLinesPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TreeViewEnableTreeLinesPropertyInfo = IsTreeView
    type AttrSetTypeConstraint TreeViewEnableTreeLinesPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TreeViewEnableTreeLinesPropertyInfo = (~) Bool
    type AttrTransferType TreeViewEnableTreeLinesPropertyInfo = Bool
    type AttrGetType TreeViewEnableTreeLinesPropertyInfo = Bool
    type AttrLabel TreeViewEnableTreeLinesPropertyInfo = "enable-tree-lines"
    type AttrOrigin TreeViewEnableTreeLinesPropertyInfo = TreeView
    attrGet = getTreeViewEnableTreeLines
    attrSet = setTreeViewEnableTreeLines
    attrTransfer _ v = do
        return v
    attrConstruct = constructTreeViewEnableTreeLines
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.enableTreeLines"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#g:attr:enableTreeLines"
        })
#endif

-- VVV Prop "expander-column"
   -- Type: TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just True)

-- | Get the value of the “@expander-column@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeView #expanderColumn
-- @
getTreeViewExpanderColumn :: (MonadIO m, IsTreeView o) => o -> m Gtk.TreeViewColumn.TreeViewColumn
getTreeViewExpanderColumn obj = MIO.liftIO $ checkUnexpectedNothing "getTreeViewExpanderColumn" $ B.Properties.getObjectPropertyObject obj "expander-column" Gtk.TreeViewColumn.TreeViewColumn

-- | Set the value of the “@expander-column@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeView [ #expanderColumn 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeViewExpanderColumn :: (MonadIO m, IsTreeView o, Gtk.TreeViewColumn.IsTreeViewColumn a) => o -> a -> m ()
setTreeViewExpanderColumn obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "expander-column" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@expander-column@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewExpanderColumn :: (IsTreeView o, MIO.MonadIO m, Gtk.TreeViewColumn.IsTreeViewColumn a) => a -> m (GValueConstruct o)
constructTreeViewExpanderColumn val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "expander-column" (P.Just val)

-- | Set the value of the “@expander-column@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #expanderColumn
-- @
clearTreeViewExpanderColumn :: (MonadIO m, IsTreeView o) => o -> m ()
clearTreeViewExpanderColumn obj = liftIO $ B.Properties.setObjectPropertyObject obj "expander-column" (Nothing :: Maybe Gtk.TreeViewColumn.TreeViewColumn)

#if defined(ENABLE_OVERLOADING)
data TreeViewExpanderColumnPropertyInfo
instance AttrInfo TreeViewExpanderColumnPropertyInfo where
    type AttrAllowedOps TreeViewExpanderColumnPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint TreeViewExpanderColumnPropertyInfo = IsTreeView
    type AttrSetTypeConstraint TreeViewExpanderColumnPropertyInfo = Gtk.TreeViewColumn.IsTreeViewColumn
    type AttrTransferTypeConstraint TreeViewExpanderColumnPropertyInfo = Gtk.TreeViewColumn.IsTreeViewColumn
    type AttrTransferType TreeViewExpanderColumnPropertyInfo = Gtk.TreeViewColumn.TreeViewColumn
    type AttrGetType TreeViewExpanderColumnPropertyInfo = Gtk.TreeViewColumn.TreeViewColumn
    type AttrLabel TreeViewExpanderColumnPropertyInfo = "expander-column"
    type AttrOrigin TreeViewExpanderColumnPropertyInfo = TreeView
    attrGet = getTreeViewExpanderColumn
    attrSet = setTreeViewExpanderColumn
    attrTransfer _ v = do
        unsafeCastTo Gtk.TreeViewColumn.TreeViewColumn v
    attrConstruct = constructTreeViewExpanderColumn
    attrClear = clearTreeViewExpanderColumn
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.expanderColumn"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#g:attr:expanderColumn"
        })
#endif

-- VVV Prop "fixed-height-mode"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@fixed-height-mode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeView #fixedHeightMode
-- @
getTreeViewFixedHeightMode :: (MonadIO m, IsTreeView o) => o -> m Bool
getTreeViewFixedHeightMode obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "fixed-height-mode"

-- | Set the value of the “@fixed-height-mode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeView [ #fixedHeightMode 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeViewFixedHeightMode :: (MonadIO m, IsTreeView o) => o -> Bool -> m ()
setTreeViewFixedHeightMode obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "fixed-height-mode" val

-- | Construct a `GValueConstruct` with valid value for the “@fixed-height-mode@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewFixedHeightMode :: (IsTreeView o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTreeViewFixedHeightMode val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "fixed-height-mode" val

#if defined(ENABLE_OVERLOADING)
data TreeViewFixedHeightModePropertyInfo
instance AttrInfo TreeViewFixedHeightModePropertyInfo where
    type AttrAllowedOps TreeViewFixedHeightModePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TreeViewFixedHeightModePropertyInfo = IsTreeView
    type AttrSetTypeConstraint TreeViewFixedHeightModePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TreeViewFixedHeightModePropertyInfo = (~) Bool
    type AttrTransferType TreeViewFixedHeightModePropertyInfo = Bool
    type AttrGetType TreeViewFixedHeightModePropertyInfo = Bool
    type AttrLabel TreeViewFixedHeightModePropertyInfo = "fixed-height-mode"
    type AttrOrigin TreeViewFixedHeightModePropertyInfo = TreeView
    attrGet = getTreeViewFixedHeightMode
    attrSet = setTreeViewFixedHeightMode
    attrTransfer _ v = do
        return v
    attrConstruct = constructTreeViewFixedHeightMode
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.fixedHeightMode"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#g:attr:fixedHeightMode"
        })
#endif

-- VVV Prop "headers-clickable"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@headers-clickable@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeView #headersClickable
-- @
getTreeViewHeadersClickable :: (MonadIO m, IsTreeView o) => o -> m Bool
getTreeViewHeadersClickable obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "headers-clickable"

-- | Set the value of the “@headers-clickable@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeView [ #headersClickable 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeViewHeadersClickable :: (MonadIO m, IsTreeView o) => o -> Bool -> m ()
setTreeViewHeadersClickable obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "headers-clickable" val

-- | Construct a `GValueConstruct` with valid value for the “@headers-clickable@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewHeadersClickable :: (IsTreeView o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTreeViewHeadersClickable val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "headers-clickable" val

#if defined(ENABLE_OVERLOADING)
data TreeViewHeadersClickablePropertyInfo
instance AttrInfo TreeViewHeadersClickablePropertyInfo where
    type AttrAllowedOps TreeViewHeadersClickablePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TreeViewHeadersClickablePropertyInfo = IsTreeView
    type AttrSetTypeConstraint TreeViewHeadersClickablePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TreeViewHeadersClickablePropertyInfo = (~) Bool
    type AttrTransferType TreeViewHeadersClickablePropertyInfo = Bool
    type AttrGetType TreeViewHeadersClickablePropertyInfo = Bool
    type AttrLabel TreeViewHeadersClickablePropertyInfo = "headers-clickable"
    type AttrOrigin TreeViewHeadersClickablePropertyInfo = TreeView
    attrGet = getTreeViewHeadersClickable
    attrSet = setTreeViewHeadersClickable
    attrTransfer _ v = do
        return v
    attrConstruct = constructTreeViewHeadersClickable
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.headersClickable"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#g:attr:headersClickable"
        })
#endif

-- VVV Prop "headers-visible"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@headers-visible@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeView #headersVisible
-- @
getTreeViewHeadersVisible :: (MonadIO m, IsTreeView o) => o -> m Bool
getTreeViewHeadersVisible obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "headers-visible"

-- | Set the value of the “@headers-visible@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeView [ #headersVisible 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeViewHeadersVisible :: (MonadIO m, IsTreeView o) => o -> Bool -> m ()
setTreeViewHeadersVisible obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "headers-visible" val

-- | Construct a `GValueConstruct` with valid value for the “@headers-visible@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewHeadersVisible :: (IsTreeView o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTreeViewHeadersVisible val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "headers-visible" val

#if defined(ENABLE_OVERLOADING)
data TreeViewHeadersVisiblePropertyInfo
instance AttrInfo TreeViewHeadersVisiblePropertyInfo where
    type AttrAllowedOps TreeViewHeadersVisiblePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TreeViewHeadersVisiblePropertyInfo = IsTreeView
    type AttrSetTypeConstraint TreeViewHeadersVisiblePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TreeViewHeadersVisiblePropertyInfo = (~) Bool
    type AttrTransferType TreeViewHeadersVisiblePropertyInfo = Bool
    type AttrGetType TreeViewHeadersVisiblePropertyInfo = Bool
    type AttrLabel TreeViewHeadersVisiblePropertyInfo = "headers-visible"
    type AttrOrigin TreeViewHeadersVisiblePropertyInfo = TreeView
    attrGet = getTreeViewHeadersVisible
    attrSet = setTreeViewHeadersVisible
    attrTransfer _ v = do
        return v
    attrConstruct = constructTreeViewHeadersVisible
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.headersVisible"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#g:attr:headersVisible"
        })
#endif

-- VVV Prop "hover-expand"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@hover-expand@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeView #hoverExpand
-- @
getTreeViewHoverExpand :: (MonadIO m, IsTreeView o) => o -> m Bool
getTreeViewHoverExpand obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "hover-expand"

-- | Set the value of the “@hover-expand@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeView [ #hoverExpand 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeViewHoverExpand :: (MonadIO m, IsTreeView o) => o -> Bool -> m ()
setTreeViewHoverExpand obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "hover-expand" val

-- | Construct a `GValueConstruct` with valid value for the “@hover-expand@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewHoverExpand :: (IsTreeView o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTreeViewHoverExpand val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "hover-expand" val

#if defined(ENABLE_OVERLOADING)
data TreeViewHoverExpandPropertyInfo
instance AttrInfo TreeViewHoverExpandPropertyInfo where
    type AttrAllowedOps TreeViewHoverExpandPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TreeViewHoverExpandPropertyInfo = IsTreeView
    type AttrSetTypeConstraint TreeViewHoverExpandPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TreeViewHoverExpandPropertyInfo = (~) Bool
    type AttrTransferType TreeViewHoverExpandPropertyInfo = Bool
    type AttrGetType TreeViewHoverExpandPropertyInfo = Bool
    type AttrLabel TreeViewHoverExpandPropertyInfo = "hover-expand"
    type AttrOrigin TreeViewHoverExpandPropertyInfo = TreeView
    attrGet = getTreeViewHoverExpand
    attrSet = setTreeViewHoverExpand
    attrTransfer _ v = do
        return v
    attrConstruct = constructTreeViewHoverExpand
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.hoverExpand"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#g:attr:hoverExpand"
        })
#endif

-- VVV Prop "hover-selection"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@hover-selection@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeView #hoverSelection
-- @
getTreeViewHoverSelection :: (MonadIO m, IsTreeView o) => o -> m Bool
getTreeViewHoverSelection obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "hover-selection"

-- | Set the value of the “@hover-selection@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeView [ #hoverSelection 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeViewHoverSelection :: (MonadIO m, IsTreeView o) => o -> Bool -> m ()
setTreeViewHoverSelection obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "hover-selection" val

-- | Construct a `GValueConstruct` with valid value for the “@hover-selection@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewHoverSelection :: (IsTreeView o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTreeViewHoverSelection val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "hover-selection" val

#if defined(ENABLE_OVERLOADING)
data TreeViewHoverSelectionPropertyInfo
instance AttrInfo TreeViewHoverSelectionPropertyInfo where
    type AttrAllowedOps TreeViewHoverSelectionPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TreeViewHoverSelectionPropertyInfo = IsTreeView
    type AttrSetTypeConstraint TreeViewHoverSelectionPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TreeViewHoverSelectionPropertyInfo = (~) Bool
    type AttrTransferType TreeViewHoverSelectionPropertyInfo = Bool
    type AttrGetType TreeViewHoverSelectionPropertyInfo = Bool
    type AttrLabel TreeViewHoverSelectionPropertyInfo = "hover-selection"
    type AttrOrigin TreeViewHoverSelectionPropertyInfo = TreeView
    attrGet = getTreeViewHoverSelection
    attrSet = setTreeViewHoverSelection
    attrTransfer _ v = do
        return v
    attrConstruct = constructTreeViewHoverSelection
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.hoverSelection"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#g:attr:hoverSelection"
        })
#endif

-- VVV Prop "level-indentation"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@level-indentation@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeView #levelIndentation
-- @
getTreeViewLevelIndentation :: (MonadIO m, IsTreeView o) => o -> m Int32
getTreeViewLevelIndentation obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "level-indentation"

-- | Set the value of the “@level-indentation@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeView [ #levelIndentation 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeViewLevelIndentation :: (MonadIO m, IsTreeView o) => o -> Int32 -> m ()
setTreeViewLevelIndentation obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "level-indentation" val

-- | Construct a `GValueConstruct` with valid value for the “@level-indentation@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewLevelIndentation :: (IsTreeView o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructTreeViewLevelIndentation val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "level-indentation" val

#if defined(ENABLE_OVERLOADING)
data TreeViewLevelIndentationPropertyInfo
instance AttrInfo TreeViewLevelIndentationPropertyInfo where
    type AttrAllowedOps TreeViewLevelIndentationPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TreeViewLevelIndentationPropertyInfo = IsTreeView
    type AttrSetTypeConstraint TreeViewLevelIndentationPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint TreeViewLevelIndentationPropertyInfo = (~) Int32
    type AttrTransferType TreeViewLevelIndentationPropertyInfo = Int32
    type AttrGetType TreeViewLevelIndentationPropertyInfo = Int32
    type AttrLabel TreeViewLevelIndentationPropertyInfo = "level-indentation"
    type AttrOrigin TreeViewLevelIndentationPropertyInfo = TreeView
    attrGet = getTreeViewLevelIndentation
    attrSet = setTreeViewLevelIndentation
    attrTransfer _ v = do
        return v
    attrConstruct = constructTreeViewLevelIndentation
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.levelIndentation"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#g:attr:levelIndentation"
        })
#endif

-- VVV Prop "model"
   -- Type: TInterface (Name {namespace = "Gtk", name = "TreeModel"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just True)

-- | Get the value of the “@model@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeView #model
-- @
getTreeViewModel :: (MonadIO m, IsTreeView o) => o -> m (Maybe Gtk.TreeModel.TreeModel)
getTreeViewModel obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "model" Gtk.TreeModel.TreeModel

-- | Set the value of the “@model@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeView [ #model 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeViewModel :: (MonadIO m, IsTreeView o, Gtk.TreeModel.IsTreeModel a) => o -> a -> m ()
setTreeViewModel obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "model" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@model@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewModel :: (IsTreeView o, MIO.MonadIO m, Gtk.TreeModel.IsTreeModel a) => a -> m (GValueConstruct o)
constructTreeViewModel val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "model" (P.Just val)

-- | Set the value of the “@model@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #model
-- @
clearTreeViewModel :: (MonadIO m, IsTreeView o) => o -> m ()
clearTreeViewModel obj = liftIO $ B.Properties.setObjectPropertyObject obj "model" (Nothing :: Maybe Gtk.TreeModel.TreeModel)

#if defined(ENABLE_OVERLOADING)
data TreeViewModelPropertyInfo
instance AttrInfo TreeViewModelPropertyInfo where
    type AttrAllowedOps TreeViewModelPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint TreeViewModelPropertyInfo = IsTreeView
    type AttrSetTypeConstraint TreeViewModelPropertyInfo = Gtk.TreeModel.IsTreeModel
    type AttrTransferTypeConstraint TreeViewModelPropertyInfo = Gtk.TreeModel.IsTreeModel
    type AttrTransferType TreeViewModelPropertyInfo = Gtk.TreeModel.TreeModel
    type AttrGetType TreeViewModelPropertyInfo = (Maybe Gtk.TreeModel.TreeModel)
    type AttrLabel TreeViewModelPropertyInfo = "model"
    type AttrOrigin TreeViewModelPropertyInfo = TreeView
    attrGet = getTreeViewModel
    attrSet = setTreeViewModel
    attrTransfer _ v = do
        unsafeCastTo Gtk.TreeModel.TreeModel v
    attrConstruct = constructTreeViewModel
    attrClear = clearTreeViewModel
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.model"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#g:attr:model"
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
-- 'Data.GI.Base.Attributes.get' treeView #reorderable
-- @
getTreeViewReorderable :: (MonadIO m, IsTreeView o) => o -> m Bool
getTreeViewReorderable obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "reorderable"

-- | Set the value of the “@reorderable@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeView [ #reorderable 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeViewReorderable :: (MonadIO m, IsTreeView o) => o -> Bool -> m ()
setTreeViewReorderable obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "reorderable" val

-- | Construct a `GValueConstruct` with valid value for the “@reorderable@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewReorderable :: (IsTreeView o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTreeViewReorderable val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "reorderable" val

#if defined(ENABLE_OVERLOADING)
data TreeViewReorderablePropertyInfo
instance AttrInfo TreeViewReorderablePropertyInfo where
    type AttrAllowedOps TreeViewReorderablePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TreeViewReorderablePropertyInfo = IsTreeView
    type AttrSetTypeConstraint TreeViewReorderablePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TreeViewReorderablePropertyInfo = (~) Bool
    type AttrTransferType TreeViewReorderablePropertyInfo = Bool
    type AttrGetType TreeViewReorderablePropertyInfo = Bool
    type AttrLabel TreeViewReorderablePropertyInfo = "reorderable"
    type AttrOrigin TreeViewReorderablePropertyInfo = TreeView
    attrGet = getTreeViewReorderable
    attrSet = setTreeViewReorderable
    attrTransfer _ v = do
        return v
    attrConstruct = constructTreeViewReorderable
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.reorderable"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#g:attr:reorderable"
        })
#endif

-- VVV Prop "rubber-banding"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@rubber-banding@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeView #rubberBanding
-- @
getTreeViewRubberBanding :: (MonadIO m, IsTreeView o) => o -> m Bool
getTreeViewRubberBanding obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "rubber-banding"

-- | Set the value of the “@rubber-banding@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeView [ #rubberBanding 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeViewRubberBanding :: (MonadIO m, IsTreeView o) => o -> Bool -> m ()
setTreeViewRubberBanding obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "rubber-banding" val

-- | Construct a `GValueConstruct` with valid value for the “@rubber-banding@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewRubberBanding :: (IsTreeView o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTreeViewRubberBanding val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "rubber-banding" val

#if defined(ENABLE_OVERLOADING)
data TreeViewRubberBandingPropertyInfo
instance AttrInfo TreeViewRubberBandingPropertyInfo where
    type AttrAllowedOps TreeViewRubberBandingPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TreeViewRubberBandingPropertyInfo = IsTreeView
    type AttrSetTypeConstraint TreeViewRubberBandingPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TreeViewRubberBandingPropertyInfo = (~) Bool
    type AttrTransferType TreeViewRubberBandingPropertyInfo = Bool
    type AttrGetType TreeViewRubberBandingPropertyInfo = Bool
    type AttrLabel TreeViewRubberBandingPropertyInfo = "rubber-banding"
    type AttrOrigin TreeViewRubberBandingPropertyInfo = TreeView
    attrGet = getTreeViewRubberBanding
    attrSet = setTreeViewRubberBanding
    attrTransfer _ v = do
        return v
    attrConstruct = constructTreeViewRubberBanding
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.rubberBanding"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#g:attr:rubberBanding"
        })
#endif

-- VVV Prop "rules-hint"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@rules-hint@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeView #rulesHint
-- @
getTreeViewRulesHint :: (MonadIO m, IsTreeView o) => o -> m Bool
getTreeViewRulesHint obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "rules-hint"

-- | Set the value of the “@rules-hint@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeView [ #rulesHint 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeViewRulesHint :: (MonadIO m, IsTreeView o) => o -> Bool -> m ()
setTreeViewRulesHint obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "rules-hint" val

-- | Construct a `GValueConstruct` with valid value for the “@rules-hint@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewRulesHint :: (IsTreeView o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTreeViewRulesHint val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "rules-hint" val

#if defined(ENABLE_OVERLOADING)
data TreeViewRulesHintPropertyInfo
instance AttrInfo TreeViewRulesHintPropertyInfo where
    type AttrAllowedOps TreeViewRulesHintPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TreeViewRulesHintPropertyInfo = IsTreeView
    type AttrSetTypeConstraint TreeViewRulesHintPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TreeViewRulesHintPropertyInfo = (~) Bool
    type AttrTransferType TreeViewRulesHintPropertyInfo = Bool
    type AttrGetType TreeViewRulesHintPropertyInfo = Bool
    type AttrLabel TreeViewRulesHintPropertyInfo = "rules-hint"
    type AttrOrigin TreeViewRulesHintPropertyInfo = TreeView
    attrGet = getTreeViewRulesHint
    attrSet = setTreeViewRulesHint
    attrTransfer _ v = do
        return v
    attrConstruct = constructTreeViewRulesHint
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.rulesHint"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#g:attr:rulesHint"
        })
#endif

-- VVV Prop "search-column"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@search-column@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeView #searchColumn
-- @
getTreeViewSearchColumn :: (MonadIO m, IsTreeView o) => o -> m Int32
getTreeViewSearchColumn obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "search-column"

-- | Set the value of the “@search-column@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeView [ #searchColumn 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeViewSearchColumn :: (MonadIO m, IsTreeView o) => o -> Int32 -> m ()
setTreeViewSearchColumn obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "search-column" val

-- | Construct a `GValueConstruct` with valid value for the “@search-column@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewSearchColumn :: (IsTreeView o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructTreeViewSearchColumn val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "search-column" val

#if defined(ENABLE_OVERLOADING)
data TreeViewSearchColumnPropertyInfo
instance AttrInfo TreeViewSearchColumnPropertyInfo where
    type AttrAllowedOps TreeViewSearchColumnPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TreeViewSearchColumnPropertyInfo = IsTreeView
    type AttrSetTypeConstraint TreeViewSearchColumnPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint TreeViewSearchColumnPropertyInfo = (~) Int32
    type AttrTransferType TreeViewSearchColumnPropertyInfo = Int32
    type AttrGetType TreeViewSearchColumnPropertyInfo = Int32
    type AttrLabel TreeViewSearchColumnPropertyInfo = "search-column"
    type AttrOrigin TreeViewSearchColumnPropertyInfo = TreeView
    attrGet = getTreeViewSearchColumn
    attrSet = setTreeViewSearchColumn
    attrTransfer _ v = do
        return v
    attrConstruct = constructTreeViewSearchColumn
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.searchColumn"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#g:attr:searchColumn"
        })
#endif

-- VVV Prop "show-expanders"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@show-expanders@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeView #showExpanders
-- @
getTreeViewShowExpanders :: (MonadIO m, IsTreeView o) => o -> m Bool
getTreeViewShowExpanders obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "show-expanders"

-- | Set the value of the “@show-expanders@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeView [ #showExpanders 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeViewShowExpanders :: (MonadIO m, IsTreeView o) => o -> Bool -> m ()
setTreeViewShowExpanders obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "show-expanders" val

-- | Construct a `GValueConstruct` with valid value for the “@show-expanders@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewShowExpanders :: (IsTreeView o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTreeViewShowExpanders val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "show-expanders" val

#if defined(ENABLE_OVERLOADING)
data TreeViewShowExpandersPropertyInfo
instance AttrInfo TreeViewShowExpandersPropertyInfo where
    type AttrAllowedOps TreeViewShowExpandersPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TreeViewShowExpandersPropertyInfo = IsTreeView
    type AttrSetTypeConstraint TreeViewShowExpandersPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TreeViewShowExpandersPropertyInfo = (~) Bool
    type AttrTransferType TreeViewShowExpandersPropertyInfo = Bool
    type AttrGetType TreeViewShowExpandersPropertyInfo = Bool
    type AttrLabel TreeViewShowExpandersPropertyInfo = "show-expanders"
    type AttrOrigin TreeViewShowExpandersPropertyInfo = TreeView
    attrGet = getTreeViewShowExpanders
    attrSet = setTreeViewShowExpanders
    attrTransfer _ v = do
        return v
    attrConstruct = constructTreeViewShowExpanders
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.showExpanders"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#g:attr:showExpanders"
        })
#endif

-- VVV Prop "tooltip-column"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@tooltip-column@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeView #tooltipColumn
-- @
getTreeViewTooltipColumn :: (MonadIO m, IsTreeView o) => o -> m Int32
getTreeViewTooltipColumn obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "tooltip-column"

-- | Set the value of the “@tooltip-column@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeView [ #tooltipColumn 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeViewTooltipColumn :: (MonadIO m, IsTreeView o) => o -> Int32 -> m ()
setTreeViewTooltipColumn obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "tooltip-column" val

-- | Construct a `GValueConstruct` with valid value for the “@tooltip-column@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeViewTooltipColumn :: (IsTreeView o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructTreeViewTooltipColumn val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "tooltip-column" val

#if defined(ENABLE_OVERLOADING)
data TreeViewTooltipColumnPropertyInfo
instance AttrInfo TreeViewTooltipColumnPropertyInfo where
    type AttrAllowedOps TreeViewTooltipColumnPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TreeViewTooltipColumnPropertyInfo = IsTreeView
    type AttrSetTypeConstraint TreeViewTooltipColumnPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint TreeViewTooltipColumnPropertyInfo = (~) Int32
    type AttrTransferType TreeViewTooltipColumnPropertyInfo = Int32
    type AttrGetType TreeViewTooltipColumnPropertyInfo = Int32
    type AttrLabel TreeViewTooltipColumnPropertyInfo = "tooltip-column"
    type AttrOrigin TreeViewTooltipColumnPropertyInfo = TreeView
    attrGet = getTreeViewTooltipColumn
    attrSet = setTreeViewTooltipColumn
    attrTransfer _ v = do
        return v
    attrConstruct = constructTreeViewTooltipColumn
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.tooltipColumn"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#g:attr:tooltipColumn"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList TreeView
type instance O.AttributeList TreeView = TreeViewAttributeList
type TreeViewAttributeList = ('[ '("activateOnSingleClick", TreeViewActivateOnSingleClickPropertyInfo), '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("enableGridLines", TreeViewEnableGridLinesPropertyInfo), '("enableSearch", TreeViewEnableSearchPropertyInfo), '("enableTreeLines", TreeViewEnableTreeLinesPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("expanderColumn", TreeViewExpanderColumnPropertyInfo), '("fixedHeightMode", TreeViewFixedHeightModePropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("hadjustment", Gtk.Scrollable.ScrollableHadjustmentPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("headersClickable", TreeViewHeadersClickablePropertyInfo), '("headersVisible", TreeViewHeadersVisiblePropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("hoverExpand", TreeViewHoverExpandPropertyInfo), '("hoverSelection", TreeViewHoverSelectionPropertyInfo), '("hscrollPolicy", Gtk.Scrollable.ScrollableHscrollPolicyPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("levelIndentation", TreeViewLevelIndentationPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("model", TreeViewModelPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("reorderable", TreeViewReorderablePropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("rubberBanding", TreeViewRubberBandingPropertyInfo), '("rulesHint", TreeViewRulesHintPropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("searchColumn", TreeViewSearchColumnPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("showExpanders", TreeViewShowExpandersPropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tooltipColumn", TreeViewTooltipColumnPropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("vadjustment", Gtk.Scrollable.ScrollableVadjustmentPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("vscrollPolicy", Gtk.Scrollable.ScrollableVscrollPolicyPropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
treeViewActivateOnSingleClick :: AttrLabelProxy "activateOnSingleClick"
treeViewActivateOnSingleClick = AttrLabelProxy

treeViewEnableGridLines :: AttrLabelProxy "enableGridLines"
treeViewEnableGridLines = AttrLabelProxy

treeViewEnableSearch :: AttrLabelProxy "enableSearch"
treeViewEnableSearch = AttrLabelProxy

treeViewEnableTreeLines :: AttrLabelProxy "enableTreeLines"
treeViewEnableTreeLines = AttrLabelProxy

treeViewExpanderColumn :: AttrLabelProxy "expanderColumn"
treeViewExpanderColumn = AttrLabelProxy

treeViewFixedHeightMode :: AttrLabelProxy "fixedHeightMode"
treeViewFixedHeightMode = AttrLabelProxy

treeViewHeadersClickable :: AttrLabelProxy "headersClickable"
treeViewHeadersClickable = AttrLabelProxy

treeViewHeadersVisible :: AttrLabelProxy "headersVisible"
treeViewHeadersVisible = AttrLabelProxy

treeViewHoverExpand :: AttrLabelProxy "hoverExpand"
treeViewHoverExpand = AttrLabelProxy

treeViewHoverSelection :: AttrLabelProxy "hoverSelection"
treeViewHoverSelection = AttrLabelProxy

treeViewLevelIndentation :: AttrLabelProxy "levelIndentation"
treeViewLevelIndentation = AttrLabelProxy

treeViewModel :: AttrLabelProxy "model"
treeViewModel = AttrLabelProxy

treeViewReorderable :: AttrLabelProxy "reorderable"
treeViewReorderable = AttrLabelProxy

treeViewRubberBanding :: AttrLabelProxy "rubberBanding"
treeViewRubberBanding = AttrLabelProxy

treeViewRulesHint :: AttrLabelProxy "rulesHint"
treeViewRulesHint = AttrLabelProxy

treeViewSearchColumn :: AttrLabelProxy "searchColumn"
treeViewSearchColumn = AttrLabelProxy

treeViewShowExpanders :: AttrLabelProxy "showExpanders"
treeViewShowExpanders = AttrLabelProxy

treeViewTooltipColumn :: AttrLabelProxy "tooltipColumn"
treeViewTooltipColumn = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList TreeView = TreeViewSignalList
type TreeViewSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("columnsChanged", TreeViewColumnsChangedSignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("cursorChanged", TreeViewCursorChangedSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("expandCollapseCursorRow", TreeViewExpandCollapseCursorRowSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveCursor", TreeViewMoveCursorSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("rowActivated", TreeViewRowActivatedSignalInfo), '("rowCollapsed", TreeViewRowCollapsedSignalInfo), '("rowExpanded", TreeViewRowExpandedSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectAll", TreeViewSelectAllSignalInfo), '("selectCursorParent", TreeViewSelectCursorParentSignalInfo), '("selectCursorRow", TreeViewSelectCursorRowSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("startInteractiveSearch", TreeViewStartInteractiveSearchSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("testCollapseRow", TreeViewTestCollapseRowSignalInfo), '("testExpandRow", TreeViewTestExpandRowSignalInfo), '("toggleCursorRow", TreeViewToggleCursorRowSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("unselectAll", TreeViewUnselectAllSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method TreeView::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "TreeView" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_new" gtk_tree_view_new :: 
    IO (Ptr TreeView)

-- | Creates a new t'GI.Gtk.Objects.TreeView.TreeView' widget.
treeViewNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m TreeView
    -- ^ __Returns:__ A newly created t'GI.Gtk.Objects.TreeView.TreeView' widget.
treeViewNew  = liftIO $ do
    result <- gtk_tree_view_new
    checkUnexpectedReturnNULL "treeViewNew" result
    result' <- (newObject TreeView) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method TreeView::new_with_model
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the model." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "TreeView" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_new_with_model" gtk_tree_view_new_with_model :: 
    Ptr Gtk.TreeModel.TreeModel ->          -- model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    IO (Ptr TreeView)

-- | Creates a new t'GI.Gtk.Objects.TreeView.TreeView' widget with the model initialized to /@model@/.
treeViewNewWithModel ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.TreeModel.IsTreeModel a) =>
    a
    -- ^ /@model@/: the model.
    -> m TreeView
    -- ^ __Returns:__ A newly created t'GI.Gtk.Objects.TreeView.TreeView' widget.
treeViewNewWithModel model = liftIO $ do
    model' <- unsafeManagedPtrCastPtr model
    result <- gtk_tree_view_new_with_model model'
    checkUnexpectedReturnNULL "treeViewNewWithModel" result
    result' <- (newObject TreeView) result
    touchManagedPtr model
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method TreeView::append_column
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The #GtkTreeViewColumn to add."
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

foreign import ccall "gtk_tree_view_append_column" gtk_tree_view_append_column :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Ptr Gtk.TreeViewColumn.TreeViewColumn -> -- column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    IO Int32

-- | Appends /@column@/ to the list of columns. If /@treeView@/ has “fixed_height”
-- mode enabled, then /@column@/ must have its “sizing” property set to be
-- GTK_TREE_VIEW_COLUMN_FIXED.
treeViewAppendColumn ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a, Gtk.TreeViewColumn.IsTreeViewColumn b) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'.
    -> b
    -- ^ /@column@/: The t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn' to add.
    -> m Int32
    -- ^ __Returns:__ The number of columns in /@treeView@/ after appending.
treeViewAppendColumn treeView column = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    column' <- unsafeManagedPtrCastPtr column
    result <- gtk_tree_view_append_column treeView' column'
    touchManagedPtr treeView
    touchManagedPtr column
    return result

#if defined(ENABLE_OVERLOADING)
data TreeViewAppendColumnMethodInfo
instance (signature ~ (b -> m Int32), MonadIO m, IsTreeView a, Gtk.TreeViewColumn.IsTreeViewColumn b) => O.OverloadedMethod TreeViewAppendColumnMethodInfo a signature where
    overloadedMethod = treeViewAppendColumn

instance O.OverloadedMethodInfo TreeViewAppendColumnMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewAppendColumn",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewAppendColumn"
        })


#endif

-- method TreeView::collapse_all
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView." , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_view_collapse_all" gtk_tree_view_collapse_all :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    IO ()

-- | Recursively collapses all visible, expanded nodes in /@treeView@/.
treeViewCollapseAll ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'.
    -> m ()
treeViewCollapseAll treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    gtk_tree_view_collapse_all treeView'
    touchManagedPtr treeView
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewCollapseAllMethodInfo
instance (signature ~ (m ()), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewCollapseAllMethodInfo a signature where
    overloadedMethod = treeViewCollapseAll

instance O.OverloadedMethodInfo TreeViewCollapseAllMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewCollapseAll",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewCollapseAll"
        })


#endif

-- method TreeView::collapse_row
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "path to a row in the @tree_view"
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

foreign import ccall "gtk_tree_view_collapse_row" gtk_tree_view_collapse_row :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Ptr Gtk.TreePath.TreePath ->            -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO CInt

-- | Collapses a row (hides its child rows, if they exist).
treeViewCollapseRow ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> Gtk.TreePath.TreePath
    -- ^ /@path@/: path to a row in the /@treeView@/
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the row was collapsed.
treeViewCollapseRow treeView path = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    path' <- unsafeManagedPtrGetPtr path
    result <- gtk_tree_view_collapse_row treeView' path'
    let result' = (/= 0) result
    touchManagedPtr treeView
    touchManagedPtr path
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewCollapseRowMethodInfo
instance (signature ~ (Gtk.TreePath.TreePath -> m Bool), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewCollapseRowMethodInfo a signature where
    overloadedMethod = treeViewCollapseRow

instance O.OverloadedMethodInfo TreeViewCollapseRowMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewCollapseRow",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewCollapseRow"
        })


#endif

-- method TreeView::columns_autosize
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView." , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_view_columns_autosize" gtk_tree_view_columns_autosize :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    IO ()

-- | Resizes all columns to their optimal width. Only works after the
-- treeview has been realized.
treeViewColumnsAutosize ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'.
    -> m ()
treeViewColumnsAutosize treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    gtk_tree_view_columns_autosize treeView'
    touchManagedPtr treeView
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewColumnsAutosizeMethodInfo
instance (signature ~ (m ()), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewColumnsAutosizeMethodInfo a signature where
    overloadedMethod = treeViewColumnsAutosize

instance O.OverloadedMethodInfo TreeViewColumnsAutosizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewColumnsAutosize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewColumnsAutosize"
        })


#endif

-- method TreeView::convert_bin_window_to_tree_coords
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "bx"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "X coordinate relative to bin_window"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "by"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Y coordinate relative to bin_window"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "tx"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return location for tree X coordinate"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "ty"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return location for tree Y coordinate"
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

foreign import ccall "gtk_tree_view_convert_bin_window_to_tree_coords" gtk_tree_view_convert_bin_window_to_tree_coords :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Int32 ->                                -- bx : TBasicType TInt
    Int32 ->                                -- by : TBasicType TInt
    Ptr Int32 ->                            -- tx : TBasicType TInt
    Ptr Int32 ->                            -- ty : TBasicType TInt
    IO ()

-- | Converts bin_window coordinates to coordinates for the
-- tree (the full scrollable area of the tree).
-- 
-- /Since: 2.12/
treeViewConvertBinWindowToTreeCoords ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> Int32
    -- ^ /@bx@/: X coordinate relative to bin_window
    -> Int32
    -- ^ /@by@/: Y coordinate relative to bin_window
    -> m ((Int32, Int32))
treeViewConvertBinWindowToTreeCoords treeView bx by = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    tx <- allocMem :: IO (Ptr Int32)
    ty <- allocMem :: IO (Ptr Int32)
    gtk_tree_view_convert_bin_window_to_tree_coords treeView' bx by tx ty
    tx' <- peek tx
    ty' <- peek ty
    touchManagedPtr treeView
    freeMem tx
    freeMem ty
    return (tx', ty')

#if defined(ENABLE_OVERLOADING)
data TreeViewConvertBinWindowToTreeCoordsMethodInfo
instance (signature ~ (Int32 -> Int32 -> m ((Int32, Int32))), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewConvertBinWindowToTreeCoordsMethodInfo a signature where
    overloadedMethod = treeViewConvertBinWindowToTreeCoords

instance O.OverloadedMethodInfo TreeViewConvertBinWindowToTreeCoordsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewConvertBinWindowToTreeCoords",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewConvertBinWindowToTreeCoords"
        })


#endif

-- method TreeView::convert_bin_window_to_widget_coords
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "bx"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "bin_window X coordinate"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "by"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "bin_window Y coordinate"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "wx"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return location for widget X coordinate"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "wy"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return location for widget Y coordinate"
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

foreign import ccall "gtk_tree_view_convert_bin_window_to_widget_coords" gtk_tree_view_convert_bin_window_to_widget_coords :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Int32 ->                                -- bx : TBasicType TInt
    Int32 ->                                -- by : TBasicType TInt
    Ptr Int32 ->                            -- wx : TBasicType TInt
    Ptr Int32 ->                            -- wy : TBasicType TInt
    IO ()

-- | Converts bin_window coordinates (see 'GI.Gtk.Objects.TreeView.treeViewGetBinWindow')
-- to widget relative coordinates.
-- 
-- /Since: 2.12/
treeViewConvertBinWindowToWidgetCoords ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> Int32
    -- ^ /@bx@/: bin_window X coordinate
    -> Int32
    -- ^ /@by@/: bin_window Y coordinate
    -> m ((Int32, Int32))
treeViewConvertBinWindowToWidgetCoords treeView bx by = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    wx <- allocMem :: IO (Ptr Int32)
    wy <- allocMem :: IO (Ptr Int32)
    gtk_tree_view_convert_bin_window_to_widget_coords treeView' bx by wx wy
    wx' <- peek wx
    wy' <- peek wy
    touchManagedPtr treeView
    freeMem wx
    freeMem wy
    return (wx', wy')

#if defined(ENABLE_OVERLOADING)
data TreeViewConvertBinWindowToWidgetCoordsMethodInfo
instance (signature ~ (Int32 -> Int32 -> m ((Int32, Int32))), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewConvertBinWindowToWidgetCoordsMethodInfo a signature where
    overloadedMethod = treeViewConvertBinWindowToWidgetCoords

instance O.OverloadedMethodInfo TreeViewConvertBinWindowToWidgetCoordsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewConvertBinWindowToWidgetCoords",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewConvertBinWindowToWidgetCoords"
        })


#endif

-- method TreeView::convert_tree_to_bin_window_coords
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "tx"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "tree X coordinate" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "ty"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "tree Y coordinate" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "bx"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "return location for X coordinate relative to bin_window"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "by"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "return location for Y coordinate relative to bin_window"
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

foreign import ccall "gtk_tree_view_convert_tree_to_bin_window_coords" gtk_tree_view_convert_tree_to_bin_window_coords :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Int32 ->                                -- tx : TBasicType TInt
    Int32 ->                                -- ty : TBasicType TInt
    Ptr Int32 ->                            -- bx : TBasicType TInt
    Ptr Int32 ->                            -- by : TBasicType TInt
    IO ()

-- | Converts tree coordinates (coordinates in full scrollable area of the tree)
-- to bin_window coordinates.
-- 
-- /Since: 2.12/
treeViewConvertTreeToBinWindowCoords ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> Int32
    -- ^ /@tx@/: tree X coordinate
    -> Int32
    -- ^ /@ty@/: tree Y coordinate
    -> m ((Int32, Int32))
treeViewConvertTreeToBinWindowCoords treeView tx ty = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    bx <- allocMem :: IO (Ptr Int32)
    by <- allocMem :: IO (Ptr Int32)
    gtk_tree_view_convert_tree_to_bin_window_coords treeView' tx ty bx by
    bx' <- peek bx
    by' <- peek by
    touchManagedPtr treeView
    freeMem bx
    freeMem by
    return (bx', by')

#if defined(ENABLE_OVERLOADING)
data TreeViewConvertTreeToBinWindowCoordsMethodInfo
instance (signature ~ (Int32 -> Int32 -> m ((Int32, Int32))), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewConvertTreeToBinWindowCoordsMethodInfo a signature where
    overloadedMethod = treeViewConvertTreeToBinWindowCoords

instance O.OverloadedMethodInfo TreeViewConvertTreeToBinWindowCoordsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewConvertTreeToBinWindowCoords",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewConvertTreeToBinWindowCoords"
        })


#endif

-- method TreeView::convert_tree_to_widget_coords
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "tx"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "X coordinate relative to the tree"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "ty"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Y coordinate relative to the tree"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "wx"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return location for widget X coordinate"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "wy"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return location for widget Y coordinate"
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

foreign import ccall "gtk_tree_view_convert_tree_to_widget_coords" gtk_tree_view_convert_tree_to_widget_coords :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Int32 ->                                -- tx : TBasicType TInt
    Int32 ->                                -- ty : TBasicType TInt
    Ptr Int32 ->                            -- wx : TBasicType TInt
    Ptr Int32 ->                            -- wy : TBasicType TInt
    IO ()

-- | Converts tree coordinates (coordinates in full scrollable area of the tree)
-- to widget coordinates.
-- 
-- /Since: 2.12/
treeViewConvertTreeToWidgetCoords ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> Int32
    -- ^ /@tx@/: X coordinate relative to the tree
    -> Int32
    -- ^ /@ty@/: Y coordinate relative to the tree
    -> m ((Int32, Int32))
treeViewConvertTreeToWidgetCoords treeView tx ty = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    wx <- allocMem :: IO (Ptr Int32)
    wy <- allocMem :: IO (Ptr Int32)
    gtk_tree_view_convert_tree_to_widget_coords treeView' tx ty wx wy
    wx' <- peek wx
    wy' <- peek wy
    touchManagedPtr treeView
    freeMem wx
    freeMem wy
    return (wx', wy')

#if defined(ENABLE_OVERLOADING)
data TreeViewConvertTreeToWidgetCoordsMethodInfo
instance (signature ~ (Int32 -> Int32 -> m ((Int32, Int32))), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewConvertTreeToWidgetCoordsMethodInfo a signature where
    overloadedMethod = treeViewConvertTreeToWidgetCoords

instance O.OverloadedMethodInfo TreeViewConvertTreeToWidgetCoordsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewConvertTreeToWidgetCoords",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewConvertTreeToWidgetCoords"
        })


#endif

-- method TreeView::convert_widget_to_bin_window_coords
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "wx"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "X coordinate relative to the widget"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "wy"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Y coordinate relative to the widget"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "bx"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return location for bin_window X coordinate"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "by"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return location for bin_window Y coordinate"
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

foreign import ccall "gtk_tree_view_convert_widget_to_bin_window_coords" gtk_tree_view_convert_widget_to_bin_window_coords :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Int32 ->                                -- wx : TBasicType TInt
    Int32 ->                                -- wy : TBasicType TInt
    Ptr Int32 ->                            -- bx : TBasicType TInt
    Ptr Int32 ->                            -- by : TBasicType TInt
    IO ()

-- | Converts widget coordinates to coordinates for the bin_window
-- (see 'GI.Gtk.Objects.TreeView.treeViewGetBinWindow').
-- 
-- /Since: 2.12/
treeViewConvertWidgetToBinWindowCoords ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> Int32
    -- ^ /@wx@/: X coordinate relative to the widget
    -> Int32
    -- ^ /@wy@/: Y coordinate relative to the widget
    -> m ((Int32, Int32))
treeViewConvertWidgetToBinWindowCoords treeView wx wy = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    bx <- allocMem :: IO (Ptr Int32)
    by <- allocMem :: IO (Ptr Int32)
    gtk_tree_view_convert_widget_to_bin_window_coords treeView' wx wy bx by
    bx' <- peek bx
    by' <- peek by
    touchManagedPtr treeView
    freeMem bx
    freeMem by
    return (bx', by')

#if defined(ENABLE_OVERLOADING)
data TreeViewConvertWidgetToBinWindowCoordsMethodInfo
instance (signature ~ (Int32 -> Int32 -> m ((Int32, Int32))), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewConvertWidgetToBinWindowCoordsMethodInfo a signature where
    overloadedMethod = treeViewConvertWidgetToBinWindowCoords

instance O.OverloadedMethodInfo TreeViewConvertWidgetToBinWindowCoordsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewConvertWidgetToBinWindowCoords",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewConvertWidgetToBinWindowCoords"
        })


#endif

-- method TreeView::convert_widget_to_tree_coords
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "wx"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "X coordinate relative to the widget"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "wy"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Y coordinate relative to the widget"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "tx"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return location for tree X coordinate"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "ty"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return location for tree Y coordinate"
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

foreign import ccall "gtk_tree_view_convert_widget_to_tree_coords" gtk_tree_view_convert_widget_to_tree_coords :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Int32 ->                                -- wx : TBasicType TInt
    Int32 ->                                -- wy : TBasicType TInt
    Ptr Int32 ->                            -- tx : TBasicType TInt
    Ptr Int32 ->                            -- ty : TBasicType TInt
    IO ()

-- | Converts widget coordinates to coordinates for the
-- tree (the full scrollable area of the tree).
-- 
-- /Since: 2.12/
treeViewConvertWidgetToTreeCoords ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> Int32
    -- ^ /@wx@/: X coordinate relative to the widget
    -> Int32
    -- ^ /@wy@/: Y coordinate relative to the widget
    -> m ((Int32, Int32))
treeViewConvertWidgetToTreeCoords treeView wx wy = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    tx <- allocMem :: IO (Ptr Int32)
    ty <- allocMem :: IO (Ptr Int32)
    gtk_tree_view_convert_widget_to_tree_coords treeView' wx wy tx ty
    tx' <- peek tx
    ty' <- peek ty
    touchManagedPtr treeView
    freeMem tx
    freeMem ty
    return (tx', ty')

#if defined(ENABLE_OVERLOADING)
data TreeViewConvertWidgetToTreeCoordsMethodInfo
instance (signature ~ (Int32 -> Int32 -> m ((Int32, Int32))), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewConvertWidgetToTreeCoordsMethodInfo a signature where
    overloadedMethod = treeViewConvertWidgetToTreeCoords

instance O.OverloadedMethodInfo TreeViewConvertWidgetToTreeCoordsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewConvertWidgetToTreeCoords",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewConvertWidgetToTreeCoords"
        })


#endif

-- method TreeView::create_row_drag_icon
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreePath in @tree_view"
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
-- returnType: Just (TInterface Name { namespace = "cairo" , name = "Surface" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_create_row_drag_icon" gtk_tree_view_create_row_drag_icon :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Ptr Gtk.TreePath.TreePath ->            -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO (Ptr Cairo.Surface.Surface)

-- | Creates a t'GI.Cairo.Structs.Surface.Surface' representation of the row at /@path@/.
-- This image is used for a drag icon.
treeViewCreateRowDragIcon ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> Gtk.TreePath.TreePath
    -- ^ /@path@/: a t'GI.Gtk.Structs.TreePath.TreePath' in /@treeView@/
    -> m Cairo.Surface.Surface
    -- ^ __Returns:__ a newly-allocated surface of the drag icon.
treeViewCreateRowDragIcon treeView path = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    path' <- unsafeManagedPtrGetPtr path
    result <- gtk_tree_view_create_row_drag_icon treeView' path'
    checkUnexpectedReturnNULL "treeViewCreateRowDragIcon" result
    result' <- (wrapBoxed Cairo.Surface.Surface) result
    touchManagedPtr treeView
    touchManagedPtr path
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewCreateRowDragIconMethodInfo
instance (signature ~ (Gtk.TreePath.TreePath -> m Cairo.Surface.Surface), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewCreateRowDragIconMethodInfo a signature where
    overloadedMethod = treeViewCreateRowDragIcon

instance O.OverloadedMethodInfo TreeViewCreateRowDragIconMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewCreateRowDragIcon",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewCreateRowDragIcon"
        })


#endif

-- method TreeView::enable_model_drag_dest
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "targets"
--           , argType =
--               TCArray
--                 False
--                 (-1)
--                 2
--                 (TInterface Name { namespace = "Gtk" , name = "TargetEntry" })
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the table of targets that\n          the drag will support"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "n_targets"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the number of items in @targets"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "actions"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "DragAction" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the bitmask of possible actions for a drag from this\n   widget"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: [ Arg
--              { argCName = "n_targets"
--              , argType = TBasicType TInt
--              , direction = DirectionIn
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText = Just "the number of items in @targets"
--                    , sinceVersion = Nothing
--                    }
--              , argScope = ScopeTypeInvalid
--              , argClosure = -1
--              , argDestroy = -1
--              , argCallerAllocates = False
--              , transfer = TransferNothing
--              }
--          ]
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_enable_model_drag_dest" gtk_tree_view_enable_model_drag_dest :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Ptr Gtk.TargetEntry.TargetEntry ->      -- targets : TCArray False (-1) 2 (TInterface (Name {namespace = "Gtk", name = "TargetEntry"}))
    Int32 ->                                -- n_targets : TBasicType TInt
    CUInt ->                                -- actions : TInterface (Name {namespace = "Gdk", name = "DragAction"})
    IO ()

-- | Turns /@treeView@/ into a drop destination for automatic DND. Calling
-- this method sets [TreeView:reorderable]("GI.Gtk.Objects.TreeView#g:attr:reorderable") to 'P.False'.
treeViewEnableModelDragDest ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> [Gtk.TargetEntry.TargetEntry]
    -- ^ /@targets@/: the table of targets that
    --           the drag will support
    -> [Gdk.Flags.DragAction]
    -- ^ /@actions@/: the bitmask of possible actions for a drag from this
    --    widget
    -> m ()
treeViewEnableModelDragDest treeView targets actions = liftIO $ do
    let nTargets = fromIntegral $ P.length targets
    treeView' <- unsafeManagedPtrCastPtr treeView
    targets' <- mapM unsafeManagedPtrGetPtr targets
    targets'' <- packBlockArray 16 targets'
    let actions' = gflagsToWord actions
    gtk_tree_view_enable_model_drag_dest treeView' targets'' nTargets actions'
    touchManagedPtr treeView
    mapM_ touchManagedPtr targets
    freeMem targets''
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewEnableModelDragDestMethodInfo
instance (signature ~ ([Gtk.TargetEntry.TargetEntry] -> [Gdk.Flags.DragAction] -> m ()), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewEnableModelDragDestMethodInfo a signature where
    overloadedMethod = treeViewEnableModelDragDest

instance O.OverloadedMethodInfo TreeViewEnableModelDragDestMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewEnableModelDragDest",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewEnableModelDragDest"
        })


#endif

-- method TreeView::enable_model_drag_source
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "start_button_mask"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "ModifierType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Mask of allowed buttons to start drag"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "targets"
--           , argType =
--               TCArray
--                 False
--                 (-1)
--                 3
--                 (TInterface Name { namespace = "Gtk" , name = "TargetEntry" })
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the table of targets that the drag will support"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "n_targets"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the number of items in @targets"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "actions"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "DragAction" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the bitmask of possible actions for a drag from this\n   widget"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: [ Arg
--              { argCName = "n_targets"
--              , argType = TBasicType TInt
--              , direction = DirectionIn
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText = Just "the number of items in @targets"
--                    , sinceVersion = Nothing
--                    }
--              , argScope = ScopeTypeInvalid
--              , argClosure = -1
--              , argDestroy = -1
--              , argCallerAllocates = False
--              , transfer = TransferNothing
--              }
--          ]
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_enable_model_drag_source" gtk_tree_view_enable_model_drag_source :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    CUInt ->                                -- start_button_mask : TInterface (Name {namespace = "Gdk", name = "ModifierType"})
    Ptr Gtk.TargetEntry.TargetEntry ->      -- targets : TCArray False (-1) 3 (TInterface (Name {namespace = "Gtk", name = "TargetEntry"}))
    Int32 ->                                -- n_targets : TBasicType TInt
    CUInt ->                                -- actions : TInterface (Name {namespace = "Gdk", name = "DragAction"})
    IO ()

-- | Turns /@treeView@/ into a drag source for automatic DND. Calling this
-- method sets [TreeView:reorderable]("GI.Gtk.Objects.TreeView#g:attr:reorderable") to 'P.False'.
treeViewEnableModelDragSource ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> [Gdk.Flags.ModifierType]
    -- ^ /@startButtonMask@/: Mask of allowed buttons to start drag
    -> [Gtk.TargetEntry.TargetEntry]
    -- ^ /@targets@/: the table of targets that the drag will support
    -> [Gdk.Flags.DragAction]
    -- ^ /@actions@/: the bitmask of possible actions for a drag from this
    --    widget
    -> m ()
treeViewEnableModelDragSource treeView startButtonMask targets actions = liftIO $ do
    let nTargets = fromIntegral $ P.length targets
    treeView' <- unsafeManagedPtrCastPtr treeView
    let startButtonMask' = gflagsToWord startButtonMask
    targets' <- mapM unsafeManagedPtrGetPtr targets
    targets'' <- packBlockArray 16 targets'
    let actions' = gflagsToWord actions
    gtk_tree_view_enable_model_drag_source treeView' startButtonMask' targets'' nTargets actions'
    touchManagedPtr treeView
    mapM_ touchManagedPtr targets
    freeMem targets''
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewEnableModelDragSourceMethodInfo
instance (signature ~ ([Gdk.Flags.ModifierType] -> [Gtk.TargetEntry.TargetEntry] -> [Gdk.Flags.DragAction] -> m ()), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewEnableModelDragSourceMethodInfo a signature where
    overloadedMethod = treeViewEnableModelDragSource

instance O.OverloadedMethodInfo TreeViewEnableModelDragSourceMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewEnableModelDragSource",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewEnableModelDragSource"
        })


#endif

-- method TreeView::expand_all
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView." , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_view_expand_all" gtk_tree_view_expand_all :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    IO ()

-- | Recursively expands all nodes in the /@treeView@/.
treeViewExpandAll ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'.
    -> m ()
treeViewExpandAll treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    gtk_tree_view_expand_all treeView'
    touchManagedPtr treeView
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewExpandAllMethodInfo
instance (signature ~ (m ()), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewExpandAllMethodInfo a signature where
    overloadedMethod = treeViewExpandAll

instance O.OverloadedMethodInfo TreeViewExpandAllMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewExpandAll",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewExpandAll"
        })


#endif

-- method TreeView::expand_row
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "path to a row" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "open_all"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "whether to recursively expand, or just expand immediate children"
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

foreign import ccall "gtk_tree_view_expand_row" gtk_tree_view_expand_row :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Ptr Gtk.TreePath.TreePath ->            -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    CInt ->                                 -- open_all : TBasicType TBoolean
    IO CInt

-- | Opens the row so its children are visible.
treeViewExpandRow ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> Gtk.TreePath.TreePath
    -- ^ /@path@/: path to a row
    -> Bool
    -- ^ /@openAll@/: whether to recursively expand, or just expand immediate children
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the row existed and had children
treeViewExpandRow treeView path openAll = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    path' <- unsafeManagedPtrGetPtr path
    let openAll' = (fromIntegral . fromEnum) openAll
    result <- gtk_tree_view_expand_row treeView' path' openAll'
    let result' = (/= 0) result
    touchManagedPtr treeView
    touchManagedPtr path
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewExpandRowMethodInfo
instance (signature ~ (Gtk.TreePath.TreePath -> Bool -> m Bool), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewExpandRowMethodInfo a signature where
    overloadedMethod = treeViewExpandRow

instance O.OverloadedMethodInfo TreeViewExpandRowMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewExpandRow",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewExpandRow"
        })


#endif

-- method TreeView::expand_to_path
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "path to a row." , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_view_expand_to_path" gtk_tree_view_expand_to_path :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Ptr Gtk.TreePath.TreePath ->            -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO ()

-- | Expands the row at /@path@/. This will also expand all parent rows of
-- /@path@/ as necessary.
-- 
-- /Since: 2.2/
treeViewExpandToPath ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'.
    -> Gtk.TreePath.TreePath
    -- ^ /@path@/: path to a row.
    -> m ()
treeViewExpandToPath treeView path = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    path' <- unsafeManagedPtrGetPtr path
    gtk_tree_view_expand_to_path treeView' path'
    touchManagedPtr treeView
    touchManagedPtr path
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewExpandToPathMethodInfo
instance (signature ~ (Gtk.TreePath.TreePath -> m ()), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewExpandToPathMethodInfo a signature where
    overloadedMethod = treeViewExpandToPath

instance O.OverloadedMethodInfo TreeViewExpandToPathMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewExpandToPath",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewExpandToPath"
        })


#endif

-- method TreeView::get_activate_on_single_click
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_view_get_activate_on_single_click" gtk_tree_view_get_activate_on_single_click :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    IO CInt

-- | Gets the setting set by 'GI.Gtk.Objects.TreeView.treeViewSetActivateOnSingleClick'.
-- 
-- /Since: 3.8/
treeViewGetActivateOnSingleClick ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if row-activated will be emitted on a single click
treeViewGetActivateOnSingleClick treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    result <- gtk_tree_view_get_activate_on_single_click treeView'
    let result' = (/= 0) result
    touchManagedPtr treeView
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewGetActivateOnSingleClickMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewGetActivateOnSingleClickMethodInfo a signature where
    overloadedMethod = treeViewGetActivateOnSingleClick

instance O.OverloadedMethodInfo TreeViewGetActivateOnSingleClickMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetActivateOnSingleClick",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetActivateOnSingleClick"
        })


#endif

-- method TreeView::get_background_area
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a #GtkTreePath for the row, or %NULL to get only horizontal coordinates"
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
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a #GtkTreeViewColumn for the column, or %NULL to get only vertical coordiantes"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "rect"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "rectangle to fill with cell background rect"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_get_background_area" gtk_tree_view_get_background_area :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Ptr Gtk.TreePath.TreePath ->            -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    Ptr Gtk.TreeViewColumn.TreeViewColumn -> -- column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    Ptr Gdk.Rectangle.Rectangle ->          -- rect : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    IO ()

-- | Fills the bounding rectangle in bin_window coordinates for the cell at the
-- row specified by /@path@/ and the column specified by /@column@/.  If /@path@/ is
-- 'P.Nothing', or points to a node not found in the tree, the /@y@/ and /@height@/ fields of
-- the rectangle will be filled with 0. If /@column@/ is 'P.Nothing', the /@x@/ and /@width@/
-- fields will be filled with 0.  The returned rectangle is equivalent to the
-- /@backgroundArea@/ passed to 'GI.Gtk.Objects.CellRenderer.cellRendererRender'.  These background
-- areas tile to cover the entire bin window.  Contrast with the /@cellArea@/,
-- returned by 'GI.Gtk.Objects.TreeView.treeViewGetCellArea', which returns only the cell
-- itself, excluding surrounding borders and the tree expander area.
treeViewGetBackgroundArea ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a, Gtk.TreeViewColumn.IsTreeViewColumn b) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> Maybe (Gtk.TreePath.TreePath)
    -- ^ /@path@/: a t'GI.Gtk.Structs.TreePath.TreePath' for the row, or 'P.Nothing' to get only horizontal coordinates
    -> Maybe (b)
    -- ^ /@column@/: a t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn' for the column, or 'P.Nothing' to get only vertical coordiantes
    -> m (Gdk.Rectangle.Rectangle)
treeViewGetBackgroundArea treeView path column = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    maybePath <- case path of
        Nothing -> return nullPtr
        Just jPath -> do
            jPath' <- unsafeManagedPtrGetPtr jPath
            return jPath'
    maybeColumn <- case column of
        Nothing -> return nullPtr
        Just jColumn -> do
            jColumn' <- unsafeManagedPtrCastPtr jColumn
            return jColumn'
    rect <- SP.callocBoxedBytes 16 :: IO (Ptr Gdk.Rectangle.Rectangle)
    gtk_tree_view_get_background_area treeView' maybePath maybeColumn rect
    rect' <- (wrapBoxed Gdk.Rectangle.Rectangle) rect
    touchManagedPtr treeView
    whenJust path touchManagedPtr
    whenJust column touchManagedPtr
    return rect'

#if defined(ENABLE_OVERLOADING)
data TreeViewGetBackgroundAreaMethodInfo
instance (signature ~ (Maybe (Gtk.TreePath.TreePath) -> Maybe (b) -> m (Gdk.Rectangle.Rectangle)), MonadIO m, IsTreeView a, Gtk.TreeViewColumn.IsTreeViewColumn b) => O.OverloadedMethod TreeViewGetBackgroundAreaMethodInfo a signature where
    overloadedMethod = treeViewGetBackgroundArea

instance O.OverloadedMethodInfo TreeViewGetBackgroundAreaMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetBackgroundArea",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetBackgroundArea"
        })


#endif

-- method TreeView::get_bin_window
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gdk" , name = "Window" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_get_bin_window" gtk_tree_view_get_bin_window :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    IO (Ptr Gdk.Window.Window)

-- | Returns the window that /@treeView@/ renders to.
-- This is used primarily to compare to @event->window@
-- to confirm that the event on /@treeView@/ is on the right window.
treeViewGetBinWindow ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'
    -> m (Maybe Gdk.Window.Window)
    -- ^ __Returns:__ A t'GI.Gdk.Objects.Window.Window', or 'P.Nothing' when /@treeView@/
    -- hasn’t been realized yet.
treeViewGetBinWindow treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    result <- gtk_tree_view_get_bin_window treeView'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gdk.Window.Window) result'
        return result''
    touchManagedPtr treeView
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data TreeViewGetBinWindowMethodInfo
instance (signature ~ (m (Maybe Gdk.Window.Window)), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewGetBinWindowMethodInfo a signature where
    overloadedMethod = treeViewGetBinWindow

instance O.OverloadedMethodInfo TreeViewGetBinWindowMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetBinWindow",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetBinWindow"
        })


#endif

-- method TreeView::get_cell_area
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a #GtkTreePath for the row, or %NULL to get only horizontal coordinates"
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
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a #GtkTreeViewColumn for the column, or %NULL to get only vertical coordinates"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "rect"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "rectangle to fill with cell rect"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_get_cell_area" gtk_tree_view_get_cell_area :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Ptr Gtk.TreePath.TreePath ->            -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    Ptr Gtk.TreeViewColumn.TreeViewColumn -> -- column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    Ptr Gdk.Rectangle.Rectangle ->          -- rect : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    IO ()

-- | Fills the bounding rectangle in bin_window coordinates for the cell at the
-- row specified by /@path@/ and the column specified by /@column@/.  If /@path@/ is
-- 'P.Nothing', or points to a path not currently displayed, the /@y@/ and /@height@/ fields
-- of the rectangle will be filled with 0. If /@column@/ is 'P.Nothing', the /@x@/ and /@width@/
-- fields will be filled with 0.  The sum of all cell rects does not cover the
-- entire tree; there are extra pixels in between rows, for example. The
-- returned rectangle is equivalent to the /@cellArea@/ passed to
-- 'GI.Gtk.Objects.CellRenderer.cellRendererRender'.  This function is only valid if /@treeView@/ is
-- realized.
treeViewGetCellArea ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a, Gtk.TreeViewColumn.IsTreeViewColumn b) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> Maybe (Gtk.TreePath.TreePath)
    -- ^ /@path@/: a t'GI.Gtk.Structs.TreePath.TreePath' for the row, or 'P.Nothing' to get only horizontal coordinates
    -> Maybe (b)
    -- ^ /@column@/: a t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn' for the column, or 'P.Nothing' to get only vertical coordinates
    -> m (Gdk.Rectangle.Rectangle)
treeViewGetCellArea treeView path column = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    maybePath <- case path of
        Nothing -> return nullPtr
        Just jPath -> do
            jPath' <- unsafeManagedPtrGetPtr jPath
            return jPath'
    maybeColumn <- case column of
        Nothing -> return nullPtr
        Just jColumn -> do
            jColumn' <- unsafeManagedPtrCastPtr jColumn
            return jColumn'
    rect <- SP.callocBoxedBytes 16 :: IO (Ptr Gdk.Rectangle.Rectangle)
    gtk_tree_view_get_cell_area treeView' maybePath maybeColumn rect
    rect' <- (wrapBoxed Gdk.Rectangle.Rectangle) rect
    touchManagedPtr treeView
    whenJust path touchManagedPtr
    whenJust column touchManagedPtr
    return rect'

#if defined(ENABLE_OVERLOADING)
data TreeViewGetCellAreaMethodInfo
instance (signature ~ (Maybe (Gtk.TreePath.TreePath) -> Maybe (b) -> m (Gdk.Rectangle.Rectangle)), MonadIO m, IsTreeView a, Gtk.TreeViewColumn.IsTreeViewColumn b) => O.OverloadedMethod TreeViewGetCellAreaMethodInfo a signature where
    overloadedMethod = treeViewGetCellArea

instance O.OverloadedMethodInfo TreeViewGetCellAreaMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetCellArea",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetCellArea"
        })


#endif

-- method TreeView::get_column
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "n"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The position of the column, counting from 0."
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

foreign import ccall "gtk_tree_view_get_column" gtk_tree_view_get_column :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Int32 ->                                -- n : TBasicType TInt
    IO (Ptr Gtk.TreeViewColumn.TreeViewColumn)

-- | Gets the t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn' at the given position in the @/tree_view/@.
treeViewGetColumn ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'.
    -> Int32
    -- ^ /@n@/: The position of the column, counting from 0.
    -> m (Maybe Gtk.TreeViewColumn.TreeViewColumn)
    -- ^ __Returns:__ The t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn', or 'P.Nothing' if the
    -- position is outside the range of columns.
treeViewGetColumn treeView n = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    result <- gtk_tree_view_get_column treeView' n
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.TreeViewColumn.TreeViewColumn) result'
        return result''
    touchManagedPtr treeView
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data TreeViewGetColumnMethodInfo
instance (signature ~ (Int32 -> m (Maybe Gtk.TreeViewColumn.TreeViewColumn)), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewGetColumnMethodInfo a signature where
    overloadedMethod = treeViewGetColumn

instance O.OverloadedMethodInfo TreeViewGetColumnMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetColumn",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetColumn"
        })


#endif

-- method TreeView::get_columns
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView" , sinceVersion = Nothing }
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
--                  (TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_get_columns" gtk_tree_view_get_columns :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    IO (Ptr (GList (Ptr Gtk.TreeViewColumn.TreeViewColumn)))

-- | Returns a t'GI.GLib.Structs.List.List' of all the t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn' s currently in /@treeView@/.
-- The returned list must be freed with g_list_free ().
treeViewGetColumns ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'
    -> m [Gtk.TreeViewColumn.TreeViewColumn]
    -- ^ __Returns:__ A list of t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn' s
treeViewGetColumns treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    result <- gtk_tree_view_get_columns treeView'
    result' <- unpackGList result
    result'' <- mapM (newObject Gtk.TreeViewColumn.TreeViewColumn) result'
    g_list_free result
    touchManagedPtr treeView
    return result''

#if defined(ENABLE_OVERLOADING)
data TreeViewGetColumnsMethodInfo
instance (signature ~ (m [Gtk.TreeViewColumn.TreeViewColumn]), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewGetColumnsMethodInfo a signature where
    overloadedMethod = treeViewGetColumns

instance O.OverloadedMethodInfo TreeViewGetColumnsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetColumns",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetColumns"
        })


#endif

-- method TreeView::get_cursor
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionOut
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "A pointer to be\n  filled with the current cursor path, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "focus_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionOut
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "A\n  pointer to be filled with the current focus column, or %NULL"
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

foreign import ccall "gtk_tree_view_get_cursor" gtk_tree_view_get_cursor :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Ptr (Ptr Gtk.TreePath.TreePath) ->      -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    Ptr (Ptr Gtk.TreeViewColumn.TreeViewColumn) -> -- focus_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    IO ()

-- | Fills in /@path@/ and /@focusColumn@/ with the current path and focus column.  If
-- the cursor isn’t currently set, then */@path@/ will be 'P.Nothing'.  If no column
-- currently has focus, then */@focusColumn@/ will be 'P.Nothing'.
-- 
-- The returned t'GI.Gtk.Structs.TreePath.TreePath' must be freed with 'GI.Gtk.Structs.TreePath.treePathFree' when
-- you are done with it.
treeViewGetCursor ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'
    -> m ((Maybe Gtk.TreePath.TreePath, Maybe Gtk.TreeViewColumn.TreeViewColumn))
treeViewGetCursor treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    path <- callocMem :: IO (Ptr (Ptr Gtk.TreePath.TreePath))
    focusColumn <- callocMem :: IO (Ptr (Ptr Gtk.TreeViewColumn.TreeViewColumn))
    gtk_tree_view_get_cursor treeView' path focusColumn
    path' <- peek path
    maybePath' <- convertIfNonNull path' $ \path'' -> do
        path''' <- (wrapBoxed Gtk.TreePath.TreePath) path''
        return path'''
    focusColumn' <- peek focusColumn
    maybeFocusColumn' <- convertIfNonNull focusColumn' $ \focusColumn'' -> do
        focusColumn''' <- (newObject Gtk.TreeViewColumn.TreeViewColumn) focusColumn''
        return focusColumn'''
    touchManagedPtr treeView
    freeMem path
    freeMem focusColumn
    return (maybePath', maybeFocusColumn')

#if defined(ENABLE_OVERLOADING)
data TreeViewGetCursorMethodInfo
instance (signature ~ (m ((Maybe Gtk.TreePath.TreePath, Maybe Gtk.TreeViewColumn.TreeViewColumn))), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewGetCursorMethodInfo a signature where
    overloadedMethod = treeViewGetCursor

instance O.OverloadedMethodInfo TreeViewGetCursorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetCursor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetCursor"
        })


#endif

-- method TreeView::get_dest_row_at_pos
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "drag_x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the position to determine the destination row for"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "drag_y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the position to determine the destination row for"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionOut
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "Return location for the path of\n  the highlighted row, or %NULL."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "pos"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "TreeViewDropPosition" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "Return location for the drop position, or\n  %NULL"
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

foreign import ccall "gtk_tree_view_get_dest_row_at_pos" gtk_tree_view_get_dest_row_at_pos :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Int32 ->                                -- drag_x : TBasicType TInt
    Int32 ->                                -- drag_y : TBasicType TInt
    Ptr (Ptr Gtk.TreePath.TreePath) ->      -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    Ptr CUInt ->                            -- pos : TInterface (Name {namespace = "Gtk", name = "TreeViewDropPosition"})
    IO CInt

-- | Determines the destination row for a given position.  /@dragX@/ and
-- /@dragY@/ are expected to be in widget coordinates.  This function is only
-- meaningful if /@treeView@/ is realized.  Therefore this function will always
-- return 'P.False' if /@treeView@/ is not realized or does not have a model.
treeViewGetDestRowAtPos ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> Int32
    -- ^ /@dragX@/: the position to determine the destination row for
    -> Int32
    -- ^ /@dragY@/: the position to determine the destination row for
    -> m ((Bool, Maybe Gtk.TreePath.TreePath, Gtk.Enums.TreeViewDropPosition))
    -- ^ __Returns:__ whether there is a row at the given position, 'P.True' if this
    -- is indeed the case.
treeViewGetDestRowAtPos treeView dragX dragY = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    path <- callocMem :: IO (Ptr (Ptr Gtk.TreePath.TreePath))
    pos <- allocMem :: IO (Ptr CUInt)
    result <- gtk_tree_view_get_dest_row_at_pos treeView' dragX dragY path pos
    let result' = (/= 0) result
    path' <- peek path
    maybePath' <- convertIfNonNull path' $ \path'' -> do
        path''' <- (wrapBoxed Gtk.TreePath.TreePath) path''
        return path'''
    pos' <- peek pos
    let pos'' = (toEnum . fromIntegral) pos'
    touchManagedPtr treeView
    freeMem path
    freeMem pos
    return (result', maybePath', pos'')

#if defined(ENABLE_OVERLOADING)
data TreeViewGetDestRowAtPosMethodInfo
instance (signature ~ (Int32 -> Int32 -> m ((Bool, Maybe Gtk.TreePath.TreePath, Gtk.Enums.TreeViewDropPosition))), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewGetDestRowAtPosMethodInfo a signature where
    overloadedMethod = treeViewGetDestRowAtPos

instance O.OverloadedMethodInfo TreeViewGetDestRowAtPosMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetDestRowAtPos",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetDestRowAtPos"
        })


#endif

-- method TreeView::get_drag_dest_row
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionOut
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "Return location for the path of the highlighted row, or %NULL."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "pos"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "TreeViewDropPosition" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "Return location for the drop position, or %NULL"
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

foreign import ccall "gtk_tree_view_get_drag_dest_row" gtk_tree_view_get_drag_dest_row :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Ptr (Ptr Gtk.TreePath.TreePath) ->      -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    Ptr CUInt ->                            -- pos : TInterface (Name {namespace = "Gtk", name = "TreeViewDropPosition"})
    IO ()

-- | Gets information about the row that is highlighted for feedback.
treeViewGetDragDestRow ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> m ((Maybe Gtk.TreePath.TreePath, Gtk.Enums.TreeViewDropPosition))
treeViewGetDragDestRow treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    path <- callocMem :: IO (Ptr (Ptr Gtk.TreePath.TreePath))
    pos <- allocMem :: IO (Ptr CUInt)
    gtk_tree_view_get_drag_dest_row treeView' path pos
    path' <- peek path
    maybePath' <- convertIfNonNull path' $ \path'' -> do
        path''' <- (wrapBoxed Gtk.TreePath.TreePath) path''
        return path'''
    pos' <- peek pos
    let pos'' = (toEnum . fromIntegral) pos'
    touchManagedPtr treeView
    freeMem path
    freeMem pos
    return (maybePath', pos'')

#if defined(ENABLE_OVERLOADING)
data TreeViewGetDragDestRowMethodInfo
instance (signature ~ (m ((Maybe Gtk.TreePath.TreePath, Gtk.Enums.TreeViewDropPosition))), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewGetDragDestRowMethodInfo a signature where
    overloadedMethod = treeViewGetDragDestRow

instance O.OverloadedMethodInfo TreeViewGetDragDestRowMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetDragDestRow",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetDragDestRow"
        })


#endif

-- method TreeView::get_enable_search
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView" , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_view_get_enable_search" gtk_tree_view_get_enable_search :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    IO CInt

-- | Returns whether or not the tree allows to start interactive searching
-- by typing in text.
treeViewGetEnableSearch ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'
    -> m Bool
    -- ^ __Returns:__ whether or not to let the user search interactively
treeViewGetEnableSearch treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    result <- gtk_tree_view_get_enable_search treeView'
    let result' = (/= 0) result
    touchManagedPtr treeView
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewGetEnableSearchMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewGetEnableSearchMethodInfo a signature where
    overloadedMethod = treeViewGetEnableSearch

instance O.OverloadedMethodInfo TreeViewGetEnableSearchMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetEnableSearch",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetEnableSearch"
        })


#endif

-- method TreeView::get_enable_tree_lines
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView." , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_view_get_enable_tree_lines" gtk_tree_view_get_enable_tree_lines :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    IO CInt

-- | Returns whether or not tree lines are drawn in /@treeView@/.
-- 
-- /Since: 2.10/
treeViewGetEnableTreeLines ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'.
    -> m Bool
    -- ^ __Returns:__ 'P.True' if tree lines are drawn in /@treeView@/, 'P.False'
    -- otherwise.
treeViewGetEnableTreeLines treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    result <- gtk_tree_view_get_enable_tree_lines treeView'
    let result' = (/= 0) result
    touchManagedPtr treeView
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewGetEnableTreeLinesMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewGetEnableTreeLinesMethodInfo a signature where
    overloadedMethod = treeViewGetEnableTreeLines

instance O.OverloadedMethodInfo TreeViewGetEnableTreeLinesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetEnableTreeLines",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetEnableTreeLines"
        })


#endif

-- method TreeView::get_expander_column
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView" , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_view_get_expander_column" gtk_tree_view_get_expander_column :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    IO (Ptr Gtk.TreeViewColumn.TreeViewColumn)

-- | Returns the column that is the current expander column.
-- This column has the expander arrow drawn next to it.
treeViewGetExpanderColumn ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'
    -> m Gtk.TreeViewColumn.TreeViewColumn
    -- ^ __Returns:__ The expander column.
treeViewGetExpanderColumn treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    result <- gtk_tree_view_get_expander_column treeView'
    checkUnexpectedReturnNULL "treeViewGetExpanderColumn" result
    result' <- (newObject Gtk.TreeViewColumn.TreeViewColumn) result
    touchManagedPtr treeView
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewGetExpanderColumnMethodInfo
instance (signature ~ (m Gtk.TreeViewColumn.TreeViewColumn), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewGetExpanderColumnMethodInfo a signature where
    overloadedMethod = treeViewGetExpanderColumn

instance O.OverloadedMethodInfo TreeViewGetExpanderColumnMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetExpanderColumn",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetExpanderColumn"
        })


#endif

-- method TreeView::get_fixed_height_mode
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_view_get_fixed_height_mode" gtk_tree_view_get_fixed_height_mode :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    IO CInt

-- | Returns whether fixed height mode is turned on for /@treeView@/.
-- 
-- /Since: 2.6/
treeViewGetFixedHeightMode ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@treeView@/ is in fixed height mode
treeViewGetFixedHeightMode treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    result <- gtk_tree_view_get_fixed_height_mode treeView'
    let result' = (/= 0) result
    touchManagedPtr treeView
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewGetFixedHeightModeMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewGetFixedHeightModeMethodInfo a signature where
    overloadedMethod = treeViewGetFixedHeightMode

instance O.OverloadedMethodInfo TreeViewGetFixedHeightModeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetFixedHeightMode",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetFixedHeightMode"
        })


#endif

-- method TreeView::get_grid_lines
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
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
--                  Name { namespace = "Gtk" , name = "TreeViewGridLines" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_get_grid_lines" gtk_tree_view_get_grid_lines :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    IO CUInt

-- | Returns which grid lines are enabled in /@treeView@/.
-- 
-- /Since: 2.10/
treeViewGetGridLines ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> m Gtk.Enums.TreeViewGridLines
    -- ^ __Returns:__ a t'GI.Gtk.Enums.TreeViewGridLines' value indicating which grid lines
    -- are enabled.
treeViewGetGridLines treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    result <- gtk_tree_view_get_grid_lines treeView'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr treeView
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewGetGridLinesMethodInfo
instance (signature ~ (m Gtk.Enums.TreeViewGridLines), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewGetGridLinesMethodInfo a signature where
    overloadedMethod = treeViewGetGridLines

instance O.OverloadedMethodInfo TreeViewGetGridLinesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetGridLines",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetGridLines"
        })


#endif

-- method TreeView::get_hadjustment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Adjustment" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_get_hadjustment" gtk_tree_view_get_hadjustment :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    IO (Ptr Gtk.Adjustment.Adjustment)

{-# DEPRECATED treeViewGetHadjustment ["(Since version 3.0)","Use 'GI.Gtk.Interfaces.Scrollable.scrollableGetHadjustment'"] #-}
-- | Gets the t'GI.Gtk.Objects.Adjustment.Adjustment' currently being used for the horizontal aspect.
treeViewGetHadjustment ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'
    -> m Gtk.Adjustment.Adjustment
    -- ^ __Returns:__ A t'GI.Gtk.Objects.Adjustment.Adjustment' object, or 'P.Nothing'
    --     if none is currently being used.
treeViewGetHadjustment treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    result <- gtk_tree_view_get_hadjustment treeView'
    checkUnexpectedReturnNULL "treeViewGetHadjustment" result
    result' <- (newObject Gtk.Adjustment.Adjustment) result
    touchManagedPtr treeView
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewGetHadjustmentMethodInfo
instance (signature ~ (m Gtk.Adjustment.Adjustment), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewGetHadjustmentMethodInfo a signature where
    overloadedMethod = treeViewGetHadjustment

instance O.OverloadedMethodInfo TreeViewGetHadjustmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetHadjustment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetHadjustment"
        })


#endif

-- method TreeView::get_headers_clickable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView." , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_view_get_headers_clickable" gtk_tree_view_get_headers_clickable :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    IO CInt

-- | Returns whether all header columns are clickable.
-- 
-- /Since: 2.10/
treeViewGetHeadersClickable ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'.
    -> m Bool
    -- ^ __Returns:__ 'P.True' if all header columns are clickable, otherwise 'P.False'
treeViewGetHeadersClickable treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    result <- gtk_tree_view_get_headers_clickable treeView'
    let result' = (/= 0) result
    touchManagedPtr treeView
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewGetHeadersClickableMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewGetHeadersClickableMethodInfo a signature where
    overloadedMethod = treeViewGetHeadersClickable

instance O.OverloadedMethodInfo TreeViewGetHeadersClickableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetHeadersClickable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetHeadersClickable"
        })


#endif

-- method TreeView::get_headers_visible
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView." , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_view_get_headers_visible" gtk_tree_view_get_headers_visible :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    IO CInt

-- | Returns 'P.True' if the headers on the /@treeView@/ are visible.
treeViewGetHeadersVisible ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'.
    -> m Bool
    -- ^ __Returns:__ Whether the headers are visible or not.
treeViewGetHeadersVisible treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    result <- gtk_tree_view_get_headers_visible treeView'
    let result' = (/= 0) result
    touchManagedPtr treeView
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewGetHeadersVisibleMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewGetHeadersVisibleMethodInfo a signature where
    overloadedMethod = treeViewGetHeadersVisible

instance O.OverloadedMethodInfo TreeViewGetHeadersVisibleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetHeadersVisible",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetHeadersVisible"
        })


#endif

-- method TreeView::get_hover_expand
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_view_get_hover_expand" gtk_tree_view_get_hover_expand :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    IO CInt

-- | Returns whether hover expansion mode is turned on for /@treeView@/.
-- 
-- /Since: 2.6/
treeViewGetHoverExpand ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@treeView@/ is in hover expansion mode
treeViewGetHoverExpand treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    result <- gtk_tree_view_get_hover_expand treeView'
    let result' = (/= 0) result
    touchManagedPtr treeView
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewGetHoverExpandMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewGetHoverExpandMethodInfo a signature where
    overloadedMethod = treeViewGetHoverExpand

instance O.OverloadedMethodInfo TreeViewGetHoverExpandMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetHoverExpand",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetHoverExpand"
        })


#endif

-- method TreeView::get_hover_selection
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_view_get_hover_selection" gtk_tree_view_get_hover_selection :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    IO CInt

-- | Returns whether hover selection mode is turned on for /@treeView@/.
-- 
-- /Since: 2.6/
treeViewGetHoverSelection ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@treeView@/ is in hover selection mode
treeViewGetHoverSelection treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    result <- gtk_tree_view_get_hover_selection treeView'
    let result' = (/= 0) result
    touchManagedPtr treeView
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewGetHoverSelectionMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewGetHoverSelectionMethodInfo a signature where
    overloadedMethod = treeViewGetHoverSelection

instance O.OverloadedMethodInfo TreeViewGetHoverSelectionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetHoverSelection",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetHoverSelection"
        })


#endif

-- method TreeView::get_level_indentation
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView." , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_view_get_level_indentation" gtk_tree_view_get_level_indentation :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    IO Int32

-- | Returns the amount, in pixels, of extra indentation for child levels
-- in /@treeView@/.
-- 
-- /Since: 2.12/
treeViewGetLevelIndentation ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'.
    -> m Int32
    -- ^ __Returns:__ the amount of extra indentation for child levels in
    -- /@treeView@/.  A return value of 0 means that this feature is disabled.
treeViewGetLevelIndentation treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    result <- gtk_tree_view_get_level_indentation treeView'
    touchManagedPtr treeView
    return result

#if defined(ENABLE_OVERLOADING)
data TreeViewGetLevelIndentationMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewGetLevelIndentationMethodInfo a signature where
    overloadedMethod = treeViewGetLevelIndentation

instance O.OverloadedMethodInfo TreeViewGetLevelIndentationMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetLevelIndentation",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetLevelIndentation"
        })


#endif

-- method TreeView::get_model
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "TreeModel" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_get_model" gtk_tree_view_get_model :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    IO (Ptr Gtk.TreeModel.TreeModel)

-- | Returns the model the t'GI.Gtk.Objects.TreeView.TreeView' is based on.  Returns 'P.Nothing' if the
-- model is unset.
treeViewGetModel ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> m (Maybe Gtk.TreeModel.TreeModel)
    -- ^ __Returns:__ A t'GI.Gtk.Interfaces.TreeModel.TreeModel', or 'P.Nothing' if
    -- none is currently being used.
treeViewGetModel treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    result <- gtk_tree_view_get_model treeView'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.TreeModel.TreeModel) result'
        return result''
    touchManagedPtr treeView
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data TreeViewGetModelMethodInfo
instance (signature ~ (m (Maybe Gtk.TreeModel.TreeModel)), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewGetModelMethodInfo a signature where
    overloadedMethod = treeViewGetModel

instance O.OverloadedMethodInfo TreeViewGetModelMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetModel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetModel"
        })


#endif

-- method TreeView::get_n_columns
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TUInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_get_n_columns" gtk_tree_view_get_n_columns :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    IO Word32

-- | Queries the number of columns in the given /@treeView@/.
-- 
-- /Since: 3.4/
treeViewGetNColumns ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> m Word32
    -- ^ __Returns:__ The number of columns in the /@treeView@/
treeViewGetNColumns treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    result <- gtk_tree_view_get_n_columns treeView'
    touchManagedPtr treeView
    return result

#if defined(ENABLE_OVERLOADING)
data TreeViewGetNColumnsMethodInfo
instance (signature ~ (m Word32), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewGetNColumnsMethodInfo a signature where
    overloadedMethod = treeViewGetNColumns

instance O.OverloadedMethodInfo TreeViewGetNColumnsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetNColumns",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetNColumns"
        })


#endif

-- method TreeView::get_path_at_pos
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The x position to be identified (relative to bin_window)."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The y position to be identified (relative to bin_window)."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionOut
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "A pointer to a #GtkTreePath\n  pointer to be filled in, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionOut
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "A pointer to\n  a #GtkTreeViewColumn pointer to be filled in, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cell_x"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "A pointer where the X coordinate\n  relative to the cell can be placed, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "cell_y"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "A pointer where the Y coordinate\n  relative to the cell can be placed, or %NULL"
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

foreign import ccall "gtk_tree_view_get_path_at_pos" gtk_tree_view_get_path_at_pos :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Int32 ->                                -- x : TBasicType TInt
    Int32 ->                                -- y : TBasicType TInt
    Ptr (Ptr Gtk.TreePath.TreePath) ->      -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    Ptr (Ptr Gtk.TreeViewColumn.TreeViewColumn) -> -- column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    Ptr Int32 ->                            -- cell_x : TBasicType TInt
    Ptr Int32 ->                            -- cell_y : TBasicType TInt
    IO CInt

-- | Finds the path at the point (/@x@/, /@y@/), relative to bin_window coordinates
-- (please see 'GI.Gtk.Objects.TreeView.treeViewGetBinWindow').
-- That is, /@x@/ and /@y@/ are relative to an events coordinates. /@x@/ and /@y@/ must
-- come from an event on the /@treeView@/ only where @event->window ==
-- gtk_tree_view_get_bin_window ()@. It is primarily for
-- things like popup menus. If /@path@/ is non-'P.Nothing', then it will be filled
-- with the t'GI.Gtk.Structs.TreePath.TreePath' at that point.  This path should be freed with
-- 'GI.Gtk.Structs.TreePath.treePathFree'.  If /@column@/ is non-'P.Nothing', then it will be filled
-- with the column at that point.  /@cellX@/ and /@cellY@/ return the coordinates
-- relative to the cell background (i.e. the /@backgroundArea@/ passed to
-- 'GI.Gtk.Objects.CellRenderer.cellRendererRender').  This function is only meaningful if
-- /@treeView@/ is realized.  Therefore this function will always return 'P.False'
-- if /@treeView@/ is not realized or does not have a model.
-- 
-- For converting widget coordinates (eg. the ones you get from
-- GtkWidget[queryTooltip](#g:signal:queryTooltip)), please see
-- 'GI.Gtk.Objects.TreeView.treeViewConvertWidgetToBinWindowCoords'.
treeViewGetPathAtPos ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'.
    -> Int32
    -- ^ /@x@/: The x position to be identified (relative to bin_window).
    -> Int32
    -- ^ /@y@/: The y position to be identified (relative to bin_window).
    -> m ((Bool, Maybe Gtk.TreePath.TreePath, Maybe Gtk.TreeViewColumn.TreeViewColumn, Int32, Int32))
    -- ^ __Returns:__ 'P.True' if a row exists at that coordinate.
treeViewGetPathAtPos treeView x y = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    path <- callocMem :: IO (Ptr (Ptr Gtk.TreePath.TreePath))
    column <- callocMem :: IO (Ptr (Ptr Gtk.TreeViewColumn.TreeViewColumn))
    cellX <- allocMem :: IO (Ptr Int32)
    cellY <- allocMem :: IO (Ptr Int32)
    result <- gtk_tree_view_get_path_at_pos treeView' x y path column cellX cellY
    let result' = (/= 0) result
    path' <- peek path
    maybePath' <- convertIfNonNull path' $ \path'' -> do
        path''' <- (wrapBoxed Gtk.TreePath.TreePath) path''
        return path'''
    column' <- peek column
    maybeColumn' <- convertIfNonNull column' $ \column'' -> do
        column''' <- (newObject Gtk.TreeViewColumn.TreeViewColumn) column''
        return column'''
    cellX' <- peek cellX
    cellY' <- peek cellY
    touchManagedPtr treeView
    freeMem path
    freeMem column
    freeMem cellX
    freeMem cellY
    return (result', maybePath', maybeColumn', cellX', cellY')

#if defined(ENABLE_OVERLOADING)
data TreeViewGetPathAtPosMethodInfo
instance (signature ~ (Int32 -> Int32 -> m ((Bool, Maybe Gtk.TreePath.TreePath, Maybe Gtk.TreeViewColumn.TreeViewColumn, Int32, Int32))), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewGetPathAtPosMethodInfo a signature where
    overloadedMethod = treeViewGetPathAtPos

instance O.OverloadedMethodInfo TreeViewGetPathAtPosMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetPathAtPos",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetPathAtPos"
        })


#endif

-- method TreeView::get_reorderable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_view_get_reorderable" gtk_tree_view_get_reorderable :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    IO CInt

-- | Retrieves whether the user can reorder the tree via drag-and-drop. See
-- 'GI.Gtk.Objects.TreeView.treeViewSetReorderable'.
treeViewGetReorderable ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the tree can be reordered.
treeViewGetReorderable treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    result <- gtk_tree_view_get_reorderable treeView'
    let result' = (/= 0) result
    touchManagedPtr treeView
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewGetReorderableMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewGetReorderableMethodInfo a signature where
    overloadedMethod = treeViewGetReorderable

instance O.OverloadedMethodInfo TreeViewGetReorderableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetReorderable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetReorderable"
        })


#endif

-- method TreeView::get_rubber_banding
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_view_get_rubber_banding" gtk_tree_view_get_rubber_banding :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    IO CInt

-- | Returns whether rubber banding is turned on for /@treeView@/.  If the
-- selection mode is @/GTK_SELECTION_MULTIPLE/@, rubber banding will allow the
-- user to select multiple rows by dragging the mouse.
-- 
-- /Since: 2.10/
treeViewGetRubberBanding ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if rubber banding in /@treeView@/ is enabled.
treeViewGetRubberBanding treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    result <- gtk_tree_view_get_rubber_banding treeView'
    let result' = (/= 0) result
    touchManagedPtr treeView
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewGetRubberBandingMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewGetRubberBandingMethodInfo a signature where
    overloadedMethod = treeViewGetRubberBanding

instance O.OverloadedMethodInfo TreeViewGetRubberBandingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetRubberBanding",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetRubberBanding"
        })


#endif

-- method TreeView::get_rules_hint
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_view_get_rules_hint" gtk_tree_view_get_rules_hint :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    IO CInt

{-# DEPRECATED treeViewGetRulesHint ["(Since version 3.14)"] #-}
-- | Gets the setting set by 'GI.Gtk.Objects.TreeView.treeViewSetRulesHint'.
treeViewGetRulesHint ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the hint is set
treeViewGetRulesHint treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    result <- gtk_tree_view_get_rules_hint treeView'
    let result' = (/= 0) result
    touchManagedPtr treeView
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewGetRulesHintMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewGetRulesHintMethodInfo a signature where
    overloadedMethod = treeViewGetRulesHint

instance O.OverloadedMethodInfo TreeViewGetRulesHintMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetRulesHint",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetRulesHint"
        })


#endif

-- method TreeView::get_search_column
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView" , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_view_get_search_column" gtk_tree_view_get_search_column :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    IO Int32

-- | Gets the column searched on by the interactive search code.
treeViewGetSearchColumn ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'
    -> m Int32
    -- ^ __Returns:__ the column the interactive search code searches in.
treeViewGetSearchColumn treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    result <- gtk_tree_view_get_search_column treeView'
    touchManagedPtr treeView
    return result

#if defined(ENABLE_OVERLOADING)
data TreeViewGetSearchColumnMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewGetSearchColumnMethodInfo a signature where
    overloadedMethod = treeViewGetSearchColumn

instance O.OverloadedMethodInfo TreeViewGetSearchColumnMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetSearchColumn",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetSearchColumn"
        })


#endif

-- method TreeView::get_search_entry
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Entry" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_get_search_entry" gtk_tree_view_get_search_entry :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    IO (Ptr Gtk.Entry.Entry)

-- | Returns the t'GI.Gtk.Objects.Entry.Entry' which is currently in use as interactive search
-- entry for /@treeView@/.  In case the built-in entry is being used, 'P.Nothing'
-- will be returned.
-- 
-- /Since: 2.10/
treeViewGetSearchEntry ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'
    -> m Gtk.Entry.Entry
    -- ^ __Returns:__ the entry currently in use as search entry.
treeViewGetSearchEntry treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    result <- gtk_tree_view_get_search_entry treeView'
    checkUnexpectedReturnNULL "treeViewGetSearchEntry" result
    result' <- (newObject Gtk.Entry.Entry) result
    touchManagedPtr treeView
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewGetSearchEntryMethodInfo
instance (signature ~ (m Gtk.Entry.Entry), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewGetSearchEntryMethodInfo a signature where
    overloadedMethod = treeViewGetSearchEntry

instance O.OverloadedMethodInfo TreeViewGetSearchEntryMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetSearchEntry",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetSearchEntry"
        })


#endif

-- method TreeView::get_selection
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "TreeSelection" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_get_selection" gtk_tree_view_get_selection :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    IO (Ptr Gtk.TreeSelection.TreeSelection)

-- | Gets the t'GI.Gtk.Objects.TreeSelection.TreeSelection' associated with /@treeView@/.
treeViewGetSelection ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'.
    -> m Gtk.TreeSelection.TreeSelection
    -- ^ __Returns:__ A t'GI.Gtk.Objects.TreeSelection.TreeSelection' object.
treeViewGetSelection treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    result <- gtk_tree_view_get_selection treeView'
    checkUnexpectedReturnNULL "treeViewGetSelection" result
    result' <- (newObject Gtk.TreeSelection.TreeSelection) result
    touchManagedPtr treeView
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewGetSelectionMethodInfo
instance (signature ~ (m Gtk.TreeSelection.TreeSelection), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewGetSelectionMethodInfo a signature where
    overloadedMethod = treeViewGetSelection

instance O.OverloadedMethodInfo TreeViewGetSelectionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetSelection",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetSelection"
        })


#endif

-- method TreeView::get_show_expanders
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView." , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_view_get_show_expanders" gtk_tree_view_get_show_expanders :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    IO CInt

-- | Returns whether or not expanders are drawn in /@treeView@/.
-- 
-- /Since: 2.12/
treeViewGetShowExpanders ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'.
    -> m Bool
    -- ^ __Returns:__ 'P.True' if expanders are drawn in /@treeView@/, 'P.False'
    -- otherwise.
treeViewGetShowExpanders treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    result <- gtk_tree_view_get_show_expanders treeView'
    let result' = (/= 0) result
    touchManagedPtr treeView
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewGetShowExpandersMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewGetShowExpandersMethodInfo a signature where
    overloadedMethod = treeViewGetShowExpanders

instance O.OverloadedMethodInfo TreeViewGetShowExpandersMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetShowExpanders",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetShowExpanders"
        })


#endif

-- method TreeView::get_tooltip_column
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_view_get_tooltip_column" gtk_tree_view_get_tooltip_column :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    IO Int32

-- | Returns the column of /@treeView@/’s model which is being used for
-- displaying tooltips on /@treeView@/’s rows.
-- 
-- /Since: 2.12/
treeViewGetTooltipColumn ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> m Int32
    -- ^ __Returns:__ the index of the tooltip column that is currently being
    -- used, or -1 if this is disabled.
treeViewGetTooltipColumn treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    result <- gtk_tree_view_get_tooltip_column treeView'
    touchManagedPtr treeView
    return result

#if defined(ENABLE_OVERLOADING)
data TreeViewGetTooltipColumnMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewGetTooltipColumnMethodInfo a signature where
    overloadedMethod = treeViewGetTooltipColumn

instance O.OverloadedMethodInfo TreeViewGetTooltipColumnMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetTooltipColumn",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetTooltipColumn"
        })


#endif

-- method TreeView::get_tooltip_context
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TInt
--           , direction = DirectionInout
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the x coordinate (relative to widget coordinates)"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TInt
--           , direction = DirectionInout
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the y coordinate (relative to widget coordinates)"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "keyboard_tip"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "whether this is a keyboard tooltip or not"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionOut
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a pointer to\n        receive a #GtkTreeModel or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a pointer to receive a #GtkTreePath or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeIter" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a pointer to receive a #GtkTreeIter or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_get_tooltip_context" gtk_tree_view_get_tooltip_context :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Ptr Int32 ->                            -- x : TBasicType TInt
    Ptr Int32 ->                            -- y : TBasicType TInt
    CInt ->                                 -- keyboard_tip : TBasicType TBoolean
    Ptr (Ptr Gtk.TreeModel.TreeModel) ->    -- model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    Ptr (Ptr Gtk.TreePath.TreePath) ->      -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO CInt

-- | This function is supposed to be used in a [Widget::queryTooltip]("GI.Gtk.Objects.Widget#g:signal:queryTooltip")
-- signal handler for t'GI.Gtk.Objects.TreeView.TreeView'.  The /@x@/, /@y@/ and /@keyboardTip@/ values
-- which are received in the signal handler, should be passed to this
-- function without modification.
-- 
-- The return value indicates whether there is a tree view row at the given
-- coordinates ('P.True') or not ('P.False') for mouse tooltips.  For keyboard
-- tooltips the row returned will be the cursor row.  When 'P.True', then any of
-- /@model@/, /@path@/ and /@iter@/ which have been provided will be set to point to
-- that row and the corresponding model.  /@x@/ and /@y@/ will always be converted
-- to be relative to /@treeView@/’s bin_window if /@keyboardTooltip@/ is 'P.False'.
-- 
-- /Since: 2.12/
treeViewGetTooltipContext ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> Int32
    -- ^ /@x@/: the x coordinate (relative to widget coordinates)
    -> Int32
    -- ^ /@y@/: the y coordinate (relative to widget coordinates)
    -> Bool
    -- ^ /@keyboardTip@/: whether this is a keyboard tooltip or not
    -> m ((Bool, Int32, Int32, Maybe Gtk.TreeModel.TreeModel, Gtk.TreePath.TreePath, Gtk.TreeIter.TreeIter))
    -- ^ __Returns:__ whether or not the given tooltip context points to a row.
treeViewGetTooltipContext treeView x y keyboardTip = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    x' <- allocMem :: IO (Ptr Int32)
    poke x' x
    y' <- allocMem :: IO (Ptr Int32)
    poke y' y
    let keyboardTip' = (fromIntegral . fromEnum) keyboardTip
    model <- callocMem :: IO (Ptr (Ptr Gtk.TreeModel.TreeModel))
    path <- callocMem :: IO (Ptr (Ptr Gtk.TreePath.TreePath))
    iter <- SP.callocBoxedBytes 32 :: IO (Ptr Gtk.TreeIter.TreeIter)
    result <- gtk_tree_view_get_tooltip_context treeView' x' y' keyboardTip' model path iter
    let result' = (/= 0) result
    x'' <- peek x'
    y'' <- peek y'
    model' <- peek model
    maybeModel' <- convertIfNonNull model' $ \model'' -> do
        model''' <- (newObject Gtk.TreeModel.TreeModel) model''
        return model'''
    path' <- peek path
    path'' <- (wrapBoxed Gtk.TreePath.TreePath) path'
    iter' <- (wrapBoxed Gtk.TreeIter.TreeIter) iter
    touchManagedPtr treeView
    freeMem x'
    freeMem y'
    freeMem model
    freeMem path
    return (result', x'', y'', maybeModel', path'', iter')

#if defined(ENABLE_OVERLOADING)
data TreeViewGetTooltipContextMethodInfo
instance (signature ~ (Int32 -> Int32 -> Bool -> m ((Bool, Int32, Int32, Maybe Gtk.TreeModel.TreeModel, Gtk.TreePath.TreePath, Gtk.TreeIter.TreeIter))), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewGetTooltipContextMethodInfo a signature where
    overloadedMethod = treeViewGetTooltipContext

instance O.OverloadedMethodInfo TreeViewGetTooltipContextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetTooltipContext",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetTooltipContext"
        })


#endif

-- method TreeView::get_vadjustment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Adjustment" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_get_vadjustment" gtk_tree_view_get_vadjustment :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    IO (Ptr Gtk.Adjustment.Adjustment)

{-# DEPRECATED treeViewGetVadjustment ["(Since version 3.0)","Use 'GI.Gtk.Interfaces.Scrollable.scrollableGetVadjustment'"] #-}
-- | Gets the t'GI.Gtk.Objects.Adjustment.Adjustment' currently being used for the vertical aspect.
treeViewGetVadjustment ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'
    -> m Gtk.Adjustment.Adjustment
    -- ^ __Returns:__ A t'GI.Gtk.Objects.Adjustment.Adjustment' object, or 'P.Nothing'
    --     if none is currently being used.
treeViewGetVadjustment treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    result <- gtk_tree_view_get_vadjustment treeView'
    checkUnexpectedReturnNULL "treeViewGetVadjustment" result
    result' <- (newObject Gtk.Adjustment.Adjustment) result
    touchManagedPtr treeView
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewGetVadjustmentMethodInfo
instance (signature ~ (m Gtk.Adjustment.Adjustment), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewGetVadjustmentMethodInfo a signature where
    overloadedMethod = treeViewGetVadjustment

instance O.OverloadedMethodInfo TreeViewGetVadjustmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetVadjustment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetVadjustment"
        })


#endif

-- method TreeView::get_visible_range
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "start_path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "Return location for start of region,\n             or %NULL."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "end_path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Return location for end of region, or %NULL."
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

foreign import ccall "gtk_tree_view_get_visible_range" gtk_tree_view_get_visible_range :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Ptr (Ptr Gtk.TreePath.TreePath) ->      -- start_path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    Ptr (Ptr Gtk.TreePath.TreePath) ->      -- end_path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO CInt

-- | Sets /@startPath@/ and /@endPath@/ to be the first and last visible path.
-- Note that there may be invisible paths in between.
-- 
-- The paths should be freed with 'GI.Gtk.Structs.TreePath.treePathFree' after use.
-- 
-- /Since: 2.8/
treeViewGetVisibleRange ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'
    -> m ((Bool, Gtk.TreePath.TreePath, Gtk.TreePath.TreePath))
    -- ^ __Returns:__ 'P.True', if valid paths were placed in /@startPath@/ and /@endPath@/.
treeViewGetVisibleRange treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    startPath <- callocMem :: IO (Ptr (Ptr Gtk.TreePath.TreePath))
    endPath <- callocMem :: IO (Ptr (Ptr Gtk.TreePath.TreePath))
    result <- gtk_tree_view_get_visible_range treeView' startPath endPath
    let result' = (/= 0) result
    startPath' <- peek startPath
    startPath'' <- (wrapBoxed Gtk.TreePath.TreePath) startPath'
    endPath' <- peek endPath
    endPath'' <- (wrapBoxed Gtk.TreePath.TreePath) endPath'
    touchManagedPtr treeView
    freeMem startPath
    freeMem endPath
    return (result', startPath'', endPath'')

#if defined(ENABLE_OVERLOADING)
data TreeViewGetVisibleRangeMethodInfo
instance (signature ~ (m ((Bool, Gtk.TreePath.TreePath, Gtk.TreePath.TreePath))), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewGetVisibleRangeMethodInfo a signature where
    overloadedMethod = treeViewGetVisibleRange

instance O.OverloadedMethodInfo TreeViewGetVisibleRangeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetVisibleRange",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetVisibleRange"
        })


#endif

-- method TreeView::get_visible_rect
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "visible_rect"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "rectangle to fill" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_get_visible_rect" gtk_tree_view_get_visible_rect :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Ptr Gdk.Rectangle.Rectangle ->          -- visible_rect : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    IO ()

-- | Fills /@visibleRect@/ with the currently-visible region of the
-- buffer, in tree coordinates. Convert to bin_window coordinates with
-- 'GI.Gtk.Objects.TreeView.treeViewConvertTreeToBinWindowCoords'.
-- Tree coordinates start at 0,0 for row 0 of the tree, and cover the entire
-- scrollable area of the tree.
treeViewGetVisibleRect ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> m (Gdk.Rectangle.Rectangle)
treeViewGetVisibleRect treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    visibleRect <- SP.callocBoxedBytes 16 :: IO (Ptr Gdk.Rectangle.Rectangle)
    gtk_tree_view_get_visible_rect treeView' visibleRect
    visibleRect' <- (wrapBoxed Gdk.Rectangle.Rectangle) visibleRect
    touchManagedPtr treeView
    return visibleRect'

#if defined(ENABLE_OVERLOADING)
data TreeViewGetVisibleRectMethodInfo
instance (signature ~ (m (Gdk.Rectangle.Rectangle)), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewGetVisibleRectMethodInfo a signature where
    overloadedMethod = treeViewGetVisibleRect

instance O.OverloadedMethodInfo TreeViewGetVisibleRectMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewGetVisibleRect",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewGetVisibleRect"
        })


#endif

-- method TreeView::insert_column
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The #GtkTreeViewColumn to be inserted."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "position"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The position to insert @column in."
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

foreign import ccall "gtk_tree_view_insert_column" gtk_tree_view_insert_column :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Ptr Gtk.TreeViewColumn.TreeViewColumn -> -- column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    Int32 ->                                -- position : TBasicType TInt
    IO Int32

-- | This inserts the /@column@/ into the /@treeView@/ at /@position@/.  If /@position@/ is
-- -1, then the column is inserted at the end. If /@treeView@/ has
-- “fixed_height” mode enabled, then /@column@/ must have its “sizing” property
-- set to be GTK_TREE_VIEW_COLUMN_FIXED.
treeViewInsertColumn ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a, Gtk.TreeViewColumn.IsTreeViewColumn b) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'.
    -> b
    -- ^ /@column@/: The t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn' to be inserted.
    -> Int32
    -- ^ /@position@/: The position to insert /@column@/ in.
    -> m Int32
    -- ^ __Returns:__ The number of columns in /@treeView@/ after insertion.
treeViewInsertColumn treeView column position = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    column' <- unsafeManagedPtrCastPtr column
    result <- gtk_tree_view_insert_column treeView' column' position
    touchManagedPtr treeView
    touchManagedPtr column
    return result

#if defined(ENABLE_OVERLOADING)
data TreeViewInsertColumnMethodInfo
instance (signature ~ (b -> Int32 -> m Int32), MonadIO m, IsTreeView a, Gtk.TreeViewColumn.IsTreeViewColumn b) => O.OverloadedMethod TreeViewInsertColumnMethodInfo a signature where
    overloadedMethod = treeViewInsertColumn

instance O.OverloadedMethodInfo TreeViewInsertColumnMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewInsertColumn",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewInsertColumn"
        })


#endif

-- method TreeView::insert_column_with_data_func
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "position"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Position to insert, -1 for append"
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
--                 { rawDocText = Just "column title" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "cell renderer for column"
--                 , sinceVersion = Nothing
--                 }
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
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "function to set attributes of cell renderer"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeNotified
--           , argClosure = 5
--           , argDestroy = 6
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "data for @func" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "dnotify"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "DestroyNotify" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "destroy notifier for @data"
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
-- returnType: Just (TBasicType TInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_view_insert_column_with_data_func" gtk_tree_view_insert_column_with_data_func :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Int32 ->                                -- position : TBasicType TInt
    CString ->                              -- title : TBasicType TUTF8
    Ptr Gtk.CellRenderer.CellRenderer ->    -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    FunPtr Gtk.Callbacks.C_TreeCellDataFunc -> -- func : TInterface (Name {namespace = "Gtk", name = "TreeCellDataFunc"})
    Ptr () ->                               -- data : TBasicType TPtr
    FunPtr GLib.Callbacks.C_DestroyNotify -> -- dnotify : TInterface (Name {namespace = "GLib", name = "DestroyNotify"})
    IO Int32

-- | Convenience function that inserts a new column into the t'GI.Gtk.Objects.TreeView.TreeView'
-- with the given cell renderer and a t'GI.Gtk.Callbacks.TreeCellDataFunc' to set cell renderer
-- attributes (normally using data from the model). See also
-- 'GI.Gtk.Objects.TreeViewColumn.treeViewColumnSetCellDataFunc', 'GI.Gtk.Objects.TreeViewColumn.treeViewColumnPackStart'.
-- If /@treeView@/ has “fixed_height” mode enabled, then the new column will have its
-- “sizing” property set to be GTK_TREE_VIEW_COLUMN_FIXED.
treeViewInsertColumnWithDataFunc ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a, Gtk.CellRenderer.IsCellRenderer b) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> Int32
    -- ^ /@position@/: Position to insert, -1 for append
    -> T.Text
    -- ^ /@title@/: column title
    -> b
    -- ^ /@cell@/: cell renderer for column
    -> Gtk.Callbacks.TreeCellDataFunc
    -- ^ /@func@/: function to set attributes of cell renderer
    -> m Int32
    -- ^ __Returns:__ number of columns in the tree view post-insert
treeViewInsertColumnWithDataFunc treeView position title cell func = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    title' <- textToCString title
    cell' <- unsafeManagedPtrCastPtr cell
    func' <- Gtk.Callbacks.mk_TreeCellDataFunc (Gtk.Callbacks.wrap_TreeCellDataFunc Nothing (Gtk.Callbacks.drop_closures_TreeCellDataFunc func))
    let data_ = castFunPtrToPtr func'
    let dnotify = SP.safeFreeFunPtrPtr
    result <- gtk_tree_view_insert_column_with_data_func treeView' position title' cell' func' data_ dnotify
    touchManagedPtr treeView
    touchManagedPtr cell
    freeMem title'
    return result

#if defined(ENABLE_OVERLOADING)
data TreeViewInsertColumnWithDataFuncMethodInfo
instance (signature ~ (Int32 -> T.Text -> b -> Gtk.Callbacks.TreeCellDataFunc -> m Int32), MonadIO m, IsTreeView a, Gtk.CellRenderer.IsCellRenderer b) => O.OverloadedMethod TreeViewInsertColumnWithDataFuncMethodInfo a signature where
    overloadedMethod = treeViewInsertColumnWithDataFunc

instance O.OverloadedMethodInfo TreeViewInsertColumnWithDataFuncMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewInsertColumnWithDataFunc",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewInsertColumnWithDataFunc"
        })


#endif

-- method TreeView::is_blank_at_pos
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The x position to be identified (relative to bin_window)"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The y position to be identified (relative to bin_window)"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionOut
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "A pointer to a #GtkTreePath pointer to\n  be filled in, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionOut
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "A pointer to a\n  #GtkTreeViewColumn pointer to be filled in, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cell_x"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "A pointer where the X coordinate relative to the\n  cell can be placed, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "cell_y"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "A pointer where the Y coordinate relative to the\n  cell can be placed, or %NULL"
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

foreign import ccall "gtk_tree_view_is_blank_at_pos" gtk_tree_view_is_blank_at_pos :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Int32 ->                                -- x : TBasicType TInt
    Int32 ->                                -- y : TBasicType TInt
    Ptr (Ptr Gtk.TreePath.TreePath) ->      -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    Ptr (Ptr Gtk.TreeViewColumn.TreeViewColumn) -> -- column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    Ptr Int32 ->                            -- cell_x : TBasicType TInt
    Ptr Int32 ->                            -- cell_y : TBasicType TInt
    IO CInt

-- | Determine whether the point (/@x@/, /@y@/) in /@treeView@/ is blank, that is no
-- cell content nor an expander arrow is drawn at the location. If so, the
-- location can be considered as the background. You might wish to take
-- special action on clicks on the background, such as clearing a current
-- selection, having a custom context menu or starting rubber banding.
-- 
-- The /@x@/ and /@y@/ coordinate that are provided must be relative to bin_window
-- coordinates.  That is, /@x@/ and /@y@/ must come from an event on /@treeView@/
-- where @event->window == gtk_tree_view_get_bin_window ()@.
-- 
-- For converting widget coordinates (eg. the ones you get from
-- GtkWidget[queryTooltip](#g:signal:queryTooltip)), please see
-- 'GI.Gtk.Objects.TreeView.treeViewConvertWidgetToBinWindowCoords'.
-- 
-- The /@path@/, /@column@/, /@cellX@/ and /@cellY@/ arguments will be filled in
-- likewise as for 'GI.Gtk.Objects.TreeView.treeViewGetPathAtPos'.  Please see
-- 'GI.Gtk.Objects.TreeView.treeViewGetPathAtPos' for more information.
-- 
-- /Since: 3.0/
treeViewIsBlankAtPos ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'
    -> Int32
    -- ^ /@x@/: The x position to be identified (relative to bin_window)
    -> Int32
    -- ^ /@y@/: The y position to be identified (relative to bin_window)
    -> m ((Bool, Maybe Gtk.TreePath.TreePath, Maybe Gtk.TreeViewColumn.TreeViewColumn, Int32, Int32))
    -- ^ __Returns:__ 'P.True' if the area at the given coordinates is blank,
    -- 'P.False' otherwise.
treeViewIsBlankAtPos treeView x y = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    path <- callocMem :: IO (Ptr (Ptr Gtk.TreePath.TreePath))
    column <- callocMem :: IO (Ptr (Ptr Gtk.TreeViewColumn.TreeViewColumn))
    cellX <- allocMem :: IO (Ptr Int32)
    cellY <- allocMem :: IO (Ptr Int32)
    result <- gtk_tree_view_is_blank_at_pos treeView' x y path column cellX cellY
    let result' = (/= 0) result
    path' <- peek path
    maybePath' <- convertIfNonNull path' $ \path'' -> do
        path''' <- (wrapBoxed Gtk.TreePath.TreePath) path''
        return path'''
    column' <- peek column
    maybeColumn' <- convertIfNonNull column' $ \column'' -> do
        column''' <- (newObject Gtk.TreeViewColumn.TreeViewColumn) column''
        return column'''
    cellX' <- peek cellX
    cellY' <- peek cellY
    touchManagedPtr treeView
    freeMem path
    freeMem column
    freeMem cellX
    freeMem cellY
    return (result', maybePath', maybeColumn', cellX', cellY')

#if defined(ENABLE_OVERLOADING)
data TreeViewIsBlankAtPosMethodInfo
instance (signature ~ (Int32 -> Int32 -> m ((Bool, Maybe Gtk.TreePath.TreePath, Maybe Gtk.TreeViewColumn.TreeViewColumn, Int32, Int32))), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewIsBlankAtPosMethodInfo a signature where
    overloadedMethod = treeViewIsBlankAtPos

instance O.OverloadedMethodInfo TreeViewIsBlankAtPosMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewIsBlankAtPos",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewIsBlankAtPos"
        })


#endif

-- method TreeView::is_rubber_banding_active
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_view_is_rubber_banding_active" gtk_tree_view_is_rubber_banding_active :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    IO CInt

-- | Returns whether a rubber banding operation is currently being done
-- in /@treeView@/.
-- 
-- /Since: 2.12/
treeViewIsRubberBandingActive ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if a rubber banding operation is currently being
    -- done in /@treeView@/.
treeViewIsRubberBandingActive treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    result <- gtk_tree_view_is_rubber_banding_active treeView'
    let result' = (/= 0) result
    touchManagedPtr treeView
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewIsRubberBandingActiveMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewIsRubberBandingActiveMethodInfo a signature where
    overloadedMethod = treeViewIsRubberBandingActive

instance O.OverloadedMethodInfo TreeViewIsRubberBandingActiveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewIsRubberBandingActive",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewIsRubberBandingActive"
        })


#endif

-- method TreeView::map_expanded_rows
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "func"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "TreeViewMappingFunc" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A function to be called"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeCall
--           , argClosure = 2
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "User data to be passed to the function."
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

foreign import ccall "gtk_tree_view_map_expanded_rows" gtk_tree_view_map_expanded_rows :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    FunPtr Gtk.Callbacks.C_TreeViewMappingFunc -> -- func : TInterface (Name {namespace = "Gtk", name = "TreeViewMappingFunc"})
    Ptr () ->                               -- data : TBasicType TPtr
    IO ()

-- | Calls /@func@/ on all expanded rows.
treeViewMapExpandedRows ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'
    -> Gtk.Callbacks.TreeViewMappingFunc
    -- ^ /@func@/: A function to be called
    -> m ()
treeViewMapExpandedRows treeView func = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    func' <- Gtk.Callbacks.mk_TreeViewMappingFunc (Gtk.Callbacks.wrap_TreeViewMappingFunc Nothing (Gtk.Callbacks.drop_closures_TreeViewMappingFunc func))
    let data_ = nullPtr
    gtk_tree_view_map_expanded_rows treeView' func' data_
    safeFreeFunPtr $ castFunPtrToPtr func'
    touchManagedPtr treeView
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewMapExpandedRowsMethodInfo
instance (signature ~ (Gtk.Callbacks.TreeViewMappingFunc -> m ()), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewMapExpandedRowsMethodInfo a signature where
    overloadedMethod = treeViewMapExpandedRows

instance O.OverloadedMethodInfo TreeViewMapExpandedRowsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewMapExpandedRows",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewMapExpandedRows"
        })


#endif

-- method TreeView::move_column_after
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The #GtkTreeViewColumn to be moved."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "base_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The #GtkTreeViewColumn to be moved relative to, or %NULL."
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

foreign import ccall "gtk_tree_view_move_column_after" gtk_tree_view_move_column_after :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Ptr Gtk.TreeViewColumn.TreeViewColumn -> -- column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    Ptr Gtk.TreeViewColumn.TreeViewColumn -> -- base_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    IO ()

-- | Moves /@column@/ to be after to /@baseColumn@/.  If /@baseColumn@/ is 'P.Nothing', then
-- /@column@/ is placed in the first position.
treeViewMoveColumnAfter ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a, Gtk.TreeViewColumn.IsTreeViewColumn b, Gtk.TreeViewColumn.IsTreeViewColumn c) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'
    -> b
    -- ^ /@column@/: The t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn' to be moved.
    -> Maybe (c)
    -- ^ /@baseColumn@/: The t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn' to be moved relative to, or 'P.Nothing'.
    -> m ()
treeViewMoveColumnAfter treeView column baseColumn = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    column' <- unsafeManagedPtrCastPtr column
    maybeBaseColumn <- case baseColumn of
        Nothing -> return nullPtr
        Just jBaseColumn -> do
            jBaseColumn' <- unsafeManagedPtrCastPtr jBaseColumn
            return jBaseColumn'
    gtk_tree_view_move_column_after treeView' column' maybeBaseColumn
    touchManagedPtr treeView
    touchManagedPtr column
    whenJust baseColumn touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewMoveColumnAfterMethodInfo
instance (signature ~ (b -> Maybe (c) -> m ()), MonadIO m, IsTreeView a, Gtk.TreeViewColumn.IsTreeViewColumn b, Gtk.TreeViewColumn.IsTreeViewColumn c) => O.OverloadedMethod TreeViewMoveColumnAfterMethodInfo a signature where
    overloadedMethod = treeViewMoveColumnAfter

instance O.OverloadedMethodInfo TreeViewMoveColumnAfterMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewMoveColumnAfter",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewMoveColumnAfter"
        })


#endif

-- method TreeView::remove_column
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The #GtkTreeViewColumn to remove."
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

foreign import ccall "gtk_tree_view_remove_column" gtk_tree_view_remove_column :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Ptr Gtk.TreeViewColumn.TreeViewColumn -> -- column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    IO Int32

-- | Removes /@column@/ from /@treeView@/.
treeViewRemoveColumn ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a, Gtk.TreeViewColumn.IsTreeViewColumn b) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'.
    -> b
    -- ^ /@column@/: The t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn' to remove.
    -> m Int32
    -- ^ __Returns:__ The number of columns in /@treeView@/ after removing.
treeViewRemoveColumn treeView column = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    column' <- unsafeManagedPtrCastPtr column
    result <- gtk_tree_view_remove_column treeView' column'
    touchManagedPtr treeView
    touchManagedPtr column
    return result

#if defined(ENABLE_OVERLOADING)
data TreeViewRemoveColumnMethodInfo
instance (signature ~ (b -> m Int32), MonadIO m, IsTreeView a, Gtk.TreeViewColumn.IsTreeViewColumn b) => O.OverloadedMethod TreeViewRemoveColumnMethodInfo a signature where
    overloadedMethod = treeViewRemoveColumn

instance O.OverloadedMethodInfo TreeViewRemoveColumnMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewRemoveColumn",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewRemoveColumn"
        })


#endif

-- method TreeView::row_activated
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The #GtkTreePath to be activated."
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
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The #GtkTreeViewColumn to be activated."
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

foreign import ccall "gtk_tree_view_row_activated" gtk_tree_view_row_activated :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Ptr Gtk.TreePath.TreePath ->            -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    Ptr Gtk.TreeViewColumn.TreeViewColumn -> -- column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    IO ()

-- | Activates the cell determined by /@path@/ and /@column@/.
treeViewRowActivated ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a, Gtk.TreeViewColumn.IsTreeViewColumn b) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'
    -> Gtk.TreePath.TreePath
    -- ^ /@path@/: The t'GI.Gtk.Structs.TreePath.TreePath' to be activated.
    -> b
    -- ^ /@column@/: The t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn' to be activated.
    -> m ()
treeViewRowActivated treeView path column = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    path' <- unsafeManagedPtrGetPtr path
    column' <- unsafeManagedPtrCastPtr column
    gtk_tree_view_row_activated treeView' path' column'
    touchManagedPtr treeView
    touchManagedPtr path
    touchManagedPtr column
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewRowActivatedMethodInfo
instance (signature ~ (Gtk.TreePath.TreePath -> b -> m ()), MonadIO m, IsTreeView a, Gtk.TreeViewColumn.IsTreeViewColumn b) => O.OverloadedMethod TreeViewRowActivatedMethodInfo a signature where
    overloadedMethod = treeViewRowActivated

instance O.OverloadedMethodInfo TreeViewRowActivatedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewRowActivated",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewRowActivated"
        })


#endif

-- method TreeView::row_expanded
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreePath to test expansion state."
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

foreign import ccall "gtk_tree_view_row_expanded" gtk_tree_view_row_expanded :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Ptr Gtk.TreePath.TreePath ->            -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO CInt

-- | Returns 'P.True' if the node pointed to by /@path@/ is expanded in /@treeView@/.
treeViewRowExpanded ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'.
    -> Gtk.TreePath.TreePath
    -- ^ /@path@/: A t'GI.Gtk.Structs.TreePath.TreePath' to test expansion state.
    -> m Bool
    -- ^ __Returns:__ 'P.True' if @/path/@ is expanded.
treeViewRowExpanded treeView path = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    path' <- unsafeManagedPtrGetPtr path
    result <- gtk_tree_view_row_expanded treeView' path'
    let result' = (/= 0) result
    touchManagedPtr treeView
    touchManagedPtr path
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeViewRowExpandedMethodInfo
instance (signature ~ (Gtk.TreePath.TreePath -> m Bool), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewRowExpandedMethodInfo a signature where
    overloadedMethod = treeViewRowExpanded

instance O.OverloadedMethodInfo TreeViewRowExpandedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewRowExpanded",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewRowExpanded"
        })


#endif

-- method TreeView::scroll_to_cell
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The path of the row to move to, or %NULL."
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
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The #GtkTreeViewColumn to move horizontally to, or %NULL."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "use_align"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "whether to use alignment arguments, or %FALSE."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "row_align"
--           , argType = TBasicType TFloat
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The vertical alignment of the row specified by @path."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "col_align"
--           , argType = TBasicType TFloat
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The horizontal alignment of the column specified by @column."
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

foreign import ccall "gtk_tree_view_scroll_to_cell" gtk_tree_view_scroll_to_cell :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Ptr Gtk.TreePath.TreePath ->            -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    Ptr Gtk.TreeViewColumn.TreeViewColumn -> -- column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    CInt ->                                 -- use_align : TBasicType TBoolean
    CFloat ->                               -- row_align : TBasicType TFloat
    CFloat ->                               -- col_align : TBasicType TFloat
    IO ()

-- | Moves the alignments of /@treeView@/ to the position specified by /@column@/ and
-- /@path@/.  If /@column@/ is 'P.Nothing', then no horizontal scrolling occurs.  Likewise,
-- if /@path@/ is 'P.Nothing' no vertical scrolling occurs.  At a minimum, one of /@column@/
-- or /@path@/ need to be non-'P.Nothing'.  /@rowAlign@/ determines where the row is
-- placed, and /@colAlign@/ determines where /@column@/ is placed.  Both are expected
-- to be between 0.0 and 1.0. 0.0 means left\/top alignment, 1.0 means
-- right\/bottom alignment, 0.5 means center.
-- 
-- If /@useAlign@/ is 'P.False', then the alignment arguments are ignored, and the
-- tree does the minimum amount of work to scroll the cell onto the screen.
-- This means that the cell will be scrolled to the edge closest to its current
-- position.  If the cell is currently visible on the screen, nothing is done.
-- 
-- This function only works if the model is set, and /@path@/ is a valid row on the
-- model.  If the model changes before the /@treeView@/ is realized, the centered
-- path will be modified to reflect this change.
treeViewScrollToCell ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a, Gtk.TreeViewColumn.IsTreeViewColumn b) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'.
    -> Maybe (Gtk.TreePath.TreePath)
    -- ^ /@path@/: The path of the row to move to, or 'P.Nothing'.
    -> Maybe (b)
    -- ^ /@column@/: The t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn' to move horizontally to, or 'P.Nothing'.
    -> Bool
    -- ^ /@useAlign@/: whether to use alignment arguments, or 'P.False'.
    -> Float
    -- ^ /@rowAlign@/: The vertical alignment of the row specified by /@path@/.
    -> Float
    -- ^ /@colAlign@/: The horizontal alignment of the column specified by /@column@/.
    -> m ()
treeViewScrollToCell treeView path column useAlign rowAlign colAlign = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    maybePath <- case path of
        Nothing -> return nullPtr
        Just jPath -> do
            jPath' <- unsafeManagedPtrGetPtr jPath
            return jPath'
    maybeColumn <- case column of
        Nothing -> return nullPtr
        Just jColumn -> do
            jColumn' <- unsafeManagedPtrCastPtr jColumn
            return jColumn'
    let useAlign' = (fromIntegral . fromEnum) useAlign
    let rowAlign' = realToFrac rowAlign
    let colAlign' = realToFrac colAlign
    gtk_tree_view_scroll_to_cell treeView' maybePath maybeColumn useAlign' rowAlign' colAlign'
    touchManagedPtr treeView
    whenJust path touchManagedPtr
    whenJust column touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewScrollToCellMethodInfo
instance (signature ~ (Maybe (Gtk.TreePath.TreePath) -> Maybe (b) -> Bool -> Float -> Float -> m ()), MonadIO m, IsTreeView a, Gtk.TreeViewColumn.IsTreeViewColumn b) => O.OverloadedMethod TreeViewScrollToCellMethodInfo a signature where
    overloadedMethod = treeViewScrollToCell

instance O.OverloadedMethodInfo TreeViewScrollToCellMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewScrollToCell",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewScrollToCell"
        })


#endif

-- method TreeView::scroll_to_point
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "tree_x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "X coordinate of new top-left pixel of visible area, or -1"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "tree_y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "Y coordinate of new top-left pixel of visible area, or -1"
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

foreign import ccall "gtk_tree_view_scroll_to_point" gtk_tree_view_scroll_to_point :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Int32 ->                                -- tree_x : TBasicType TInt
    Int32 ->                                -- tree_y : TBasicType TInt
    IO ()

-- | Scrolls the tree view such that the top-left corner of the visible
-- area is /@treeX@/, /@treeY@/, where /@treeX@/ and /@treeY@/ are specified
-- in tree coordinates.  The /@treeView@/ must be realized before
-- this function is called.  If it isn\'t, you probably want to be
-- using 'GI.Gtk.Objects.TreeView.treeViewScrollToCell'.
-- 
-- If either /@treeX@/ or /@treeY@/ are -1, then that direction isn’t scrolled.
treeViewScrollToPoint ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> Int32
    -- ^ /@treeX@/: X coordinate of new top-left pixel of visible area, or -1
    -> Int32
    -- ^ /@treeY@/: Y coordinate of new top-left pixel of visible area, or -1
    -> m ()
treeViewScrollToPoint treeView treeX treeY = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    gtk_tree_view_scroll_to_point treeView' treeX treeY
    touchManagedPtr treeView
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewScrollToPointMethodInfo
instance (signature ~ (Int32 -> Int32 -> m ()), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewScrollToPointMethodInfo a signature where
    overloadedMethod = treeViewScrollToPoint

instance O.OverloadedMethodInfo TreeViewScrollToPointMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewScrollToPoint",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewScrollToPoint"
        })


#endif

-- method TreeView::set_activate_on_single_click
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "single"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE to emit row-activated on a single click"
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

foreign import ccall "gtk_tree_view_set_activate_on_single_click" gtk_tree_view_set_activate_on_single_click :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    CInt ->                                 -- single : TBasicType TBoolean
    IO ()

-- | Cause the [TreeView::rowActivated]("GI.Gtk.Objects.TreeView#g:signal:rowActivated") signal to be emitted
-- on a single click instead of a double click.
-- 
-- /Since: 3.8/
treeViewSetActivateOnSingleClick ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> Bool
    -- ^ /@single@/: 'P.True' to emit row-activated on a single click
    -> m ()
treeViewSetActivateOnSingleClick treeView single = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    let single' = (fromIntegral . fromEnum) single
    gtk_tree_view_set_activate_on_single_click treeView' single'
    touchManagedPtr treeView
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewSetActivateOnSingleClickMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewSetActivateOnSingleClickMethodInfo a signature where
    overloadedMethod = treeViewSetActivateOnSingleClick

instance O.OverloadedMethodInfo TreeViewSetActivateOnSingleClickMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewSetActivateOnSingleClick",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewSetActivateOnSingleClick"
        })


#endif

-- method TreeView::set_column_drag_function
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "func"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "TreeViewColumnDropFunc" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "A function to determine which columns are reorderable, or %NULL."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeNotified
--           , argClosure = 2
--           , argDestroy = 3
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "user_data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "User data to be passed to @func, or %NULL"
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Destroy notifier for @user_data, or %NULL"
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

foreign import ccall "gtk_tree_view_set_column_drag_function" gtk_tree_view_set_column_drag_function :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    FunPtr Gtk.Callbacks.C_TreeViewColumnDropFunc -> -- func : TInterface (Name {namespace = "Gtk", name = "TreeViewColumnDropFunc"})
    Ptr () ->                               -- user_data : TBasicType TPtr
    FunPtr GLib.Callbacks.C_DestroyNotify -> -- destroy : TInterface (Name {namespace = "GLib", name = "DestroyNotify"})
    IO ()

-- | Sets a user function for determining where a column may be dropped when
-- dragged.  This function is called on every column pair in turn at the
-- beginning of a column drag to determine where a drop can take place.  The
-- arguments passed to /@func@/ are: the /@treeView@/, the t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn' being
-- dragged, the two t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn' s determining the drop spot, and
-- /@userData@/.  If either of the t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn' arguments for the drop spot
-- are 'P.Nothing', then they indicate an edge.  If /@func@/ is set to be 'P.Nothing', then
-- /@treeView@/ reverts to the default behavior of allowing all columns to be
-- dropped everywhere.
treeViewSetColumnDragFunction ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'.
    -> Maybe (Gtk.Callbacks.TreeViewColumnDropFunc)
    -- ^ /@func@/: A function to determine which columns are reorderable, or 'P.Nothing'.
    -> m ()
treeViewSetColumnDragFunction treeView func = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    maybeFunc <- case func of
        Nothing -> return (castPtrToFunPtr nullPtr)
        Just jFunc -> do
            jFunc' <- Gtk.Callbacks.mk_TreeViewColumnDropFunc (Gtk.Callbacks.wrap_TreeViewColumnDropFunc Nothing (Gtk.Callbacks.drop_closures_TreeViewColumnDropFunc jFunc))
            return jFunc'
    let userData = castFunPtrToPtr maybeFunc
    let destroy = SP.safeFreeFunPtrPtr
    gtk_tree_view_set_column_drag_function treeView' maybeFunc userData destroy
    touchManagedPtr treeView
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewSetColumnDragFunctionMethodInfo
instance (signature ~ (Maybe (Gtk.Callbacks.TreeViewColumnDropFunc) -> m ()), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewSetColumnDragFunctionMethodInfo a signature where
    overloadedMethod = treeViewSetColumnDragFunction

instance O.OverloadedMethodInfo TreeViewSetColumnDragFunctionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewSetColumnDragFunction",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewSetColumnDragFunction"
        })


#endif

-- method TreeView::set_cursor
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreePath" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "focus_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "start_editing"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "%TRUE if the specified cell should start being edited."
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

foreign import ccall "gtk_tree_view_set_cursor" gtk_tree_view_set_cursor :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Ptr Gtk.TreePath.TreePath ->            -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    Ptr Gtk.TreeViewColumn.TreeViewColumn -> -- focus_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    CInt ->                                 -- start_editing : TBasicType TBoolean
    IO ()

-- | Sets the current keyboard focus to be at /@path@/, and selects it.  This is
-- useful when you want to focus the user’s attention on a particular row.  If
-- /@focusColumn@/ is not 'P.Nothing', then focus is given to the column specified by
-- it. Additionally, if /@focusColumn@/ is specified, and /@startEditing@/ is
-- 'P.True', then editing should be started in the specified cell.
-- This function is often followed by /@gtkWidgetGrabFocus@/ (/@treeView@/)
-- in order to give keyboard focus to the widget.  Please note that editing
-- can only happen when the widget is realized.
-- 
-- If /@path@/ is invalid for /@model@/, the current cursor (if any) will be unset
-- and the function will return without failing.
treeViewSetCursor ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a, Gtk.TreeViewColumn.IsTreeViewColumn b) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'
    -> Gtk.TreePath.TreePath
    -- ^ /@path@/: A t'GI.Gtk.Structs.TreePath.TreePath'
    -> Maybe (b)
    -- ^ /@focusColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn', or 'P.Nothing'
    -> Bool
    -- ^ /@startEditing@/: 'P.True' if the specified cell should start being edited.
    -> m ()
treeViewSetCursor treeView path focusColumn startEditing = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    path' <- unsafeManagedPtrGetPtr path
    maybeFocusColumn <- case focusColumn of
        Nothing -> return nullPtr
        Just jFocusColumn -> do
            jFocusColumn' <- unsafeManagedPtrCastPtr jFocusColumn
            return jFocusColumn'
    let startEditing' = (fromIntegral . fromEnum) startEditing
    gtk_tree_view_set_cursor treeView' path' maybeFocusColumn startEditing'
    touchManagedPtr treeView
    touchManagedPtr path
    whenJust focusColumn touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewSetCursorMethodInfo
instance (signature ~ (Gtk.TreePath.TreePath -> Maybe (b) -> Bool -> m ()), MonadIO m, IsTreeView a, Gtk.TreeViewColumn.IsTreeViewColumn b) => O.OverloadedMethod TreeViewSetCursorMethodInfo a signature where
    overloadedMethod = treeViewSetCursor

instance O.OverloadedMethodInfo TreeViewSetCursorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewSetCursor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewSetCursor"
        })


#endif

-- method TreeView::set_cursor_on_cell
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreePath" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "focus_column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeViewColumn, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "focus_cell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkCellRenderer, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "start_editing"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "%TRUE if the specified cell should start being edited."
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

foreign import ccall "gtk_tree_view_set_cursor_on_cell" gtk_tree_view_set_cursor_on_cell :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Ptr Gtk.TreePath.TreePath ->            -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    Ptr Gtk.TreeViewColumn.TreeViewColumn -> -- focus_column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- focus_cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    CInt ->                                 -- start_editing : TBasicType TBoolean
    IO ()

-- | Sets the current keyboard focus to be at /@path@/, and selects it.  This is
-- useful when you want to focus the user’s attention on a particular row.  If
-- /@focusColumn@/ is not 'P.Nothing', then focus is given to the column specified by
-- it. If /@focusColumn@/ and /@focusCell@/ are not 'P.Nothing', and /@focusColumn@/
-- contains 2 or more editable or activatable cells, then focus is given to
-- the cell specified by /@focusCell@/. Additionally, if /@focusColumn@/ is
-- specified, and /@startEditing@/ is 'P.True', then editing should be started in
-- the specified cell.  This function is often followed by
-- /@gtkWidgetGrabFocus@/ (/@treeView@/) in order to give keyboard focus to the
-- widget.  Please note that editing can only happen when the widget is
-- realized.
-- 
-- If /@path@/ is invalid for /@model@/, the current cursor (if any) will be unset
-- and the function will return without failing.
-- 
-- /Since: 2.2/
treeViewSetCursorOnCell ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a, Gtk.TreeViewColumn.IsTreeViewColumn b, Gtk.CellRenderer.IsCellRenderer c) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'
    -> Gtk.TreePath.TreePath
    -- ^ /@path@/: A t'GI.Gtk.Structs.TreePath.TreePath'
    -> Maybe (b)
    -- ^ /@focusColumn@/: A t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn', or 'P.Nothing'
    -> Maybe (c)
    -- ^ /@focusCell@/: A t'GI.Gtk.Objects.CellRenderer.CellRenderer', or 'P.Nothing'
    -> Bool
    -- ^ /@startEditing@/: 'P.True' if the specified cell should start being edited.
    -> m ()
treeViewSetCursorOnCell treeView path focusColumn focusCell startEditing = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    path' <- unsafeManagedPtrGetPtr path
    maybeFocusColumn <- case focusColumn of
        Nothing -> return nullPtr
        Just jFocusColumn -> do
            jFocusColumn' <- unsafeManagedPtrCastPtr jFocusColumn
            return jFocusColumn'
    maybeFocusCell <- case focusCell of
        Nothing -> return nullPtr
        Just jFocusCell -> do
            jFocusCell' <- unsafeManagedPtrCastPtr jFocusCell
            return jFocusCell'
    let startEditing' = (fromIntegral . fromEnum) startEditing
    gtk_tree_view_set_cursor_on_cell treeView' path' maybeFocusColumn maybeFocusCell startEditing'
    touchManagedPtr treeView
    touchManagedPtr path
    whenJust focusColumn touchManagedPtr
    whenJust focusCell touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewSetCursorOnCellMethodInfo
instance (signature ~ (Gtk.TreePath.TreePath -> Maybe (b) -> Maybe (c) -> Bool -> m ()), MonadIO m, IsTreeView a, Gtk.TreeViewColumn.IsTreeViewColumn b, Gtk.CellRenderer.IsCellRenderer c) => O.OverloadedMethod TreeViewSetCursorOnCellMethodInfo a signature where
    overloadedMethod = treeViewSetCursorOnCell

instance O.OverloadedMethodInfo TreeViewSetCursorOnCellMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewSetCursorOnCell",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewSetCursorOnCell"
        })


#endif

-- method TreeView::set_destroy_count_func
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "func"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "TreeDestroyCountFunc" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "Function to be called when a view row is destroyed, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeNotified
--           , argClosure = 2
--           , argDestroy = 3
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "User data to be passed to @func, or %NULL"
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Destroy notifier for @data, or %NULL"
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

foreign import ccall "gtk_tree_view_set_destroy_count_func" gtk_tree_view_set_destroy_count_func :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    FunPtr Gtk.Callbacks.C_TreeDestroyCountFunc -> -- func : TInterface (Name {namespace = "Gtk", name = "TreeDestroyCountFunc"})
    Ptr () ->                               -- data : TBasicType TPtr
    FunPtr GLib.Callbacks.C_DestroyNotify -> -- destroy : TInterface (Name {namespace = "GLib", name = "DestroyNotify"})
    IO ()

{-# DEPRECATED treeViewSetDestroyCountFunc ["(Since version 3.4)","Accessibility does not need the function anymore."] #-}
-- | This function should almost never be used.  It is meant for private use by
-- ATK for determining the number of visible children that are removed when the
-- user collapses a row, or a row is deleted.
treeViewSetDestroyCountFunc ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'
    -> Maybe (Gtk.Callbacks.TreeDestroyCountFunc)
    -- ^ /@func@/: Function to be called when a view row is destroyed, or 'P.Nothing'
    -> m ()
treeViewSetDestroyCountFunc treeView func = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    maybeFunc <- case func of
        Nothing -> return (castPtrToFunPtr nullPtr)
        Just jFunc -> do
            jFunc' <- Gtk.Callbacks.mk_TreeDestroyCountFunc (Gtk.Callbacks.wrap_TreeDestroyCountFunc Nothing (Gtk.Callbacks.drop_closures_TreeDestroyCountFunc jFunc))
            return jFunc'
    let data_ = castFunPtrToPtr maybeFunc
    let destroy = SP.safeFreeFunPtrPtr
    gtk_tree_view_set_destroy_count_func treeView' maybeFunc data_ destroy
    touchManagedPtr treeView
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewSetDestroyCountFuncMethodInfo
instance (signature ~ (Maybe (Gtk.Callbacks.TreeDestroyCountFunc) -> m ()), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewSetDestroyCountFuncMethodInfo a signature where
    overloadedMethod = treeViewSetDestroyCountFunc

instance O.OverloadedMethodInfo TreeViewSetDestroyCountFuncMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewSetDestroyCountFunc",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewSetDestroyCountFunc"
        })


#endif

-- method TreeView::set_drag_dest_row
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The path of the row to highlight, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pos"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "TreeViewDropPosition" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "Specifies whether to drop before, after or into the row"
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

foreign import ccall "gtk_tree_view_set_drag_dest_row" gtk_tree_view_set_drag_dest_row :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Ptr Gtk.TreePath.TreePath ->            -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    CUInt ->                                -- pos : TInterface (Name {namespace = "Gtk", name = "TreeViewDropPosition"})
    IO ()

-- | Sets the row that is highlighted for feedback.
-- If /@path@/ is 'P.Nothing', an existing highlight is removed.
treeViewSetDragDestRow ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> Maybe (Gtk.TreePath.TreePath)
    -- ^ /@path@/: The path of the row to highlight, or 'P.Nothing'
    -> Gtk.Enums.TreeViewDropPosition
    -- ^ /@pos@/: Specifies whether to drop before, after or into the row
    -> m ()
treeViewSetDragDestRow treeView path pos = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    maybePath <- case path of
        Nothing -> return nullPtr
        Just jPath -> do
            jPath' <- unsafeManagedPtrGetPtr jPath
            return jPath'
    let pos' = (fromIntegral . fromEnum) pos
    gtk_tree_view_set_drag_dest_row treeView' maybePath pos'
    touchManagedPtr treeView
    whenJust path touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewSetDragDestRowMethodInfo
instance (signature ~ (Maybe (Gtk.TreePath.TreePath) -> Gtk.Enums.TreeViewDropPosition -> m ()), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewSetDragDestRowMethodInfo a signature where
    overloadedMethod = treeViewSetDragDestRow

instance O.OverloadedMethodInfo TreeViewSetDragDestRowMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewSetDragDestRow",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewSetDragDestRow"
        })


#endif

-- method TreeView::set_enable_search
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "enable_search"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE, if the user can search interactively"
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

foreign import ccall "gtk_tree_view_set_enable_search" gtk_tree_view_set_enable_search :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    CInt ->                                 -- enable_search : TBasicType TBoolean
    IO ()

-- | If /@enableSearch@/ is set, then the user can type in text to search through
-- the tree interactively (this is sometimes called \"typeahead find\").
-- 
-- Note that even if this is 'P.False', the user can still initiate a search
-- using the “start-interactive-search” key binding.
treeViewSetEnableSearch ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'
    -> Bool
    -- ^ /@enableSearch@/: 'P.True', if the user can search interactively
    -> m ()
treeViewSetEnableSearch treeView enableSearch = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    let enableSearch' = (fromIntegral . fromEnum) enableSearch
    gtk_tree_view_set_enable_search treeView' enableSearch'
    touchManagedPtr treeView
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewSetEnableSearchMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewSetEnableSearchMethodInfo a signature where
    overloadedMethod = treeViewSetEnableSearch

instance O.OverloadedMethodInfo TreeViewSetEnableSearchMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewSetEnableSearch",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewSetEnableSearch"
        })


#endif

-- method TreeView::set_enable_tree_lines
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "enabled"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "%TRUE to enable tree line drawing, %FALSE otherwise."
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

foreign import ccall "gtk_tree_view_set_enable_tree_lines" gtk_tree_view_set_enable_tree_lines :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    CInt ->                                 -- enabled : TBasicType TBoolean
    IO ()

-- | Sets whether to draw lines interconnecting the expanders in /@treeView@/.
-- This does not have any visible effects for lists.
-- 
-- /Since: 2.10/
treeViewSetEnableTreeLines ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> Bool
    -- ^ /@enabled@/: 'P.True' to enable tree line drawing, 'P.False' otherwise.
    -> m ()
treeViewSetEnableTreeLines treeView enabled = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    let enabled' = (fromIntegral . fromEnum) enabled
    gtk_tree_view_set_enable_tree_lines treeView' enabled'
    touchManagedPtr treeView
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewSetEnableTreeLinesMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewSetEnableTreeLinesMethodInfo a signature where
    overloadedMethod = treeViewSetEnableTreeLines

instance O.OverloadedMethodInfo TreeViewSetEnableTreeLinesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewSetEnableTreeLines",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewSetEnableTreeLines"
        })


#endif

-- method TreeView::set_expander_column
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "column"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "%NULL, or the column to draw the expander arrow at."
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

foreign import ccall "gtk_tree_view_set_expander_column" gtk_tree_view_set_expander_column :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Ptr Gtk.TreeViewColumn.TreeViewColumn -> -- column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    IO ()

-- | Sets the column to draw the expander arrow at. It must be in /@treeView@/.
-- If /@column@/ is 'P.Nothing', then the expander arrow is always at the first
-- visible column.
-- 
-- If you do not want expander arrow to appear in your tree, set the
-- expander column to a hidden column.
treeViewSetExpanderColumn ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a, Gtk.TreeViewColumn.IsTreeViewColumn b) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'
    -> Maybe (b)
    -- ^ /@column@/: 'P.Nothing', or the column to draw the expander arrow at.
    -> m ()
treeViewSetExpanderColumn treeView column = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    maybeColumn <- case column of
        Nothing -> return nullPtr
        Just jColumn -> do
            jColumn' <- unsafeManagedPtrCastPtr jColumn
            return jColumn'
    gtk_tree_view_set_expander_column treeView' maybeColumn
    touchManagedPtr treeView
    whenJust column touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewSetExpanderColumnMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsTreeView a, Gtk.TreeViewColumn.IsTreeViewColumn b) => O.OverloadedMethod TreeViewSetExpanderColumnMethodInfo a signature where
    overloadedMethod = treeViewSetExpanderColumn

instance O.OverloadedMethodInfo TreeViewSetExpanderColumnMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewSetExpanderColumn",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewSetExpanderColumn"
        })


#endif

-- method TreeView::set_fixed_height_mode
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "enable"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE to enable fixed height mode"
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

foreign import ccall "gtk_tree_view_set_fixed_height_mode" gtk_tree_view_set_fixed_height_mode :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    CInt ->                                 -- enable : TBasicType TBoolean
    IO ()

-- | Enables or disables the fixed height mode of /@treeView@/.
-- Fixed height mode speeds up t'GI.Gtk.Objects.TreeView.TreeView' by assuming that all
-- rows have the same height.
-- Only enable this option if all rows are the same height and all
-- columns are of type 'GI.Gtk.Enums.TreeViewColumnSizingFixed'.
-- 
-- /Since: 2.6/
treeViewSetFixedHeightMode ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> Bool
    -- ^ /@enable@/: 'P.True' to enable fixed height mode
    -> m ()
treeViewSetFixedHeightMode treeView enable = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    let enable' = (fromIntegral . fromEnum) enable
    gtk_tree_view_set_fixed_height_mode treeView' enable'
    touchManagedPtr treeView
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewSetFixedHeightModeMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewSetFixedHeightModeMethodInfo a signature where
    overloadedMethod = treeViewSetFixedHeightMode

instance O.OverloadedMethodInfo TreeViewSetFixedHeightModeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewSetFixedHeightMode",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewSetFixedHeightMode"
        })


#endif

-- method TreeView::set_grid_lines
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "grid_lines"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewGridLines" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a #GtkTreeViewGridLines value indicating which grid lines to\nenable."
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

foreign import ccall "gtk_tree_view_set_grid_lines" gtk_tree_view_set_grid_lines :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    CUInt ->                                -- grid_lines : TInterface (Name {namespace = "Gtk", name = "TreeViewGridLines"})
    IO ()

-- | Sets which grid lines to draw in /@treeView@/.
-- 
-- /Since: 2.10/
treeViewSetGridLines ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> Gtk.Enums.TreeViewGridLines
    -- ^ /@gridLines@/: a t'GI.Gtk.Enums.TreeViewGridLines' value indicating which grid lines to
    -- enable.
    -> m ()
treeViewSetGridLines treeView gridLines = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    let gridLines' = (fromIntegral . fromEnum) gridLines
    gtk_tree_view_set_grid_lines treeView' gridLines'
    touchManagedPtr treeView
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewSetGridLinesMethodInfo
instance (signature ~ (Gtk.Enums.TreeViewGridLines -> m ()), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewSetGridLinesMethodInfo a signature where
    overloadedMethod = treeViewSetGridLines

instance O.OverloadedMethodInfo TreeViewSetGridLinesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewSetGridLines",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewSetGridLines"
        })


#endif

-- method TreeView::set_hadjustment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "adjustment"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Adjustment" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The #GtkAdjustment to set, or %NULL"
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

foreign import ccall "gtk_tree_view_set_hadjustment" gtk_tree_view_set_hadjustment :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Ptr Gtk.Adjustment.Adjustment ->        -- adjustment : TInterface (Name {namespace = "Gtk", name = "Adjustment"})
    IO ()

{-# DEPRECATED treeViewSetHadjustment ["(Since version 3.0)","Use 'GI.Gtk.Interfaces.Scrollable.scrollableSetHadjustment'"] #-}
-- | Sets the t'GI.Gtk.Objects.Adjustment.Adjustment' for the current horizontal aspect.
treeViewSetHadjustment ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a, Gtk.Adjustment.IsAdjustment b) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'
    -> Maybe (b)
    -- ^ /@adjustment@/: The t'GI.Gtk.Objects.Adjustment.Adjustment' to set, or 'P.Nothing'
    -> m ()
treeViewSetHadjustment treeView adjustment = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    maybeAdjustment <- case adjustment of
        Nothing -> return nullPtr
        Just jAdjustment -> do
            jAdjustment' <- unsafeManagedPtrCastPtr jAdjustment
            return jAdjustment'
    gtk_tree_view_set_hadjustment treeView' maybeAdjustment
    touchManagedPtr treeView
    whenJust adjustment touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewSetHadjustmentMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsTreeView a, Gtk.Adjustment.IsAdjustment b) => O.OverloadedMethod TreeViewSetHadjustmentMethodInfo a signature where
    overloadedMethod = treeViewSetHadjustment

instance O.OverloadedMethodInfo TreeViewSetHadjustmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewSetHadjustment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewSetHadjustment"
        })


#endif

-- method TreeView::set_headers_clickable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView." , sinceVersion = Nothing }
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
--                 { rawDocText = Just "%TRUE if the columns are clickable."
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

foreign import ccall "gtk_tree_view_set_headers_clickable" gtk_tree_view_set_headers_clickable :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | Allow the column title buttons to be clicked.
treeViewSetHeadersClickable ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'.
    -> Bool
    -- ^ /@setting@/: 'P.True' if the columns are clickable.
    -> m ()
treeViewSetHeadersClickable treeView setting = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    let setting' = (fromIntegral . fromEnum) setting
    gtk_tree_view_set_headers_clickable treeView' setting'
    touchManagedPtr treeView
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewSetHeadersClickableMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewSetHeadersClickableMethodInfo a signature where
    overloadedMethod = treeViewSetHeadersClickable

instance O.OverloadedMethodInfo TreeViewSetHeadersClickableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewSetHeadersClickable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewSetHeadersClickable"
        })


#endif

-- method TreeView::set_headers_visible
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "headers_visible"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE if the headers are visible"
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

foreign import ccall "gtk_tree_view_set_headers_visible" gtk_tree_view_set_headers_visible :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    CInt ->                                 -- headers_visible : TBasicType TBoolean
    IO ()

-- | Sets the visibility state of the headers.
treeViewSetHeadersVisible ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'.
    -> Bool
    -- ^ /@headersVisible@/: 'P.True' if the headers are visible
    -> m ()
treeViewSetHeadersVisible treeView headersVisible = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    let headersVisible' = (fromIntegral . fromEnum) headersVisible
    gtk_tree_view_set_headers_visible treeView' headersVisible'
    touchManagedPtr treeView
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewSetHeadersVisibleMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewSetHeadersVisibleMethodInfo a signature where
    overloadedMethod = treeViewSetHeadersVisible

instance O.OverloadedMethodInfo TreeViewSetHeadersVisibleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewSetHeadersVisible",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewSetHeadersVisible"
        })


#endif

-- method TreeView::set_hover_expand
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "%TRUE to enable hover selection mode"
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

foreign import ccall "gtk_tree_view_set_hover_expand" gtk_tree_view_set_hover_expand :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    CInt ->                                 -- expand : TBasicType TBoolean
    IO ()

-- | Enables or disables the hover expansion mode of /@treeView@/.
-- Hover expansion makes rows expand or collapse if the pointer
-- moves over them.
-- 
-- /Since: 2.6/
treeViewSetHoverExpand ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> Bool
    -- ^ /@expand@/: 'P.True' to enable hover selection mode
    -> m ()
treeViewSetHoverExpand treeView expand = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    let expand' = (fromIntegral . fromEnum) expand
    gtk_tree_view_set_hover_expand treeView' expand'
    touchManagedPtr treeView
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewSetHoverExpandMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewSetHoverExpandMethodInfo a signature where
    overloadedMethod = treeViewSetHoverExpand

instance O.OverloadedMethodInfo TreeViewSetHoverExpandMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewSetHoverExpand",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewSetHoverExpand"
        })


#endif

-- method TreeView::set_hover_selection
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "hover"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE to enable hover selection mode"
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

foreign import ccall "gtk_tree_view_set_hover_selection" gtk_tree_view_set_hover_selection :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    CInt ->                                 -- hover : TBasicType TBoolean
    IO ()

-- | Enables or disables the hover selection mode of /@treeView@/.
-- Hover selection makes the selected row follow the pointer.
-- Currently, this works only for the selection modes
-- 'GI.Gtk.Enums.SelectionModeSingle' and 'GI.Gtk.Enums.SelectionModeBrowse'.
-- 
-- /Since: 2.6/
treeViewSetHoverSelection ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> Bool
    -- ^ /@hover@/: 'P.True' to enable hover selection mode
    -> m ()
treeViewSetHoverSelection treeView hover = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    let hover' = (fromIntegral . fromEnum) hover
    gtk_tree_view_set_hover_selection treeView' hover'
    touchManagedPtr treeView
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewSetHoverSelectionMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewSetHoverSelectionMethodInfo a signature where
    overloadedMethod = treeViewSetHoverSelection

instance O.OverloadedMethodInfo TreeViewSetHoverSelectionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewSetHoverSelection",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewSetHoverSelection"
        })


#endif

-- method TreeView::set_level_indentation
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "indentation"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the amount, in pixels, of extra indentation in @tree_view."
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

foreign import ccall "gtk_tree_view_set_level_indentation" gtk_tree_view_set_level_indentation :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Int32 ->                                -- indentation : TBasicType TInt
    IO ()

-- | Sets the amount of extra indentation for child levels to use in /@treeView@/
-- in addition to the default indentation.  The value should be specified in
-- pixels, a value of 0 disables this feature and in this case only the default
-- indentation will be used.
-- This does not have any visible effects for lists.
-- 
-- /Since: 2.12/
treeViewSetLevelIndentation ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> Int32
    -- ^ /@indentation@/: the amount, in pixels, of extra indentation in /@treeView@/.
    -> m ()
treeViewSetLevelIndentation treeView indentation = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    gtk_tree_view_set_level_indentation treeView' indentation
    touchManagedPtr treeView
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewSetLevelIndentationMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewSetLevelIndentationMethodInfo a signature where
    overloadedMethod = treeViewSetLevelIndentation

instance O.OverloadedMethodInfo TreeViewSetLevelIndentationMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewSetLevelIndentation",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewSetLevelIndentation"
        })


#endif

-- method TreeView::set_model
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The model." , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_view_set_model" gtk_tree_view_set_model :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Ptr Gtk.TreeModel.TreeModel ->          -- model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    IO ()

-- | Sets the model for a t'GI.Gtk.Objects.TreeView.TreeView'.  If the /@treeView@/ already has a model
-- set, it will remove it before setting the new model.  If /@model@/ is 'P.Nothing',
-- then it will unset the old model.
treeViewSetModel ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a, Gtk.TreeModel.IsTreeModel b) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'.
    -> Maybe (b)
    -- ^ /@model@/: The model.
    -> m ()
treeViewSetModel treeView model = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    maybeModel <- case model of
        Nothing -> return nullPtr
        Just jModel -> do
            jModel' <- unsafeManagedPtrCastPtr jModel
            return jModel'
    gtk_tree_view_set_model treeView' maybeModel
    touchManagedPtr treeView
    whenJust model touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewSetModelMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsTreeView a, Gtk.TreeModel.IsTreeModel b) => O.OverloadedMethod TreeViewSetModelMethodInfo a signature where
    overloadedMethod = treeViewSetModel

instance O.OverloadedMethodInfo TreeViewSetModelMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewSetModel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewSetModel"
        })


#endif

-- method TreeView::set_reorderable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView." , sinceVersion = Nothing }
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
--                 { rawDocText = Just "%TRUE, if the tree can be reordered."
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

foreign import ccall "gtk_tree_view_set_reorderable" gtk_tree_view_set_reorderable :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    CInt ->                                 -- reorderable : TBasicType TBoolean
    IO ()

-- | This function is a convenience function to allow you to reorder
-- models that support the t'GI.Gtk.Structs.TreeDragSourceIface.TreeDragSourceIface' and the
-- t'GI.Gtk.Structs.TreeDragDestIface.TreeDragDestIface'.  Both t'GI.Gtk.Objects.TreeStore.TreeStore' and t'GI.Gtk.Objects.ListStore.ListStore' support
-- these.  If /@reorderable@/ is 'P.True', then the user can reorder the
-- model by dragging and dropping rows. The developer can listen to
-- these changes by connecting to the model’s [TreeModel::rowInserted]("GI.Gtk.Interfaces.TreeModel#g:signal:rowInserted")
-- and [TreeModel::rowDeleted]("GI.Gtk.Interfaces.TreeModel#g:signal:rowDeleted") signals. The reordering is implemented
-- by setting up the tree view as a drag source and destination.
-- Therefore, drag and drop can not be used in a reorderable view for any
-- other purpose.
-- 
-- This function does not give you any degree of control over the order -- any
-- reordering is allowed.  If more control is needed, you should probably
-- handle drag and drop manually.
treeViewSetReorderable ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'.
    -> Bool
    -- ^ /@reorderable@/: 'P.True', if the tree can be reordered.
    -> m ()
treeViewSetReorderable treeView reorderable = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    let reorderable' = (fromIntegral . fromEnum) reorderable
    gtk_tree_view_set_reorderable treeView' reorderable'
    touchManagedPtr treeView
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewSetReorderableMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewSetReorderableMethodInfo a signature where
    overloadedMethod = treeViewSetReorderable

instance O.OverloadedMethodInfo TreeViewSetReorderableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewSetReorderable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewSetReorderable"
        })


#endif

-- method TreeView::set_row_separator_func
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "func"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "TreeViewRowSeparatorFunc" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeViewRowSeparatorFunc"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeNotified
--           , argClosure = 2
--           , argDestroy = 3
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "user data to pass to @func, or %NULL"
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "destroy notifier for @data, or %NULL"
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

foreign import ccall "gtk_tree_view_set_row_separator_func" gtk_tree_view_set_row_separator_func :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    FunPtr Gtk.Callbacks.C_TreeViewRowSeparatorFunc -> -- func : TInterface (Name {namespace = "Gtk", name = "TreeViewRowSeparatorFunc"})
    Ptr () ->                               -- data : TBasicType TPtr
    FunPtr GLib.Callbacks.C_DestroyNotify -> -- destroy : TInterface (Name {namespace = "GLib", name = "DestroyNotify"})
    IO ()

-- | Sets the row separator function, which is used to determine
-- whether a row should be drawn as a separator. If the row separator
-- function is 'P.Nothing', no separators are drawn. This is the default value.
-- 
-- /Since: 2.6/
treeViewSetRowSeparatorFunc ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> Maybe (Gtk.Callbacks.TreeViewRowSeparatorFunc)
    -- ^ /@func@/: a t'GI.Gtk.Callbacks.TreeViewRowSeparatorFunc'
    -> m ()
treeViewSetRowSeparatorFunc treeView func = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    maybeFunc <- case func of
        Nothing -> return (castPtrToFunPtr nullPtr)
        Just jFunc -> do
            jFunc' <- Gtk.Callbacks.mk_TreeViewRowSeparatorFunc (Gtk.Callbacks.wrap_TreeViewRowSeparatorFunc Nothing (Gtk.Callbacks.drop_closures_TreeViewRowSeparatorFunc jFunc))
            return jFunc'
    let data_ = castFunPtrToPtr maybeFunc
    let destroy = SP.safeFreeFunPtrPtr
    gtk_tree_view_set_row_separator_func treeView' maybeFunc data_ destroy
    touchManagedPtr treeView
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewSetRowSeparatorFuncMethodInfo
instance (signature ~ (Maybe (Gtk.Callbacks.TreeViewRowSeparatorFunc) -> m ()), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewSetRowSeparatorFuncMethodInfo a signature where
    overloadedMethod = treeViewSetRowSeparatorFunc

instance O.OverloadedMethodInfo TreeViewSetRowSeparatorFuncMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewSetRowSeparatorFunc",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewSetRowSeparatorFunc"
        })


#endif

-- method TreeView::set_rubber_banding
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "enable"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE to enable rubber banding"
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

foreign import ccall "gtk_tree_view_set_rubber_banding" gtk_tree_view_set_rubber_banding :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    CInt ->                                 -- enable : TBasicType TBoolean
    IO ()

-- | Enables or disables rubber banding in /@treeView@/.  If the selection mode
-- is @/GTK_SELECTION_MULTIPLE/@, rubber banding will allow the user to select
-- multiple rows by dragging the mouse.
-- 
-- /Since: 2.10/
treeViewSetRubberBanding ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> Bool
    -- ^ /@enable@/: 'P.True' to enable rubber banding
    -> m ()
treeViewSetRubberBanding treeView enable = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    let enable' = (fromIntegral . fromEnum) enable
    gtk_tree_view_set_rubber_banding treeView' enable'
    touchManagedPtr treeView
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewSetRubberBandingMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewSetRubberBandingMethodInfo a signature where
    overloadedMethod = treeViewSetRubberBanding

instance O.OverloadedMethodInfo TreeViewSetRubberBandingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewSetRubberBanding",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewSetRubberBanding"
        })


#endif

-- method TreeView::set_rules_hint
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
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
--                     Just "%TRUE if the tree requires reading across rows"
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

foreign import ccall "gtk_tree_view_set_rules_hint" gtk_tree_view_set_rules_hint :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

{-# DEPRECATED treeViewSetRulesHint ["(Since version 3.14)"] #-}
-- | Sets a hint for the theme to draw even\/odd rows in the /@treeView@/
-- with different colors, also known as \"zebra striping\".
-- 
-- This function tells the GTK+ theme that the user interface for your
-- application requires users to read across tree rows and associate
-- cells with one another.
-- 
-- Do not use it just because you prefer the appearance of the ruled
-- tree; that’s a question for the theme. Some themes will draw tree
-- rows in alternating colors even when rules are turned off, and
-- users who prefer that appearance all the time can choose those
-- themes. You should call this function only as a semantic hint to
-- the theme engine that your tree makes alternating colors useful
-- from a functional standpoint (since it has lots of columns,
-- generally).
treeViewSetRulesHint ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> Bool
    -- ^ /@setting@/: 'P.True' if the tree requires reading across rows
    -> m ()
treeViewSetRulesHint treeView setting = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    let setting' = (fromIntegral . fromEnum) setting
    gtk_tree_view_set_rules_hint treeView' setting'
    touchManagedPtr treeView
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewSetRulesHintMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewSetRulesHintMethodInfo a signature where
    overloadedMethod = treeViewSetRulesHint

instance O.OverloadedMethodInfo TreeViewSetRulesHintMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewSetRulesHint",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewSetRulesHint"
        })


#endif

-- method TreeView::set_search_column
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView" , sinceVersion = Nothing }
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
--                     Just
--                       "the column of the model to search in, or -1 to disable searching"
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

foreign import ccall "gtk_tree_view_set_search_column" gtk_tree_view_set_search_column :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Int32 ->                                -- column : TBasicType TInt
    IO ()

-- | Sets /@column@/ as the column where the interactive search code should
-- search in for the current model.
-- 
-- If the search column is set, users can use the “start-interactive-search”
-- key binding to bring up search popup. The enable-search property controls
-- whether simply typing text will also start an interactive search.
-- 
-- Note that /@column@/ refers to a column of the current model. The search
-- column is reset to -1 when the model is changed.
treeViewSetSearchColumn ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'
    -> Int32
    -- ^ /@column@/: the column of the model to search in, or -1 to disable searching
    -> m ()
treeViewSetSearchColumn treeView column = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    gtk_tree_view_set_search_column treeView' column
    touchManagedPtr treeView
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewSetSearchColumnMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewSetSearchColumnMethodInfo a signature where
    overloadedMethod = treeViewSetSearchColumn

instance O.OverloadedMethodInfo TreeViewSetSearchColumnMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewSetSearchColumn",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewSetSearchColumn"
        })


#endif

-- method TreeView::set_search_entry
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the entry the interactive search code of @tree_view should use or %NULL"
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

foreign import ccall "gtk_tree_view_set_search_entry" gtk_tree_view_set_search_entry :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Ptr Gtk.Entry.Entry ->                  -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    IO ()

-- | Sets the entry which the interactive search code will use for this
-- /@treeView@/.  This is useful when you want to provide a search entry
-- in our interface at all time at a fixed position.  Passing 'P.Nothing' for
-- /@entry@/ will make the interactive search code use the built-in popup
-- entry again.
-- 
-- /Since: 2.10/
treeViewSetSearchEntry ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a, Gtk.Entry.IsEntry b) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'
    -> Maybe (b)
    -- ^ /@entry@/: the entry the interactive search code of /@treeView@/ should use or 'P.Nothing'
    -> m ()
treeViewSetSearchEntry treeView entry = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    maybeEntry <- case entry of
        Nothing -> return nullPtr
        Just jEntry -> do
            jEntry' <- unsafeManagedPtrCastPtr jEntry
            return jEntry'
    gtk_tree_view_set_search_entry treeView' maybeEntry
    touchManagedPtr treeView
    whenJust entry touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewSetSearchEntryMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsTreeView a, Gtk.Entry.IsEntry b) => O.OverloadedMethod TreeViewSetSearchEntryMethodInfo a signature where
    overloadedMethod = treeViewSetSearchEntry

instance O.OverloadedMethodInfo TreeViewSetSearchEntryMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewSetSearchEntry",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewSetSearchEntry"
        })


#endif

-- method TreeView::set_search_equal_func
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "search_equal_func"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "TreeViewSearchEqualFunc" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the compare function to use during the search"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeNotified
--           , argClosure = 2
--           , argDestroy = 3
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "search_user_data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "user data to pass to @search_equal_func, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "search_destroy"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "DestroyNotify" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "Destroy notifier for @search_user_data, or %NULL"
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

foreign import ccall "gtk_tree_view_set_search_equal_func" gtk_tree_view_set_search_equal_func :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    FunPtr Gtk.Callbacks.C_TreeViewSearchEqualFunc -> -- search_equal_func : TInterface (Name {namespace = "Gtk", name = "TreeViewSearchEqualFunc"})
    Ptr () ->                               -- search_user_data : TBasicType TPtr
    FunPtr GLib.Callbacks.C_DestroyNotify -> -- search_destroy : TInterface (Name {namespace = "GLib", name = "DestroyNotify"})
    IO ()

-- | Sets the compare function for the interactive search capabilities; note
-- that somewhat like @/strcmp()/@ returning 0 for equality
-- t'GI.Gtk.Callbacks.TreeViewSearchEqualFunc' returns 'P.False' on matches.
treeViewSetSearchEqualFunc ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'
    -> Gtk.Callbacks.TreeViewSearchEqualFunc
    -- ^ /@searchEqualFunc@/: the compare function to use during the search
    -> m ()
treeViewSetSearchEqualFunc treeView searchEqualFunc = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    searchEqualFunc' <- Gtk.Callbacks.mk_TreeViewSearchEqualFunc (Gtk.Callbacks.wrap_TreeViewSearchEqualFunc Nothing (Gtk.Callbacks.drop_closures_TreeViewSearchEqualFunc searchEqualFunc))
    let searchUserData = castFunPtrToPtr searchEqualFunc'
    let searchDestroy = SP.safeFreeFunPtrPtr
    gtk_tree_view_set_search_equal_func treeView' searchEqualFunc' searchUserData searchDestroy
    touchManagedPtr treeView
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewSetSearchEqualFuncMethodInfo
instance (signature ~ (Gtk.Callbacks.TreeViewSearchEqualFunc -> m ()), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewSetSearchEqualFuncMethodInfo a signature where
    overloadedMethod = treeViewSetSearchEqualFunc

instance O.OverloadedMethodInfo TreeViewSetSearchEqualFuncMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewSetSearchEqualFunc",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewSetSearchEqualFunc"
        })


#endif

-- method TreeView::set_search_position_func
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "func"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "TreeViewSearchPositionFunc" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the function to use to position the search dialog, or %NULL\n   to use the default search position function"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeNotified
--           , argClosure = 2
--           , argDestroy = 3
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "user data to pass to @func, or %NULL"
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Destroy notifier for @data, or %NULL"
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

foreign import ccall "gtk_tree_view_set_search_position_func" gtk_tree_view_set_search_position_func :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    FunPtr Gtk.Callbacks.C_TreeViewSearchPositionFunc -> -- func : TInterface (Name {namespace = "Gtk", name = "TreeViewSearchPositionFunc"})
    Ptr () ->                               -- data : TBasicType TPtr
    FunPtr GLib.Callbacks.C_DestroyNotify -> -- destroy : TInterface (Name {namespace = "GLib", name = "DestroyNotify"})
    IO ()

-- | Sets the function to use when positioning the search dialog.
-- 
-- /Since: 2.10/
treeViewSetSearchPositionFunc ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'
    -> Maybe (Gtk.Callbacks.TreeViewSearchPositionFunc)
    -- ^ /@func@/: the function to use to position the search dialog, or 'P.Nothing'
    --    to use the default search position function
    -> m ()
treeViewSetSearchPositionFunc treeView func = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    maybeFunc <- case func of
        Nothing -> return (castPtrToFunPtr nullPtr)
        Just jFunc -> do
            jFunc' <- Gtk.Callbacks.mk_TreeViewSearchPositionFunc (Gtk.Callbacks.wrap_TreeViewSearchPositionFunc Nothing (Gtk.Callbacks.drop_closures_TreeViewSearchPositionFunc jFunc))
            return jFunc'
    let data_ = castFunPtrToPtr maybeFunc
    let destroy = SP.safeFreeFunPtrPtr
    gtk_tree_view_set_search_position_func treeView' maybeFunc data_ destroy
    touchManagedPtr treeView
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewSetSearchPositionFuncMethodInfo
instance (signature ~ (Maybe (Gtk.Callbacks.TreeViewSearchPositionFunc) -> m ()), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewSetSearchPositionFuncMethodInfo a signature where
    overloadedMethod = treeViewSetSearchPositionFunc

instance O.OverloadedMethodInfo TreeViewSetSearchPositionFuncMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewSetSearchPositionFunc",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewSetSearchPositionFunc"
        })


#endif

-- method TreeView::set_show_expanders
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "enabled"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "%TRUE to enable expander drawing, %FALSE otherwise."
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

foreign import ccall "gtk_tree_view_set_show_expanders" gtk_tree_view_set_show_expanders :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    CInt ->                                 -- enabled : TBasicType TBoolean
    IO ()

-- | Sets whether to draw and enable expanders and indent child rows in
-- /@treeView@/.  When disabled there will be no expanders visible in trees
-- and there will be no way to expand and collapse rows by default.  Also
-- note that hiding the expanders will disable the default indentation.  You
-- can set a custom indentation in this case using
-- 'GI.Gtk.Objects.TreeView.treeViewSetLevelIndentation'.
-- This does not have any visible effects for lists.
-- 
-- /Since: 2.12/
treeViewSetShowExpanders ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> Bool
    -- ^ /@enabled@/: 'P.True' to enable expander drawing, 'P.False' otherwise.
    -> m ()
treeViewSetShowExpanders treeView enabled = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    let enabled' = (fromIntegral . fromEnum) enabled
    gtk_tree_view_set_show_expanders treeView' enabled'
    touchManagedPtr treeView
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewSetShowExpandersMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewSetShowExpandersMethodInfo a signature where
    overloadedMethod = treeViewSetShowExpanders

instance O.OverloadedMethodInfo TreeViewSetShowExpandersMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewSetShowExpanders",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewSetShowExpanders"
        })


#endif

-- method TreeView::set_tooltip_cell
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "tooltip"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Tooltip" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTooltip" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreePath or %NULL"
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
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeViewColumn" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeViewColumn or %NULL"
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellRenderer or %NULL"
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

foreign import ccall "gtk_tree_view_set_tooltip_cell" gtk_tree_view_set_tooltip_cell :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Ptr Gtk.Tooltip.Tooltip ->              -- tooltip : TInterface (Name {namespace = "Gtk", name = "Tooltip"})
    Ptr Gtk.TreePath.TreePath ->            -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    Ptr Gtk.TreeViewColumn.TreeViewColumn -> -- column : TInterface (Name {namespace = "Gtk", name = "TreeViewColumn"})
    Ptr Gtk.CellRenderer.CellRenderer ->    -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    IO ()

-- | Sets the tip area of /@tooltip@/ to the area /@path@/, /@column@/ and /@cell@/ have
-- in common.  For example if /@path@/ is 'P.Nothing' and /@column@/ is set, the tip
-- area will be set to the full area covered by /@column@/.  See also
-- 'GI.Gtk.Objects.Tooltip.tooltipSetTipArea'.
-- 
-- Note that if /@path@/ is not specified and /@cell@/ is set and part of a column
-- containing the expander, the tooltip might not show and hide at the correct
-- position.  In such cases /@path@/ must be set to the current node under the
-- mouse cursor for this function to operate correctly.
-- 
-- See also 'GI.Gtk.Objects.TreeView.treeViewSetTooltipColumn' for a simpler alternative.
-- 
-- /Since: 2.12/
treeViewSetTooltipCell ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a, Gtk.Tooltip.IsTooltip b, Gtk.TreeViewColumn.IsTreeViewColumn c, Gtk.CellRenderer.IsCellRenderer d) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> b
    -- ^ /@tooltip@/: a t'GI.Gtk.Objects.Tooltip.Tooltip'
    -> Maybe (Gtk.TreePath.TreePath)
    -- ^ /@path@/: a t'GI.Gtk.Structs.TreePath.TreePath' or 'P.Nothing'
    -> Maybe (c)
    -- ^ /@column@/: a t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn' or 'P.Nothing'
    -> Maybe (d)
    -- ^ /@cell@/: a t'GI.Gtk.Objects.CellRenderer.CellRenderer' or 'P.Nothing'
    -> m ()
treeViewSetTooltipCell treeView tooltip path column cell = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    tooltip' <- unsafeManagedPtrCastPtr tooltip
    maybePath <- case path of
        Nothing -> return nullPtr
        Just jPath -> do
            jPath' <- unsafeManagedPtrGetPtr jPath
            return jPath'
    maybeColumn <- case column of
        Nothing -> return nullPtr
        Just jColumn -> do
            jColumn' <- unsafeManagedPtrCastPtr jColumn
            return jColumn'
    maybeCell <- case cell of
        Nothing -> return nullPtr
        Just jCell -> do
            jCell' <- unsafeManagedPtrCastPtr jCell
            return jCell'
    gtk_tree_view_set_tooltip_cell treeView' tooltip' maybePath maybeColumn maybeCell
    touchManagedPtr treeView
    touchManagedPtr tooltip
    whenJust path touchManagedPtr
    whenJust column touchManagedPtr
    whenJust cell touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewSetTooltipCellMethodInfo
instance (signature ~ (b -> Maybe (Gtk.TreePath.TreePath) -> Maybe (c) -> Maybe (d) -> m ()), MonadIO m, IsTreeView a, Gtk.Tooltip.IsTooltip b, Gtk.TreeViewColumn.IsTreeViewColumn c, Gtk.CellRenderer.IsCellRenderer d) => O.OverloadedMethod TreeViewSetTooltipCellMethodInfo a signature where
    overloadedMethod = treeViewSetTooltipCell

instance O.OverloadedMethodInfo TreeViewSetTooltipCellMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewSetTooltipCell",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewSetTooltipCell"
        })


#endif

-- method TreeView::set_tooltip_column
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
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
--                     Just
--                       "an integer, which is a valid column number for @tree_view\8217s model"
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

foreign import ccall "gtk_tree_view_set_tooltip_column" gtk_tree_view_set_tooltip_column :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Int32 ->                                -- column : TBasicType TInt
    IO ()

-- | If you only plan to have simple (text-only) tooltips on full rows, you
-- can use this function to have t'GI.Gtk.Objects.TreeView.TreeView' handle these automatically
-- for you. /@column@/ should be set to the column in /@treeView@/’s model
-- containing the tooltip texts, or -1 to disable this feature.
-- 
-- When enabled, [Widget:hasTooltip]("GI.Gtk.Objects.Widget#g:attr:hasTooltip") will be set to 'P.True' and
-- /@treeView@/ will connect a [Widget::queryTooltip]("GI.Gtk.Objects.Widget#g:signal:queryTooltip") signal handler.
-- 
-- Note that the signal handler sets the text with 'GI.Gtk.Objects.Tooltip.tooltipSetMarkup',
-- so &, \<, etc have to be escaped in the text.
-- 
-- /Since: 2.12/
treeViewSetTooltipColumn ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> Int32
    -- ^ /@column@/: an integer, which is a valid column number for /@treeView@/’s model
    -> m ()
treeViewSetTooltipColumn treeView column = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    gtk_tree_view_set_tooltip_column treeView' column
    touchManagedPtr treeView
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewSetTooltipColumnMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewSetTooltipColumnMethodInfo a signature where
    overloadedMethod = treeViewSetTooltipColumn

instance O.OverloadedMethodInfo TreeViewSetTooltipColumnMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewSetTooltipColumn",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewSetTooltipColumn"
        })


#endif

-- method TreeView::set_tooltip_row
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "tooltip"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Tooltip" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTooltip" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreePath" , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_view_set_tooltip_row" gtk_tree_view_set_tooltip_row :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Ptr Gtk.Tooltip.Tooltip ->              -- tooltip : TInterface (Name {namespace = "Gtk", name = "Tooltip"})
    Ptr Gtk.TreePath.TreePath ->            -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO ()

-- | Sets the tip area of /@tooltip@/ to be the area covered by the row at /@path@/.
-- See also 'GI.Gtk.Objects.TreeView.treeViewSetTooltipColumn' for a simpler alternative.
-- See also 'GI.Gtk.Objects.Tooltip.tooltipSetTipArea'.
-- 
-- /Since: 2.12/
treeViewSetTooltipRow ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a, Gtk.Tooltip.IsTooltip b) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> b
    -- ^ /@tooltip@/: a t'GI.Gtk.Objects.Tooltip.Tooltip'
    -> Gtk.TreePath.TreePath
    -- ^ /@path@/: a t'GI.Gtk.Structs.TreePath.TreePath'
    -> m ()
treeViewSetTooltipRow treeView tooltip path = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    tooltip' <- unsafeManagedPtrCastPtr tooltip
    path' <- unsafeManagedPtrGetPtr path
    gtk_tree_view_set_tooltip_row treeView' tooltip' path'
    touchManagedPtr treeView
    touchManagedPtr tooltip
    touchManagedPtr path
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewSetTooltipRowMethodInfo
instance (signature ~ (b -> Gtk.TreePath.TreePath -> m ()), MonadIO m, IsTreeView a, Gtk.Tooltip.IsTooltip b) => O.OverloadedMethod TreeViewSetTooltipRowMethodInfo a signature where
    overloadedMethod = treeViewSetTooltipRow

instance O.OverloadedMethodInfo TreeViewSetTooltipRowMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewSetTooltipRow",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewSetTooltipRow"
        })


#endif

-- method TreeView::set_vadjustment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "adjustment"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Adjustment" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The #GtkAdjustment to set, or %NULL"
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

foreign import ccall "gtk_tree_view_set_vadjustment" gtk_tree_view_set_vadjustment :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    Ptr Gtk.Adjustment.Adjustment ->        -- adjustment : TInterface (Name {namespace = "Gtk", name = "Adjustment"})
    IO ()

{-# DEPRECATED treeViewSetVadjustment ["(Since version 3.0)","Use 'GI.Gtk.Interfaces.Scrollable.scrollableSetVadjustment'"] #-}
-- | Sets the t'GI.Gtk.Objects.Adjustment.Adjustment' for the current vertical aspect.
treeViewSetVadjustment ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a, Gtk.Adjustment.IsAdjustment b) =>
    a
    -- ^ /@treeView@/: A t'GI.Gtk.Objects.TreeView.TreeView'
    -> Maybe (b)
    -- ^ /@adjustment@/: The t'GI.Gtk.Objects.Adjustment.Adjustment' to set, or 'P.Nothing'
    -> m ()
treeViewSetVadjustment treeView adjustment = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    maybeAdjustment <- case adjustment of
        Nothing -> return nullPtr
        Just jAdjustment -> do
            jAdjustment' <- unsafeManagedPtrCastPtr jAdjustment
            return jAdjustment'
    gtk_tree_view_set_vadjustment treeView' maybeAdjustment
    touchManagedPtr treeView
    whenJust adjustment touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewSetVadjustmentMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsTreeView a, Gtk.Adjustment.IsAdjustment b) => O.OverloadedMethod TreeViewSetVadjustmentMethodInfo a signature where
    overloadedMethod = treeViewSetVadjustment

instance O.OverloadedMethodInfo TreeViewSetVadjustmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewSetVadjustment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewSetVadjustment"
        })


#endif

-- method TreeView::unset_rows_drag_dest
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_view_unset_rows_drag_dest" gtk_tree_view_unset_rows_drag_dest :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    IO ()

-- | Undoes the effect of
-- 'GI.Gtk.Objects.TreeView.treeViewEnableModelDragDest'. Calling this method sets
-- [TreeView:reorderable]("GI.Gtk.Objects.TreeView#g:attr:reorderable") to 'P.False'.
treeViewUnsetRowsDragDest ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> m ()
treeViewUnsetRowsDragDest treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    gtk_tree_view_unset_rows_drag_dest treeView'
    touchManagedPtr treeView
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewUnsetRowsDragDestMethodInfo
instance (signature ~ (m ()), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewUnsetRowsDragDestMethodInfo a signature where
    overloadedMethod = treeViewUnsetRowsDragDest

instance O.OverloadedMethodInfo TreeViewUnsetRowsDragDestMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewUnsetRowsDragDest",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewUnsetRowsDragDest"
        })


#endif

-- method TreeView::unset_rows_drag_source
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeView" , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_view_unset_rows_drag_source" gtk_tree_view_unset_rows_drag_source :: 
    Ptr TreeView ->                         -- tree_view : TInterface (Name {namespace = "Gtk", name = "TreeView"})
    IO ()

-- | Undoes the effect of
-- 'GI.Gtk.Objects.TreeView.treeViewEnableModelDragSource'. Calling this method sets
-- [TreeView:reorderable]("GI.Gtk.Objects.TreeView#g:attr:reorderable") to 'P.False'.
treeViewUnsetRowsDragSource ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeView a) =>
    a
    -- ^ /@treeView@/: a t'GI.Gtk.Objects.TreeView.TreeView'
    -> m ()
treeViewUnsetRowsDragSource treeView = liftIO $ do
    treeView' <- unsafeManagedPtrCastPtr treeView
    gtk_tree_view_unset_rows_drag_source treeView'
    touchManagedPtr treeView
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeViewUnsetRowsDragSourceMethodInfo
instance (signature ~ (m ()), MonadIO m, IsTreeView a) => O.OverloadedMethod TreeViewUnsetRowsDragSourceMethodInfo a signature where
    overloadedMethod = treeViewUnsetRowsDragSource

instance O.OverloadedMethodInfo TreeViewUnsetRowsDragSourceMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeView.treeViewUnsetRowsDragSource",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeView.html#v:treeViewUnsetRowsDragSource"
        })


#endif


