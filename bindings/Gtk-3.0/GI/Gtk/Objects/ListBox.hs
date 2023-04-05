{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A GtkListBox is a vertical container that contains GtkListBoxRow
-- children. These rows can be dynamically sorted and filtered, and
-- headers can be added dynamically depending on the row content.
-- It also allows keyboard and mouse navigation and selection like
-- a typical list.
-- 
-- Using GtkListBox is often an alternative to t'GI.Gtk.Objects.TreeView.TreeView', especially
-- when the list contents has a more complicated layout than what is allowed
-- by a t'GI.Gtk.Objects.CellRenderer.CellRenderer', or when the contents is interactive (i.e. has a
-- button in it).
-- 
-- Although a t'GI.Gtk.Objects.ListBox.ListBox' must have only t'GI.Gtk.Objects.ListBoxRow.ListBoxRow' children you can
-- add any kind of widget to it via 'GI.Gtk.Objects.Container.containerAdd', and a t'GI.Gtk.Objects.ListBoxRow.ListBoxRow'
-- widget will automatically be inserted between the list and the widget.
-- 
-- @/GtkListBoxRows/@ can be marked as activatable or selectable. If a row
-- is activatable, [ListBox::rowActivated]("GI.Gtk.Objects.ListBox#g:signal:rowActivated") will be emitted for it when
-- the user tries to activate it. If it is selectable, the row will be marked
-- as selected when the user tries to select it.
-- 
-- The GtkListBox widget was added in GTK+ 3.10.
-- 
-- = GtkListBox as GtkBuildable
-- 
-- The GtkListBox implementation of the t'GI.Gtk.Interfaces.Buildable.Buildable' interface supports
-- setting a child as the placeholder by specifying “placeholder” as the “type”
-- attribute of a @\<child>@ element. See 'GI.Gtk.Objects.ListBox.listBoxSetPlaceholder' for info.
-- 
-- = CSS nodes
-- 
-- 
-- === /plain code/
-- >
-- >list
-- >╰── row[.activatable]
-- 
-- 
-- GtkListBox uses a single CSS node named list. Each GtkListBoxRow uses
-- a single CSS node named row. The row nodes get the .activatable
-- style class added when appropriate.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.ListBox
    ( 

-- * Exported types
    ListBox(..)                             ,
    IsListBox                               ,
    toListBox                               ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindModel]("GI.Gtk.Objects.ListBox#g:method:bindModel"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragHighlightRow]("GI.Gtk.Objects.ListBox#g:method:dragHighlightRow"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [dragUnhighlightRow]("GI.Gtk.Objects.ListBox#g:method:dragUnhighlightRow"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insert]("GI.Gtk.Objects.ListBox#g:method:insert"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [invalidateFilter]("GI.Gtk.Objects.ListBox#g:method:invalidateFilter"), [invalidateHeaders]("GI.Gtk.Objects.ListBox#g:method:invalidateHeaders"), [invalidateSort]("GI.Gtk.Objects.ListBox#g:method:invalidateSort"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [prepend]("GI.Gtk.Objects.ListBox#g:method:prepend"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [selectAll]("GI.Gtk.Objects.ListBox#g:method:selectAll"), [selectRow]("GI.Gtk.Objects.ListBox#g:method:selectRow"), [selectedForeach]("GI.Gtk.Objects.ListBox#g:method:selectedForeach"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unselectAll]("GI.Gtk.Objects.ListBox#g:method:unselectAll"), [unselectRow]("GI.Gtk.Objects.ListBox#g:method:unselectRow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getActivateOnSingleClick]("GI.Gtk.Objects.ListBox#g:method:getActivateOnSingleClick"), [getAdjustment]("GI.Gtk.Objects.ListBox#g:method:getAdjustment"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getRowAtIndex]("GI.Gtk.Objects.ListBox#g:method:getRowAtIndex"), [getRowAtY]("GI.Gtk.Objects.ListBox#g:method:getRowAtY"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSelectedRow]("GI.Gtk.Objects.ListBox#g:method:getSelectedRow"), [getSelectedRows]("GI.Gtk.Objects.ListBox#g:method:getSelectedRows"), [getSelectionMode]("GI.Gtk.Objects.ListBox#g:method:getSelectionMode"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setActivateOnSingleClick]("GI.Gtk.Objects.ListBox#g:method:setActivateOnSingleClick"), [setAdjustment]("GI.Gtk.Objects.ListBox#g:method:setAdjustment"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFilterFunc]("GI.Gtk.Objects.ListBox#g:method:setFilterFunc"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHeaderFunc]("GI.Gtk.Objects.ListBox#g:method:setHeaderFunc"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setPlaceholder]("GI.Gtk.Objects.ListBox#g:method:setPlaceholder"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setSelectionMode]("GI.Gtk.Objects.ListBox#g:method:setSelectionMode"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setSortFunc]("GI.Gtk.Objects.ListBox#g:method:setSortFunc"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveListBoxMethod                    ,
#endif

-- ** bindModel #method:bindModel#

#if defined(ENABLE_OVERLOADING)
    ListBoxBindModelMethodInfo              ,
#endif
    listBoxBindModel                        ,


-- ** dragHighlightRow #method:dragHighlightRow#

#if defined(ENABLE_OVERLOADING)
    ListBoxDragHighlightRowMethodInfo       ,
#endif
    listBoxDragHighlightRow                 ,


-- ** dragUnhighlightRow #method:dragUnhighlightRow#

#if defined(ENABLE_OVERLOADING)
    ListBoxDragUnhighlightRowMethodInfo     ,
#endif
    listBoxDragUnhighlightRow               ,


-- ** getActivateOnSingleClick #method:getActivateOnSingleClick#

#if defined(ENABLE_OVERLOADING)
    ListBoxGetActivateOnSingleClickMethodInfo,
#endif
    listBoxGetActivateOnSingleClick         ,


-- ** getAdjustment #method:getAdjustment#

#if defined(ENABLE_OVERLOADING)
    ListBoxGetAdjustmentMethodInfo          ,
#endif
    listBoxGetAdjustment                    ,


-- ** getRowAtIndex #method:getRowAtIndex#

#if defined(ENABLE_OVERLOADING)
    ListBoxGetRowAtIndexMethodInfo          ,
#endif
    listBoxGetRowAtIndex                    ,


-- ** getRowAtY #method:getRowAtY#

#if defined(ENABLE_OVERLOADING)
    ListBoxGetRowAtYMethodInfo              ,
#endif
    listBoxGetRowAtY                        ,


-- ** getSelectedRow #method:getSelectedRow#

#if defined(ENABLE_OVERLOADING)
    ListBoxGetSelectedRowMethodInfo         ,
#endif
    listBoxGetSelectedRow                   ,


-- ** getSelectedRows #method:getSelectedRows#

#if defined(ENABLE_OVERLOADING)
    ListBoxGetSelectedRowsMethodInfo        ,
#endif
    listBoxGetSelectedRows                  ,


-- ** getSelectionMode #method:getSelectionMode#

#if defined(ENABLE_OVERLOADING)
    ListBoxGetSelectionModeMethodInfo       ,
#endif
    listBoxGetSelectionMode                 ,


-- ** insert #method:insert#

#if defined(ENABLE_OVERLOADING)
    ListBoxInsertMethodInfo                 ,
#endif
    listBoxInsert                           ,


-- ** invalidateFilter #method:invalidateFilter#

#if defined(ENABLE_OVERLOADING)
    ListBoxInvalidateFilterMethodInfo       ,
#endif
    listBoxInvalidateFilter                 ,


-- ** invalidateHeaders #method:invalidateHeaders#

#if defined(ENABLE_OVERLOADING)
    ListBoxInvalidateHeadersMethodInfo      ,
#endif
    listBoxInvalidateHeaders                ,


-- ** invalidateSort #method:invalidateSort#

#if defined(ENABLE_OVERLOADING)
    ListBoxInvalidateSortMethodInfo         ,
#endif
    listBoxInvalidateSort                   ,


-- ** new #method:new#

    listBoxNew                              ,


-- ** prepend #method:prepend#

#if defined(ENABLE_OVERLOADING)
    ListBoxPrependMethodInfo                ,
#endif
    listBoxPrepend                          ,


-- ** selectAll #method:selectAll#

#if defined(ENABLE_OVERLOADING)
    ListBoxSelectAllMethodInfo              ,
#endif
    listBoxSelectAll                        ,


-- ** selectRow #method:selectRow#

#if defined(ENABLE_OVERLOADING)
    ListBoxSelectRowMethodInfo              ,
#endif
    listBoxSelectRow                        ,


-- ** selectedForeach #method:selectedForeach#

#if defined(ENABLE_OVERLOADING)
    ListBoxSelectedForeachMethodInfo        ,
#endif
    listBoxSelectedForeach                  ,


-- ** setActivateOnSingleClick #method:setActivateOnSingleClick#

#if defined(ENABLE_OVERLOADING)
    ListBoxSetActivateOnSingleClickMethodInfo,
#endif
    listBoxSetActivateOnSingleClick         ,


-- ** setAdjustment #method:setAdjustment#

#if defined(ENABLE_OVERLOADING)
    ListBoxSetAdjustmentMethodInfo          ,
#endif
    listBoxSetAdjustment                    ,


-- ** setFilterFunc #method:setFilterFunc#

#if defined(ENABLE_OVERLOADING)
    ListBoxSetFilterFuncMethodInfo          ,
#endif
    listBoxSetFilterFunc                    ,


-- ** setHeaderFunc #method:setHeaderFunc#

#if defined(ENABLE_OVERLOADING)
    ListBoxSetHeaderFuncMethodInfo          ,
#endif
    listBoxSetHeaderFunc                    ,


-- ** setPlaceholder #method:setPlaceholder#

#if defined(ENABLE_OVERLOADING)
    ListBoxSetPlaceholderMethodInfo         ,
#endif
    listBoxSetPlaceholder                   ,


-- ** setSelectionMode #method:setSelectionMode#

#if defined(ENABLE_OVERLOADING)
    ListBoxSetSelectionModeMethodInfo       ,
#endif
    listBoxSetSelectionMode                 ,


-- ** setSortFunc #method:setSortFunc#

#if defined(ENABLE_OVERLOADING)
    ListBoxSetSortFuncMethodInfo            ,
#endif
    listBoxSetSortFunc                      ,


-- ** unselectAll #method:unselectAll#

#if defined(ENABLE_OVERLOADING)
    ListBoxUnselectAllMethodInfo            ,
#endif
    listBoxUnselectAll                      ,


-- ** unselectRow #method:unselectRow#

#if defined(ENABLE_OVERLOADING)
    ListBoxUnselectRowMethodInfo            ,
#endif
    listBoxUnselectRow                      ,




 -- * Properties


-- ** activateOnSingleClick #attr:activateOnSingleClick#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ListBoxActivateOnSingleClickPropertyInfo,
#endif
    constructListBoxActivateOnSingleClick   ,
    getListBoxActivateOnSingleClick         ,
#if defined(ENABLE_OVERLOADING)
    listBoxActivateOnSingleClick            ,
#endif
    setListBoxActivateOnSingleClick         ,


-- ** selectionMode #attr:selectionMode#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ListBoxSelectionModePropertyInfo        ,
#endif
    constructListBoxSelectionMode           ,
    getListBoxSelectionMode                 ,
#if defined(ENABLE_OVERLOADING)
    listBoxSelectionMode                    ,
#endif
    setListBoxSelectionMode                 ,




 -- * Signals


-- ** activateCursorRow #signal:activateCursorRow#

    ListBoxActivateCursorRowCallback        ,
#if defined(ENABLE_OVERLOADING)
    ListBoxActivateCursorRowSignalInfo      ,
#endif
    afterListBoxActivateCursorRow           ,
    onListBoxActivateCursorRow              ,


-- ** moveCursor #signal:moveCursor#

    ListBoxMoveCursorCallback               ,
#if defined(ENABLE_OVERLOADING)
    ListBoxMoveCursorSignalInfo             ,
#endif
    afterListBoxMoveCursor                  ,
    onListBoxMoveCursor                     ,


-- ** rowActivated #signal:rowActivated#

    ListBoxRowActivatedCallback             ,
#if defined(ENABLE_OVERLOADING)
    ListBoxRowActivatedSignalInfo           ,
#endif
    afterListBoxRowActivated                ,
    onListBoxRowActivated                   ,


-- ** rowSelected #signal:rowSelected#

    ListBoxRowSelectedCallback              ,
#if defined(ENABLE_OVERLOADING)
    ListBoxRowSelectedSignalInfo            ,
#endif
    afterListBoxRowSelected                 ,
    onListBoxRowSelected                    ,


-- ** selectAll #signal:selectAll#

    ListBoxSelectAllCallback                ,
#if defined(ENABLE_OVERLOADING)
    ListBoxSelectAllSignalInfo              ,
#endif
    afterListBoxSelectAll                   ,
    onListBoxSelectAll                      ,


-- ** selectedRowsChanged #signal:selectedRowsChanged#

    ListBoxSelectedRowsChangedCallback      ,
#if defined(ENABLE_OVERLOADING)
    ListBoxSelectedRowsChangedSignalInfo    ,
#endif
    afterListBoxSelectedRowsChanged         ,
    onListBoxSelectedRowsChanged            ,


-- ** toggleCursorRow #signal:toggleCursorRow#

    ListBoxToggleCursorRowCallback          ,
#if defined(ENABLE_OVERLOADING)
    ListBoxToggleCursorRowSignalInfo        ,
#endif
    afterListBoxToggleCursorRow             ,
    onListBoxToggleCursorRow                ,


-- ** unselectAll #signal:unselectAll#

    ListBoxUnselectAllCallback              ,
#if defined(ENABLE_OVERLOADING)
    ListBoxUnselectAllSignalInfo            ,
#endif
    afterListBoxUnselectAll                 ,
    onListBoxUnselectAll                    ,




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
import qualified GI.GLib.Callbacks as GLib.Callbacks
import qualified GI.GObject.Objects.Object as GObject.Object
import qualified GI.Gio.Interfaces.ListModel as Gio.ListModel
import qualified GI.Gtk.Callbacks as Gtk.Callbacks
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Objects.Adjustment as Gtk.Adjustment
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.ListBoxRow as Gtk.ListBoxRow
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype ListBox = ListBox (SP.ManagedPtr ListBox)
    deriving (Eq)

instance SP.ManagedPtrNewtype ListBox where
    toManagedPtr (ListBox p) = p

foreign import ccall "gtk_list_box_get_type"
    c_gtk_list_box_get_type :: IO B.Types.GType

instance B.Types.TypedObject ListBox where
    glibType = c_gtk_list_box_get_type

instance B.Types.GObject ListBox

-- | Type class for types which can be safely cast to `ListBox`, for instance with `toListBox`.
class (SP.GObject o, O.IsDescendantOf ListBox o) => IsListBox o
instance (SP.GObject o, O.IsDescendantOf ListBox o) => IsListBox o

instance O.HasParentTypes ListBox
type instance O.ParentTypes ListBox = '[Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable]

-- | Cast to `ListBox`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toListBox :: (MIO.MonadIO m, IsListBox o) => o -> m ListBox
toListBox = MIO.liftIO . B.ManagedPtr.unsafeCastTo ListBox

-- | Convert 'ListBox' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe ListBox) where
    gvalueGType_ = c_gtk_list_box_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr ListBox)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr ListBox)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject ListBox ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveListBoxMethod (t :: Symbol) (o :: *) :: * where
    ResolveListBoxMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveListBoxMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveListBoxMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveListBoxMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveListBoxMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveListBoxMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveListBoxMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveListBoxMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveListBoxMethod "bindModel" o = ListBoxBindModelMethodInfo
    ResolveListBoxMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveListBoxMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveListBoxMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveListBoxMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveListBoxMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveListBoxMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveListBoxMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveListBoxMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveListBoxMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveListBoxMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveListBoxMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveListBoxMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveListBoxMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveListBoxMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveListBoxMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveListBoxMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveListBoxMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveListBoxMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveListBoxMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveListBoxMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveListBoxMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveListBoxMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveListBoxMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveListBoxMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveListBoxMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveListBoxMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveListBoxMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveListBoxMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveListBoxMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveListBoxMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveListBoxMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveListBoxMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveListBoxMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveListBoxMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveListBoxMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveListBoxMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveListBoxMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveListBoxMethod "dragHighlightRow" o = ListBoxDragHighlightRowMethodInfo
    ResolveListBoxMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveListBoxMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveListBoxMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveListBoxMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveListBoxMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveListBoxMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveListBoxMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveListBoxMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveListBoxMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveListBoxMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveListBoxMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveListBoxMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveListBoxMethod "dragUnhighlightRow" o = ListBoxDragUnhighlightRowMethodInfo
    ResolveListBoxMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveListBoxMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveListBoxMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveListBoxMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveListBoxMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveListBoxMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveListBoxMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveListBoxMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveListBoxMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveListBoxMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveListBoxMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveListBoxMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveListBoxMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveListBoxMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveListBoxMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveListBoxMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveListBoxMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveListBoxMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveListBoxMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveListBoxMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveListBoxMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveListBoxMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveListBoxMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveListBoxMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveListBoxMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveListBoxMethod "insert" o = ListBoxInsertMethodInfo
    ResolveListBoxMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveListBoxMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveListBoxMethod "invalidateFilter" o = ListBoxInvalidateFilterMethodInfo
    ResolveListBoxMethod "invalidateHeaders" o = ListBoxInvalidateHeadersMethodInfo
    ResolveListBoxMethod "invalidateSort" o = ListBoxInvalidateSortMethodInfo
    ResolveListBoxMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveListBoxMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveListBoxMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveListBoxMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveListBoxMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveListBoxMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveListBoxMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveListBoxMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveListBoxMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveListBoxMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveListBoxMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveListBoxMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveListBoxMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveListBoxMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveListBoxMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveListBoxMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveListBoxMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveListBoxMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveListBoxMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveListBoxMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveListBoxMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveListBoxMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveListBoxMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveListBoxMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveListBoxMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveListBoxMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveListBoxMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveListBoxMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveListBoxMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveListBoxMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveListBoxMethod "prepend" o = ListBoxPrependMethodInfo
    ResolveListBoxMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveListBoxMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveListBoxMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveListBoxMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveListBoxMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveListBoxMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveListBoxMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveListBoxMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveListBoxMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveListBoxMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveListBoxMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveListBoxMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveListBoxMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveListBoxMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolveListBoxMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveListBoxMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveListBoxMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveListBoxMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveListBoxMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveListBoxMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveListBoxMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveListBoxMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveListBoxMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveListBoxMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveListBoxMethod "selectAll" o = ListBoxSelectAllMethodInfo
    ResolveListBoxMethod "selectRow" o = ListBoxSelectRowMethodInfo
    ResolveListBoxMethod "selectedForeach" o = ListBoxSelectedForeachMethodInfo
    ResolveListBoxMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveListBoxMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveListBoxMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveListBoxMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveListBoxMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveListBoxMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveListBoxMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveListBoxMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveListBoxMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveListBoxMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveListBoxMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveListBoxMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveListBoxMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveListBoxMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveListBoxMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveListBoxMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveListBoxMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveListBoxMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveListBoxMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveListBoxMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveListBoxMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveListBoxMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveListBoxMethod "unselectAll" o = ListBoxUnselectAllMethodInfo
    ResolveListBoxMethod "unselectRow" o = ListBoxUnselectRowMethodInfo
    ResolveListBoxMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveListBoxMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveListBoxMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveListBoxMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveListBoxMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveListBoxMethod "getActivateOnSingleClick" o = ListBoxGetActivateOnSingleClickMethodInfo
    ResolveListBoxMethod "getAdjustment" o = ListBoxGetAdjustmentMethodInfo
    ResolveListBoxMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveListBoxMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveListBoxMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveListBoxMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveListBoxMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveListBoxMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveListBoxMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveListBoxMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveListBoxMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveListBoxMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveListBoxMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveListBoxMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveListBoxMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveListBoxMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveListBoxMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveListBoxMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveListBoxMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveListBoxMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveListBoxMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveListBoxMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveListBoxMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveListBoxMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveListBoxMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveListBoxMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveListBoxMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveListBoxMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveListBoxMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveListBoxMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveListBoxMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveListBoxMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveListBoxMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveListBoxMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveListBoxMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveListBoxMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveListBoxMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveListBoxMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveListBoxMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveListBoxMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveListBoxMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveListBoxMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveListBoxMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveListBoxMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveListBoxMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveListBoxMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveListBoxMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveListBoxMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveListBoxMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveListBoxMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveListBoxMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveListBoxMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveListBoxMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveListBoxMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveListBoxMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveListBoxMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveListBoxMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveListBoxMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveListBoxMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveListBoxMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveListBoxMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveListBoxMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveListBoxMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveListBoxMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveListBoxMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveListBoxMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveListBoxMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveListBoxMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveListBoxMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveListBoxMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveListBoxMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveListBoxMethod "getRowAtIndex" o = ListBoxGetRowAtIndexMethodInfo
    ResolveListBoxMethod "getRowAtY" o = ListBoxGetRowAtYMethodInfo
    ResolveListBoxMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveListBoxMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveListBoxMethod "getSelectedRow" o = ListBoxGetSelectedRowMethodInfo
    ResolveListBoxMethod "getSelectedRows" o = ListBoxGetSelectedRowsMethodInfo
    ResolveListBoxMethod "getSelectionMode" o = ListBoxGetSelectionModeMethodInfo
    ResolveListBoxMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveListBoxMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveListBoxMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveListBoxMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveListBoxMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveListBoxMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveListBoxMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveListBoxMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveListBoxMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveListBoxMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveListBoxMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveListBoxMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveListBoxMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveListBoxMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveListBoxMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveListBoxMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveListBoxMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveListBoxMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveListBoxMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveListBoxMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveListBoxMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveListBoxMethod "setActivateOnSingleClick" o = ListBoxSetActivateOnSingleClickMethodInfo
    ResolveListBoxMethod "setAdjustment" o = ListBoxSetAdjustmentMethodInfo
    ResolveListBoxMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveListBoxMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveListBoxMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveListBoxMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveListBoxMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveListBoxMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveListBoxMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveListBoxMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveListBoxMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveListBoxMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveListBoxMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveListBoxMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveListBoxMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveListBoxMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveListBoxMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveListBoxMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveListBoxMethod "setFilterFunc" o = ListBoxSetFilterFuncMethodInfo
    ResolveListBoxMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveListBoxMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveListBoxMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveListBoxMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveListBoxMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveListBoxMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveListBoxMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveListBoxMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveListBoxMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveListBoxMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveListBoxMethod "setHeaderFunc" o = ListBoxSetHeaderFuncMethodInfo
    ResolveListBoxMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveListBoxMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveListBoxMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveListBoxMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveListBoxMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveListBoxMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveListBoxMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveListBoxMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveListBoxMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveListBoxMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveListBoxMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveListBoxMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveListBoxMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveListBoxMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveListBoxMethod "setPlaceholder" o = ListBoxSetPlaceholderMethodInfo
    ResolveListBoxMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveListBoxMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveListBoxMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveListBoxMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveListBoxMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveListBoxMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveListBoxMethod "setSelectionMode" o = ListBoxSetSelectionModeMethodInfo
    ResolveListBoxMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveListBoxMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveListBoxMethod "setSortFunc" o = ListBoxSetSortFuncMethodInfo
    ResolveListBoxMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveListBoxMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveListBoxMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveListBoxMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveListBoxMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveListBoxMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveListBoxMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveListBoxMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveListBoxMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveListBoxMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveListBoxMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveListBoxMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveListBoxMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveListBoxMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveListBoxMethod t ListBox, O.OverloadedMethod info ListBox p) => OL.IsLabel t (ListBox -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveListBoxMethod t ListBox, O.OverloadedMethod info ListBox p, R.HasField t ListBox p) => R.HasField t ListBox p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveListBoxMethod t ListBox, O.OverloadedMethodInfo info ListBox) => OL.IsLabel t (O.MethodProxy info ListBox) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal ListBox::activate-cursor-row
-- | /No description available in the introspection data./
type ListBoxActivateCursorRowCallback =
    IO ()

type C_ListBoxActivateCursorRowCallback =
    Ptr ListBox ->                          -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ListBoxActivateCursorRowCallback`.
foreign import ccall "wrapper"
    mk_ListBoxActivateCursorRowCallback :: C_ListBoxActivateCursorRowCallback -> IO (FunPtr C_ListBoxActivateCursorRowCallback)

wrap_ListBoxActivateCursorRowCallback :: 
    GObject a => (a -> ListBoxActivateCursorRowCallback) ->
    C_ListBoxActivateCursorRowCallback
wrap_ListBoxActivateCursorRowCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [activateCursorRow](#signal:activateCursorRow) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' listBox #activateCursorRow callback
-- @
-- 
-- 
onListBoxActivateCursorRow :: (IsListBox a, MonadIO m) => a -> ((?self :: a) => ListBoxActivateCursorRowCallback) -> m SignalHandlerId
onListBoxActivateCursorRow obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ListBoxActivateCursorRowCallback wrapped
    wrapped'' <- mk_ListBoxActivateCursorRowCallback wrapped'
    connectSignalFunPtr obj "activate-cursor-row" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [activateCursorRow](#signal:activateCursorRow) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' listBox #activateCursorRow callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterListBoxActivateCursorRow :: (IsListBox a, MonadIO m) => a -> ((?self :: a) => ListBoxActivateCursorRowCallback) -> m SignalHandlerId
afterListBoxActivateCursorRow obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ListBoxActivateCursorRowCallback wrapped
    wrapped'' <- mk_ListBoxActivateCursorRowCallback wrapped'
    connectSignalFunPtr obj "activate-cursor-row" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ListBoxActivateCursorRowSignalInfo
instance SignalInfo ListBoxActivateCursorRowSignalInfo where
    type HaskellCallbackType ListBoxActivateCursorRowSignalInfo = ListBoxActivateCursorRowCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ListBoxActivateCursorRowCallback cb
        cb'' <- mk_ListBoxActivateCursorRowCallback cb'
        connectSignalFunPtr obj "activate-cursor-row" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox::activate-cursor-row"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#g:signal:activateCursorRow"})

#endif

-- signal ListBox::move-cursor
-- | /No description available in the introspection data./
type ListBoxMoveCursorCallback =
    Gtk.Enums.MovementStep
    -> Int32
    -> IO ()

type C_ListBoxMoveCursorCallback =
    Ptr ListBox ->                          -- object
    CUInt ->
    Int32 ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ListBoxMoveCursorCallback`.
foreign import ccall "wrapper"
    mk_ListBoxMoveCursorCallback :: C_ListBoxMoveCursorCallback -> IO (FunPtr C_ListBoxMoveCursorCallback)

wrap_ListBoxMoveCursorCallback :: 
    GObject a => (a -> ListBoxMoveCursorCallback) ->
    C_ListBoxMoveCursorCallback
wrap_ListBoxMoveCursorCallback gi'cb gi'selfPtr object p0 _ = do
    let object' = (toEnum . fromIntegral) object
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  object' p0


-- | Connect a signal handler for the [moveCursor](#signal:moveCursor) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' listBox #moveCursor callback
-- @
-- 
-- 
onListBoxMoveCursor :: (IsListBox a, MonadIO m) => a -> ((?self :: a) => ListBoxMoveCursorCallback) -> m SignalHandlerId
onListBoxMoveCursor obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ListBoxMoveCursorCallback wrapped
    wrapped'' <- mk_ListBoxMoveCursorCallback wrapped'
    connectSignalFunPtr obj "move-cursor" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [moveCursor](#signal:moveCursor) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' listBox #moveCursor callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterListBoxMoveCursor :: (IsListBox a, MonadIO m) => a -> ((?self :: a) => ListBoxMoveCursorCallback) -> m SignalHandlerId
afterListBoxMoveCursor obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ListBoxMoveCursorCallback wrapped
    wrapped'' <- mk_ListBoxMoveCursorCallback wrapped'
    connectSignalFunPtr obj "move-cursor" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ListBoxMoveCursorSignalInfo
instance SignalInfo ListBoxMoveCursorSignalInfo where
    type HaskellCallbackType ListBoxMoveCursorSignalInfo = ListBoxMoveCursorCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ListBoxMoveCursorCallback cb
        cb'' <- mk_ListBoxMoveCursorCallback cb'
        connectSignalFunPtr obj "move-cursor" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox::move-cursor"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#g:signal:moveCursor"})

#endif

-- signal ListBox::row-activated
-- | The [rowActivated](#g:signal:rowActivated) signal is emitted when a row has been activated by the user.
-- 
-- /Since: 3.10/
type ListBoxRowActivatedCallback =
    Gtk.ListBoxRow.ListBoxRow
    -- ^ /@row@/: the activated row
    -> IO ()

type C_ListBoxRowActivatedCallback =
    Ptr ListBox ->                          -- object
    Ptr Gtk.ListBoxRow.ListBoxRow ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ListBoxRowActivatedCallback`.
foreign import ccall "wrapper"
    mk_ListBoxRowActivatedCallback :: C_ListBoxRowActivatedCallback -> IO (FunPtr C_ListBoxRowActivatedCallback)

wrap_ListBoxRowActivatedCallback :: 
    GObject a => (a -> ListBoxRowActivatedCallback) ->
    C_ListBoxRowActivatedCallback
wrap_ListBoxRowActivatedCallback gi'cb gi'selfPtr row _ = do
    row' <- (newObject Gtk.ListBoxRow.ListBoxRow) row
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  row'


-- | Connect a signal handler for the [rowActivated](#signal:rowActivated) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' listBox #rowActivated callback
-- @
-- 
-- 
onListBoxRowActivated :: (IsListBox a, MonadIO m) => a -> ((?self :: a) => ListBoxRowActivatedCallback) -> m SignalHandlerId
onListBoxRowActivated obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ListBoxRowActivatedCallback wrapped
    wrapped'' <- mk_ListBoxRowActivatedCallback wrapped'
    connectSignalFunPtr obj "row-activated" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [rowActivated](#signal:rowActivated) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' listBox #rowActivated callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterListBoxRowActivated :: (IsListBox a, MonadIO m) => a -> ((?self :: a) => ListBoxRowActivatedCallback) -> m SignalHandlerId
afterListBoxRowActivated obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ListBoxRowActivatedCallback wrapped
    wrapped'' <- mk_ListBoxRowActivatedCallback wrapped'
    connectSignalFunPtr obj "row-activated" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ListBoxRowActivatedSignalInfo
instance SignalInfo ListBoxRowActivatedSignalInfo where
    type HaskellCallbackType ListBoxRowActivatedSignalInfo = ListBoxRowActivatedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ListBoxRowActivatedCallback cb
        cb'' <- mk_ListBoxRowActivatedCallback cb'
        connectSignalFunPtr obj "row-activated" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox::row-activated"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#g:signal:rowActivated"})

#endif

-- signal ListBox::row-selected
-- | The [rowSelected](#g:signal:rowSelected) signal is emitted when a new row is selected, or
-- (with a 'P.Nothing' /@row@/) when the selection is cleared.
-- 
-- When the /@box@/ is using @/GTK_SELECTION_MULTIPLE/@, this signal will not
-- give you the full picture of selection changes, and you should use
-- the [ListBox::selectedRowsChanged]("GI.Gtk.Objects.ListBox#g:signal:selectedRowsChanged") signal instead.
-- 
-- /Since: 3.10/
type ListBoxRowSelectedCallback =
    Maybe Gtk.ListBoxRow.ListBoxRow
    -- ^ /@row@/: the selected row
    -> IO ()

type C_ListBoxRowSelectedCallback =
    Ptr ListBox ->                          -- object
    Ptr Gtk.ListBoxRow.ListBoxRow ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ListBoxRowSelectedCallback`.
foreign import ccall "wrapper"
    mk_ListBoxRowSelectedCallback :: C_ListBoxRowSelectedCallback -> IO (FunPtr C_ListBoxRowSelectedCallback)

wrap_ListBoxRowSelectedCallback :: 
    GObject a => (a -> ListBoxRowSelectedCallback) ->
    C_ListBoxRowSelectedCallback
wrap_ListBoxRowSelectedCallback gi'cb gi'selfPtr row _ = do
    maybeRow <-
        if row == nullPtr
        then return Nothing
        else do
            row' <- (newObject Gtk.ListBoxRow.ListBoxRow) row
            return $ Just row'
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  maybeRow


-- | Connect a signal handler for the [rowSelected](#signal:rowSelected) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' listBox #rowSelected callback
-- @
-- 
-- 
onListBoxRowSelected :: (IsListBox a, MonadIO m) => a -> ((?self :: a) => ListBoxRowSelectedCallback) -> m SignalHandlerId
onListBoxRowSelected obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ListBoxRowSelectedCallback wrapped
    wrapped'' <- mk_ListBoxRowSelectedCallback wrapped'
    connectSignalFunPtr obj "row-selected" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [rowSelected](#signal:rowSelected) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' listBox #rowSelected callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterListBoxRowSelected :: (IsListBox a, MonadIO m) => a -> ((?self :: a) => ListBoxRowSelectedCallback) -> m SignalHandlerId
afterListBoxRowSelected obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ListBoxRowSelectedCallback wrapped
    wrapped'' <- mk_ListBoxRowSelectedCallback wrapped'
    connectSignalFunPtr obj "row-selected" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ListBoxRowSelectedSignalInfo
instance SignalInfo ListBoxRowSelectedSignalInfo where
    type HaskellCallbackType ListBoxRowSelectedSignalInfo = ListBoxRowSelectedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ListBoxRowSelectedCallback cb
        cb'' <- mk_ListBoxRowSelectedCallback cb'
        connectSignalFunPtr obj "row-selected" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox::row-selected"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#g:signal:rowSelected"})

#endif

-- signal ListBox::select-all
-- | The [selectAll](#g:signal:selectAll) signal is a [keybinding signal][GtkBindingSignal]
-- which gets emitted to select all children of the box, if the selection
-- mode permits it.
-- 
-- The default bindings for this signal is Ctrl-a.
-- 
-- /Since: 3.14/
type ListBoxSelectAllCallback =
    IO ()

type C_ListBoxSelectAllCallback =
    Ptr ListBox ->                          -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ListBoxSelectAllCallback`.
foreign import ccall "wrapper"
    mk_ListBoxSelectAllCallback :: C_ListBoxSelectAllCallback -> IO (FunPtr C_ListBoxSelectAllCallback)

wrap_ListBoxSelectAllCallback :: 
    GObject a => (a -> ListBoxSelectAllCallback) ->
    C_ListBoxSelectAllCallback
wrap_ListBoxSelectAllCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [selectAll](#signal:selectAll) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' listBox #selectAll callback
-- @
-- 
-- 
onListBoxSelectAll :: (IsListBox a, MonadIO m) => a -> ((?self :: a) => ListBoxSelectAllCallback) -> m SignalHandlerId
onListBoxSelectAll obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ListBoxSelectAllCallback wrapped
    wrapped'' <- mk_ListBoxSelectAllCallback wrapped'
    connectSignalFunPtr obj "select-all" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [selectAll](#signal:selectAll) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' listBox #selectAll callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterListBoxSelectAll :: (IsListBox a, MonadIO m) => a -> ((?self :: a) => ListBoxSelectAllCallback) -> m SignalHandlerId
afterListBoxSelectAll obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ListBoxSelectAllCallback wrapped
    wrapped'' <- mk_ListBoxSelectAllCallback wrapped'
    connectSignalFunPtr obj "select-all" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ListBoxSelectAllSignalInfo
instance SignalInfo ListBoxSelectAllSignalInfo where
    type HaskellCallbackType ListBoxSelectAllSignalInfo = ListBoxSelectAllCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ListBoxSelectAllCallback cb
        cb'' <- mk_ListBoxSelectAllCallback cb'
        connectSignalFunPtr obj "select-all" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox::select-all"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#g:signal:selectAll"})

#endif

-- signal ListBox::selected-rows-changed
-- | The [selectedRowsChanged](#g:signal:selectedRowsChanged) signal is emitted when the
-- set of selected rows changes.
-- 
-- /Since: 3.14/
type ListBoxSelectedRowsChangedCallback =
    IO ()

type C_ListBoxSelectedRowsChangedCallback =
    Ptr ListBox ->                          -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ListBoxSelectedRowsChangedCallback`.
foreign import ccall "wrapper"
    mk_ListBoxSelectedRowsChangedCallback :: C_ListBoxSelectedRowsChangedCallback -> IO (FunPtr C_ListBoxSelectedRowsChangedCallback)

wrap_ListBoxSelectedRowsChangedCallback :: 
    GObject a => (a -> ListBoxSelectedRowsChangedCallback) ->
    C_ListBoxSelectedRowsChangedCallback
wrap_ListBoxSelectedRowsChangedCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [selectedRowsChanged](#signal:selectedRowsChanged) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' listBox #selectedRowsChanged callback
-- @
-- 
-- 
onListBoxSelectedRowsChanged :: (IsListBox a, MonadIO m) => a -> ((?self :: a) => ListBoxSelectedRowsChangedCallback) -> m SignalHandlerId
onListBoxSelectedRowsChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ListBoxSelectedRowsChangedCallback wrapped
    wrapped'' <- mk_ListBoxSelectedRowsChangedCallback wrapped'
    connectSignalFunPtr obj "selected-rows-changed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [selectedRowsChanged](#signal:selectedRowsChanged) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' listBox #selectedRowsChanged callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterListBoxSelectedRowsChanged :: (IsListBox a, MonadIO m) => a -> ((?self :: a) => ListBoxSelectedRowsChangedCallback) -> m SignalHandlerId
afterListBoxSelectedRowsChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ListBoxSelectedRowsChangedCallback wrapped
    wrapped'' <- mk_ListBoxSelectedRowsChangedCallback wrapped'
    connectSignalFunPtr obj "selected-rows-changed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ListBoxSelectedRowsChangedSignalInfo
instance SignalInfo ListBoxSelectedRowsChangedSignalInfo where
    type HaskellCallbackType ListBoxSelectedRowsChangedSignalInfo = ListBoxSelectedRowsChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ListBoxSelectedRowsChangedCallback cb
        cb'' <- mk_ListBoxSelectedRowsChangedCallback cb'
        connectSignalFunPtr obj "selected-rows-changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox::selected-rows-changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#g:signal:selectedRowsChanged"})

#endif

-- signal ListBox::toggle-cursor-row
-- | /No description available in the introspection data./
type ListBoxToggleCursorRowCallback =
    IO ()

type C_ListBoxToggleCursorRowCallback =
    Ptr ListBox ->                          -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ListBoxToggleCursorRowCallback`.
foreign import ccall "wrapper"
    mk_ListBoxToggleCursorRowCallback :: C_ListBoxToggleCursorRowCallback -> IO (FunPtr C_ListBoxToggleCursorRowCallback)

wrap_ListBoxToggleCursorRowCallback :: 
    GObject a => (a -> ListBoxToggleCursorRowCallback) ->
    C_ListBoxToggleCursorRowCallback
wrap_ListBoxToggleCursorRowCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [toggleCursorRow](#signal:toggleCursorRow) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' listBox #toggleCursorRow callback
-- @
-- 
-- 
onListBoxToggleCursorRow :: (IsListBox a, MonadIO m) => a -> ((?self :: a) => ListBoxToggleCursorRowCallback) -> m SignalHandlerId
onListBoxToggleCursorRow obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ListBoxToggleCursorRowCallback wrapped
    wrapped'' <- mk_ListBoxToggleCursorRowCallback wrapped'
    connectSignalFunPtr obj "toggle-cursor-row" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [toggleCursorRow](#signal:toggleCursorRow) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' listBox #toggleCursorRow callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterListBoxToggleCursorRow :: (IsListBox a, MonadIO m) => a -> ((?self :: a) => ListBoxToggleCursorRowCallback) -> m SignalHandlerId
afterListBoxToggleCursorRow obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ListBoxToggleCursorRowCallback wrapped
    wrapped'' <- mk_ListBoxToggleCursorRowCallback wrapped'
    connectSignalFunPtr obj "toggle-cursor-row" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ListBoxToggleCursorRowSignalInfo
instance SignalInfo ListBoxToggleCursorRowSignalInfo where
    type HaskellCallbackType ListBoxToggleCursorRowSignalInfo = ListBoxToggleCursorRowCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ListBoxToggleCursorRowCallback cb
        cb'' <- mk_ListBoxToggleCursorRowCallback cb'
        connectSignalFunPtr obj "toggle-cursor-row" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox::toggle-cursor-row"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#g:signal:toggleCursorRow"})

#endif

-- signal ListBox::unselect-all
-- | The [unselectAll](#g:signal:unselectAll) signal is a [keybinding signal][GtkBindingSignal]
-- which gets emitted to unselect all children of the box, if the selection
-- mode permits it.
-- 
-- The default bindings for this signal is Ctrl-Shift-a.
-- 
-- /Since: 3.14/
type ListBoxUnselectAllCallback =
    IO ()

type C_ListBoxUnselectAllCallback =
    Ptr ListBox ->                          -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ListBoxUnselectAllCallback`.
foreign import ccall "wrapper"
    mk_ListBoxUnselectAllCallback :: C_ListBoxUnselectAllCallback -> IO (FunPtr C_ListBoxUnselectAllCallback)

wrap_ListBoxUnselectAllCallback :: 
    GObject a => (a -> ListBoxUnselectAllCallback) ->
    C_ListBoxUnselectAllCallback
wrap_ListBoxUnselectAllCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [unselectAll](#signal:unselectAll) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' listBox #unselectAll callback
-- @
-- 
-- 
onListBoxUnselectAll :: (IsListBox a, MonadIO m) => a -> ((?self :: a) => ListBoxUnselectAllCallback) -> m SignalHandlerId
onListBoxUnselectAll obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ListBoxUnselectAllCallback wrapped
    wrapped'' <- mk_ListBoxUnselectAllCallback wrapped'
    connectSignalFunPtr obj "unselect-all" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [unselectAll](#signal:unselectAll) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' listBox #unselectAll callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterListBoxUnselectAll :: (IsListBox a, MonadIO m) => a -> ((?self :: a) => ListBoxUnselectAllCallback) -> m SignalHandlerId
afterListBoxUnselectAll obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ListBoxUnselectAllCallback wrapped
    wrapped'' <- mk_ListBoxUnselectAllCallback wrapped'
    connectSignalFunPtr obj "unselect-all" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ListBoxUnselectAllSignalInfo
instance SignalInfo ListBoxUnselectAllSignalInfo where
    type HaskellCallbackType ListBoxUnselectAllSignalInfo = ListBoxUnselectAllCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ListBoxUnselectAllCallback cb
        cb'' <- mk_ListBoxUnselectAllCallback cb'
        connectSignalFunPtr obj "unselect-all" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox::unselect-all"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#g:signal:unselectAll"})

#endif

-- VVV Prop "activate-on-single-click"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@activate-on-single-click@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' listBox #activateOnSingleClick
-- @
getListBoxActivateOnSingleClick :: (MonadIO m, IsListBox o) => o -> m Bool
getListBoxActivateOnSingleClick obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "activate-on-single-click"

-- | Set the value of the “@activate-on-single-click@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' listBox [ #activateOnSingleClick 'Data.GI.Base.Attributes.:=' value ]
-- @
setListBoxActivateOnSingleClick :: (MonadIO m, IsListBox o) => o -> Bool -> m ()
setListBoxActivateOnSingleClick obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "activate-on-single-click" val

-- | Construct a `GValueConstruct` with valid value for the “@activate-on-single-click@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructListBoxActivateOnSingleClick :: (IsListBox o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructListBoxActivateOnSingleClick val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "activate-on-single-click" val

#if defined(ENABLE_OVERLOADING)
data ListBoxActivateOnSingleClickPropertyInfo
instance AttrInfo ListBoxActivateOnSingleClickPropertyInfo where
    type AttrAllowedOps ListBoxActivateOnSingleClickPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ListBoxActivateOnSingleClickPropertyInfo = IsListBox
    type AttrSetTypeConstraint ListBoxActivateOnSingleClickPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint ListBoxActivateOnSingleClickPropertyInfo = (~) Bool
    type AttrTransferType ListBoxActivateOnSingleClickPropertyInfo = Bool
    type AttrGetType ListBoxActivateOnSingleClickPropertyInfo = Bool
    type AttrLabel ListBoxActivateOnSingleClickPropertyInfo = "activate-on-single-click"
    type AttrOrigin ListBoxActivateOnSingleClickPropertyInfo = ListBox
    attrGet = getListBoxActivateOnSingleClick
    attrSet = setListBoxActivateOnSingleClick
    attrTransfer _ v = do
        return v
    attrConstruct = constructListBoxActivateOnSingleClick
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox.activateOnSingleClick"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#g:attr:activateOnSingleClick"
        })
#endif

-- VVV Prop "selection-mode"
   -- Type: TInterface (Name {namespace = "Gtk", name = "SelectionMode"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@selection-mode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' listBox #selectionMode
-- @
getListBoxSelectionMode :: (MonadIO m, IsListBox o) => o -> m Gtk.Enums.SelectionMode
getListBoxSelectionMode obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "selection-mode"

-- | Set the value of the “@selection-mode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' listBox [ #selectionMode 'Data.GI.Base.Attributes.:=' value ]
-- @
setListBoxSelectionMode :: (MonadIO m, IsListBox o) => o -> Gtk.Enums.SelectionMode -> m ()
setListBoxSelectionMode obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "selection-mode" val

-- | Construct a `GValueConstruct` with valid value for the “@selection-mode@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructListBoxSelectionMode :: (IsListBox o, MIO.MonadIO m) => Gtk.Enums.SelectionMode -> m (GValueConstruct o)
constructListBoxSelectionMode val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "selection-mode" val

#if defined(ENABLE_OVERLOADING)
data ListBoxSelectionModePropertyInfo
instance AttrInfo ListBoxSelectionModePropertyInfo where
    type AttrAllowedOps ListBoxSelectionModePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ListBoxSelectionModePropertyInfo = IsListBox
    type AttrSetTypeConstraint ListBoxSelectionModePropertyInfo = (~) Gtk.Enums.SelectionMode
    type AttrTransferTypeConstraint ListBoxSelectionModePropertyInfo = (~) Gtk.Enums.SelectionMode
    type AttrTransferType ListBoxSelectionModePropertyInfo = Gtk.Enums.SelectionMode
    type AttrGetType ListBoxSelectionModePropertyInfo = Gtk.Enums.SelectionMode
    type AttrLabel ListBoxSelectionModePropertyInfo = "selection-mode"
    type AttrOrigin ListBoxSelectionModePropertyInfo = ListBox
    attrGet = getListBoxSelectionMode
    attrSet = setListBoxSelectionMode
    attrTransfer _ v = do
        return v
    attrConstruct = constructListBoxSelectionMode
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox.selectionMode"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#g:attr:selectionMode"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList ListBox
type instance O.AttributeList ListBox = ListBoxAttributeList
type ListBoxAttributeList = ('[ '("activateOnSingleClick", ListBoxActivateOnSingleClickPropertyInfo), '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("selectionMode", ListBoxSelectionModePropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
listBoxActivateOnSingleClick :: AttrLabelProxy "activateOnSingleClick"
listBoxActivateOnSingleClick = AttrLabelProxy

listBoxSelectionMode :: AttrLabelProxy "selectionMode"
listBoxSelectionMode = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList ListBox = ListBoxSignalList
type ListBoxSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("activateCursorRow", ListBoxActivateCursorRowSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveCursor", ListBoxMoveCursorSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("rowActivated", ListBoxRowActivatedSignalInfo), '("rowSelected", ListBoxRowSelectedSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectAll", ListBoxSelectAllSignalInfo), '("selectedRowsChanged", ListBoxSelectedRowsChangedSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("toggleCursorRow", ListBoxToggleCursorRowSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("unselectAll", ListBoxUnselectAllSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method ListBox::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "ListBox" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_list_box_new" gtk_list_box_new :: 
    IO (Ptr ListBox)

-- | Creates a new t'GI.Gtk.Objects.ListBox.ListBox' container.
-- 
-- /Since: 3.10/
listBoxNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m ListBox
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.ListBox.ListBox'
listBoxNew  = liftIO $ do
    result <- gtk_list_box_new
    checkUnexpectedReturnNULL "listBoxNew" result
    result' <- (newObject ListBox) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method ListBox::bind_model
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkListBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "model"
--           , argType =
--               TInterface Name { namespace = "Gio" , name = "ListModel" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GListModel to be bound to @box"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "create_widget_func"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "ListBoxCreateWidgetFunc" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a function that creates widgets for items\n  or %NULL in case you also passed %NULL as @model"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeNotified
--           , argClosure = 3
--           , argDestroy = 4
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
--                 { rawDocText = Just "user data passed to @create_widget_func"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "user_data_free_func"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "DestroyNotify" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "function for freeing @user_data"
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

foreign import ccall "gtk_list_box_bind_model" gtk_list_box_bind_model :: 
    Ptr ListBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "ListBox"})
    Ptr Gio.ListModel.ListModel ->          -- model : TInterface (Name {namespace = "Gio", name = "ListModel"})
    FunPtr Gtk.Callbacks.C_ListBoxCreateWidgetFunc -> -- create_widget_func : TInterface (Name {namespace = "Gtk", name = "ListBoxCreateWidgetFunc"})
    Ptr () ->                               -- user_data : TBasicType TPtr
    FunPtr GLib.Callbacks.C_DestroyNotify -> -- user_data_free_func : TInterface (Name {namespace = "GLib", name = "DestroyNotify"})
    IO ()

-- | Binds /@model@/ to /@box@/.
-- 
-- If /@box@/ was already bound to a model, that previous binding is
-- destroyed.
-- 
-- The contents of /@box@/ are cleared and then filled with widgets that
-- represent items from /@model@/. /@box@/ is updated whenever /@model@/ changes.
-- If /@model@/ is 'P.Nothing', /@box@/ is left empty.
-- 
-- It is undefined to add or remove widgets directly (for example, with
-- 'GI.Gtk.Objects.ListBox.listBoxInsert' or 'GI.Gtk.Objects.Container.containerAdd') while /@box@/ is bound to a
-- model.
-- 
-- Note that using a model is incompatible with the filtering and sorting
-- functionality in GtkListBox. When using a model, filtering and sorting
-- should be implemented by the model.
-- 
-- /Since: 3.16/
listBoxBindModel ::
    (B.CallStack.HasCallStack, MonadIO m, IsListBox a, Gio.ListModel.IsListModel b) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.ListBox.ListBox'
    -> Maybe (b)
    -- ^ /@model@/: the t'GI.Gio.Interfaces.ListModel.ListModel' to be bound to /@box@/
    -> Maybe (Gtk.Callbacks.ListBoxCreateWidgetFunc)
    -- ^ /@createWidgetFunc@/: a function that creates widgets for items
    --   or 'P.Nothing' in case you also passed 'P.Nothing' as /@model@/
    -> m ()
listBoxBindModel box model createWidgetFunc = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    maybeModel <- case model of
        Nothing -> return nullPtr
        Just jModel -> do
            jModel' <- unsafeManagedPtrCastPtr jModel
            return jModel'
    maybeCreateWidgetFunc <- case createWidgetFunc of
        Nothing -> return (castPtrToFunPtr nullPtr)
        Just jCreateWidgetFunc -> do
            jCreateWidgetFunc' <- Gtk.Callbacks.mk_ListBoxCreateWidgetFunc (Gtk.Callbacks.wrap_ListBoxCreateWidgetFunc Nothing (Gtk.Callbacks.drop_closures_ListBoxCreateWidgetFunc jCreateWidgetFunc))
            return jCreateWidgetFunc'
    let userData = castFunPtrToPtr maybeCreateWidgetFunc
    let userDataFreeFunc = SP.safeFreeFunPtrPtr
    gtk_list_box_bind_model box' maybeModel maybeCreateWidgetFunc userData userDataFreeFunc
    touchManagedPtr box
    whenJust model touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data ListBoxBindModelMethodInfo
instance (signature ~ (Maybe (b) -> Maybe (Gtk.Callbacks.ListBoxCreateWidgetFunc) -> m ()), MonadIO m, IsListBox a, Gio.ListModel.IsListModel b) => O.OverloadedMethod ListBoxBindModelMethodInfo a signature where
    overloadedMethod = listBoxBindModel

instance O.OverloadedMethodInfo ListBoxBindModelMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox.listBoxBindModel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#v:listBoxBindModel"
        })


#endif

-- method ListBox::drag_highlight_row
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkListBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "row"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListBoxRow" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkListBoxRow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_list_box_drag_highlight_row" gtk_list_box_drag_highlight_row :: 
    Ptr ListBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "ListBox"})
    Ptr Gtk.ListBoxRow.ListBoxRow ->        -- row : TInterface (Name {namespace = "Gtk", name = "ListBoxRow"})
    IO ()

-- | This is a helper function for implementing DnD onto a t'GI.Gtk.Objects.ListBox.ListBox'.
-- The passed in /@row@/ will be highlighted via 'GI.Gtk.Objects.Widget.widgetDragHighlight',
-- and any previously highlighted row will be unhighlighted.
-- 
-- The row will also be unhighlighted when the widget gets
-- a drag leave event.
-- 
-- /Since: 3.10/
listBoxDragHighlightRow ::
    (B.CallStack.HasCallStack, MonadIO m, IsListBox a, Gtk.ListBoxRow.IsListBoxRow b) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.ListBox.ListBox'
    -> b
    -- ^ /@row@/: a t'GI.Gtk.Objects.ListBoxRow.ListBoxRow'
    -> m ()
listBoxDragHighlightRow box row = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    row' <- unsafeManagedPtrCastPtr row
    gtk_list_box_drag_highlight_row box' row'
    touchManagedPtr box
    touchManagedPtr row
    return ()

#if defined(ENABLE_OVERLOADING)
data ListBoxDragHighlightRowMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsListBox a, Gtk.ListBoxRow.IsListBoxRow b) => O.OverloadedMethod ListBoxDragHighlightRowMethodInfo a signature where
    overloadedMethod = listBoxDragHighlightRow

instance O.OverloadedMethodInfo ListBoxDragHighlightRowMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox.listBoxDragHighlightRow",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#v:listBoxDragHighlightRow"
        })


#endif

-- method ListBox::drag_unhighlight_row
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkListBox" , sinceVersion = Nothing }
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

foreign import ccall "gtk_list_box_drag_unhighlight_row" gtk_list_box_drag_unhighlight_row :: 
    Ptr ListBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "ListBox"})
    IO ()

-- | If a row has previously been highlighted via 'GI.Gtk.Objects.ListBox.listBoxDragHighlightRow'
-- it will have the highlight removed.
-- 
-- /Since: 3.10/
listBoxDragUnhighlightRow ::
    (B.CallStack.HasCallStack, MonadIO m, IsListBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.ListBox.ListBox'
    -> m ()
listBoxDragUnhighlightRow box = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    gtk_list_box_drag_unhighlight_row box'
    touchManagedPtr box
    return ()

#if defined(ENABLE_OVERLOADING)
data ListBoxDragUnhighlightRowMethodInfo
instance (signature ~ (m ()), MonadIO m, IsListBox a) => O.OverloadedMethod ListBoxDragUnhighlightRowMethodInfo a signature where
    overloadedMethod = listBoxDragUnhighlightRow

instance O.OverloadedMethodInfo ListBoxDragUnhighlightRowMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox.listBoxDragUnhighlightRow",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#v:listBoxDragUnhighlightRow"
        })


#endif

-- method ListBox::get_activate_on_single_click
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkListBox" , sinceVersion = Nothing }
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

foreign import ccall "gtk_list_box_get_activate_on_single_click" gtk_list_box_get_activate_on_single_click :: 
    Ptr ListBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "ListBox"})
    IO CInt

-- | Returns whether rows activate on single clicks.
-- 
-- /Since: 3.10/
listBoxGetActivateOnSingleClick ::
    (B.CallStack.HasCallStack, MonadIO m, IsListBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.ListBox.ListBox'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if rows are activated on single click, 'P.False' otherwise
listBoxGetActivateOnSingleClick box = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    result <- gtk_list_box_get_activate_on_single_click box'
    let result' = (/= 0) result
    touchManagedPtr box
    return result'

#if defined(ENABLE_OVERLOADING)
data ListBoxGetActivateOnSingleClickMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsListBox a) => O.OverloadedMethod ListBoxGetActivateOnSingleClickMethodInfo a signature where
    overloadedMethod = listBoxGetActivateOnSingleClick

instance O.OverloadedMethodInfo ListBoxGetActivateOnSingleClickMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox.listBoxGetActivateOnSingleClick",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#v:listBoxGetActivateOnSingleClick"
        })


#endif

-- method ListBox::get_adjustment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkListBox" , sinceVersion = Nothing }
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

foreign import ccall "gtk_list_box_get_adjustment" gtk_list_box_get_adjustment :: 
    Ptr ListBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "ListBox"})
    IO (Ptr Gtk.Adjustment.Adjustment)

-- | Gets the adjustment (if any) that the widget uses to
-- for vertical scrolling.
-- 
-- /Since: 3.10/
listBoxGetAdjustment ::
    (B.CallStack.HasCallStack, MonadIO m, IsListBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.ListBox.ListBox'
    -> m Gtk.Adjustment.Adjustment
    -- ^ __Returns:__ the adjustment
listBoxGetAdjustment box = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    result <- gtk_list_box_get_adjustment box'
    checkUnexpectedReturnNULL "listBoxGetAdjustment" result
    result' <- (newObject Gtk.Adjustment.Adjustment) result
    touchManagedPtr box
    return result'

#if defined(ENABLE_OVERLOADING)
data ListBoxGetAdjustmentMethodInfo
instance (signature ~ (m Gtk.Adjustment.Adjustment), MonadIO m, IsListBox a) => O.OverloadedMethod ListBoxGetAdjustmentMethodInfo a signature where
    overloadedMethod = listBoxGetAdjustment

instance O.OverloadedMethodInfo ListBoxGetAdjustmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox.listBoxGetAdjustment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#v:listBoxGetAdjustment"
        })


#endif

-- method ListBox::get_row_at_index
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkListBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "index_"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the index of the row"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "ListBoxRow" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_list_box_get_row_at_index" gtk_list_box_get_row_at_index :: 
    Ptr ListBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "ListBox"})
    Int32 ->                                -- index_ : TBasicType TInt
    IO (Ptr Gtk.ListBoxRow.ListBoxRow)

-- | Gets the n-th child in the list (not counting headers).
-- If /@index_@/ is negative or larger than the number of items in the
-- list, 'P.Nothing' is returned.
-- 
-- /Since: 3.10/
listBoxGetRowAtIndex ::
    (B.CallStack.HasCallStack, MonadIO m, IsListBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.ListBox.ListBox'
    -> Int32
    -- ^ /@index_@/: the index of the row
    -> m (Maybe Gtk.ListBoxRow.ListBoxRow)
    -- ^ __Returns:__ the child t'GI.Gtk.Objects.Widget.Widget' or 'P.Nothing'
listBoxGetRowAtIndex box index_ = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    result <- gtk_list_box_get_row_at_index box' index_
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.ListBoxRow.ListBoxRow) result'
        return result''
    touchManagedPtr box
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data ListBoxGetRowAtIndexMethodInfo
instance (signature ~ (Int32 -> m (Maybe Gtk.ListBoxRow.ListBoxRow)), MonadIO m, IsListBox a) => O.OverloadedMethod ListBoxGetRowAtIndexMethodInfo a signature where
    overloadedMethod = listBoxGetRowAtIndex

instance O.OverloadedMethodInfo ListBoxGetRowAtIndexMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox.listBoxGetRowAtIndex",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#v:listBoxGetRowAtIndex"
        })


#endif

-- method ListBox::get_row_at_y
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkListBox" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "position" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "ListBoxRow" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_list_box_get_row_at_y" gtk_list_box_get_row_at_y :: 
    Ptr ListBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "ListBox"})
    Int32 ->                                -- y : TBasicType TInt
    IO (Ptr Gtk.ListBoxRow.ListBoxRow)

-- | Gets the row at the /@y@/ position.
-- 
-- /Since: 3.10/
listBoxGetRowAtY ::
    (B.CallStack.HasCallStack, MonadIO m, IsListBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.ListBox.ListBox'
    -> Int32
    -- ^ /@y@/: position
    -> m (Maybe Gtk.ListBoxRow.ListBoxRow)
    -- ^ __Returns:__ the row or 'P.Nothing'
    --   in case no row exists for the given y coordinate.
listBoxGetRowAtY box y = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    result <- gtk_list_box_get_row_at_y box' y
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.ListBoxRow.ListBoxRow) result'
        return result''
    touchManagedPtr box
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data ListBoxGetRowAtYMethodInfo
instance (signature ~ (Int32 -> m (Maybe Gtk.ListBoxRow.ListBoxRow)), MonadIO m, IsListBox a) => O.OverloadedMethod ListBoxGetRowAtYMethodInfo a signature where
    overloadedMethod = listBoxGetRowAtY

instance O.OverloadedMethodInfo ListBoxGetRowAtYMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox.listBoxGetRowAtY",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#v:listBoxGetRowAtY"
        })


#endif

-- method ListBox::get_selected_row
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkListBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "ListBoxRow" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_list_box_get_selected_row" gtk_list_box_get_selected_row :: 
    Ptr ListBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "ListBox"})
    IO (Ptr Gtk.ListBoxRow.ListBoxRow)

-- | Gets the selected row.
-- 
-- Note that the box may allow multiple selection, in which
-- case you should use 'GI.Gtk.Objects.ListBox.listBoxSelectedForeach' to
-- find all selected rows.
-- 
-- /Since: 3.10/
listBoxGetSelectedRow ::
    (B.CallStack.HasCallStack, MonadIO m, IsListBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.ListBox.ListBox'
    -> m Gtk.ListBoxRow.ListBoxRow
    -- ^ __Returns:__ the selected row
listBoxGetSelectedRow box = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    result <- gtk_list_box_get_selected_row box'
    checkUnexpectedReturnNULL "listBoxGetSelectedRow" result
    result' <- (newObject Gtk.ListBoxRow.ListBoxRow) result
    touchManagedPtr box
    return result'

#if defined(ENABLE_OVERLOADING)
data ListBoxGetSelectedRowMethodInfo
instance (signature ~ (m Gtk.ListBoxRow.ListBoxRow), MonadIO m, IsListBox a) => O.OverloadedMethod ListBoxGetSelectedRowMethodInfo a signature where
    overloadedMethod = listBoxGetSelectedRow

instance O.OverloadedMethodInfo ListBoxGetSelectedRowMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox.listBoxGetSelectedRow",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#v:listBoxGetSelectedRow"
        })


#endif

-- method ListBox::get_selected_rows
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkListBox" , sinceVersion = Nothing }
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
--                  (TInterface Name { namespace = "Gtk" , name = "ListBoxRow" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_list_box_get_selected_rows" gtk_list_box_get_selected_rows :: 
    Ptr ListBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "ListBox"})
    IO (Ptr (GList (Ptr Gtk.ListBoxRow.ListBoxRow)))

-- | Creates a list of all selected children.
-- 
-- /Since: 3.14/
listBoxGetSelectedRows ::
    (B.CallStack.HasCallStack, MonadIO m, IsListBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.ListBox.ListBox'
    -> m [Gtk.ListBoxRow.ListBoxRow]
    -- ^ __Returns:__ 
    --     A t'GI.GLib.Structs.List.List' containing the t'GI.Gtk.Objects.Widget.Widget' for each selected child.
    --     Free with @/g_list_free()/@ when done.
listBoxGetSelectedRows box = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    result <- gtk_list_box_get_selected_rows box'
    result' <- unpackGList result
    result'' <- mapM (newObject Gtk.ListBoxRow.ListBoxRow) result'
    g_list_free result
    touchManagedPtr box
    return result''

#if defined(ENABLE_OVERLOADING)
data ListBoxGetSelectedRowsMethodInfo
instance (signature ~ (m [Gtk.ListBoxRow.ListBoxRow]), MonadIO m, IsListBox a) => O.OverloadedMethod ListBoxGetSelectedRowsMethodInfo a signature where
    overloadedMethod = listBoxGetSelectedRows

instance O.OverloadedMethodInfo ListBoxGetSelectedRowsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox.listBoxGetSelectedRows",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#v:listBoxGetSelectedRows"
        })


#endif

-- method ListBox::get_selection_mode
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkListBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "SelectionMode" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_list_box_get_selection_mode" gtk_list_box_get_selection_mode :: 
    Ptr ListBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "ListBox"})
    IO CUInt

-- | Gets the selection mode of the listbox.
-- 
-- /Since: 3.10/
listBoxGetSelectionMode ::
    (B.CallStack.HasCallStack, MonadIO m, IsListBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.ListBox.ListBox'
    -> m Gtk.Enums.SelectionMode
    -- ^ __Returns:__ a t'GI.Gtk.Enums.SelectionMode'
listBoxGetSelectionMode box = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    result <- gtk_list_box_get_selection_mode box'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr box
    return result'

#if defined(ENABLE_OVERLOADING)
data ListBoxGetSelectionModeMethodInfo
instance (signature ~ (m Gtk.Enums.SelectionMode), MonadIO m, IsListBox a) => O.OverloadedMethod ListBoxGetSelectionModeMethodInfo a signature where
    overloadedMethod = listBoxGetSelectionMode

instance O.OverloadedMethodInfo ListBoxGetSelectionModeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox.listBoxGetSelectionMode",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#v:listBoxGetSelectionMode"
        })


#endif

-- method ListBox::insert
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkListBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "child"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkWidget to add"
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
--                 { rawDocText = Just "the position to insert @child in"
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

foreign import ccall "gtk_list_box_insert" gtk_list_box_insert :: 
    Ptr ListBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "ListBox"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Int32 ->                                -- position : TBasicType TInt
    IO ()

-- | Insert the /@child@/ into the /@box@/ at /@position@/. If a sort function is
-- set, the widget will actually be inserted at the calculated position and
-- this function has the same effect of 'GI.Gtk.Objects.Container.containerAdd'.
-- 
-- If /@position@/ is -1, or larger than the total number of items in the
-- /@box@/, then the /@child@/ will be appended to the end.
-- 
-- /Since: 3.10/
listBoxInsert ::
    (B.CallStack.HasCallStack, MonadIO m, IsListBox a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.ListBox.ListBox'
    -> b
    -- ^ /@child@/: the t'GI.Gtk.Objects.Widget.Widget' to add
    -> Int32
    -- ^ /@position@/: the position to insert /@child@/ in
    -> m ()
listBoxInsert box child position = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    child' <- unsafeManagedPtrCastPtr child
    gtk_list_box_insert box' child' position
    touchManagedPtr box
    touchManagedPtr child
    return ()

#if defined(ENABLE_OVERLOADING)
data ListBoxInsertMethodInfo
instance (signature ~ (b -> Int32 -> m ()), MonadIO m, IsListBox a, Gtk.Widget.IsWidget b) => O.OverloadedMethod ListBoxInsertMethodInfo a signature where
    overloadedMethod = listBoxInsert

instance O.OverloadedMethodInfo ListBoxInsertMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox.listBoxInsert",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#v:listBoxInsert"
        })


#endif

-- method ListBox::invalidate_filter
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkListBox" , sinceVersion = Nothing }
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

foreign import ccall "gtk_list_box_invalidate_filter" gtk_list_box_invalidate_filter :: 
    Ptr ListBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "ListBox"})
    IO ()

-- | Update the filtering for all rows. Call this when result
-- of the filter function on the /@box@/ is changed due
-- to an external factor. For instance, this would be used
-- if the filter function just looked for a specific search
-- string and the entry with the search string has changed.
-- 
-- /Since: 3.10/
listBoxInvalidateFilter ::
    (B.CallStack.HasCallStack, MonadIO m, IsListBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.ListBox.ListBox'
    -> m ()
listBoxInvalidateFilter box = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    gtk_list_box_invalidate_filter box'
    touchManagedPtr box
    return ()

#if defined(ENABLE_OVERLOADING)
data ListBoxInvalidateFilterMethodInfo
instance (signature ~ (m ()), MonadIO m, IsListBox a) => O.OverloadedMethod ListBoxInvalidateFilterMethodInfo a signature where
    overloadedMethod = listBoxInvalidateFilter

instance O.OverloadedMethodInfo ListBoxInvalidateFilterMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox.listBoxInvalidateFilter",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#v:listBoxInvalidateFilter"
        })


#endif

-- method ListBox::invalidate_headers
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkListBox" , sinceVersion = Nothing }
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

foreign import ccall "gtk_list_box_invalidate_headers" gtk_list_box_invalidate_headers :: 
    Ptr ListBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "ListBox"})
    IO ()

-- | Update the separators for all rows. Call this when result
-- of the header function on the /@box@/ is changed due
-- to an external factor.
-- 
-- /Since: 3.10/
listBoxInvalidateHeaders ::
    (B.CallStack.HasCallStack, MonadIO m, IsListBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.ListBox.ListBox'
    -> m ()
listBoxInvalidateHeaders box = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    gtk_list_box_invalidate_headers box'
    touchManagedPtr box
    return ()

#if defined(ENABLE_OVERLOADING)
data ListBoxInvalidateHeadersMethodInfo
instance (signature ~ (m ()), MonadIO m, IsListBox a) => O.OverloadedMethod ListBoxInvalidateHeadersMethodInfo a signature where
    overloadedMethod = listBoxInvalidateHeaders

instance O.OverloadedMethodInfo ListBoxInvalidateHeadersMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox.listBoxInvalidateHeaders",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#v:listBoxInvalidateHeaders"
        })


#endif

-- method ListBox::invalidate_sort
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkListBox" , sinceVersion = Nothing }
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

foreign import ccall "gtk_list_box_invalidate_sort" gtk_list_box_invalidate_sort :: 
    Ptr ListBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "ListBox"})
    IO ()

-- | Update the sorting for all rows. Call this when result
-- of the sort function on the /@box@/ is changed due
-- to an external factor.
-- 
-- /Since: 3.10/
listBoxInvalidateSort ::
    (B.CallStack.HasCallStack, MonadIO m, IsListBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.ListBox.ListBox'
    -> m ()
listBoxInvalidateSort box = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    gtk_list_box_invalidate_sort box'
    touchManagedPtr box
    return ()

#if defined(ENABLE_OVERLOADING)
data ListBoxInvalidateSortMethodInfo
instance (signature ~ (m ()), MonadIO m, IsListBox a) => O.OverloadedMethod ListBoxInvalidateSortMethodInfo a signature where
    overloadedMethod = listBoxInvalidateSort

instance O.OverloadedMethodInfo ListBoxInvalidateSortMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox.listBoxInvalidateSort",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#v:listBoxInvalidateSort"
        })


#endif

-- method ListBox::prepend
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkListBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "child"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkWidget to add"
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

foreign import ccall "gtk_list_box_prepend" gtk_list_box_prepend :: 
    Ptr ListBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "ListBox"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | Prepend a widget to the list. If a sort function is set, the widget will
-- actually be inserted at the calculated position and this function has the
-- same effect of 'GI.Gtk.Objects.Container.containerAdd'.
-- 
-- /Since: 3.10/
listBoxPrepend ::
    (B.CallStack.HasCallStack, MonadIO m, IsListBox a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.ListBox.ListBox'
    -> b
    -- ^ /@child@/: the t'GI.Gtk.Objects.Widget.Widget' to add
    -> m ()
listBoxPrepend box child = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    child' <- unsafeManagedPtrCastPtr child
    gtk_list_box_prepend box' child'
    touchManagedPtr box
    touchManagedPtr child
    return ()

#if defined(ENABLE_OVERLOADING)
data ListBoxPrependMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsListBox a, Gtk.Widget.IsWidget b) => O.OverloadedMethod ListBoxPrependMethodInfo a signature where
    overloadedMethod = listBoxPrepend

instance O.OverloadedMethodInfo ListBoxPrependMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox.listBoxPrepend",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#v:listBoxPrepend"
        })


#endif

-- method ListBox::select_all
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkListBox" , sinceVersion = Nothing }
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

foreign import ccall "gtk_list_box_select_all" gtk_list_box_select_all :: 
    Ptr ListBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "ListBox"})
    IO ()

-- | Select all children of /@box@/, if the selection mode allows it.
-- 
-- /Since: 3.14/
listBoxSelectAll ::
    (B.CallStack.HasCallStack, MonadIO m, IsListBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.ListBox.ListBox'
    -> m ()
listBoxSelectAll box = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    gtk_list_box_select_all box'
    touchManagedPtr box
    return ()

#if defined(ENABLE_OVERLOADING)
data ListBoxSelectAllMethodInfo
instance (signature ~ (m ()), MonadIO m, IsListBox a) => O.OverloadedMethod ListBoxSelectAllMethodInfo a signature where
    overloadedMethod = listBoxSelectAll

instance O.OverloadedMethodInfo ListBoxSelectAllMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox.listBoxSelectAll",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#v:listBoxSelectAll"
        })


#endif

-- method ListBox::select_row
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkListBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "row"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListBoxRow" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The row to select or %NULL"
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

foreign import ccall "gtk_list_box_select_row" gtk_list_box_select_row :: 
    Ptr ListBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "ListBox"})
    Ptr Gtk.ListBoxRow.ListBoxRow ->        -- row : TInterface (Name {namespace = "Gtk", name = "ListBoxRow"})
    IO ()

-- | Make /@row@/ the currently selected row.
-- 
-- /Since: 3.10/
listBoxSelectRow ::
    (B.CallStack.HasCallStack, MonadIO m, IsListBox a, Gtk.ListBoxRow.IsListBoxRow b) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.ListBox.ListBox'
    -> Maybe (b)
    -- ^ /@row@/: The row to select or 'P.Nothing'
    -> m ()
listBoxSelectRow box row = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    maybeRow <- case row of
        Nothing -> return nullPtr
        Just jRow -> do
            jRow' <- unsafeManagedPtrCastPtr jRow
            return jRow'
    gtk_list_box_select_row box' maybeRow
    touchManagedPtr box
    whenJust row touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data ListBoxSelectRowMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsListBox a, Gtk.ListBoxRow.IsListBoxRow b) => O.OverloadedMethod ListBoxSelectRowMethodInfo a signature where
    overloadedMethod = listBoxSelectRow

instance O.OverloadedMethodInfo ListBoxSelectRowMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox.listBoxSelectRow",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#v:listBoxSelectRow"
        })


#endif

-- method ListBox::selected_foreach
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkListBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "func"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListBoxForeachFunc" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the function to call for each selected child"
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
--                 { rawDocText = Just "user data to pass to the function"
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

foreign import ccall "gtk_list_box_selected_foreach" gtk_list_box_selected_foreach :: 
    Ptr ListBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "ListBox"})
    FunPtr Gtk.Callbacks.C_ListBoxForeachFunc -> -- func : TInterface (Name {namespace = "Gtk", name = "ListBoxForeachFunc"})
    Ptr () ->                               -- data : TBasicType TPtr
    IO ()

-- | Calls a function for each selected child.
-- 
-- Note that the selection cannot be modified from within this function.
-- 
-- /Since: 3.14/
listBoxSelectedForeach ::
    (B.CallStack.HasCallStack, MonadIO m, IsListBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.ListBox.ListBox'
    -> Gtk.Callbacks.ListBoxForeachFunc
    -- ^ /@func@/: the function to call for each selected child
    -> m ()
listBoxSelectedForeach box func = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    func' <- Gtk.Callbacks.mk_ListBoxForeachFunc (Gtk.Callbacks.wrap_ListBoxForeachFunc Nothing (Gtk.Callbacks.drop_closures_ListBoxForeachFunc func))
    let data_ = nullPtr
    gtk_list_box_selected_foreach box' func' data_
    safeFreeFunPtr $ castFunPtrToPtr func'
    touchManagedPtr box
    return ()

#if defined(ENABLE_OVERLOADING)
data ListBoxSelectedForeachMethodInfo
instance (signature ~ (Gtk.Callbacks.ListBoxForeachFunc -> m ()), MonadIO m, IsListBox a) => O.OverloadedMethod ListBoxSelectedForeachMethodInfo a signature where
    overloadedMethod = listBoxSelectedForeach

instance O.OverloadedMethodInfo ListBoxSelectedForeachMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox.listBoxSelectedForeach",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#v:listBoxSelectedForeach"
        })


#endif

-- method ListBox::set_activate_on_single_click
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkListBox" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a boolean" , sinceVersion = Nothing }
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

foreign import ccall "gtk_list_box_set_activate_on_single_click" gtk_list_box_set_activate_on_single_click :: 
    Ptr ListBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "ListBox"})
    CInt ->                                 -- single : TBasicType TBoolean
    IO ()

-- | If /@single@/ is 'P.True', rows will be activated when you click on them,
-- otherwise you need to double-click.
-- 
-- /Since: 3.10/
listBoxSetActivateOnSingleClick ::
    (B.CallStack.HasCallStack, MonadIO m, IsListBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.ListBox.ListBox'
    -> Bool
    -- ^ /@single@/: a boolean
    -> m ()
listBoxSetActivateOnSingleClick box single = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    let single' = (fromIntegral . fromEnum) single
    gtk_list_box_set_activate_on_single_click box' single'
    touchManagedPtr box
    return ()

#if defined(ENABLE_OVERLOADING)
data ListBoxSetActivateOnSingleClickMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsListBox a) => O.OverloadedMethod ListBoxSetActivateOnSingleClickMethodInfo a signature where
    overloadedMethod = listBoxSetActivateOnSingleClick

instance O.OverloadedMethodInfo ListBoxSetActivateOnSingleClickMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox.listBoxSetActivateOnSingleClick",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#v:listBoxSetActivateOnSingleClick"
        })


#endif

-- method ListBox::set_adjustment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkListBox" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the adjustment, or %NULL"
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

foreign import ccall "gtk_list_box_set_adjustment" gtk_list_box_set_adjustment :: 
    Ptr ListBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "ListBox"})
    Ptr Gtk.Adjustment.Adjustment ->        -- adjustment : TInterface (Name {namespace = "Gtk", name = "Adjustment"})
    IO ()

-- | Sets the adjustment (if any) that the widget uses to
-- for vertical scrolling. For instance, this is used
-- to get the page size for PageUp\/Down key handling.
-- 
-- In the normal case when the /@box@/ is packed inside
-- a t'GI.Gtk.Objects.ScrolledWindow.ScrolledWindow' the adjustment from that will
-- be picked up automatically, so there is no need
-- to manually do that.
-- 
-- /Since: 3.10/
listBoxSetAdjustment ::
    (B.CallStack.HasCallStack, MonadIO m, IsListBox a, Gtk.Adjustment.IsAdjustment b) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.ListBox.ListBox'
    -> Maybe (b)
    -- ^ /@adjustment@/: the adjustment, or 'P.Nothing'
    -> m ()
listBoxSetAdjustment box adjustment = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    maybeAdjustment <- case adjustment of
        Nothing -> return nullPtr
        Just jAdjustment -> do
            jAdjustment' <- unsafeManagedPtrCastPtr jAdjustment
            return jAdjustment'
    gtk_list_box_set_adjustment box' maybeAdjustment
    touchManagedPtr box
    whenJust adjustment touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data ListBoxSetAdjustmentMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsListBox a, Gtk.Adjustment.IsAdjustment b) => O.OverloadedMethod ListBoxSetAdjustmentMethodInfo a signature where
    overloadedMethod = listBoxSetAdjustment

instance O.OverloadedMethodInfo ListBoxSetAdjustmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox.listBoxSetAdjustment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#v:listBoxSetAdjustment"
        })


#endif

-- method ListBox::set_filter_func
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkListBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "filter_func"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListBoxFilterFunc" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "callback that lets you filter which rows to show"
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
--                 { rawDocText = Just "user data passed to @filter_func"
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
--                 { rawDocText = Just "destroy notifier for @user_data"
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

foreign import ccall "gtk_list_box_set_filter_func" gtk_list_box_set_filter_func :: 
    Ptr ListBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "ListBox"})
    FunPtr Gtk.Callbacks.C_ListBoxFilterFunc -> -- filter_func : TInterface (Name {namespace = "Gtk", name = "ListBoxFilterFunc"})
    Ptr () ->                               -- user_data : TBasicType TPtr
    FunPtr GLib.Callbacks.C_DestroyNotify -> -- destroy : TInterface (Name {namespace = "GLib", name = "DestroyNotify"})
    IO ()

-- | By setting a filter function on the /@box@/ one can decide dynamically which
-- of the rows to show. For instance, to implement a search function on a list that
-- filters the original list to only show the matching rows.
-- 
-- The /@filterFunc@/ will be called for each row after the call, and it will
-- continue to be called each time a row changes (via 'GI.Gtk.Objects.ListBoxRow.listBoxRowChanged') or
-- when 'GI.Gtk.Objects.ListBox.listBoxInvalidateFilter' is called.
-- 
-- Note that using a filter function is incompatible with using a model
-- (see 'GI.Gtk.Objects.ListBox.listBoxBindModel').
-- 
-- /Since: 3.10/
listBoxSetFilterFunc ::
    (B.CallStack.HasCallStack, MonadIO m, IsListBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.ListBox.ListBox'
    -> Maybe (Gtk.Callbacks.ListBoxFilterFunc)
    -- ^ /@filterFunc@/: callback that lets you filter which rows to show
    -> m ()
listBoxSetFilterFunc box filterFunc = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    maybeFilterFunc <- case filterFunc of
        Nothing -> return (castPtrToFunPtr nullPtr)
        Just jFilterFunc -> do
            jFilterFunc' <- Gtk.Callbacks.mk_ListBoxFilterFunc (Gtk.Callbacks.wrap_ListBoxFilterFunc Nothing (Gtk.Callbacks.drop_closures_ListBoxFilterFunc jFilterFunc))
            return jFilterFunc'
    let userData = castFunPtrToPtr maybeFilterFunc
    let destroy = SP.safeFreeFunPtrPtr
    gtk_list_box_set_filter_func box' maybeFilterFunc userData destroy
    touchManagedPtr box
    return ()

#if defined(ENABLE_OVERLOADING)
data ListBoxSetFilterFuncMethodInfo
instance (signature ~ (Maybe (Gtk.Callbacks.ListBoxFilterFunc) -> m ()), MonadIO m, IsListBox a) => O.OverloadedMethod ListBoxSetFilterFuncMethodInfo a signature where
    overloadedMethod = listBoxSetFilterFunc

instance O.OverloadedMethodInfo ListBoxSetFilterFuncMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox.listBoxSetFilterFunc",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#v:listBoxSetFilterFunc"
        })


#endif

-- method ListBox::set_header_func
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkListBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "update_header"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "ListBoxUpdateHeaderFunc" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "callback that lets you add row headers"
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
--                 { rawDocText = Just "user data passed to @update_header"
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
--                 { rawDocText = Just "destroy notifier for @user_data"
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

foreign import ccall "gtk_list_box_set_header_func" gtk_list_box_set_header_func :: 
    Ptr ListBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "ListBox"})
    FunPtr Gtk.Callbacks.C_ListBoxUpdateHeaderFunc -> -- update_header : TInterface (Name {namespace = "Gtk", name = "ListBoxUpdateHeaderFunc"})
    Ptr () ->                               -- user_data : TBasicType TPtr
    FunPtr GLib.Callbacks.C_DestroyNotify -> -- destroy : TInterface (Name {namespace = "GLib", name = "DestroyNotify"})
    IO ()

-- | By setting a header function on the /@box@/ one can dynamically add headers
-- in front of rows, depending on the contents of the row and its position in the list.
-- For instance, one could use it to add headers in front of the first item of a
-- new kind, in a list sorted by the kind.
-- 
-- The /@updateHeader@/ can look at the current header widget using 'GI.Gtk.Objects.ListBoxRow.listBoxRowGetHeader'
-- and either update the state of the widget as needed, or set a new one using
-- 'GI.Gtk.Objects.ListBoxRow.listBoxRowSetHeader'. If no header is needed, set the header to 'P.Nothing'.
-- 
-- Note that you may get many calls /@updateHeader@/ to this for a particular row when e.g.
-- changing things that don’t affect the header. In this case it is important for performance
-- to not blindly replace an existing header with an identical one.
-- 
-- The /@updateHeader@/ function will be called for each row after the call, and it will
-- continue to be called each time a row changes (via 'GI.Gtk.Objects.ListBoxRow.listBoxRowChanged') and when
-- the row before changes (either by 'GI.Gtk.Objects.ListBoxRow.listBoxRowChanged' on the previous row, or when
-- the previous row becomes a different row). It is also called for all rows when
-- 'GI.Gtk.Objects.ListBox.listBoxInvalidateHeaders' is called.
-- 
-- /Since: 3.10/
listBoxSetHeaderFunc ::
    (B.CallStack.HasCallStack, MonadIO m, IsListBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.ListBox.ListBox'
    -> Maybe (Gtk.Callbacks.ListBoxUpdateHeaderFunc)
    -- ^ /@updateHeader@/: callback that lets you add row headers
    -> m ()
listBoxSetHeaderFunc box updateHeader = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    maybeUpdateHeader <- case updateHeader of
        Nothing -> return (castPtrToFunPtr nullPtr)
        Just jUpdateHeader -> do
            jUpdateHeader' <- Gtk.Callbacks.mk_ListBoxUpdateHeaderFunc (Gtk.Callbacks.wrap_ListBoxUpdateHeaderFunc Nothing (Gtk.Callbacks.drop_closures_ListBoxUpdateHeaderFunc jUpdateHeader))
            return jUpdateHeader'
    let userData = castFunPtrToPtr maybeUpdateHeader
    let destroy = SP.safeFreeFunPtrPtr
    gtk_list_box_set_header_func box' maybeUpdateHeader userData destroy
    touchManagedPtr box
    return ()

#if defined(ENABLE_OVERLOADING)
data ListBoxSetHeaderFuncMethodInfo
instance (signature ~ (Maybe (Gtk.Callbacks.ListBoxUpdateHeaderFunc) -> m ()), MonadIO m, IsListBox a) => O.OverloadedMethod ListBoxSetHeaderFuncMethodInfo a signature where
    overloadedMethod = listBoxSetHeaderFunc

instance O.OverloadedMethodInfo ListBoxSetHeaderFuncMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox.listBoxSetHeaderFunc",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#v:listBoxSetHeaderFunc"
        })


#endif

-- method ListBox::set_placeholder
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkListBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "placeholder"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidget or %NULL"
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

foreign import ccall "gtk_list_box_set_placeholder" gtk_list_box_set_placeholder :: 
    Ptr ListBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "ListBox"})
    Ptr Gtk.Widget.Widget ->                -- placeholder : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | Sets the placeholder widget that is shown in the list when
-- it doesn\'t display any visible children.
-- 
-- /Since: 3.10/
listBoxSetPlaceholder ::
    (B.CallStack.HasCallStack, MonadIO m, IsListBox a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.ListBox.ListBox'
    -> Maybe (b)
    -- ^ /@placeholder@/: a t'GI.Gtk.Objects.Widget.Widget' or 'P.Nothing'
    -> m ()
listBoxSetPlaceholder box placeholder = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    maybePlaceholder <- case placeholder of
        Nothing -> return nullPtr
        Just jPlaceholder -> do
            jPlaceholder' <- unsafeManagedPtrCastPtr jPlaceholder
            return jPlaceholder'
    gtk_list_box_set_placeholder box' maybePlaceholder
    touchManagedPtr box
    whenJust placeholder touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data ListBoxSetPlaceholderMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsListBox a, Gtk.Widget.IsWidget b) => O.OverloadedMethod ListBoxSetPlaceholderMethodInfo a signature where
    overloadedMethod = listBoxSetPlaceholder

instance O.OverloadedMethodInfo ListBoxSetPlaceholderMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox.listBoxSetPlaceholder",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#v:listBoxSetPlaceholder"
        })


#endif

-- method ListBox::set_selection_mode
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkListBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "mode"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SelectionMode" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The #GtkSelectionMode"
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

foreign import ccall "gtk_list_box_set_selection_mode" gtk_list_box_set_selection_mode :: 
    Ptr ListBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "ListBox"})
    CUInt ->                                -- mode : TInterface (Name {namespace = "Gtk", name = "SelectionMode"})
    IO ()

-- | Sets how selection works in the listbox.
-- See t'GI.Gtk.Enums.SelectionMode' for details.
-- 
-- /Since: 3.10/
listBoxSetSelectionMode ::
    (B.CallStack.HasCallStack, MonadIO m, IsListBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.ListBox.ListBox'
    -> Gtk.Enums.SelectionMode
    -- ^ /@mode@/: The t'GI.Gtk.Enums.SelectionMode'
    -> m ()
listBoxSetSelectionMode box mode = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    let mode' = (fromIntegral . fromEnum) mode
    gtk_list_box_set_selection_mode box' mode'
    touchManagedPtr box
    return ()

#if defined(ENABLE_OVERLOADING)
data ListBoxSetSelectionModeMethodInfo
instance (signature ~ (Gtk.Enums.SelectionMode -> m ()), MonadIO m, IsListBox a) => O.OverloadedMethod ListBoxSetSelectionModeMethodInfo a signature where
    overloadedMethod = listBoxSetSelectionMode

instance O.OverloadedMethodInfo ListBoxSetSelectionModeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox.listBoxSetSelectionMode",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#v:listBoxSetSelectionMode"
        })


#endif

-- method ListBox::set_sort_func
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkListBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "sort_func"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListBoxSortFunc" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the sort function" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "user data passed to @sort_func"
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
--                 { rawDocText = Just "destroy notifier for @user_data"
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

foreign import ccall "gtk_list_box_set_sort_func" gtk_list_box_set_sort_func :: 
    Ptr ListBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "ListBox"})
    FunPtr Gtk.Callbacks.C_ListBoxSortFunc -> -- sort_func : TInterface (Name {namespace = "Gtk", name = "ListBoxSortFunc"})
    Ptr () ->                               -- user_data : TBasicType TPtr
    FunPtr GLib.Callbacks.C_DestroyNotify -> -- destroy : TInterface (Name {namespace = "GLib", name = "DestroyNotify"})
    IO ()

-- | By setting a sort function on the /@box@/ one can dynamically reorder the rows
-- of the list, based on the contents of the rows.
-- 
-- The /@sortFunc@/ will be called for each row after the call, and will continue to
-- be called each time a row changes (via 'GI.Gtk.Objects.ListBoxRow.listBoxRowChanged') and when
-- 'GI.Gtk.Objects.ListBox.listBoxInvalidateSort' is called.
-- 
-- Note that using a sort function is incompatible with using a model
-- (see 'GI.Gtk.Objects.ListBox.listBoxBindModel').
-- 
-- /Since: 3.10/
listBoxSetSortFunc ::
    (B.CallStack.HasCallStack, MonadIO m, IsListBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.ListBox.ListBox'
    -> Maybe (Gtk.Callbacks.ListBoxSortFunc)
    -- ^ /@sortFunc@/: the sort function
    -> m ()
listBoxSetSortFunc box sortFunc = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    maybeSortFunc <- case sortFunc of
        Nothing -> return (castPtrToFunPtr nullPtr)
        Just jSortFunc -> do
            jSortFunc' <- Gtk.Callbacks.mk_ListBoxSortFunc (Gtk.Callbacks.wrap_ListBoxSortFunc Nothing (Gtk.Callbacks.drop_closures_ListBoxSortFunc jSortFunc))
            return jSortFunc'
    let userData = castFunPtrToPtr maybeSortFunc
    let destroy = SP.safeFreeFunPtrPtr
    gtk_list_box_set_sort_func box' maybeSortFunc userData destroy
    touchManagedPtr box
    return ()

#if defined(ENABLE_OVERLOADING)
data ListBoxSetSortFuncMethodInfo
instance (signature ~ (Maybe (Gtk.Callbacks.ListBoxSortFunc) -> m ()), MonadIO m, IsListBox a) => O.OverloadedMethod ListBoxSetSortFuncMethodInfo a signature where
    overloadedMethod = listBoxSetSortFunc

instance O.OverloadedMethodInfo ListBoxSetSortFuncMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox.listBoxSetSortFunc",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#v:listBoxSetSortFunc"
        })


#endif

-- method ListBox::unselect_all
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkListBox" , sinceVersion = Nothing }
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

foreign import ccall "gtk_list_box_unselect_all" gtk_list_box_unselect_all :: 
    Ptr ListBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "ListBox"})
    IO ()

-- | Unselect all children of /@box@/, if the selection mode allows it.
-- 
-- /Since: 3.14/
listBoxUnselectAll ::
    (B.CallStack.HasCallStack, MonadIO m, IsListBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.ListBox.ListBox'
    -> m ()
listBoxUnselectAll box = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    gtk_list_box_unselect_all box'
    touchManagedPtr box
    return ()

#if defined(ENABLE_OVERLOADING)
data ListBoxUnselectAllMethodInfo
instance (signature ~ (m ()), MonadIO m, IsListBox a) => O.OverloadedMethod ListBoxUnselectAllMethodInfo a signature where
    overloadedMethod = listBoxUnselectAll

instance O.OverloadedMethodInfo ListBoxUnselectAllMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox.listBoxUnselectAll",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#v:listBoxUnselectAll"
        })


#endif

-- method ListBox::unselect_row
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkListBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "row"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListBoxRow" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the row to unselected"
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

foreign import ccall "gtk_list_box_unselect_row" gtk_list_box_unselect_row :: 
    Ptr ListBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "ListBox"})
    Ptr Gtk.ListBoxRow.ListBoxRow ->        -- row : TInterface (Name {namespace = "Gtk", name = "ListBoxRow"})
    IO ()

-- | Unselects a single row of /@box@/, if the selection mode allows it.
-- 
-- /Since: 3.14/
listBoxUnselectRow ::
    (B.CallStack.HasCallStack, MonadIO m, IsListBox a, Gtk.ListBoxRow.IsListBoxRow b) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.ListBox.ListBox'
    -> b
    -- ^ /@row@/: the row to unselected
    -> m ()
listBoxUnselectRow box row = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    row' <- unsafeManagedPtrCastPtr row
    gtk_list_box_unselect_row box' row'
    touchManagedPtr box
    touchManagedPtr row
    return ()

#if defined(ENABLE_OVERLOADING)
data ListBoxUnselectRowMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsListBox a, Gtk.ListBoxRow.IsListBoxRow b) => O.OverloadedMethod ListBoxUnselectRowMethodInfo a signature where
    overloadedMethod = listBoxUnselectRow

instance O.OverloadedMethodInfo ListBoxUnselectRowMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListBox.listBoxUnselectRow",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListBox.html#v:listBoxUnselectRow"
        })


#endif


