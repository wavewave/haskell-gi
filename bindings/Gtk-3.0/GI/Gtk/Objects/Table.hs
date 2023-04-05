{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- The t'GI.Gtk.Objects.Table.Table' functions allow the programmer to arrange widgets in rows and
-- columns, making it easy to align many widgets next to each other,
-- horizontally and vertically.
-- 
-- Tables are created with a call to 'GI.Gtk.Objects.Table.tableNew', the size of which can
-- later be changed with 'GI.Gtk.Objects.Table.tableResize'.
-- 
-- Widgets can be added to a table using 'GI.Gtk.Objects.Table.tableAttach' or the more
-- convenient (but slightly less flexible) 'GI.Gtk.Objects.Table.tableAttachDefaults'.
-- 
-- To alter the space next to a specific row, use 'GI.Gtk.Objects.Table.tableSetRowSpacing',
-- and for a column, 'GI.Gtk.Objects.Table.tableSetColSpacing'.
-- The gaps between all rows or columns can be changed by
-- calling 'GI.Gtk.Objects.Table.tableSetRowSpacings' or 'GI.Gtk.Objects.Table.tableSetColSpacings'
-- respectively. Note that spacing is added between the
-- children, while padding added by 'GI.Gtk.Objects.Table.tableAttach' is added on
-- either side of the widget it belongs to.
-- 
-- 'GI.Gtk.Objects.Table.tableSetHomogeneous', can be used to set whether all cells in the
-- table will resize themselves to the size of the largest widget in the table.
-- 
-- > t'GI.Gtk.Objects.Table.Table' has been deprecated. Use t'GI.Gtk.Objects.Grid.Grid' instead. It provides the same
-- > capabilities as GtkTable for arranging widgets in a rectangular grid, but
-- > does support height-for-width geometry management.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.Table
    ( 

-- * Exported types
    Table(..)                               ,
    IsTable                                 ,
    toTable                                 ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [attach]("GI.Gtk.Objects.Table#g:method:attach"), [attachDefaults]("GI.Gtk.Objects.Table#g:method:attachDefaults"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [resize]("GI.Gtk.Objects.Table#g:method:resize"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getColSpacing]("GI.Gtk.Objects.Table#g:method:getColSpacing"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDefaultColSpacing]("GI.Gtk.Objects.Table#g:method:getDefaultColSpacing"), [getDefaultRowSpacing]("GI.Gtk.Objects.Table#g:method:getDefaultRowSpacing"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getHomogeneous]("GI.Gtk.Objects.Table#g:method:getHomogeneous"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getRowSpacing]("GI.Gtk.Objects.Table#g:method:getRowSpacing"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSize]("GI.Gtk.Objects.Table#g:method:getSize"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setColSpacing]("GI.Gtk.Objects.Table#g:method:setColSpacing"), [setColSpacings]("GI.Gtk.Objects.Table#g:method:setColSpacings"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setHomogeneous]("GI.Gtk.Objects.Table#g:method:setHomogeneous"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setRowSpacing]("GI.Gtk.Objects.Table#g:method:setRowSpacing"), [setRowSpacings]("GI.Gtk.Objects.Table#g:method:setRowSpacings"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveTableMethod                      ,
#endif

-- ** attach #method:attach#

#if defined(ENABLE_OVERLOADING)
    TableAttachMethodInfo                   ,
#endif
    tableAttach                             ,


-- ** attachDefaults #method:attachDefaults#

#if defined(ENABLE_OVERLOADING)
    TableAttachDefaultsMethodInfo           ,
#endif
    tableAttachDefaults                     ,


-- ** getColSpacing #method:getColSpacing#

#if defined(ENABLE_OVERLOADING)
    TableGetColSpacingMethodInfo            ,
#endif
    tableGetColSpacing                      ,


-- ** getDefaultColSpacing #method:getDefaultColSpacing#

#if defined(ENABLE_OVERLOADING)
    TableGetDefaultColSpacingMethodInfo     ,
#endif
    tableGetDefaultColSpacing               ,


-- ** getDefaultRowSpacing #method:getDefaultRowSpacing#

#if defined(ENABLE_OVERLOADING)
    TableGetDefaultRowSpacingMethodInfo     ,
#endif
    tableGetDefaultRowSpacing               ,


-- ** getHomogeneous #method:getHomogeneous#

#if defined(ENABLE_OVERLOADING)
    TableGetHomogeneousMethodInfo           ,
#endif
    tableGetHomogeneous                     ,


-- ** getRowSpacing #method:getRowSpacing#

#if defined(ENABLE_OVERLOADING)
    TableGetRowSpacingMethodInfo            ,
#endif
    tableGetRowSpacing                      ,


-- ** getSize #method:getSize#

#if defined(ENABLE_OVERLOADING)
    TableGetSizeMethodInfo                  ,
#endif
    tableGetSize                            ,


-- ** new #method:new#

    tableNew                                ,


-- ** resize #method:resize#

#if defined(ENABLE_OVERLOADING)
    TableResizeMethodInfo                   ,
#endif
    tableResize                             ,


-- ** setColSpacing #method:setColSpacing#

#if defined(ENABLE_OVERLOADING)
    TableSetColSpacingMethodInfo            ,
#endif
    tableSetColSpacing                      ,


-- ** setColSpacings #method:setColSpacings#

#if defined(ENABLE_OVERLOADING)
    TableSetColSpacingsMethodInfo           ,
#endif
    tableSetColSpacings                     ,


-- ** setHomogeneous #method:setHomogeneous#

#if defined(ENABLE_OVERLOADING)
    TableSetHomogeneousMethodInfo           ,
#endif
    tableSetHomogeneous                     ,


-- ** setRowSpacing #method:setRowSpacing#

#if defined(ENABLE_OVERLOADING)
    TableSetRowSpacingMethodInfo            ,
#endif
    tableSetRowSpacing                      ,


-- ** setRowSpacings #method:setRowSpacings#

#if defined(ENABLE_OVERLOADING)
    TableSetRowSpacingsMethodInfo           ,
#endif
    tableSetRowSpacings                     ,




 -- * Properties


-- ** columnSpacing #attr:columnSpacing#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TableColumnSpacingPropertyInfo          ,
#endif
    constructTableColumnSpacing             ,
    getTableColumnSpacing                   ,
    setTableColumnSpacing                   ,
#if defined(ENABLE_OVERLOADING)
    tableColumnSpacing                      ,
#endif


-- ** homogeneous #attr:homogeneous#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TableHomogeneousPropertyInfo            ,
#endif
    constructTableHomogeneous               ,
    getTableHomogeneous                     ,
    setTableHomogeneous                     ,
#if defined(ENABLE_OVERLOADING)
    tableHomogeneous                        ,
#endif


-- ** nColumns #attr:nColumns#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TableNColumnsPropertyInfo               ,
#endif
    constructTableNColumns                  ,
    getTableNColumns                        ,
    setTableNColumns                        ,
#if defined(ENABLE_OVERLOADING)
    tableNColumns                           ,
#endif


-- ** nRows #attr:nRows#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TableNRowsPropertyInfo                  ,
#endif
    constructTableNRows                     ,
    getTableNRows                           ,
    setTableNRows                           ,
#if defined(ENABLE_OVERLOADING)
    tableNRows                              ,
#endif


-- ** rowSpacing #attr:rowSpacing#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TableRowSpacingPropertyInfo             ,
#endif
    constructTableRowSpacing                ,
    getTableRowSpacing                      ,
    setTableRowSpacing                      ,
#if defined(ENABLE_OVERLOADING)
    tableRowSpacing                         ,
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

import qualified GI.Atk.Interfaces.ImplementorIface as Atk.ImplementorIface
import qualified GI.GObject.Objects.Object as GObject.Object
import {-# SOURCE #-} qualified GI.Gtk.Flags as Gtk.Flags
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype Table = Table (SP.ManagedPtr Table)
    deriving (Eq)

instance SP.ManagedPtrNewtype Table where
    toManagedPtr (Table p) = p

foreign import ccall "gtk_table_get_type"
    c_gtk_table_get_type :: IO B.Types.GType

instance B.Types.TypedObject Table where
    glibType = c_gtk_table_get_type

instance B.Types.GObject Table

-- | Type class for types which can be safely cast to `Table`, for instance with `toTable`.
class (SP.GObject o, O.IsDescendantOf Table o) => IsTable o
instance (SP.GObject o, O.IsDescendantOf Table o) => IsTable o

instance O.HasParentTypes Table
type instance O.ParentTypes Table = '[Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable]

-- | Cast to `Table`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toTable :: (MIO.MonadIO m, IsTable o) => o -> m Table
toTable = MIO.liftIO . B.ManagedPtr.unsafeCastTo Table

-- | Convert 'Table' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe Table) where
    gvalueGType_ = c_gtk_table_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr Table)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr Table)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject Table ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveTableMethod (t :: Symbol) (o :: *) :: * where
    ResolveTableMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveTableMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveTableMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveTableMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveTableMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveTableMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveTableMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveTableMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveTableMethod "attach" o = TableAttachMethodInfo
    ResolveTableMethod "attachDefaults" o = TableAttachDefaultsMethodInfo
    ResolveTableMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveTableMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveTableMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveTableMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveTableMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveTableMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveTableMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveTableMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveTableMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveTableMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveTableMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveTableMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveTableMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveTableMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveTableMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveTableMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveTableMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveTableMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveTableMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveTableMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveTableMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveTableMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveTableMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveTableMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveTableMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveTableMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveTableMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveTableMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveTableMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveTableMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveTableMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveTableMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveTableMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveTableMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveTableMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveTableMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveTableMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveTableMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveTableMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveTableMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveTableMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveTableMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveTableMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveTableMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveTableMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveTableMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveTableMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveTableMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveTableMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveTableMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveTableMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveTableMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveTableMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveTableMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveTableMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveTableMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveTableMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveTableMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveTableMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveTableMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveTableMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveTableMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveTableMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveTableMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveTableMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveTableMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveTableMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveTableMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveTableMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveTableMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveTableMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveTableMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveTableMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveTableMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveTableMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveTableMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveTableMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveTableMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveTableMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveTableMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveTableMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveTableMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveTableMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveTableMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveTableMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveTableMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveTableMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveTableMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveTableMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveTableMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveTableMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveTableMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveTableMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveTableMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveTableMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveTableMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveTableMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveTableMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveTableMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveTableMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveTableMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveTableMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveTableMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveTableMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveTableMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveTableMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveTableMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveTableMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveTableMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveTableMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveTableMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveTableMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveTableMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveTableMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveTableMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveTableMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveTableMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveTableMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveTableMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveTableMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolveTableMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveTableMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveTableMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveTableMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveTableMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveTableMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveTableMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveTableMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveTableMethod "resize" o = TableResizeMethodInfo
    ResolveTableMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveTableMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveTableMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveTableMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveTableMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveTableMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveTableMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveTableMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveTableMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveTableMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveTableMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveTableMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveTableMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveTableMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveTableMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveTableMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveTableMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveTableMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveTableMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveTableMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveTableMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveTableMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveTableMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveTableMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveTableMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveTableMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveTableMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveTableMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveTableMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveTableMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveTableMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveTableMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveTableMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveTableMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveTableMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveTableMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveTableMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveTableMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveTableMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveTableMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveTableMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveTableMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveTableMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveTableMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveTableMethod "getColSpacing" o = TableGetColSpacingMethodInfo
    ResolveTableMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveTableMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveTableMethod "getDefaultColSpacing" o = TableGetDefaultColSpacingMethodInfo
    ResolveTableMethod "getDefaultRowSpacing" o = TableGetDefaultRowSpacingMethodInfo
    ResolveTableMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveTableMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveTableMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveTableMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveTableMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveTableMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveTableMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveTableMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveTableMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveTableMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveTableMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveTableMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveTableMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveTableMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveTableMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveTableMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveTableMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveTableMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveTableMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveTableMethod "getHomogeneous" o = TableGetHomogeneousMethodInfo
    ResolveTableMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveTableMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveTableMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveTableMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveTableMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveTableMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveTableMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveTableMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveTableMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveTableMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveTableMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveTableMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveTableMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveTableMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveTableMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveTableMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveTableMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveTableMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveTableMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveTableMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveTableMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveTableMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveTableMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveTableMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveTableMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveTableMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveTableMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveTableMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveTableMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveTableMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveTableMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveTableMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveTableMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveTableMethod "getRowSpacing" o = TableGetRowSpacingMethodInfo
    ResolveTableMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveTableMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveTableMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveTableMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveTableMethod "getSize" o = TableGetSizeMethodInfo
    ResolveTableMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveTableMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveTableMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveTableMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveTableMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveTableMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveTableMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveTableMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveTableMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveTableMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveTableMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveTableMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveTableMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveTableMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveTableMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveTableMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveTableMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveTableMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveTableMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveTableMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveTableMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveTableMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveTableMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveTableMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveTableMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveTableMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveTableMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveTableMethod "setColSpacing" o = TableSetColSpacingMethodInfo
    ResolveTableMethod "setColSpacings" o = TableSetColSpacingsMethodInfo
    ResolveTableMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveTableMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveTableMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveTableMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveTableMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveTableMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveTableMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveTableMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveTableMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveTableMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveTableMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveTableMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveTableMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveTableMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveTableMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveTableMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveTableMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveTableMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveTableMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveTableMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveTableMethod "setHomogeneous" o = TableSetHomogeneousMethodInfo
    ResolveTableMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveTableMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveTableMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveTableMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveTableMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveTableMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveTableMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveTableMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveTableMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveTableMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveTableMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveTableMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveTableMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveTableMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveTableMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveTableMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveTableMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveTableMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveTableMethod "setRowSpacing" o = TableSetRowSpacingMethodInfo
    ResolveTableMethod "setRowSpacings" o = TableSetRowSpacingsMethodInfo
    ResolveTableMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveTableMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveTableMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveTableMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveTableMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveTableMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveTableMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveTableMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveTableMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveTableMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveTableMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveTableMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveTableMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveTableMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveTableMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveTableMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveTableMethod t Table, O.OverloadedMethod info Table p) => OL.IsLabel t (Table -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveTableMethod t Table, O.OverloadedMethod info Table p, R.HasField t Table p) => R.HasField t Table p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveTableMethod t Table, O.OverloadedMethodInfo info Table) => OL.IsLabel t (O.MethodProxy info Table) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- VVV Prop "column-spacing"
   -- Type: TBasicType TUInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@column-spacing@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' table #columnSpacing
-- @
getTableColumnSpacing :: (MonadIO m, IsTable o) => o -> m Word32
getTableColumnSpacing obj = MIO.liftIO $ B.Properties.getObjectPropertyUInt32 obj "column-spacing"

-- | Set the value of the “@column-spacing@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' table [ #columnSpacing 'Data.GI.Base.Attributes.:=' value ]
-- @
setTableColumnSpacing :: (MonadIO m, IsTable o) => o -> Word32 -> m ()
setTableColumnSpacing obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyUInt32 obj "column-spacing" val

-- | Construct a `GValueConstruct` with valid value for the “@column-spacing@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTableColumnSpacing :: (IsTable o, MIO.MonadIO m) => Word32 -> m (GValueConstruct o)
constructTableColumnSpacing val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyUInt32 "column-spacing" val

#if defined(ENABLE_OVERLOADING)
data TableColumnSpacingPropertyInfo
instance AttrInfo TableColumnSpacingPropertyInfo where
    type AttrAllowedOps TableColumnSpacingPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TableColumnSpacingPropertyInfo = IsTable
    type AttrSetTypeConstraint TableColumnSpacingPropertyInfo = (~) Word32
    type AttrTransferTypeConstraint TableColumnSpacingPropertyInfo = (~) Word32
    type AttrTransferType TableColumnSpacingPropertyInfo = Word32
    type AttrGetType TableColumnSpacingPropertyInfo = Word32
    type AttrLabel TableColumnSpacingPropertyInfo = "column-spacing"
    type AttrOrigin TableColumnSpacingPropertyInfo = Table
    attrGet = getTableColumnSpacing
    attrSet = setTableColumnSpacing
    attrTransfer _ v = do
        return v
    attrConstruct = constructTableColumnSpacing
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Table.columnSpacing"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Table.html#g:attr:columnSpacing"
        })
#endif

-- VVV Prop "homogeneous"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@homogeneous@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' table #homogeneous
-- @
getTableHomogeneous :: (MonadIO m, IsTable o) => o -> m Bool
getTableHomogeneous obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "homogeneous"

-- | Set the value of the “@homogeneous@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' table [ #homogeneous 'Data.GI.Base.Attributes.:=' value ]
-- @
setTableHomogeneous :: (MonadIO m, IsTable o) => o -> Bool -> m ()
setTableHomogeneous obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "homogeneous" val

-- | Construct a `GValueConstruct` with valid value for the “@homogeneous@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTableHomogeneous :: (IsTable o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTableHomogeneous val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "homogeneous" val

#if defined(ENABLE_OVERLOADING)
data TableHomogeneousPropertyInfo
instance AttrInfo TableHomogeneousPropertyInfo where
    type AttrAllowedOps TableHomogeneousPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TableHomogeneousPropertyInfo = IsTable
    type AttrSetTypeConstraint TableHomogeneousPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TableHomogeneousPropertyInfo = (~) Bool
    type AttrTransferType TableHomogeneousPropertyInfo = Bool
    type AttrGetType TableHomogeneousPropertyInfo = Bool
    type AttrLabel TableHomogeneousPropertyInfo = "homogeneous"
    type AttrOrigin TableHomogeneousPropertyInfo = Table
    attrGet = getTableHomogeneous
    attrSet = setTableHomogeneous
    attrTransfer _ v = do
        return v
    attrConstruct = constructTableHomogeneous
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Table.homogeneous"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Table.html#g:attr:homogeneous"
        })
#endif

-- VVV Prop "n-columns"
   -- Type: TBasicType TUInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@n-columns@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' table #nColumns
-- @
getTableNColumns :: (MonadIO m, IsTable o) => o -> m Word32
getTableNColumns obj = MIO.liftIO $ B.Properties.getObjectPropertyUInt32 obj "n-columns"

-- | Set the value of the “@n-columns@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' table [ #nColumns 'Data.GI.Base.Attributes.:=' value ]
-- @
setTableNColumns :: (MonadIO m, IsTable o) => o -> Word32 -> m ()
setTableNColumns obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyUInt32 obj "n-columns" val

-- | Construct a `GValueConstruct` with valid value for the “@n-columns@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTableNColumns :: (IsTable o, MIO.MonadIO m) => Word32 -> m (GValueConstruct o)
constructTableNColumns val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyUInt32 "n-columns" val

#if defined(ENABLE_OVERLOADING)
data TableNColumnsPropertyInfo
instance AttrInfo TableNColumnsPropertyInfo where
    type AttrAllowedOps TableNColumnsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TableNColumnsPropertyInfo = IsTable
    type AttrSetTypeConstraint TableNColumnsPropertyInfo = (~) Word32
    type AttrTransferTypeConstraint TableNColumnsPropertyInfo = (~) Word32
    type AttrTransferType TableNColumnsPropertyInfo = Word32
    type AttrGetType TableNColumnsPropertyInfo = Word32
    type AttrLabel TableNColumnsPropertyInfo = "n-columns"
    type AttrOrigin TableNColumnsPropertyInfo = Table
    attrGet = getTableNColumns
    attrSet = setTableNColumns
    attrTransfer _ v = do
        return v
    attrConstruct = constructTableNColumns
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Table.nColumns"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Table.html#g:attr:nColumns"
        })
#endif

-- VVV Prop "n-rows"
   -- Type: TBasicType TUInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@n-rows@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' table #nRows
-- @
getTableNRows :: (MonadIO m, IsTable o) => o -> m Word32
getTableNRows obj = MIO.liftIO $ B.Properties.getObjectPropertyUInt32 obj "n-rows"

-- | Set the value of the “@n-rows@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' table [ #nRows 'Data.GI.Base.Attributes.:=' value ]
-- @
setTableNRows :: (MonadIO m, IsTable o) => o -> Word32 -> m ()
setTableNRows obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyUInt32 obj "n-rows" val

-- | Construct a `GValueConstruct` with valid value for the “@n-rows@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTableNRows :: (IsTable o, MIO.MonadIO m) => Word32 -> m (GValueConstruct o)
constructTableNRows val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyUInt32 "n-rows" val

#if defined(ENABLE_OVERLOADING)
data TableNRowsPropertyInfo
instance AttrInfo TableNRowsPropertyInfo where
    type AttrAllowedOps TableNRowsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TableNRowsPropertyInfo = IsTable
    type AttrSetTypeConstraint TableNRowsPropertyInfo = (~) Word32
    type AttrTransferTypeConstraint TableNRowsPropertyInfo = (~) Word32
    type AttrTransferType TableNRowsPropertyInfo = Word32
    type AttrGetType TableNRowsPropertyInfo = Word32
    type AttrLabel TableNRowsPropertyInfo = "n-rows"
    type AttrOrigin TableNRowsPropertyInfo = Table
    attrGet = getTableNRows
    attrSet = setTableNRows
    attrTransfer _ v = do
        return v
    attrConstruct = constructTableNRows
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Table.nRows"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Table.html#g:attr:nRows"
        })
#endif

-- VVV Prop "row-spacing"
   -- Type: TBasicType TUInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@row-spacing@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' table #rowSpacing
-- @
getTableRowSpacing :: (MonadIO m, IsTable o) => o -> m Word32
getTableRowSpacing obj = MIO.liftIO $ B.Properties.getObjectPropertyUInt32 obj "row-spacing"

-- | Set the value of the “@row-spacing@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' table [ #rowSpacing 'Data.GI.Base.Attributes.:=' value ]
-- @
setTableRowSpacing :: (MonadIO m, IsTable o) => o -> Word32 -> m ()
setTableRowSpacing obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyUInt32 obj "row-spacing" val

-- | Construct a `GValueConstruct` with valid value for the “@row-spacing@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTableRowSpacing :: (IsTable o, MIO.MonadIO m) => Word32 -> m (GValueConstruct o)
constructTableRowSpacing val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyUInt32 "row-spacing" val

#if defined(ENABLE_OVERLOADING)
data TableRowSpacingPropertyInfo
instance AttrInfo TableRowSpacingPropertyInfo where
    type AttrAllowedOps TableRowSpacingPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TableRowSpacingPropertyInfo = IsTable
    type AttrSetTypeConstraint TableRowSpacingPropertyInfo = (~) Word32
    type AttrTransferTypeConstraint TableRowSpacingPropertyInfo = (~) Word32
    type AttrTransferType TableRowSpacingPropertyInfo = Word32
    type AttrGetType TableRowSpacingPropertyInfo = Word32
    type AttrLabel TableRowSpacingPropertyInfo = "row-spacing"
    type AttrOrigin TableRowSpacingPropertyInfo = Table
    attrGet = getTableRowSpacing
    attrSet = setTableRowSpacing
    attrTransfer _ v = do
        return v
    attrConstruct = constructTableRowSpacing
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Table.rowSpacing"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Table.html#g:attr:rowSpacing"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList Table
type instance O.AttributeList Table = TableAttributeList
type TableAttributeList = ('[ '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("columnSpacing", TableColumnSpacingPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("homogeneous", TableHomogeneousPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("nColumns", TableNColumnsPropertyInfo), '("nRows", TableNRowsPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("rowSpacing", TableRowSpacingPropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
tableColumnSpacing :: AttrLabelProxy "columnSpacing"
tableColumnSpacing = AttrLabelProxy

tableHomogeneous :: AttrLabelProxy "homogeneous"
tableHomogeneous = AttrLabelProxy

tableNColumns :: AttrLabelProxy "nColumns"
tableNColumns = AttrLabelProxy

tableNRows :: AttrLabelProxy "nRows"
tableNRows = AttrLabelProxy

tableRowSpacing :: AttrLabelProxy "rowSpacing"
tableRowSpacing = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList Table = TableSignalList
type TableSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method Table::new
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "rows"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The number of rows the new table should have."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "columns"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The number of columns the new table should have."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "homogeneous"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "If set to %TRUE, all table cells are resized to the size of\n  the cell containing the largest widget."
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Table" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_table_new" gtk_table_new :: 
    Word32 ->                               -- rows : TBasicType TUInt
    Word32 ->                               -- columns : TBasicType TUInt
    CInt ->                                 -- homogeneous : TBasicType TBoolean
    IO (Ptr Table)

{-# DEPRECATED tableNew ["(Since version 3.4)","Use 'GI.Gtk.Objects.Grid.gridNew'."] #-}
-- | Used to create a new table widget. An initial size must be given by
-- specifying how many rows and columns the table should have, although
-- this can be changed later with 'GI.Gtk.Objects.Table.tableResize'.  /@rows@/ and /@columns@/
-- must both be in the range 1 .. 65535. For historical reasons, 0 is accepted
-- as well and is silently interpreted as 1.
tableNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Word32
    -- ^ /@rows@/: The number of rows the new table should have.
    -> Word32
    -- ^ /@columns@/: The number of columns the new table should have.
    -> Bool
    -- ^ /@homogeneous@/: If set to 'P.True', all table cells are resized to the size of
    --   the cell containing the largest widget.
    -> m Table
    -- ^ __Returns:__ A pointer to the newly created table widget.
tableNew rows columns homogeneous = liftIO $ do
    let homogeneous' = (fromIntegral . fromEnum) homogeneous
    result <- gtk_table_new rows columns homogeneous'
    checkUnexpectedReturnNULL "tableNew" result
    result' <- (newObject Table) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Table::attach
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "table"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Table" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The #GtkTable to add a new widget to."
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "The widget to add." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "left_attach"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the column number to attach the left side of a child widget to."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "right_attach"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the column number to attach the right side of a child widget to."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "top_attach"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the row number to attach the top of a child widget to."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "bottom_attach"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the row number to attach the bottom of a child widget to."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "xoptions"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AttachOptions" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "Used to specify the properties of the child widget when the table is resized."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "yoptions"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AttachOptions" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "The same as xoptions, except this field determines behaviour of vertical resizing."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "xpadding"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "An integer value specifying the padding on the left and right of the widget being added to the table."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "ypadding"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The amount of padding above and below the child widget."
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

foreign import ccall "gtk_table_attach" gtk_table_attach :: 
    Ptr Table ->                            -- table : TInterface (Name {namespace = "Gtk", name = "Table"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Word32 ->                               -- left_attach : TBasicType TUInt
    Word32 ->                               -- right_attach : TBasicType TUInt
    Word32 ->                               -- top_attach : TBasicType TUInt
    Word32 ->                               -- bottom_attach : TBasicType TUInt
    CUInt ->                                -- xoptions : TInterface (Name {namespace = "Gtk", name = "AttachOptions"})
    CUInt ->                                -- yoptions : TInterface (Name {namespace = "Gtk", name = "AttachOptions"})
    Word32 ->                               -- xpadding : TBasicType TUInt
    Word32 ->                               -- ypadding : TBasicType TUInt
    IO ()

{-# DEPRECATED tableAttach ["(Since version 3.4)","Use 'GI.Gtk.Objects.Grid.gridAttach' with t'GI.Gtk.Objects.Grid.Grid'. Note that the attach","    arguments differ between those two functions."] #-}
-- | Adds a widget to a table. The number of “cells” that a widget will occupy is
-- specified by /@leftAttach@/, /@rightAttach@/, /@topAttach@/ and /@bottomAttach@/.
-- These each represent the leftmost, rightmost, uppermost and lowest column
-- and row numbers of the table. (Columns and rows are indexed from zero).
-- 
-- To make a button occupy the lower right cell of a 2x2 table, use
-- >
-- >gtk_table_attach (table, button,
-- >                  1, 2, // left, right attach
-- >                  1, 2, // top, bottom attach
-- >                  xoptions, yoptions,
-- >                  xpadding, ypadding);
-- 
-- If you want to make the button span the entire bottom row, use /@leftAttach@/ == 0 and /@rightAttach@/ = 2 instead.
tableAttach ::
    (B.CallStack.HasCallStack, MonadIO m, IsTable a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@table@/: The t'GI.Gtk.Objects.Table.Table' to add a new widget to.
    -> b
    -- ^ /@child@/: The widget to add.
    -> Word32
    -- ^ /@leftAttach@/: the column number to attach the left side of a child widget to.
    -> Word32
    -- ^ /@rightAttach@/: the column number to attach the right side of a child widget to.
    -> Word32
    -- ^ /@topAttach@/: the row number to attach the top of a child widget to.
    -> Word32
    -- ^ /@bottomAttach@/: the row number to attach the bottom of a child widget to.
    -> [Gtk.Flags.AttachOptions]
    -- ^ /@xoptions@/: Used to specify the properties of the child widget when the table is resized.
    -> [Gtk.Flags.AttachOptions]
    -- ^ /@yoptions@/: The same as xoptions, except this field determines behaviour of vertical resizing.
    -> Word32
    -- ^ /@xpadding@/: An integer value specifying the padding on the left and right of the widget being added to the table.
    -> Word32
    -- ^ /@ypadding@/: The amount of padding above and below the child widget.
    -> m ()
tableAttach table child leftAttach rightAttach topAttach bottomAttach xoptions yoptions xpadding ypadding = liftIO $ do
    table' <- unsafeManagedPtrCastPtr table
    child' <- unsafeManagedPtrCastPtr child
    let xoptions' = gflagsToWord xoptions
    let yoptions' = gflagsToWord yoptions
    gtk_table_attach table' child' leftAttach rightAttach topAttach bottomAttach xoptions' yoptions' xpadding ypadding
    touchManagedPtr table
    touchManagedPtr child
    return ()

#if defined(ENABLE_OVERLOADING)
data TableAttachMethodInfo
instance (signature ~ (b -> Word32 -> Word32 -> Word32 -> Word32 -> [Gtk.Flags.AttachOptions] -> [Gtk.Flags.AttachOptions] -> Word32 -> Word32 -> m ()), MonadIO m, IsTable a, Gtk.Widget.IsWidget b) => O.OverloadedMethod TableAttachMethodInfo a signature where
    overloadedMethod = tableAttach

instance O.OverloadedMethodInfo TableAttachMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Table.tableAttach",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Table.html#v:tableAttach"
        })


#endif

-- method Table::attach_defaults
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "table"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Table" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The table to add a new child widget to."
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
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The child widget to add."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "left_attach"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "The column number to attach the left side of the child widget to."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "right_attach"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "The column number to attach the right side of the child widget to."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "top_attach"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The row number to attach the top of the child widget to."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "bottom_attach"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The row number to attach the bottom of the child widget to."
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

foreign import ccall "gtk_table_attach_defaults" gtk_table_attach_defaults :: 
    Ptr Table ->                            -- table : TInterface (Name {namespace = "Gtk", name = "Table"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Word32 ->                               -- left_attach : TBasicType TUInt
    Word32 ->                               -- right_attach : TBasicType TUInt
    Word32 ->                               -- top_attach : TBasicType TUInt
    Word32 ->                               -- bottom_attach : TBasicType TUInt
    IO ()

{-# DEPRECATED tableAttachDefaults ["(Since version 3.4)","Use 'GI.Gtk.Objects.Grid.gridAttach' with t'GI.Gtk.Objects.Grid.Grid'. Note that the attach","    arguments differ between those two functions."] #-}
-- | As there are many options associated with 'GI.Gtk.Objects.Table.tableAttach', this convenience
-- function provides the programmer with a means to add children to a table with
-- identical padding and expansion options. The values used for the t'GI.Gtk.Flags.AttachOptions'
-- are @GTK_EXPAND | GTK_FILL@, and the padding is set to 0.
tableAttachDefaults ::
    (B.CallStack.HasCallStack, MonadIO m, IsTable a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@table@/: The table to add a new child widget to.
    -> b
    -- ^ /@widget@/: The child widget to add.
    -> Word32
    -- ^ /@leftAttach@/: The column number to attach the left side of the child widget to.
    -> Word32
    -- ^ /@rightAttach@/: The column number to attach the right side of the child widget to.
    -> Word32
    -- ^ /@topAttach@/: The row number to attach the top of the child widget to.
    -> Word32
    -- ^ /@bottomAttach@/: The row number to attach the bottom of the child widget to.
    -> m ()
tableAttachDefaults table widget leftAttach rightAttach topAttach bottomAttach = liftIO $ do
    table' <- unsafeManagedPtrCastPtr table
    widget' <- unsafeManagedPtrCastPtr widget
    gtk_table_attach_defaults table' widget' leftAttach rightAttach topAttach bottomAttach
    touchManagedPtr table
    touchManagedPtr widget
    return ()

#if defined(ENABLE_OVERLOADING)
data TableAttachDefaultsMethodInfo
instance (signature ~ (b -> Word32 -> Word32 -> Word32 -> Word32 -> m ()), MonadIO m, IsTable a, Gtk.Widget.IsWidget b) => O.OverloadedMethod TableAttachDefaultsMethodInfo a signature where
    overloadedMethod = tableAttachDefaults

instance O.OverloadedMethodInfo TableAttachDefaultsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Table.tableAttachDefaults",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Table.html#v:tableAttachDefaults"
        })


#endif

-- method Table::get_col_spacing
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "table"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Table" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTable" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "column"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a column in the table, 0 indicates the first column"
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
-- returnType: Just (TBasicType TUInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_table_get_col_spacing" gtk_table_get_col_spacing :: 
    Ptr Table ->                            -- table : TInterface (Name {namespace = "Gtk", name = "Table"})
    Word32 ->                               -- column : TBasicType TUInt
    IO Word32

{-# DEPRECATED tableGetColSpacing ["(Since version 3.4)","t'GI.Gtk.Objects.Grid.Grid' does not offer a replacement for this","    functionality."] #-}
-- | Gets the amount of space between column /@col@/, and
-- column /@col@/ + 1. See 'GI.Gtk.Objects.Table.tableSetColSpacing'.
tableGetColSpacing ::
    (B.CallStack.HasCallStack, MonadIO m, IsTable a) =>
    a
    -- ^ /@table@/: a t'GI.Gtk.Objects.Table.Table'
    -> Word32
    -- ^ /@column@/: a column in the table, 0 indicates the first column
    -> m Word32
    -- ^ __Returns:__ the column spacing
tableGetColSpacing table column = liftIO $ do
    table' <- unsafeManagedPtrCastPtr table
    result <- gtk_table_get_col_spacing table' column
    touchManagedPtr table
    return result

#if defined(ENABLE_OVERLOADING)
data TableGetColSpacingMethodInfo
instance (signature ~ (Word32 -> m Word32), MonadIO m, IsTable a) => O.OverloadedMethod TableGetColSpacingMethodInfo a signature where
    overloadedMethod = tableGetColSpacing

instance O.OverloadedMethodInfo TableGetColSpacingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Table.tableGetColSpacing",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Table.html#v:tableGetColSpacing"
        })


#endif

-- method Table::get_default_col_spacing
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "table"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Table" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTable" , sinceVersion = Nothing }
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

foreign import ccall "gtk_table_get_default_col_spacing" gtk_table_get_default_col_spacing :: 
    Ptr Table ->                            -- table : TInterface (Name {namespace = "Gtk", name = "Table"})
    IO Word32

{-# DEPRECATED tableGetDefaultColSpacing ["(Since version 3.4)","Use 'GI.Gtk.Objects.Grid.gridGetColumnSpacing' with t'GI.Gtk.Objects.Grid.Grid'."] #-}
-- | Gets the default column spacing for the table. This is
-- the spacing that will be used for newly added columns.
-- (See 'GI.Gtk.Objects.Table.tableSetColSpacings')
tableGetDefaultColSpacing ::
    (B.CallStack.HasCallStack, MonadIO m, IsTable a) =>
    a
    -- ^ /@table@/: a t'GI.Gtk.Objects.Table.Table'
    -> m Word32
    -- ^ __Returns:__ the default column spacing
tableGetDefaultColSpacing table = liftIO $ do
    table' <- unsafeManagedPtrCastPtr table
    result <- gtk_table_get_default_col_spacing table'
    touchManagedPtr table
    return result

#if defined(ENABLE_OVERLOADING)
data TableGetDefaultColSpacingMethodInfo
instance (signature ~ (m Word32), MonadIO m, IsTable a) => O.OverloadedMethod TableGetDefaultColSpacingMethodInfo a signature where
    overloadedMethod = tableGetDefaultColSpacing

instance O.OverloadedMethodInfo TableGetDefaultColSpacingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Table.tableGetDefaultColSpacing",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Table.html#v:tableGetDefaultColSpacing"
        })


#endif

-- method Table::get_default_row_spacing
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "table"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Table" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTable" , sinceVersion = Nothing }
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

foreign import ccall "gtk_table_get_default_row_spacing" gtk_table_get_default_row_spacing :: 
    Ptr Table ->                            -- table : TInterface (Name {namespace = "Gtk", name = "Table"})
    IO Word32

{-# DEPRECATED tableGetDefaultRowSpacing ["(Since version 3.4)","Use 'GI.Gtk.Objects.Grid.gridGetRowSpacing' with t'GI.Gtk.Objects.Grid.Grid'."] #-}
-- | Gets the default row spacing for the table. This is
-- the spacing that will be used for newly added rows.
-- (See 'GI.Gtk.Objects.Table.tableSetRowSpacings')
tableGetDefaultRowSpacing ::
    (B.CallStack.HasCallStack, MonadIO m, IsTable a) =>
    a
    -- ^ /@table@/: a t'GI.Gtk.Objects.Table.Table'
    -> m Word32
    -- ^ __Returns:__ the default row spacing
tableGetDefaultRowSpacing table = liftIO $ do
    table' <- unsafeManagedPtrCastPtr table
    result <- gtk_table_get_default_row_spacing table'
    touchManagedPtr table
    return result

#if defined(ENABLE_OVERLOADING)
data TableGetDefaultRowSpacingMethodInfo
instance (signature ~ (m Word32), MonadIO m, IsTable a) => O.OverloadedMethod TableGetDefaultRowSpacingMethodInfo a signature where
    overloadedMethod = tableGetDefaultRowSpacing

instance O.OverloadedMethodInfo TableGetDefaultRowSpacingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Table.tableGetDefaultRowSpacing",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Table.html#v:tableGetDefaultRowSpacing"
        })


#endif

-- method Table::get_homogeneous
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "table"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Table" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTable" , sinceVersion = Nothing }
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

foreign import ccall "gtk_table_get_homogeneous" gtk_table_get_homogeneous :: 
    Ptr Table ->                            -- table : TInterface (Name {namespace = "Gtk", name = "Table"})
    IO CInt

{-# DEPRECATED tableGetHomogeneous ["(Since version 3.4)","Use 'GI.Gtk.Objects.Grid.gridGetRowHomogeneous' and","    'GI.Gtk.Objects.Grid.gridGetColumnHomogeneous' with t'GI.Gtk.Objects.Grid.Grid'."] #-}
-- | Returns whether the table cells are all constrained to the same
-- width and height. (See gtk_table_set_homogeneous ())
tableGetHomogeneous ::
    (B.CallStack.HasCallStack, MonadIO m, IsTable a) =>
    a
    -- ^ /@table@/: a t'GI.Gtk.Objects.Table.Table'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the cells are all constrained to the same size
tableGetHomogeneous table = liftIO $ do
    table' <- unsafeManagedPtrCastPtr table
    result <- gtk_table_get_homogeneous table'
    let result' = (/= 0) result
    touchManagedPtr table
    return result'

#if defined(ENABLE_OVERLOADING)
data TableGetHomogeneousMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsTable a) => O.OverloadedMethod TableGetHomogeneousMethodInfo a signature where
    overloadedMethod = tableGetHomogeneous

instance O.OverloadedMethodInfo TableGetHomogeneousMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Table.tableGetHomogeneous",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Table.html#v:tableGetHomogeneous"
        })


#endif

-- method Table::get_row_spacing
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "table"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Table" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTable" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "row"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a row in the table, 0 indicates the first row"
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
-- returnType: Just (TBasicType TUInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_table_get_row_spacing" gtk_table_get_row_spacing :: 
    Ptr Table ->                            -- table : TInterface (Name {namespace = "Gtk", name = "Table"})
    Word32 ->                               -- row : TBasicType TUInt
    IO Word32

{-# DEPRECATED tableGetRowSpacing ["(Since version 3.4)","t'GI.Gtk.Objects.Grid.Grid' does not offer a replacement for this","    functionality."] #-}
-- | Gets the amount of space between row /@row@/, and
-- row /@row@/ + 1. See 'GI.Gtk.Objects.Table.tableSetRowSpacing'.
tableGetRowSpacing ::
    (B.CallStack.HasCallStack, MonadIO m, IsTable a) =>
    a
    -- ^ /@table@/: a t'GI.Gtk.Objects.Table.Table'
    -> Word32
    -- ^ /@row@/: a row in the table, 0 indicates the first row
    -> m Word32
    -- ^ __Returns:__ the row spacing
tableGetRowSpacing table row = liftIO $ do
    table' <- unsafeManagedPtrCastPtr table
    result <- gtk_table_get_row_spacing table' row
    touchManagedPtr table
    return result

#if defined(ENABLE_OVERLOADING)
data TableGetRowSpacingMethodInfo
instance (signature ~ (Word32 -> m Word32), MonadIO m, IsTable a) => O.OverloadedMethod TableGetRowSpacingMethodInfo a signature where
    overloadedMethod = tableGetRowSpacing

instance O.OverloadedMethodInfo TableGetRowSpacingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Table.tableGetRowSpacing",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Table.html#v:tableGetRowSpacing"
        })


#endif

-- method Table::get_size
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "table"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Table" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTable" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "rows"
--           , argType = TBasicType TUInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "return location for the number of\n  rows, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "columns"
--           , argType = TBasicType TUInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "return location for the number\n  of columns, or %NULL"
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

foreign import ccall "gtk_table_get_size" gtk_table_get_size :: 
    Ptr Table ->                            -- table : TInterface (Name {namespace = "Gtk", name = "Table"})
    Ptr Word32 ->                           -- rows : TBasicType TUInt
    Ptr Word32 ->                           -- columns : TBasicType TUInt
    IO ()

{-# DEPRECATED tableGetSize ["(Since version 3.4)","t'GI.Gtk.Objects.Grid.Grid' does not expose the number of columns and","    rows."] #-}
-- | Gets the number of rows and columns in the table.
-- 
-- /Since: 2.22/
tableGetSize ::
    (B.CallStack.HasCallStack, MonadIO m, IsTable a) =>
    a
    -- ^ /@table@/: a t'GI.Gtk.Objects.Table.Table'
    -> m ((Word32, Word32))
tableGetSize table = liftIO $ do
    table' <- unsafeManagedPtrCastPtr table
    rows <- allocMem :: IO (Ptr Word32)
    columns <- allocMem :: IO (Ptr Word32)
    gtk_table_get_size table' rows columns
    rows' <- peek rows
    columns' <- peek columns
    touchManagedPtr table
    freeMem rows
    freeMem columns
    return (rows', columns')

#if defined(ENABLE_OVERLOADING)
data TableGetSizeMethodInfo
instance (signature ~ (m ((Word32, Word32))), MonadIO m, IsTable a) => O.OverloadedMethod TableGetSizeMethodInfo a signature where
    overloadedMethod = tableGetSize

instance O.OverloadedMethodInfo TableGetSizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Table.tableGetSize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Table.html#v:tableGetSize"
        })


#endif

-- method Table::resize
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "table"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Table" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The #GtkTable you wish to change the size of."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "rows"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The new number of rows."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "columns"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The new number of columns."
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

foreign import ccall "gtk_table_resize" gtk_table_resize :: 
    Ptr Table ->                            -- table : TInterface (Name {namespace = "Gtk", name = "Table"})
    Word32 ->                               -- rows : TBasicType TUInt
    Word32 ->                               -- columns : TBasicType TUInt
    IO ()

{-# DEPRECATED tableResize ["(Since version 3.4)","t'GI.Gtk.Objects.Grid.Grid' resizes automatically."] #-}
-- | If you need to change a table’s size after
-- it has been created, this function allows you to do so.
tableResize ::
    (B.CallStack.HasCallStack, MonadIO m, IsTable a) =>
    a
    -- ^ /@table@/: The t'GI.Gtk.Objects.Table.Table' you wish to change the size of.
    -> Word32
    -- ^ /@rows@/: The new number of rows.
    -> Word32
    -- ^ /@columns@/: The new number of columns.
    -> m ()
tableResize table rows columns = liftIO $ do
    table' <- unsafeManagedPtrCastPtr table
    gtk_table_resize table' rows columns
    touchManagedPtr table
    return ()

#if defined(ENABLE_OVERLOADING)
data TableResizeMethodInfo
instance (signature ~ (Word32 -> Word32 -> m ()), MonadIO m, IsTable a) => O.OverloadedMethod TableResizeMethodInfo a signature where
    overloadedMethod = tableResize

instance O.OverloadedMethodInfo TableResizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Table.tableResize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Table.html#v:tableResize"
        })


#endif

-- method Table::set_col_spacing
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "table"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Table" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTable." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "column"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the column whose spacing should be changed."
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
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "number of pixels that the spacing should take up."
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

foreign import ccall "gtk_table_set_col_spacing" gtk_table_set_col_spacing :: 
    Ptr Table ->                            -- table : TInterface (Name {namespace = "Gtk", name = "Table"})
    Word32 ->                               -- column : TBasicType TUInt
    Word32 ->                               -- spacing : TBasicType TUInt
    IO ()

{-# DEPRECATED tableSetColSpacing ["(Since version 3.4)","Use 'GI.Gtk.Objects.Widget.widgetSetMarginStart' and","    'GI.Gtk.Objects.Widget.widgetSetMarginEnd' on the widgets contained in the row if","    you need this functionality. t'GI.Gtk.Objects.Grid.Grid' does not support per-row spacing."] #-}
-- | Alters the amount of space between a given table column and the following
-- column.
tableSetColSpacing ::
    (B.CallStack.HasCallStack, MonadIO m, IsTable a) =>
    a
    -- ^ /@table@/: a t'GI.Gtk.Objects.Table.Table'.
    -> Word32
    -- ^ /@column@/: the column whose spacing should be changed.
    -> Word32
    -- ^ /@spacing@/: number of pixels that the spacing should take up.
    -> m ()
tableSetColSpacing table column spacing = liftIO $ do
    table' <- unsafeManagedPtrCastPtr table
    gtk_table_set_col_spacing table' column spacing
    touchManagedPtr table
    return ()

#if defined(ENABLE_OVERLOADING)
data TableSetColSpacingMethodInfo
instance (signature ~ (Word32 -> Word32 -> m ()), MonadIO m, IsTable a) => O.OverloadedMethod TableSetColSpacingMethodInfo a signature where
    overloadedMethod = tableSetColSpacing

instance O.OverloadedMethodInfo TableSetColSpacingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Table.tableSetColSpacing",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Table.html#v:tableSetColSpacing"
        })


#endif

-- method Table::set_col_spacings
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "table"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Table" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTable." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "spacing"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the number of pixels of space to place between every column\n  in the table."
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

foreign import ccall "gtk_table_set_col_spacings" gtk_table_set_col_spacings :: 
    Ptr Table ->                            -- table : TInterface (Name {namespace = "Gtk", name = "Table"})
    Word32 ->                               -- spacing : TBasicType TUInt
    IO ()

{-# DEPRECATED tableSetColSpacings ["(Since version 3.4)","Use 'GI.Gtk.Objects.Grid.gridSetColumnSpacing' with t'GI.Gtk.Objects.Grid.Grid'."] #-}
-- | Sets the space between every column in /@table@/ equal to /@spacing@/.
tableSetColSpacings ::
    (B.CallStack.HasCallStack, MonadIO m, IsTable a) =>
    a
    -- ^ /@table@/: a t'GI.Gtk.Objects.Table.Table'.
    -> Word32
    -- ^ /@spacing@/: the number of pixels of space to place between every column
    --   in the table.
    -> m ()
tableSetColSpacings table spacing = liftIO $ do
    table' <- unsafeManagedPtrCastPtr table
    gtk_table_set_col_spacings table' spacing
    touchManagedPtr table
    return ()

#if defined(ENABLE_OVERLOADING)
data TableSetColSpacingsMethodInfo
instance (signature ~ (Word32 -> m ()), MonadIO m, IsTable a) => O.OverloadedMethod TableSetColSpacingsMethodInfo a signature where
    overloadedMethod = tableSetColSpacings

instance O.OverloadedMethodInfo TableSetColSpacingsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Table.tableSetColSpacings",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Table.html#v:tableSetColSpacings"
        })


#endif

-- method Table::set_homogeneous
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "table"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Table" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The #GtkTable you wish to set the homogeneous properties of."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "homogeneous"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "Set to %TRUE to ensure all table cells are the same size. Set\n  to %FALSE if this is not your desired behaviour."
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

foreign import ccall "gtk_table_set_homogeneous" gtk_table_set_homogeneous :: 
    Ptr Table ->                            -- table : TInterface (Name {namespace = "Gtk", name = "Table"})
    CInt ->                                 -- homogeneous : TBasicType TBoolean
    IO ()

{-# DEPRECATED tableSetHomogeneous ["(Since version 3.4)","Use 'GI.Gtk.Objects.Grid.gridSetRowHomogeneous' and","    'GI.Gtk.Objects.Grid.gridSetColumnHomogeneous' with t'GI.Gtk.Objects.Grid.Grid'."] #-}
-- | Changes the homogenous property of table cells, ie. whether all cells are
-- an equal size or not.
tableSetHomogeneous ::
    (B.CallStack.HasCallStack, MonadIO m, IsTable a) =>
    a
    -- ^ /@table@/: The t'GI.Gtk.Objects.Table.Table' you wish to set the homogeneous properties of.
    -> Bool
    -- ^ /@homogeneous@/: Set to 'P.True' to ensure all table cells are the same size. Set
    --   to 'P.False' if this is not your desired behaviour.
    -> m ()
tableSetHomogeneous table homogeneous = liftIO $ do
    table' <- unsafeManagedPtrCastPtr table
    let homogeneous' = (fromIntegral . fromEnum) homogeneous
    gtk_table_set_homogeneous table' homogeneous'
    touchManagedPtr table
    return ()

#if defined(ENABLE_OVERLOADING)
data TableSetHomogeneousMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsTable a) => O.OverloadedMethod TableSetHomogeneousMethodInfo a signature where
    overloadedMethod = tableSetHomogeneous

instance O.OverloadedMethodInfo TableSetHomogeneousMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Table.tableSetHomogeneous",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Table.html#v:tableSetHomogeneous"
        })


#endif

-- method Table::set_row_spacing
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "table"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Table" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a #GtkTable containing the row whose properties you wish to change."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "row"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "row number whose spacing will be changed."
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
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "number of pixels that the spacing should take up."
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

foreign import ccall "gtk_table_set_row_spacing" gtk_table_set_row_spacing :: 
    Ptr Table ->                            -- table : TInterface (Name {namespace = "Gtk", name = "Table"})
    Word32 ->                               -- row : TBasicType TUInt
    Word32 ->                               -- spacing : TBasicType TUInt
    IO ()

{-# DEPRECATED tableSetRowSpacing ["(Since version 3.4)","Use 'GI.Gtk.Objects.Widget.widgetSetMarginTop' and","    'GI.Gtk.Objects.Widget.widgetSetMarginBottom' on the widgets contained in the row if","    you need this functionality. t'GI.Gtk.Objects.Grid.Grid' does not support per-row spacing."] #-}
-- | Changes the space between a given table row and the subsequent row.
tableSetRowSpacing ::
    (B.CallStack.HasCallStack, MonadIO m, IsTable a) =>
    a
    -- ^ /@table@/: a t'GI.Gtk.Objects.Table.Table' containing the row whose properties you wish to change.
    -> Word32
    -- ^ /@row@/: row number whose spacing will be changed.
    -> Word32
    -- ^ /@spacing@/: number of pixels that the spacing should take up.
    -> m ()
tableSetRowSpacing table row spacing = liftIO $ do
    table' <- unsafeManagedPtrCastPtr table
    gtk_table_set_row_spacing table' row spacing
    touchManagedPtr table
    return ()

#if defined(ENABLE_OVERLOADING)
data TableSetRowSpacingMethodInfo
instance (signature ~ (Word32 -> Word32 -> m ()), MonadIO m, IsTable a) => O.OverloadedMethod TableSetRowSpacingMethodInfo a signature where
    overloadedMethod = tableSetRowSpacing

instance O.OverloadedMethodInfo TableSetRowSpacingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Table.tableSetRowSpacing",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Table.html#v:tableSetRowSpacing"
        })


#endif

-- method Table::set_row_spacings
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "table"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Table" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTable." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "spacing"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the number of pixels of space to place between every row in the table."
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

foreign import ccall "gtk_table_set_row_spacings" gtk_table_set_row_spacings :: 
    Ptr Table ->                            -- table : TInterface (Name {namespace = "Gtk", name = "Table"})
    Word32 ->                               -- spacing : TBasicType TUInt
    IO ()

{-# DEPRECATED tableSetRowSpacings ["(Since version 3.4)","Use 'GI.Gtk.Objects.Grid.gridSetRowSpacing' with t'GI.Gtk.Objects.Grid.Grid'."] #-}
-- | Sets the space between every row in /@table@/ equal to /@spacing@/.
tableSetRowSpacings ::
    (B.CallStack.HasCallStack, MonadIO m, IsTable a) =>
    a
    -- ^ /@table@/: a t'GI.Gtk.Objects.Table.Table'.
    -> Word32
    -- ^ /@spacing@/: the number of pixels of space to place between every row in the table.
    -> m ()
tableSetRowSpacings table spacing = liftIO $ do
    table' <- unsafeManagedPtrCastPtr table
    gtk_table_set_row_spacings table' spacing
    touchManagedPtr table
    return ()

#if defined(ENABLE_OVERLOADING)
data TableSetRowSpacingsMethodInfo
instance (signature ~ (Word32 -> m ()), MonadIO m, IsTable a) => O.OverloadedMethod TableSetRowSpacingsMethodInfo a signature where
    overloadedMethod = tableSetRowSpacings

instance O.OverloadedMethodInfo TableSetRowSpacingsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Table.tableSetRowSpacings",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Table.html#v:tableSetRowSpacings"
        })


#endif


