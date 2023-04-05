{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Objects.Layout.Layout' is similar to t'GI.Gtk.Objects.DrawingArea.DrawingArea' in that it’s a “blank slate” and
-- doesn’t do anything except paint a blank background by default. It’s
-- different in that it supports scrolling natively due to implementing
-- t'GI.Gtk.Interfaces.Scrollable.Scrollable', and can contain child widgets since it’s a t'GI.Gtk.Objects.Container.Container'.
-- 
-- If you just want to draw, a t'GI.Gtk.Objects.DrawingArea.DrawingArea' is a better choice since it has
-- lower overhead. If you just need to position child widgets at specific
-- points, then t'GI.Gtk.Objects.Fixed.Fixed' provides that functionality on its own.
-- 
-- When handling expose events on a t'GI.Gtk.Objects.Layout.Layout', you must draw to the t'GI.Gdk.Objects.Window.Window'
-- returned by 'GI.Gtk.Objects.Layout.layoutGetBinWindow', rather than to the one returned by
-- 'GI.Gtk.Objects.Widget.widgetGetWindow' as you would for a t'GI.Gtk.Objects.DrawingArea.DrawingArea'.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.Layout
    ( 

-- * Exported types
    Layout(..)                              ,
    IsLayout                                ,
    toLayout                                ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [move]("GI.Gtk.Objects.Layout#g:method:move"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [put]("GI.Gtk.Objects.Layout#g:method:put"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getBinWindow]("GI.Gtk.Objects.Layout#g:method:getBinWindow"), [getBorder]("GI.Gtk.Interfaces.Scrollable#g:method:getBorder"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHadjustment]("GI.Gtk.Objects.Layout#g:method:getHadjustment"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getHscrollPolicy]("GI.Gtk.Interfaces.Scrollable#g:method:getHscrollPolicy"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSize]("GI.Gtk.Objects.Layout#g:method:getSize"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getVadjustment]("GI.Gtk.Objects.Layout#g:method:getVadjustment"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getVscrollPolicy]("GI.Gtk.Interfaces.Scrollable#g:method:getVscrollPolicy"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHadjustment]("GI.Gtk.Objects.Layout#g:method:setHadjustment"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setHscrollPolicy]("GI.Gtk.Interfaces.Scrollable#g:method:setHscrollPolicy"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSize]("GI.Gtk.Objects.Layout#g:method:setSize"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setVadjustment]("GI.Gtk.Objects.Layout#g:method:setVadjustment"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setVscrollPolicy]("GI.Gtk.Interfaces.Scrollable#g:method:setVscrollPolicy"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveLayoutMethod                     ,
#endif

-- ** getBinWindow #method:getBinWindow#

#if defined(ENABLE_OVERLOADING)
    LayoutGetBinWindowMethodInfo            ,
#endif
    layoutGetBinWindow                      ,


-- ** getHadjustment #method:getHadjustment#

#if defined(ENABLE_OVERLOADING)
    LayoutGetHadjustmentMethodInfo          ,
#endif
    layoutGetHadjustment                    ,


-- ** getSize #method:getSize#

#if defined(ENABLE_OVERLOADING)
    LayoutGetSizeMethodInfo                 ,
#endif
    layoutGetSize                           ,


-- ** getVadjustment #method:getVadjustment#

#if defined(ENABLE_OVERLOADING)
    LayoutGetVadjustmentMethodInfo          ,
#endif
    layoutGetVadjustment                    ,


-- ** move #method:move#

#if defined(ENABLE_OVERLOADING)
    LayoutMoveMethodInfo                    ,
#endif
    layoutMove                              ,


-- ** new #method:new#

    layoutNew                               ,


-- ** put #method:put#

#if defined(ENABLE_OVERLOADING)
    LayoutPutMethodInfo                     ,
#endif
    layoutPut                               ,


-- ** setHadjustment #method:setHadjustment#

#if defined(ENABLE_OVERLOADING)
    LayoutSetHadjustmentMethodInfo          ,
#endif
    layoutSetHadjustment                    ,


-- ** setSize #method:setSize#

#if defined(ENABLE_OVERLOADING)
    LayoutSetSizeMethodInfo                 ,
#endif
    layoutSetSize                           ,


-- ** setVadjustment #method:setVadjustment#

#if defined(ENABLE_OVERLOADING)
    LayoutSetVadjustmentMethodInfo          ,
#endif
    layoutSetVadjustment                    ,




 -- * Properties


-- ** height #attr:height#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    LayoutHeightPropertyInfo                ,
#endif
    constructLayoutHeight                   ,
    getLayoutHeight                         ,
#if defined(ENABLE_OVERLOADING)
    layoutHeight                            ,
#endif
    setLayoutHeight                         ,


-- ** width #attr:width#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    LayoutWidthPropertyInfo                 ,
#endif
    constructLayoutWidth                    ,
    getLayoutWidth                          ,
#if defined(ENABLE_OVERLOADING)
    layoutWidth                             ,
#endif
    setLayoutWidth                          ,




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
import qualified GI.Gdk.Objects.Window as Gdk.Window
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Scrollable as Gtk.Scrollable
import {-# SOURCE #-} qualified GI.Gtk.Objects.Adjustment as Gtk.Adjustment
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype Layout = Layout (SP.ManagedPtr Layout)
    deriving (Eq)

instance SP.ManagedPtrNewtype Layout where
    toManagedPtr (Layout p) = p

foreign import ccall "gtk_layout_get_type"
    c_gtk_layout_get_type :: IO B.Types.GType

instance B.Types.TypedObject Layout where
    glibType = c_gtk_layout_get_type

instance B.Types.GObject Layout

-- | Type class for types which can be safely cast to `Layout`, for instance with `toLayout`.
class (SP.GObject o, O.IsDescendantOf Layout o) => IsLayout o
instance (SP.GObject o, O.IsDescendantOf Layout o) => IsLayout o

instance O.HasParentTypes Layout
type instance O.ParentTypes Layout = '[Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable, Gtk.Scrollable.Scrollable]

-- | Cast to `Layout`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toLayout :: (MIO.MonadIO m, IsLayout o) => o -> m Layout
toLayout = MIO.liftIO . B.ManagedPtr.unsafeCastTo Layout

-- | Convert 'Layout' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe Layout) where
    gvalueGType_ = c_gtk_layout_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr Layout)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr Layout)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject Layout ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveLayoutMethod (t :: Symbol) (o :: *) :: * where
    ResolveLayoutMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveLayoutMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveLayoutMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveLayoutMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveLayoutMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveLayoutMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveLayoutMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveLayoutMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveLayoutMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveLayoutMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveLayoutMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveLayoutMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveLayoutMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveLayoutMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveLayoutMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveLayoutMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveLayoutMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveLayoutMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveLayoutMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveLayoutMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveLayoutMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveLayoutMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveLayoutMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveLayoutMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveLayoutMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveLayoutMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveLayoutMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveLayoutMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveLayoutMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveLayoutMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveLayoutMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveLayoutMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveLayoutMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveLayoutMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveLayoutMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveLayoutMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveLayoutMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveLayoutMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveLayoutMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveLayoutMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveLayoutMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveLayoutMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveLayoutMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveLayoutMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveLayoutMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveLayoutMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveLayoutMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveLayoutMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveLayoutMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveLayoutMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveLayoutMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveLayoutMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveLayoutMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveLayoutMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveLayoutMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveLayoutMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveLayoutMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveLayoutMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveLayoutMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveLayoutMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveLayoutMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveLayoutMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveLayoutMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveLayoutMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveLayoutMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveLayoutMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveLayoutMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveLayoutMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveLayoutMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveLayoutMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveLayoutMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveLayoutMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveLayoutMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveLayoutMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveLayoutMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveLayoutMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveLayoutMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveLayoutMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveLayoutMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveLayoutMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveLayoutMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveLayoutMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveLayoutMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveLayoutMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveLayoutMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveLayoutMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveLayoutMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveLayoutMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveLayoutMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveLayoutMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveLayoutMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveLayoutMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveLayoutMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveLayoutMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveLayoutMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveLayoutMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveLayoutMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveLayoutMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveLayoutMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveLayoutMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveLayoutMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveLayoutMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveLayoutMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveLayoutMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveLayoutMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveLayoutMethod "move" o = LayoutMoveMethodInfo
    ResolveLayoutMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveLayoutMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveLayoutMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveLayoutMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveLayoutMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveLayoutMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveLayoutMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveLayoutMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveLayoutMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveLayoutMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveLayoutMethod "put" o = LayoutPutMethodInfo
    ResolveLayoutMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveLayoutMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveLayoutMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveLayoutMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveLayoutMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveLayoutMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveLayoutMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveLayoutMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveLayoutMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveLayoutMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveLayoutMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveLayoutMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveLayoutMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolveLayoutMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveLayoutMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveLayoutMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveLayoutMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveLayoutMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveLayoutMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveLayoutMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveLayoutMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveLayoutMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveLayoutMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveLayoutMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveLayoutMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveLayoutMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveLayoutMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveLayoutMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveLayoutMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveLayoutMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveLayoutMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveLayoutMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveLayoutMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveLayoutMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveLayoutMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveLayoutMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveLayoutMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveLayoutMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveLayoutMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveLayoutMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveLayoutMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveLayoutMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveLayoutMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveLayoutMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveLayoutMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveLayoutMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveLayoutMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveLayoutMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveLayoutMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveLayoutMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveLayoutMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveLayoutMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveLayoutMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveLayoutMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveLayoutMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveLayoutMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveLayoutMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveLayoutMethod "getBinWindow" o = LayoutGetBinWindowMethodInfo
    ResolveLayoutMethod "getBorder" o = Gtk.Scrollable.ScrollableGetBorderMethodInfo
    ResolveLayoutMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveLayoutMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveLayoutMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveLayoutMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveLayoutMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveLayoutMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveLayoutMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveLayoutMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveLayoutMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveLayoutMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveLayoutMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveLayoutMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveLayoutMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveLayoutMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveLayoutMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveLayoutMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveLayoutMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveLayoutMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveLayoutMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveLayoutMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveLayoutMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveLayoutMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveLayoutMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveLayoutMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveLayoutMethod "getHadjustment" o = LayoutGetHadjustmentMethodInfo
    ResolveLayoutMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveLayoutMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveLayoutMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveLayoutMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveLayoutMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveLayoutMethod "getHscrollPolicy" o = Gtk.Scrollable.ScrollableGetHscrollPolicyMethodInfo
    ResolveLayoutMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveLayoutMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveLayoutMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveLayoutMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveLayoutMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveLayoutMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveLayoutMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveLayoutMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveLayoutMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveLayoutMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveLayoutMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveLayoutMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveLayoutMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveLayoutMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveLayoutMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveLayoutMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveLayoutMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveLayoutMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveLayoutMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveLayoutMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveLayoutMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveLayoutMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveLayoutMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveLayoutMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveLayoutMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveLayoutMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveLayoutMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveLayoutMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveLayoutMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveLayoutMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveLayoutMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveLayoutMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveLayoutMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveLayoutMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveLayoutMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveLayoutMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveLayoutMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveLayoutMethod "getSize" o = LayoutGetSizeMethodInfo
    ResolveLayoutMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveLayoutMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveLayoutMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveLayoutMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveLayoutMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveLayoutMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveLayoutMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveLayoutMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveLayoutMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveLayoutMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveLayoutMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveLayoutMethod "getVadjustment" o = LayoutGetVadjustmentMethodInfo
    ResolveLayoutMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveLayoutMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveLayoutMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveLayoutMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveLayoutMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveLayoutMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveLayoutMethod "getVscrollPolicy" o = Gtk.Scrollable.ScrollableGetVscrollPolicyMethodInfo
    ResolveLayoutMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveLayoutMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveLayoutMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveLayoutMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveLayoutMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveLayoutMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveLayoutMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveLayoutMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveLayoutMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveLayoutMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveLayoutMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveLayoutMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveLayoutMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveLayoutMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveLayoutMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveLayoutMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveLayoutMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveLayoutMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveLayoutMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveLayoutMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveLayoutMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveLayoutMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveLayoutMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveLayoutMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveLayoutMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveLayoutMethod "setHadjustment" o = LayoutSetHadjustmentMethodInfo
    ResolveLayoutMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveLayoutMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveLayoutMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveLayoutMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveLayoutMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveLayoutMethod "setHscrollPolicy" o = Gtk.Scrollable.ScrollableSetHscrollPolicyMethodInfo
    ResolveLayoutMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveLayoutMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveLayoutMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveLayoutMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveLayoutMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveLayoutMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveLayoutMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveLayoutMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveLayoutMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveLayoutMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveLayoutMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveLayoutMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveLayoutMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveLayoutMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveLayoutMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveLayoutMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveLayoutMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveLayoutMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveLayoutMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveLayoutMethod "setSize" o = LayoutSetSizeMethodInfo
    ResolveLayoutMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveLayoutMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveLayoutMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveLayoutMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveLayoutMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveLayoutMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveLayoutMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveLayoutMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveLayoutMethod "setVadjustment" o = LayoutSetVadjustmentMethodInfo
    ResolveLayoutMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveLayoutMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveLayoutMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveLayoutMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveLayoutMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveLayoutMethod "setVscrollPolicy" o = Gtk.Scrollable.ScrollableSetVscrollPolicyMethodInfo
    ResolveLayoutMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveLayoutMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveLayoutMethod t Layout, O.OverloadedMethod info Layout p) => OL.IsLabel t (Layout -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveLayoutMethod t Layout, O.OverloadedMethod info Layout p, R.HasField t Layout p) => R.HasField t Layout p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveLayoutMethod t Layout, O.OverloadedMethodInfo info Layout) => OL.IsLabel t (O.MethodProxy info Layout) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- VVV Prop "height"
   -- Type: TBasicType TUInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@height@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' layout #height
-- @
getLayoutHeight :: (MonadIO m, IsLayout o) => o -> m Word32
getLayoutHeight obj = MIO.liftIO $ B.Properties.getObjectPropertyUInt32 obj "height"

-- | Set the value of the “@height@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' layout [ #height 'Data.GI.Base.Attributes.:=' value ]
-- @
setLayoutHeight :: (MonadIO m, IsLayout o) => o -> Word32 -> m ()
setLayoutHeight obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyUInt32 obj "height" val

-- | Construct a `GValueConstruct` with valid value for the “@height@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructLayoutHeight :: (IsLayout o, MIO.MonadIO m) => Word32 -> m (GValueConstruct o)
constructLayoutHeight val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyUInt32 "height" val

#if defined(ENABLE_OVERLOADING)
data LayoutHeightPropertyInfo
instance AttrInfo LayoutHeightPropertyInfo where
    type AttrAllowedOps LayoutHeightPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint LayoutHeightPropertyInfo = IsLayout
    type AttrSetTypeConstraint LayoutHeightPropertyInfo = (~) Word32
    type AttrTransferTypeConstraint LayoutHeightPropertyInfo = (~) Word32
    type AttrTransferType LayoutHeightPropertyInfo = Word32
    type AttrGetType LayoutHeightPropertyInfo = Word32
    type AttrLabel LayoutHeightPropertyInfo = "height"
    type AttrOrigin LayoutHeightPropertyInfo = Layout
    attrGet = getLayoutHeight
    attrSet = setLayoutHeight
    attrTransfer _ v = do
        return v
    attrConstruct = constructLayoutHeight
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Layout.height"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Layout.html#g:attr:height"
        })
#endif

-- VVV Prop "width"
   -- Type: TBasicType TUInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@width@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' layout #width
-- @
getLayoutWidth :: (MonadIO m, IsLayout o) => o -> m Word32
getLayoutWidth obj = MIO.liftIO $ B.Properties.getObjectPropertyUInt32 obj "width"

-- | Set the value of the “@width@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' layout [ #width 'Data.GI.Base.Attributes.:=' value ]
-- @
setLayoutWidth :: (MonadIO m, IsLayout o) => o -> Word32 -> m ()
setLayoutWidth obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyUInt32 obj "width" val

-- | Construct a `GValueConstruct` with valid value for the “@width@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructLayoutWidth :: (IsLayout o, MIO.MonadIO m) => Word32 -> m (GValueConstruct o)
constructLayoutWidth val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyUInt32 "width" val

#if defined(ENABLE_OVERLOADING)
data LayoutWidthPropertyInfo
instance AttrInfo LayoutWidthPropertyInfo where
    type AttrAllowedOps LayoutWidthPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint LayoutWidthPropertyInfo = IsLayout
    type AttrSetTypeConstraint LayoutWidthPropertyInfo = (~) Word32
    type AttrTransferTypeConstraint LayoutWidthPropertyInfo = (~) Word32
    type AttrTransferType LayoutWidthPropertyInfo = Word32
    type AttrGetType LayoutWidthPropertyInfo = Word32
    type AttrLabel LayoutWidthPropertyInfo = "width"
    type AttrOrigin LayoutWidthPropertyInfo = Layout
    attrGet = getLayoutWidth
    attrSet = setLayoutWidth
    attrTransfer _ v = do
        return v
    attrConstruct = constructLayoutWidth
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Layout.width"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Layout.html#g:attr:width"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList Layout
type instance O.AttributeList Layout = LayoutAttributeList
type LayoutAttributeList = ('[ '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("hadjustment", Gtk.Scrollable.ScrollableHadjustmentPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("height", LayoutHeightPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("hscrollPolicy", Gtk.Scrollable.ScrollableHscrollPolicyPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("vadjustment", Gtk.Scrollable.ScrollableVadjustmentPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("vscrollPolicy", Gtk.Scrollable.ScrollableVscrollPolicyPropertyInfo), '("width", LayoutWidthPropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
layoutHeight :: AttrLabelProxy "height"
layoutHeight = AttrLabelProxy

layoutWidth :: AttrLabelProxy "width"
layoutWidth = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList Layout = LayoutSignalList
type LayoutSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method Layout::new
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "hadjustment"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Adjustment" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "horizontal scroll adjustment, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "vadjustment"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Adjustment" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "vertical scroll adjustment, or %NULL"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Layout" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_layout_new" gtk_layout_new :: 
    Ptr Gtk.Adjustment.Adjustment ->        -- hadjustment : TInterface (Name {namespace = "Gtk", name = "Adjustment"})
    Ptr Gtk.Adjustment.Adjustment ->        -- vadjustment : TInterface (Name {namespace = "Gtk", name = "Adjustment"})
    IO (Ptr Layout)

-- | Creates a new t'GI.Gtk.Objects.Layout.Layout'. Unless you have a specific adjustment
-- you’d like the layout to use for scrolling, pass 'P.Nothing' for
-- /@hadjustment@/ and /@vadjustment@/.
layoutNew ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Adjustment.IsAdjustment a, Gtk.Adjustment.IsAdjustment b) =>
    Maybe (a)
    -- ^ /@hadjustment@/: horizontal scroll adjustment, or 'P.Nothing'
    -> Maybe (b)
    -- ^ /@vadjustment@/: vertical scroll adjustment, or 'P.Nothing'
    -> m Layout
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.Layout.Layout'
layoutNew hadjustment vadjustment = liftIO $ do
    maybeHadjustment <- case hadjustment of
        Nothing -> return nullPtr
        Just jHadjustment -> do
            jHadjustment' <- unsafeManagedPtrCastPtr jHadjustment
            return jHadjustment'
    maybeVadjustment <- case vadjustment of
        Nothing -> return nullPtr
        Just jVadjustment -> do
            jVadjustment' <- unsafeManagedPtrCastPtr jVadjustment
            return jVadjustment'
    result <- gtk_layout_new maybeHadjustment maybeVadjustment
    checkUnexpectedReturnNULL "layoutNew" result
    result' <- (newObject Layout) result
    whenJust hadjustment touchManagedPtr
    whenJust vadjustment touchManagedPtr
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Layout::get_bin_window
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "layout"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Layout" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLayout" , sinceVersion = Nothing }
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

foreign import ccall "gtk_layout_get_bin_window" gtk_layout_get_bin_window :: 
    Ptr Layout ->                           -- layout : TInterface (Name {namespace = "Gtk", name = "Layout"})
    IO (Ptr Gdk.Window.Window)

-- | Retrieve the bin window of the layout used for drawing operations.
-- 
-- /Since: 2.14/
layoutGetBinWindow ::
    (B.CallStack.HasCallStack, MonadIO m, IsLayout a) =>
    a
    -- ^ /@layout@/: a t'GI.Gtk.Objects.Layout.Layout'
    -> m Gdk.Window.Window
    -- ^ __Returns:__ a t'GI.Gdk.Objects.Window.Window'
layoutGetBinWindow layout = liftIO $ do
    layout' <- unsafeManagedPtrCastPtr layout
    result <- gtk_layout_get_bin_window layout'
    checkUnexpectedReturnNULL "layoutGetBinWindow" result
    result' <- (newObject Gdk.Window.Window) result
    touchManagedPtr layout
    return result'

#if defined(ENABLE_OVERLOADING)
data LayoutGetBinWindowMethodInfo
instance (signature ~ (m Gdk.Window.Window), MonadIO m, IsLayout a) => O.OverloadedMethod LayoutGetBinWindowMethodInfo a signature where
    overloadedMethod = layoutGetBinWindow

instance O.OverloadedMethodInfo LayoutGetBinWindowMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Layout.layoutGetBinWindow",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Layout.html#v:layoutGetBinWindow"
        })


#endif

-- method Layout::get_hadjustment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "layout"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Layout" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLayout" , sinceVersion = Nothing }
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

foreign import ccall "gtk_layout_get_hadjustment" gtk_layout_get_hadjustment :: 
    Ptr Layout ->                           -- layout : TInterface (Name {namespace = "Gtk", name = "Layout"})
    IO (Ptr Gtk.Adjustment.Adjustment)

{-# DEPRECATED layoutGetHadjustment ["(Since version 3.0)","Use 'GI.Gtk.Interfaces.Scrollable.scrollableGetHadjustment'"] #-}
-- | This function should only be called after the layout has been
-- placed in a t'GI.Gtk.Objects.ScrolledWindow.ScrolledWindow' or otherwise configured for
-- scrolling. It returns the t'GI.Gtk.Objects.Adjustment.Adjustment' used for communication
-- between the horizontal scrollbar and /@layout@/.
-- 
-- See t'GI.Gtk.Objects.ScrolledWindow.ScrolledWindow', t'GI.Gtk.Objects.Scrollbar.Scrollbar', t'GI.Gtk.Objects.Adjustment.Adjustment' for details.
layoutGetHadjustment ::
    (B.CallStack.HasCallStack, MonadIO m, IsLayout a) =>
    a
    -- ^ /@layout@/: a t'GI.Gtk.Objects.Layout.Layout'
    -> m Gtk.Adjustment.Adjustment
    -- ^ __Returns:__ horizontal scroll adjustment
layoutGetHadjustment layout = liftIO $ do
    layout' <- unsafeManagedPtrCastPtr layout
    result <- gtk_layout_get_hadjustment layout'
    checkUnexpectedReturnNULL "layoutGetHadjustment" result
    result' <- (newObject Gtk.Adjustment.Adjustment) result
    touchManagedPtr layout
    return result'

#if defined(ENABLE_OVERLOADING)
data LayoutGetHadjustmentMethodInfo
instance (signature ~ (m Gtk.Adjustment.Adjustment), MonadIO m, IsLayout a) => O.OverloadedMethod LayoutGetHadjustmentMethodInfo a signature where
    overloadedMethod = layoutGetHadjustment

instance O.OverloadedMethodInfo LayoutGetHadjustmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Layout.layoutGetHadjustment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Layout.html#v:layoutGetHadjustment"
        })


#endif

-- method Layout::get_size
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "layout"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Layout" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLayout" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "width"
--           , argType = TBasicType TUInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location to store the width set on\n    @layout, or %NULL"
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
--           , argType = TBasicType TUInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location to store the height set on\n    @layout, or %NULL"
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

foreign import ccall "gtk_layout_get_size" gtk_layout_get_size :: 
    Ptr Layout ->                           -- layout : TInterface (Name {namespace = "Gtk", name = "Layout"})
    Ptr Word32 ->                           -- width : TBasicType TUInt
    Ptr Word32 ->                           -- height : TBasicType TUInt
    IO ()

-- | Gets the size that has been set on the layout, and that determines
-- the total extents of the layout’s scrollbar area. See
-- gtk_layout_set_size ().
layoutGetSize ::
    (B.CallStack.HasCallStack, MonadIO m, IsLayout a) =>
    a
    -- ^ /@layout@/: a t'GI.Gtk.Objects.Layout.Layout'
    -> m ((Word32, Word32))
layoutGetSize layout = liftIO $ do
    layout' <- unsafeManagedPtrCastPtr layout
    width <- allocMem :: IO (Ptr Word32)
    height <- allocMem :: IO (Ptr Word32)
    gtk_layout_get_size layout' width height
    width' <- peek width
    height' <- peek height
    touchManagedPtr layout
    freeMem width
    freeMem height
    return (width', height')

#if defined(ENABLE_OVERLOADING)
data LayoutGetSizeMethodInfo
instance (signature ~ (m ((Word32, Word32))), MonadIO m, IsLayout a) => O.OverloadedMethod LayoutGetSizeMethodInfo a signature where
    overloadedMethod = layoutGetSize

instance O.OverloadedMethodInfo LayoutGetSizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Layout.layoutGetSize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Layout.html#v:layoutGetSize"
        })


#endif

-- method Layout::get_vadjustment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "layout"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Layout" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLayout" , sinceVersion = Nothing }
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

foreign import ccall "gtk_layout_get_vadjustment" gtk_layout_get_vadjustment :: 
    Ptr Layout ->                           -- layout : TInterface (Name {namespace = "Gtk", name = "Layout"})
    IO (Ptr Gtk.Adjustment.Adjustment)

{-# DEPRECATED layoutGetVadjustment ["(Since version 3.0)","Use 'GI.Gtk.Interfaces.Scrollable.scrollableGetVadjustment'"] #-}
-- | This function should only be called after the layout has been
-- placed in a t'GI.Gtk.Objects.ScrolledWindow.ScrolledWindow' or otherwise configured for
-- scrolling. It returns the t'GI.Gtk.Objects.Adjustment.Adjustment' used for communication
-- between the vertical scrollbar and /@layout@/.
-- 
-- See t'GI.Gtk.Objects.ScrolledWindow.ScrolledWindow', t'GI.Gtk.Objects.Scrollbar.Scrollbar', t'GI.Gtk.Objects.Adjustment.Adjustment' for details.
layoutGetVadjustment ::
    (B.CallStack.HasCallStack, MonadIO m, IsLayout a) =>
    a
    -- ^ /@layout@/: a t'GI.Gtk.Objects.Layout.Layout'
    -> m Gtk.Adjustment.Adjustment
    -- ^ __Returns:__ vertical scroll adjustment
layoutGetVadjustment layout = liftIO $ do
    layout' <- unsafeManagedPtrCastPtr layout
    result <- gtk_layout_get_vadjustment layout'
    checkUnexpectedReturnNULL "layoutGetVadjustment" result
    result' <- (newObject Gtk.Adjustment.Adjustment) result
    touchManagedPtr layout
    return result'

#if defined(ENABLE_OVERLOADING)
data LayoutGetVadjustmentMethodInfo
instance (signature ~ (m Gtk.Adjustment.Adjustment), MonadIO m, IsLayout a) => O.OverloadedMethod LayoutGetVadjustmentMethodInfo a signature where
    overloadedMethod = layoutGetVadjustment

instance O.OverloadedMethodInfo LayoutGetVadjustmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Layout.layoutGetVadjustment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Layout.html#v:layoutGetVadjustment"
        })


#endif

-- method Layout::move
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "layout"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Layout" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLayout" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "child_widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a current child of @layout"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "X position to move to"
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
--                 { rawDocText = Just "Y position to move to"
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

foreign import ccall "gtk_layout_move" gtk_layout_move :: 
    Ptr Layout ->                           -- layout : TInterface (Name {namespace = "Gtk", name = "Layout"})
    Ptr Gtk.Widget.Widget ->                -- child_widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Int32 ->                                -- x : TBasicType TInt
    Int32 ->                                -- y : TBasicType TInt
    IO ()

-- | Moves a current child of /@layout@/ to a new position.
layoutMove ::
    (B.CallStack.HasCallStack, MonadIO m, IsLayout a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@layout@/: a t'GI.Gtk.Objects.Layout.Layout'
    -> b
    -- ^ /@childWidget@/: a current child of /@layout@/
    -> Int32
    -- ^ /@x@/: X position to move to
    -> Int32
    -- ^ /@y@/: Y position to move to
    -> m ()
layoutMove layout childWidget x y = liftIO $ do
    layout' <- unsafeManagedPtrCastPtr layout
    childWidget' <- unsafeManagedPtrCastPtr childWidget
    gtk_layout_move layout' childWidget' x y
    touchManagedPtr layout
    touchManagedPtr childWidget
    return ()

#if defined(ENABLE_OVERLOADING)
data LayoutMoveMethodInfo
instance (signature ~ (b -> Int32 -> Int32 -> m ()), MonadIO m, IsLayout a, Gtk.Widget.IsWidget b) => O.OverloadedMethod LayoutMoveMethodInfo a signature where
    overloadedMethod = layoutMove

instance O.OverloadedMethodInfo LayoutMoveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Layout.layoutMove",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Layout.html#v:layoutMove"
        })


#endif

-- method Layout::put
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "layout"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Layout" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLayout" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "child_widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "child widget" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "X position of child widget"
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
--                 { rawDocText = Just "Y position of child widget"
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

foreign import ccall "gtk_layout_put" gtk_layout_put :: 
    Ptr Layout ->                           -- layout : TInterface (Name {namespace = "Gtk", name = "Layout"})
    Ptr Gtk.Widget.Widget ->                -- child_widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Int32 ->                                -- x : TBasicType TInt
    Int32 ->                                -- y : TBasicType TInt
    IO ()

-- | Adds /@childWidget@/ to /@layout@/, at position (/@x@/,/@y@/).
-- /@layout@/ becomes the new parent container of /@childWidget@/.
layoutPut ::
    (B.CallStack.HasCallStack, MonadIO m, IsLayout a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@layout@/: a t'GI.Gtk.Objects.Layout.Layout'
    -> b
    -- ^ /@childWidget@/: child widget
    -> Int32
    -- ^ /@x@/: X position of child widget
    -> Int32
    -- ^ /@y@/: Y position of child widget
    -> m ()
layoutPut layout childWidget x y = liftIO $ do
    layout' <- unsafeManagedPtrCastPtr layout
    childWidget' <- unsafeManagedPtrCastPtr childWidget
    gtk_layout_put layout' childWidget' x y
    touchManagedPtr layout
    touchManagedPtr childWidget
    return ()

#if defined(ENABLE_OVERLOADING)
data LayoutPutMethodInfo
instance (signature ~ (b -> Int32 -> Int32 -> m ()), MonadIO m, IsLayout a, Gtk.Widget.IsWidget b) => O.OverloadedMethod LayoutPutMethodInfo a signature where
    overloadedMethod = layoutPut

instance O.OverloadedMethodInfo LayoutPutMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Layout.layoutPut",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Layout.html#v:layoutPut"
        })


#endif

-- method Layout::set_hadjustment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "layout"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Layout" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLayout" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "new scroll adjustment"
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

foreign import ccall "gtk_layout_set_hadjustment" gtk_layout_set_hadjustment :: 
    Ptr Layout ->                           -- layout : TInterface (Name {namespace = "Gtk", name = "Layout"})
    Ptr Gtk.Adjustment.Adjustment ->        -- adjustment : TInterface (Name {namespace = "Gtk", name = "Adjustment"})
    IO ()

{-# DEPRECATED layoutSetHadjustment ["(Since version 3.0)","Use 'GI.Gtk.Interfaces.Scrollable.scrollableSetHadjustment'"] #-}
-- | Sets the horizontal scroll adjustment for the layout.
-- 
-- See t'GI.Gtk.Objects.ScrolledWindow.ScrolledWindow', t'GI.Gtk.Objects.Scrollbar.Scrollbar', t'GI.Gtk.Objects.Adjustment.Adjustment' for details.
layoutSetHadjustment ::
    (B.CallStack.HasCallStack, MonadIO m, IsLayout a, Gtk.Adjustment.IsAdjustment b) =>
    a
    -- ^ /@layout@/: a t'GI.Gtk.Objects.Layout.Layout'
    -> Maybe (b)
    -- ^ /@adjustment@/: new scroll adjustment
    -> m ()
layoutSetHadjustment layout adjustment = liftIO $ do
    layout' <- unsafeManagedPtrCastPtr layout
    maybeAdjustment <- case adjustment of
        Nothing -> return nullPtr
        Just jAdjustment -> do
            jAdjustment' <- unsafeManagedPtrCastPtr jAdjustment
            return jAdjustment'
    gtk_layout_set_hadjustment layout' maybeAdjustment
    touchManagedPtr layout
    whenJust adjustment touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data LayoutSetHadjustmentMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsLayout a, Gtk.Adjustment.IsAdjustment b) => O.OverloadedMethod LayoutSetHadjustmentMethodInfo a signature where
    overloadedMethod = layoutSetHadjustment

instance O.OverloadedMethodInfo LayoutSetHadjustmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Layout.layoutSetHadjustment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Layout.html#v:layoutSetHadjustment"
        })


#endif

-- method Layout::set_size
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "layout"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Layout" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLayout" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "width"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "width of entire scrollable area"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "height"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "height of entire scrollable area"
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

foreign import ccall "gtk_layout_set_size" gtk_layout_set_size :: 
    Ptr Layout ->                           -- layout : TInterface (Name {namespace = "Gtk", name = "Layout"})
    Word32 ->                               -- width : TBasicType TUInt
    Word32 ->                               -- height : TBasicType TUInt
    IO ()

-- | Sets the size of the scrollable area of the layout.
layoutSetSize ::
    (B.CallStack.HasCallStack, MonadIO m, IsLayout a) =>
    a
    -- ^ /@layout@/: a t'GI.Gtk.Objects.Layout.Layout'
    -> Word32
    -- ^ /@width@/: width of entire scrollable area
    -> Word32
    -- ^ /@height@/: height of entire scrollable area
    -> m ()
layoutSetSize layout width height = liftIO $ do
    layout' <- unsafeManagedPtrCastPtr layout
    gtk_layout_set_size layout' width height
    touchManagedPtr layout
    return ()

#if defined(ENABLE_OVERLOADING)
data LayoutSetSizeMethodInfo
instance (signature ~ (Word32 -> Word32 -> m ()), MonadIO m, IsLayout a) => O.OverloadedMethod LayoutSetSizeMethodInfo a signature where
    overloadedMethod = layoutSetSize

instance O.OverloadedMethodInfo LayoutSetSizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Layout.layoutSetSize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Layout.html#v:layoutSetSize"
        })


#endif

-- method Layout::set_vadjustment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "layout"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Layout" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLayout" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "new scroll adjustment"
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

foreign import ccall "gtk_layout_set_vadjustment" gtk_layout_set_vadjustment :: 
    Ptr Layout ->                           -- layout : TInterface (Name {namespace = "Gtk", name = "Layout"})
    Ptr Gtk.Adjustment.Adjustment ->        -- adjustment : TInterface (Name {namespace = "Gtk", name = "Adjustment"})
    IO ()

{-# DEPRECATED layoutSetVadjustment ["(Since version 3.0)","Use 'GI.Gtk.Interfaces.Scrollable.scrollableSetVadjustment'"] #-}
-- | Sets the vertical scroll adjustment for the layout.
-- 
-- See t'GI.Gtk.Objects.ScrolledWindow.ScrolledWindow', t'GI.Gtk.Objects.Scrollbar.Scrollbar', t'GI.Gtk.Objects.Adjustment.Adjustment' for details.
layoutSetVadjustment ::
    (B.CallStack.HasCallStack, MonadIO m, IsLayout a, Gtk.Adjustment.IsAdjustment b) =>
    a
    -- ^ /@layout@/: a t'GI.Gtk.Objects.Layout.Layout'
    -> Maybe (b)
    -- ^ /@adjustment@/: new scroll adjustment
    -> m ()
layoutSetVadjustment layout adjustment = liftIO $ do
    layout' <- unsafeManagedPtrCastPtr layout
    maybeAdjustment <- case adjustment of
        Nothing -> return nullPtr
        Just jAdjustment -> do
            jAdjustment' <- unsafeManagedPtrCastPtr jAdjustment
            return jAdjustment'
    gtk_layout_set_vadjustment layout' maybeAdjustment
    touchManagedPtr layout
    whenJust adjustment touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data LayoutSetVadjustmentMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsLayout a, Gtk.Adjustment.IsAdjustment b) => O.OverloadedMethod LayoutSetVadjustmentMethodInfo a signature where
    overloadedMethod = layoutSetVadjustment

instance O.OverloadedMethodInfo LayoutSetVadjustmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Layout.layoutSetVadjustment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Layout.html#v:layoutSetVadjustment"
        })


#endif


