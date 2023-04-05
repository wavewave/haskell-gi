{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- The GtkRevealer widget is a container which animates
-- the transition of its child from invisible to visible.
-- 
-- The style of transition can be controlled with
-- 'GI.Gtk.Objects.Revealer.revealerSetTransitionType'.
-- 
-- These animations respect the [Settings:gtkEnableAnimations]("GI.Gtk.Objects.Settings#g:attr:gtkEnableAnimations")
-- setting.
-- 
-- = CSS nodes
-- 
-- GtkRevealer has a single CSS node with name revealer.
-- 
-- The GtkRevealer widget was added in GTK+ 3.10.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.Revealer
    ( 

-- * Exported types
    Revealer(..)                            ,
    IsRevealer                              ,
    toRevealer                              ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChild]("GI.Gtk.Objects.Bin#g:method:getChild"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildRevealed]("GI.Gtk.Objects.Revealer#g:method:getChildRevealed"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRevealChild]("GI.Gtk.Objects.Revealer#g:method:getRevealChild"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getTransitionDuration]("GI.Gtk.Objects.Revealer#g:method:getTransitionDuration"), [getTransitionType]("GI.Gtk.Objects.Revealer#g:method:getTransitionType"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setRevealChild]("GI.Gtk.Objects.Revealer#g:method:setRevealChild"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setTransitionDuration]("GI.Gtk.Objects.Revealer#g:method:setTransitionDuration"), [setTransitionType]("GI.Gtk.Objects.Revealer#g:method:setTransitionType"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveRevealerMethod                   ,
#endif

-- ** getChildRevealed #method:getChildRevealed#

#if defined(ENABLE_OVERLOADING)
    RevealerGetChildRevealedMethodInfo      ,
#endif
    revealerGetChildRevealed                ,


-- ** getRevealChild #method:getRevealChild#

#if defined(ENABLE_OVERLOADING)
    RevealerGetRevealChildMethodInfo        ,
#endif
    revealerGetRevealChild                  ,


-- ** getTransitionDuration #method:getTransitionDuration#

#if defined(ENABLE_OVERLOADING)
    RevealerGetTransitionDurationMethodInfo ,
#endif
    revealerGetTransitionDuration           ,


-- ** getTransitionType #method:getTransitionType#

#if defined(ENABLE_OVERLOADING)
    RevealerGetTransitionTypeMethodInfo     ,
#endif
    revealerGetTransitionType               ,


-- ** new #method:new#

    revealerNew                             ,


-- ** setRevealChild #method:setRevealChild#

#if defined(ENABLE_OVERLOADING)
    RevealerSetRevealChildMethodInfo        ,
#endif
    revealerSetRevealChild                  ,


-- ** setTransitionDuration #method:setTransitionDuration#

#if defined(ENABLE_OVERLOADING)
    RevealerSetTransitionDurationMethodInfo ,
#endif
    revealerSetTransitionDuration           ,


-- ** setTransitionType #method:setTransitionType#

#if defined(ENABLE_OVERLOADING)
    RevealerSetTransitionTypeMethodInfo     ,
#endif
    revealerSetTransitionType               ,




 -- * Properties


-- ** childRevealed #attr:childRevealed#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    RevealerChildRevealedPropertyInfo       ,
#endif
    getRevealerChildRevealed                ,
#if defined(ENABLE_OVERLOADING)
    revealerChildRevealed                   ,
#endif


-- ** revealChild #attr:revealChild#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    RevealerRevealChildPropertyInfo         ,
#endif
    constructRevealerRevealChild            ,
    getRevealerRevealChild                  ,
#if defined(ENABLE_OVERLOADING)
    revealerRevealChild                     ,
#endif
    setRevealerRevealChild                  ,


-- ** transitionDuration #attr:transitionDuration#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    RevealerTransitionDurationPropertyInfo  ,
#endif
    constructRevealerTransitionDuration     ,
    getRevealerTransitionDuration           ,
#if defined(ENABLE_OVERLOADING)
    revealerTransitionDuration              ,
#endif
    setRevealerTransitionDuration           ,


-- ** transitionType #attr:transitionType#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    RevealerTransitionTypePropertyInfo      ,
#endif
    constructRevealerTransitionType         ,
    getRevealerTransitionType               ,
#if defined(ENABLE_OVERLOADING)
    revealerTransitionType                  ,
#endif
    setRevealerTransitionType               ,




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
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Objects.Bin as Gtk.Bin
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype Revealer = Revealer (SP.ManagedPtr Revealer)
    deriving (Eq)

instance SP.ManagedPtrNewtype Revealer where
    toManagedPtr (Revealer p) = p

foreign import ccall "gtk_revealer_get_type"
    c_gtk_revealer_get_type :: IO B.Types.GType

instance B.Types.TypedObject Revealer where
    glibType = c_gtk_revealer_get_type

instance B.Types.GObject Revealer

-- | Type class for types which can be safely cast to `Revealer`, for instance with `toRevealer`.
class (SP.GObject o, O.IsDescendantOf Revealer o) => IsRevealer o
instance (SP.GObject o, O.IsDescendantOf Revealer o) => IsRevealer o

instance O.HasParentTypes Revealer
type instance O.ParentTypes Revealer = '[Gtk.Bin.Bin, Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable]

-- | Cast to `Revealer`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toRevealer :: (MIO.MonadIO m, IsRevealer o) => o -> m Revealer
toRevealer = MIO.liftIO . B.ManagedPtr.unsafeCastTo Revealer

-- | Convert 'Revealer' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe Revealer) where
    gvalueGType_ = c_gtk_revealer_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr Revealer)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr Revealer)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject Revealer ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveRevealerMethod (t :: Symbol) (o :: *) :: * where
    ResolveRevealerMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveRevealerMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveRevealerMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveRevealerMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveRevealerMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveRevealerMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveRevealerMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveRevealerMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveRevealerMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveRevealerMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveRevealerMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveRevealerMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveRevealerMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveRevealerMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveRevealerMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveRevealerMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveRevealerMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveRevealerMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveRevealerMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveRevealerMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveRevealerMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveRevealerMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveRevealerMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveRevealerMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveRevealerMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveRevealerMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveRevealerMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveRevealerMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveRevealerMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveRevealerMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveRevealerMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveRevealerMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveRevealerMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveRevealerMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveRevealerMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveRevealerMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveRevealerMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveRevealerMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveRevealerMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveRevealerMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveRevealerMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveRevealerMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveRevealerMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveRevealerMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveRevealerMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveRevealerMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveRevealerMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveRevealerMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveRevealerMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveRevealerMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveRevealerMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveRevealerMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveRevealerMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveRevealerMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveRevealerMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveRevealerMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveRevealerMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveRevealerMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveRevealerMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveRevealerMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveRevealerMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveRevealerMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveRevealerMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveRevealerMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveRevealerMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveRevealerMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveRevealerMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveRevealerMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveRevealerMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveRevealerMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveRevealerMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveRevealerMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveRevealerMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveRevealerMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveRevealerMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveRevealerMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveRevealerMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveRevealerMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveRevealerMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveRevealerMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveRevealerMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveRevealerMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveRevealerMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveRevealerMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveRevealerMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveRevealerMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveRevealerMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveRevealerMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveRevealerMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveRevealerMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveRevealerMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveRevealerMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveRevealerMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveRevealerMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveRevealerMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveRevealerMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveRevealerMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveRevealerMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveRevealerMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveRevealerMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveRevealerMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveRevealerMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveRevealerMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveRevealerMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveRevealerMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveRevealerMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveRevealerMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveRevealerMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveRevealerMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveRevealerMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveRevealerMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveRevealerMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveRevealerMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveRevealerMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveRevealerMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveRevealerMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveRevealerMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveRevealerMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveRevealerMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveRevealerMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveRevealerMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveRevealerMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveRevealerMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveRevealerMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveRevealerMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveRevealerMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveRevealerMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveRevealerMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolveRevealerMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveRevealerMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveRevealerMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveRevealerMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveRevealerMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveRevealerMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveRevealerMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveRevealerMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveRevealerMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveRevealerMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveRevealerMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveRevealerMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveRevealerMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveRevealerMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveRevealerMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveRevealerMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveRevealerMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveRevealerMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveRevealerMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveRevealerMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveRevealerMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveRevealerMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveRevealerMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveRevealerMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveRevealerMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveRevealerMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveRevealerMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveRevealerMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveRevealerMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveRevealerMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveRevealerMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveRevealerMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveRevealerMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveRevealerMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveRevealerMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveRevealerMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveRevealerMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveRevealerMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveRevealerMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveRevealerMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveRevealerMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveRevealerMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveRevealerMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveRevealerMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveRevealerMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveRevealerMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveRevealerMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveRevealerMethod "getChild" o = Gtk.Bin.BinGetChildMethodInfo
    ResolveRevealerMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveRevealerMethod "getChildRevealed" o = RevealerGetChildRevealedMethodInfo
    ResolveRevealerMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveRevealerMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveRevealerMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveRevealerMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveRevealerMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveRevealerMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveRevealerMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveRevealerMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveRevealerMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveRevealerMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveRevealerMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveRevealerMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveRevealerMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveRevealerMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveRevealerMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveRevealerMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveRevealerMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveRevealerMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveRevealerMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveRevealerMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveRevealerMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveRevealerMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveRevealerMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveRevealerMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveRevealerMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveRevealerMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveRevealerMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveRevealerMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveRevealerMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveRevealerMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveRevealerMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveRevealerMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveRevealerMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveRevealerMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveRevealerMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveRevealerMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveRevealerMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveRevealerMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveRevealerMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveRevealerMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveRevealerMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveRevealerMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveRevealerMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveRevealerMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveRevealerMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveRevealerMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveRevealerMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveRevealerMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveRevealerMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveRevealerMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveRevealerMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveRevealerMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveRevealerMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveRevealerMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveRevealerMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveRevealerMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveRevealerMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveRevealerMethod "getRevealChild" o = RevealerGetRevealChildMethodInfo
    ResolveRevealerMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveRevealerMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveRevealerMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveRevealerMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveRevealerMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveRevealerMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveRevealerMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveRevealerMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveRevealerMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveRevealerMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveRevealerMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveRevealerMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveRevealerMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveRevealerMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveRevealerMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveRevealerMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveRevealerMethod "getTransitionDuration" o = RevealerGetTransitionDurationMethodInfo
    ResolveRevealerMethod "getTransitionType" o = RevealerGetTransitionTypeMethodInfo
    ResolveRevealerMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveRevealerMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveRevealerMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveRevealerMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveRevealerMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveRevealerMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveRevealerMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveRevealerMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveRevealerMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveRevealerMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveRevealerMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveRevealerMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveRevealerMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveRevealerMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveRevealerMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveRevealerMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveRevealerMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveRevealerMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveRevealerMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveRevealerMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveRevealerMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveRevealerMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveRevealerMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveRevealerMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveRevealerMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveRevealerMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveRevealerMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveRevealerMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveRevealerMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveRevealerMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveRevealerMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveRevealerMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveRevealerMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveRevealerMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveRevealerMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveRevealerMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveRevealerMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveRevealerMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveRevealerMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveRevealerMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveRevealerMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveRevealerMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveRevealerMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveRevealerMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveRevealerMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveRevealerMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveRevealerMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveRevealerMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveRevealerMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveRevealerMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveRevealerMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveRevealerMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveRevealerMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveRevealerMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveRevealerMethod "setRevealChild" o = RevealerSetRevealChildMethodInfo
    ResolveRevealerMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveRevealerMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveRevealerMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveRevealerMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveRevealerMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveRevealerMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveRevealerMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveRevealerMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveRevealerMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveRevealerMethod "setTransitionDuration" o = RevealerSetTransitionDurationMethodInfo
    ResolveRevealerMethod "setTransitionType" o = RevealerSetTransitionTypeMethodInfo
    ResolveRevealerMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveRevealerMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveRevealerMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveRevealerMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveRevealerMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveRevealerMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveRevealerMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveRevealerMethod t Revealer, O.OverloadedMethod info Revealer p) => OL.IsLabel t (Revealer -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveRevealerMethod t Revealer, O.OverloadedMethod info Revealer p, R.HasField t Revealer p) => R.HasField t Revealer p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveRevealerMethod t Revealer, O.OverloadedMethodInfo info Revealer) => OL.IsLabel t (O.MethodProxy info Revealer) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- VVV Prop "child-revealed"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable]
   -- Nullable: (Just False,Nothing)

-- | Get the value of the “@child-revealed@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' revealer #childRevealed
-- @
getRevealerChildRevealed :: (MonadIO m, IsRevealer o) => o -> m Bool
getRevealerChildRevealed obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "child-revealed"

#if defined(ENABLE_OVERLOADING)
data RevealerChildRevealedPropertyInfo
instance AttrInfo RevealerChildRevealedPropertyInfo where
    type AttrAllowedOps RevealerChildRevealedPropertyInfo = '[ 'AttrGet]
    type AttrBaseTypeConstraint RevealerChildRevealedPropertyInfo = IsRevealer
    type AttrSetTypeConstraint RevealerChildRevealedPropertyInfo = (~) ()
    type AttrTransferTypeConstraint RevealerChildRevealedPropertyInfo = (~) ()
    type AttrTransferType RevealerChildRevealedPropertyInfo = ()
    type AttrGetType RevealerChildRevealedPropertyInfo = Bool
    type AttrLabel RevealerChildRevealedPropertyInfo = "child-revealed"
    type AttrOrigin RevealerChildRevealedPropertyInfo = Revealer
    attrGet = getRevealerChildRevealed
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Revealer.childRevealed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Revealer.html#g:attr:childRevealed"
        })
#endif

-- VVV Prop "reveal-child"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstruct]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@reveal-child@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' revealer #revealChild
-- @
getRevealerRevealChild :: (MonadIO m, IsRevealer o) => o -> m Bool
getRevealerRevealChild obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "reveal-child"

-- | Set the value of the “@reveal-child@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' revealer [ #revealChild 'Data.GI.Base.Attributes.:=' value ]
-- @
setRevealerRevealChild :: (MonadIO m, IsRevealer o) => o -> Bool -> m ()
setRevealerRevealChild obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "reveal-child" val

-- | Construct a `GValueConstruct` with valid value for the “@reveal-child@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructRevealerRevealChild :: (IsRevealer o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructRevealerRevealChild val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "reveal-child" val

#if defined(ENABLE_OVERLOADING)
data RevealerRevealChildPropertyInfo
instance AttrInfo RevealerRevealChildPropertyInfo where
    type AttrAllowedOps RevealerRevealChildPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint RevealerRevealChildPropertyInfo = IsRevealer
    type AttrSetTypeConstraint RevealerRevealChildPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint RevealerRevealChildPropertyInfo = (~) Bool
    type AttrTransferType RevealerRevealChildPropertyInfo = Bool
    type AttrGetType RevealerRevealChildPropertyInfo = Bool
    type AttrLabel RevealerRevealChildPropertyInfo = "reveal-child"
    type AttrOrigin RevealerRevealChildPropertyInfo = Revealer
    attrGet = getRevealerRevealChild
    attrSet = setRevealerRevealChild
    attrTransfer _ v = do
        return v
    attrConstruct = constructRevealerRevealChild
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Revealer.revealChild"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Revealer.html#g:attr:revealChild"
        })
#endif

-- VVV Prop "transition-duration"
   -- Type: TBasicType TUInt
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstruct]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@transition-duration@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' revealer #transitionDuration
-- @
getRevealerTransitionDuration :: (MonadIO m, IsRevealer o) => o -> m Word32
getRevealerTransitionDuration obj = MIO.liftIO $ B.Properties.getObjectPropertyUInt32 obj "transition-duration"

-- | Set the value of the “@transition-duration@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' revealer [ #transitionDuration 'Data.GI.Base.Attributes.:=' value ]
-- @
setRevealerTransitionDuration :: (MonadIO m, IsRevealer o) => o -> Word32 -> m ()
setRevealerTransitionDuration obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyUInt32 obj "transition-duration" val

-- | Construct a `GValueConstruct` with valid value for the “@transition-duration@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructRevealerTransitionDuration :: (IsRevealer o, MIO.MonadIO m) => Word32 -> m (GValueConstruct o)
constructRevealerTransitionDuration val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyUInt32 "transition-duration" val

#if defined(ENABLE_OVERLOADING)
data RevealerTransitionDurationPropertyInfo
instance AttrInfo RevealerTransitionDurationPropertyInfo where
    type AttrAllowedOps RevealerTransitionDurationPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint RevealerTransitionDurationPropertyInfo = IsRevealer
    type AttrSetTypeConstraint RevealerTransitionDurationPropertyInfo = (~) Word32
    type AttrTransferTypeConstraint RevealerTransitionDurationPropertyInfo = (~) Word32
    type AttrTransferType RevealerTransitionDurationPropertyInfo = Word32
    type AttrGetType RevealerTransitionDurationPropertyInfo = Word32
    type AttrLabel RevealerTransitionDurationPropertyInfo = "transition-duration"
    type AttrOrigin RevealerTransitionDurationPropertyInfo = Revealer
    attrGet = getRevealerTransitionDuration
    attrSet = setRevealerTransitionDuration
    attrTransfer _ v = do
        return v
    attrConstruct = constructRevealerTransitionDuration
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Revealer.transitionDuration"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Revealer.html#g:attr:transitionDuration"
        })
#endif

-- VVV Prop "transition-type"
   -- Type: TInterface (Name {namespace = "Gtk", name = "RevealerTransitionType"})
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstruct]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@transition-type@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' revealer #transitionType
-- @
getRevealerTransitionType :: (MonadIO m, IsRevealer o) => o -> m Gtk.Enums.RevealerTransitionType
getRevealerTransitionType obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "transition-type"

-- | Set the value of the “@transition-type@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' revealer [ #transitionType 'Data.GI.Base.Attributes.:=' value ]
-- @
setRevealerTransitionType :: (MonadIO m, IsRevealer o) => o -> Gtk.Enums.RevealerTransitionType -> m ()
setRevealerTransitionType obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "transition-type" val

-- | Construct a `GValueConstruct` with valid value for the “@transition-type@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructRevealerTransitionType :: (IsRevealer o, MIO.MonadIO m) => Gtk.Enums.RevealerTransitionType -> m (GValueConstruct o)
constructRevealerTransitionType val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "transition-type" val

#if defined(ENABLE_OVERLOADING)
data RevealerTransitionTypePropertyInfo
instance AttrInfo RevealerTransitionTypePropertyInfo where
    type AttrAllowedOps RevealerTransitionTypePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint RevealerTransitionTypePropertyInfo = IsRevealer
    type AttrSetTypeConstraint RevealerTransitionTypePropertyInfo = (~) Gtk.Enums.RevealerTransitionType
    type AttrTransferTypeConstraint RevealerTransitionTypePropertyInfo = (~) Gtk.Enums.RevealerTransitionType
    type AttrTransferType RevealerTransitionTypePropertyInfo = Gtk.Enums.RevealerTransitionType
    type AttrGetType RevealerTransitionTypePropertyInfo = Gtk.Enums.RevealerTransitionType
    type AttrLabel RevealerTransitionTypePropertyInfo = "transition-type"
    type AttrOrigin RevealerTransitionTypePropertyInfo = Revealer
    attrGet = getRevealerTransitionType
    attrSet = setRevealerTransitionType
    attrTransfer _ v = do
        return v
    attrConstruct = constructRevealerTransitionType
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Revealer.transitionType"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Revealer.html#g:attr:transitionType"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList Revealer
type instance O.AttributeList Revealer = RevealerAttributeList
type RevealerAttributeList = ('[ '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("childRevealed", RevealerChildRevealedPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("revealChild", RevealerRevealChildPropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("transitionDuration", RevealerTransitionDurationPropertyInfo), '("transitionType", RevealerTransitionTypePropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
revealerChildRevealed :: AttrLabelProxy "childRevealed"
revealerChildRevealed = AttrLabelProxy

revealerRevealChild :: AttrLabelProxy "revealChild"
revealerRevealChild = AttrLabelProxy

revealerTransitionDuration :: AttrLabelProxy "transitionDuration"
revealerTransitionDuration = AttrLabelProxy

revealerTransitionType :: AttrLabelProxy "transitionType"
revealerTransitionType = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList Revealer = RevealerSignalList
type RevealerSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method Revealer::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Revealer" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_revealer_new" gtk_revealer_new :: 
    IO (Ptr Revealer)

-- | Creates a new t'GI.Gtk.Objects.Revealer.Revealer'.
-- 
-- /Since: 3.10/
revealerNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m Revealer
    -- ^ __Returns:__ a newly created t'GI.Gtk.Objects.Revealer.Revealer'
revealerNew  = liftIO $ do
    result <- gtk_revealer_new
    checkUnexpectedReturnNULL "revealerNew" result
    result' <- (newObject Revealer) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Revealer::get_child_revealed
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "revealer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Revealer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRevealer" , sinceVersion = Nothing }
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

foreign import ccall "gtk_revealer_get_child_revealed" gtk_revealer_get_child_revealed :: 
    Ptr Revealer ->                         -- revealer : TInterface (Name {namespace = "Gtk", name = "Revealer"})
    IO CInt

-- | Returns whether the child is fully revealed, in other words whether
-- the transition to the revealed state is completed.
-- 
-- /Since: 3.10/
revealerGetChildRevealed ::
    (B.CallStack.HasCallStack, MonadIO m, IsRevealer a) =>
    a
    -- ^ /@revealer@/: a t'GI.Gtk.Objects.Revealer.Revealer'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the child is fully revealed
revealerGetChildRevealed revealer = liftIO $ do
    revealer' <- unsafeManagedPtrCastPtr revealer
    result <- gtk_revealer_get_child_revealed revealer'
    let result' = (/= 0) result
    touchManagedPtr revealer
    return result'

#if defined(ENABLE_OVERLOADING)
data RevealerGetChildRevealedMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsRevealer a) => O.OverloadedMethod RevealerGetChildRevealedMethodInfo a signature where
    overloadedMethod = revealerGetChildRevealed

instance O.OverloadedMethodInfo RevealerGetChildRevealedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Revealer.revealerGetChildRevealed",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Revealer.html#v:revealerGetChildRevealed"
        })


#endif

-- method Revealer::get_reveal_child
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "revealer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Revealer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRevealer" , sinceVersion = Nothing }
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

foreign import ccall "gtk_revealer_get_reveal_child" gtk_revealer_get_reveal_child :: 
    Ptr Revealer ->                         -- revealer : TInterface (Name {namespace = "Gtk", name = "Revealer"})
    IO CInt

-- | Returns whether the child is currently
-- revealed. See 'GI.Gtk.Objects.Revealer.revealerSetRevealChild'.
-- 
-- This function returns 'P.True' as soon as the transition
-- is to the revealed state is started. To learn whether
-- the child is fully revealed (ie the transition is completed),
-- use 'GI.Gtk.Objects.Revealer.revealerGetChildRevealed'.
-- 
-- /Since: 3.10/
revealerGetRevealChild ::
    (B.CallStack.HasCallStack, MonadIO m, IsRevealer a) =>
    a
    -- ^ /@revealer@/: a t'GI.Gtk.Objects.Revealer.Revealer'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the child is revealed.
revealerGetRevealChild revealer = liftIO $ do
    revealer' <- unsafeManagedPtrCastPtr revealer
    result <- gtk_revealer_get_reveal_child revealer'
    let result' = (/= 0) result
    touchManagedPtr revealer
    return result'

#if defined(ENABLE_OVERLOADING)
data RevealerGetRevealChildMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsRevealer a) => O.OverloadedMethod RevealerGetRevealChildMethodInfo a signature where
    overloadedMethod = revealerGetRevealChild

instance O.OverloadedMethodInfo RevealerGetRevealChildMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Revealer.revealerGetRevealChild",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Revealer.html#v:revealerGetRevealChild"
        })


#endif

-- method Revealer::get_transition_duration
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "revealer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Revealer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRevealer" , sinceVersion = Nothing }
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

foreign import ccall "gtk_revealer_get_transition_duration" gtk_revealer_get_transition_duration :: 
    Ptr Revealer ->                         -- revealer : TInterface (Name {namespace = "Gtk", name = "Revealer"})
    IO Word32

-- | Returns the amount of time (in milliseconds) that
-- transitions will take.
-- 
-- /Since: 3.10/
revealerGetTransitionDuration ::
    (B.CallStack.HasCallStack, MonadIO m, IsRevealer a) =>
    a
    -- ^ /@revealer@/: a t'GI.Gtk.Objects.Revealer.Revealer'
    -> m Word32
    -- ^ __Returns:__ the transition duration
revealerGetTransitionDuration revealer = liftIO $ do
    revealer' <- unsafeManagedPtrCastPtr revealer
    result <- gtk_revealer_get_transition_duration revealer'
    touchManagedPtr revealer
    return result

#if defined(ENABLE_OVERLOADING)
data RevealerGetTransitionDurationMethodInfo
instance (signature ~ (m Word32), MonadIO m, IsRevealer a) => O.OverloadedMethod RevealerGetTransitionDurationMethodInfo a signature where
    overloadedMethod = revealerGetTransitionDuration

instance O.OverloadedMethodInfo RevealerGetTransitionDurationMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Revealer.revealerGetTransitionDuration",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Revealer.html#v:revealerGetTransitionDuration"
        })


#endif

-- method Revealer::get_transition_type
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "revealer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Revealer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRevealer" , sinceVersion = Nothing }
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
--                  Name { namespace = "Gtk" , name = "RevealerTransitionType" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_revealer_get_transition_type" gtk_revealer_get_transition_type :: 
    Ptr Revealer ->                         -- revealer : TInterface (Name {namespace = "Gtk", name = "Revealer"})
    IO CUInt

-- | Gets the type of animation that will be used
-- for transitions in /@revealer@/.
-- 
-- /Since: 3.10/
revealerGetTransitionType ::
    (B.CallStack.HasCallStack, MonadIO m, IsRevealer a) =>
    a
    -- ^ /@revealer@/: a t'GI.Gtk.Objects.Revealer.Revealer'
    -> m Gtk.Enums.RevealerTransitionType
    -- ^ __Returns:__ the current transition type of /@revealer@/
revealerGetTransitionType revealer = liftIO $ do
    revealer' <- unsafeManagedPtrCastPtr revealer
    result <- gtk_revealer_get_transition_type revealer'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr revealer
    return result'

#if defined(ENABLE_OVERLOADING)
data RevealerGetTransitionTypeMethodInfo
instance (signature ~ (m Gtk.Enums.RevealerTransitionType), MonadIO m, IsRevealer a) => O.OverloadedMethod RevealerGetTransitionTypeMethodInfo a signature where
    overloadedMethod = revealerGetTransitionType

instance O.OverloadedMethodInfo RevealerGetTransitionTypeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Revealer.revealerGetTransitionType",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Revealer.html#v:revealerGetTransitionType"
        })


#endif

-- method Revealer::set_reveal_child
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "revealer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Revealer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRevealer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "reveal_child"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE to reveal the child"
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

foreign import ccall "gtk_revealer_set_reveal_child" gtk_revealer_set_reveal_child :: 
    Ptr Revealer ->                         -- revealer : TInterface (Name {namespace = "Gtk", name = "Revealer"})
    CInt ->                                 -- reveal_child : TBasicType TBoolean
    IO ()

-- | Tells the t'GI.Gtk.Objects.Revealer.Revealer' to reveal or conceal its child.
-- 
-- The transition will be animated with the current
-- transition type of /@revealer@/.
-- 
-- /Since: 3.10/
revealerSetRevealChild ::
    (B.CallStack.HasCallStack, MonadIO m, IsRevealer a) =>
    a
    -- ^ /@revealer@/: a t'GI.Gtk.Objects.Revealer.Revealer'
    -> Bool
    -- ^ /@revealChild@/: 'P.True' to reveal the child
    -> m ()
revealerSetRevealChild revealer revealChild = liftIO $ do
    revealer' <- unsafeManagedPtrCastPtr revealer
    let revealChild' = (fromIntegral . fromEnum) revealChild
    gtk_revealer_set_reveal_child revealer' revealChild'
    touchManagedPtr revealer
    return ()

#if defined(ENABLE_OVERLOADING)
data RevealerSetRevealChildMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsRevealer a) => O.OverloadedMethod RevealerSetRevealChildMethodInfo a signature where
    overloadedMethod = revealerSetRevealChild

instance O.OverloadedMethodInfo RevealerSetRevealChildMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Revealer.revealerSetRevealChild",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Revealer.html#v:revealerSetRevealChild"
        })


#endif

-- method Revealer::set_transition_duration
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "revealer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Revealer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRevealer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "duration"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the new duration, in milliseconds"
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

foreign import ccall "gtk_revealer_set_transition_duration" gtk_revealer_set_transition_duration :: 
    Ptr Revealer ->                         -- revealer : TInterface (Name {namespace = "Gtk", name = "Revealer"})
    Word32 ->                               -- duration : TBasicType TUInt
    IO ()

-- | Sets the duration that transitions will take.
-- 
-- /Since: 3.10/
revealerSetTransitionDuration ::
    (B.CallStack.HasCallStack, MonadIO m, IsRevealer a) =>
    a
    -- ^ /@revealer@/: a t'GI.Gtk.Objects.Revealer.Revealer'
    -> Word32
    -- ^ /@duration@/: the new duration, in milliseconds
    -> m ()
revealerSetTransitionDuration revealer duration = liftIO $ do
    revealer' <- unsafeManagedPtrCastPtr revealer
    gtk_revealer_set_transition_duration revealer' duration
    touchManagedPtr revealer
    return ()

#if defined(ENABLE_OVERLOADING)
data RevealerSetTransitionDurationMethodInfo
instance (signature ~ (Word32 -> m ()), MonadIO m, IsRevealer a) => O.OverloadedMethod RevealerSetTransitionDurationMethodInfo a signature where
    overloadedMethod = revealerSetTransitionDuration

instance O.OverloadedMethodInfo RevealerSetTransitionDurationMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Revealer.revealerSetTransitionDuration",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Revealer.html#v:revealerSetTransitionDuration"
        })


#endif

-- method Revealer::set_transition_type
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "revealer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Revealer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRevealer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "transition"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "RevealerTransitionType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the new transition type"
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

foreign import ccall "gtk_revealer_set_transition_type" gtk_revealer_set_transition_type :: 
    Ptr Revealer ->                         -- revealer : TInterface (Name {namespace = "Gtk", name = "Revealer"})
    CUInt ->                                -- transition : TInterface (Name {namespace = "Gtk", name = "RevealerTransitionType"})
    IO ()

-- | Sets the type of animation that will be used for
-- transitions in /@revealer@/. Available types include
-- various kinds of fades and slides.
-- 
-- /Since: 3.10/
revealerSetTransitionType ::
    (B.CallStack.HasCallStack, MonadIO m, IsRevealer a) =>
    a
    -- ^ /@revealer@/: a t'GI.Gtk.Objects.Revealer.Revealer'
    -> Gtk.Enums.RevealerTransitionType
    -- ^ /@transition@/: the new transition type
    -> m ()
revealerSetTransitionType revealer transition = liftIO $ do
    revealer' <- unsafeManagedPtrCastPtr revealer
    let transition' = (fromIntegral . fromEnum) transition
    gtk_revealer_set_transition_type revealer' transition'
    touchManagedPtr revealer
    return ()

#if defined(ENABLE_OVERLOADING)
data RevealerSetTransitionTypeMethodInfo
instance (signature ~ (Gtk.Enums.RevealerTransitionType -> m ()), MonadIO m, IsRevealer a) => O.OverloadedMethod RevealerSetTransitionTypeMethodInfo a signature where
    overloadedMethod = revealerSetTransitionType

instance O.OverloadedMethodInfo RevealerSetTransitionTypeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Revealer.revealerSetTransitionType",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Revealer.html#v:revealerSetTransitionType"
        })


#endif


