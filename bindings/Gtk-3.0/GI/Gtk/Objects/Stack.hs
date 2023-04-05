{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- The GtkStack widget is a container which only shows
-- one of its children at a time. In contrast to GtkNotebook,
-- GtkStack does not provide a means for users to change the
-- visible child. Instead, the t'GI.Gtk.Objects.StackSwitcher.StackSwitcher' widget can be
-- used with GtkStack to provide this functionality.
-- 
-- Transitions between pages can be animated as slides or
-- fades. This can be controlled with 'GI.Gtk.Objects.Stack.stackSetTransitionType'.
-- These animations respect the [Settings:gtkEnableAnimations]("GI.Gtk.Objects.Settings#g:attr:gtkEnableAnimations")
-- setting.
-- 
-- The GtkStack widget was added in GTK+ 3.10.
-- 
-- = CSS nodes
-- 
-- GtkStack has a single CSS node named stack.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.Stack
    ( 

-- * Exported types
    Stack(..)                               ,
    IsStack                                 ,
    toStack                                 ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addNamed]("GI.Gtk.Objects.Stack#g:method:addNamed"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [addTitled]("GI.Gtk.Objects.Stack#g:method:addTitled"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChildByName]("GI.Gtk.Objects.Stack#g:method:getChildByName"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getHhomogeneous]("GI.Gtk.Objects.Stack#g:method:getHhomogeneous"), [getHomogeneous]("GI.Gtk.Objects.Stack#g:method:getHomogeneous"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getInterpolateSize]("GI.Gtk.Objects.Stack#g:method:getInterpolateSize"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getTransitionDuration]("GI.Gtk.Objects.Stack#g:method:getTransitionDuration"), [getTransitionRunning]("GI.Gtk.Objects.Stack#g:method:getTransitionRunning"), [getTransitionType]("GI.Gtk.Objects.Stack#g:method:getTransitionType"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVhomogeneous]("GI.Gtk.Objects.Stack#g:method:getVhomogeneous"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisibleChild]("GI.Gtk.Objects.Stack#g:method:getVisibleChild"), [getVisibleChildName]("GI.Gtk.Objects.Stack#g:method:getVisibleChildName"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setHhomogeneous]("GI.Gtk.Objects.Stack#g:method:setHhomogeneous"), [setHomogeneous]("GI.Gtk.Objects.Stack#g:method:setHomogeneous"), [setInterpolateSize]("GI.Gtk.Objects.Stack#g:method:setInterpolateSize"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setTransitionDuration]("GI.Gtk.Objects.Stack#g:method:setTransitionDuration"), [setTransitionType]("GI.Gtk.Objects.Stack#g:method:setTransitionType"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVhomogeneous]("GI.Gtk.Objects.Stack#g:method:setVhomogeneous"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisibleChild]("GI.Gtk.Objects.Stack#g:method:setVisibleChild"), [setVisibleChildFull]("GI.Gtk.Objects.Stack#g:method:setVisibleChildFull"), [setVisibleChildName]("GI.Gtk.Objects.Stack#g:method:setVisibleChildName"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveStackMethod                      ,
#endif

-- ** addNamed #method:addNamed#

#if defined(ENABLE_OVERLOADING)
    StackAddNamedMethodInfo                 ,
#endif
    stackAddNamed                           ,


-- ** addTitled #method:addTitled#

#if defined(ENABLE_OVERLOADING)
    StackAddTitledMethodInfo                ,
#endif
    stackAddTitled                          ,


-- ** getChildByName #method:getChildByName#

#if defined(ENABLE_OVERLOADING)
    StackGetChildByNameMethodInfo           ,
#endif
    stackGetChildByName                     ,


-- ** getHhomogeneous #method:getHhomogeneous#

#if defined(ENABLE_OVERLOADING)
    StackGetHhomogeneousMethodInfo          ,
#endif
    stackGetHhomogeneous                    ,


-- ** getHomogeneous #method:getHomogeneous#

#if defined(ENABLE_OVERLOADING)
    StackGetHomogeneousMethodInfo           ,
#endif
    stackGetHomogeneous                     ,


-- ** getInterpolateSize #method:getInterpolateSize#

#if defined(ENABLE_OVERLOADING)
    StackGetInterpolateSizeMethodInfo       ,
#endif
    stackGetInterpolateSize                 ,


-- ** getTransitionDuration #method:getTransitionDuration#

#if defined(ENABLE_OVERLOADING)
    StackGetTransitionDurationMethodInfo    ,
#endif
    stackGetTransitionDuration              ,


-- ** getTransitionRunning #method:getTransitionRunning#

#if defined(ENABLE_OVERLOADING)
    StackGetTransitionRunningMethodInfo     ,
#endif
    stackGetTransitionRunning               ,


-- ** getTransitionType #method:getTransitionType#

#if defined(ENABLE_OVERLOADING)
    StackGetTransitionTypeMethodInfo        ,
#endif
    stackGetTransitionType                  ,


-- ** getVhomogeneous #method:getVhomogeneous#

#if defined(ENABLE_OVERLOADING)
    StackGetVhomogeneousMethodInfo          ,
#endif
    stackGetVhomogeneous                    ,


-- ** getVisibleChild #method:getVisibleChild#

#if defined(ENABLE_OVERLOADING)
    StackGetVisibleChildMethodInfo          ,
#endif
    stackGetVisibleChild                    ,


-- ** getVisibleChildName #method:getVisibleChildName#

#if defined(ENABLE_OVERLOADING)
    StackGetVisibleChildNameMethodInfo      ,
#endif
    stackGetVisibleChildName                ,


-- ** new #method:new#

    stackNew                                ,


-- ** setHhomogeneous #method:setHhomogeneous#

#if defined(ENABLE_OVERLOADING)
    StackSetHhomogeneousMethodInfo          ,
#endif
    stackSetHhomogeneous                    ,


-- ** setHomogeneous #method:setHomogeneous#

#if defined(ENABLE_OVERLOADING)
    StackSetHomogeneousMethodInfo           ,
#endif
    stackSetHomogeneous                     ,


-- ** setInterpolateSize #method:setInterpolateSize#

#if defined(ENABLE_OVERLOADING)
    StackSetInterpolateSizeMethodInfo       ,
#endif
    stackSetInterpolateSize                 ,


-- ** setTransitionDuration #method:setTransitionDuration#

#if defined(ENABLE_OVERLOADING)
    StackSetTransitionDurationMethodInfo    ,
#endif
    stackSetTransitionDuration              ,


-- ** setTransitionType #method:setTransitionType#

#if defined(ENABLE_OVERLOADING)
    StackSetTransitionTypeMethodInfo        ,
#endif
    stackSetTransitionType                  ,


-- ** setVhomogeneous #method:setVhomogeneous#

#if defined(ENABLE_OVERLOADING)
    StackSetVhomogeneousMethodInfo          ,
#endif
    stackSetVhomogeneous                    ,


-- ** setVisibleChild #method:setVisibleChild#

#if defined(ENABLE_OVERLOADING)
    StackSetVisibleChildMethodInfo          ,
#endif
    stackSetVisibleChild                    ,


-- ** setVisibleChildFull #method:setVisibleChildFull#

#if defined(ENABLE_OVERLOADING)
    StackSetVisibleChildFullMethodInfo      ,
#endif
    stackSetVisibleChildFull                ,


-- ** setVisibleChildName #method:setVisibleChildName#

#if defined(ENABLE_OVERLOADING)
    StackSetVisibleChildNameMethodInfo      ,
#endif
    stackSetVisibleChildName                ,




 -- * Properties


-- ** hhomogeneous #attr:hhomogeneous#
-- | 'P.True' if the stack allocates the same width for all children.
-- 
-- /Since: 3.16/

#if defined(ENABLE_OVERLOADING)
    StackHhomogeneousPropertyInfo           ,
#endif
    constructStackHhomogeneous              ,
    getStackHhomogeneous                    ,
    setStackHhomogeneous                    ,
#if defined(ENABLE_OVERLOADING)
    stackHhomogeneous                       ,
#endif


-- ** homogeneous #attr:homogeneous#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    StackHomogeneousPropertyInfo            ,
#endif
    constructStackHomogeneous               ,
    getStackHomogeneous                     ,
    setStackHomogeneous                     ,
#if defined(ENABLE_OVERLOADING)
    stackHomogeneous                        ,
#endif


-- ** interpolateSize #attr:interpolateSize#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    StackInterpolateSizePropertyInfo        ,
#endif
    constructStackInterpolateSize           ,
    getStackInterpolateSize                 ,
    setStackInterpolateSize                 ,
#if defined(ENABLE_OVERLOADING)
    stackInterpolateSize                    ,
#endif


-- ** transitionDuration #attr:transitionDuration#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    StackTransitionDurationPropertyInfo     ,
#endif
    constructStackTransitionDuration        ,
    getStackTransitionDuration              ,
    setStackTransitionDuration              ,
#if defined(ENABLE_OVERLOADING)
    stackTransitionDuration                 ,
#endif


-- ** transitionRunning #attr:transitionRunning#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    StackTransitionRunningPropertyInfo      ,
#endif
    getStackTransitionRunning               ,
#if defined(ENABLE_OVERLOADING)
    stackTransitionRunning                  ,
#endif


-- ** transitionType #attr:transitionType#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    StackTransitionTypePropertyInfo         ,
#endif
    constructStackTransitionType            ,
    getStackTransitionType                  ,
    setStackTransitionType                  ,
#if defined(ENABLE_OVERLOADING)
    stackTransitionType                     ,
#endif


-- ** vhomogeneous #attr:vhomogeneous#
-- | 'P.True' if the stack allocates the same height for all children.
-- 
-- /Since: 3.16/

#if defined(ENABLE_OVERLOADING)
    StackVhomogeneousPropertyInfo           ,
#endif
    constructStackVhomogeneous              ,
    getStackVhomogeneous                    ,
    setStackVhomogeneous                    ,
#if defined(ENABLE_OVERLOADING)
    stackVhomogeneous                       ,
#endif


-- ** visibleChild #attr:visibleChild#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    StackVisibleChildPropertyInfo           ,
#endif
    constructStackVisibleChild              ,
    getStackVisibleChild                    ,
    setStackVisibleChild                    ,
#if defined(ENABLE_OVERLOADING)
    stackVisibleChild                       ,
#endif


-- ** visibleChildName #attr:visibleChildName#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    StackVisibleChildNamePropertyInfo       ,
#endif
    constructStackVisibleChildName          ,
    getStackVisibleChildName                ,
    setStackVisibleChildName                ,
#if defined(ENABLE_OVERLOADING)
    stackVisibleChildName                   ,
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
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype Stack = Stack (SP.ManagedPtr Stack)
    deriving (Eq)

instance SP.ManagedPtrNewtype Stack where
    toManagedPtr (Stack p) = p

foreign import ccall "gtk_stack_get_type"
    c_gtk_stack_get_type :: IO B.Types.GType

instance B.Types.TypedObject Stack where
    glibType = c_gtk_stack_get_type

instance B.Types.GObject Stack

-- | Type class for types which can be safely cast to `Stack`, for instance with `toStack`.
class (SP.GObject o, O.IsDescendantOf Stack o) => IsStack o
instance (SP.GObject o, O.IsDescendantOf Stack o) => IsStack o

instance O.HasParentTypes Stack
type instance O.ParentTypes Stack = '[Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable]

-- | Cast to `Stack`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toStack :: (MIO.MonadIO m, IsStack o) => o -> m Stack
toStack = MIO.liftIO . B.ManagedPtr.unsafeCastTo Stack

-- | Convert 'Stack' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe Stack) where
    gvalueGType_ = c_gtk_stack_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr Stack)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr Stack)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject Stack ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveStackMethod (t :: Symbol) (o :: *) :: * where
    ResolveStackMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveStackMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveStackMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveStackMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveStackMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveStackMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveStackMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveStackMethod "addNamed" o = StackAddNamedMethodInfo
    ResolveStackMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveStackMethod "addTitled" o = StackAddTitledMethodInfo
    ResolveStackMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveStackMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveStackMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveStackMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveStackMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveStackMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveStackMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveStackMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveStackMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveStackMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveStackMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveStackMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveStackMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveStackMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveStackMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveStackMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveStackMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveStackMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveStackMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveStackMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveStackMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveStackMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveStackMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveStackMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveStackMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveStackMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveStackMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveStackMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveStackMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveStackMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveStackMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveStackMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveStackMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveStackMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveStackMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveStackMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveStackMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveStackMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveStackMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveStackMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveStackMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveStackMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveStackMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveStackMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveStackMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveStackMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveStackMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveStackMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveStackMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveStackMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveStackMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveStackMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveStackMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveStackMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveStackMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveStackMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveStackMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveStackMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveStackMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveStackMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveStackMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveStackMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveStackMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveStackMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveStackMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveStackMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveStackMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveStackMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveStackMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveStackMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveStackMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveStackMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveStackMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveStackMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveStackMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveStackMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveStackMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveStackMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveStackMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveStackMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveStackMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveStackMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveStackMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveStackMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveStackMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveStackMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveStackMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveStackMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveStackMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveStackMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveStackMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveStackMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveStackMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveStackMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveStackMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveStackMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveStackMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveStackMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveStackMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveStackMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveStackMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveStackMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveStackMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveStackMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveStackMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveStackMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveStackMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveStackMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveStackMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveStackMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveStackMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveStackMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveStackMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveStackMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveStackMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveStackMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveStackMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveStackMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveStackMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveStackMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolveStackMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveStackMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveStackMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveStackMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveStackMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveStackMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveStackMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveStackMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveStackMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveStackMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveStackMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveStackMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveStackMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveStackMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveStackMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveStackMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveStackMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveStackMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveStackMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveStackMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveStackMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveStackMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveStackMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveStackMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveStackMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveStackMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveStackMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveStackMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveStackMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveStackMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveStackMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveStackMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveStackMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveStackMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveStackMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveStackMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveStackMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveStackMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveStackMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveStackMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveStackMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveStackMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveStackMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveStackMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveStackMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveStackMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveStackMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveStackMethod "getChildByName" o = StackGetChildByNameMethodInfo
    ResolveStackMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveStackMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveStackMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveStackMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveStackMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveStackMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveStackMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveStackMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveStackMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveStackMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveStackMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveStackMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveStackMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveStackMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveStackMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveStackMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveStackMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveStackMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveStackMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveStackMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveStackMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveStackMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveStackMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveStackMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveStackMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveStackMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveStackMethod "getHhomogeneous" o = StackGetHhomogeneousMethodInfo
    ResolveStackMethod "getHomogeneous" o = StackGetHomogeneousMethodInfo
    ResolveStackMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveStackMethod "getInterpolateSize" o = StackGetInterpolateSizeMethodInfo
    ResolveStackMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveStackMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveStackMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveStackMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveStackMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveStackMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveStackMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveStackMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveStackMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveStackMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveStackMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveStackMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveStackMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveStackMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveStackMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveStackMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveStackMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveStackMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveStackMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveStackMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveStackMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveStackMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveStackMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveStackMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveStackMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveStackMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveStackMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveStackMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveStackMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveStackMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveStackMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveStackMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveStackMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveStackMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveStackMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveStackMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveStackMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveStackMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveStackMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveStackMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveStackMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveStackMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveStackMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveStackMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveStackMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveStackMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveStackMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveStackMethod "getTransitionDuration" o = StackGetTransitionDurationMethodInfo
    ResolveStackMethod "getTransitionRunning" o = StackGetTransitionRunningMethodInfo
    ResolveStackMethod "getTransitionType" o = StackGetTransitionTypeMethodInfo
    ResolveStackMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveStackMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveStackMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveStackMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveStackMethod "getVhomogeneous" o = StackGetVhomogeneousMethodInfo
    ResolveStackMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveStackMethod "getVisibleChild" o = StackGetVisibleChildMethodInfo
    ResolveStackMethod "getVisibleChildName" o = StackGetVisibleChildNameMethodInfo
    ResolveStackMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveStackMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveStackMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveStackMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveStackMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveStackMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveStackMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveStackMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveStackMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveStackMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveStackMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveStackMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveStackMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveStackMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveStackMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveStackMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveStackMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveStackMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveStackMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveStackMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveStackMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveStackMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveStackMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveStackMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveStackMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveStackMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveStackMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveStackMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveStackMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveStackMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveStackMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveStackMethod "setHhomogeneous" o = StackSetHhomogeneousMethodInfo
    ResolveStackMethod "setHomogeneous" o = StackSetHomogeneousMethodInfo
    ResolveStackMethod "setInterpolateSize" o = StackSetInterpolateSizeMethodInfo
    ResolveStackMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveStackMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveStackMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveStackMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveStackMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveStackMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveStackMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveStackMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveStackMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveStackMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveStackMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveStackMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveStackMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveStackMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveStackMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveStackMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveStackMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveStackMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveStackMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveStackMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveStackMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveStackMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveStackMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveStackMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveStackMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveStackMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveStackMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveStackMethod "setTransitionDuration" o = StackSetTransitionDurationMethodInfo
    ResolveStackMethod "setTransitionType" o = StackSetTransitionTypeMethodInfo
    ResolveStackMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveStackMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveStackMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveStackMethod "setVhomogeneous" o = StackSetVhomogeneousMethodInfo
    ResolveStackMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveStackMethod "setVisibleChild" o = StackSetVisibleChildMethodInfo
    ResolveStackMethod "setVisibleChildFull" o = StackSetVisibleChildFullMethodInfo
    ResolveStackMethod "setVisibleChildName" o = StackSetVisibleChildNameMethodInfo
    ResolveStackMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveStackMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveStackMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveStackMethod t Stack, O.OverloadedMethod info Stack p) => OL.IsLabel t (Stack -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveStackMethod t Stack, O.OverloadedMethod info Stack p, R.HasField t Stack p) => R.HasField t Stack p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveStackMethod t Stack, O.OverloadedMethodInfo info Stack) => OL.IsLabel t (O.MethodProxy info Stack) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- VVV Prop "hhomogeneous"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@hhomogeneous@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' stack #hhomogeneous
-- @
getStackHhomogeneous :: (MonadIO m, IsStack o) => o -> m Bool
getStackHhomogeneous obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "hhomogeneous"

-- | Set the value of the “@hhomogeneous@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' stack [ #hhomogeneous 'Data.GI.Base.Attributes.:=' value ]
-- @
setStackHhomogeneous :: (MonadIO m, IsStack o) => o -> Bool -> m ()
setStackHhomogeneous obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "hhomogeneous" val

-- | Construct a `GValueConstruct` with valid value for the “@hhomogeneous@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructStackHhomogeneous :: (IsStack o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructStackHhomogeneous val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "hhomogeneous" val

#if defined(ENABLE_OVERLOADING)
data StackHhomogeneousPropertyInfo
instance AttrInfo StackHhomogeneousPropertyInfo where
    type AttrAllowedOps StackHhomogeneousPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint StackHhomogeneousPropertyInfo = IsStack
    type AttrSetTypeConstraint StackHhomogeneousPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint StackHhomogeneousPropertyInfo = (~) Bool
    type AttrTransferType StackHhomogeneousPropertyInfo = Bool
    type AttrGetType StackHhomogeneousPropertyInfo = Bool
    type AttrLabel StackHhomogeneousPropertyInfo = "hhomogeneous"
    type AttrOrigin StackHhomogeneousPropertyInfo = Stack
    attrGet = getStackHhomogeneous
    attrSet = setStackHhomogeneous
    attrTransfer _ v = do
        return v
    attrConstruct = constructStackHhomogeneous
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Stack.hhomogeneous"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Stack.html#g:attr:hhomogeneous"
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
-- 'Data.GI.Base.Attributes.get' stack #homogeneous
-- @
getStackHomogeneous :: (MonadIO m, IsStack o) => o -> m Bool
getStackHomogeneous obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "homogeneous"

-- | Set the value of the “@homogeneous@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' stack [ #homogeneous 'Data.GI.Base.Attributes.:=' value ]
-- @
setStackHomogeneous :: (MonadIO m, IsStack o) => o -> Bool -> m ()
setStackHomogeneous obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "homogeneous" val

-- | Construct a `GValueConstruct` with valid value for the “@homogeneous@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructStackHomogeneous :: (IsStack o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructStackHomogeneous val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "homogeneous" val

#if defined(ENABLE_OVERLOADING)
data StackHomogeneousPropertyInfo
instance AttrInfo StackHomogeneousPropertyInfo where
    type AttrAllowedOps StackHomogeneousPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint StackHomogeneousPropertyInfo = IsStack
    type AttrSetTypeConstraint StackHomogeneousPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint StackHomogeneousPropertyInfo = (~) Bool
    type AttrTransferType StackHomogeneousPropertyInfo = Bool
    type AttrGetType StackHomogeneousPropertyInfo = Bool
    type AttrLabel StackHomogeneousPropertyInfo = "homogeneous"
    type AttrOrigin StackHomogeneousPropertyInfo = Stack
    attrGet = getStackHomogeneous
    attrSet = setStackHomogeneous
    attrTransfer _ v = do
        return v
    attrConstruct = constructStackHomogeneous
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Stack.homogeneous"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Stack.html#g:attr:homogeneous"
        })
#endif

-- VVV Prop "interpolate-size"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@interpolate-size@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' stack #interpolateSize
-- @
getStackInterpolateSize :: (MonadIO m, IsStack o) => o -> m Bool
getStackInterpolateSize obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "interpolate-size"

-- | Set the value of the “@interpolate-size@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' stack [ #interpolateSize 'Data.GI.Base.Attributes.:=' value ]
-- @
setStackInterpolateSize :: (MonadIO m, IsStack o) => o -> Bool -> m ()
setStackInterpolateSize obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "interpolate-size" val

-- | Construct a `GValueConstruct` with valid value for the “@interpolate-size@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructStackInterpolateSize :: (IsStack o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructStackInterpolateSize val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "interpolate-size" val

#if defined(ENABLE_OVERLOADING)
data StackInterpolateSizePropertyInfo
instance AttrInfo StackInterpolateSizePropertyInfo where
    type AttrAllowedOps StackInterpolateSizePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint StackInterpolateSizePropertyInfo = IsStack
    type AttrSetTypeConstraint StackInterpolateSizePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint StackInterpolateSizePropertyInfo = (~) Bool
    type AttrTransferType StackInterpolateSizePropertyInfo = Bool
    type AttrGetType StackInterpolateSizePropertyInfo = Bool
    type AttrLabel StackInterpolateSizePropertyInfo = "interpolate-size"
    type AttrOrigin StackInterpolateSizePropertyInfo = Stack
    attrGet = getStackInterpolateSize
    attrSet = setStackInterpolateSize
    attrTransfer _ v = do
        return v
    attrConstruct = constructStackInterpolateSize
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Stack.interpolateSize"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Stack.html#g:attr:interpolateSize"
        })
#endif

-- VVV Prop "transition-duration"
   -- Type: TBasicType TUInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@transition-duration@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' stack #transitionDuration
-- @
getStackTransitionDuration :: (MonadIO m, IsStack o) => o -> m Word32
getStackTransitionDuration obj = MIO.liftIO $ B.Properties.getObjectPropertyUInt32 obj "transition-duration"

-- | Set the value of the “@transition-duration@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' stack [ #transitionDuration 'Data.GI.Base.Attributes.:=' value ]
-- @
setStackTransitionDuration :: (MonadIO m, IsStack o) => o -> Word32 -> m ()
setStackTransitionDuration obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyUInt32 obj "transition-duration" val

-- | Construct a `GValueConstruct` with valid value for the “@transition-duration@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructStackTransitionDuration :: (IsStack o, MIO.MonadIO m) => Word32 -> m (GValueConstruct o)
constructStackTransitionDuration val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyUInt32 "transition-duration" val

#if defined(ENABLE_OVERLOADING)
data StackTransitionDurationPropertyInfo
instance AttrInfo StackTransitionDurationPropertyInfo where
    type AttrAllowedOps StackTransitionDurationPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint StackTransitionDurationPropertyInfo = IsStack
    type AttrSetTypeConstraint StackTransitionDurationPropertyInfo = (~) Word32
    type AttrTransferTypeConstraint StackTransitionDurationPropertyInfo = (~) Word32
    type AttrTransferType StackTransitionDurationPropertyInfo = Word32
    type AttrGetType StackTransitionDurationPropertyInfo = Word32
    type AttrLabel StackTransitionDurationPropertyInfo = "transition-duration"
    type AttrOrigin StackTransitionDurationPropertyInfo = Stack
    attrGet = getStackTransitionDuration
    attrSet = setStackTransitionDuration
    attrTransfer _ v = do
        return v
    attrConstruct = constructStackTransitionDuration
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Stack.transitionDuration"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Stack.html#g:attr:transitionDuration"
        })
#endif

-- VVV Prop "transition-running"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable]
   -- Nullable: (Just False,Nothing)

-- | Get the value of the “@transition-running@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' stack #transitionRunning
-- @
getStackTransitionRunning :: (MonadIO m, IsStack o) => o -> m Bool
getStackTransitionRunning obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "transition-running"

#if defined(ENABLE_OVERLOADING)
data StackTransitionRunningPropertyInfo
instance AttrInfo StackTransitionRunningPropertyInfo where
    type AttrAllowedOps StackTransitionRunningPropertyInfo = '[ 'AttrGet]
    type AttrBaseTypeConstraint StackTransitionRunningPropertyInfo = IsStack
    type AttrSetTypeConstraint StackTransitionRunningPropertyInfo = (~) ()
    type AttrTransferTypeConstraint StackTransitionRunningPropertyInfo = (~) ()
    type AttrTransferType StackTransitionRunningPropertyInfo = ()
    type AttrGetType StackTransitionRunningPropertyInfo = Bool
    type AttrLabel StackTransitionRunningPropertyInfo = "transition-running"
    type AttrOrigin StackTransitionRunningPropertyInfo = Stack
    attrGet = getStackTransitionRunning
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Stack.transitionRunning"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Stack.html#g:attr:transitionRunning"
        })
#endif

-- VVV Prop "transition-type"
   -- Type: TInterface (Name {namespace = "Gtk", name = "StackTransitionType"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@transition-type@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' stack #transitionType
-- @
getStackTransitionType :: (MonadIO m, IsStack o) => o -> m Gtk.Enums.StackTransitionType
getStackTransitionType obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "transition-type"

-- | Set the value of the “@transition-type@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' stack [ #transitionType 'Data.GI.Base.Attributes.:=' value ]
-- @
setStackTransitionType :: (MonadIO m, IsStack o) => o -> Gtk.Enums.StackTransitionType -> m ()
setStackTransitionType obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "transition-type" val

-- | Construct a `GValueConstruct` with valid value for the “@transition-type@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructStackTransitionType :: (IsStack o, MIO.MonadIO m) => Gtk.Enums.StackTransitionType -> m (GValueConstruct o)
constructStackTransitionType val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "transition-type" val

#if defined(ENABLE_OVERLOADING)
data StackTransitionTypePropertyInfo
instance AttrInfo StackTransitionTypePropertyInfo where
    type AttrAllowedOps StackTransitionTypePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint StackTransitionTypePropertyInfo = IsStack
    type AttrSetTypeConstraint StackTransitionTypePropertyInfo = (~) Gtk.Enums.StackTransitionType
    type AttrTransferTypeConstraint StackTransitionTypePropertyInfo = (~) Gtk.Enums.StackTransitionType
    type AttrTransferType StackTransitionTypePropertyInfo = Gtk.Enums.StackTransitionType
    type AttrGetType StackTransitionTypePropertyInfo = Gtk.Enums.StackTransitionType
    type AttrLabel StackTransitionTypePropertyInfo = "transition-type"
    type AttrOrigin StackTransitionTypePropertyInfo = Stack
    attrGet = getStackTransitionType
    attrSet = setStackTransitionType
    attrTransfer _ v = do
        return v
    attrConstruct = constructStackTransitionType
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Stack.transitionType"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Stack.html#g:attr:transitionType"
        })
#endif

-- VVV Prop "vhomogeneous"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@vhomogeneous@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' stack #vhomogeneous
-- @
getStackVhomogeneous :: (MonadIO m, IsStack o) => o -> m Bool
getStackVhomogeneous obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "vhomogeneous"

-- | Set the value of the “@vhomogeneous@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' stack [ #vhomogeneous 'Data.GI.Base.Attributes.:=' value ]
-- @
setStackVhomogeneous :: (MonadIO m, IsStack o) => o -> Bool -> m ()
setStackVhomogeneous obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "vhomogeneous" val

-- | Construct a `GValueConstruct` with valid value for the “@vhomogeneous@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructStackVhomogeneous :: (IsStack o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructStackVhomogeneous val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "vhomogeneous" val

#if defined(ENABLE_OVERLOADING)
data StackVhomogeneousPropertyInfo
instance AttrInfo StackVhomogeneousPropertyInfo where
    type AttrAllowedOps StackVhomogeneousPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint StackVhomogeneousPropertyInfo = IsStack
    type AttrSetTypeConstraint StackVhomogeneousPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint StackVhomogeneousPropertyInfo = (~) Bool
    type AttrTransferType StackVhomogeneousPropertyInfo = Bool
    type AttrGetType StackVhomogeneousPropertyInfo = Bool
    type AttrLabel StackVhomogeneousPropertyInfo = "vhomogeneous"
    type AttrOrigin StackVhomogeneousPropertyInfo = Stack
    attrGet = getStackVhomogeneous
    attrSet = setStackVhomogeneous
    attrTransfer _ v = do
        return v
    attrConstruct = constructStackVhomogeneous
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Stack.vhomogeneous"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Stack.html#g:attr:vhomogeneous"
        })
#endif

-- VVV Prop "visible-child"
   -- Type: TInterface (Name {namespace = "Gtk", name = "Widget"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just False)

-- | Get the value of the “@visible-child@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' stack #visibleChild
-- @
getStackVisibleChild :: (MonadIO m, IsStack o) => o -> m (Maybe Gtk.Widget.Widget)
getStackVisibleChild obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "visible-child" Gtk.Widget.Widget

-- | Set the value of the “@visible-child@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' stack [ #visibleChild 'Data.GI.Base.Attributes.:=' value ]
-- @
setStackVisibleChild :: (MonadIO m, IsStack o, Gtk.Widget.IsWidget a) => o -> a -> m ()
setStackVisibleChild obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "visible-child" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@visible-child@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructStackVisibleChild :: (IsStack o, MIO.MonadIO m, Gtk.Widget.IsWidget a) => a -> m (GValueConstruct o)
constructStackVisibleChild val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "visible-child" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data StackVisibleChildPropertyInfo
instance AttrInfo StackVisibleChildPropertyInfo where
    type AttrAllowedOps StackVisibleChildPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint StackVisibleChildPropertyInfo = IsStack
    type AttrSetTypeConstraint StackVisibleChildPropertyInfo = Gtk.Widget.IsWidget
    type AttrTransferTypeConstraint StackVisibleChildPropertyInfo = Gtk.Widget.IsWidget
    type AttrTransferType StackVisibleChildPropertyInfo = Gtk.Widget.Widget
    type AttrGetType StackVisibleChildPropertyInfo = (Maybe Gtk.Widget.Widget)
    type AttrLabel StackVisibleChildPropertyInfo = "visible-child"
    type AttrOrigin StackVisibleChildPropertyInfo = Stack
    attrGet = getStackVisibleChild
    attrSet = setStackVisibleChild
    attrTransfer _ v = do
        unsafeCastTo Gtk.Widget.Widget v
    attrConstruct = constructStackVisibleChild
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Stack.visibleChild"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Stack.html#g:attr:visibleChild"
        })
#endif

-- VVV Prop "visible-child-name"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just False)

-- | Get the value of the “@visible-child-name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' stack #visibleChildName
-- @
getStackVisibleChildName :: (MonadIO m, IsStack o) => o -> m (Maybe T.Text)
getStackVisibleChildName obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "visible-child-name"

-- | Set the value of the “@visible-child-name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' stack [ #visibleChildName 'Data.GI.Base.Attributes.:=' value ]
-- @
setStackVisibleChildName :: (MonadIO m, IsStack o) => o -> T.Text -> m ()
setStackVisibleChildName obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "visible-child-name" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@visible-child-name@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructStackVisibleChildName :: (IsStack o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructStackVisibleChildName val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "visible-child-name" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data StackVisibleChildNamePropertyInfo
instance AttrInfo StackVisibleChildNamePropertyInfo where
    type AttrAllowedOps StackVisibleChildNamePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint StackVisibleChildNamePropertyInfo = IsStack
    type AttrSetTypeConstraint StackVisibleChildNamePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint StackVisibleChildNamePropertyInfo = (~) T.Text
    type AttrTransferType StackVisibleChildNamePropertyInfo = T.Text
    type AttrGetType StackVisibleChildNamePropertyInfo = (Maybe T.Text)
    type AttrLabel StackVisibleChildNamePropertyInfo = "visible-child-name"
    type AttrOrigin StackVisibleChildNamePropertyInfo = Stack
    attrGet = getStackVisibleChildName
    attrSet = setStackVisibleChildName
    attrTransfer _ v = do
        return v
    attrConstruct = constructStackVisibleChildName
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Stack.visibleChildName"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Stack.html#g:attr:visibleChildName"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList Stack
type instance O.AttributeList Stack = StackAttributeList
type StackAttributeList = ('[ '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("hhomogeneous", StackHhomogeneousPropertyInfo), '("homogeneous", StackHomogeneousPropertyInfo), '("interpolateSize", StackInterpolateSizePropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("transitionDuration", StackTransitionDurationPropertyInfo), '("transitionRunning", StackTransitionRunningPropertyInfo), '("transitionType", StackTransitionTypePropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("vhomogeneous", StackVhomogeneousPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("visibleChild", StackVisibleChildPropertyInfo), '("visibleChildName", StackVisibleChildNamePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
stackHhomogeneous :: AttrLabelProxy "hhomogeneous"
stackHhomogeneous = AttrLabelProxy

stackHomogeneous :: AttrLabelProxy "homogeneous"
stackHomogeneous = AttrLabelProxy

stackInterpolateSize :: AttrLabelProxy "interpolateSize"
stackInterpolateSize = AttrLabelProxy

stackTransitionDuration :: AttrLabelProxy "transitionDuration"
stackTransitionDuration = AttrLabelProxy

stackTransitionRunning :: AttrLabelProxy "transitionRunning"
stackTransitionRunning = AttrLabelProxy

stackTransitionType :: AttrLabelProxy "transitionType"
stackTransitionType = AttrLabelProxy

stackVhomogeneous :: AttrLabelProxy "vhomogeneous"
stackVhomogeneous = AttrLabelProxy

stackVisibleChild :: AttrLabelProxy "visibleChild"
stackVisibleChild = AttrLabelProxy

stackVisibleChildName :: AttrLabelProxy "visibleChildName"
stackVisibleChildName = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList Stack = StackSignalList
type StackSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method Stack::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Stack" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_stack_new" gtk_stack_new :: 
    IO (Ptr Stack)

-- | Creates a new t'GI.Gtk.Objects.Stack.Stack' container.
-- 
-- /Since: 3.10/
stackNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m Stack
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.Stack.Stack'
stackNew  = liftIO $ do
    result <- gtk_stack_new
    checkUnexpectedReturnNULL "stackNew" result
    result' <- (newObject Stack) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Stack::add_named
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "stack"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Stack" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStack" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the widget to add" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the name for @child"
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

foreign import ccall "gtk_stack_add_named" gtk_stack_add_named :: 
    Ptr Stack ->                            -- stack : TInterface (Name {namespace = "Gtk", name = "Stack"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CString ->                              -- name : TBasicType TUTF8
    IO ()

-- | Adds a child to /@stack@/.
-- The child is identified by the /@name@/.
-- 
-- /Since: 3.10/
stackAddNamed ::
    (B.CallStack.HasCallStack, MonadIO m, IsStack a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@stack@/: a t'GI.Gtk.Objects.Stack.Stack'
    -> b
    -- ^ /@child@/: the widget to add
    -> T.Text
    -- ^ /@name@/: the name for /@child@/
    -> m ()
stackAddNamed stack child name = liftIO $ do
    stack' <- unsafeManagedPtrCastPtr stack
    child' <- unsafeManagedPtrCastPtr child
    name' <- textToCString name
    gtk_stack_add_named stack' child' name'
    touchManagedPtr stack
    touchManagedPtr child
    freeMem name'
    return ()

#if defined(ENABLE_OVERLOADING)
data StackAddNamedMethodInfo
instance (signature ~ (b -> T.Text -> m ()), MonadIO m, IsStack a, Gtk.Widget.IsWidget b) => O.OverloadedMethod StackAddNamedMethodInfo a signature where
    overloadedMethod = stackAddNamed

instance O.OverloadedMethodInfo StackAddNamedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Stack.stackAddNamed",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Stack.html#v:stackAddNamed"
        })


#endif

-- method Stack::add_titled
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "stack"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Stack" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStack" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the widget to add" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the name for @child"
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
--                 { rawDocText = Just "a human-readable title for @child"
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

foreign import ccall "gtk_stack_add_titled" gtk_stack_add_titled :: 
    Ptr Stack ->                            -- stack : TInterface (Name {namespace = "Gtk", name = "Stack"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CString ->                              -- name : TBasicType TUTF8
    CString ->                              -- title : TBasicType TUTF8
    IO ()

-- | Adds a child to /@stack@/.
-- The child is identified by the /@name@/. The /@title@/
-- will be used by t'GI.Gtk.Objects.StackSwitcher.StackSwitcher' to represent
-- /@child@/ in a tab bar, so it should be short.
-- 
-- /Since: 3.10/
stackAddTitled ::
    (B.CallStack.HasCallStack, MonadIO m, IsStack a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@stack@/: a t'GI.Gtk.Objects.Stack.Stack'
    -> b
    -- ^ /@child@/: the widget to add
    -> T.Text
    -- ^ /@name@/: the name for /@child@/
    -> T.Text
    -- ^ /@title@/: a human-readable title for /@child@/
    -> m ()
stackAddTitled stack child name title = liftIO $ do
    stack' <- unsafeManagedPtrCastPtr stack
    child' <- unsafeManagedPtrCastPtr child
    name' <- textToCString name
    title' <- textToCString title
    gtk_stack_add_titled stack' child' name' title'
    touchManagedPtr stack
    touchManagedPtr child
    freeMem name'
    freeMem title'
    return ()

#if defined(ENABLE_OVERLOADING)
data StackAddTitledMethodInfo
instance (signature ~ (b -> T.Text -> T.Text -> m ()), MonadIO m, IsStack a, Gtk.Widget.IsWidget b) => O.OverloadedMethod StackAddTitledMethodInfo a signature where
    overloadedMethod = stackAddTitled

instance O.OverloadedMethodInfo StackAddTitledMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Stack.stackAddTitled",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Stack.html#v:stackAddTitled"
        })


#endif

-- method Stack::get_child_by_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "stack"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Stack" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStack" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the name of the child to find"
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

foreign import ccall "gtk_stack_get_child_by_name" gtk_stack_get_child_by_name :: 
    Ptr Stack ->                            -- stack : TInterface (Name {namespace = "Gtk", name = "Stack"})
    CString ->                              -- name : TBasicType TUTF8
    IO (Ptr Gtk.Widget.Widget)

-- | Finds the child of the t'GI.Gtk.Objects.Stack.Stack' with the name given as
-- the argument. Returns 'P.Nothing' if there is no child with this
-- name.
-- 
-- /Since: 3.12/
stackGetChildByName ::
    (B.CallStack.HasCallStack, MonadIO m, IsStack a) =>
    a
    -- ^ /@stack@/: a t'GI.Gtk.Objects.Stack.Stack'
    -> T.Text
    -- ^ /@name@/: the name of the child to find
    -> m (Maybe Gtk.Widget.Widget)
    -- ^ __Returns:__ the requested child of the t'GI.Gtk.Objects.Stack.Stack'
stackGetChildByName stack name = liftIO $ do
    stack' <- unsafeManagedPtrCastPtr stack
    name' <- textToCString name
    result <- gtk_stack_get_child_by_name stack' name'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Widget.Widget) result'
        return result''
    touchManagedPtr stack
    freeMem name'
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data StackGetChildByNameMethodInfo
instance (signature ~ (T.Text -> m (Maybe Gtk.Widget.Widget)), MonadIO m, IsStack a) => O.OverloadedMethod StackGetChildByNameMethodInfo a signature where
    overloadedMethod = stackGetChildByName

instance O.OverloadedMethodInfo StackGetChildByNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Stack.stackGetChildByName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Stack.html#v:stackGetChildByName"
        })


#endif

-- method Stack::get_hhomogeneous
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "stack"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Stack" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStack" , sinceVersion = Nothing }
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

foreign import ccall "gtk_stack_get_hhomogeneous" gtk_stack_get_hhomogeneous :: 
    Ptr Stack ->                            -- stack : TInterface (Name {namespace = "Gtk", name = "Stack"})
    IO CInt

-- | Gets whether /@stack@/ is horizontally homogeneous.
-- See 'GI.Gtk.Objects.Stack.stackSetHhomogeneous'.
-- 
-- /Since: 3.16/
stackGetHhomogeneous ::
    (B.CallStack.HasCallStack, MonadIO m, IsStack a) =>
    a
    -- ^ /@stack@/: a t'GI.Gtk.Objects.Stack.Stack'
    -> m Bool
    -- ^ __Returns:__ whether /@stack@/ is horizontally homogeneous.
stackGetHhomogeneous stack = liftIO $ do
    stack' <- unsafeManagedPtrCastPtr stack
    result <- gtk_stack_get_hhomogeneous stack'
    let result' = (/= 0) result
    touchManagedPtr stack
    return result'

#if defined(ENABLE_OVERLOADING)
data StackGetHhomogeneousMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsStack a) => O.OverloadedMethod StackGetHhomogeneousMethodInfo a signature where
    overloadedMethod = stackGetHhomogeneous

instance O.OverloadedMethodInfo StackGetHhomogeneousMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Stack.stackGetHhomogeneous",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Stack.html#v:stackGetHhomogeneous"
        })


#endif

-- method Stack::get_homogeneous
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "stack"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Stack" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStack" , sinceVersion = Nothing }
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

foreign import ccall "gtk_stack_get_homogeneous" gtk_stack_get_homogeneous :: 
    Ptr Stack ->                            -- stack : TInterface (Name {namespace = "Gtk", name = "Stack"})
    IO CInt

-- | Gets whether /@stack@/ is homogeneous.
-- See 'GI.Gtk.Objects.Stack.stackSetHomogeneous'.
-- 
-- /Since: 3.10/
stackGetHomogeneous ::
    (B.CallStack.HasCallStack, MonadIO m, IsStack a) =>
    a
    -- ^ /@stack@/: a t'GI.Gtk.Objects.Stack.Stack'
    -> m Bool
    -- ^ __Returns:__ whether /@stack@/ is homogeneous.
stackGetHomogeneous stack = liftIO $ do
    stack' <- unsafeManagedPtrCastPtr stack
    result <- gtk_stack_get_homogeneous stack'
    let result' = (/= 0) result
    touchManagedPtr stack
    return result'

#if defined(ENABLE_OVERLOADING)
data StackGetHomogeneousMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsStack a) => O.OverloadedMethod StackGetHomogeneousMethodInfo a signature where
    overloadedMethod = stackGetHomogeneous

instance O.OverloadedMethodInfo StackGetHomogeneousMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Stack.stackGetHomogeneous",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Stack.html#v:stackGetHomogeneous"
        })


#endif

-- method Stack::get_interpolate_size
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "stack"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Stack" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkStack" , sinceVersion = Nothing }
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

foreign import ccall "gtk_stack_get_interpolate_size" gtk_stack_get_interpolate_size :: 
    Ptr Stack ->                            -- stack : TInterface (Name {namespace = "Gtk", name = "Stack"})
    IO CInt

-- | Returns wether the t'GI.Gtk.Objects.Stack.Stack' is set up to interpolate between
-- the sizes of children on page switch.
-- 
-- /Since: 3.18/
stackGetInterpolateSize ::
    (B.CallStack.HasCallStack, MonadIO m, IsStack a) =>
    a
    -- ^ /@stack@/: A t'GI.Gtk.Objects.Stack.Stack'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if child sizes are interpolated
stackGetInterpolateSize stack = liftIO $ do
    stack' <- unsafeManagedPtrCastPtr stack
    result <- gtk_stack_get_interpolate_size stack'
    let result' = (/= 0) result
    touchManagedPtr stack
    return result'

#if defined(ENABLE_OVERLOADING)
data StackGetInterpolateSizeMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsStack a) => O.OverloadedMethod StackGetInterpolateSizeMethodInfo a signature where
    overloadedMethod = stackGetInterpolateSize

instance O.OverloadedMethodInfo StackGetInterpolateSizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Stack.stackGetInterpolateSize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Stack.html#v:stackGetInterpolateSize"
        })


#endif

-- method Stack::get_transition_duration
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "stack"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Stack" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStack" , sinceVersion = Nothing }
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

foreign import ccall "gtk_stack_get_transition_duration" gtk_stack_get_transition_duration :: 
    Ptr Stack ->                            -- stack : TInterface (Name {namespace = "Gtk", name = "Stack"})
    IO Word32

-- | Returns the amount of time (in milliseconds) that
-- transitions between pages in /@stack@/ will take.
-- 
-- /Since: 3.10/
stackGetTransitionDuration ::
    (B.CallStack.HasCallStack, MonadIO m, IsStack a) =>
    a
    -- ^ /@stack@/: a t'GI.Gtk.Objects.Stack.Stack'
    -> m Word32
    -- ^ __Returns:__ the transition duration
stackGetTransitionDuration stack = liftIO $ do
    stack' <- unsafeManagedPtrCastPtr stack
    result <- gtk_stack_get_transition_duration stack'
    touchManagedPtr stack
    return result

#if defined(ENABLE_OVERLOADING)
data StackGetTransitionDurationMethodInfo
instance (signature ~ (m Word32), MonadIO m, IsStack a) => O.OverloadedMethod StackGetTransitionDurationMethodInfo a signature where
    overloadedMethod = stackGetTransitionDuration

instance O.OverloadedMethodInfo StackGetTransitionDurationMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Stack.stackGetTransitionDuration",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Stack.html#v:stackGetTransitionDuration"
        })


#endif

-- method Stack::get_transition_running
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "stack"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Stack" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStack" , sinceVersion = Nothing }
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

foreign import ccall "gtk_stack_get_transition_running" gtk_stack_get_transition_running :: 
    Ptr Stack ->                            -- stack : TInterface (Name {namespace = "Gtk", name = "Stack"})
    IO CInt

-- | Returns whether the /@stack@/ is currently in a transition from one page to
-- another.
-- 
-- /Since: 3.12/
stackGetTransitionRunning ::
    (B.CallStack.HasCallStack, MonadIO m, IsStack a) =>
    a
    -- ^ /@stack@/: a t'GI.Gtk.Objects.Stack.Stack'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the transition is currently running, 'P.False' otherwise.
stackGetTransitionRunning stack = liftIO $ do
    stack' <- unsafeManagedPtrCastPtr stack
    result <- gtk_stack_get_transition_running stack'
    let result' = (/= 0) result
    touchManagedPtr stack
    return result'

#if defined(ENABLE_OVERLOADING)
data StackGetTransitionRunningMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsStack a) => O.OverloadedMethod StackGetTransitionRunningMethodInfo a signature where
    overloadedMethod = stackGetTransitionRunning

instance O.OverloadedMethodInfo StackGetTransitionRunningMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Stack.stackGetTransitionRunning",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Stack.html#v:stackGetTransitionRunning"
        })


#endif

-- method Stack::get_transition_type
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "stack"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Stack" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStack" , sinceVersion = Nothing }
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
--                  Name { namespace = "Gtk" , name = "StackTransitionType" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_stack_get_transition_type" gtk_stack_get_transition_type :: 
    Ptr Stack ->                            -- stack : TInterface (Name {namespace = "Gtk", name = "Stack"})
    IO CUInt

-- | Gets the type of animation that will be used
-- for transitions between pages in /@stack@/.
-- 
-- /Since: 3.10/
stackGetTransitionType ::
    (B.CallStack.HasCallStack, MonadIO m, IsStack a) =>
    a
    -- ^ /@stack@/: a t'GI.Gtk.Objects.Stack.Stack'
    -> m Gtk.Enums.StackTransitionType
    -- ^ __Returns:__ the current transition type of /@stack@/
stackGetTransitionType stack = liftIO $ do
    stack' <- unsafeManagedPtrCastPtr stack
    result <- gtk_stack_get_transition_type stack'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr stack
    return result'

#if defined(ENABLE_OVERLOADING)
data StackGetTransitionTypeMethodInfo
instance (signature ~ (m Gtk.Enums.StackTransitionType), MonadIO m, IsStack a) => O.OverloadedMethod StackGetTransitionTypeMethodInfo a signature where
    overloadedMethod = stackGetTransitionType

instance O.OverloadedMethodInfo StackGetTransitionTypeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Stack.stackGetTransitionType",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Stack.html#v:stackGetTransitionType"
        })


#endif

-- method Stack::get_vhomogeneous
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "stack"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Stack" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStack" , sinceVersion = Nothing }
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

foreign import ccall "gtk_stack_get_vhomogeneous" gtk_stack_get_vhomogeneous :: 
    Ptr Stack ->                            -- stack : TInterface (Name {namespace = "Gtk", name = "Stack"})
    IO CInt

-- | Gets whether /@stack@/ is vertically homogeneous.
-- See 'GI.Gtk.Objects.Stack.stackSetVhomogeneous'.
-- 
-- /Since: 3.16/
stackGetVhomogeneous ::
    (B.CallStack.HasCallStack, MonadIO m, IsStack a) =>
    a
    -- ^ /@stack@/: a t'GI.Gtk.Objects.Stack.Stack'
    -> m Bool
    -- ^ __Returns:__ whether /@stack@/ is vertically homogeneous.
stackGetVhomogeneous stack = liftIO $ do
    stack' <- unsafeManagedPtrCastPtr stack
    result <- gtk_stack_get_vhomogeneous stack'
    let result' = (/= 0) result
    touchManagedPtr stack
    return result'

#if defined(ENABLE_OVERLOADING)
data StackGetVhomogeneousMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsStack a) => O.OverloadedMethod StackGetVhomogeneousMethodInfo a signature where
    overloadedMethod = stackGetVhomogeneous

instance O.OverloadedMethodInfo StackGetVhomogeneousMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Stack.stackGetVhomogeneous",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Stack.html#v:stackGetVhomogeneous"
        })


#endif

-- method Stack::get_visible_child
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "stack"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Stack" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStack" , sinceVersion = Nothing }
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

foreign import ccall "gtk_stack_get_visible_child" gtk_stack_get_visible_child :: 
    Ptr Stack ->                            -- stack : TInterface (Name {namespace = "Gtk", name = "Stack"})
    IO (Ptr Gtk.Widget.Widget)

-- | Gets the currently visible child of /@stack@/, or 'P.Nothing' if
-- there are no visible children.
-- 
-- /Since: 3.10/
stackGetVisibleChild ::
    (B.CallStack.HasCallStack, MonadIO m, IsStack a) =>
    a
    -- ^ /@stack@/: a t'GI.Gtk.Objects.Stack.Stack'
    -> m (Maybe Gtk.Widget.Widget)
    -- ^ __Returns:__ the visible child of the t'GI.Gtk.Objects.Stack.Stack'
stackGetVisibleChild stack = liftIO $ do
    stack' <- unsafeManagedPtrCastPtr stack
    result <- gtk_stack_get_visible_child stack'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Widget.Widget) result'
        return result''
    touchManagedPtr stack
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data StackGetVisibleChildMethodInfo
instance (signature ~ (m (Maybe Gtk.Widget.Widget)), MonadIO m, IsStack a) => O.OverloadedMethod StackGetVisibleChildMethodInfo a signature where
    overloadedMethod = stackGetVisibleChild

instance O.OverloadedMethodInfo StackGetVisibleChildMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Stack.stackGetVisibleChild",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Stack.html#v:stackGetVisibleChild"
        })


#endif

-- method Stack::get_visible_child_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "stack"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Stack" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStack" , sinceVersion = Nothing }
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

foreign import ccall "gtk_stack_get_visible_child_name" gtk_stack_get_visible_child_name :: 
    Ptr Stack ->                            -- stack : TInterface (Name {namespace = "Gtk", name = "Stack"})
    IO CString

-- | Returns the name of the currently visible child of /@stack@/, or
-- 'P.Nothing' if there is no visible child.
-- 
-- /Since: 3.10/
stackGetVisibleChildName ::
    (B.CallStack.HasCallStack, MonadIO m, IsStack a) =>
    a
    -- ^ /@stack@/: a t'GI.Gtk.Objects.Stack.Stack'
    -> m (Maybe T.Text)
    -- ^ __Returns:__ the name of the visible child of the t'GI.Gtk.Objects.Stack.Stack'
stackGetVisibleChildName stack = liftIO $ do
    stack' <- unsafeManagedPtrCastPtr stack
    result <- gtk_stack_get_visible_child_name stack'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToText result'
        return result''
    touchManagedPtr stack
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data StackGetVisibleChildNameMethodInfo
instance (signature ~ (m (Maybe T.Text)), MonadIO m, IsStack a) => O.OverloadedMethod StackGetVisibleChildNameMethodInfo a signature where
    overloadedMethod = stackGetVisibleChildName

instance O.OverloadedMethodInfo StackGetVisibleChildNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Stack.stackGetVisibleChildName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Stack.html#v:stackGetVisibleChildName"
        })


#endif

-- method Stack::set_hhomogeneous
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "stack"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Stack" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStack" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "hhomogeneous"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE to make @stack horizontally homogeneous"
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

foreign import ccall "gtk_stack_set_hhomogeneous" gtk_stack_set_hhomogeneous :: 
    Ptr Stack ->                            -- stack : TInterface (Name {namespace = "Gtk", name = "Stack"})
    CInt ->                                 -- hhomogeneous : TBasicType TBoolean
    IO ()

-- | Sets the t'GI.Gtk.Objects.Stack.Stack' to be horizontally homogeneous or not.
-- If it is homogeneous, the t'GI.Gtk.Objects.Stack.Stack' will request the same
-- width for all its children. If it isn\'t, the stack
-- may change width when a different child becomes visible.
-- 
-- /Since: 3.16/
stackSetHhomogeneous ::
    (B.CallStack.HasCallStack, MonadIO m, IsStack a) =>
    a
    -- ^ /@stack@/: a t'GI.Gtk.Objects.Stack.Stack'
    -> Bool
    -- ^ /@hhomogeneous@/: 'P.True' to make /@stack@/ horizontally homogeneous
    -> m ()
stackSetHhomogeneous stack hhomogeneous = liftIO $ do
    stack' <- unsafeManagedPtrCastPtr stack
    let hhomogeneous' = (fromIntegral . fromEnum) hhomogeneous
    gtk_stack_set_hhomogeneous stack' hhomogeneous'
    touchManagedPtr stack
    return ()

#if defined(ENABLE_OVERLOADING)
data StackSetHhomogeneousMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsStack a) => O.OverloadedMethod StackSetHhomogeneousMethodInfo a signature where
    overloadedMethod = stackSetHhomogeneous

instance O.OverloadedMethodInfo StackSetHhomogeneousMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Stack.stackSetHhomogeneous",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Stack.html#v:stackSetHhomogeneous"
        })


#endif

-- method Stack::set_homogeneous
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "stack"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Stack" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStack" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "%TRUE to make @stack homogeneous"
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

foreign import ccall "gtk_stack_set_homogeneous" gtk_stack_set_homogeneous :: 
    Ptr Stack ->                            -- stack : TInterface (Name {namespace = "Gtk", name = "Stack"})
    CInt ->                                 -- homogeneous : TBasicType TBoolean
    IO ()

-- | Sets the t'GI.Gtk.Objects.Stack.Stack' to be homogeneous or not. If it
-- is homogeneous, the t'GI.Gtk.Objects.Stack.Stack' will request the same
-- size for all its children. If it isn\'t, the stack
-- may change size when a different child becomes visible.
-- 
-- Since 3.16, homogeneity can be controlled separately
-- for horizontal and vertical size, with the
-- [Stack:hhomogeneous]("GI.Gtk.Objects.Stack#g:attr:hhomogeneous") and [Stack:vhomogeneous]("GI.Gtk.Objects.Stack#g:attr:vhomogeneous").
-- 
-- /Since: 3.10/
stackSetHomogeneous ::
    (B.CallStack.HasCallStack, MonadIO m, IsStack a) =>
    a
    -- ^ /@stack@/: a t'GI.Gtk.Objects.Stack.Stack'
    -> Bool
    -- ^ /@homogeneous@/: 'P.True' to make /@stack@/ homogeneous
    -> m ()
stackSetHomogeneous stack homogeneous = liftIO $ do
    stack' <- unsafeManagedPtrCastPtr stack
    let homogeneous' = (fromIntegral . fromEnum) homogeneous
    gtk_stack_set_homogeneous stack' homogeneous'
    touchManagedPtr stack
    return ()

#if defined(ENABLE_OVERLOADING)
data StackSetHomogeneousMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsStack a) => O.OverloadedMethod StackSetHomogeneousMethodInfo a signature where
    overloadedMethod = stackSetHomogeneous

instance O.OverloadedMethodInfo StackSetHomogeneousMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Stack.stackSetHomogeneous",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Stack.html#v:stackSetHomogeneous"
        })


#endif

-- method Stack::set_interpolate_size
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "stack"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Stack" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkStack" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "interpolate_size"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the new value" , sinceVersion = Nothing }
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

foreign import ccall "gtk_stack_set_interpolate_size" gtk_stack_set_interpolate_size :: 
    Ptr Stack ->                            -- stack : TInterface (Name {namespace = "Gtk", name = "Stack"})
    CInt ->                                 -- interpolate_size : TBasicType TBoolean
    IO ()

-- | Sets whether or not /@stack@/ will interpolate its size when
-- changing the visible child. If the [Stack:interpolateSize]("GI.Gtk.Objects.Stack#g:attr:interpolateSize")
-- property is set to 'P.True', /@stack@/ will interpolate its size between
-- the current one and the one it\'ll take after changing the
-- visible child, according to the set transition duration.
-- 
-- /Since: 3.18/
stackSetInterpolateSize ::
    (B.CallStack.HasCallStack, MonadIO m, IsStack a) =>
    a
    -- ^ /@stack@/: A t'GI.Gtk.Objects.Stack.Stack'
    -> Bool
    -- ^ /@interpolateSize@/: the new value
    -> m ()
stackSetInterpolateSize stack interpolateSize = liftIO $ do
    stack' <- unsafeManagedPtrCastPtr stack
    let interpolateSize' = (fromIntegral . fromEnum) interpolateSize
    gtk_stack_set_interpolate_size stack' interpolateSize'
    touchManagedPtr stack
    return ()

#if defined(ENABLE_OVERLOADING)
data StackSetInterpolateSizeMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsStack a) => O.OverloadedMethod StackSetInterpolateSizeMethodInfo a signature where
    overloadedMethod = stackSetInterpolateSize

instance O.OverloadedMethodInfo StackSetInterpolateSizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Stack.stackSetInterpolateSize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Stack.html#v:stackSetInterpolateSize"
        })


#endif

-- method Stack::set_transition_duration
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "stack"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Stack" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStack" , sinceVersion = Nothing }
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

foreign import ccall "gtk_stack_set_transition_duration" gtk_stack_set_transition_duration :: 
    Ptr Stack ->                            -- stack : TInterface (Name {namespace = "Gtk", name = "Stack"})
    Word32 ->                               -- duration : TBasicType TUInt
    IO ()

-- | Sets the duration that transitions between pages in /@stack@/
-- will take.
-- 
-- /Since: 3.10/
stackSetTransitionDuration ::
    (B.CallStack.HasCallStack, MonadIO m, IsStack a) =>
    a
    -- ^ /@stack@/: a t'GI.Gtk.Objects.Stack.Stack'
    -> Word32
    -- ^ /@duration@/: the new duration, in milliseconds
    -> m ()
stackSetTransitionDuration stack duration = liftIO $ do
    stack' <- unsafeManagedPtrCastPtr stack
    gtk_stack_set_transition_duration stack' duration
    touchManagedPtr stack
    return ()

#if defined(ENABLE_OVERLOADING)
data StackSetTransitionDurationMethodInfo
instance (signature ~ (Word32 -> m ()), MonadIO m, IsStack a) => O.OverloadedMethod StackSetTransitionDurationMethodInfo a signature where
    overloadedMethod = stackSetTransitionDuration

instance O.OverloadedMethodInfo StackSetTransitionDurationMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Stack.stackSetTransitionDuration",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Stack.html#v:stackSetTransitionDuration"
        })


#endif

-- method Stack::set_transition_type
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "stack"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Stack" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStack" , sinceVersion = Nothing }
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
--                 Name { namespace = "Gtk" , name = "StackTransitionType" }
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

foreign import ccall "gtk_stack_set_transition_type" gtk_stack_set_transition_type :: 
    Ptr Stack ->                            -- stack : TInterface (Name {namespace = "Gtk", name = "Stack"})
    CUInt ->                                -- transition : TInterface (Name {namespace = "Gtk", name = "StackTransitionType"})
    IO ()

-- | Sets the type of animation that will be used for
-- transitions between pages in /@stack@/. Available
-- types include various kinds of fades and slides.
-- 
-- The transition type can be changed without problems
-- at runtime, so it is possible to change the animation
-- based on the page that is about to become current.
-- 
-- /Since: 3.10/
stackSetTransitionType ::
    (B.CallStack.HasCallStack, MonadIO m, IsStack a) =>
    a
    -- ^ /@stack@/: a t'GI.Gtk.Objects.Stack.Stack'
    -> Gtk.Enums.StackTransitionType
    -- ^ /@transition@/: the new transition type
    -> m ()
stackSetTransitionType stack transition = liftIO $ do
    stack' <- unsafeManagedPtrCastPtr stack
    let transition' = (fromIntegral . fromEnum) transition
    gtk_stack_set_transition_type stack' transition'
    touchManagedPtr stack
    return ()

#if defined(ENABLE_OVERLOADING)
data StackSetTransitionTypeMethodInfo
instance (signature ~ (Gtk.Enums.StackTransitionType -> m ()), MonadIO m, IsStack a) => O.OverloadedMethod StackSetTransitionTypeMethodInfo a signature where
    overloadedMethod = stackSetTransitionType

instance O.OverloadedMethodInfo StackSetTransitionTypeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Stack.stackSetTransitionType",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Stack.html#v:stackSetTransitionType"
        })


#endif

-- method Stack::set_vhomogeneous
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "stack"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Stack" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStack" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "vhomogeneous"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE to make @stack vertically homogeneous"
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

foreign import ccall "gtk_stack_set_vhomogeneous" gtk_stack_set_vhomogeneous :: 
    Ptr Stack ->                            -- stack : TInterface (Name {namespace = "Gtk", name = "Stack"})
    CInt ->                                 -- vhomogeneous : TBasicType TBoolean
    IO ()

-- | Sets the t'GI.Gtk.Objects.Stack.Stack' to be vertically homogeneous or not.
-- If it is homogeneous, the t'GI.Gtk.Objects.Stack.Stack' will request the same
-- height for all its children. If it isn\'t, the stack
-- may change height when a different child becomes visible.
-- 
-- /Since: 3.16/
stackSetVhomogeneous ::
    (B.CallStack.HasCallStack, MonadIO m, IsStack a) =>
    a
    -- ^ /@stack@/: a t'GI.Gtk.Objects.Stack.Stack'
    -> Bool
    -- ^ /@vhomogeneous@/: 'P.True' to make /@stack@/ vertically homogeneous
    -> m ()
stackSetVhomogeneous stack vhomogeneous = liftIO $ do
    stack' <- unsafeManagedPtrCastPtr stack
    let vhomogeneous' = (fromIntegral . fromEnum) vhomogeneous
    gtk_stack_set_vhomogeneous stack' vhomogeneous'
    touchManagedPtr stack
    return ()

#if defined(ENABLE_OVERLOADING)
data StackSetVhomogeneousMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsStack a) => O.OverloadedMethod StackSetVhomogeneousMethodInfo a signature where
    overloadedMethod = stackSetVhomogeneous

instance O.OverloadedMethodInfo StackSetVhomogeneousMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Stack.stackSetVhomogeneous",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Stack.html#v:stackSetVhomogeneous"
        })


#endif

-- method Stack::set_visible_child
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "stack"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Stack" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStack" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a child of @stack" , sinceVersion = Nothing }
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

foreign import ccall "gtk_stack_set_visible_child" gtk_stack_set_visible_child :: 
    Ptr Stack ->                            -- stack : TInterface (Name {namespace = "Gtk", name = "Stack"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | Makes /@child@/ the visible child of /@stack@/.
-- 
-- If /@child@/ is different from the currently
-- visible child, the transition between the
-- two will be animated with the current
-- transition type of /@stack@/.
-- 
-- Note that the /@child@/ widget has to be visible itself
-- (see 'GI.Gtk.Objects.Widget.widgetShow') in order to become the visible
-- child of /@stack@/.
-- 
-- /Since: 3.10/
stackSetVisibleChild ::
    (B.CallStack.HasCallStack, MonadIO m, IsStack a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@stack@/: a t'GI.Gtk.Objects.Stack.Stack'
    -> b
    -- ^ /@child@/: a child of /@stack@/
    -> m ()
stackSetVisibleChild stack child = liftIO $ do
    stack' <- unsafeManagedPtrCastPtr stack
    child' <- unsafeManagedPtrCastPtr child
    gtk_stack_set_visible_child stack' child'
    touchManagedPtr stack
    touchManagedPtr child
    return ()

#if defined(ENABLE_OVERLOADING)
data StackSetVisibleChildMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsStack a, Gtk.Widget.IsWidget b) => O.OverloadedMethod StackSetVisibleChildMethodInfo a signature where
    overloadedMethod = stackSetVisibleChild

instance O.OverloadedMethodInfo StackSetVisibleChildMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Stack.stackSetVisibleChild",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Stack.html#v:stackSetVisibleChild"
        })


#endif

-- method Stack::set_visible_child_full
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "stack"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Stack" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStack" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the name of the child to make visible"
--                 , sinceVersion = Nothing
--                 }
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
--                 Name { namespace = "Gtk" , name = "StackTransitionType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the transition type to use"
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

foreign import ccall "gtk_stack_set_visible_child_full" gtk_stack_set_visible_child_full :: 
    Ptr Stack ->                            -- stack : TInterface (Name {namespace = "Gtk", name = "Stack"})
    CString ->                              -- name : TBasicType TUTF8
    CUInt ->                                -- transition : TInterface (Name {namespace = "Gtk", name = "StackTransitionType"})
    IO ()

-- | Makes the child with the given name visible.
-- 
-- Note that the child widget has to be visible itself
-- (see 'GI.Gtk.Objects.Widget.widgetShow') in order to become the visible
-- child of /@stack@/.
-- 
-- /Since: 3.10/
stackSetVisibleChildFull ::
    (B.CallStack.HasCallStack, MonadIO m, IsStack a) =>
    a
    -- ^ /@stack@/: a t'GI.Gtk.Objects.Stack.Stack'
    -> T.Text
    -- ^ /@name@/: the name of the child to make visible
    -> Gtk.Enums.StackTransitionType
    -- ^ /@transition@/: the transition type to use
    -> m ()
stackSetVisibleChildFull stack name transition = liftIO $ do
    stack' <- unsafeManagedPtrCastPtr stack
    name' <- textToCString name
    let transition' = (fromIntegral . fromEnum) transition
    gtk_stack_set_visible_child_full stack' name' transition'
    touchManagedPtr stack
    freeMem name'
    return ()

#if defined(ENABLE_OVERLOADING)
data StackSetVisibleChildFullMethodInfo
instance (signature ~ (T.Text -> Gtk.Enums.StackTransitionType -> m ()), MonadIO m, IsStack a) => O.OverloadedMethod StackSetVisibleChildFullMethodInfo a signature where
    overloadedMethod = stackSetVisibleChildFull

instance O.OverloadedMethodInfo StackSetVisibleChildFullMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Stack.stackSetVisibleChildFull",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Stack.html#v:stackSetVisibleChildFull"
        })


#endif

-- method Stack::set_visible_child_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "stack"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Stack" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStack" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the name of the child to make visible"
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

foreign import ccall "gtk_stack_set_visible_child_name" gtk_stack_set_visible_child_name :: 
    Ptr Stack ->                            -- stack : TInterface (Name {namespace = "Gtk", name = "Stack"})
    CString ->                              -- name : TBasicType TUTF8
    IO ()

-- | Makes the child with the given name visible.
-- 
-- If /@child@/ is different from the currently
-- visible child, the transition between the
-- two will be animated with the current
-- transition type of /@stack@/.
-- 
-- Note that the child widget has to be visible itself
-- (see 'GI.Gtk.Objects.Widget.widgetShow') in order to become the visible
-- child of /@stack@/.
-- 
-- /Since: 3.10/
stackSetVisibleChildName ::
    (B.CallStack.HasCallStack, MonadIO m, IsStack a) =>
    a
    -- ^ /@stack@/: a t'GI.Gtk.Objects.Stack.Stack'
    -> T.Text
    -- ^ /@name@/: the name of the child to make visible
    -> m ()
stackSetVisibleChildName stack name = liftIO $ do
    stack' <- unsafeManagedPtrCastPtr stack
    name' <- textToCString name
    gtk_stack_set_visible_child_name stack' name'
    touchManagedPtr stack
    freeMem name'
    return ()

#if defined(ENABLE_OVERLOADING)
data StackSetVisibleChildNameMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsStack a) => O.OverloadedMethod StackSetVisibleChildNameMethodInfo a signature where
    overloadedMethod = stackSetVisibleChildName

instance O.OverloadedMethodInfo StackSetVisibleChildNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Stack.stackSetVisibleChildName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Stack.html#v:stackSetVisibleChildName"
        })


#endif


