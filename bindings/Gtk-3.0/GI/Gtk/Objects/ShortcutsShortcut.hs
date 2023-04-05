{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A GtkShortcutsShortcut represents a single keyboard shortcut or gesture
-- with a short text. This widget is only meant to be used with t'GI.Gtk.Objects.ShortcutsWindow.ShortcutsWindow'.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.ShortcutsShortcut
    ( 

-- * Exported types
    ShortcutsShortcut(..)                   ,
    IsShortcutsShortcut                     ,
    toShortcutsShortcut                     ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [packEnd]("GI.Gtk.Objects.Box#g:method:packEnd"), [packStart]("GI.Gtk.Objects.Box#g:method:packStart"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [queryChildPacking]("GI.Gtk.Objects.Box#g:method:queryChildPacking"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reorderChild]("GI.Gtk.Objects.Box#g:method:reorderChild"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getBaselinePosition]("GI.Gtk.Objects.Box#g:method:getBaselinePosition"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getCenterWidget]("GI.Gtk.Objects.Box#g:method:getCenterWidget"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getHomogeneous]("GI.Gtk.Objects.Box#g:method:getHomogeneous"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getOrientation]("GI.Gtk.Interfaces.Orientable#g:method:getOrientation"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getSpacing]("GI.Gtk.Objects.Box#g:method:getSpacing"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBaselinePosition]("GI.Gtk.Objects.Box#g:method:setBaselinePosition"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setCenterWidget]("GI.Gtk.Objects.Box#g:method:setCenterWidget"), [setChildPacking]("GI.Gtk.Objects.Box#g:method:setChildPacking"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setHomogeneous]("GI.Gtk.Objects.Box#g:method:setHomogeneous"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setOrientation]("GI.Gtk.Interfaces.Orientable#g:method:setOrientation"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setSpacing]("GI.Gtk.Objects.Box#g:method:setSpacing"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveShortcutsShortcutMethod          ,
#endif



 -- * Properties


-- ** accelSizeGroup #attr:accelSizeGroup#
-- | The size group for the accelerator portion of this shortcut.
-- 
-- This is used internally by GTK+, and must not be modified by applications.

#if defined(ENABLE_OVERLOADING)
    ShortcutsShortcutAccelSizeGroupPropertyInfo,
#endif
    clearShortcutsShortcutAccelSizeGroup    ,
    constructShortcutsShortcutAccelSizeGroup,
    setShortcutsShortcutAccelSizeGroup      ,
#if defined(ENABLE_OVERLOADING)
    shortcutsShortcutAccelSizeGroup         ,
#endif


-- ** accelerator #attr:accelerator#
-- | The accelerator(s) represented by this object. This property is used
-- if [ShortcutsShortcut:shortcutType]("GI.Gtk.Objects.ShortcutsShortcut#g:attr:shortcutType") is set to @/GTK_SHORTCUT_ACCELERATOR/@.
-- 
-- The syntax of this property is (an extension of) the syntax understood by
-- 'GI.Gtk.Functions.acceleratorParse'. Multiple accelerators can be specified by separating
-- them with a space, but keep in mind that the available width is limited.
-- It is also possible to specify ranges of shortcuts, using @...@ between the keys.
-- Sequences of keys can be specified using a @+@ or @&@ between the keys.
-- 
-- Examples:
-- 
-- * A single shortcut: @\<ctl>\<alt>delete@
-- * Two alternative shortcuts: @\<shift>a Home@
-- * A range of shortcuts: @\<alt>1...\<alt>9@
-- * Several keys pressed together: @Control_L&Control_R@
-- * A sequence of shortcuts or keys: @\<ctl>c+\<ctl>x@
-- 
-- 
-- Use + instead of & when the keys may (or have to be) pressed sequentially (e.g
-- use t+t for \'press the t key twice\').
-- 
-- Note that @\<@, @>@ and @&@ need to be escaped as &lt;, &gt; and &amp; when used
-- in .ui files.

#if defined(ENABLE_OVERLOADING)
    ShortcutsShortcutAcceleratorPropertyInfo,
#endif
    clearShortcutsShortcutAccelerator       ,
    constructShortcutsShortcutAccelerator   ,
    getShortcutsShortcutAccelerator         ,
    setShortcutsShortcutAccelerator         ,
#if defined(ENABLE_OVERLOADING)
    shortcutsShortcutAccelerator            ,
#endif


-- ** actionName #attr:actionName#
-- | A detailed action name. If this is set for a shortcut
-- of type 'GI.Gtk.Enums.ShortcutTypeAccelerator', then GTK+ will use
-- the accelerators that are associated with the action
-- via 'GI.Gtk.Objects.Application.applicationSetAccelsForAction', and setting
-- t'GI.Gtk.Objects.ShortcutsShortcut.ShortcutsShortcut'::@/accelerator/@ is not necessary.
-- 
-- /Since: 3.22/

#if defined(ENABLE_OVERLOADING)
    ShortcutsShortcutActionNamePropertyInfo ,
#endif
    clearShortcutsShortcutActionName        ,
    constructShortcutsShortcutActionName    ,
    getShortcutsShortcutActionName          ,
    setShortcutsShortcutActionName          ,
#if defined(ENABLE_OVERLOADING)
    shortcutsShortcutActionName             ,
#endif


-- ** direction #attr:direction#
-- | The text direction for which this shortcut is active. If the shortcut
-- is used regardless of the text direction, set this property to
-- @/GTK_TEXT_DIR_NONE/@.

#if defined(ENABLE_OVERLOADING)
    ShortcutsShortcutDirectionPropertyInfo  ,
#endif
    constructShortcutsShortcutDirection     ,
    getShortcutsShortcutDirection           ,
    setShortcutsShortcutDirection           ,
#if defined(ENABLE_OVERLOADING)
    shortcutsShortcutDirection              ,
#endif


-- ** icon #attr:icon#
-- | An icon to represent the shortcut or gesture. This property is used if
-- [ShortcutsShortcut:shortcutType]("GI.Gtk.Objects.ShortcutsShortcut#g:attr:shortcutType") is set to @/GTK_SHORTCUT_GESTURE/@.
-- For the other predefined gesture types, GTK+ provides an icon on its own.

#if defined(ENABLE_OVERLOADING)
    ShortcutsShortcutIconPropertyInfo       ,
#endif
    clearShortcutsShortcutIcon              ,
    constructShortcutsShortcutIcon          ,
    getShortcutsShortcutIcon                ,
    setShortcutsShortcutIcon                ,
#if defined(ENABLE_OVERLOADING)
    shortcutsShortcutIcon                   ,
#endif


-- ** iconSet #attr:iconSet#
-- | 'P.True' if an icon has been set.

#if defined(ENABLE_OVERLOADING)
    ShortcutsShortcutIconSetPropertyInfo    ,
#endif
    constructShortcutsShortcutIconSet       ,
    getShortcutsShortcutIconSet             ,
    setShortcutsShortcutIconSet             ,
#if defined(ENABLE_OVERLOADING)
    shortcutsShortcutIconSet                ,
#endif


-- ** shortcutType #attr:shortcutType#
-- | The type of shortcut that is represented.

#if defined(ENABLE_OVERLOADING)
    ShortcutsShortcutShortcutTypePropertyInfo,
#endif
    constructShortcutsShortcutShortcutType  ,
    getShortcutsShortcutShortcutType        ,
    setShortcutsShortcutShortcutType        ,
#if defined(ENABLE_OVERLOADING)
    shortcutsShortcutShortcutType           ,
#endif


-- ** subtitle #attr:subtitle#
-- | The subtitle for the shortcut or gesture.
-- 
-- This is typically used for gestures and should be a short, one-line
-- text that describes the gesture itself. For the predefined gesture
-- types, GTK+ provides a subtitle on its own.

#if defined(ENABLE_OVERLOADING)
    ShortcutsShortcutSubtitlePropertyInfo   ,
#endif
    clearShortcutsShortcutSubtitle          ,
    constructShortcutsShortcutSubtitle      ,
    getShortcutsShortcutSubtitle            ,
    setShortcutsShortcutSubtitle            ,
#if defined(ENABLE_OVERLOADING)
    shortcutsShortcutSubtitle               ,
#endif


-- ** subtitleSet #attr:subtitleSet#
-- | 'P.True' if a subtitle has been set.

#if defined(ENABLE_OVERLOADING)
    ShortcutsShortcutSubtitleSetPropertyInfo,
#endif
    constructShortcutsShortcutSubtitleSet   ,
    getShortcutsShortcutSubtitleSet         ,
    setShortcutsShortcutSubtitleSet         ,
#if defined(ENABLE_OVERLOADING)
    shortcutsShortcutSubtitleSet            ,
#endif


-- ** title #attr:title#
-- | The textual description for the shortcut or gesture represented by
-- this object. This should be a short string that can fit in a single line.

#if defined(ENABLE_OVERLOADING)
    ShortcutsShortcutTitlePropertyInfo      ,
#endif
    clearShortcutsShortcutTitle             ,
    constructShortcutsShortcutTitle         ,
    getShortcutsShortcutTitle               ,
    setShortcutsShortcutTitle               ,
#if defined(ENABLE_OVERLOADING)
    shortcutsShortcutTitle                  ,
#endif


-- ** titleSizeGroup #attr:titleSizeGroup#
-- | The size group for the textual portion of this shortcut.
-- 
-- This is used internally by GTK+, and must not be modified by applications.

#if defined(ENABLE_OVERLOADING)
    ShortcutsShortcutTitleSizeGroupPropertyInfo,
#endif
    clearShortcutsShortcutTitleSizeGroup    ,
    constructShortcutsShortcutTitleSizeGroup,
    setShortcutsShortcutTitleSizeGroup      ,
#if defined(ENABLE_OVERLOADING)
    shortcutsShortcutTitleSizeGroup         ,
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
import qualified GI.Gio.Interfaces.Icon as Gio.Icon
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Orientable as Gtk.Orientable
import {-# SOURCE #-} qualified GI.Gtk.Objects.Box as Gtk.Box
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.SizeGroup as Gtk.SizeGroup
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype ShortcutsShortcut = ShortcutsShortcut (SP.ManagedPtr ShortcutsShortcut)
    deriving (Eq)

instance SP.ManagedPtrNewtype ShortcutsShortcut where
    toManagedPtr (ShortcutsShortcut p) = p

foreign import ccall "gtk_shortcuts_shortcut_get_type"
    c_gtk_shortcuts_shortcut_get_type :: IO B.Types.GType

instance B.Types.TypedObject ShortcutsShortcut where
    glibType = c_gtk_shortcuts_shortcut_get_type

instance B.Types.GObject ShortcutsShortcut

-- | Type class for types which can be safely cast to `ShortcutsShortcut`, for instance with `toShortcutsShortcut`.
class (SP.GObject o, O.IsDescendantOf ShortcutsShortcut o) => IsShortcutsShortcut o
instance (SP.GObject o, O.IsDescendantOf ShortcutsShortcut o) => IsShortcutsShortcut o

instance O.HasParentTypes ShortcutsShortcut
type instance O.ParentTypes ShortcutsShortcut = '[Gtk.Box.Box, Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable, Gtk.Orientable.Orientable]

-- | Cast to `ShortcutsShortcut`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toShortcutsShortcut :: (MIO.MonadIO m, IsShortcutsShortcut o) => o -> m ShortcutsShortcut
toShortcutsShortcut = MIO.liftIO . B.ManagedPtr.unsafeCastTo ShortcutsShortcut

-- | Convert 'ShortcutsShortcut' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe ShortcutsShortcut) where
    gvalueGType_ = c_gtk_shortcuts_shortcut_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr ShortcutsShortcut)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr ShortcutsShortcut)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject ShortcutsShortcut ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveShortcutsShortcutMethod (t :: Symbol) (o :: *) :: * where
    ResolveShortcutsShortcutMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveShortcutsShortcutMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveShortcutsShortcutMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveShortcutsShortcutMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveShortcutsShortcutMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveShortcutsShortcutMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveShortcutsShortcutMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveShortcutsShortcutMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveShortcutsShortcutMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveShortcutsShortcutMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveShortcutsShortcutMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveShortcutsShortcutMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveShortcutsShortcutMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveShortcutsShortcutMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveShortcutsShortcutMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveShortcutsShortcutMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveShortcutsShortcutMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveShortcutsShortcutMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveShortcutsShortcutMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveShortcutsShortcutMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveShortcutsShortcutMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveShortcutsShortcutMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveShortcutsShortcutMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveShortcutsShortcutMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveShortcutsShortcutMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveShortcutsShortcutMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveShortcutsShortcutMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveShortcutsShortcutMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveShortcutsShortcutMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveShortcutsShortcutMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveShortcutsShortcutMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveShortcutsShortcutMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveShortcutsShortcutMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveShortcutsShortcutMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveShortcutsShortcutMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveShortcutsShortcutMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveShortcutsShortcutMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveShortcutsShortcutMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveShortcutsShortcutMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveShortcutsShortcutMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveShortcutsShortcutMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveShortcutsShortcutMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveShortcutsShortcutMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveShortcutsShortcutMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveShortcutsShortcutMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveShortcutsShortcutMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveShortcutsShortcutMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveShortcutsShortcutMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveShortcutsShortcutMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveShortcutsShortcutMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveShortcutsShortcutMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveShortcutsShortcutMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveShortcutsShortcutMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveShortcutsShortcutMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveShortcutsShortcutMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveShortcutsShortcutMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveShortcutsShortcutMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveShortcutsShortcutMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveShortcutsShortcutMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveShortcutsShortcutMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveShortcutsShortcutMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveShortcutsShortcutMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveShortcutsShortcutMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveShortcutsShortcutMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveShortcutsShortcutMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveShortcutsShortcutMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveShortcutsShortcutMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveShortcutsShortcutMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveShortcutsShortcutMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveShortcutsShortcutMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveShortcutsShortcutMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveShortcutsShortcutMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveShortcutsShortcutMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveShortcutsShortcutMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveShortcutsShortcutMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveShortcutsShortcutMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveShortcutsShortcutMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveShortcutsShortcutMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveShortcutsShortcutMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveShortcutsShortcutMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveShortcutsShortcutMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveShortcutsShortcutMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveShortcutsShortcutMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveShortcutsShortcutMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveShortcutsShortcutMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveShortcutsShortcutMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveShortcutsShortcutMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveShortcutsShortcutMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveShortcutsShortcutMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveShortcutsShortcutMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveShortcutsShortcutMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveShortcutsShortcutMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveShortcutsShortcutMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveShortcutsShortcutMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveShortcutsShortcutMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveShortcutsShortcutMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveShortcutsShortcutMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveShortcutsShortcutMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveShortcutsShortcutMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveShortcutsShortcutMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveShortcutsShortcutMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveShortcutsShortcutMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveShortcutsShortcutMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveShortcutsShortcutMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveShortcutsShortcutMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveShortcutsShortcutMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveShortcutsShortcutMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveShortcutsShortcutMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveShortcutsShortcutMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveShortcutsShortcutMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveShortcutsShortcutMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveShortcutsShortcutMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveShortcutsShortcutMethod "packEnd" o = Gtk.Box.BoxPackEndMethodInfo
    ResolveShortcutsShortcutMethod "packStart" o = Gtk.Box.BoxPackStartMethodInfo
    ResolveShortcutsShortcutMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveShortcutsShortcutMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveShortcutsShortcutMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveShortcutsShortcutMethod "queryChildPacking" o = Gtk.Box.BoxQueryChildPackingMethodInfo
    ResolveShortcutsShortcutMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveShortcutsShortcutMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveShortcutsShortcutMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveShortcutsShortcutMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveShortcutsShortcutMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveShortcutsShortcutMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveShortcutsShortcutMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveShortcutsShortcutMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveShortcutsShortcutMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveShortcutsShortcutMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveShortcutsShortcutMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveShortcutsShortcutMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveShortcutsShortcutMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolveShortcutsShortcutMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveShortcutsShortcutMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveShortcutsShortcutMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveShortcutsShortcutMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveShortcutsShortcutMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveShortcutsShortcutMethod "reorderChild" o = Gtk.Box.BoxReorderChildMethodInfo
    ResolveShortcutsShortcutMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveShortcutsShortcutMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveShortcutsShortcutMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveShortcutsShortcutMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveShortcutsShortcutMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveShortcutsShortcutMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveShortcutsShortcutMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveShortcutsShortcutMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveShortcutsShortcutMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveShortcutsShortcutMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveShortcutsShortcutMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveShortcutsShortcutMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveShortcutsShortcutMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveShortcutsShortcutMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveShortcutsShortcutMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveShortcutsShortcutMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveShortcutsShortcutMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveShortcutsShortcutMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveShortcutsShortcutMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveShortcutsShortcutMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveShortcutsShortcutMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveShortcutsShortcutMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveShortcutsShortcutMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveShortcutsShortcutMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveShortcutsShortcutMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveShortcutsShortcutMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveShortcutsShortcutMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveShortcutsShortcutMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveShortcutsShortcutMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveShortcutsShortcutMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveShortcutsShortcutMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveShortcutsShortcutMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveShortcutsShortcutMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveShortcutsShortcutMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveShortcutsShortcutMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveShortcutsShortcutMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveShortcutsShortcutMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveShortcutsShortcutMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveShortcutsShortcutMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveShortcutsShortcutMethod "getBaselinePosition" o = Gtk.Box.BoxGetBaselinePositionMethodInfo
    ResolveShortcutsShortcutMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveShortcutsShortcutMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveShortcutsShortcutMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveShortcutsShortcutMethod "getCenterWidget" o = Gtk.Box.BoxGetCenterWidgetMethodInfo
    ResolveShortcutsShortcutMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveShortcutsShortcutMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveShortcutsShortcutMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveShortcutsShortcutMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveShortcutsShortcutMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveShortcutsShortcutMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveShortcutsShortcutMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveShortcutsShortcutMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveShortcutsShortcutMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveShortcutsShortcutMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveShortcutsShortcutMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveShortcutsShortcutMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveShortcutsShortcutMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveShortcutsShortcutMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveShortcutsShortcutMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveShortcutsShortcutMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveShortcutsShortcutMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveShortcutsShortcutMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveShortcutsShortcutMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveShortcutsShortcutMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveShortcutsShortcutMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveShortcutsShortcutMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveShortcutsShortcutMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveShortcutsShortcutMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveShortcutsShortcutMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveShortcutsShortcutMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveShortcutsShortcutMethod "getHomogeneous" o = Gtk.Box.BoxGetHomogeneousMethodInfo
    ResolveShortcutsShortcutMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveShortcutsShortcutMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveShortcutsShortcutMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveShortcutsShortcutMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveShortcutsShortcutMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveShortcutsShortcutMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveShortcutsShortcutMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveShortcutsShortcutMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveShortcutsShortcutMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveShortcutsShortcutMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveShortcutsShortcutMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveShortcutsShortcutMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveShortcutsShortcutMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveShortcutsShortcutMethod "getOrientation" o = Gtk.Orientable.OrientableGetOrientationMethodInfo
    ResolveShortcutsShortcutMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveShortcutsShortcutMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveShortcutsShortcutMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveShortcutsShortcutMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveShortcutsShortcutMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveShortcutsShortcutMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveShortcutsShortcutMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveShortcutsShortcutMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveShortcutsShortcutMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveShortcutsShortcutMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveShortcutsShortcutMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveShortcutsShortcutMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveShortcutsShortcutMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveShortcutsShortcutMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveShortcutsShortcutMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveShortcutsShortcutMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveShortcutsShortcutMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveShortcutsShortcutMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveShortcutsShortcutMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveShortcutsShortcutMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveShortcutsShortcutMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveShortcutsShortcutMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveShortcutsShortcutMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveShortcutsShortcutMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveShortcutsShortcutMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveShortcutsShortcutMethod "getSpacing" o = Gtk.Box.BoxGetSpacingMethodInfo
    ResolveShortcutsShortcutMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveShortcutsShortcutMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveShortcutsShortcutMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveShortcutsShortcutMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveShortcutsShortcutMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveShortcutsShortcutMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveShortcutsShortcutMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveShortcutsShortcutMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveShortcutsShortcutMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveShortcutsShortcutMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveShortcutsShortcutMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveShortcutsShortcutMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveShortcutsShortcutMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveShortcutsShortcutMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveShortcutsShortcutMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveShortcutsShortcutMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveShortcutsShortcutMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveShortcutsShortcutMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveShortcutsShortcutMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveShortcutsShortcutMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveShortcutsShortcutMethod "setBaselinePosition" o = Gtk.Box.BoxSetBaselinePositionMethodInfo
    ResolveShortcutsShortcutMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveShortcutsShortcutMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveShortcutsShortcutMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveShortcutsShortcutMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveShortcutsShortcutMethod "setCenterWidget" o = Gtk.Box.BoxSetCenterWidgetMethodInfo
    ResolveShortcutsShortcutMethod "setChildPacking" o = Gtk.Box.BoxSetChildPackingMethodInfo
    ResolveShortcutsShortcutMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveShortcutsShortcutMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveShortcutsShortcutMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveShortcutsShortcutMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveShortcutsShortcutMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveShortcutsShortcutMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveShortcutsShortcutMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveShortcutsShortcutMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveShortcutsShortcutMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveShortcutsShortcutMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveShortcutsShortcutMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveShortcutsShortcutMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveShortcutsShortcutMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveShortcutsShortcutMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveShortcutsShortcutMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveShortcutsShortcutMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveShortcutsShortcutMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveShortcutsShortcutMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveShortcutsShortcutMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveShortcutsShortcutMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveShortcutsShortcutMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveShortcutsShortcutMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveShortcutsShortcutMethod "setHomogeneous" o = Gtk.Box.BoxSetHomogeneousMethodInfo
    ResolveShortcutsShortcutMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveShortcutsShortcutMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveShortcutsShortcutMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveShortcutsShortcutMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveShortcutsShortcutMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveShortcutsShortcutMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveShortcutsShortcutMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveShortcutsShortcutMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveShortcutsShortcutMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveShortcutsShortcutMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveShortcutsShortcutMethod "setOrientation" o = Gtk.Orientable.OrientableSetOrientationMethodInfo
    ResolveShortcutsShortcutMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveShortcutsShortcutMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveShortcutsShortcutMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveShortcutsShortcutMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveShortcutsShortcutMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveShortcutsShortcutMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveShortcutsShortcutMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveShortcutsShortcutMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveShortcutsShortcutMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveShortcutsShortcutMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveShortcutsShortcutMethod "setSpacing" o = Gtk.Box.BoxSetSpacingMethodInfo
    ResolveShortcutsShortcutMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveShortcutsShortcutMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveShortcutsShortcutMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveShortcutsShortcutMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveShortcutsShortcutMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveShortcutsShortcutMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveShortcutsShortcutMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveShortcutsShortcutMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveShortcutsShortcutMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveShortcutsShortcutMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveShortcutsShortcutMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveShortcutsShortcutMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveShortcutsShortcutMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveShortcutsShortcutMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveShortcutsShortcutMethod t ShortcutsShortcut, O.OverloadedMethod info ShortcutsShortcut p) => OL.IsLabel t (ShortcutsShortcut -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveShortcutsShortcutMethod t ShortcutsShortcut, O.OverloadedMethod info ShortcutsShortcut p, R.HasField t ShortcutsShortcut p) => R.HasField t ShortcutsShortcut p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveShortcutsShortcutMethod t ShortcutsShortcut, O.OverloadedMethodInfo info ShortcutsShortcut) => OL.IsLabel t (O.MethodProxy info ShortcutsShortcut) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- VVV Prop "accel-size-group"
   -- Type: TInterface (Name {namespace = "Gtk", name = "SizeGroup"})
   -- Flags: [PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Set the value of the “@accel-size-group@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' shortcutsShortcut [ #accelSizeGroup 'Data.GI.Base.Attributes.:=' value ]
-- @
setShortcutsShortcutAccelSizeGroup :: (MonadIO m, IsShortcutsShortcut o, Gtk.SizeGroup.IsSizeGroup a) => o -> a -> m ()
setShortcutsShortcutAccelSizeGroup obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "accel-size-group" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@accel-size-group@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructShortcutsShortcutAccelSizeGroup :: (IsShortcutsShortcut o, MIO.MonadIO m, Gtk.SizeGroup.IsSizeGroup a) => a -> m (GValueConstruct o)
constructShortcutsShortcutAccelSizeGroup val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "accel-size-group" (P.Just val)

-- | Set the value of the “@accel-size-group@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #accelSizeGroup
-- @
clearShortcutsShortcutAccelSizeGroup :: (MonadIO m, IsShortcutsShortcut o) => o -> m ()
clearShortcutsShortcutAccelSizeGroup obj = liftIO $ B.Properties.setObjectPropertyObject obj "accel-size-group" (Nothing :: Maybe Gtk.SizeGroup.SizeGroup)

#if defined(ENABLE_OVERLOADING)
data ShortcutsShortcutAccelSizeGroupPropertyInfo
instance AttrInfo ShortcutsShortcutAccelSizeGroupPropertyInfo where
    type AttrAllowedOps ShortcutsShortcutAccelSizeGroupPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrClear]
    type AttrBaseTypeConstraint ShortcutsShortcutAccelSizeGroupPropertyInfo = IsShortcutsShortcut
    type AttrSetTypeConstraint ShortcutsShortcutAccelSizeGroupPropertyInfo = Gtk.SizeGroup.IsSizeGroup
    type AttrTransferTypeConstraint ShortcutsShortcutAccelSizeGroupPropertyInfo = Gtk.SizeGroup.IsSizeGroup
    type AttrTransferType ShortcutsShortcutAccelSizeGroupPropertyInfo = Gtk.SizeGroup.SizeGroup
    type AttrGetType ShortcutsShortcutAccelSizeGroupPropertyInfo = ()
    type AttrLabel ShortcutsShortcutAccelSizeGroupPropertyInfo = "accel-size-group"
    type AttrOrigin ShortcutsShortcutAccelSizeGroupPropertyInfo = ShortcutsShortcut
    attrGet = undefined
    attrSet = setShortcutsShortcutAccelSizeGroup
    attrTransfer _ v = do
        unsafeCastTo Gtk.SizeGroup.SizeGroup v
    attrConstruct = constructShortcutsShortcutAccelSizeGroup
    attrClear = clearShortcutsShortcutAccelSizeGroup
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ShortcutsShortcut.accelSizeGroup"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ShortcutsShortcut.html#g:attr:accelSizeGroup"
        })
#endif

-- VVV Prop "accelerator"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@accelerator@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' shortcutsShortcut #accelerator
-- @
getShortcutsShortcutAccelerator :: (MonadIO m, IsShortcutsShortcut o) => o -> m (Maybe T.Text)
getShortcutsShortcutAccelerator obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "accelerator"

-- | Set the value of the “@accelerator@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' shortcutsShortcut [ #accelerator 'Data.GI.Base.Attributes.:=' value ]
-- @
setShortcutsShortcutAccelerator :: (MonadIO m, IsShortcutsShortcut o) => o -> T.Text -> m ()
setShortcutsShortcutAccelerator obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "accelerator" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@accelerator@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructShortcutsShortcutAccelerator :: (IsShortcutsShortcut o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructShortcutsShortcutAccelerator val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "accelerator" (P.Just val)

-- | Set the value of the “@accelerator@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #accelerator
-- @
clearShortcutsShortcutAccelerator :: (MonadIO m, IsShortcutsShortcut o) => o -> m ()
clearShortcutsShortcutAccelerator obj = liftIO $ B.Properties.setObjectPropertyString obj "accelerator" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data ShortcutsShortcutAcceleratorPropertyInfo
instance AttrInfo ShortcutsShortcutAcceleratorPropertyInfo where
    type AttrAllowedOps ShortcutsShortcutAcceleratorPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint ShortcutsShortcutAcceleratorPropertyInfo = IsShortcutsShortcut
    type AttrSetTypeConstraint ShortcutsShortcutAcceleratorPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint ShortcutsShortcutAcceleratorPropertyInfo = (~) T.Text
    type AttrTransferType ShortcutsShortcutAcceleratorPropertyInfo = T.Text
    type AttrGetType ShortcutsShortcutAcceleratorPropertyInfo = (Maybe T.Text)
    type AttrLabel ShortcutsShortcutAcceleratorPropertyInfo = "accelerator"
    type AttrOrigin ShortcutsShortcutAcceleratorPropertyInfo = ShortcutsShortcut
    attrGet = getShortcutsShortcutAccelerator
    attrSet = setShortcutsShortcutAccelerator
    attrTransfer _ v = do
        return v
    attrConstruct = constructShortcutsShortcutAccelerator
    attrClear = clearShortcutsShortcutAccelerator
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ShortcutsShortcut.accelerator"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ShortcutsShortcut.html#g:attr:accelerator"
        })
#endif

-- VVV Prop "action-name"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@action-name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' shortcutsShortcut #actionName
-- @
getShortcutsShortcutActionName :: (MonadIO m, IsShortcutsShortcut o) => o -> m (Maybe T.Text)
getShortcutsShortcutActionName obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "action-name"

-- | Set the value of the “@action-name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' shortcutsShortcut [ #actionName 'Data.GI.Base.Attributes.:=' value ]
-- @
setShortcutsShortcutActionName :: (MonadIO m, IsShortcutsShortcut o) => o -> T.Text -> m ()
setShortcutsShortcutActionName obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "action-name" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@action-name@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructShortcutsShortcutActionName :: (IsShortcutsShortcut o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructShortcutsShortcutActionName val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "action-name" (P.Just val)

-- | Set the value of the “@action-name@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #actionName
-- @
clearShortcutsShortcutActionName :: (MonadIO m, IsShortcutsShortcut o) => o -> m ()
clearShortcutsShortcutActionName obj = liftIO $ B.Properties.setObjectPropertyString obj "action-name" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data ShortcutsShortcutActionNamePropertyInfo
instance AttrInfo ShortcutsShortcutActionNamePropertyInfo where
    type AttrAllowedOps ShortcutsShortcutActionNamePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint ShortcutsShortcutActionNamePropertyInfo = IsShortcutsShortcut
    type AttrSetTypeConstraint ShortcutsShortcutActionNamePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint ShortcutsShortcutActionNamePropertyInfo = (~) T.Text
    type AttrTransferType ShortcutsShortcutActionNamePropertyInfo = T.Text
    type AttrGetType ShortcutsShortcutActionNamePropertyInfo = (Maybe T.Text)
    type AttrLabel ShortcutsShortcutActionNamePropertyInfo = "action-name"
    type AttrOrigin ShortcutsShortcutActionNamePropertyInfo = ShortcutsShortcut
    attrGet = getShortcutsShortcutActionName
    attrSet = setShortcutsShortcutActionName
    attrTransfer _ v = do
        return v
    attrConstruct = constructShortcutsShortcutActionName
    attrClear = clearShortcutsShortcutActionName
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ShortcutsShortcut.actionName"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ShortcutsShortcut.html#g:attr:actionName"
        })
#endif

-- VVV Prop "direction"
   -- Type: TInterface (Name {namespace = "Gtk", name = "TextDirection"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@direction@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' shortcutsShortcut #direction
-- @
getShortcutsShortcutDirection :: (MonadIO m, IsShortcutsShortcut o) => o -> m Gtk.Enums.TextDirection
getShortcutsShortcutDirection obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "direction"

-- | Set the value of the “@direction@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' shortcutsShortcut [ #direction 'Data.GI.Base.Attributes.:=' value ]
-- @
setShortcutsShortcutDirection :: (MonadIO m, IsShortcutsShortcut o) => o -> Gtk.Enums.TextDirection -> m ()
setShortcutsShortcutDirection obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "direction" val

-- | Construct a `GValueConstruct` with valid value for the “@direction@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructShortcutsShortcutDirection :: (IsShortcutsShortcut o, MIO.MonadIO m) => Gtk.Enums.TextDirection -> m (GValueConstruct o)
constructShortcutsShortcutDirection val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "direction" val

#if defined(ENABLE_OVERLOADING)
data ShortcutsShortcutDirectionPropertyInfo
instance AttrInfo ShortcutsShortcutDirectionPropertyInfo where
    type AttrAllowedOps ShortcutsShortcutDirectionPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ShortcutsShortcutDirectionPropertyInfo = IsShortcutsShortcut
    type AttrSetTypeConstraint ShortcutsShortcutDirectionPropertyInfo = (~) Gtk.Enums.TextDirection
    type AttrTransferTypeConstraint ShortcutsShortcutDirectionPropertyInfo = (~) Gtk.Enums.TextDirection
    type AttrTransferType ShortcutsShortcutDirectionPropertyInfo = Gtk.Enums.TextDirection
    type AttrGetType ShortcutsShortcutDirectionPropertyInfo = Gtk.Enums.TextDirection
    type AttrLabel ShortcutsShortcutDirectionPropertyInfo = "direction"
    type AttrOrigin ShortcutsShortcutDirectionPropertyInfo = ShortcutsShortcut
    attrGet = getShortcutsShortcutDirection
    attrSet = setShortcutsShortcutDirection
    attrTransfer _ v = do
        return v
    attrConstruct = constructShortcutsShortcutDirection
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ShortcutsShortcut.direction"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ShortcutsShortcut.html#g:attr:direction"
        })
#endif

-- VVV Prop "icon"
   -- Type: TInterface (Name {namespace = "Gio", name = "Icon"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@icon@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' shortcutsShortcut #icon
-- @
getShortcutsShortcutIcon :: (MonadIO m, IsShortcutsShortcut o) => o -> m (Maybe Gio.Icon.Icon)
getShortcutsShortcutIcon obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "icon" Gio.Icon.Icon

-- | Set the value of the “@icon@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' shortcutsShortcut [ #icon 'Data.GI.Base.Attributes.:=' value ]
-- @
setShortcutsShortcutIcon :: (MonadIO m, IsShortcutsShortcut o, Gio.Icon.IsIcon a) => o -> a -> m ()
setShortcutsShortcutIcon obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "icon" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@icon@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructShortcutsShortcutIcon :: (IsShortcutsShortcut o, MIO.MonadIO m, Gio.Icon.IsIcon a) => a -> m (GValueConstruct o)
constructShortcutsShortcutIcon val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "icon" (P.Just val)

-- | Set the value of the “@icon@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #icon
-- @
clearShortcutsShortcutIcon :: (MonadIO m, IsShortcutsShortcut o) => o -> m ()
clearShortcutsShortcutIcon obj = liftIO $ B.Properties.setObjectPropertyObject obj "icon" (Nothing :: Maybe Gio.Icon.Icon)

#if defined(ENABLE_OVERLOADING)
data ShortcutsShortcutIconPropertyInfo
instance AttrInfo ShortcutsShortcutIconPropertyInfo where
    type AttrAllowedOps ShortcutsShortcutIconPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint ShortcutsShortcutIconPropertyInfo = IsShortcutsShortcut
    type AttrSetTypeConstraint ShortcutsShortcutIconPropertyInfo = Gio.Icon.IsIcon
    type AttrTransferTypeConstraint ShortcutsShortcutIconPropertyInfo = Gio.Icon.IsIcon
    type AttrTransferType ShortcutsShortcutIconPropertyInfo = Gio.Icon.Icon
    type AttrGetType ShortcutsShortcutIconPropertyInfo = (Maybe Gio.Icon.Icon)
    type AttrLabel ShortcutsShortcutIconPropertyInfo = "icon"
    type AttrOrigin ShortcutsShortcutIconPropertyInfo = ShortcutsShortcut
    attrGet = getShortcutsShortcutIcon
    attrSet = setShortcutsShortcutIcon
    attrTransfer _ v = do
        unsafeCastTo Gio.Icon.Icon v
    attrConstruct = constructShortcutsShortcutIcon
    attrClear = clearShortcutsShortcutIcon
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ShortcutsShortcut.icon"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ShortcutsShortcut.html#g:attr:icon"
        })
#endif

-- VVV Prop "icon-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@icon-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' shortcutsShortcut #iconSet
-- @
getShortcutsShortcutIconSet :: (MonadIO m, IsShortcutsShortcut o) => o -> m Bool
getShortcutsShortcutIconSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "icon-set"

-- | Set the value of the “@icon-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' shortcutsShortcut [ #iconSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setShortcutsShortcutIconSet :: (MonadIO m, IsShortcutsShortcut o) => o -> Bool -> m ()
setShortcutsShortcutIconSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "icon-set" val

-- | Construct a `GValueConstruct` with valid value for the “@icon-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructShortcutsShortcutIconSet :: (IsShortcutsShortcut o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructShortcutsShortcutIconSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "icon-set" val

#if defined(ENABLE_OVERLOADING)
data ShortcutsShortcutIconSetPropertyInfo
instance AttrInfo ShortcutsShortcutIconSetPropertyInfo where
    type AttrAllowedOps ShortcutsShortcutIconSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ShortcutsShortcutIconSetPropertyInfo = IsShortcutsShortcut
    type AttrSetTypeConstraint ShortcutsShortcutIconSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint ShortcutsShortcutIconSetPropertyInfo = (~) Bool
    type AttrTransferType ShortcutsShortcutIconSetPropertyInfo = Bool
    type AttrGetType ShortcutsShortcutIconSetPropertyInfo = Bool
    type AttrLabel ShortcutsShortcutIconSetPropertyInfo = "icon-set"
    type AttrOrigin ShortcutsShortcutIconSetPropertyInfo = ShortcutsShortcut
    attrGet = getShortcutsShortcutIconSet
    attrSet = setShortcutsShortcutIconSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructShortcutsShortcutIconSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ShortcutsShortcut.iconSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ShortcutsShortcut.html#g:attr:iconSet"
        })
#endif

-- VVV Prop "shortcut-type"
   -- Type: TInterface (Name {namespace = "Gtk", name = "ShortcutType"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@shortcut-type@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' shortcutsShortcut #shortcutType
-- @
getShortcutsShortcutShortcutType :: (MonadIO m, IsShortcutsShortcut o) => o -> m Gtk.Enums.ShortcutType
getShortcutsShortcutShortcutType obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "shortcut-type"

-- | Set the value of the “@shortcut-type@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' shortcutsShortcut [ #shortcutType 'Data.GI.Base.Attributes.:=' value ]
-- @
setShortcutsShortcutShortcutType :: (MonadIO m, IsShortcutsShortcut o) => o -> Gtk.Enums.ShortcutType -> m ()
setShortcutsShortcutShortcutType obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "shortcut-type" val

-- | Construct a `GValueConstruct` with valid value for the “@shortcut-type@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructShortcutsShortcutShortcutType :: (IsShortcutsShortcut o, MIO.MonadIO m) => Gtk.Enums.ShortcutType -> m (GValueConstruct o)
constructShortcutsShortcutShortcutType val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "shortcut-type" val

#if defined(ENABLE_OVERLOADING)
data ShortcutsShortcutShortcutTypePropertyInfo
instance AttrInfo ShortcutsShortcutShortcutTypePropertyInfo where
    type AttrAllowedOps ShortcutsShortcutShortcutTypePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ShortcutsShortcutShortcutTypePropertyInfo = IsShortcutsShortcut
    type AttrSetTypeConstraint ShortcutsShortcutShortcutTypePropertyInfo = (~) Gtk.Enums.ShortcutType
    type AttrTransferTypeConstraint ShortcutsShortcutShortcutTypePropertyInfo = (~) Gtk.Enums.ShortcutType
    type AttrTransferType ShortcutsShortcutShortcutTypePropertyInfo = Gtk.Enums.ShortcutType
    type AttrGetType ShortcutsShortcutShortcutTypePropertyInfo = Gtk.Enums.ShortcutType
    type AttrLabel ShortcutsShortcutShortcutTypePropertyInfo = "shortcut-type"
    type AttrOrigin ShortcutsShortcutShortcutTypePropertyInfo = ShortcutsShortcut
    attrGet = getShortcutsShortcutShortcutType
    attrSet = setShortcutsShortcutShortcutType
    attrTransfer _ v = do
        return v
    attrConstruct = constructShortcutsShortcutShortcutType
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ShortcutsShortcut.shortcutType"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ShortcutsShortcut.html#g:attr:shortcutType"
        })
#endif

-- VVV Prop "subtitle"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@subtitle@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' shortcutsShortcut #subtitle
-- @
getShortcutsShortcutSubtitle :: (MonadIO m, IsShortcutsShortcut o) => o -> m (Maybe T.Text)
getShortcutsShortcutSubtitle obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "subtitle"

-- | Set the value of the “@subtitle@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' shortcutsShortcut [ #subtitle 'Data.GI.Base.Attributes.:=' value ]
-- @
setShortcutsShortcutSubtitle :: (MonadIO m, IsShortcutsShortcut o) => o -> T.Text -> m ()
setShortcutsShortcutSubtitle obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "subtitle" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@subtitle@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructShortcutsShortcutSubtitle :: (IsShortcutsShortcut o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructShortcutsShortcutSubtitle val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "subtitle" (P.Just val)

-- | Set the value of the “@subtitle@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #subtitle
-- @
clearShortcutsShortcutSubtitle :: (MonadIO m, IsShortcutsShortcut o) => o -> m ()
clearShortcutsShortcutSubtitle obj = liftIO $ B.Properties.setObjectPropertyString obj "subtitle" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data ShortcutsShortcutSubtitlePropertyInfo
instance AttrInfo ShortcutsShortcutSubtitlePropertyInfo where
    type AttrAllowedOps ShortcutsShortcutSubtitlePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint ShortcutsShortcutSubtitlePropertyInfo = IsShortcutsShortcut
    type AttrSetTypeConstraint ShortcutsShortcutSubtitlePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint ShortcutsShortcutSubtitlePropertyInfo = (~) T.Text
    type AttrTransferType ShortcutsShortcutSubtitlePropertyInfo = T.Text
    type AttrGetType ShortcutsShortcutSubtitlePropertyInfo = (Maybe T.Text)
    type AttrLabel ShortcutsShortcutSubtitlePropertyInfo = "subtitle"
    type AttrOrigin ShortcutsShortcutSubtitlePropertyInfo = ShortcutsShortcut
    attrGet = getShortcutsShortcutSubtitle
    attrSet = setShortcutsShortcutSubtitle
    attrTransfer _ v = do
        return v
    attrConstruct = constructShortcutsShortcutSubtitle
    attrClear = clearShortcutsShortcutSubtitle
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ShortcutsShortcut.subtitle"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ShortcutsShortcut.html#g:attr:subtitle"
        })
#endif

-- VVV Prop "subtitle-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@subtitle-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' shortcutsShortcut #subtitleSet
-- @
getShortcutsShortcutSubtitleSet :: (MonadIO m, IsShortcutsShortcut o) => o -> m Bool
getShortcutsShortcutSubtitleSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "subtitle-set"

-- | Set the value of the “@subtitle-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' shortcutsShortcut [ #subtitleSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setShortcutsShortcutSubtitleSet :: (MonadIO m, IsShortcutsShortcut o) => o -> Bool -> m ()
setShortcutsShortcutSubtitleSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "subtitle-set" val

-- | Construct a `GValueConstruct` with valid value for the “@subtitle-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructShortcutsShortcutSubtitleSet :: (IsShortcutsShortcut o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructShortcutsShortcutSubtitleSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "subtitle-set" val

#if defined(ENABLE_OVERLOADING)
data ShortcutsShortcutSubtitleSetPropertyInfo
instance AttrInfo ShortcutsShortcutSubtitleSetPropertyInfo where
    type AttrAllowedOps ShortcutsShortcutSubtitleSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ShortcutsShortcutSubtitleSetPropertyInfo = IsShortcutsShortcut
    type AttrSetTypeConstraint ShortcutsShortcutSubtitleSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint ShortcutsShortcutSubtitleSetPropertyInfo = (~) Bool
    type AttrTransferType ShortcutsShortcutSubtitleSetPropertyInfo = Bool
    type AttrGetType ShortcutsShortcutSubtitleSetPropertyInfo = Bool
    type AttrLabel ShortcutsShortcutSubtitleSetPropertyInfo = "subtitle-set"
    type AttrOrigin ShortcutsShortcutSubtitleSetPropertyInfo = ShortcutsShortcut
    attrGet = getShortcutsShortcutSubtitleSet
    attrSet = setShortcutsShortcutSubtitleSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructShortcutsShortcutSubtitleSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ShortcutsShortcut.subtitleSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ShortcutsShortcut.html#g:attr:subtitleSet"
        })
#endif

-- VVV Prop "title"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@title@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' shortcutsShortcut #title
-- @
getShortcutsShortcutTitle :: (MonadIO m, IsShortcutsShortcut o) => o -> m (Maybe T.Text)
getShortcutsShortcutTitle obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "title"

-- | Set the value of the “@title@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' shortcutsShortcut [ #title 'Data.GI.Base.Attributes.:=' value ]
-- @
setShortcutsShortcutTitle :: (MonadIO m, IsShortcutsShortcut o) => o -> T.Text -> m ()
setShortcutsShortcutTitle obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "title" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@title@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructShortcutsShortcutTitle :: (IsShortcutsShortcut o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructShortcutsShortcutTitle val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "title" (P.Just val)

-- | Set the value of the “@title@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #title
-- @
clearShortcutsShortcutTitle :: (MonadIO m, IsShortcutsShortcut o) => o -> m ()
clearShortcutsShortcutTitle obj = liftIO $ B.Properties.setObjectPropertyString obj "title" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data ShortcutsShortcutTitlePropertyInfo
instance AttrInfo ShortcutsShortcutTitlePropertyInfo where
    type AttrAllowedOps ShortcutsShortcutTitlePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint ShortcutsShortcutTitlePropertyInfo = IsShortcutsShortcut
    type AttrSetTypeConstraint ShortcutsShortcutTitlePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint ShortcutsShortcutTitlePropertyInfo = (~) T.Text
    type AttrTransferType ShortcutsShortcutTitlePropertyInfo = T.Text
    type AttrGetType ShortcutsShortcutTitlePropertyInfo = (Maybe T.Text)
    type AttrLabel ShortcutsShortcutTitlePropertyInfo = "title"
    type AttrOrigin ShortcutsShortcutTitlePropertyInfo = ShortcutsShortcut
    attrGet = getShortcutsShortcutTitle
    attrSet = setShortcutsShortcutTitle
    attrTransfer _ v = do
        return v
    attrConstruct = constructShortcutsShortcutTitle
    attrClear = clearShortcutsShortcutTitle
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ShortcutsShortcut.title"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ShortcutsShortcut.html#g:attr:title"
        })
#endif

-- VVV Prop "title-size-group"
   -- Type: TInterface (Name {namespace = "Gtk", name = "SizeGroup"})
   -- Flags: [PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Set the value of the “@title-size-group@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' shortcutsShortcut [ #titleSizeGroup 'Data.GI.Base.Attributes.:=' value ]
-- @
setShortcutsShortcutTitleSizeGroup :: (MonadIO m, IsShortcutsShortcut o, Gtk.SizeGroup.IsSizeGroup a) => o -> a -> m ()
setShortcutsShortcutTitleSizeGroup obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "title-size-group" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@title-size-group@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructShortcutsShortcutTitleSizeGroup :: (IsShortcutsShortcut o, MIO.MonadIO m, Gtk.SizeGroup.IsSizeGroup a) => a -> m (GValueConstruct o)
constructShortcutsShortcutTitleSizeGroup val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "title-size-group" (P.Just val)

-- | Set the value of the “@title-size-group@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #titleSizeGroup
-- @
clearShortcutsShortcutTitleSizeGroup :: (MonadIO m, IsShortcutsShortcut o) => o -> m ()
clearShortcutsShortcutTitleSizeGroup obj = liftIO $ B.Properties.setObjectPropertyObject obj "title-size-group" (Nothing :: Maybe Gtk.SizeGroup.SizeGroup)

#if defined(ENABLE_OVERLOADING)
data ShortcutsShortcutTitleSizeGroupPropertyInfo
instance AttrInfo ShortcutsShortcutTitleSizeGroupPropertyInfo where
    type AttrAllowedOps ShortcutsShortcutTitleSizeGroupPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrClear]
    type AttrBaseTypeConstraint ShortcutsShortcutTitleSizeGroupPropertyInfo = IsShortcutsShortcut
    type AttrSetTypeConstraint ShortcutsShortcutTitleSizeGroupPropertyInfo = Gtk.SizeGroup.IsSizeGroup
    type AttrTransferTypeConstraint ShortcutsShortcutTitleSizeGroupPropertyInfo = Gtk.SizeGroup.IsSizeGroup
    type AttrTransferType ShortcutsShortcutTitleSizeGroupPropertyInfo = Gtk.SizeGroup.SizeGroup
    type AttrGetType ShortcutsShortcutTitleSizeGroupPropertyInfo = ()
    type AttrLabel ShortcutsShortcutTitleSizeGroupPropertyInfo = "title-size-group"
    type AttrOrigin ShortcutsShortcutTitleSizeGroupPropertyInfo = ShortcutsShortcut
    attrGet = undefined
    attrSet = setShortcutsShortcutTitleSizeGroup
    attrTransfer _ v = do
        unsafeCastTo Gtk.SizeGroup.SizeGroup v
    attrConstruct = constructShortcutsShortcutTitleSizeGroup
    attrClear = clearShortcutsShortcutTitleSizeGroup
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ShortcutsShortcut.titleSizeGroup"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ShortcutsShortcut.html#g:attr:titleSizeGroup"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList ShortcutsShortcut
type instance O.AttributeList ShortcutsShortcut = ShortcutsShortcutAttributeList
type ShortcutsShortcutAttributeList = ('[ '("accelSizeGroup", ShortcutsShortcutAccelSizeGroupPropertyInfo), '("accelerator", ShortcutsShortcutAcceleratorPropertyInfo), '("actionName", ShortcutsShortcutActionNamePropertyInfo), '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("baselinePosition", Gtk.Box.BoxBaselinePositionPropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("direction", ShortcutsShortcutDirectionPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("homogeneous", Gtk.Box.BoxHomogeneousPropertyInfo), '("icon", ShortcutsShortcutIconPropertyInfo), '("iconSet", ShortcutsShortcutIconSetPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("orientation", Gtk.Orientable.OrientableOrientationPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("shortcutType", ShortcutsShortcutShortcutTypePropertyInfo), '("spacing", Gtk.Box.BoxSpacingPropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("subtitle", ShortcutsShortcutSubtitlePropertyInfo), '("subtitleSet", ShortcutsShortcutSubtitleSetPropertyInfo), '("title", ShortcutsShortcutTitlePropertyInfo), '("titleSizeGroup", ShortcutsShortcutTitleSizeGroupPropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
shortcutsShortcutAccelSizeGroup :: AttrLabelProxy "accelSizeGroup"
shortcutsShortcutAccelSizeGroup = AttrLabelProxy

shortcutsShortcutAccelerator :: AttrLabelProxy "accelerator"
shortcutsShortcutAccelerator = AttrLabelProxy

shortcutsShortcutActionName :: AttrLabelProxy "actionName"
shortcutsShortcutActionName = AttrLabelProxy

shortcutsShortcutDirection :: AttrLabelProxy "direction"
shortcutsShortcutDirection = AttrLabelProxy

shortcutsShortcutIcon :: AttrLabelProxy "icon"
shortcutsShortcutIcon = AttrLabelProxy

shortcutsShortcutIconSet :: AttrLabelProxy "iconSet"
shortcutsShortcutIconSet = AttrLabelProxy

shortcutsShortcutShortcutType :: AttrLabelProxy "shortcutType"
shortcutsShortcutShortcutType = AttrLabelProxy

shortcutsShortcutSubtitle :: AttrLabelProxy "subtitle"
shortcutsShortcutSubtitle = AttrLabelProxy

shortcutsShortcutSubtitleSet :: AttrLabelProxy "subtitleSet"
shortcutsShortcutSubtitleSet = AttrLabelProxy

shortcutsShortcutTitle :: AttrLabelProxy "title"
shortcutsShortcutTitle = AttrLabelProxy

shortcutsShortcutTitleSizeGroup :: AttrLabelProxy "titleSizeGroup"
shortcutsShortcutTitleSizeGroup = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList ShortcutsShortcut = ShortcutsShortcutSignalList
type ShortcutsShortcutSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif


