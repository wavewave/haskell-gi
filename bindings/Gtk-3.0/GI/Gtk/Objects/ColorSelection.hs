{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- /No description available in the introspection data./

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.ColorSelection
    ( 

-- * Exported types
    ColorSelection(..)                      ,
    IsColorSelection                        ,
    toColorSelection                        ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAdjusting]("GI.Gtk.Objects.ColorSelection#g:method:isAdjusting"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [packEnd]("GI.Gtk.Objects.Box#g:method:packEnd"), [packStart]("GI.Gtk.Objects.Box#g:method:packStart"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [queryChildPacking]("GI.Gtk.Objects.Box#g:method:queryChildPacking"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reorderChild]("GI.Gtk.Objects.Box#g:method:reorderChild"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getBaselinePosition]("GI.Gtk.Objects.Box#g:method:getBaselinePosition"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getCenterWidget]("GI.Gtk.Objects.Box#g:method:getCenterWidget"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getCurrentAlpha]("GI.Gtk.Objects.ColorSelection#g:method:getCurrentAlpha"), [getCurrentColor]("GI.Gtk.Objects.ColorSelection#g:method:getCurrentColor"), [getCurrentRgba]("GI.Gtk.Objects.ColorSelection#g:method:getCurrentRgba"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasOpacityControl]("GI.Gtk.Objects.ColorSelection#g:method:getHasOpacityControl"), [getHasPalette]("GI.Gtk.Objects.ColorSelection#g:method:getHasPalette"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getHomogeneous]("GI.Gtk.Objects.Box#g:method:getHomogeneous"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getOrientation]("GI.Gtk.Interfaces.Orientable#g:method:getOrientation"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getPreviousAlpha]("GI.Gtk.Objects.ColorSelection#g:method:getPreviousAlpha"), [getPreviousColor]("GI.Gtk.Objects.ColorSelection#g:method:getPreviousColor"), [getPreviousRgba]("GI.Gtk.Objects.ColorSelection#g:method:getPreviousRgba"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getSpacing]("GI.Gtk.Objects.Box#g:method:getSpacing"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBaselinePosition]("GI.Gtk.Objects.Box#g:method:setBaselinePosition"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setCenterWidget]("GI.Gtk.Objects.Box#g:method:setCenterWidget"), [setChildPacking]("GI.Gtk.Objects.Box#g:method:setChildPacking"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setCurrentAlpha]("GI.Gtk.Objects.ColorSelection#g:method:setCurrentAlpha"), [setCurrentColor]("GI.Gtk.Objects.ColorSelection#g:method:setCurrentColor"), [setCurrentRgba]("GI.Gtk.Objects.ColorSelection#g:method:setCurrentRgba"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasOpacityControl]("GI.Gtk.Objects.ColorSelection#g:method:setHasOpacityControl"), [setHasPalette]("GI.Gtk.Objects.ColorSelection#g:method:setHasPalette"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setHomogeneous]("GI.Gtk.Objects.Box#g:method:setHomogeneous"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setOrientation]("GI.Gtk.Interfaces.Orientable#g:method:setOrientation"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setPreviousAlpha]("GI.Gtk.Objects.ColorSelection#g:method:setPreviousAlpha"), [setPreviousColor]("GI.Gtk.Objects.ColorSelection#g:method:setPreviousColor"), [setPreviousRgba]("GI.Gtk.Objects.ColorSelection#g:method:setPreviousRgba"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setSpacing]("GI.Gtk.Objects.Box#g:method:setSpacing"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveColorSelectionMethod             ,
#endif

-- ** getCurrentAlpha #method:getCurrentAlpha#

#if defined(ENABLE_OVERLOADING)
    ColorSelectionGetCurrentAlphaMethodInfo ,
#endif
    colorSelectionGetCurrentAlpha           ,


-- ** getCurrentColor #method:getCurrentColor#

#if defined(ENABLE_OVERLOADING)
    ColorSelectionGetCurrentColorMethodInfo ,
#endif
    colorSelectionGetCurrentColor           ,


-- ** getCurrentRgba #method:getCurrentRgba#

#if defined(ENABLE_OVERLOADING)
    ColorSelectionGetCurrentRgbaMethodInfo  ,
#endif
    colorSelectionGetCurrentRgba            ,


-- ** getHasOpacityControl #method:getHasOpacityControl#

#if defined(ENABLE_OVERLOADING)
    ColorSelectionGetHasOpacityControlMethodInfo,
#endif
    colorSelectionGetHasOpacityControl      ,


-- ** getHasPalette #method:getHasPalette#

#if defined(ENABLE_OVERLOADING)
    ColorSelectionGetHasPaletteMethodInfo   ,
#endif
    colorSelectionGetHasPalette             ,


-- ** getPreviousAlpha #method:getPreviousAlpha#

#if defined(ENABLE_OVERLOADING)
    ColorSelectionGetPreviousAlphaMethodInfo,
#endif
    colorSelectionGetPreviousAlpha          ,


-- ** getPreviousColor #method:getPreviousColor#

#if defined(ENABLE_OVERLOADING)
    ColorSelectionGetPreviousColorMethodInfo,
#endif
    colorSelectionGetPreviousColor          ,


-- ** getPreviousRgba #method:getPreviousRgba#

#if defined(ENABLE_OVERLOADING)
    ColorSelectionGetPreviousRgbaMethodInfo ,
#endif
    colorSelectionGetPreviousRgba           ,


-- ** isAdjusting #method:isAdjusting#

#if defined(ENABLE_OVERLOADING)
    ColorSelectionIsAdjustingMethodInfo     ,
#endif
    colorSelectionIsAdjusting               ,


-- ** new #method:new#

    colorSelectionNew                       ,


-- ** paletteFromString #method:paletteFromString#

    colorSelectionPaletteFromString         ,


-- ** paletteToString #method:paletteToString#

    colorSelectionPaletteToString           ,


-- ** setCurrentAlpha #method:setCurrentAlpha#

#if defined(ENABLE_OVERLOADING)
    ColorSelectionSetCurrentAlphaMethodInfo ,
#endif
    colorSelectionSetCurrentAlpha           ,


-- ** setCurrentColor #method:setCurrentColor#

#if defined(ENABLE_OVERLOADING)
    ColorSelectionSetCurrentColorMethodInfo ,
#endif
    colorSelectionSetCurrentColor           ,


-- ** setCurrentRgba #method:setCurrentRgba#

#if defined(ENABLE_OVERLOADING)
    ColorSelectionSetCurrentRgbaMethodInfo  ,
#endif
    colorSelectionSetCurrentRgba            ,


-- ** setHasOpacityControl #method:setHasOpacityControl#

#if defined(ENABLE_OVERLOADING)
    ColorSelectionSetHasOpacityControlMethodInfo,
#endif
    colorSelectionSetHasOpacityControl      ,


-- ** setHasPalette #method:setHasPalette#

#if defined(ENABLE_OVERLOADING)
    ColorSelectionSetHasPaletteMethodInfo   ,
#endif
    colorSelectionSetHasPalette             ,


-- ** setPreviousAlpha #method:setPreviousAlpha#

#if defined(ENABLE_OVERLOADING)
    ColorSelectionSetPreviousAlphaMethodInfo,
#endif
    colorSelectionSetPreviousAlpha          ,


-- ** setPreviousColor #method:setPreviousColor#

#if defined(ENABLE_OVERLOADING)
    ColorSelectionSetPreviousColorMethodInfo,
#endif
    colorSelectionSetPreviousColor          ,


-- ** setPreviousRgba #method:setPreviousRgba#

#if defined(ENABLE_OVERLOADING)
    ColorSelectionSetPreviousRgbaMethodInfo ,
#endif
    colorSelectionSetPreviousRgba           ,




 -- * Properties


-- ** currentAlpha #attr:currentAlpha#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ColorSelectionCurrentAlphaPropertyInfo  ,
#endif
#if defined(ENABLE_OVERLOADING)
    colorSelectionCurrentAlpha              ,
#endif
    constructColorSelectionCurrentAlpha     ,
    getColorSelectionCurrentAlpha           ,
    setColorSelectionCurrentAlpha           ,


-- ** currentColor #attr:currentColor#
-- | The current GdkColor color.

#if defined(ENABLE_OVERLOADING)
    ColorSelectionCurrentColorPropertyInfo  ,
#endif
#if defined(ENABLE_OVERLOADING)
    colorSelectionCurrentColor              ,
#endif
    constructColorSelectionCurrentColor     ,
    getColorSelectionCurrentColor           ,
    setColorSelectionCurrentColor           ,


-- ** currentRgba #attr:currentRgba#
-- | The current RGBA color.
-- 
-- /Since: 3.0/

#if defined(ENABLE_OVERLOADING)
    ColorSelectionCurrentRgbaPropertyInfo   ,
#endif
#if defined(ENABLE_OVERLOADING)
    colorSelectionCurrentRgba               ,
#endif
    constructColorSelectionCurrentRgba      ,
    getColorSelectionCurrentRgba            ,
    setColorSelectionCurrentRgba            ,


-- ** hasOpacityControl #attr:hasOpacityControl#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ColorSelectionHasOpacityControlPropertyInfo,
#endif
#if defined(ENABLE_OVERLOADING)
    colorSelectionHasOpacityControl         ,
#endif
    constructColorSelectionHasOpacityControl,
    getColorSelectionHasOpacityControl      ,
    setColorSelectionHasOpacityControl      ,


-- ** hasPalette #attr:hasPalette#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ColorSelectionHasPalettePropertyInfo    ,
#endif
#if defined(ENABLE_OVERLOADING)
    colorSelectionHasPalette                ,
#endif
    constructColorSelectionHasPalette       ,
    getColorSelectionHasPalette             ,
    setColorSelectionHasPalette             ,




 -- * Signals


-- ** colorChanged #signal:colorChanged#

    ColorSelectionColorChangedCallback      ,
#if defined(ENABLE_OVERLOADING)
    ColorSelectionColorChangedSignalInfo    ,
#endif
    afterColorSelectionColorChanged         ,
    onColorSelectionColorChanged            ,




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
import qualified GI.Gdk.Structs.Color as Gdk.Color
import qualified GI.Gdk.Structs.RGBA as Gdk.RGBA
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Orientable as Gtk.Orientable
import {-# SOURCE #-} qualified GI.Gtk.Objects.Box as Gtk.Box
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype ColorSelection = ColorSelection (SP.ManagedPtr ColorSelection)
    deriving (Eq)

instance SP.ManagedPtrNewtype ColorSelection where
    toManagedPtr (ColorSelection p) = p

foreign import ccall "gtk_color_selection_get_type"
    c_gtk_color_selection_get_type :: IO B.Types.GType

instance B.Types.TypedObject ColorSelection where
    glibType = c_gtk_color_selection_get_type

instance B.Types.GObject ColorSelection

-- | Type class for types which can be safely cast to `ColorSelection`, for instance with `toColorSelection`.
class (SP.GObject o, O.IsDescendantOf ColorSelection o) => IsColorSelection o
instance (SP.GObject o, O.IsDescendantOf ColorSelection o) => IsColorSelection o

instance O.HasParentTypes ColorSelection
type instance O.ParentTypes ColorSelection = '[Gtk.Box.Box, Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable, Gtk.Orientable.Orientable]

-- | Cast to `ColorSelection`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toColorSelection :: (MIO.MonadIO m, IsColorSelection o) => o -> m ColorSelection
toColorSelection = MIO.liftIO . B.ManagedPtr.unsafeCastTo ColorSelection

-- | Convert 'ColorSelection' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe ColorSelection) where
    gvalueGType_ = c_gtk_color_selection_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr ColorSelection)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr ColorSelection)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject ColorSelection ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveColorSelectionMethod (t :: Symbol) (o :: *) :: * where
    ResolveColorSelectionMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveColorSelectionMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveColorSelectionMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveColorSelectionMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveColorSelectionMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveColorSelectionMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveColorSelectionMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveColorSelectionMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveColorSelectionMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveColorSelectionMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveColorSelectionMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveColorSelectionMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveColorSelectionMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveColorSelectionMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveColorSelectionMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveColorSelectionMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveColorSelectionMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveColorSelectionMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveColorSelectionMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveColorSelectionMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveColorSelectionMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveColorSelectionMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveColorSelectionMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveColorSelectionMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveColorSelectionMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveColorSelectionMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveColorSelectionMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveColorSelectionMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveColorSelectionMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveColorSelectionMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveColorSelectionMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveColorSelectionMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveColorSelectionMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveColorSelectionMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveColorSelectionMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveColorSelectionMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveColorSelectionMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveColorSelectionMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveColorSelectionMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveColorSelectionMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveColorSelectionMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveColorSelectionMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveColorSelectionMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveColorSelectionMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveColorSelectionMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveColorSelectionMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveColorSelectionMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveColorSelectionMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveColorSelectionMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveColorSelectionMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveColorSelectionMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveColorSelectionMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveColorSelectionMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveColorSelectionMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveColorSelectionMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveColorSelectionMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveColorSelectionMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveColorSelectionMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveColorSelectionMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveColorSelectionMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveColorSelectionMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveColorSelectionMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveColorSelectionMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveColorSelectionMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveColorSelectionMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveColorSelectionMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveColorSelectionMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveColorSelectionMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveColorSelectionMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveColorSelectionMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveColorSelectionMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveColorSelectionMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveColorSelectionMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveColorSelectionMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveColorSelectionMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveColorSelectionMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveColorSelectionMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveColorSelectionMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveColorSelectionMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveColorSelectionMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveColorSelectionMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveColorSelectionMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveColorSelectionMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveColorSelectionMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveColorSelectionMethod "isAdjusting" o = ColorSelectionIsAdjustingMethodInfo
    ResolveColorSelectionMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveColorSelectionMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveColorSelectionMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveColorSelectionMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveColorSelectionMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveColorSelectionMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveColorSelectionMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveColorSelectionMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveColorSelectionMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveColorSelectionMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveColorSelectionMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveColorSelectionMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveColorSelectionMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveColorSelectionMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveColorSelectionMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveColorSelectionMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveColorSelectionMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveColorSelectionMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveColorSelectionMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveColorSelectionMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveColorSelectionMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveColorSelectionMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveColorSelectionMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveColorSelectionMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveColorSelectionMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveColorSelectionMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveColorSelectionMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveColorSelectionMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveColorSelectionMethod "packEnd" o = Gtk.Box.BoxPackEndMethodInfo
    ResolveColorSelectionMethod "packStart" o = Gtk.Box.BoxPackStartMethodInfo
    ResolveColorSelectionMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveColorSelectionMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveColorSelectionMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveColorSelectionMethod "queryChildPacking" o = Gtk.Box.BoxQueryChildPackingMethodInfo
    ResolveColorSelectionMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveColorSelectionMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveColorSelectionMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveColorSelectionMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveColorSelectionMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveColorSelectionMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveColorSelectionMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveColorSelectionMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveColorSelectionMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveColorSelectionMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveColorSelectionMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveColorSelectionMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveColorSelectionMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolveColorSelectionMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveColorSelectionMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveColorSelectionMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveColorSelectionMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveColorSelectionMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveColorSelectionMethod "reorderChild" o = Gtk.Box.BoxReorderChildMethodInfo
    ResolveColorSelectionMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveColorSelectionMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveColorSelectionMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveColorSelectionMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveColorSelectionMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveColorSelectionMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveColorSelectionMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveColorSelectionMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveColorSelectionMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveColorSelectionMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveColorSelectionMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveColorSelectionMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveColorSelectionMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveColorSelectionMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveColorSelectionMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveColorSelectionMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveColorSelectionMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveColorSelectionMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveColorSelectionMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveColorSelectionMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveColorSelectionMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveColorSelectionMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveColorSelectionMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveColorSelectionMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveColorSelectionMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveColorSelectionMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveColorSelectionMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveColorSelectionMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveColorSelectionMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveColorSelectionMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveColorSelectionMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveColorSelectionMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveColorSelectionMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveColorSelectionMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveColorSelectionMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveColorSelectionMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveColorSelectionMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveColorSelectionMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveColorSelectionMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveColorSelectionMethod "getBaselinePosition" o = Gtk.Box.BoxGetBaselinePositionMethodInfo
    ResolveColorSelectionMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveColorSelectionMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveColorSelectionMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveColorSelectionMethod "getCenterWidget" o = Gtk.Box.BoxGetCenterWidgetMethodInfo
    ResolveColorSelectionMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveColorSelectionMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveColorSelectionMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveColorSelectionMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveColorSelectionMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveColorSelectionMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveColorSelectionMethod "getCurrentAlpha" o = ColorSelectionGetCurrentAlphaMethodInfo
    ResolveColorSelectionMethod "getCurrentColor" o = ColorSelectionGetCurrentColorMethodInfo
    ResolveColorSelectionMethod "getCurrentRgba" o = ColorSelectionGetCurrentRgbaMethodInfo
    ResolveColorSelectionMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveColorSelectionMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveColorSelectionMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveColorSelectionMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveColorSelectionMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveColorSelectionMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveColorSelectionMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveColorSelectionMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveColorSelectionMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveColorSelectionMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveColorSelectionMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveColorSelectionMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveColorSelectionMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveColorSelectionMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveColorSelectionMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveColorSelectionMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveColorSelectionMethod "getHasOpacityControl" o = ColorSelectionGetHasOpacityControlMethodInfo
    ResolveColorSelectionMethod "getHasPalette" o = ColorSelectionGetHasPaletteMethodInfo
    ResolveColorSelectionMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveColorSelectionMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveColorSelectionMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveColorSelectionMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveColorSelectionMethod "getHomogeneous" o = Gtk.Box.BoxGetHomogeneousMethodInfo
    ResolveColorSelectionMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveColorSelectionMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveColorSelectionMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveColorSelectionMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveColorSelectionMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveColorSelectionMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveColorSelectionMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveColorSelectionMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveColorSelectionMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveColorSelectionMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveColorSelectionMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveColorSelectionMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveColorSelectionMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveColorSelectionMethod "getOrientation" o = Gtk.Orientable.OrientableGetOrientationMethodInfo
    ResolveColorSelectionMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveColorSelectionMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveColorSelectionMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveColorSelectionMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveColorSelectionMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveColorSelectionMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveColorSelectionMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveColorSelectionMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveColorSelectionMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveColorSelectionMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveColorSelectionMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveColorSelectionMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveColorSelectionMethod "getPreviousAlpha" o = ColorSelectionGetPreviousAlphaMethodInfo
    ResolveColorSelectionMethod "getPreviousColor" o = ColorSelectionGetPreviousColorMethodInfo
    ResolveColorSelectionMethod "getPreviousRgba" o = ColorSelectionGetPreviousRgbaMethodInfo
    ResolveColorSelectionMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveColorSelectionMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveColorSelectionMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveColorSelectionMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveColorSelectionMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveColorSelectionMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveColorSelectionMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveColorSelectionMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveColorSelectionMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveColorSelectionMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveColorSelectionMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveColorSelectionMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveColorSelectionMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveColorSelectionMethod "getSpacing" o = Gtk.Box.BoxGetSpacingMethodInfo
    ResolveColorSelectionMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveColorSelectionMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveColorSelectionMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveColorSelectionMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveColorSelectionMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveColorSelectionMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveColorSelectionMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveColorSelectionMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveColorSelectionMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveColorSelectionMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveColorSelectionMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveColorSelectionMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveColorSelectionMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveColorSelectionMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveColorSelectionMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveColorSelectionMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveColorSelectionMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveColorSelectionMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveColorSelectionMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveColorSelectionMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveColorSelectionMethod "setBaselinePosition" o = Gtk.Box.BoxSetBaselinePositionMethodInfo
    ResolveColorSelectionMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveColorSelectionMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveColorSelectionMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveColorSelectionMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveColorSelectionMethod "setCenterWidget" o = Gtk.Box.BoxSetCenterWidgetMethodInfo
    ResolveColorSelectionMethod "setChildPacking" o = Gtk.Box.BoxSetChildPackingMethodInfo
    ResolveColorSelectionMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveColorSelectionMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveColorSelectionMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveColorSelectionMethod "setCurrentAlpha" o = ColorSelectionSetCurrentAlphaMethodInfo
    ResolveColorSelectionMethod "setCurrentColor" o = ColorSelectionSetCurrentColorMethodInfo
    ResolveColorSelectionMethod "setCurrentRgba" o = ColorSelectionSetCurrentRgbaMethodInfo
    ResolveColorSelectionMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveColorSelectionMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveColorSelectionMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveColorSelectionMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveColorSelectionMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveColorSelectionMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveColorSelectionMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveColorSelectionMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveColorSelectionMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveColorSelectionMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveColorSelectionMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveColorSelectionMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveColorSelectionMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveColorSelectionMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveColorSelectionMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveColorSelectionMethod "setHasOpacityControl" o = ColorSelectionSetHasOpacityControlMethodInfo
    ResolveColorSelectionMethod "setHasPalette" o = ColorSelectionSetHasPaletteMethodInfo
    ResolveColorSelectionMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveColorSelectionMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveColorSelectionMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveColorSelectionMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveColorSelectionMethod "setHomogeneous" o = Gtk.Box.BoxSetHomogeneousMethodInfo
    ResolveColorSelectionMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveColorSelectionMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveColorSelectionMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveColorSelectionMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveColorSelectionMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveColorSelectionMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveColorSelectionMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveColorSelectionMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveColorSelectionMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveColorSelectionMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveColorSelectionMethod "setOrientation" o = Gtk.Orientable.OrientableSetOrientationMethodInfo
    ResolveColorSelectionMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveColorSelectionMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveColorSelectionMethod "setPreviousAlpha" o = ColorSelectionSetPreviousAlphaMethodInfo
    ResolveColorSelectionMethod "setPreviousColor" o = ColorSelectionSetPreviousColorMethodInfo
    ResolveColorSelectionMethod "setPreviousRgba" o = ColorSelectionSetPreviousRgbaMethodInfo
    ResolveColorSelectionMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveColorSelectionMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveColorSelectionMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveColorSelectionMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveColorSelectionMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveColorSelectionMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveColorSelectionMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveColorSelectionMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveColorSelectionMethod "setSpacing" o = Gtk.Box.BoxSetSpacingMethodInfo
    ResolveColorSelectionMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveColorSelectionMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveColorSelectionMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveColorSelectionMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveColorSelectionMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveColorSelectionMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveColorSelectionMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveColorSelectionMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveColorSelectionMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveColorSelectionMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveColorSelectionMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveColorSelectionMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveColorSelectionMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveColorSelectionMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveColorSelectionMethod t ColorSelection, O.OverloadedMethod info ColorSelection p) => OL.IsLabel t (ColorSelection -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveColorSelectionMethod t ColorSelection, O.OverloadedMethod info ColorSelection p, R.HasField t ColorSelection p) => R.HasField t ColorSelection p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveColorSelectionMethod t ColorSelection, O.OverloadedMethodInfo info ColorSelection) => OL.IsLabel t (O.MethodProxy info ColorSelection) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal ColorSelection::color-changed
-- | This signal is emitted when the color changes in the t'GI.Gtk.Objects.ColorSelection.ColorSelection'
-- according to its update policy.
type ColorSelectionColorChangedCallback =
    IO ()

type C_ColorSelectionColorChangedCallback =
    Ptr ColorSelection ->                   -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ColorSelectionColorChangedCallback`.
foreign import ccall "wrapper"
    mk_ColorSelectionColorChangedCallback :: C_ColorSelectionColorChangedCallback -> IO (FunPtr C_ColorSelectionColorChangedCallback)

wrap_ColorSelectionColorChangedCallback :: 
    GObject a => (a -> ColorSelectionColorChangedCallback) ->
    C_ColorSelectionColorChangedCallback
wrap_ColorSelectionColorChangedCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [colorChanged](#signal:colorChanged) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' colorSelection #colorChanged callback
-- @
-- 
-- 
onColorSelectionColorChanged :: (IsColorSelection a, MonadIO m) => a -> ((?self :: a) => ColorSelectionColorChangedCallback) -> m SignalHandlerId
onColorSelectionColorChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ColorSelectionColorChangedCallback wrapped
    wrapped'' <- mk_ColorSelectionColorChangedCallback wrapped'
    connectSignalFunPtr obj "color-changed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [colorChanged](#signal:colorChanged) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' colorSelection #colorChanged callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterColorSelectionColorChanged :: (IsColorSelection a, MonadIO m) => a -> ((?self :: a) => ColorSelectionColorChangedCallback) -> m SignalHandlerId
afterColorSelectionColorChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ColorSelectionColorChangedCallback wrapped
    wrapped'' <- mk_ColorSelectionColorChangedCallback wrapped'
    connectSignalFunPtr obj "color-changed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ColorSelectionColorChangedSignalInfo
instance SignalInfo ColorSelectionColorChangedSignalInfo where
    type HaskellCallbackType ColorSelectionColorChangedSignalInfo = ColorSelectionColorChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ColorSelectionColorChangedCallback cb
        cb'' <- mk_ColorSelectionColorChangedCallback cb'
        connectSignalFunPtr obj "color-changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ColorSelection::color-changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ColorSelection.html#g:signal:colorChanged"})

#endif

-- VVV Prop "current-alpha"
   -- Type: TBasicType TUInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@current-alpha@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' colorSelection #currentAlpha
-- @
getColorSelectionCurrentAlpha :: (MonadIO m, IsColorSelection o) => o -> m Word32
getColorSelectionCurrentAlpha obj = MIO.liftIO $ B.Properties.getObjectPropertyUInt32 obj "current-alpha"

-- | Set the value of the “@current-alpha@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' colorSelection [ #currentAlpha 'Data.GI.Base.Attributes.:=' value ]
-- @
setColorSelectionCurrentAlpha :: (MonadIO m, IsColorSelection o) => o -> Word32 -> m ()
setColorSelectionCurrentAlpha obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyUInt32 obj "current-alpha" val

-- | Construct a `GValueConstruct` with valid value for the “@current-alpha@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructColorSelectionCurrentAlpha :: (IsColorSelection o, MIO.MonadIO m) => Word32 -> m (GValueConstruct o)
constructColorSelectionCurrentAlpha val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyUInt32 "current-alpha" val

#if defined(ENABLE_OVERLOADING)
data ColorSelectionCurrentAlphaPropertyInfo
instance AttrInfo ColorSelectionCurrentAlphaPropertyInfo where
    type AttrAllowedOps ColorSelectionCurrentAlphaPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ColorSelectionCurrentAlphaPropertyInfo = IsColorSelection
    type AttrSetTypeConstraint ColorSelectionCurrentAlphaPropertyInfo = (~) Word32
    type AttrTransferTypeConstraint ColorSelectionCurrentAlphaPropertyInfo = (~) Word32
    type AttrTransferType ColorSelectionCurrentAlphaPropertyInfo = Word32
    type AttrGetType ColorSelectionCurrentAlphaPropertyInfo = Word32
    type AttrLabel ColorSelectionCurrentAlphaPropertyInfo = "current-alpha"
    type AttrOrigin ColorSelectionCurrentAlphaPropertyInfo = ColorSelection
    attrGet = getColorSelectionCurrentAlpha
    attrSet = setColorSelectionCurrentAlpha
    attrTransfer _ v = do
        return v
    attrConstruct = constructColorSelectionCurrentAlpha
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ColorSelection.currentAlpha"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ColorSelection.html#g:attr:currentAlpha"
        })
#endif

-- VVV Prop "current-color"
   -- Type: TInterface (Name {namespace = "Gdk", name = "Color"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Just False)

-- | Get the value of the “@current-color@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' colorSelection #currentColor
-- @
getColorSelectionCurrentColor :: (MonadIO m, IsColorSelection o) => o -> m (Maybe Gdk.Color.Color)
getColorSelectionCurrentColor obj = MIO.liftIO $ B.Properties.getObjectPropertyBoxed obj "current-color" Gdk.Color.Color

-- | Set the value of the “@current-color@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' colorSelection [ #currentColor 'Data.GI.Base.Attributes.:=' value ]
-- @
setColorSelectionCurrentColor :: (MonadIO m, IsColorSelection o) => o -> Gdk.Color.Color -> m ()
setColorSelectionCurrentColor obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBoxed obj "current-color" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@current-color@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructColorSelectionCurrentColor :: (IsColorSelection o, MIO.MonadIO m) => Gdk.Color.Color -> m (GValueConstruct o)
constructColorSelectionCurrentColor val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBoxed "current-color" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data ColorSelectionCurrentColorPropertyInfo
instance AttrInfo ColorSelectionCurrentColorPropertyInfo where
    type AttrAllowedOps ColorSelectionCurrentColorPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ColorSelectionCurrentColorPropertyInfo = IsColorSelection
    type AttrSetTypeConstraint ColorSelectionCurrentColorPropertyInfo = (~) Gdk.Color.Color
    type AttrTransferTypeConstraint ColorSelectionCurrentColorPropertyInfo = (~) Gdk.Color.Color
    type AttrTransferType ColorSelectionCurrentColorPropertyInfo = Gdk.Color.Color
    type AttrGetType ColorSelectionCurrentColorPropertyInfo = (Maybe Gdk.Color.Color)
    type AttrLabel ColorSelectionCurrentColorPropertyInfo = "current-color"
    type AttrOrigin ColorSelectionCurrentColorPropertyInfo = ColorSelection
    attrGet = getColorSelectionCurrentColor
    attrSet = setColorSelectionCurrentColor
    attrTransfer _ v = do
        return v
    attrConstruct = constructColorSelectionCurrentColor
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ColorSelection.currentColor"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ColorSelection.html#g:attr:currentColor"
        })
#endif

-- VVV Prop "current-rgba"
   -- Type: TInterface (Name {namespace = "Gdk", name = "RGBA"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Just False)

-- | Get the value of the “@current-rgba@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' colorSelection #currentRgba
-- @
getColorSelectionCurrentRgba :: (MonadIO m, IsColorSelection o) => o -> m (Maybe Gdk.RGBA.RGBA)
getColorSelectionCurrentRgba obj = MIO.liftIO $ B.Properties.getObjectPropertyBoxed obj "current-rgba" Gdk.RGBA.RGBA

-- | Set the value of the “@current-rgba@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' colorSelection [ #currentRgba 'Data.GI.Base.Attributes.:=' value ]
-- @
setColorSelectionCurrentRgba :: (MonadIO m, IsColorSelection o) => o -> Gdk.RGBA.RGBA -> m ()
setColorSelectionCurrentRgba obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBoxed obj "current-rgba" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@current-rgba@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructColorSelectionCurrentRgba :: (IsColorSelection o, MIO.MonadIO m) => Gdk.RGBA.RGBA -> m (GValueConstruct o)
constructColorSelectionCurrentRgba val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBoxed "current-rgba" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data ColorSelectionCurrentRgbaPropertyInfo
instance AttrInfo ColorSelectionCurrentRgbaPropertyInfo where
    type AttrAllowedOps ColorSelectionCurrentRgbaPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ColorSelectionCurrentRgbaPropertyInfo = IsColorSelection
    type AttrSetTypeConstraint ColorSelectionCurrentRgbaPropertyInfo = (~) Gdk.RGBA.RGBA
    type AttrTransferTypeConstraint ColorSelectionCurrentRgbaPropertyInfo = (~) Gdk.RGBA.RGBA
    type AttrTransferType ColorSelectionCurrentRgbaPropertyInfo = Gdk.RGBA.RGBA
    type AttrGetType ColorSelectionCurrentRgbaPropertyInfo = (Maybe Gdk.RGBA.RGBA)
    type AttrLabel ColorSelectionCurrentRgbaPropertyInfo = "current-rgba"
    type AttrOrigin ColorSelectionCurrentRgbaPropertyInfo = ColorSelection
    attrGet = getColorSelectionCurrentRgba
    attrSet = setColorSelectionCurrentRgba
    attrTransfer _ v = do
        return v
    attrConstruct = constructColorSelectionCurrentRgba
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ColorSelection.currentRgba"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ColorSelection.html#g:attr:currentRgba"
        })
#endif

-- VVV Prop "has-opacity-control"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@has-opacity-control@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' colorSelection #hasOpacityControl
-- @
getColorSelectionHasOpacityControl :: (MonadIO m, IsColorSelection o) => o -> m Bool
getColorSelectionHasOpacityControl obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "has-opacity-control"

-- | Set the value of the “@has-opacity-control@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' colorSelection [ #hasOpacityControl 'Data.GI.Base.Attributes.:=' value ]
-- @
setColorSelectionHasOpacityControl :: (MonadIO m, IsColorSelection o) => o -> Bool -> m ()
setColorSelectionHasOpacityControl obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "has-opacity-control" val

-- | Construct a `GValueConstruct` with valid value for the “@has-opacity-control@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructColorSelectionHasOpacityControl :: (IsColorSelection o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructColorSelectionHasOpacityControl val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "has-opacity-control" val

#if defined(ENABLE_OVERLOADING)
data ColorSelectionHasOpacityControlPropertyInfo
instance AttrInfo ColorSelectionHasOpacityControlPropertyInfo where
    type AttrAllowedOps ColorSelectionHasOpacityControlPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ColorSelectionHasOpacityControlPropertyInfo = IsColorSelection
    type AttrSetTypeConstraint ColorSelectionHasOpacityControlPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint ColorSelectionHasOpacityControlPropertyInfo = (~) Bool
    type AttrTransferType ColorSelectionHasOpacityControlPropertyInfo = Bool
    type AttrGetType ColorSelectionHasOpacityControlPropertyInfo = Bool
    type AttrLabel ColorSelectionHasOpacityControlPropertyInfo = "has-opacity-control"
    type AttrOrigin ColorSelectionHasOpacityControlPropertyInfo = ColorSelection
    attrGet = getColorSelectionHasOpacityControl
    attrSet = setColorSelectionHasOpacityControl
    attrTransfer _ v = do
        return v
    attrConstruct = constructColorSelectionHasOpacityControl
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ColorSelection.hasOpacityControl"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ColorSelection.html#g:attr:hasOpacityControl"
        })
#endif

-- VVV Prop "has-palette"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@has-palette@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' colorSelection #hasPalette
-- @
getColorSelectionHasPalette :: (MonadIO m, IsColorSelection o) => o -> m Bool
getColorSelectionHasPalette obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "has-palette"

-- | Set the value of the “@has-palette@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' colorSelection [ #hasPalette 'Data.GI.Base.Attributes.:=' value ]
-- @
setColorSelectionHasPalette :: (MonadIO m, IsColorSelection o) => o -> Bool -> m ()
setColorSelectionHasPalette obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "has-palette" val

-- | Construct a `GValueConstruct` with valid value for the “@has-palette@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructColorSelectionHasPalette :: (IsColorSelection o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructColorSelectionHasPalette val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "has-palette" val

#if defined(ENABLE_OVERLOADING)
data ColorSelectionHasPalettePropertyInfo
instance AttrInfo ColorSelectionHasPalettePropertyInfo where
    type AttrAllowedOps ColorSelectionHasPalettePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ColorSelectionHasPalettePropertyInfo = IsColorSelection
    type AttrSetTypeConstraint ColorSelectionHasPalettePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint ColorSelectionHasPalettePropertyInfo = (~) Bool
    type AttrTransferType ColorSelectionHasPalettePropertyInfo = Bool
    type AttrGetType ColorSelectionHasPalettePropertyInfo = Bool
    type AttrLabel ColorSelectionHasPalettePropertyInfo = "has-palette"
    type AttrOrigin ColorSelectionHasPalettePropertyInfo = ColorSelection
    attrGet = getColorSelectionHasPalette
    attrSet = setColorSelectionHasPalette
    attrTransfer _ v = do
        return v
    attrConstruct = constructColorSelectionHasPalette
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ColorSelection.hasPalette"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ColorSelection.html#g:attr:hasPalette"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList ColorSelection
type instance O.AttributeList ColorSelection = ColorSelectionAttributeList
type ColorSelectionAttributeList = ('[ '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("baselinePosition", Gtk.Box.BoxBaselinePositionPropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("currentAlpha", ColorSelectionCurrentAlphaPropertyInfo), '("currentColor", ColorSelectionCurrentColorPropertyInfo), '("currentRgba", ColorSelectionCurrentRgbaPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasOpacityControl", ColorSelectionHasOpacityControlPropertyInfo), '("hasPalette", ColorSelectionHasPalettePropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("homogeneous", Gtk.Box.BoxHomogeneousPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("orientation", Gtk.Orientable.OrientableOrientationPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("spacing", Gtk.Box.BoxSpacingPropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
colorSelectionCurrentAlpha :: AttrLabelProxy "currentAlpha"
colorSelectionCurrentAlpha = AttrLabelProxy

colorSelectionCurrentColor :: AttrLabelProxy "currentColor"
colorSelectionCurrentColor = AttrLabelProxy

colorSelectionCurrentRgba :: AttrLabelProxy "currentRgba"
colorSelectionCurrentRgba = AttrLabelProxy

colorSelectionHasOpacityControl :: AttrLabelProxy "hasOpacityControl"
colorSelectionHasOpacityControl = AttrLabelProxy

colorSelectionHasPalette :: AttrLabelProxy "hasPalette"
colorSelectionHasPalette = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList ColorSelection = ColorSelectionSignalList
type ColorSelectionSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("colorChanged", ColorSelectionColorChangedSignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method ColorSelection::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "ColorSelection" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_color_selection_new" gtk_color_selection_new :: 
    IO (Ptr ColorSelection)

-- | Creates a new GtkColorSelection.
colorSelectionNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m ColorSelection
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.ColorSelection.ColorSelection'
colorSelectionNew  = liftIO $ do
    result <- gtk_color_selection_new
    checkUnexpectedReturnNULL "colorSelectionNew" result
    result' <- (newObject ColorSelection) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method ColorSelection::get_current_alpha
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "colorsel"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ColorSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkColorSelection"
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
-- returnType: Just (TBasicType TUInt16)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_color_selection_get_current_alpha" gtk_color_selection_get_current_alpha :: 
    Ptr ColorSelection ->                   -- colorsel : TInterface (Name {namespace = "Gtk", name = "ColorSelection"})
    IO Word16

-- | Returns the current alpha value.
colorSelectionGetCurrentAlpha ::
    (B.CallStack.HasCallStack, MonadIO m, IsColorSelection a) =>
    a
    -- ^ /@colorsel@/: a t'GI.Gtk.Objects.ColorSelection.ColorSelection'
    -> m Word16
    -- ^ __Returns:__ an integer between 0 and 65535
colorSelectionGetCurrentAlpha colorsel = liftIO $ do
    colorsel' <- unsafeManagedPtrCastPtr colorsel
    result <- gtk_color_selection_get_current_alpha colorsel'
    touchManagedPtr colorsel
    return result

#if defined(ENABLE_OVERLOADING)
data ColorSelectionGetCurrentAlphaMethodInfo
instance (signature ~ (m Word16), MonadIO m, IsColorSelection a) => O.OverloadedMethod ColorSelectionGetCurrentAlphaMethodInfo a signature where
    overloadedMethod = colorSelectionGetCurrentAlpha

instance O.OverloadedMethodInfo ColorSelectionGetCurrentAlphaMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ColorSelection.colorSelectionGetCurrentAlpha",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ColorSelection.html#v:colorSelectionGetCurrentAlpha"
        })


#endif

-- method ColorSelection::get_current_color
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "colorsel"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ColorSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkColorSelection"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "color"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Color" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkColor to fill in with the current color"
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

foreign import ccall "gtk_color_selection_get_current_color" gtk_color_selection_get_current_color :: 
    Ptr ColorSelection ->                   -- colorsel : TInterface (Name {namespace = "Gtk", name = "ColorSelection"})
    Ptr Gdk.Color.Color ->                  -- color : TInterface (Name {namespace = "Gdk", name = "Color"})
    IO ()

{-# DEPRECATED colorSelectionGetCurrentColor ["(Since version 3.4)","Use 'GI.Gtk.Objects.ColorSelection.colorSelectionGetCurrentRgba' instead."] #-}
-- | Sets /@color@/ to be the current color in the GtkColorSelection widget.
colorSelectionGetCurrentColor ::
    (B.CallStack.HasCallStack, MonadIO m, IsColorSelection a) =>
    a
    -- ^ /@colorsel@/: a t'GI.Gtk.Objects.ColorSelection.ColorSelection'
    -> m (Gdk.Color.Color)
colorSelectionGetCurrentColor colorsel = liftIO $ do
    colorsel' <- unsafeManagedPtrCastPtr colorsel
    color <- SP.callocBoxedBytes 12 :: IO (Ptr Gdk.Color.Color)
    gtk_color_selection_get_current_color colorsel' color
    color' <- (wrapBoxed Gdk.Color.Color) color
    touchManagedPtr colorsel
    return color'

#if defined(ENABLE_OVERLOADING)
data ColorSelectionGetCurrentColorMethodInfo
instance (signature ~ (m (Gdk.Color.Color)), MonadIO m, IsColorSelection a) => O.OverloadedMethod ColorSelectionGetCurrentColorMethodInfo a signature where
    overloadedMethod = colorSelectionGetCurrentColor

instance O.OverloadedMethodInfo ColorSelectionGetCurrentColorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ColorSelection.colorSelectionGetCurrentColor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ColorSelection.html#v:colorSelectionGetCurrentColor"
        })


#endif

-- method ColorSelection::get_current_rgba
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "colorsel"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ColorSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkColorSelection"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "rgba"
--           , argType = TInterface Name { namespace = "Gdk" , name = "RGBA" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkRGBA to fill in with the current color"
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

foreign import ccall "gtk_color_selection_get_current_rgba" gtk_color_selection_get_current_rgba :: 
    Ptr ColorSelection ->                   -- colorsel : TInterface (Name {namespace = "Gtk", name = "ColorSelection"})
    Ptr Gdk.RGBA.RGBA ->                    -- rgba : TInterface (Name {namespace = "Gdk", name = "RGBA"})
    IO ()

-- | Sets /@rgba@/ to be the current color in the GtkColorSelection widget.
-- 
-- /Since: 3.0/
colorSelectionGetCurrentRgba ::
    (B.CallStack.HasCallStack, MonadIO m, IsColorSelection a) =>
    a
    -- ^ /@colorsel@/: a t'GI.Gtk.Objects.ColorSelection.ColorSelection'
    -> m (Gdk.RGBA.RGBA)
colorSelectionGetCurrentRgba colorsel = liftIO $ do
    colorsel' <- unsafeManagedPtrCastPtr colorsel
    rgba <- SP.callocBoxedBytes 32 :: IO (Ptr Gdk.RGBA.RGBA)
    gtk_color_selection_get_current_rgba colorsel' rgba
    rgba' <- (wrapBoxed Gdk.RGBA.RGBA) rgba
    touchManagedPtr colorsel
    return rgba'

#if defined(ENABLE_OVERLOADING)
data ColorSelectionGetCurrentRgbaMethodInfo
instance (signature ~ (m (Gdk.RGBA.RGBA)), MonadIO m, IsColorSelection a) => O.OverloadedMethod ColorSelectionGetCurrentRgbaMethodInfo a signature where
    overloadedMethod = colorSelectionGetCurrentRgba

instance O.OverloadedMethodInfo ColorSelectionGetCurrentRgbaMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ColorSelection.colorSelectionGetCurrentRgba",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ColorSelection.html#v:colorSelectionGetCurrentRgba"
        })


#endif

-- method ColorSelection::get_has_opacity_control
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "colorsel"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ColorSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkColorSelection"
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

foreign import ccall "gtk_color_selection_get_has_opacity_control" gtk_color_selection_get_has_opacity_control :: 
    Ptr ColorSelection ->                   -- colorsel : TInterface (Name {namespace = "Gtk", name = "ColorSelection"})
    IO CInt

-- | Determines whether the colorsel has an opacity control.
colorSelectionGetHasOpacityControl ::
    (B.CallStack.HasCallStack, MonadIO m, IsColorSelection a) =>
    a
    -- ^ /@colorsel@/: a t'GI.Gtk.Objects.ColorSelection.ColorSelection'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the /@colorsel@/ has an opacity control,
    --     'P.False' if it does\'t
colorSelectionGetHasOpacityControl colorsel = liftIO $ do
    colorsel' <- unsafeManagedPtrCastPtr colorsel
    result <- gtk_color_selection_get_has_opacity_control colorsel'
    let result' = (/= 0) result
    touchManagedPtr colorsel
    return result'

#if defined(ENABLE_OVERLOADING)
data ColorSelectionGetHasOpacityControlMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsColorSelection a) => O.OverloadedMethod ColorSelectionGetHasOpacityControlMethodInfo a signature where
    overloadedMethod = colorSelectionGetHasOpacityControl

instance O.OverloadedMethodInfo ColorSelectionGetHasOpacityControlMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ColorSelection.colorSelectionGetHasOpacityControl",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ColorSelection.html#v:colorSelectionGetHasOpacityControl"
        })


#endif

-- method ColorSelection::get_has_palette
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "colorsel"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ColorSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkColorSelection"
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

foreign import ccall "gtk_color_selection_get_has_palette" gtk_color_selection_get_has_palette :: 
    Ptr ColorSelection ->                   -- colorsel : TInterface (Name {namespace = "Gtk", name = "ColorSelection"})
    IO CInt

-- | Determines whether the color selector has a color palette.
colorSelectionGetHasPalette ::
    (B.CallStack.HasCallStack, MonadIO m, IsColorSelection a) =>
    a
    -- ^ /@colorsel@/: a t'GI.Gtk.Objects.ColorSelection.ColorSelection'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the selector has a palette, 'P.False' if it hasn\'t
colorSelectionGetHasPalette colorsel = liftIO $ do
    colorsel' <- unsafeManagedPtrCastPtr colorsel
    result <- gtk_color_selection_get_has_palette colorsel'
    let result' = (/= 0) result
    touchManagedPtr colorsel
    return result'

#if defined(ENABLE_OVERLOADING)
data ColorSelectionGetHasPaletteMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsColorSelection a) => O.OverloadedMethod ColorSelectionGetHasPaletteMethodInfo a signature where
    overloadedMethod = colorSelectionGetHasPalette

instance O.OverloadedMethodInfo ColorSelectionGetHasPaletteMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ColorSelection.colorSelectionGetHasPalette",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ColorSelection.html#v:colorSelectionGetHasPalette"
        })


#endif

-- method ColorSelection::get_previous_alpha
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "colorsel"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ColorSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkColorSelection"
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
-- returnType: Just (TBasicType TUInt16)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_color_selection_get_previous_alpha" gtk_color_selection_get_previous_alpha :: 
    Ptr ColorSelection ->                   -- colorsel : TInterface (Name {namespace = "Gtk", name = "ColorSelection"})
    IO Word16

-- | Returns the previous alpha value.
colorSelectionGetPreviousAlpha ::
    (B.CallStack.HasCallStack, MonadIO m, IsColorSelection a) =>
    a
    -- ^ /@colorsel@/: a t'GI.Gtk.Objects.ColorSelection.ColorSelection'
    -> m Word16
    -- ^ __Returns:__ an integer between 0 and 65535
colorSelectionGetPreviousAlpha colorsel = liftIO $ do
    colorsel' <- unsafeManagedPtrCastPtr colorsel
    result <- gtk_color_selection_get_previous_alpha colorsel'
    touchManagedPtr colorsel
    return result

#if defined(ENABLE_OVERLOADING)
data ColorSelectionGetPreviousAlphaMethodInfo
instance (signature ~ (m Word16), MonadIO m, IsColorSelection a) => O.OverloadedMethod ColorSelectionGetPreviousAlphaMethodInfo a signature where
    overloadedMethod = colorSelectionGetPreviousAlpha

instance O.OverloadedMethodInfo ColorSelectionGetPreviousAlphaMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ColorSelection.colorSelectionGetPreviousAlpha",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ColorSelection.html#v:colorSelectionGetPreviousAlpha"
        })


#endif

-- method ColorSelection::get_previous_color
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "colorsel"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ColorSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkColorSelection"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "color"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Color" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GdkColor to fill in with the original color value"
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

foreign import ccall "gtk_color_selection_get_previous_color" gtk_color_selection_get_previous_color :: 
    Ptr ColorSelection ->                   -- colorsel : TInterface (Name {namespace = "Gtk", name = "ColorSelection"})
    Ptr Gdk.Color.Color ->                  -- color : TInterface (Name {namespace = "Gdk", name = "Color"})
    IO ()

{-# DEPRECATED colorSelectionGetPreviousColor ["(Since version 3.4)","Use 'GI.Gtk.Objects.ColorSelection.colorSelectionGetPreviousRgba' instead."] #-}
-- | Fills /@color@/ in with the original color value.
colorSelectionGetPreviousColor ::
    (B.CallStack.HasCallStack, MonadIO m, IsColorSelection a) =>
    a
    -- ^ /@colorsel@/: a t'GI.Gtk.Objects.ColorSelection.ColorSelection'
    -> m (Gdk.Color.Color)
colorSelectionGetPreviousColor colorsel = liftIO $ do
    colorsel' <- unsafeManagedPtrCastPtr colorsel
    color <- SP.callocBoxedBytes 12 :: IO (Ptr Gdk.Color.Color)
    gtk_color_selection_get_previous_color colorsel' color
    color' <- (wrapBoxed Gdk.Color.Color) color
    touchManagedPtr colorsel
    return color'

#if defined(ENABLE_OVERLOADING)
data ColorSelectionGetPreviousColorMethodInfo
instance (signature ~ (m (Gdk.Color.Color)), MonadIO m, IsColorSelection a) => O.OverloadedMethod ColorSelectionGetPreviousColorMethodInfo a signature where
    overloadedMethod = colorSelectionGetPreviousColor

instance O.OverloadedMethodInfo ColorSelectionGetPreviousColorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ColorSelection.colorSelectionGetPreviousColor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ColorSelection.html#v:colorSelectionGetPreviousColor"
        })


#endif

-- method ColorSelection::get_previous_rgba
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "colorsel"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ColorSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkColorSelection"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "rgba"
--           , argType = TInterface Name { namespace = "Gdk" , name = "RGBA" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GdkRGBA to fill in with the original color value"
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

foreign import ccall "gtk_color_selection_get_previous_rgba" gtk_color_selection_get_previous_rgba :: 
    Ptr ColorSelection ->                   -- colorsel : TInterface (Name {namespace = "Gtk", name = "ColorSelection"})
    Ptr Gdk.RGBA.RGBA ->                    -- rgba : TInterface (Name {namespace = "Gdk", name = "RGBA"})
    IO ()

-- | Fills /@rgba@/ in with the original color value.
-- 
-- /Since: 3.0/
colorSelectionGetPreviousRgba ::
    (B.CallStack.HasCallStack, MonadIO m, IsColorSelection a) =>
    a
    -- ^ /@colorsel@/: a t'GI.Gtk.Objects.ColorSelection.ColorSelection'
    -> m (Gdk.RGBA.RGBA)
colorSelectionGetPreviousRgba colorsel = liftIO $ do
    colorsel' <- unsafeManagedPtrCastPtr colorsel
    rgba <- SP.callocBoxedBytes 32 :: IO (Ptr Gdk.RGBA.RGBA)
    gtk_color_selection_get_previous_rgba colorsel' rgba
    rgba' <- (wrapBoxed Gdk.RGBA.RGBA) rgba
    touchManagedPtr colorsel
    return rgba'

#if defined(ENABLE_OVERLOADING)
data ColorSelectionGetPreviousRgbaMethodInfo
instance (signature ~ (m (Gdk.RGBA.RGBA)), MonadIO m, IsColorSelection a) => O.OverloadedMethod ColorSelectionGetPreviousRgbaMethodInfo a signature where
    overloadedMethod = colorSelectionGetPreviousRgba

instance O.OverloadedMethodInfo ColorSelectionGetPreviousRgbaMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ColorSelection.colorSelectionGetPreviousRgba",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ColorSelection.html#v:colorSelectionGetPreviousRgba"
        })


#endif

-- method ColorSelection::is_adjusting
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "colorsel"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ColorSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkColorSelection"
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

foreign import ccall "gtk_color_selection_is_adjusting" gtk_color_selection_is_adjusting :: 
    Ptr ColorSelection ->                   -- colorsel : TInterface (Name {namespace = "Gtk", name = "ColorSelection"})
    IO CInt

-- | Gets the current state of the /@colorsel@/.
colorSelectionIsAdjusting ::
    (B.CallStack.HasCallStack, MonadIO m, IsColorSelection a) =>
    a
    -- ^ /@colorsel@/: a t'GI.Gtk.Objects.ColorSelection.ColorSelection'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the user is currently dragging
    --     a color around, and 'P.False' if the selection has stopped
colorSelectionIsAdjusting colorsel = liftIO $ do
    colorsel' <- unsafeManagedPtrCastPtr colorsel
    result <- gtk_color_selection_is_adjusting colorsel'
    let result' = (/= 0) result
    touchManagedPtr colorsel
    return result'

#if defined(ENABLE_OVERLOADING)
data ColorSelectionIsAdjustingMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsColorSelection a) => O.OverloadedMethod ColorSelectionIsAdjustingMethodInfo a signature where
    overloadedMethod = colorSelectionIsAdjusting

instance O.OverloadedMethodInfo ColorSelectionIsAdjustingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ColorSelection.colorSelectionIsAdjusting",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ColorSelection.html#v:colorSelectionIsAdjusting"
        })


#endif

-- method ColorSelection::set_current_alpha
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "colorsel"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ColorSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkColorSelection"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "alpha"
--           , argType = TBasicType TUInt16
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an integer between 0 and 65535"
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

foreign import ccall "gtk_color_selection_set_current_alpha" gtk_color_selection_set_current_alpha :: 
    Ptr ColorSelection ->                   -- colorsel : TInterface (Name {namespace = "Gtk", name = "ColorSelection"})
    Word16 ->                               -- alpha : TBasicType TUInt16
    IO ()

-- | Sets the current opacity to be /@alpha@/.
-- 
-- The first time this is called, it will also set
-- the original opacity to be /@alpha@/ too.
colorSelectionSetCurrentAlpha ::
    (B.CallStack.HasCallStack, MonadIO m, IsColorSelection a) =>
    a
    -- ^ /@colorsel@/: a t'GI.Gtk.Objects.ColorSelection.ColorSelection'
    -> Word16
    -- ^ /@alpha@/: an integer between 0 and 65535
    -> m ()
colorSelectionSetCurrentAlpha colorsel alpha = liftIO $ do
    colorsel' <- unsafeManagedPtrCastPtr colorsel
    gtk_color_selection_set_current_alpha colorsel' alpha
    touchManagedPtr colorsel
    return ()

#if defined(ENABLE_OVERLOADING)
data ColorSelectionSetCurrentAlphaMethodInfo
instance (signature ~ (Word16 -> m ()), MonadIO m, IsColorSelection a) => O.OverloadedMethod ColorSelectionSetCurrentAlphaMethodInfo a signature where
    overloadedMethod = colorSelectionSetCurrentAlpha

instance O.OverloadedMethodInfo ColorSelectionSetCurrentAlphaMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ColorSelection.colorSelectionSetCurrentAlpha",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ColorSelection.html#v:colorSelectionSetCurrentAlpha"
        })


#endif

-- method ColorSelection::set_current_color
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "colorsel"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ColorSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkColorSelection"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "color"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Color" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkColor to set the current color with"
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

foreign import ccall "gtk_color_selection_set_current_color" gtk_color_selection_set_current_color :: 
    Ptr ColorSelection ->                   -- colorsel : TInterface (Name {namespace = "Gtk", name = "ColorSelection"})
    Ptr Gdk.Color.Color ->                  -- color : TInterface (Name {namespace = "Gdk", name = "Color"})
    IO ()

{-# DEPRECATED colorSelectionSetCurrentColor ["(Since version 3.4)","Use 'GI.Gtk.Objects.ColorSelection.colorSelectionSetCurrentRgba' instead."] #-}
-- | Sets the current color to be /@color@/.
-- 
-- The first time this is called, it will also set
-- the original color to be /@color@/ too.
colorSelectionSetCurrentColor ::
    (B.CallStack.HasCallStack, MonadIO m, IsColorSelection a) =>
    a
    -- ^ /@colorsel@/: a t'GI.Gtk.Objects.ColorSelection.ColorSelection'
    -> Gdk.Color.Color
    -- ^ /@color@/: a t'GI.Gdk.Structs.Color.Color' to set the current color with
    -> m ()
colorSelectionSetCurrentColor colorsel color = liftIO $ do
    colorsel' <- unsafeManagedPtrCastPtr colorsel
    color' <- unsafeManagedPtrGetPtr color
    gtk_color_selection_set_current_color colorsel' color'
    touchManagedPtr colorsel
    touchManagedPtr color
    return ()

#if defined(ENABLE_OVERLOADING)
data ColorSelectionSetCurrentColorMethodInfo
instance (signature ~ (Gdk.Color.Color -> m ()), MonadIO m, IsColorSelection a) => O.OverloadedMethod ColorSelectionSetCurrentColorMethodInfo a signature where
    overloadedMethod = colorSelectionSetCurrentColor

instance O.OverloadedMethodInfo ColorSelectionSetCurrentColorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ColorSelection.colorSelectionSetCurrentColor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ColorSelection.html#v:colorSelectionSetCurrentColor"
        })


#endif

-- method ColorSelection::set_current_rgba
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "colorsel"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ColorSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkColorSelection"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "rgba"
--           , argType = TInterface Name { namespace = "Gdk" , name = "RGBA" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GdkRGBA to set the current color with"
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

foreign import ccall "gtk_color_selection_set_current_rgba" gtk_color_selection_set_current_rgba :: 
    Ptr ColorSelection ->                   -- colorsel : TInterface (Name {namespace = "Gtk", name = "ColorSelection"})
    Ptr Gdk.RGBA.RGBA ->                    -- rgba : TInterface (Name {namespace = "Gdk", name = "RGBA"})
    IO ()

-- | Sets the current color to be /@rgba@/.
-- 
-- The first time this is called, it will also set
-- the original color to be /@rgba@/ too.
-- 
-- /Since: 3.0/
colorSelectionSetCurrentRgba ::
    (B.CallStack.HasCallStack, MonadIO m, IsColorSelection a) =>
    a
    -- ^ /@colorsel@/: a t'GI.Gtk.Objects.ColorSelection.ColorSelection'
    -> Gdk.RGBA.RGBA
    -- ^ /@rgba@/: A t'GI.Gdk.Structs.RGBA.RGBA' to set the current color with
    -> m ()
colorSelectionSetCurrentRgba colorsel rgba = liftIO $ do
    colorsel' <- unsafeManagedPtrCastPtr colorsel
    rgba' <- unsafeManagedPtrGetPtr rgba
    gtk_color_selection_set_current_rgba colorsel' rgba'
    touchManagedPtr colorsel
    touchManagedPtr rgba
    return ()

#if defined(ENABLE_OVERLOADING)
data ColorSelectionSetCurrentRgbaMethodInfo
instance (signature ~ (Gdk.RGBA.RGBA -> m ()), MonadIO m, IsColorSelection a) => O.OverloadedMethod ColorSelectionSetCurrentRgbaMethodInfo a signature where
    overloadedMethod = colorSelectionSetCurrentRgba

instance O.OverloadedMethodInfo ColorSelectionSetCurrentRgbaMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ColorSelection.colorSelectionSetCurrentRgba",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ColorSelection.html#v:colorSelectionSetCurrentRgba"
        })


#endif

-- method ColorSelection::set_has_opacity_control
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "colorsel"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ColorSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkColorSelection"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "has_opacity"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "%TRUE if @colorsel can set the opacity, %FALSE otherwise"
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

foreign import ccall "gtk_color_selection_set_has_opacity_control" gtk_color_selection_set_has_opacity_control :: 
    Ptr ColorSelection ->                   -- colorsel : TInterface (Name {namespace = "Gtk", name = "ColorSelection"})
    CInt ->                                 -- has_opacity : TBasicType TBoolean
    IO ()

-- | Sets the /@colorsel@/ to use or not use opacity.
colorSelectionSetHasOpacityControl ::
    (B.CallStack.HasCallStack, MonadIO m, IsColorSelection a) =>
    a
    -- ^ /@colorsel@/: a t'GI.Gtk.Objects.ColorSelection.ColorSelection'
    -> Bool
    -- ^ /@hasOpacity@/: 'P.True' if /@colorsel@/ can set the opacity, 'P.False' otherwise
    -> m ()
colorSelectionSetHasOpacityControl colorsel hasOpacity = liftIO $ do
    colorsel' <- unsafeManagedPtrCastPtr colorsel
    let hasOpacity' = (fromIntegral . fromEnum) hasOpacity
    gtk_color_selection_set_has_opacity_control colorsel' hasOpacity'
    touchManagedPtr colorsel
    return ()

#if defined(ENABLE_OVERLOADING)
data ColorSelectionSetHasOpacityControlMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsColorSelection a) => O.OverloadedMethod ColorSelectionSetHasOpacityControlMethodInfo a signature where
    overloadedMethod = colorSelectionSetHasOpacityControl

instance O.OverloadedMethodInfo ColorSelectionSetHasOpacityControlMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ColorSelection.colorSelectionSetHasOpacityControl",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ColorSelection.html#v:colorSelectionSetHasOpacityControl"
        })


#endif

-- method ColorSelection::set_has_palette
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "colorsel"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ColorSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkColorSelection"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "has_palette"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "%TRUE if palette is to be visible, %FALSE otherwise"
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

foreign import ccall "gtk_color_selection_set_has_palette" gtk_color_selection_set_has_palette :: 
    Ptr ColorSelection ->                   -- colorsel : TInterface (Name {namespace = "Gtk", name = "ColorSelection"})
    CInt ->                                 -- has_palette : TBasicType TBoolean
    IO ()

-- | Shows and hides the palette based upon the value of /@hasPalette@/.
colorSelectionSetHasPalette ::
    (B.CallStack.HasCallStack, MonadIO m, IsColorSelection a) =>
    a
    -- ^ /@colorsel@/: a t'GI.Gtk.Objects.ColorSelection.ColorSelection'
    -> Bool
    -- ^ /@hasPalette@/: 'P.True' if palette is to be visible, 'P.False' otherwise
    -> m ()
colorSelectionSetHasPalette colorsel hasPalette = liftIO $ do
    colorsel' <- unsafeManagedPtrCastPtr colorsel
    let hasPalette' = (fromIntegral . fromEnum) hasPalette
    gtk_color_selection_set_has_palette colorsel' hasPalette'
    touchManagedPtr colorsel
    return ()

#if defined(ENABLE_OVERLOADING)
data ColorSelectionSetHasPaletteMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsColorSelection a) => O.OverloadedMethod ColorSelectionSetHasPaletteMethodInfo a signature where
    overloadedMethod = colorSelectionSetHasPalette

instance O.OverloadedMethodInfo ColorSelectionSetHasPaletteMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ColorSelection.colorSelectionSetHasPalette",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ColorSelection.html#v:colorSelectionSetHasPalette"
        })


#endif

-- method ColorSelection::set_previous_alpha
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "colorsel"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ColorSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkColorSelection"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "alpha"
--           , argType = TBasicType TUInt16
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an integer between 0 and 65535"
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

foreign import ccall "gtk_color_selection_set_previous_alpha" gtk_color_selection_set_previous_alpha :: 
    Ptr ColorSelection ->                   -- colorsel : TInterface (Name {namespace = "Gtk", name = "ColorSelection"})
    Word16 ->                               -- alpha : TBasicType TUInt16
    IO ()

-- | Sets the “previous” alpha to be /@alpha@/.
-- 
-- This function should be called with some hesitations,
-- as it might seem confusing to have that alpha change.
colorSelectionSetPreviousAlpha ::
    (B.CallStack.HasCallStack, MonadIO m, IsColorSelection a) =>
    a
    -- ^ /@colorsel@/: a t'GI.Gtk.Objects.ColorSelection.ColorSelection'
    -> Word16
    -- ^ /@alpha@/: an integer between 0 and 65535
    -> m ()
colorSelectionSetPreviousAlpha colorsel alpha = liftIO $ do
    colorsel' <- unsafeManagedPtrCastPtr colorsel
    gtk_color_selection_set_previous_alpha colorsel' alpha
    touchManagedPtr colorsel
    return ()

#if defined(ENABLE_OVERLOADING)
data ColorSelectionSetPreviousAlphaMethodInfo
instance (signature ~ (Word16 -> m ()), MonadIO m, IsColorSelection a) => O.OverloadedMethod ColorSelectionSetPreviousAlphaMethodInfo a signature where
    overloadedMethod = colorSelectionSetPreviousAlpha

instance O.OverloadedMethodInfo ColorSelectionSetPreviousAlphaMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ColorSelection.colorSelectionSetPreviousAlpha",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ColorSelection.html#v:colorSelectionSetPreviousAlpha"
        })


#endif

-- method ColorSelection::set_previous_color
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "colorsel"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ColorSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkColorSelection"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "color"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Color" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkColor to set the previous color with"
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

foreign import ccall "gtk_color_selection_set_previous_color" gtk_color_selection_set_previous_color :: 
    Ptr ColorSelection ->                   -- colorsel : TInterface (Name {namespace = "Gtk", name = "ColorSelection"})
    Ptr Gdk.Color.Color ->                  -- color : TInterface (Name {namespace = "Gdk", name = "Color"})
    IO ()

{-# DEPRECATED colorSelectionSetPreviousColor ["(Since version 3.4)","Use 'GI.Gtk.Objects.ColorSelection.colorSelectionSetPreviousRgba' instead."] #-}
-- | Sets the “previous” color to be /@color@/.
-- 
-- This function should be called with some hesitations,
-- as it might seem confusing to have that color change.
-- Calling 'GI.Gtk.Objects.ColorSelection.colorSelectionSetCurrentColor' will also
-- set this color the first time it is called.
colorSelectionSetPreviousColor ::
    (B.CallStack.HasCallStack, MonadIO m, IsColorSelection a) =>
    a
    -- ^ /@colorsel@/: a t'GI.Gtk.Objects.ColorSelection.ColorSelection'
    -> Gdk.Color.Color
    -- ^ /@color@/: a t'GI.Gdk.Structs.Color.Color' to set the previous color with
    -> m ()
colorSelectionSetPreviousColor colorsel color = liftIO $ do
    colorsel' <- unsafeManagedPtrCastPtr colorsel
    color' <- unsafeManagedPtrGetPtr color
    gtk_color_selection_set_previous_color colorsel' color'
    touchManagedPtr colorsel
    touchManagedPtr color
    return ()

#if defined(ENABLE_OVERLOADING)
data ColorSelectionSetPreviousColorMethodInfo
instance (signature ~ (Gdk.Color.Color -> m ()), MonadIO m, IsColorSelection a) => O.OverloadedMethod ColorSelectionSetPreviousColorMethodInfo a signature where
    overloadedMethod = colorSelectionSetPreviousColor

instance O.OverloadedMethodInfo ColorSelectionSetPreviousColorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ColorSelection.colorSelectionSetPreviousColor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ColorSelection.html#v:colorSelectionSetPreviousColor"
        })


#endif

-- method ColorSelection::set_previous_rgba
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "colorsel"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ColorSelection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkColorSelection"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "rgba"
--           , argType = TInterface Name { namespace = "Gdk" , name = "RGBA" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkRGBA to set the previous color with"
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

foreign import ccall "gtk_color_selection_set_previous_rgba" gtk_color_selection_set_previous_rgba :: 
    Ptr ColorSelection ->                   -- colorsel : TInterface (Name {namespace = "Gtk", name = "ColorSelection"})
    Ptr Gdk.RGBA.RGBA ->                    -- rgba : TInterface (Name {namespace = "Gdk", name = "RGBA"})
    IO ()

-- | Sets the “previous” color to be /@rgba@/.
-- 
-- This function should be called with some hesitations,
-- as it might seem confusing to have that color change.
-- Calling 'GI.Gtk.Objects.ColorSelection.colorSelectionSetCurrentRgba' will also
-- set this color the first time it is called.
-- 
-- /Since: 3.0/
colorSelectionSetPreviousRgba ::
    (B.CallStack.HasCallStack, MonadIO m, IsColorSelection a) =>
    a
    -- ^ /@colorsel@/: a t'GI.Gtk.Objects.ColorSelection.ColorSelection'
    -> Gdk.RGBA.RGBA
    -- ^ /@rgba@/: a t'GI.Gdk.Structs.RGBA.RGBA' to set the previous color with
    -> m ()
colorSelectionSetPreviousRgba colorsel rgba = liftIO $ do
    colorsel' <- unsafeManagedPtrCastPtr colorsel
    rgba' <- unsafeManagedPtrGetPtr rgba
    gtk_color_selection_set_previous_rgba colorsel' rgba'
    touchManagedPtr colorsel
    touchManagedPtr rgba
    return ()

#if defined(ENABLE_OVERLOADING)
data ColorSelectionSetPreviousRgbaMethodInfo
instance (signature ~ (Gdk.RGBA.RGBA -> m ()), MonadIO m, IsColorSelection a) => O.OverloadedMethod ColorSelectionSetPreviousRgbaMethodInfo a signature where
    overloadedMethod = colorSelectionSetPreviousRgba

instance O.OverloadedMethodInfo ColorSelectionSetPreviousRgbaMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ColorSelection.colorSelectionSetPreviousRgba",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ColorSelection.html#v:colorSelectionSetPreviousRgba"
        })


#endif

-- method ColorSelection::palette_from_string
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "str"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a string encoding a color palette"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "colors"
--           , argType =
--               TCArray
--                 False
--                 (-1)
--                 2
--                 (TInterface Name { namespace = "Gdk" , name = "Color" })
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "return location for\n    allocated array of #GdkColor"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "n_colors"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return location for length of array"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       ]
-- Lengths: [ Arg
--              { argCName = "n_colors"
--              , argType = TBasicType TInt
--              , direction = DirectionOut
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText = Just "return location for length of array"
--                    , sinceVersion = Nothing
--                    }
--              , argScope = ScopeTypeInvalid
--              , argClosure = -1
--              , argDestroy = -1
--              , argCallerAllocates = False
--              , transfer = TransferEverything
--              }
--          ]
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_color_selection_palette_from_string" gtk_color_selection_palette_from_string :: 
    CString ->                              -- str : TBasicType TUTF8
    Ptr (Ptr Gdk.Color.Color) ->            -- colors : TCArray False (-1) 2 (TInterface (Name {namespace = "Gdk", name = "Color"}))
    Ptr Int32 ->                            -- n_colors : TBasicType TInt
    IO CInt

-- | Parses a color palette string; the string is a colon-separated
-- list of color names readable by 'GI.Gdk.Functions.colorParse'.
colorSelectionPaletteFromString ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@str@/: a string encoding a color palette
    -> m ((Bool, [Gdk.Color.Color]))
    -- ^ __Returns:__ 'P.True' if a palette was successfully parsed
colorSelectionPaletteFromString str = liftIO $ do
    str' <- textToCString str
    colors <- callocMem :: IO (Ptr (Ptr Gdk.Color.Color))
    nColors <- allocMem :: IO (Ptr Int32)
    result <- gtk_color_selection_palette_from_string str' colors nColors
    nColors' <- peek nColors
    let result' = (/= 0) result
    colors' <- peek colors
    colors'' <- (unpackBoxedArrayWithLength 12 nColors') colors'
    colors''' <- mapM (wrapBoxed Gdk.Color.Color) colors''
    freeMem colors'
    freeMem str'
    freeMem colors
    freeMem nColors
    return (result', colors''')

#if defined(ENABLE_OVERLOADING)
#endif

-- method ColorSelection::palette_to_string
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "colors"
--           , argType =
--               TCArray
--                 False
--                 (-1)
--                 1
--                 (TInterface Name { namespace = "Gdk" , name = "Color" })
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an array of colors" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "n_colors"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "length of the array"
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
--              { argCName = "n_colors"
--              , argType = TBasicType TInt
--              , direction = DirectionIn
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText = Just "length of the array"
--                    , sinceVersion = Nothing
--                    }
--              , argScope = ScopeTypeInvalid
--              , argClosure = -1
--              , argDestroy = -1
--              , argCallerAllocates = False
--              , transfer = TransferNothing
--              }
--          ]
-- returnType: Just (TBasicType TUTF8)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_color_selection_palette_to_string" gtk_color_selection_palette_to_string :: 
    Ptr Gdk.Color.Color ->                  -- colors : TCArray False (-1) 1 (TInterface (Name {namespace = "Gdk", name = "Color"}))
    Int32 ->                                -- n_colors : TBasicType TInt
    IO CString

-- | Encodes a palette as a string, useful for persistent storage.
colorSelectionPaletteToString ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    [Gdk.Color.Color]
    -- ^ /@colors@/: an array of colors
    -> m T.Text
    -- ^ __Returns:__ allocated string encoding the palette
colorSelectionPaletteToString colors = liftIO $ do
    let nColors = fromIntegral $ P.length colors
    colors' <- mapM unsafeManagedPtrGetPtr colors
    colors'' <- packBlockArray 12 colors'
    result <- gtk_color_selection_palette_to_string colors'' nColors
    checkUnexpectedReturnNULL "colorSelectionPaletteToString" result
    result' <- cstringToText result
    freeMem result
    mapM_ touchManagedPtr colors
    freeMem colors''
    return result'

#if defined(ENABLE_OVERLOADING)
#endif


