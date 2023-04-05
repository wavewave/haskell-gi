{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Objects.ScaleButton.ScaleButton' provides a button which pops up a scale widget.
-- This kind of widget is commonly used for volume controls in multimedia
-- applications, and GTK+ provides a t'GI.Gtk.Objects.VolumeButton.VolumeButton' subclass that
-- is tailored for this use case.
-- 
-- = CSS nodes
-- 
-- GtkScaleButton has a single CSS node with name button. To differentiate
-- it from a plain t'GI.Gtk.Objects.Button.Button', it gets the .scale style class.
-- 
-- The popup widget that contains the scale has a .scale-popup style class.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.ScaleButton
    ( 

-- * Exported types
    ScaleButton(..)                         ,
    IsScaleButton                           ,
    toScaleButton                           ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [clicked]("GI.Gtk.Objects.Button#g:method:clicked"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [doSetRelatedAction]("GI.Gtk.Interfaces.Activatable#g:method:doSetRelatedAction"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [enter]("GI.Gtk.Objects.Button#g:method:enter"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [leave]("GI.Gtk.Objects.Button#g:method:leave"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [pressed]("GI.Gtk.Objects.Button#g:method:pressed"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [released]("GI.Gtk.Objects.Button#g:method:released"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [syncActionProperties]("GI.Gtk.Interfaces.Activatable#g:method:syncActionProperties"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getActionName]("GI.Gtk.Interfaces.Actionable#g:method:getActionName"), [getActionTargetValue]("GI.Gtk.Interfaces.Actionable#g:method:getActionTargetValue"), [getAdjustment]("GI.Gtk.Objects.ScaleButton#g:method:getAdjustment"), [getAlignment]("GI.Gtk.Objects.Button#g:method:getAlignment"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAlwaysShowImage]("GI.Gtk.Objects.Button#g:method:getAlwaysShowImage"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChild]("GI.Gtk.Objects.Bin#g:method:getChild"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEventWindow]("GI.Gtk.Objects.Button#g:method:getEventWindow"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Button#g:method:getFocusOnClick"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getImage]("GI.Gtk.Objects.Button#g:method:getImage"), [getImagePosition]("GI.Gtk.Objects.Button#g:method:getImagePosition"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getLabel]("GI.Gtk.Objects.Button#g:method:getLabel"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getMinusButton]("GI.Gtk.Objects.ScaleButton#g:method:getMinusButton"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getOrientation]("GI.Gtk.Interfaces.Orientable#g:method:getOrientation"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPlusButton]("GI.Gtk.Objects.ScaleButton#g:method:getPlusButton"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPopup]("GI.Gtk.Objects.ScaleButton#g:method:getPopup"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRelatedAction]("GI.Gtk.Interfaces.Activatable#g:method:getRelatedAction"), [getRelief]("GI.Gtk.Objects.Button#g:method:getRelief"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getUseActionAppearance]("GI.Gtk.Interfaces.Activatable#g:method:getUseActionAppearance"), [getUseStock]("GI.Gtk.Objects.Button#g:method:getUseStock"), [getUseUnderline]("GI.Gtk.Objects.Button#g:method:getUseUnderline"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getValue]("GI.Gtk.Objects.ScaleButton#g:method:getValue"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setActionName]("GI.Gtk.Interfaces.Actionable#g:method:setActionName"), [setActionTargetValue]("GI.Gtk.Interfaces.Actionable#g:method:setActionTargetValue"), [setAdjustment]("GI.Gtk.Objects.ScaleButton#g:method:setAdjustment"), [setAlignment]("GI.Gtk.Objects.Button#g:method:setAlignment"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAlwaysShowImage]("GI.Gtk.Objects.Button#g:method:setAlwaysShowImage"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDetailedActionName]("GI.Gtk.Interfaces.Actionable#g:method:setDetailedActionName"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Button#g:method:setFocusOnClick"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setIcons]("GI.Gtk.Objects.ScaleButton#g:method:setIcons"), [setImage]("GI.Gtk.Objects.Button#g:method:setImage"), [setImagePosition]("GI.Gtk.Objects.Button#g:method:setImagePosition"), [setLabel]("GI.Gtk.Objects.Button#g:method:setLabel"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setOrientation]("GI.Gtk.Interfaces.Orientable#g:method:setOrientation"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setRelatedAction]("GI.Gtk.Interfaces.Activatable#g:method:setRelatedAction"), [setRelief]("GI.Gtk.Objects.Button#g:method:setRelief"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setUseActionAppearance]("GI.Gtk.Interfaces.Activatable#g:method:setUseActionAppearance"), [setUseStock]("GI.Gtk.Objects.Button#g:method:setUseStock"), [setUseUnderline]("GI.Gtk.Objects.Button#g:method:setUseUnderline"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setValue]("GI.Gtk.Objects.ScaleButton#g:method:setValue"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveScaleButtonMethod                ,
#endif

-- ** getAdjustment #method:getAdjustment#

#if defined(ENABLE_OVERLOADING)
    ScaleButtonGetAdjustmentMethodInfo      ,
#endif
    scaleButtonGetAdjustment                ,


-- ** getMinusButton #method:getMinusButton#

#if defined(ENABLE_OVERLOADING)
    ScaleButtonGetMinusButtonMethodInfo     ,
#endif
    scaleButtonGetMinusButton               ,


-- ** getPlusButton #method:getPlusButton#

#if defined(ENABLE_OVERLOADING)
    ScaleButtonGetPlusButtonMethodInfo      ,
#endif
    scaleButtonGetPlusButton                ,


-- ** getPopup #method:getPopup#

#if defined(ENABLE_OVERLOADING)
    ScaleButtonGetPopupMethodInfo           ,
#endif
    scaleButtonGetPopup                     ,


-- ** getValue #method:getValue#

#if defined(ENABLE_OVERLOADING)
    ScaleButtonGetValueMethodInfo           ,
#endif
    scaleButtonGetValue                     ,


-- ** new #method:new#

    scaleButtonNew                          ,


-- ** setAdjustment #method:setAdjustment#

#if defined(ENABLE_OVERLOADING)
    ScaleButtonSetAdjustmentMethodInfo      ,
#endif
    scaleButtonSetAdjustment                ,


-- ** setIcons #method:setIcons#

#if defined(ENABLE_OVERLOADING)
    ScaleButtonSetIconsMethodInfo           ,
#endif
    scaleButtonSetIcons                     ,


-- ** setValue #method:setValue#

#if defined(ENABLE_OVERLOADING)
    ScaleButtonSetValueMethodInfo           ,
#endif
    scaleButtonSetValue                     ,




 -- * Properties


-- ** adjustment #attr:adjustment#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ScaleButtonAdjustmentPropertyInfo       ,
#endif
    constructScaleButtonAdjustment          ,
    getScaleButtonAdjustment                ,
#if defined(ENABLE_OVERLOADING)
    scaleButtonAdjustment                   ,
#endif
    setScaleButtonAdjustment                ,


-- ** icons #attr:icons#
-- | The names of the icons to be used by the scale button.
-- The first item in the array will be used in the button
-- when the current value is the lowest value, the second
-- item for the highest value. All the subsequent icons will
-- be used for all the other values, spread evenly over the
-- range of values.
-- 
-- If there\'s only one icon name in the /@icons@/ array, it will
-- be used for all the values. If only two icon names are in
-- the /@icons@/ array, the first one will be used for the bottom
-- 50% of the scale, and the second one for the top 50%.
-- 
-- It is recommended to use at least 3 icons so that the
-- t'GI.Gtk.Objects.ScaleButton.ScaleButton' reflects the current value of the scale
-- better for the users.
-- 
-- /Since: 2.12/

#if defined(ENABLE_OVERLOADING)
    ScaleButtonIconsPropertyInfo            ,
#endif
    constructScaleButtonIcons               ,
    getScaleButtonIcons                     ,
#if defined(ENABLE_OVERLOADING)
    scaleButtonIcons                        ,
#endif
    setScaleButtonIcons                     ,


-- ** size #attr:size#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ScaleButtonSizePropertyInfo             ,
#endif
    constructScaleButtonSize                ,
    getScaleButtonSize                      ,
#if defined(ENABLE_OVERLOADING)
    scaleButtonSize                         ,
#endif
    setScaleButtonSize                      ,


-- ** value #attr:value#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ScaleButtonValuePropertyInfo            ,
#endif
    constructScaleButtonValue               ,
    getScaleButtonValue                     ,
#if defined(ENABLE_OVERLOADING)
    scaleButtonValue                        ,
#endif
    setScaleButtonValue                     ,




 -- * Signals


-- ** popdown #signal:popdown#

    ScaleButtonPopdownCallback              ,
#if defined(ENABLE_OVERLOADING)
    ScaleButtonPopdownSignalInfo            ,
#endif
    afterScaleButtonPopdown                 ,
    onScaleButtonPopdown                    ,


-- ** popup #signal:popup#

    ScaleButtonPopupCallback                ,
#if defined(ENABLE_OVERLOADING)
    ScaleButtonPopupSignalInfo              ,
#endif
    afterScaleButtonPopup                   ,
    onScaleButtonPopup                      ,


-- ** valueChanged #signal:valueChanged#

    ScaleButtonValueChangedCallback         ,
#if defined(ENABLE_OVERLOADING)
    ScaleButtonValueChangedSignalInfo       ,
#endif
    afterScaleButtonValueChanged            ,
    onScaleButtonValueChanged               ,




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
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Actionable as Gtk.Actionable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Activatable as Gtk.Activatable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Orientable as Gtk.Orientable
import {-# SOURCE #-} qualified GI.Gtk.Objects.Adjustment as Gtk.Adjustment
import {-# SOURCE #-} qualified GI.Gtk.Objects.Bin as Gtk.Bin
import {-# SOURCE #-} qualified GI.Gtk.Objects.Button as Gtk.Button
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype ScaleButton = ScaleButton (SP.ManagedPtr ScaleButton)
    deriving (Eq)

instance SP.ManagedPtrNewtype ScaleButton where
    toManagedPtr (ScaleButton p) = p

foreign import ccall "gtk_scale_button_get_type"
    c_gtk_scale_button_get_type :: IO B.Types.GType

instance B.Types.TypedObject ScaleButton where
    glibType = c_gtk_scale_button_get_type

instance B.Types.GObject ScaleButton

-- | Type class for types which can be safely cast to `ScaleButton`, for instance with `toScaleButton`.
class (SP.GObject o, O.IsDescendantOf ScaleButton o) => IsScaleButton o
instance (SP.GObject o, O.IsDescendantOf ScaleButton o) => IsScaleButton o

instance O.HasParentTypes ScaleButton
type instance O.ParentTypes ScaleButton = '[Gtk.Button.Button, Gtk.Bin.Bin, Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Actionable.Actionable, Gtk.Activatable.Activatable, Gtk.Buildable.Buildable, Gtk.Orientable.Orientable]

-- | Cast to `ScaleButton`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toScaleButton :: (MIO.MonadIO m, IsScaleButton o) => o -> m ScaleButton
toScaleButton = MIO.liftIO . B.ManagedPtr.unsafeCastTo ScaleButton

-- | Convert 'ScaleButton' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe ScaleButton) where
    gvalueGType_ = c_gtk_scale_button_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr ScaleButton)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr ScaleButton)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject ScaleButton ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveScaleButtonMethod (t :: Symbol) (o :: *) :: * where
    ResolveScaleButtonMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveScaleButtonMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveScaleButtonMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveScaleButtonMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveScaleButtonMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveScaleButtonMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveScaleButtonMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveScaleButtonMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveScaleButtonMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveScaleButtonMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveScaleButtonMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveScaleButtonMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveScaleButtonMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveScaleButtonMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveScaleButtonMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveScaleButtonMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveScaleButtonMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveScaleButtonMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveScaleButtonMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveScaleButtonMethod "clicked" o = Gtk.Button.ButtonClickedMethodInfo
    ResolveScaleButtonMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveScaleButtonMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveScaleButtonMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveScaleButtonMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveScaleButtonMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveScaleButtonMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveScaleButtonMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveScaleButtonMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveScaleButtonMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveScaleButtonMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveScaleButtonMethod "doSetRelatedAction" o = Gtk.Activatable.ActivatableDoSetRelatedActionMethodInfo
    ResolveScaleButtonMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveScaleButtonMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveScaleButtonMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveScaleButtonMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveScaleButtonMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveScaleButtonMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveScaleButtonMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveScaleButtonMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveScaleButtonMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveScaleButtonMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveScaleButtonMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveScaleButtonMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveScaleButtonMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveScaleButtonMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveScaleButtonMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveScaleButtonMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveScaleButtonMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveScaleButtonMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveScaleButtonMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveScaleButtonMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveScaleButtonMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveScaleButtonMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveScaleButtonMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveScaleButtonMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveScaleButtonMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveScaleButtonMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveScaleButtonMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveScaleButtonMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveScaleButtonMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveScaleButtonMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveScaleButtonMethod "enter" o = Gtk.Button.ButtonEnterMethodInfo
    ResolveScaleButtonMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveScaleButtonMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveScaleButtonMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveScaleButtonMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveScaleButtonMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveScaleButtonMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveScaleButtonMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveScaleButtonMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveScaleButtonMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveScaleButtonMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveScaleButtonMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveScaleButtonMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveScaleButtonMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveScaleButtonMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveScaleButtonMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveScaleButtonMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveScaleButtonMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveScaleButtonMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveScaleButtonMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveScaleButtonMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveScaleButtonMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveScaleButtonMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveScaleButtonMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveScaleButtonMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveScaleButtonMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveScaleButtonMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveScaleButtonMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveScaleButtonMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveScaleButtonMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveScaleButtonMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveScaleButtonMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveScaleButtonMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveScaleButtonMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveScaleButtonMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveScaleButtonMethod "leave" o = Gtk.Button.ButtonLeaveMethodInfo
    ResolveScaleButtonMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveScaleButtonMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveScaleButtonMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveScaleButtonMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveScaleButtonMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveScaleButtonMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveScaleButtonMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveScaleButtonMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveScaleButtonMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveScaleButtonMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveScaleButtonMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveScaleButtonMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveScaleButtonMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveScaleButtonMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveScaleButtonMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveScaleButtonMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveScaleButtonMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveScaleButtonMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveScaleButtonMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveScaleButtonMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveScaleButtonMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveScaleButtonMethod "pressed" o = Gtk.Button.ButtonPressedMethodInfo
    ResolveScaleButtonMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveScaleButtonMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveScaleButtonMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveScaleButtonMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveScaleButtonMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveScaleButtonMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveScaleButtonMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveScaleButtonMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveScaleButtonMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveScaleButtonMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveScaleButtonMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveScaleButtonMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveScaleButtonMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveScaleButtonMethod "released" o = Gtk.Button.ButtonReleasedMethodInfo
    ResolveScaleButtonMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolveScaleButtonMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveScaleButtonMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveScaleButtonMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveScaleButtonMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveScaleButtonMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveScaleButtonMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveScaleButtonMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveScaleButtonMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveScaleButtonMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveScaleButtonMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveScaleButtonMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveScaleButtonMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveScaleButtonMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveScaleButtonMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveScaleButtonMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveScaleButtonMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveScaleButtonMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveScaleButtonMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveScaleButtonMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveScaleButtonMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveScaleButtonMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveScaleButtonMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveScaleButtonMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveScaleButtonMethod "syncActionProperties" o = Gtk.Activatable.ActivatableSyncActionPropertiesMethodInfo
    ResolveScaleButtonMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveScaleButtonMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveScaleButtonMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveScaleButtonMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveScaleButtonMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveScaleButtonMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveScaleButtonMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveScaleButtonMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveScaleButtonMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveScaleButtonMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveScaleButtonMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveScaleButtonMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveScaleButtonMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveScaleButtonMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveScaleButtonMethod "getActionName" o = Gtk.Actionable.ActionableGetActionNameMethodInfo
    ResolveScaleButtonMethod "getActionTargetValue" o = Gtk.Actionable.ActionableGetActionTargetValueMethodInfo
    ResolveScaleButtonMethod "getAdjustment" o = ScaleButtonGetAdjustmentMethodInfo
    ResolveScaleButtonMethod "getAlignment" o = Gtk.Button.ButtonGetAlignmentMethodInfo
    ResolveScaleButtonMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveScaleButtonMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveScaleButtonMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveScaleButtonMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveScaleButtonMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveScaleButtonMethod "getAlwaysShowImage" o = Gtk.Button.ButtonGetAlwaysShowImageMethodInfo
    ResolveScaleButtonMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveScaleButtonMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveScaleButtonMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveScaleButtonMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveScaleButtonMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveScaleButtonMethod "getChild" o = Gtk.Bin.BinGetChildMethodInfo
    ResolveScaleButtonMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveScaleButtonMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveScaleButtonMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveScaleButtonMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveScaleButtonMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveScaleButtonMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveScaleButtonMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveScaleButtonMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveScaleButtonMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveScaleButtonMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveScaleButtonMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveScaleButtonMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveScaleButtonMethod "getEventWindow" o = Gtk.Button.ButtonGetEventWindowMethodInfo
    ResolveScaleButtonMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveScaleButtonMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveScaleButtonMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveScaleButtonMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveScaleButtonMethod "getFocusOnClick" o = Gtk.Button.ButtonGetFocusOnClickMethodInfo
    ResolveScaleButtonMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveScaleButtonMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveScaleButtonMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveScaleButtonMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveScaleButtonMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveScaleButtonMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveScaleButtonMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveScaleButtonMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveScaleButtonMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveScaleButtonMethod "getImage" o = Gtk.Button.ButtonGetImageMethodInfo
    ResolveScaleButtonMethod "getImagePosition" o = Gtk.Button.ButtonGetImagePositionMethodInfo
    ResolveScaleButtonMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveScaleButtonMethod "getLabel" o = Gtk.Button.ButtonGetLabelMethodInfo
    ResolveScaleButtonMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveScaleButtonMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveScaleButtonMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveScaleButtonMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveScaleButtonMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveScaleButtonMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveScaleButtonMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveScaleButtonMethod "getMinusButton" o = ScaleButtonGetMinusButtonMethodInfo
    ResolveScaleButtonMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveScaleButtonMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveScaleButtonMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveScaleButtonMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveScaleButtonMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveScaleButtonMethod "getOrientation" o = Gtk.Orientable.OrientableGetOrientationMethodInfo
    ResolveScaleButtonMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveScaleButtonMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveScaleButtonMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveScaleButtonMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveScaleButtonMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveScaleButtonMethod "getPlusButton" o = ScaleButtonGetPlusButtonMethodInfo
    ResolveScaleButtonMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveScaleButtonMethod "getPopup" o = ScaleButtonGetPopupMethodInfo
    ResolveScaleButtonMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveScaleButtonMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveScaleButtonMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveScaleButtonMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveScaleButtonMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveScaleButtonMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveScaleButtonMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveScaleButtonMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveScaleButtonMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveScaleButtonMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveScaleButtonMethod "getRelatedAction" o = Gtk.Activatable.ActivatableGetRelatedActionMethodInfo
    ResolveScaleButtonMethod "getRelief" o = Gtk.Button.ButtonGetReliefMethodInfo
    ResolveScaleButtonMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveScaleButtonMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveScaleButtonMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveScaleButtonMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveScaleButtonMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveScaleButtonMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveScaleButtonMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveScaleButtonMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveScaleButtonMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveScaleButtonMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveScaleButtonMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveScaleButtonMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveScaleButtonMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveScaleButtonMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveScaleButtonMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveScaleButtonMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveScaleButtonMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveScaleButtonMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveScaleButtonMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveScaleButtonMethod "getUseActionAppearance" o = Gtk.Activatable.ActivatableGetUseActionAppearanceMethodInfo
    ResolveScaleButtonMethod "getUseStock" o = Gtk.Button.ButtonGetUseStockMethodInfo
    ResolveScaleButtonMethod "getUseUnderline" o = Gtk.Button.ButtonGetUseUnderlineMethodInfo
    ResolveScaleButtonMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveScaleButtonMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveScaleButtonMethod "getValue" o = ScaleButtonGetValueMethodInfo
    ResolveScaleButtonMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveScaleButtonMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveScaleButtonMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveScaleButtonMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveScaleButtonMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveScaleButtonMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveScaleButtonMethod "setActionName" o = Gtk.Actionable.ActionableSetActionNameMethodInfo
    ResolveScaleButtonMethod "setActionTargetValue" o = Gtk.Actionable.ActionableSetActionTargetValueMethodInfo
    ResolveScaleButtonMethod "setAdjustment" o = ScaleButtonSetAdjustmentMethodInfo
    ResolveScaleButtonMethod "setAlignment" o = Gtk.Button.ButtonSetAlignmentMethodInfo
    ResolveScaleButtonMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveScaleButtonMethod "setAlwaysShowImage" o = Gtk.Button.ButtonSetAlwaysShowImageMethodInfo
    ResolveScaleButtonMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveScaleButtonMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveScaleButtonMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveScaleButtonMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveScaleButtonMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveScaleButtonMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveScaleButtonMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveScaleButtonMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveScaleButtonMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveScaleButtonMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveScaleButtonMethod "setDetailedActionName" o = Gtk.Actionable.ActionableSetDetailedActionNameMethodInfo
    ResolveScaleButtonMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveScaleButtonMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveScaleButtonMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveScaleButtonMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveScaleButtonMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveScaleButtonMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveScaleButtonMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveScaleButtonMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveScaleButtonMethod "setFocusOnClick" o = Gtk.Button.ButtonSetFocusOnClickMethodInfo
    ResolveScaleButtonMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveScaleButtonMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveScaleButtonMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveScaleButtonMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveScaleButtonMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveScaleButtonMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveScaleButtonMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveScaleButtonMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveScaleButtonMethod "setIcons" o = ScaleButtonSetIconsMethodInfo
    ResolveScaleButtonMethod "setImage" o = Gtk.Button.ButtonSetImageMethodInfo
    ResolveScaleButtonMethod "setImagePosition" o = Gtk.Button.ButtonSetImagePositionMethodInfo
    ResolveScaleButtonMethod "setLabel" o = Gtk.Button.ButtonSetLabelMethodInfo
    ResolveScaleButtonMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveScaleButtonMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveScaleButtonMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveScaleButtonMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveScaleButtonMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveScaleButtonMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveScaleButtonMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveScaleButtonMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveScaleButtonMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveScaleButtonMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveScaleButtonMethod "setOrientation" o = Gtk.Orientable.OrientableSetOrientationMethodInfo
    ResolveScaleButtonMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveScaleButtonMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveScaleButtonMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveScaleButtonMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveScaleButtonMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveScaleButtonMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveScaleButtonMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveScaleButtonMethod "setRelatedAction" o = Gtk.Activatable.ActivatableSetRelatedActionMethodInfo
    ResolveScaleButtonMethod "setRelief" o = Gtk.Button.ButtonSetReliefMethodInfo
    ResolveScaleButtonMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveScaleButtonMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveScaleButtonMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveScaleButtonMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveScaleButtonMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveScaleButtonMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveScaleButtonMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveScaleButtonMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveScaleButtonMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveScaleButtonMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveScaleButtonMethod "setUseActionAppearance" o = Gtk.Activatable.ActivatableSetUseActionAppearanceMethodInfo
    ResolveScaleButtonMethod "setUseStock" o = Gtk.Button.ButtonSetUseStockMethodInfo
    ResolveScaleButtonMethod "setUseUnderline" o = Gtk.Button.ButtonSetUseUnderlineMethodInfo
    ResolveScaleButtonMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveScaleButtonMethod "setValue" o = ScaleButtonSetValueMethodInfo
    ResolveScaleButtonMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveScaleButtonMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveScaleButtonMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveScaleButtonMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveScaleButtonMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveScaleButtonMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveScaleButtonMethod t ScaleButton, O.OverloadedMethod info ScaleButton p) => OL.IsLabel t (ScaleButton -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveScaleButtonMethod t ScaleButton, O.OverloadedMethod info ScaleButton p, R.HasField t ScaleButton p) => R.HasField t ScaleButton p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveScaleButtonMethod t ScaleButton, O.OverloadedMethodInfo info ScaleButton) => OL.IsLabel t (O.MethodProxy info ScaleButton) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal ScaleButton::popdown
-- | The [popdown](#g:signal:popdown) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted to popdown the scale widget.
-- 
-- The default binding for this signal is Escape.
-- 
-- /Since: 2.12/
type ScaleButtonPopdownCallback =
    IO ()

type C_ScaleButtonPopdownCallback =
    Ptr ScaleButton ->                      -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ScaleButtonPopdownCallback`.
foreign import ccall "wrapper"
    mk_ScaleButtonPopdownCallback :: C_ScaleButtonPopdownCallback -> IO (FunPtr C_ScaleButtonPopdownCallback)

wrap_ScaleButtonPopdownCallback :: 
    GObject a => (a -> ScaleButtonPopdownCallback) ->
    C_ScaleButtonPopdownCallback
wrap_ScaleButtonPopdownCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [popdown](#signal:popdown) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' scaleButton #popdown callback
-- @
-- 
-- 
onScaleButtonPopdown :: (IsScaleButton a, MonadIO m) => a -> ((?self :: a) => ScaleButtonPopdownCallback) -> m SignalHandlerId
onScaleButtonPopdown obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ScaleButtonPopdownCallback wrapped
    wrapped'' <- mk_ScaleButtonPopdownCallback wrapped'
    connectSignalFunPtr obj "popdown" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [popdown](#signal:popdown) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' scaleButton #popdown callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterScaleButtonPopdown :: (IsScaleButton a, MonadIO m) => a -> ((?self :: a) => ScaleButtonPopdownCallback) -> m SignalHandlerId
afterScaleButtonPopdown obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ScaleButtonPopdownCallback wrapped
    wrapped'' <- mk_ScaleButtonPopdownCallback wrapped'
    connectSignalFunPtr obj "popdown" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ScaleButtonPopdownSignalInfo
instance SignalInfo ScaleButtonPopdownSignalInfo where
    type HaskellCallbackType ScaleButtonPopdownSignalInfo = ScaleButtonPopdownCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ScaleButtonPopdownCallback cb
        cb'' <- mk_ScaleButtonPopdownCallback cb'
        connectSignalFunPtr obj "popdown" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ScaleButton::popdown"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ScaleButton.html#g:signal:popdown"})

#endif

-- signal ScaleButton::popup
-- | The [popup](#g:signal:popup) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted to popup the scale widget.
-- 
-- The default bindings for this signal are Space, Enter and Return.
-- 
-- /Since: 2.12/
type ScaleButtonPopupCallback =
    IO ()

type C_ScaleButtonPopupCallback =
    Ptr ScaleButton ->                      -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ScaleButtonPopupCallback`.
foreign import ccall "wrapper"
    mk_ScaleButtonPopupCallback :: C_ScaleButtonPopupCallback -> IO (FunPtr C_ScaleButtonPopupCallback)

wrap_ScaleButtonPopupCallback :: 
    GObject a => (a -> ScaleButtonPopupCallback) ->
    C_ScaleButtonPopupCallback
wrap_ScaleButtonPopupCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [popup](#signal:popup) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' scaleButton #popup callback
-- @
-- 
-- 
onScaleButtonPopup :: (IsScaleButton a, MonadIO m) => a -> ((?self :: a) => ScaleButtonPopupCallback) -> m SignalHandlerId
onScaleButtonPopup obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ScaleButtonPopupCallback wrapped
    wrapped'' <- mk_ScaleButtonPopupCallback wrapped'
    connectSignalFunPtr obj "popup" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [popup](#signal:popup) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' scaleButton #popup callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterScaleButtonPopup :: (IsScaleButton a, MonadIO m) => a -> ((?self :: a) => ScaleButtonPopupCallback) -> m SignalHandlerId
afterScaleButtonPopup obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ScaleButtonPopupCallback wrapped
    wrapped'' <- mk_ScaleButtonPopupCallback wrapped'
    connectSignalFunPtr obj "popup" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ScaleButtonPopupSignalInfo
instance SignalInfo ScaleButtonPopupSignalInfo where
    type HaskellCallbackType ScaleButtonPopupSignalInfo = ScaleButtonPopupCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ScaleButtonPopupCallback cb
        cb'' <- mk_ScaleButtonPopupCallback cb'
        connectSignalFunPtr obj "popup" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ScaleButton::popup"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ScaleButton.html#g:signal:popup"})

#endif

-- signal ScaleButton::value-changed
-- | The [valueChanged](#g:signal:valueChanged) signal is emitted when the value field has
-- changed.
-- 
-- /Since: 2.12/
type ScaleButtonValueChangedCallback =
    Double
    -- ^ /@value@/: the new value
    -> IO ()

type C_ScaleButtonValueChangedCallback =
    Ptr ScaleButton ->                      -- object
    CDouble ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ScaleButtonValueChangedCallback`.
foreign import ccall "wrapper"
    mk_ScaleButtonValueChangedCallback :: C_ScaleButtonValueChangedCallback -> IO (FunPtr C_ScaleButtonValueChangedCallback)

wrap_ScaleButtonValueChangedCallback :: 
    GObject a => (a -> ScaleButtonValueChangedCallback) ->
    C_ScaleButtonValueChangedCallback
wrap_ScaleButtonValueChangedCallback gi'cb gi'selfPtr value _ = do
    let value' = realToFrac value
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  value'


-- | Connect a signal handler for the [valueChanged](#signal:valueChanged) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' scaleButton #valueChanged callback
-- @
-- 
-- 
onScaleButtonValueChanged :: (IsScaleButton a, MonadIO m) => a -> ((?self :: a) => ScaleButtonValueChangedCallback) -> m SignalHandlerId
onScaleButtonValueChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ScaleButtonValueChangedCallback wrapped
    wrapped'' <- mk_ScaleButtonValueChangedCallback wrapped'
    connectSignalFunPtr obj "value-changed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [valueChanged](#signal:valueChanged) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' scaleButton #valueChanged callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterScaleButtonValueChanged :: (IsScaleButton a, MonadIO m) => a -> ((?self :: a) => ScaleButtonValueChangedCallback) -> m SignalHandlerId
afterScaleButtonValueChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ScaleButtonValueChangedCallback wrapped
    wrapped'' <- mk_ScaleButtonValueChangedCallback wrapped'
    connectSignalFunPtr obj "value-changed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ScaleButtonValueChangedSignalInfo
instance SignalInfo ScaleButtonValueChangedSignalInfo where
    type HaskellCallbackType ScaleButtonValueChangedSignalInfo = ScaleButtonValueChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ScaleButtonValueChangedCallback cb
        cb'' <- mk_ScaleButtonValueChangedCallback cb'
        connectSignalFunPtr obj "value-changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ScaleButton::value-changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ScaleButton.html#g:signal:valueChanged"})

#endif

-- VVV Prop "adjustment"
   -- Type: TInterface (Name {namespace = "Gtk", name = "Adjustment"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@adjustment@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' scaleButton #adjustment
-- @
getScaleButtonAdjustment :: (MonadIO m, IsScaleButton o) => o -> m Gtk.Adjustment.Adjustment
getScaleButtonAdjustment obj = MIO.liftIO $ checkUnexpectedNothing "getScaleButtonAdjustment" $ B.Properties.getObjectPropertyObject obj "adjustment" Gtk.Adjustment.Adjustment

-- | Set the value of the “@adjustment@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' scaleButton [ #adjustment 'Data.GI.Base.Attributes.:=' value ]
-- @
setScaleButtonAdjustment :: (MonadIO m, IsScaleButton o, Gtk.Adjustment.IsAdjustment a) => o -> a -> m ()
setScaleButtonAdjustment obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "adjustment" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@adjustment@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructScaleButtonAdjustment :: (IsScaleButton o, MIO.MonadIO m, Gtk.Adjustment.IsAdjustment a) => a -> m (GValueConstruct o)
constructScaleButtonAdjustment val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "adjustment" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data ScaleButtonAdjustmentPropertyInfo
instance AttrInfo ScaleButtonAdjustmentPropertyInfo where
    type AttrAllowedOps ScaleButtonAdjustmentPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ScaleButtonAdjustmentPropertyInfo = IsScaleButton
    type AttrSetTypeConstraint ScaleButtonAdjustmentPropertyInfo = Gtk.Adjustment.IsAdjustment
    type AttrTransferTypeConstraint ScaleButtonAdjustmentPropertyInfo = Gtk.Adjustment.IsAdjustment
    type AttrTransferType ScaleButtonAdjustmentPropertyInfo = Gtk.Adjustment.Adjustment
    type AttrGetType ScaleButtonAdjustmentPropertyInfo = Gtk.Adjustment.Adjustment
    type AttrLabel ScaleButtonAdjustmentPropertyInfo = "adjustment"
    type AttrOrigin ScaleButtonAdjustmentPropertyInfo = ScaleButton
    attrGet = getScaleButtonAdjustment
    attrSet = setScaleButtonAdjustment
    attrTransfer _ v = do
        unsafeCastTo Gtk.Adjustment.Adjustment v
    attrConstruct = constructScaleButtonAdjustment
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ScaleButton.adjustment"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ScaleButton.html#g:attr:adjustment"
        })
#endif

-- VVV Prop "icons"
   -- Type: TCArray True (-1) (-1) (TBasicType TUTF8)
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Just False)

-- | Get the value of the “@icons@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' scaleButton #icons
-- @
getScaleButtonIcons :: (MonadIO m, IsScaleButton o) => o -> m (Maybe [T.Text])
getScaleButtonIcons obj = MIO.liftIO $ B.Properties.getObjectPropertyStringArray obj "icons"

-- | Set the value of the “@icons@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' scaleButton [ #icons 'Data.GI.Base.Attributes.:=' value ]
-- @
setScaleButtonIcons :: (MonadIO m, IsScaleButton o) => o -> [T.Text] -> m ()
setScaleButtonIcons obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyStringArray obj "icons" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@icons@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructScaleButtonIcons :: (IsScaleButton o, MIO.MonadIO m) => [T.Text] -> m (GValueConstruct o)
constructScaleButtonIcons val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyStringArray "icons" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data ScaleButtonIconsPropertyInfo
instance AttrInfo ScaleButtonIconsPropertyInfo where
    type AttrAllowedOps ScaleButtonIconsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ScaleButtonIconsPropertyInfo = IsScaleButton
    type AttrSetTypeConstraint ScaleButtonIconsPropertyInfo = (~) [T.Text]
    type AttrTransferTypeConstraint ScaleButtonIconsPropertyInfo = (~) [T.Text]
    type AttrTransferType ScaleButtonIconsPropertyInfo = [T.Text]
    type AttrGetType ScaleButtonIconsPropertyInfo = (Maybe [T.Text])
    type AttrLabel ScaleButtonIconsPropertyInfo = "icons"
    type AttrOrigin ScaleButtonIconsPropertyInfo = ScaleButton
    attrGet = getScaleButtonIcons
    attrSet = setScaleButtonIcons
    attrTransfer _ v = do
        return v
    attrConstruct = constructScaleButtonIcons
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ScaleButton.icons"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ScaleButton.html#g:attr:icons"
        })
#endif

-- VVV Prop "size"
   -- Type: TInterface (Name {namespace = "Gtk", name = "IconSize"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@size@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' scaleButton #size
-- @
getScaleButtonSize :: (MonadIO m, IsScaleButton o) => o -> m Gtk.Enums.IconSize
getScaleButtonSize obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "size"

-- | Set the value of the “@size@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' scaleButton [ #size 'Data.GI.Base.Attributes.:=' value ]
-- @
setScaleButtonSize :: (MonadIO m, IsScaleButton o) => o -> Gtk.Enums.IconSize -> m ()
setScaleButtonSize obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "size" val

-- | Construct a `GValueConstruct` with valid value for the “@size@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructScaleButtonSize :: (IsScaleButton o, MIO.MonadIO m) => Gtk.Enums.IconSize -> m (GValueConstruct o)
constructScaleButtonSize val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "size" val

#if defined(ENABLE_OVERLOADING)
data ScaleButtonSizePropertyInfo
instance AttrInfo ScaleButtonSizePropertyInfo where
    type AttrAllowedOps ScaleButtonSizePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ScaleButtonSizePropertyInfo = IsScaleButton
    type AttrSetTypeConstraint ScaleButtonSizePropertyInfo = (~) Gtk.Enums.IconSize
    type AttrTransferTypeConstraint ScaleButtonSizePropertyInfo = (~) Gtk.Enums.IconSize
    type AttrTransferType ScaleButtonSizePropertyInfo = Gtk.Enums.IconSize
    type AttrGetType ScaleButtonSizePropertyInfo = Gtk.Enums.IconSize
    type AttrLabel ScaleButtonSizePropertyInfo = "size"
    type AttrOrigin ScaleButtonSizePropertyInfo = ScaleButton
    attrGet = getScaleButtonSize
    attrSet = setScaleButtonSize
    attrTransfer _ v = do
        return v
    attrConstruct = constructScaleButtonSize
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ScaleButton.size"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ScaleButton.html#g:attr:size"
        })
#endif

-- VVV Prop "value"
   -- Type: TBasicType TDouble
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@value@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' scaleButton #value
-- @
getScaleButtonValue :: (MonadIO m, IsScaleButton o) => o -> m Double
getScaleButtonValue obj = MIO.liftIO $ B.Properties.getObjectPropertyDouble obj "value"

-- | Set the value of the “@value@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' scaleButton [ #value 'Data.GI.Base.Attributes.:=' value ]
-- @
setScaleButtonValue :: (MonadIO m, IsScaleButton o) => o -> Double -> m ()
setScaleButtonValue obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyDouble obj "value" val

-- | Construct a `GValueConstruct` with valid value for the “@value@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructScaleButtonValue :: (IsScaleButton o, MIO.MonadIO m) => Double -> m (GValueConstruct o)
constructScaleButtonValue val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyDouble "value" val

#if defined(ENABLE_OVERLOADING)
data ScaleButtonValuePropertyInfo
instance AttrInfo ScaleButtonValuePropertyInfo where
    type AttrAllowedOps ScaleButtonValuePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ScaleButtonValuePropertyInfo = IsScaleButton
    type AttrSetTypeConstraint ScaleButtonValuePropertyInfo = (~) Double
    type AttrTransferTypeConstraint ScaleButtonValuePropertyInfo = (~) Double
    type AttrTransferType ScaleButtonValuePropertyInfo = Double
    type AttrGetType ScaleButtonValuePropertyInfo = Double
    type AttrLabel ScaleButtonValuePropertyInfo = "value"
    type AttrOrigin ScaleButtonValuePropertyInfo = ScaleButton
    attrGet = getScaleButtonValue
    attrSet = setScaleButtonValue
    attrTransfer _ v = do
        return v
    attrConstruct = constructScaleButtonValue
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ScaleButton.value"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ScaleButton.html#g:attr:value"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList ScaleButton
type instance O.AttributeList ScaleButton = ScaleButtonAttributeList
type ScaleButtonAttributeList = ('[ '("actionName", Gtk.Actionable.ActionableActionNamePropertyInfo), '("actionTarget", Gtk.Actionable.ActionableActionTargetPropertyInfo), '("adjustment", ScaleButtonAdjustmentPropertyInfo), '("alwaysShowImage", Gtk.Button.ButtonAlwaysShowImagePropertyInfo), '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("icons", ScaleButtonIconsPropertyInfo), '("image", Gtk.Button.ButtonImagePropertyInfo), '("imagePosition", Gtk.Button.ButtonImagePositionPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("label", Gtk.Button.ButtonLabelPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("orientation", Gtk.Orientable.OrientableOrientationPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("relatedAction", Gtk.Activatable.ActivatableRelatedActionPropertyInfo), '("relief", Gtk.Button.ButtonReliefPropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("size", ScaleButtonSizePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("useActionAppearance", Gtk.Activatable.ActivatableUseActionAppearancePropertyInfo), '("useStock", Gtk.Button.ButtonUseStockPropertyInfo), '("useUnderline", Gtk.Button.ButtonUseUnderlinePropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("value", ScaleButtonValuePropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo), '("xalign", Gtk.Button.ButtonXalignPropertyInfo), '("yalign", Gtk.Button.ButtonYalignPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
scaleButtonAdjustment :: AttrLabelProxy "adjustment"
scaleButtonAdjustment = AttrLabelProxy

scaleButtonIcons :: AttrLabelProxy "icons"
scaleButtonIcons = AttrLabelProxy

scaleButtonSize :: AttrLabelProxy "size"
scaleButtonSize = AttrLabelProxy

scaleButtonValue :: AttrLabelProxy "value"
scaleButtonValue = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList ScaleButton = ScaleButtonSignalList
type ScaleButtonSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("activate", Gtk.Button.ButtonActivateSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("clicked", Gtk.Button.ButtonClickedSignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enter", Gtk.Button.ButtonEnterSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leave", Gtk.Button.ButtonLeaveSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popdown", ScaleButtonPopdownSignalInfo), '("popup", ScaleButtonPopupSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("pressed", Gtk.Button.ButtonPressedSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("released", Gtk.Button.ButtonReleasedSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("valueChanged", ScaleButtonValueChangedSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method ScaleButton::new
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "size"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a stock icon size (#GtkIconSize)"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "min"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the minimum value of the scale (usually 0)"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "max"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the maximum value of the scale (usually 100)"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "step"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the stepping of value when a scroll-wheel event,\n       or up/down arrow event occurs (usually 2)"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icons"
--           , argType = TCArray True (-1) (-1) (TBasicType TUTF8)
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a %NULL-terminated\n        array of icon names, or %NULL if you want to set the list\n        later with gtk_scale_button_set_icons()"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "ScaleButton" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_scale_button_new" gtk_scale_button_new :: 
    Int32 ->                                -- size : TBasicType TInt
    CDouble ->                              -- min : TBasicType TDouble
    CDouble ->                              -- max : TBasicType TDouble
    CDouble ->                              -- step : TBasicType TDouble
    Ptr CString ->                          -- icons : TCArray True (-1) (-1) (TBasicType TUTF8)
    IO (Ptr ScaleButton)

-- | Creates a t'GI.Gtk.Objects.ScaleButton.ScaleButton', with a range between /@min@/ and /@max@/, with
-- a stepping of /@step@/.
-- 
-- /Since: 2.12/
scaleButtonNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Int32
    -- ^ /@size@/: a stock icon size (t'GI.Gtk.Enums.IconSize')
    -> Double
    -- ^ /@min@/: the minimum value of the scale (usually 0)
    -> Double
    -- ^ /@max@/: the maximum value of the scale (usually 100)
    -> Double
    -- ^ /@step@/: the stepping of value when a scroll-wheel event,
    --        or up\/down arrow event occurs (usually 2)
    -> Maybe ([T.Text])
    -- ^ /@icons@/: a 'P.Nothing'-terminated
    --         array of icon names, or 'P.Nothing' if you want to set the list
    --         later with 'GI.Gtk.Objects.ScaleButton.scaleButtonSetIcons'
    -> m ScaleButton
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.ScaleButton.ScaleButton'
scaleButtonNew size min max step icons = liftIO $ do
    let min' = realToFrac min
    let max' = realToFrac max
    let step' = realToFrac step
    maybeIcons <- case icons of
        Nothing -> return nullPtr
        Just jIcons -> do
            jIcons' <- packZeroTerminatedUTF8CArray jIcons
            return jIcons'
    result <- gtk_scale_button_new size min' max' step' maybeIcons
    checkUnexpectedReturnNULL "scaleButtonNew" result
    result' <- (newObject ScaleButton) result
    mapZeroTerminatedCArray freeMem maybeIcons
    freeMem maybeIcons
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method ScaleButton::get_adjustment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ScaleButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkScaleButton" , sinceVersion = Nothing }
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

foreign import ccall "gtk_scale_button_get_adjustment" gtk_scale_button_get_adjustment :: 
    Ptr ScaleButton ->                      -- button : TInterface (Name {namespace = "Gtk", name = "ScaleButton"})
    IO (Ptr Gtk.Adjustment.Adjustment)

-- | Gets the t'GI.Gtk.Objects.Adjustment.Adjustment' associated with the t'GI.Gtk.Objects.ScaleButton.ScaleButton'’s scale.
-- See 'GI.Gtk.Objects.Range.rangeGetAdjustment' for details.
-- 
-- /Since: 2.12/
scaleButtonGetAdjustment ::
    (B.CallStack.HasCallStack, MonadIO m, IsScaleButton a) =>
    a
    -- ^ /@button@/: a t'GI.Gtk.Objects.ScaleButton.ScaleButton'
    -> m Gtk.Adjustment.Adjustment
    -- ^ __Returns:__ the adjustment associated with the scale
scaleButtonGetAdjustment button = liftIO $ do
    button' <- unsafeManagedPtrCastPtr button
    result <- gtk_scale_button_get_adjustment button'
    checkUnexpectedReturnNULL "scaleButtonGetAdjustment" result
    result' <- (newObject Gtk.Adjustment.Adjustment) result
    touchManagedPtr button
    return result'

#if defined(ENABLE_OVERLOADING)
data ScaleButtonGetAdjustmentMethodInfo
instance (signature ~ (m Gtk.Adjustment.Adjustment), MonadIO m, IsScaleButton a) => O.OverloadedMethod ScaleButtonGetAdjustmentMethodInfo a signature where
    overloadedMethod = scaleButtonGetAdjustment

instance O.OverloadedMethodInfo ScaleButtonGetAdjustmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ScaleButton.scaleButtonGetAdjustment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ScaleButton.html#v:scaleButtonGetAdjustment"
        })


#endif

-- method ScaleButton::get_minus_button
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ScaleButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkScaleButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Button" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_scale_button_get_minus_button" gtk_scale_button_get_minus_button :: 
    Ptr ScaleButton ->                      -- button : TInterface (Name {namespace = "Gtk", name = "ScaleButton"})
    IO (Ptr Gtk.Button.Button)

-- | Retrieves the minus button of the t'GI.Gtk.Objects.ScaleButton.ScaleButton'.
-- 
-- /Since: 2.14/
scaleButtonGetMinusButton ::
    (B.CallStack.HasCallStack, MonadIO m, IsScaleButton a) =>
    a
    -- ^ /@button@/: a t'GI.Gtk.Objects.ScaleButton.ScaleButton'
    -> m Gtk.Button.Button
    -- ^ __Returns:__ the minus button of the t'GI.Gtk.Objects.ScaleButton.ScaleButton' as a t'GI.Gtk.Objects.Button.Button'
scaleButtonGetMinusButton button = liftIO $ do
    button' <- unsafeManagedPtrCastPtr button
    result <- gtk_scale_button_get_minus_button button'
    checkUnexpectedReturnNULL "scaleButtonGetMinusButton" result
    result' <- (newObject Gtk.Button.Button) result
    touchManagedPtr button
    return result'

#if defined(ENABLE_OVERLOADING)
data ScaleButtonGetMinusButtonMethodInfo
instance (signature ~ (m Gtk.Button.Button), MonadIO m, IsScaleButton a) => O.OverloadedMethod ScaleButtonGetMinusButtonMethodInfo a signature where
    overloadedMethod = scaleButtonGetMinusButton

instance O.OverloadedMethodInfo ScaleButtonGetMinusButtonMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ScaleButton.scaleButtonGetMinusButton",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ScaleButton.html#v:scaleButtonGetMinusButton"
        })


#endif

-- method ScaleButton::get_plus_button
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ScaleButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkScaleButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Button" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_scale_button_get_plus_button" gtk_scale_button_get_plus_button :: 
    Ptr ScaleButton ->                      -- button : TInterface (Name {namespace = "Gtk", name = "ScaleButton"})
    IO (Ptr Gtk.Button.Button)

-- | Retrieves the plus button of the t'GI.Gtk.Objects.ScaleButton.ScaleButton'.
-- 
-- /Since: 2.14/
scaleButtonGetPlusButton ::
    (B.CallStack.HasCallStack, MonadIO m, IsScaleButton a) =>
    a
    -- ^ /@button@/: a t'GI.Gtk.Objects.ScaleButton.ScaleButton'
    -> m Gtk.Button.Button
    -- ^ __Returns:__ the plus button of the t'GI.Gtk.Objects.ScaleButton.ScaleButton' as a t'GI.Gtk.Objects.Button.Button'
scaleButtonGetPlusButton button = liftIO $ do
    button' <- unsafeManagedPtrCastPtr button
    result <- gtk_scale_button_get_plus_button button'
    checkUnexpectedReturnNULL "scaleButtonGetPlusButton" result
    result' <- (newObject Gtk.Button.Button) result
    touchManagedPtr button
    return result'

#if defined(ENABLE_OVERLOADING)
data ScaleButtonGetPlusButtonMethodInfo
instance (signature ~ (m Gtk.Button.Button), MonadIO m, IsScaleButton a) => O.OverloadedMethod ScaleButtonGetPlusButtonMethodInfo a signature where
    overloadedMethod = scaleButtonGetPlusButton

instance O.OverloadedMethodInfo ScaleButtonGetPlusButtonMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ScaleButton.scaleButtonGetPlusButton",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ScaleButton.html#v:scaleButtonGetPlusButton"
        })


#endif

-- method ScaleButton::get_popup
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ScaleButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkScaleButton" , sinceVersion = Nothing }
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

foreign import ccall "gtk_scale_button_get_popup" gtk_scale_button_get_popup :: 
    Ptr ScaleButton ->                      -- button : TInterface (Name {namespace = "Gtk", name = "ScaleButton"})
    IO (Ptr Gtk.Widget.Widget)

-- | Retrieves the popup of the t'GI.Gtk.Objects.ScaleButton.ScaleButton'.
-- 
-- /Since: 2.14/
scaleButtonGetPopup ::
    (B.CallStack.HasCallStack, MonadIO m, IsScaleButton a) =>
    a
    -- ^ /@button@/: a t'GI.Gtk.Objects.ScaleButton.ScaleButton'
    -> m Gtk.Widget.Widget
    -- ^ __Returns:__ the popup of the t'GI.Gtk.Objects.ScaleButton.ScaleButton'
scaleButtonGetPopup button = liftIO $ do
    button' <- unsafeManagedPtrCastPtr button
    result <- gtk_scale_button_get_popup button'
    checkUnexpectedReturnNULL "scaleButtonGetPopup" result
    result' <- (newObject Gtk.Widget.Widget) result
    touchManagedPtr button
    return result'

#if defined(ENABLE_OVERLOADING)
data ScaleButtonGetPopupMethodInfo
instance (signature ~ (m Gtk.Widget.Widget), MonadIO m, IsScaleButton a) => O.OverloadedMethod ScaleButtonGetPopupMethodInfo a signature where
    overloadedMethod = scaleButtonGetPopup

instance O.OverloadedMethodInfo ScaleButtonGetPopupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ScaleButton.scaleButtonGetPopup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ScaleButton.html#v:scaleButtonGetPopup"
        })


#endif

-- method ScaleButton::get_value
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ScaleButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkScaleButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TDouble)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_scale_button_get_value" gtk_scale_button_get_value :: 
    Ptr ScaleButton ->                      -- button : TInterface (Name {namespace = "Gtk", name = "ScaleButton"})
    IO CDouble

-- | Gets the current value of the scale button.
-- 
-- /Since: 2.12/
scaleButtonGetValue ::
    (B.CallStack.HasCallStack, MonadIO m, IsScaleButton a) =>
    a
    -- ^ /@button@/: a t'GI.Gtk.Objects.ScaleButton.ScaleButton'
    -> m Double
    -- ^ __Returns:__ current value of the scale button
scaleButtonGetValue button = liftIO $ do
    button' <- unsafeManagedPtrCastPtr button
    result <- gtk_scale_button_get_value button'
    let result' = realToFrac result
    touchManagedPtr button
    return result'

#if defined(ENABLE_OVERLOADING)
data ScaleButtonGetValueMethodInfo
instance (signature ~ (m Double), MonadIO m, IsScaleButton a) => O.OverloadedMethod ScaleButtonGetValueMethodInfo a signature where
    overloadedMethod = scaleButtonGetValue

instance O.OverloadedMethodInfo ScaleButtonGetValueMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ScaleButton.scaleButtonGetValue",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ScaleButton.html#v:scaleButtonGetValue"
        })


#endif

-- method ScaleButton::set_adjustment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ScaleButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkScaleButton" , sinceVersion = Nothing }
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
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAdjustment" , sinceVersion = Nothing }
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

foreign import ccall "gtk_scale_button_set_adjustment" gtk_scale_button_set_adjustment :: 
    Ptr ScaleButton ->                      -- button : TInterface (Name {namespace = "Gtk", name = "ScaleButton"})
    Ptr Gtk.Adjustment.Adjustment ->        -- adjustment : TInterface (Name {namespace = "Gtk", name = "Adjustment"})
    IO ()

-- | Sets the t'GI.Gtk.Objects.Adjustment.Adjustment' to be used as a model
-- for the t'GI.Gtk.Objects.ScaleButton.ScaleButton'’s scale.
-- See 'GI.Gtk.Objects.Range.rangeSetAdjustment' for details.
-- 
-- /Since: 2.12/
scaleButtonSetAdjustment ::
    (B.CallStack.HasCallStack, MonadIO m, IsScaleButton a, Gtk.Adjustment.IsAdjustment b) =>
    a
    -- ^ /@button@/: a t'GI.Gtk.Objects.ScaleButton.ScaleButton'
    -> b
    -- ^ /@adjustment@/: a t'GI.Gtk.Objects.Adjustment.Adjustment'
    -> m ()
scaleButtonSetAdjustment button adjustment = liftIO $ do
    button' <- unsafeManagedPtrCastPtr button
    adjustment' <- unsafeManagedPtrCastPtr adjustment
    gtk_scale_button_set_adjustment button' adjustment'
    touchManagedPtr button
    touchManagedPtr adjustment
    return ()

#if defined(ENABLE_OVERLOADING)
data ScaleButtonSetAdjustmentMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsScaleButton a, Gtk.Adjustment.IsAdjustment b) => O.OverloadedMethod ScaleButtonSetAdjustmentMethodInfo a signature where
    overloadedMethod = scaleButtonSetAdjustment

instance O.OverloadedMethodInfo ScaleButtonSetAdjustmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ScaleButton.scaleButtonSetAdjustment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ScaleButton.html#v:scaleButtonSetAdjustment"
        })


#endif

-- method ScaleButton::set_icons
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ScaleButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkScaleButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icons"
--           , argType = TCArray True (-1) (-1) (TBasicType TUTF8)
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a %NULL-terminated array of icon names"
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

foreign import ccall "gtk_scale_button_set_icons" gtk_scale_button_set_icons :: 
    Ptr ScaleButton ->                      -- button : TInterface (Name {namespace = "Gtk", name = "ScaleButton"})
    Ptr CString ->                          -- icons : TCArray True (-1) (-1) (TBasicType TUTF8)
    IO ()

-- | Sets the icons to be used by the scale button.
-- For details, see the [ScaleButton:icons]("GI.Gtk.Objects.ScaleButton#g:attr:icons") property.
-- 
-- /Since: 2.12/
scaleButtonSetIcons ::
    (B.CallStack.HasCallStack, MonadIO m, IsScaleButton a) =>
    a
    -- ^ /@button@/: a t'GI.Gtk.Objects.ScaleButton.ScaleButton'
    -> [T.Text]
    -- ^ /@icons@/: a 'P.Nothing'-terminated array of icon names
    -> m ()
scaleButtonSetIcons button icons = liftIO $ do
    button' <- unsafeManagedPtrCastPtr button
    icons' <- packZeroTerminatedUTF8CArray icons
    gtk_scale_button_set_icons button' icons'
    touchManagedPtr button
    mapZeroTerminatedCArray freeMem icons'
    freeMem icons'
    return ()

#if defined(ENABLE_OVERLOADING)
data ScaleButtonSetIconsMethodInfo
instance (signature ~ ([T.Text] -> m ()), MonadIO m, IsScaleButton a) => O.OverloadedMethod ScaleButtonSetIconsMethodInfo a signature where
    overloadedMethod = scaleButtonSetIcons

instance O.OverloadedMethodInfo ScaleButtonSetIconsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ScaleButton.scaleButtonSetIcons",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ScaleButton.html#v:scaleButtonSetIcons"
        })


#endif

-- method ScaleButton::set_value
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ScaleButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkScaleButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "value"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "new value of the scale button"
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

foreign import ccall "gtk_scale_button_set_value" gtk_scale_button_set_value :: 
    Ptr ScaleButton ->                      -- button : TInterface (Name {namespace = "Gtk", name = "ScaleButton"})
    CDouble ->                              -- value : TBasicType TDouble
    IO ()

-- | Sets the current value of the scale; if the value is outside
-- the minimum or maximum range values, it will be clamped to fit
-- inside them. The scale button emits the [ScaleButton::valueChanged]("GI.Gtk.Objects.ScaleButton#g:signal:valueChanged")
-- signal if the value changes.
-- 
-- /Since: 2.12/
scaleButtonSetValue ::
    (B.CallStack.HasCallStack, MonadIO m, IsScaleButton a) =>
    a
    -- ^ /@button@/: a t'GI.Gtk.Objects.ScaleButton.ScaleButton'
    -> Double
    -- ^ /@value@/: new value of the scale button
    -> m ()
scaleButtonSetValue button value = liftIO $ do
    button' <- unsafeManagedPtrCastPtr button
    let value' = realToFrac value
    gtk_scale_button_set_value button' value'
    touchManagedPtr button
    return ()

#if defined(ENABLE_OVERLOADING)
data ScaleButtonSetValueMethodInfo
instance (signature ~ (Double -> m ()), MonadIO m, IsScaleButton a) => O.OverloadedMethod ScaleButtonSetValueMethodInfo a signature where
    overloadedMethod = scaleButtonSetValue

instance O.OverloadedMethodInfo ScaleButtonSetValueMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ScaleButton.scaleButtonSetValue",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ScaleButton.html#v:scaleButtonSetValue"
        })


#endif


