{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- This interface provides a convenient way of associating widgets with
-- actions on a t'GI.Gtk.Objects.ApplicationWindow.ApplicationWindow' or t'GI.Gtk.Objects.Application.Application'.
-- 
-- It primarily consists of two properties: t'GI.Gtk.Interfaces.Actionable.Actionable':@/action-name/@
-- and t'GI.Gtk.Interfaces.Actionable.Actionable':@/action-target/@. There are also some convenience APIs
-- for setting these properties.
-- 
-- The action will be looked up in action groups that are found among
-- the widgets ancestors. Most commonly, these will be the actions with
-- the “win.” or “app.” prefix that are associated with the t'GI.Gtk.Objects.ApplicationWindow.ApplicationWindow'
-- or t'GI.Gtk.Objects.Application.Application', but other action groups that are added with
-- 'GI.Gtk.Objects.Widget.widgetInsertActionGroup' will be consulted as well.
-- 
-- /Since: 3.4/

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Interfaces.Actionable
    ( 

-- * Exported types
    Actionable(..)                          ,
    IsActionable                            ,
    toActionable                            ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childNotify]("GI.Gtk.Objects.Widget#g:method:childNotify"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getActionName]("GI.Gtk.Interfaces.Actionable#g:method:getActionName"), [getActionTargetValue]("GI.Gtk.Interfaces.Actionable#g:method:getActionTargetValue"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setActionName]("GI.Gtk.Interfaces.Actionable#g:method:setActionName"), [setActionTargetValue]("GI.Gtk.Interfaces.Actionable#g:method:setActionTargetValue"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDetailedActionName]("GI.Gtk.Interfaces.Actionable#g:method:setDetailedActionName"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveActionableMethod                 ,
#endif

-- ** getActionName #method:getActionName#

#if defined(ENABLE_OVERLOADING)
    ActionableGetActionNameMethodInfo       ,
#endif
    actionableGetActionName                 ,


-- ** getActionTargetValue #method:getActionTargetValue#

#if defined(ENABLE_OVERLOADING)
    ActionableGetActionTargetValueMethodInfo,
#endif
    actionableGetActionTargetValue          ,


-- ** setActionName #method:setActionName#

#if defined(ENABLE_OVERLOADING)
    ActionableSetActionNameMethodInfo       ,
#endif
    actionableSetActionName                 ,


-- ** setActionTargetValue #method:setActionTargetValue#

#if defined(ENABLE_OVERLOADING)
    ActionableSetActionTargetValueMethodInfo,
#endif
    actionableSetActionTargetValue          ,


-- ** setDetailedActionName #method:setDetailedActionName#

#if defined(ENABLE_OVERLOADING)
    ActionableSetDetailedActionNameMethodInfo,
#endif
    actionableSetDetailedActionName         ,




 -- * Properties


-- ** actionName #attr:actionName#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ActionableActionNamePropertyInfo        ,
#endif
#if defined(ENABLE_OVERLOADING)
    actionableActionName                    ,
#endif
    clearActionableActionName               ,
    constructActionableActionName           ,
    getActionableActionName                 ,
    setActionableActionName                 ,


-- ** actionTarget #attr:actionTarget#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ActionableActionTargetPropertyInfo      ,
#endif
#if defined(ENABLE_OVERLOADING)
    actionableActionTarget                  ,
#endif
    clearActionableActionTarget             ,
    constructActionableActionTarget         ,
    getActionableActionTarget               ,
    setActionableActionTarget               ,




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

import qualified GI.GObject.Objects.Object as GObject.Object
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- interface Actionable 
-- | Memory-managed wrapper type.
newtype Actionable = Actionable (SP.ManagedPtr Actionable)
    deriving (Eq)

instance SP.ManagedPtrNewtype Actionable where
    toManagedPtr (Actionable p) = p

foreign import ccall "gtk_actionable_get_type"
    c_gtk_actionable_get_type :: IO B.Types.GType

instance B.Types.TypedObject Actionable where
    glibType = c_gtk_actionable_get_type

instance B.Types.GObject Actionable

-- | Type class for types which can be safely cast to `Actionable`, for instance with `toActionable`.
class (SP.GObject o, O.IsDescendantOf Actionable o) => IsActionable o
instance (SP.GObject o, O.IsDescendantOf Actionable o) => IsActionable o

instance O.HasParentTypes Actionable
type instance O.ParentTypes Actionable = '[GObject.Object.Object, Gtk.Widget.Widget]

-- | Cast to `Actionable`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toActionable :: (MIO.MonadIO m, IsActionable o) => o -> m Actionable
toActionable = MIO.liftIO . B.ManagedPtr.unsafeCastTo Actionable

-- | Convert 'Actionable' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe Actionable) where
    gvalueGType_ = c_gtk_actionable_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr Actionable)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr Actionable)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject Actionable ptr
        else return P.Nothing
        
    

-- VVV Prop "action-name"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just True)

-- | Get the value of the “@action-name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' actionable #actionName
-- @
getActionableActionName :: (MonadIO m, IsActionable o) => o -> m (Maybe T.Text)
getActionableActionName obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "action-name"

-- | Set the value of the “@action-name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' actionable [ #actionName 'Data.GI.Base.Attributes.:=' value ]
-- @
setActionableActionName :: (MonadIO m, IsActionable o) => o -> T.Text -> m ()
setActionableActionName obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "action-name" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@action-name@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructActionableActionName :: (IsActionable o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructActionableActionName val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "action-name" (P.Just val)

-- | Set the value of the “@action-name@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #actionName
-- @
clearActionableActionName :: (MonadIO m, IsActionable o) => o -> m ()
clearActionableActionName obj = liftIO $ B.Properties.setObjectPropertyString obj "action-name" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data ActionableActionNamePropertyInfo
instance AttrInfo ActionableActionNamePropertyInfo where
    type AttrAllowedOps ActionableActionNamePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint ActionableActionNamePropertyInfo = IsActionable
    type AttrSetTypeConstraint ActionableActionNamePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint ActionableActionNamePropertyInfo = (~) T.Text
    type AttrTransferType ActionableActionNamePropertyInfo = T.Text
    type AttrGetType ActionableActionNamePropertyInfo = (Maybe T.Text)
    type AttrLabel ActionableActionNamePropertyInfo = "action-name"
    type AttrOrigin ActionableActionNamePropertyInfo = Actionable
    attrGet = getActionableActionName
    attrSet = setActionableActionName
    attrTransfer _ v = do
        return v
    attrConstruct = constructActionableActionName
    attrClear = clearActionableActionName
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Actionable.actionName"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Actionable.html#g:attr:actionName"
        })
#endif

-- VVV Prop "action-target"
   -- Type: TVariant
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@action-target@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' actionable #actionTarget
-- @
getActionableActionTarget :: (MonadIO m, IsActionable o) => o -> m (Maybe GVariant)
getActionableActionTarget obj = MIO.liftIO $ B.Properties.getObjectPropertyVariant obj "action-target"

-- | Set the value of the “@action-target@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' actionable [ #actionTarget 'Data.GI.Base.Attributes.:=' value ]
-- @
setActionableActionTarget :: (MonadIO m, IsActionable o) => o -> GVariant -> m ()
setActionableActionTarget obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyVariant obj "action-target" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@action-target@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructActionableActionTarget :: (IsActionable o, MIO.MonadIO m) => GVariant -> m (GValueConstruct o)
constructActionableActionTarget val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyVariant "action-target" (P.Just val)

-- | Set the value of the “@action-target@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #actionTarget
-- @
clearActionableActionTarget :: (MonadIO m, IsActionable o) => o -> m ()
clearActionableActionTarget obj = liftIO $ B.Properties.setObjectPropertyVariant obj "action-target" (Nothing :: Maybe GVariant)

#if defined(ENABLE_OVERLOADING)
data ActionableActionTargetPropertyInfo
instance AttrInfo ActionableActionTargetPropertyInfo where
    type AttrAllowedOps ActionableActionTargetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint ActionableActionTargetPropertyInfo = IsActionable
    type AttrSetTypeConstraint ActionableActionTargetPropertyInfo = (~) GVariant
    type AttrTransferTypeConstraint ActionableActionTargetPropertyInfo = (~) GVariant
    type AttrTransferType ActionableActionTargetPropertyInfo = GVariant
    type AttrGetType ActionableActionTargetPropertyInfo = (Maybe GVariant)
    type AttrLabel ActionableActionTargetPropertyInfo = "action-target"
    type AttrOrigin ActionableActionTargetPropertyInfo = Actionable
    attrGet = getActionableActionTarget
    attrSet = setActionableActionTarget
    attrTransfer _ v = do
        return v
    attrConstruct = constructActionableActionTarget
    attrClear = clearActionableActionTarget
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Actionable.actionTarget"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Actionable.html#g:attr:actionTarget"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList Actionable
type instance O.AttributeList Actionable = ActionableAttributeList
type ActionableAttributeList = ('[ '("actionName", ActionableActionNamePropertyInfo), '("actionTarget", ActionableActionTargetPropertyInfo), '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
actionableActionName :: AttrLabelProxy "actionName"
actionableActionName = AttrLabelProxy

actionableActionTarget :: AttrLabelProxy "actionTarget"
actionableActionTarget = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveActionableMethod (t :: Symbol) (o :: *) :: * where
    ResolveActionableMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveActionableMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveActionableMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveActionableMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveActionableMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveActionableMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveActionableMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveActionableMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveActionableMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveActionableMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveActionableMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveActionableMethod "childNotify" o = Gtk.Widget.WidgetChildNotifyMethodInfo
    ResolveActionableMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveActionableMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveActionableMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveActionableMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveActionableMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveActionableMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveActionableMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveActionableMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveActionableMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveActionableMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveActionableMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveActionableMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveActionableMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveActionableMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveActionableMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveActionableMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveActionableMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveActionableMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveActionableMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveActionableMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveActionableMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveActionableMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveActionableMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveActionableMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveActionableMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveActionableMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveActionableMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveActionableMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveActionableMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveActionableMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveActionableMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveActionableMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveActionableMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveActionableMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveActionableMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveActionableMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveActionableMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveActionableMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveActionableMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveActionableMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveActionableMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveActionableMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveActionableMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveActionableMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveActionableMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveActionableMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveActionableMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveActionableMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveActionableMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveActionableMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveActionableMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveActionableMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveActionableMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveActionableMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveActionableMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveActionableMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveActionableMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveActionableMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveActionableMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveActionableMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveActionableMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveActionableMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveActionableMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveActionableMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveActionableMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveActionableMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveActionableMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveActionableMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveActionableMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveActionableMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveActionableMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveActionableMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveActionableMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveActionableMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveActionableMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveActionableMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveActionableMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveActionableMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveActionableMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveActionableMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveActionableMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveActionableMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveActionableMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveActionableMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveActionableMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveActionableMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveActionableMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveActionableMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveActionableMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveActionableMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveActionableMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveActionableMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveActionableMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveActionableMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveActionableMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveActionableMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveActionableMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveActionableMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveActionableMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveActionableMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveActionableMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveActionableMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveActionableMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveActionableMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveActionableMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveActionableMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveActionableMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveActionableMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveActionableMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveActionableMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveActionableMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveActionableMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveActionableMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveActionableMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveActionableMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveActionableMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveActionableMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveActionableMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveActionableMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveActionableMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveActionableMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveActionableMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveActionableMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveActionableMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveActionableMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveActionableMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveActionableMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveActionableMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveActionableMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveActionableMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveActionableMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveActionableMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveActionableMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveActionableMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveActionableMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveActionableMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveActionableMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveActionableMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveActionableMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveActionableMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveActionableMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveActionableMethod "getActionName" o = ActionableGetActionNameMethodInfo
    ResolveActionableMethod "getActionTargetValue" o = ActionableGetActionTargetValueMethodInfo
    ResolveActionableMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveActionableMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveActionableMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveActionableMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveActionableMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveActionableMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveActionableMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveActionableMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveActionableMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveActionableMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveActionableMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveActionableMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveActionableMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveActionableMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveActionableMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveActionableMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveActionableMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveActionableMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveActionableMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveActionableMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveActionableMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveActionableMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveActionableMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveActionableMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveActionableMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveActionableMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveActionableMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveActionableMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveActionableMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveActionableMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveActionableMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveActionableMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveActionableMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveActionableMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveActionableMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveActionableMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveActionableMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveActionableMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveActionableMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveActionableMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveActionableMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveActionableMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveActionableMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveActionableMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveActionableMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveActionableMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveActionableMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveActionableMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveActionableMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveActionableMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveActionableMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveActionableMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveActionableMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveActionableMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveActionableMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveActionableMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveActionableMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveActionableMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveActionableMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveActionableMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveActionableMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveActionableMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveActionableMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveActionableMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveActionableMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveActionableMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveActionableMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveActionableMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveActionableMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveActionableMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveActionableMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveActionableMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveActionableMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveActionableMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveActionableMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveActionableMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveActionableMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveActionableMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveActionableMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveActionableMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveActionableMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveActionableMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveActionableMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveActionableMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveActionableMethod "setActionName" o = ActionableSetActionNameMethodInfo
    ResolveActionableMethod "setActionTargetValue" o = ActionableSetActionTargetValueMethodInfo
    ResolveActionableMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveActionableMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveActionableMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveActionableMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveActionableMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveActionableMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveActionableMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveActionableMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveActionableMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveActionableMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveActionableMethod "setDetailedActionName" o = ActionableSetDetailedActionNameMethodInfo
    ResolveActionableMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveActionableMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveActionableMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveActionableMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveActionableMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveActionableMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveActionableMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveActionableMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveActionableMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveActionableMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveActionableMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveActionableMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveActionableMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveActionableMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveActionableMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveActionableMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveActionableMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveActionableMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveActionableMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveActionableMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveActionableMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveActionableMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveActionableMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveActionableMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveActionableMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveActionableMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveActionableMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveActionableMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveActionableMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveActionableMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveActionableMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveActionableMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveActionableMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveActionableMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveActionableMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveActionableMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveActionableMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveActionableMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveActionableMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveActionableMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveActionableMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveActionableMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveActionableMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveActionableMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveActionableMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveActionableMethod t Actionable, O.OverloadedMethod info Actionable p) => OL.IsLabel t (Actionable -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveActionableMethod t Actionable, O.OverloadedMethod info Actionable p, R.HasField t Actionable p) => R.HasField t Actionable p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveActionableMethod t Actionable, O.OverloadedMethodInfo info Actionable) => OL.IsLabel t (O.MethodProxy info Actionable) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- method Actionable::get_action_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "actionable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Actionable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkActionable widget"
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

foreign import ccall "gtk_actionable_get_action_name" gtk_actionable_get_action_name :: 
    Ptr Actionable ->                       -- actionable : TInterface (Name {namespace = "Gtk", name = "Actionable"})
    IO CString

-- | Gets the action name for /@actionable@/.
-- 
-- See 'GI.Gtk.Interfaces.Actionable.actionableSetActionName' for more information.
-- 
-- /Since: 3.4/
actionableGetActionName ::
    (B.CallStack.HasCallStack, MonadIO m, IsActionable a) =>
    a
    -- ^ /@actionable@/: a t'GI.Gtk.Interfaces.Actionable.Actionable' widget
    -> m (Maybe T.Text)
    -- ^ __Returns:__ the action name, or 'P.Nothing' if none is set
actionableGetActionName actionable = liftIO $ do
    actionable' <- unsafeManagedPtrCastPtr actionable
    result <- gtk_actionable_get_action_name actionable'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToText result'
        return result''
    touchManagedPtr actionable
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data ActionableGetActionNameMethodInfo
instance (signature ~ (m (Maybe T.Text)), MonadIO m, IsActionable a) => O.OverloadedMethod ActionableGetActionNameMethodInfo a signature where
    overloadedMethod = actionableGetActionName

instance O.OverloadedMethodInfo ActionableGetActionNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Actionable.actionableGetActionName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Actionable.html#v:actionableGetActionName"
        })


#endif

-- method Actionable::get_action_target_value
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "actionable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Actionable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkActionable widget"
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
-- returnType: Just TVariant
-- throws : False
-- Skip return : False

foreign import ccall "gtk_actionable_get_action_target_value" gtk_actionable_get_action_target_value :: 
    Ptr Actionable ->                       -- actionable : TInterface (Name {namespace = "Gtk", name = "Actionable"})
    IO (Ptr GVariant)

-- | Gets the current target value of /@actionable@/.
-- 
-- See 'GI.Gtk.Interfaces.Actionable.actionableSetActionTargetValue' for more information.
-- 
-- /Since: 3.4/
actionableGetActionTargetValue ::
    (B.CallStack.HasCallStack, MonadIO m, IsActionable a) =>
    a
    -- ^ /@actionable@/: a t'GI.Gtk.Interfaces.Actionable.Actionable' widget
    -> m GVariant
    -- ^ __Returns:__ the current target value
actionableGetActionTargetValue actionable = liftIO $ do
    actionable' <- unsafeManagedPtrCastPtr actionable
    result <- gtk_actionable_get_action_target_value actionable'
    checkUnexpectedReturnNULL "actionableGetActionTargetValue" result
    result' <- B.GVariant.newGVariantFromPtr result
    touchManagedPtr actionable
    return result'

#if defined(ENABLE_OVERLOADING)
data ActionableGetActionTargetValueMethodInfo
instance (signature ~ (m GVariant), MonadIO m, IsActionable a) => O.OverloadedMethod ActionableGetActionTargetValueMethodInfo a signature where
    overloadedMethod = actionableGetActionTargetValue

instance O.OverloadedMethodInfo ActionableGetActionTargetValueMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Actionable.actionableGetActionTargetValue",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Actionable.html#v:actionableGetActionTargetValue"
        })


#endif

-- method Actionable::set_action_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "actionable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Actionable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkActionable widget"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "action_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an action name, or %NULL"
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

foreign import ccall "gtk_actionable_set_action_name" gtk_actionable_set_action_name :: 
    Ptr Actionable ->                       -- actionable : TInterface (Name {namespace = "Gtk", name = "Actionable"})
    CString ->                              -- action_name : TBasicType TUTF8
    IO ()

-- | Specifies the name of the action with which this widget should be
-- associated.  If /@actionName@/ is 'P.Nothing' then the widget will be
-- unassociated from any previous action.
-- 
-- Usually this function is used when the widget is located (or will be
-- located) within the hierarchy of a t'GI.Gtk.Objects.ApplicationWindow.ApplicationWindow'.
-- 
-- Names are of the form “win.save” or “app.quit” for actions on the
-- containing t'GI.Gtk.Objects.ApplicationWindow.ApplicationWindow' or its associated t'GI.Gtk.Objects.Application.Application',
-- respectively.  This is the same form used for actions in the t'GI.Gio.Objects.Menu.Menu'
-- associated with the window.
-- 
-- /Since: 3.4/
actionableSetActionName ::
    (B.CallStack.HasCallStack, MonadIO m, IsActionable a) =>
    a
    -- ^ /@actionable@/: a t'GI.Gtk.Interfaces.Actionable.Actionable' widget
    -> Maybe (T.Text)
    -- ^ /@actionName@/: an action name, or 'P.Nothing'
    -> m ()
actionableSetActionName actionable actionName = liftIO $ do
    actionable' <- unsafeManagedPtrCastPtr actionable
    maybeActionName <- case actionName of
        Nothing -> return nullPtr
        Just jActionName -> do
            jActionName' <- textToCString jActionName
            return jActionName'
    gtk_actionable_set_action_name actionable' maybeActionName
    touchManagedPtr actionable
    freeMem maybeActionName
    return ()

#if defined(ENABLE_OVERLOADING)
data ActionableSetActionNameMethodInfo
instance (signature ~ (Maybe (T.Text) -> m ()), MonadIO m, IsActionable a) => O.OverloadedMethod ActionableSetActionNameMethodInfo a signature where
    overloadedMethod = actionableSetActionName

instance O.OverloadedMethodInfo ActionableSetActionNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Actionable.actionableSetActionName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Actionable.html#v:actionableSetActionName"
        })


#endif

-- method Actionable::set_action_target_value
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "actionable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Actionable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkActionable widget"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "target_value"
--           , argType = TVariant
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GVariant to set as the target value, or %NULL"
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

foreign import ccall "gtk_actionable_set_action_target_value" gtk_actionable_set_action_target_value :: 
    Ptr Actionable ->                       -- actionable : TInterface (Name {namespace = "Gtk", name = "Actionable"})
    Ptr GVariant ->                         -- target_value : TVariant
    IO ()

-- | Sets the target value of an actionable widget.
-- 
-- If /@targetValue@/ is 'P.Nothing' then the target value is unset.
-- 
-- The target value has two purposes.  First, it is used as the
-- parameter to activation of the action associated with the
-- t'GI.Gtk.Interfaces.Actionable.Actionable' widget. Second, it is used to determine if the widget
-- should be rendered as “active” — the widget is active if the state
-- is equal to the given target.
-- 
-- Consider the example of associating a set of buttons with a t'GI.Gio.Interfaces.Action.Action'
-- with string state in a typical “radio button” situation.  Each button
-- will be associated with the same action, but with a different target
-- value for that action.  Clicking on a particular button will activate
-- the action with the target of that button, which will typically cause
-- the action’s state to change to that value.  Since the action’s state
-- is now equal to the target value of the button, the button will now
-- be rendered as active (and the other buttons, with different targets,
-- rendered inactive).
-- 
-- /Since: 3.4/
actionableSetActionTargetValue ::
    (B.CallStack.HasCallStack, MonadIO m, IsActionable a) =>
    a
    -- ^ /@actionable@/: a t'GI.Gtk.Interfaces.Actionable.Actionable' widget
    -> Maybe (GVariant)
    -- ^ /@targetValue@/: a t'GVariant' to set as the target value, or 'P.Nothing'
    -> m ()
actionableSetActionTargetValue actionable targetValue = liftIO $ do
    actionable' <- unsafeManagedPtrCastPtr actionable
    maybeTargetValue <- case targetValue of
        Nothing -> return nullPtr
        Just jTargetValue -> do
            jTargetValue' <- unsafeManagedPtrGetPtr jTargetValue
            return jTargetValue'
    gtk_actionable_set_action_target_value actionable' maybeTargetValue
    touchManagedPtr actionable
    whenJust targetValue touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data ActionableSetActionTargetValueMethodInfo
instance (signature ~ (Maybe (GVariant) -> m ()), MonadIO m, IsActionable a) => O.OverloadedMethod ActionableSetActionTargetValueMethodInfo a signature where
    overloadedMethod = actionableSetActionTargetValue

instance O.OverloadedMethodInfo ActionableSetActionTargetValueMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Actionable.actionableSetActionTargetValue",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Actionable.html#v:actionableSetActionTargetValue"
        })


#endif

-- method Actionable::set_detailed_action_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "actionable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Actionable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkActionable widget"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "detailed_action_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the detailed action name"
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

foreign import ccall "gtk_actionable_set_detailed_action_name" gtk_actionable_set_detailed_action_name :: 
    Ptr Actionable ->                       -- actionable : TInterface (Name {namespace = "Gtk", name = "Actionable"})
    CString ->                              -- detailed_action_name : TBasicType TUTF8
    IO ()

-- | Sets the action-name and associated string target value of an
-- actionable widget.
-- 
-- /@detailedActionName@/ is a string in the format accepted by
-- 'GI.Gio.Functions.actionParseDetailedName'.
-- 
-- (Note that prior to version 3.22.25,
-- this function is only usable for actions with a simple \"s\" target, and
-- /@detailedActionName@/ must be of the form @\"action::target\"@ where
-- @action@ is the action name and @target@ is the string to use
-- as the target.)
-- 
-- /Since: 3.4/
actionableSetDetailedActionName ::
    (B.CallStack.HasCallStack, MonadIO m, IsActionable a) =>
    a
    -- ^ /@actionable@/: a t'GI.Gtk.Interfaces.Actionable.Actionable' widget
    -> T.Text
    -- ^ /@detailedActionName@/: the detailed action name
    -> m ()
actionableSetDetailedActionName actionable detailedActionName = liftIO $ do
    actionable' <- unsafeManagedPtrCastPtr actionable
    detailedActionName' <- textToCString detailedActionName
    gtk_actionable_set_detailed_action_name actionable' detailedActionName'
    touchManagedPtr actionable
    freeMem detailedActionName'
    return ()

#if defined(ENABLE_OVERLOADING)
data ActionableSetDetailedActionNameMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsActionable a) => O.OverloadedMethod ActionableSetDetailedActionNameMethodInfo a signature where
    overloadedMethod = actionableSetDetailedActionName

instance O.OverloadedMethodInfo ActionableSetDetailedActionNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Actionable.actionableSetDetailedActionName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Actionable.html#v:actionableSetDetailedActionName"
        })


#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList Actionable = ActionableSignalList
type ActionableSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif


