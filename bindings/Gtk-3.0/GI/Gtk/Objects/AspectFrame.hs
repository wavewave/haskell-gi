{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- The t'GI.Gtk.Objects.AspectFrame.AspectFrame' is useful when you want
-- pack a widget so that it can resize but always retains
-- the same aspect ratio. For instance, one might be
-- drawing a small preview of a larger image. t'GI.Gtk.Objects.AspectFrame.AspectFrame'
-- derives from t'GI.Gtk.Objects.Frame.Frame', so it can draw a label and
-- a frame around the child. The frame will be
-- “shrink-wrapped” to the size of the child.
-- 
-- = CSS nodes
-- 
-- GtkAspectFrame uses a CSS node with name frame.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.AspectFrame
    ( 

-- * Exported types
    AspectFrame(..)                         ,
    IsAspectFrame                           ,
    toAspectFrame                           ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [set]("GI.Gtk.Objects.AspectFrame#g:method:set"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChild]("GI.Gtk.Objects.Bin#g:method:getChild"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getLabel]("GI.Gtk.Objects.Frame#g:method:getLabel"), [getLabelAlign]("GI.Gtk.Objects.Frame#g:method:getLabelAlign"), [getLabelWidget]("GI.Gtk.Objects.Frame#g:method:getLabelWidget"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getShadowType]("GI.Gtk.Objects.Frame#g:method:getShadowType"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setLabel]("GI.Gtk.Objects.Frame#g:method:setLabel"), [setLabelAlign]("GI.Gtk.Objects.Frame#g:method:setLabelAlign"), [setLabelWidget]("GI.Gtk.Objects.Frame#g:method:setLabelWidget"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setShadowType]("GI.Gtk.Objects.Frame#g:method:setShadowType"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveAspectFrameMethod                ,
#endif

-- ** new #method:new#

    aspectFrameNew                          ,


-- ** set #method:set#

#if defined(ENABLE_OVERLOADING)
    AspectFrameSetMethodInfo                ,
#endif
    aspectFrameSet                          ,




 -- * Properties


-- ** obeyChild #attr:obeyChild#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    AspectFrameObeyChildPropertyInfo        ,
#endif
#if defined(ENABLE_OVERLOADING)
    aspectFrameObeyChild                    ,
#endif
    constructAspectFrameObeyChild           ,
    getAspectFrameObeyChild                 ,
    setAspectFrameObeyChild                 ,


-- ** ratio #attr:ratio#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    AspectFrameRatioPropertyInfo            ,
#endif
#if defined(ENABLE_OVERLOADING)
    aspectFrameRatio                        ,
#endif
    constructAspectFrameRatio               ,
    getAspectFrameRatio                     ,
    setAspectFrameRatio                     ,


-- ** xalign #attr:xalign#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    AspectFrameXalignPropertyInfo           ,
#endif
#if defined(ENABLE_OVERLOADING)
    aspectFrameXalign                       ,
#endif
    constructAspectFrameXalign              ,
    getAspectFrameXalign                    ,
    setAspectFrameXalign                    ,


-- ** yalign #attr:yalign#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    AspectFrameYalignPropertyInfo           ,
#endif
#if defined(ENABLE_OVERLOADING)
    aspectFrameYalign                       ,
#endif
    constructAspectFrameYalign              ,
    getAspectFrameYalign                    ,
    setAspectFrameYalign                    ,




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
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Objects.Bin as Gtk.Bin
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.Frame as Gtk.Frame
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype AspectFrame = AspectFrame (SP.ManagedPtr AspectFrame)
    deriving (Eq)

instance SP.ManagedPtrNewtype AspectFrame where
    toManagedPtr (AspectFrame p) = p

foreign import ccall "gtk_aspect_frame_get_type"
    c_gtk_aspect_frame_get_type :: IO B.Types.GType

instance B.Types.TypedObject AspectFrame where
    glibType = c_gtk_aspect_frame_get_type

instance B.Types.GObject AspectFrame

-- | Type class for types which can be safely cast to `AspectFrame`, for instance with `toAspectFrame`.
class (SP.GObject o, O.IsDescendantOf AspectFrame o) => IsAspectFrame o
instance (SP.GObject o, O.IsDescendantOf AspectFrame o) => IsAspectFrame o

instance O.HasParentTypes AspectFrame
type instance O.ParentTypes AspectFrame = '[Gtk.Frame.Frame, Gtk.Bin.Bin, Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable]

-- | Cast to `AspectFrame`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toAspectFrame :: (MIO.MonadIO m, IsAspectFrame o) => o -> m AspectFrame
toAspectFrame = MIO.liftIO . B.ManagedPtr.unsafeCastTo AspectFrame

-- | Convert 'AspectFrame' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe AspectFrame) where
    gvalueGType_ = c_gtk_aspect_frame_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr AspectFrame)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr AspectFrame)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject AspectFrame ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveAspectFrameMethod (t :: Symbol) (o :: *) :: * where
    ResolveAspectFrameMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveAspectFrameMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveAspectFrameMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveAspectFrameMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveAspectFrameMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveAspectFrameMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveAspectFrameMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveAspectFrameMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveAspectFrameMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveAspectFrameMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveAspectFrameMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveAspectFrameMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveAspectFrameMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveAspectFrameMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveAspectFrameMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveAspectFrameMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveAspectFrameMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveAspectFrameMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveAspectFrameMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveAspectFrameMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveAspectFrameMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveAspectFrameMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveAspectFrameMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveAspectFrameMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveAspectFrameMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveAspectFrameMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveAspectFrameMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveAspectFrameMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveAspectFrameMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveAspectFrameMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveAspectFrameMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveAspectFrameMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveAspectFrameMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveAspectFrameMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveAspectFrameMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveAspectFrameMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveAspectFrameMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveAspectFrameMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveAspectFrameMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveAspectFrameMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveAspectFrameMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveAspectFrameMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveAspectFrameMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveAspectFrameMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveAspectFrameMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveAspectFrameMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveAspectFrameMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveAspectFrameMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveAspectFrameMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveAspectFrameMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveAspectFrameMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveAspectFrameMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveAspectFrameMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveAspectFrameMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveAspectFrameMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveAspectFrameMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveAspectFrameMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveAspectFrameMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveAspectFrameMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveAspectFrameMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveAspectFrameMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveAspectFrameMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveAspectFrameMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveAspectFrameMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveAspectFrameMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveAspectFrameMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveAspectFrameMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveAspectFrameMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveAspectFrameMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveAspectFrameMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveAspectFrameMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveAspectFrameMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveAspectFrameMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveAspectFrameMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveAspectFrameMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveAspectFrameMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveAspectFrameMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveAspectFrameMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveAspectFrameMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveAspectFrameMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveAspectFrameMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveAspectFrameMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveAspectFrameMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveAspectFrameMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveAspectFrameMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveAspectFrameMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveAspectFrameMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveAspectFrameMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveAspectFrameMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveAspectFrameMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveAspectFrameMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveAspectFrameMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveAspectFrameMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveAspectFrameMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveAspectFrameMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveAspectFrameMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveAspectFrameMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveAspectFrameMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveAspectFrameMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveAspectFrameMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveAspectFrameMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveAspectFrameMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveAspectFrameMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveAspectFrameMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveAspectFrameMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveAspectFrameMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveAspectFrameMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveAspectFrameMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveAspectFrameMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveAspectFrameMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveAspectFrameMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveAspectFrameMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveAspectFrameMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveAspectFrameMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveAspectFrameMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveAspectFrameMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveAspectFrameMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveAspectFrameMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveAspectFrameMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveAspectFrameMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveAspectFrameMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveAspectFrameMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveAspectFrameMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveAspectFrameMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveAspectFrameMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveAspectFrameMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveAspectFrameMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveAspectFrameMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolveAspectFrameMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveAspectFrameMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveAspectFrameMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveAspectFrameMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveAspectFrameMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveAspectFrameMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveAspectFrameMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveAspectFrameMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveAspectFrameMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveAspectFrameMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveAspectFrameMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveAspectFrameMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveAspectFrameMethod "set" o = AspectFrameSetMethodInfo
    ResolveAspectFrameMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveAspectFrameMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveAspectFrameMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveAspectFrameMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveAspectFrameMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveAspectFrameMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveAspectFrameMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveAspectFrameMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveAspectFrameMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveAspectFrameMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveAspectFrameMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveAspectFrameMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveAspectFrameMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveAspectFrameMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveAspectFrameMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveAspectFrameMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveAspectFrameMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveAspectFrameMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveAspectFrameMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveAspectFrameMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveAspectFrameMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveAspectFrameMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveAspectFrameMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveAspectFrameMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveAspectFrameMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveAspectFrameMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveAspectFrameMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveAspectFrameMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveAspectFrameMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveAspectFrameMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveAspectFrameMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveAspectFrameMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveAspectFrameMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveAspectFrameMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveAspectFrameMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveAspectFrameMethod "getChild" o = Gtk.Bin.BinGetChildMethodInfo
    ResolveAspectFrameMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveAspectFrameMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveAspectFrameMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveAspectFrameMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveAspectFrameMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveAspectFrameMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveAspectFrameMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveAspectFrameMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveAspectFrameMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveAspectFrameMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveAspectFrameMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveAspectFrameMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveAspectFrameMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveAspectFrameMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveAspectFrameMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveAspectFrameMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveAspectFrameMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveAspectFrameMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveAspectFrameMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveAspectFrameMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveAspectFrameMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveAspectFrameMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveAspectFrameMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveAspectFrameMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveAspectFrameMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveAspectFrameMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveAspectFrameMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveAspectFrameMethod "getLabel" o = Gtk.Frame.FrameGetLabelMethodInfo
    ResolveAspectFrameMethod "getLabelAlign" o = Gtk.Frame.FrameGetLabelAlignMethodInfo
    ResolveAspectFrameMethod "getLabelWidget" o = Gtk.Frame.FrameGetLabelWidgetMethodInfo
    ResolveAspectFrameMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveAspectFrameMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveAspectFrameMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveAspectFrameMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveAspectFrameMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveAspectFrameMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveAspectFrameMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveAspectFrameMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveAspectFrameMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveAspectFrameMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveAspectFrameMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveAspectFrameMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveAspectFrameMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveAspectFrameMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveAspectFrameMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveAspectFrameMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveAspectFrameMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveAspectFrameMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveAspectFrameMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveAspectFrameMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveAspectFrameMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveAspectFrameMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveAspectFrameMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveAspectFrameMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveAspectFrameMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveAspectFrameMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveAspectFrameMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveAspectFrameMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveAspectFrameMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveAspectFrameMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveAspectFrameMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveAspectFrameMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveAspectFrameMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveAspectFrameMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveAspectFrameMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveAspectFrameMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveAspectFrameMethod "getShadowType" o = Gtk.Frame.FrameGetShadowTypeMethodInfo
    ResolveAspectFrameMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveAspectFrameMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveAspectFrameMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveAspectFrameMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveAspectFrameMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveAspectFrameMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveAspectFrameMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveAspectFrameMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveAspectFrameMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveAspectFrameMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveAspectFrameMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveAspectFrameMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveAspectFrameMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveAspectFrameMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveAspectFrameMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveAspectFrameMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveAspectFrameMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveAspectFrameMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveAspectFrameMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveAspectFrameMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveAspectFrameMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveAspectFrameMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveAspectFrameMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveAspectFrameMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveAspectFrameMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveAspectFrameMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveAspectFrameMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveAspectFrameMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveAspectFrameMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveAspectFrameMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveAspectFrameMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveAspectFrameMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveAspectFrameMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveAspectFrameMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveAspectFrameMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveAspectFrameMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveAspectFrameMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveAspectFrameMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveAspectFrameMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveAspectFrameMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveAspectFrameMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveAspectFrameMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveAspectFrameMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveAspectFrameMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveAspectFrameMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveAspectFrameMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveAspectFrameMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveAspectFrameMethod "setLabel" o = Gtk.Frame.FrameSetLabelMethodInfo
    ResolveAspectFrameMethod "setLabelAlign" o = Gtk.Frame.FrameSetLabelAlignMethodInfo
    ResolveAspectFrameMethod "setLabelWidget" o = Gtk.Frame.FrameSetLabelWidgetMethodInfo
    ResolveAspectFrameMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveAspectFrameMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveAspectFrameMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveAspectFrameMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveAspectFrameMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveAspectFrameMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveAspectFrameMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveAspectFrameMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveAspectFrameMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveAspectFrameMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveAspectFrameMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveAspectFrameMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveAspectFrameMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveAspectFrameMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveAspectFrameMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveAspectFrameMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveAspectFrameMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveAspectFrameMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveAspectFrameMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveAspectFrameMethod "setShadowType" o = Gtk.Frame.FrameSetShadowTypeMethodInfo
    ResolveAspectFrameMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveAspectFrameMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveAspectFrameMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveAspectFrameMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveAspectFrameMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveAspectFrameMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveAspectFrameMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveAspectFrameMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveAspectFrameMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveAspectFrameMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveAspectFrameMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveAspectFrameMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveAspectFrameMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveAspectFrameMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveAspectFrameMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveAspectFrameMethod t AspectFrame, O.OverloadedMethod info AspectFrame p) => OL.IsLabel t (AspectFrame -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveAspectFrameMethod t AspectFrame, O.OverloadedMethod info AspectFrame p, R.HasField t AspectFrame p) => R.HasField t AspectFrame p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveAspectFrameMethod t AspectFrame, O.OverloadedMethodInfo info AspectFrame) => OL.IsLabel t (O.MethodProxy info AspectFrame) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- VVV Prop "obey-child"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@obey-child@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' aspectFrame #obeyChild
-- @
getAspectFrameObeyChild :: (MonadIO m, IsAspectFrame o) => o -> m Bool
getAspectFrameObeyChild obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "obey-child"

-- | Set the value of the “@obey-child@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' aspectFrame [ #obeyChild 'Data.GI.Base.Attributes.:=' value ]
-- @
setAspectFrameObeyChild :: (MonadIO m, IsAspectFrame o) => o -> Bool -> m ()
setAspectFrameObeyChild obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "obey-child" val

-- | Construct a `GValueConstruct` with valid value for the “@obey-child@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructAspectFrameObeyChild :: (IsAspectFrame o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructAspectFrameObeyChild val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "obey-child" val

#if defined(ENABLE_OVERLOADING)
data AspectFrameObeyChildPropertyInfo
instance AttrInfo AspectFrameObeyChildPropertyInfo where
    type AttrAllowedOps AspectFrameObeyChildPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint AspectFrameObeyChildPropertyInfo = IsAspectFrame
    type AttrSetTypeConstraint AspectFrameObeyChildPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint AspectFrameObeyChildPropertyInfo = (~) Bool
    type AttrTransferType AspectFrameObeyChildPropertyInfo = Bool
    type AttrGetType AspectFrameObeyChildPropertyInfo = Bool
    type AttrLabel AspectFrameObeyChildPropertyInfo = "obey-child"
    type AttrOrigin AspectFrameObeyChildPropertyInfo = AspectFrame
    attrGet = getAspectFrameObeyChild
    attrSet = setAspectFrameObeyChild
    attrTransfer _ v = do
        return v
    attrConstruct = constructAspectFrameObeyChild
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AspectFrame.obeyChild"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AspectFrame.html#g:attr:obeyChild"
        })
#endif

-- VVV Prop "ratio"
   -- Type: TBasicType TFloat
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@ratio@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' aspectFrame #ratio
-- @
getAspectFrameRatio :: (MonadIO m, IsAspectFrame o) => o -> m Float
getAspectFrameRatio obj = MIO.liftIO $ B.Properties.getObjectPropertyFloat obj "ratio"

-- | Set the value of the “@ratio@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' aspectFrame [ #ratio 'Data.GI.Base.Attributes.:=' value ]
-- @
setAspectFrameRatio :: (MonadIO m, IsAspectFrame o) => o -> Float -> m ()
setAspectFrameRatio obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyFloat obj "ratio" val

-- | Construct a `GValueConstruct` with valid value for the “@ratio@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructAspectFrameRatio :: (IsAspectFrame o, MIO.MonadIO m) => Float -> m (GValueConstruct o)
constructAspectFrameRatio val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyFloat "ratio" val

#if defined(ENABLE_OVERLOADING)
data AspectFrameRatioPropertyInfo
instance AttrInfo AspectFrameRatioPropertyInfo where
    type AttrAllowedOps AspectFrameRatioPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint AspectFrameRatioPropertyInfo = IsAspectFrame
    type AttrSetTypeConstraint AspectFrameRatioPropertyInfo = (~) Float
    type AttrTransferTypeConstraint AspectFrameRatioPropertyInfo = (~) Float
    type AttrTransferType AspectFrameRatioPropertyInfo = Float
    type AttrGetType AspectFrameRatioPropertyInfo = Float
    type AttrLabel AspectFrameRatioPropertyInfo = "ratio"
    type AttrOrigin AspectFrameRatioPropertyInfo = AspectFrame
    attrGet = getAspectFrameRatio
    attrSet = setAspectFrameRatio
    attrTransfer _ v = do
        return v
    attrConstruct = constructAspectFrameRatio
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AspectFrame.ratio"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AspectFrame.html#g:attr:ratio"
        })
#endif

-- VVV Prop "xalign"
   -- Type: TBasicType TFloat
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@xalign@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' aspectFrame #xalign
-- @
getAspectFrameXalign :: (MonadIO m, IsAspectFrame o) => o -> m Float
getAspectFrameXalign obj = MIO.liftIO $ B.Properties.getObjectPropertyFloat obj "xalign"

-- | Set the value of the “@xalign@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' aspectFrame [ #xalign 'Data.GI.Base.Attributes.:=' value ]
-- @
setAspectFrameXalign :: (MonadIO m, IsAspectFrame o) => o -> Float -> m ()
setAspectFrameXalign obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyFloat obj "xalign" val

-- | Construct a `GValueConstruct` with valid value for the “@xalign@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructAspectFrameXalign :: (IsAspectFrame o, MIO.MonadIO m) => Float -> m (GValueConstruct o)
constructAspectFrameXalign val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyFloat "xalign" val

#if defined(ENABLE_OVERLOADING)
data AspectFrameXalignPropertyInfo
instance AttrInfo AspectFrameXalignPropertyInfo where
    type AttrAllowedOps AspectFrameXalignPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint AspectFrameXalignPropertyInfo = IsAspectFrame
    type AttrSetTypeConstraint AspectFrameXalignPropertyInfo = (~) Float
    type AttrTransferTypeConstraint AspectFrameXalignPropertyInfo = (~) Float
    type AttrTransferType AspectFrameXalignPropertyInfo = Float
    type AttrGetType AspectFrameXalignPropertyInfo = Float
    type AttrLabel AspectFrameXalignPropertyInfo = "xalign"
    type AttrOrigin AspectFrameXalignPropertyInfo = AspectFrame
    attrGet = getAspectFrameXalign
    attrSet = setAspectFrameXalign
    attrTransfer _ v = do
        return v
    attrConstruct = constructAspectFrameXalign
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AspectFrame.xalign"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AspectFrame.html#g:attr:xalign"
        })
#endif

-- VVV Prop "yalign"
   -- Type: TBasicType TFloat
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@yalign@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' aspectFrame #yalign
-- @
getAspectFrameYalign :: (MonadIO m, IsAspectFrame o) => o -> m Float
getAspectFrameYalign obj = MIO.liftIO $ B.Properties.getObjectPropertyFloat obj "yalign"

-- | Set the value of the “@yalign@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' aspectFrame [ #yalign 'Data.GI.Base.Attributes.:=' value ]
-- @
setAspectFrameYalign :: (MonadIO m, IsAspectFrame o) => o -> Float -> m ()
setAspectFrameYalign obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyFloat obj "yalign" val

-- | Construct a `GValueConstruct` with valid value for the “@yalign@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructAspectFrameYalign :: (IsAspectFrame o, MIO.MonadIO m) => Float -> m (GValueConstruct o)
constructAspectFrameYalign val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyFloat "yalign" val

#if defined(ENABLE_OVERLOADING)
data AspectFrameYalignPropertyInfo
instance AttrInfo AspectFrameYalignPropertyInfo where
    type AttrAllowedOps AspectFrameYalignPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint AspectFrameYalignPropertyInfo = IsAspectFrame
    type AttrSetTypeConstraint AspectFrameYalignPropertyInfo = (~) Float
    type AttrTransferTypeConstraint AspectFrameYalignPropertyInfo = (~) Float
    type AttrTransferType AspectFrameYalignPropertyInfo = Float
    type AttrGetType AspectFrameYalignPropertyInfo = Float
    type AttrLabel AspectFrameYalignPropertyInfo = "yalign"
    type AttrOrigin AspectFrameYalignPropertyInfo = AspectFrame
    attrGet = getAspectFrameYalign
    attrSet = setAspectFrameYalign
    attrTransfer _ v = do
        return v
    attrConstruct = constructAspectFrameYalign
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AspectFrame.yalign"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AspectFrame.html#g:attr:yalign"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList AspectFrame
type instance O.AttributeList AspectFrame = AspectFrameAttributeList
type AspectFrameAttributeList = ('[ '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("label", Gtk.Frame.FrameLabelPropertyInfo), '("labelWidget", Gtk.Frame.FrameLabelWidgetPropertyInfo), '("labelXalign", Gtk.Frame.FrameLabelXalignPropertyInfo), '("labelYalign", Gtk.Frame.FrameLabelYalignPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("obeyChild", AspectFrameObeyChildPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("ratio", AspectFrameRatioPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("shadowType", Gtk.Frame.FrameShadowTypePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo), '("xalign", AspectFrameXalignPropertyInfo), '("yalign", AspectFrameYalignPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
aspectFrameObeyChild :: AttrLabelProxy "obeyChild"
aspectFrameObeyChild = AttrLabelProxy

aspectFrameRatio :: AttrLabelProxy "ratio"
aspectFrameRatio = AttrLabelProxy

aspectFrameXalign :: AttrLabelProxy "xalign"
aspectFrameXalign = AttrLabelProxy

aspectFrameYalign :: AttrLabelProxy "yalign"
aspectFrameYalign = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList AspectFrame = AspectFrameSignalList
type AspectFrameSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method AspectFrame::new
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Label text." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "xalign"
--           , argType = TBasicType TFloat
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "Horizontal alignment of the child within the allocation of\n the #GtkAspectFrame. This ranges from 0.0 (left aligned)\n to 1.0 (right aligned)"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "yalign"
--           , argType = TBasicType TFloat
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "Vertical alignment of the child within the allocation of\n the #GtkAspectFrame. This ranges from 0.0 (top aligned)\n to 1.0 (bottom aligned)"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "ratio"
--           , argType = TBasicType TFloat
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The desired aspect ratio."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "obey_child"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "If %TRUE, @ratio is ignored, and the aspect\n ratio is taken from the requistion of the child."
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "AspectFrame" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_aspect_frame_new" gtk_aspect_frame_new :: 
    CString ->                              -- label : TBasicType TUTF8
    CFloat ->                               -- xalign : TBasicType TFloat
    CFloat ->                               -- yalign : TBasicType TFloat
    CFloat ->                               -- ratio : TBasicType TFloat
    CInt ->                                 -- obey_child : TBasicType TBoolean
    IO (Ptr AspectFrame)

-- | Create a new t'GI.Gtk.Objects.AspectFrame.AspectFrame'.
aspectFrameNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Maybe (T.Text)
    -- ^ /@label@/: Label text.
    -> Float
    -- ^ /@xalign@/: Horizontal alignment of the child within the allocation of
    --  the t'GI.Gtk.Objects.AspectFrame.AspectFrame'. This ranges from 0.0 (left aligned)
    --  to 1.0 (right aligned)
    -> Float
    -- ^ /@yalign@/: Vertical alignment of the child within the allocation of
    --  the t'GI.Gtk.Objects.AspectFrame.AspectFrame'. This ranges from 0.0 (top aligned)
    --  to 1.0 (bottom aligned)
    -> Float
    -- ^ /@ratio@/: The desired aspect ratio.
    -> Bool
    -- ^ /@obeyChild@/: If 'P.True', /@ratio@/ is ignored, and the aspect
    --  ratio is taken from the requistion of the child.
    -> m AspectFrame
    -- ^ __Returns:__ the new t'GI.Gtk.Objects.AspectFrame.AspectFrame'.
aspectFrameNew label xalign yalign ratio obeyChild = liftIO $ do
    maybeLabel <- case label of
        Nothing -> return nullPtr
        Just jLabel -> do
            jLabel' <- textToCString jLabel
            return jLabel'
    let xalign' = realToFrac xalign
    let yalign' = realToFrac yalign
    let ratio' = realToFrac ratio
    let obeyChild' = (fromIntegral . fromEnum) obeyChild
    result <- gtk_aspect_frame_new maybeLabel xalign' yalign' ratio' obeyChild'
    checkUnexpectedReturnNULL "aspectFrameNew" result
    result' <- (newObject AspectFrame) result
    freeMem maybeLabel
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method AspectFrame::set
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "aspect_frame"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AspectFrame" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAspectFrame" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "xalign"
--           , argType = TBasicType TFloat
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "Horizontal alignment of the child within the allocation of\n the #GtkAspectFrame. This ranges from 0.0 (left aligned)\n to 1.0 (right aligned)"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "yalign"
--           , argType = TBasicType TFloat
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "Vertical alignment of the child within the allocation of\n the #GtkAspectFrame. This ranges from 0.0 (top aligned)\n to 1.0 (bottom aligned)"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "ratio"
--           , argType = TBasicType TFloat
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The desired aspect ratio."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "obey_child"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "If %TRUE, @ratio is ignored, and the aspect\n ratio is taken from the requistion of the child."
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

foreign import ccall "gtk_aspect_frame_set" gtk_aspect_frame_set :: 
    Ptr AspectFrame ->                      -- aspect_frame : TInterface (Name {namespace = "Gtk", name = "AspectFrame"})
    CFloat ->                               -- xalign : TBasicType TFloat
    CFloat ->                               -- yalign : TBasicType TFloat
    CFloat ->                               -- ratio : TBasicType TFloat
    CInt ->                                 -- obey_child : TBasicType TBoolean
    IO ()

-- | Set parameters for an existing t'GI.Gtk.Objects.AspectFrame.AspectFrame'.
aspectFrameSet ::
    (B.CallStack.HasCallStack, MonadIO m, IsAspectFrame a) =>
    a
    -- ^ /@aspectFrame@/: a t'GI.Gtk.Objects.AspectFrame.AspectFrame'
    -> Float
    -- ^ /@xalign@/: Horizontal alignment of the child within the allocation of
    --  the t'GI.Gtk.Objects.AspectFrame.AspectFrame'. This ranges from 0.0 (left aligned)
    --  to 1.0 (right aligned)
    -> Float
    -- ^ /@yalign@/: Vertical alignment of the child within the allocation of
    --  the t'GI.Gtk.Objects.AspectFrame.AspectFrame'. This ranges from 0.0 (top aligned)
    --  to 1.0 (bottom aligned)
    -> Float
    -- ^ /@ratio@/: The desired aspect ratio.
    -> Bool
    -- ^ /@obeyChild@/: If 'P.True', /@ratio@/ is ignored, and the aspect
    --  ratio is taken from the requistion of the child.
    -> m ()
aspectFrameSet aspectFrame xalign yalign ratio obeyChild = liftIO $ do
    aspectFrame' <- unsafeManagedPtrCastPtr aspectFrame
    let xalign' = realToFrac xalign
    let yalign' = realToFrac yalign
    let ratio' = realToFrac ratio
    let obeyChild' = (fromIntegral . fromEnum) obeyChild
    gtk_aspect_frame_set aspectFrame' xalign' yalign' ratio' obeyChild'
    touchManagedPtr aspectFrame
    return ()

#if defined(ENABLE_OVERLOADING)
data AspectFrameSetMethodInfo
instance (signature ~ (Float -> Float -> Float -> Bool -> m ()), MonadIO m, IsAspectFrame a) => O.OverloadedMethod AspectFrameSetMethodInfo a signature where
    overloadedMethod = aspectFrameSet

instance O.OverloadedMethodInfo AspectFrameSetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AspectFrame.aspectFrameSet",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AspectFrame.html#v:aspectFrameSet"
        })


#endif


