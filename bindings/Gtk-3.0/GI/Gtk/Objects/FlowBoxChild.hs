{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- /No description available in the introspection data./

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.FlowBoxChild
    ( 

-- * Exported types
    FlowBoxChild(..)                        ,
    IsFlowBoxChild                          ,
    toFlowBoxChild                          ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [changed]("GI.Gtk.Objects.FlowBoxChild#g:method:changed"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSelected]("GI.Gtk.Objects.FlowBoxChild#g:method:isSelected"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChild]("GI.Gtk.Objects.Bin#g:method:getChild"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getIndex]("GI.Gtk.Objects.FlowBoxChild#g:method:getIndex"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveFlowBoxChildMethod               ,
#endif

-- ** changed #method:changed#

#if defined(ENABLE_OVERLOADING)
    FlowBoxChildChangedMethodInfo           ,
#endif
    flowBoxChildChanged                     ,


-- ** getIndex #method:getIndex#

#if defined(ENABLE_OVERLOADING)
    FlowBoxChildGetIndexMethodInfo          ,
#endif
    flowBoxChildGetIndex                    ,


-- ** isSelected #method:isSelected#

#if defined(ENABLE_OVERLOADING)
    FlowBoxChildIsSelectedMethodInfo        ,
#endif
    flowBoxChildIsSelected                  ,


-- ** new #method:new#

    flowBoxChildNew                         ,




 -- * Signals


-- ** activate #signal:activate#

    FlowBoxChildActivateCallback            ,
#if defined(ENABLE_OVERLOADING)
    FlowBoxChildActivateSignalInfo          ,
#endif
    afterFlowBoxChildActivate               ,
    onFlowBoxChildActivate                  ,




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
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype FlowBoxChild = FlowBoxChild (SP.ManagedPtr FlowBoxChild)
    deriving (Eq)

instance SP.ManagedPtrNewtype FlowBoxChild where
    toManagedPtr (FlowBoxChild p) = p

foreign import ccall "gtk_flow_box_child_get_type"
    c_gtk_flow_box_child_get_type :: IO B.Types.GType

instance B.Types.TypedObject FlowBoxChild where
    glibType = c_gtk_flow_box_child_get_type

instance B.Types.GObject FlowBoxChild

-- | Type class for types which can be safely cast to `FlowBoxChild`, for instance with `toFlowBoxChild`.
class (SP.GObject o, O.IsDescendantOf FlowBoxChild o) => IsFlowBoxChild o
instance (SP.GObject o, O.IsDescendantOf FlowBoxChild o) => IsFlowBoxChild o

instance O.HasParentTypes FlowBoxChild
type instance O.ParentTypes FlowBoxChild = '[Gtk.Bin.Bin, Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable]

-- | Cast to `FlowBoxChild`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toFlowBoxChild :: (MIO.MonadIO m, IsFlowBoxChild o) => o -> m FlowBoxChild
toFlowBoxChild = MIO.liftIO . B.ManagedPtr.unsafeCastTo FlowBoxChild

-- | Convert 'FlowBoxChild' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe FlowBoxChild) where
    gvalueGType_ = c_gtk_flow_box_child_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr FlowBoxChild)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr FlowBoxChild)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject FlowBoxChild ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveFlowBoxChildMethod (t :: Symbol) (o :: *) :: * where
    ResolveFlowBoxChildMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveFlowBoxChildMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveFlowBoxChildMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveFlowBoxChildMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveFlowBoxChildMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveFlowBoxChildMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveFlowBoxChildMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveFlowBoxChildMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveFlowBoxChildMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveFlowBoxChildMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveFlowBoxChildMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveFlowBoxChildMethod "changed" o = FlowBoxChildChangedMethodInfo
    ResolveFlowBoxChildMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveFlowBoxChildMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveFlowBoxChildMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveFlowBoxChildMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveFlowBoxChildMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveFlowBoxChildMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveFlowBoxChildMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveFlowBoxChildMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveFlowBoxChildMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveFlowBoxChildMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveFlowBoxChildMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveFlowBoxChildMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveFlowBoxChildMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveFlowBoxChildMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveFlowBoxChildMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveFlowBoxChildMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveFlowBoxChildMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveFlowBoxChildMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveFlowBoxChildMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveFlowBoxChildMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveFlowBoxChildMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveFlowBoxChildMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveFlowBoxChildMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveFlowBoxChildMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveFlowBoxChildMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveFlowBoxChildMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveFlowBoxChildMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveFlowBoxChildMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveFlowBoxChildMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveFlowBoxChildMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveFlowBoxChildMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveFlowBoxChildMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveFlowBoxChildMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveFlowBoxChildMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveFlowBoxChildMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveFlowBoxChildMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveFlowBoxChildMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveFlowBoxChildMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveFlowBoxChildMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveFlowBoxChildMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveFlowBoxChildMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveFlowBoxChildMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveFlowBoxChildMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveFlowBoxChildMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveFlowBoxChildMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveFlowBoxChildMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveFlowBoxChildMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveFlowBoxChildMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveFlowBoxChildMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveFlowBoxChildMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveFlowBoxChildMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveFlowBoxChildMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveFlowBoxChildMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveFlowBoxChildMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveFlowBoxChildMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveFlowBoxChildMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveFlowBoxChildMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveFlowBoxChildMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveFlowBoxChildMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveFlowBoxChildMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveFlowBoxChildMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveFlowBoxChildMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveFlowBoxChildMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveFlowBoxChildMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveFlowBoxChildMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveFlowBoxChildMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveFlowBoxChildMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveFlowBoxChildMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveFlowBoxChildMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveFlowBoxChildMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveFlowBoxChildMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveFlowBoxChildMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveFlowBoxChildMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveFlowBoxChildMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveFlowBoxChildMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveFlowBoxChildMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveFlowBoxChildMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveFlowBoxChildMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveFlowBoxChildMethod "isSelected" o = FlowBoxChildIsSelectedMethodInfo
    ResolveFlowBoxChildMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveFlowBoxChildMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveFlowBoxChildMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveFlowBoxChildMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveFlowBoxChildMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveFlowBoxChildMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveFlowBoxChildMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveFlowBoxChildMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveFlowBoxChildMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveFlowBoxChildMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveFlowBoxChildMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveFlowBoxChildMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveFlowBoxChildMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveFlowBoxChildMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveFlowBoxChildMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveFlowBoxChildMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveFlowBoxChildMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveFlowBoxChildMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveFlowBoxChildMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveFlowBoxChildMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveFlowBoxChildMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveFlowBoxChildMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveFlowBoxChildMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveFlowBoxChildMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveFlowBoxChildMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveFlowBoxChildMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveFlowBoxChildMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveFlowBoxChildMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveFlowBoxChildMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveFlowBoxChildMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveFlowBoxChildMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveFlowBoxChildMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveFlowBoxChildMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveFlowBoxChildMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveFlowBoxChildMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveFlowBoxChildMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveFlowBoxChildMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveFlowBoxChildMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveFlowBoxChildMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolveFlowBoxChildMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveFlowBoxChildMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveFlowBoxChildMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveFlowBoxChildMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveFlowBoxChildMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveFlowBoxChildMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveFlowBoxChildMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveFlowBoxChildMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveFlowBoxChildMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveFlowBoxChildMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveFlowBoxChildMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveFlowBoxChildMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveFlowBoxChildMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveFlowBoxChildMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveFlowBoxChildMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveFlowBoxChildMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveFlowBoxChildMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveFlowBoxChildMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveFlowBoxChildMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveFlowBoxChildMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveFlowBoxChildMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveFlowBoxChildMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveFlowBoxChildMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveFlowBoxChildMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveFlowBoxChildMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveFlowBoxChildMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveFlowBoxChildMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveFlowBoxChildMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveFlowBoxChildMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveFlowBoxChildMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveFlowBoxChildMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveFlowBoxChildMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveFlowBoxChildMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveFlowBoxChildMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveFlowBoxChildMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveFlowBoxChildMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveFlowBoxChildMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveFlowBoxChildMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveFlowBoxChildMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveFlowBoxChildMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveFlowBoxChildMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveFlowBoxChildMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveFlowBoxChildMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveFlowBoxChildMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveFlowBoxChildMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveFlowBoxChildMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveFlowBoxChildMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveFlowBoxChildMethod "getChild" o = Gtk.Bin.BinGetChildMethodInfo
    ResolveFlowBoxChildMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveFlowBoxChildMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveFlowBoxChildMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveFlowBoxChildMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveFlowBoxChildMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveFlowBoxChildMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveFlowBoxChildMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveFlowBoxChildMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveFlowBoxChildMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveFlowBoxChildMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveFlowBoxChildMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveFlowBoxChildMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveFlowBoxChildMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveFlowBoxChildMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveFlowBoxChildMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveFlowBoxChildMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveFlowBoxChildMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveFlowBoxChildMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveFlowBoxChildMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveFlowBoxChildMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveFlowBoxChildMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveFlowBoxChildMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveFlowBoxChildMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveFlowBoxChildMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveFlowBoxChildMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveFlowBoxChildMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveFlowBoxChildMethod "getIndex" o = FlowBoxChildGetIndexMethodInfo
    ResolveFlowBoxChildMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveFlowBoxChildMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveFlowBoxChildMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveFlowBoxChildMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveFlowBoxChildMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveFlowBoxChildMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveFlowBoxChildMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveFlowBoxChildMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveFlowBoxChildMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveFlowBoxChildMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveFlowBoxChildMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveFlowBoxChildMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveFlowBoxChildMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveFlowBoxChildMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveFlowBoxChildMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveFlowBoxChildMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveFlowBoxChildMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveFlowBoxChildMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveFlowBoxChildMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveFlowBoxChildMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveFlowBoxChildMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveFlowBoxChildMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveFlowBoxChildMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveFlowBoxChildMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveFlowBoxChildMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveFlowBoxChildMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveFlowBoxChildMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveFlowBoxChildMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveFlowBoxChildMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveFlowBoxChildMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveFlowBoxChildMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveFlowBoxChildMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveFlowBoxChildMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveFlowBoxChildMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveFlowBoxChildMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveFlowBoxChildMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveFlowBoxChildMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveFlowBoxChildMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveFlowBoxChildMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveFlowBoxChildMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveFlowBoxChildMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveFlowBoxChildMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveFlowBoxChildMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveFlowBoxChildMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveFlowBoxChildMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveFlowBoxChildMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveFlowBoxChildMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveFlowBoxChildMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveFlowBoxChildMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveFlowBoxChildMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveFlowBoxChildMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveFlowBoxChildMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveFlowBoxChildMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveFlowBoxChildMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveFlowBoxChildMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveFlowBoxChildMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveFlowBoxChildMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveFlowBoxChildMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveFlowBoxChildMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveFlowBoxChildMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveFlowBoxChildMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveFlowBoxChildMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveFlowBoxChildMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveFlowBoxChildMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveFlowBoxChildMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveFlowBoxChildMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveFlowBoxChildMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveFlowBoxChildMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveFlowBoxChildMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveFlowBoxChildMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveFlowBoxChildMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveFlowBoxChildMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveFlowBoxChildMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveFlowBoxChildMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveFlowBoxChildMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveFlowBoxChildMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveFlowBoxChildMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveFlowBoxChildMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveFlowBoxChildMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveFlowBoxChildMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveFlowBoxChildMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveFlowBoxChildMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveFlowBoxChildMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveFlowBoxChildMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveFlowBoxChildMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveFlowBoxChildMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveFlowBoxChildMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveFlowBoxChildMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveFlowBoxChildMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveFlowBoxChildMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveFlowBoxChildMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveFlowBoxChildMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveFlowBoxChildMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveFlowBoxChildMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveFlowBoxChildMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveFlowBoxChildMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveFlowBoxChildMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveFlowBoxChildMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveFlowBoxChildMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveFlowBoxChildMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveFlowBoxChildMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveFlowBoxChildMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveFlowBoxChildMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveFlowBoxChildMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveFlowBoxChildMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveFlowBoxChildMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveFlowBoxChildMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveFlowBoxChildMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveFlowBoxChildMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveFlowBoxChildMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveFlowBoxChildMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveFlowBoxChildMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveFlowBoxChildMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveFlowBoxChildMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveFlowBoxChildMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveFlowBoxChildMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveFlowBoxChildMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveFlowBoxChildMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveFlowBoxChildMethod t FlowBoxChild, O.OverloadedMethod info FlowBoxChild p) => OL.IsLabel t (FlowBoxChild -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveFlowBoxChildMethod t FlowBoxChild, O.OverloadedMethod info FlowBoxChild p, R.HasField t FlowBoxChild p) => R.HasField t FlowBoxChild p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveFlowBoxChildMethod t FlowBoxChild, O.OverloadedMethodInfo info FlowBoxChild) => OL.IsLabel t (O.MethodProxy info FlowBoxChild) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal FlowBoxChild::activate
-- | The [activate](#g:signal:activate) signal is emitted when the user activates
-- a child widget in a t'GI.Gtk.Objects.FlowBox.FlowBox', either by clicking or
-- double-clicking, or by using the Space or Enter key.
-- 
-- While this signal is used as a
-- [keybinding signal][GtkBindingSignal],
-- it can be used by applications for their own purposes.
type FlowBoxChildActivateCallback =
    IO ()

type C_FlowBoxChildActivateCallback =
    Ptr FlowBoxChild ->                     -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_FlowBoxChildActivateCallback`.
foreign import ccall "wrapper"
    mk_FlowBoxChildActivateCallback :: C_FlowBoxChildActivateCallback -> IO (FunPtr C_FlowBoxChildActivateCallback)

wrap_FlowBoxChildActivateCallback :: 
    GObject a => (a -> FlowBoxChildActivateCallback) ->
    C_FlowBoxChildActivateCallback
wrap_FlowBoxChildActivateCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [activate](#signal:activate) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' flowBoxChild #activate callback
-- @
-- 
-- 
onFlowBoxChildActivate :: (IsFlowBoxChild a, MonadIO m) => a -> ((?self :: a) => FlowBoxChildActivateCallback) -> m SignalHandlerId
onFlowBoxChildActivate obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_FlowBoxChildActivateCallback wrapped
    wrapped'' <- mk_FlowBoxChildActivateCallback wrapped'
    connectSignalFunPtr obj "activate" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [activate](#signal:activate) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' flowBoxChild #activate callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterFlowBoxChildActivate :: (IsFlowBoxChild a, MonadIO m) => a -> ((?self :: a) => FlowBoxChildActivateCallback) -> m SignalHandlerId
afterFlowBoxChildActivate obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_FlowBoxChildActivateCallback wrapped
    wrapped'' <- mk_FlowBoxChildActivateCallback wrapped'
    connectSignalFunPtr obj "activate" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data FlowBoxChildActivateSignalInfo
instance SignalInfo FlowBoxChildActivateSignalInfo where
    type HaskellCallbackType FlowBoxChildActivateSignalInfo = FlowBoxChildActivateCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_FlowBoxChildActivateCallback cb
        cb'' <- mk_FlowBoxChildActivateCallback cb'
        connectSignalFunPtr obj "activate" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBoxChild::activate"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBoxChild.html#g:signal:activate"})

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList FlowBoxChild
type instance O.AttributeList FlowBoxChild = FlowBoxChildAttributeList
type FlowBoxChildAttributeList = ('[ '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList FlowBoxChild = FlowBoxChildSignalList
type FlowBoxChildSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("activate", FlowBoxChildActivateSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method FlowBoxChild::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "FlowBoxChild" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_flow_box_child_new" gtk_flow_box_child_new :: 
    IO (Ptr FlowBoxChild)

-- | Creates a new t'GI.Gtk.Objects.FlowBoxChild.FlowBoxChild', to be used as a child
-- of a t'GI.Gtk.Objects.FlowBox.FlowBox'.
-- 
-- /Since: 3.12/
flowBoxChildNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m FlowBoxChild
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.FlowBoxChild.FlowBoxChild'
flowBoxChildNew  = liftIO $ do
    result <- gtk_flow_box_child_new
    checkUnexpectedReturnNULL "flowBoxChildNew" result
    result' <- (newObject FlowBoxChild) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method FlowBoxChild::changed
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "child"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBoxChild" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFlowBoxChild" , sinceVersion = Nothing }
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

foreign import ccall "gtk_flow_box_child_changed" gtk_flow_box_child_changed :: 
    Ptr FlowBoxChild ->                     -- child : TInterface (Name {namespace = "Gtk", name = "FlowBoxChild"})
    IO ()

-- | Marks /@child@/ as changed, causing any state that depends on this
-- to be updated. This affects sorting and filtering.
-- 
-- Note that calls to this method must be in sync with the data
-- used for the sorting and filtering functions. For instance, if
-- the list is mirroring some external data set, and *two* children
-- changed in the external data set when you call
-- 'GI.Gtk.Objects.FlowBoxChild.flowBoxChildChanged' on the first child, the sort function
-- must only read the new data for the first of the two changed
-- children, otherwise the resorting of the children will be wrong.
-- 
-- This generally means that if you don’t fully control the data
-- model, you have to duplicate the data that affects the sorting
-- and filtering functions into the widgets themselves. Another
-- alternative is to call 'GI.Gtk.Objects.FlowBox.flowBoxInvalidateSort' on any
-- model change, but that is more expensive.
-- 
-- /Since: 3.12/
flowBoxChildChanged ::
    (B.CallStack.HasCallStack, MonadIO m, IsFlowBoxChild a) =>
    a
    -- ^ /@child@/: a t'GI.Gtk.Objects.FlowBoxChild.FlowBoxChild'
    -> m ()
flowBoxChildChanged child = liftIO $ do
    child' <- unsafeManagedPtrCastPtr child
    gtk_flow_box_child_changed child'
    touchManagedPtr child
    return ()

#if defined(ENABLE_OVERLOADING)
data FlowBoxChildChangedMethodInfo
instance (signature ~ (m ()), MonadIO m, IsFlowBoxChild a) => O.OverloadedMethod FlowBoxChildChangedMethodInfo a signature where
    overloadedMethod = flowBoxChildChanged

instance O.OverloadedMethodInfo FlowBoxChildChangedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBoxChild.flowBoxChildChanged",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBoxChild.html#v:flowBoxChildChanged"
        })


#endif

-- method FlowBoxChild::get_index
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "child"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBoxChild" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFlowBoxChild" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_flow_box_child_get_index" gtk_flow_box_child_get_index :: 
    Ptr FlowBoxChild ->                     -- child : TInterface (Name {namespace = "Gtk", name = "FlowBoxChild"})
    IO Int32

-- | Gets the current index of the /@child@/ in its t'GI.Gtk.Objects.FlowBox.FlowBox' container.
-- 
-- /Since: 3.12/
flowBoxChildGetIndex ::
    (B.CallStack.HasCallStack, MonadIO m, IsFlowBoxChild a) =>
    a
    -- ^ /@child@/: a t'GI.Gtk.Objects.FlowBoxChild.FlowBoxChild'
    -> m Int32
    -- ^ __Returns:__ the index of the /@child@/, or -1 if the /@child@/ is not
    --     in a flow box.
flowBoxChildGetIndex child = liftIO $ do
    child' <- unsafeManagedPtrCastPtr child
    result <- gtk_flow_box_child_get_index child'
    touchManagedPtr child
    return result

#if defined(ENABLE_OVERLOADING)
data FlowBoxChildGetIndexMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsFlowBoxChild a) => O.OverloadedMethod FlowBoxChildGetIndexMethodInfo a signature where
    overloadedMethod = flowBoxChildGetIndex

instance O.OverloadedMethodInfo FlowBoxChildGetIndexMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBoxChild.flowBoxChildGetIndex",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBoxChild.html#v:flowBoxChildGetIndex"
        })


#endif

-- method FlowBoxChild::is_selected
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "child"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBoxChild" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFlowBoxChild" , sinceVersion = Nothing }
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

foreign import ccall "gtk_flow_box_child_is_selected" gtk_flow_box_child_is_selected :: 
    Ptr FlowBoxChild ->                     -- child : TInterface (Name {namespace = "Gtk", name = "FlowBoxChild"})
    IO CInt

-- | Returns whether the /@child@/ is currently selected in its
-- t'GI.Gtk.Objects.FlowBox.FlowBox' container.
-- 
-- /Since: 3.12/
flowBoxChildIsSelected ::
    (B.CallStack.HasCallStack, MonadIO m, IsFlowBoxChild a) =>
    a
    -- ^ /@child@/: a t'GI.Gtk.Objects.FlowBoxChild.FlowBoxChild'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@child@/ is selected
flowBoxChildIsSelected child = liftIO $ do
    child' <- unsafeManagedPtrCastPtr child
    result <- gtk_flow_box_child_is_selected child'
    let result' = (/= 0) result
    touchManagedPtr child
    return result'

#if defined(ENABLE_OVERLOADING)
data FlowBoxChildIsSelectedMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsFlowBoxChild a) => O.OverloadedMethod FlowBoxChildIsSelectedMethodInfo a signature where
    overloadedMethod = flowBoxChildIsSelected

instance O.OverloadedMethodInfo FlowBoxChildIsSelectedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBoxChild.flowBoxChildIsSelected",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBoxChild.html#v:flowBoxChildIsSelected"
        })


#endif


