{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- The t'GI.Gtk.Interfaces.ToolShell.ToolShell' interface allows container widgets to provide additional
-- information when embedding t'GI.Gtk.Objects.ToolItem.ToolItem' widgets.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Interfaces.ToolShell
    ( 

-- * Exported types
    ToolShell(..)                           ,
    IsToolShell                             ,
    toToolShell                             ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childNotify]("GI.Gtk.Objects.Widget#g:method:childNotify"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [rebuildMenu]("GI.Gtk.Interfaces.ToolShell#g:method:rebuildMenu"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEllipsizeMode]("GI.Gtk.Interfaces.ToolShell#g:method:getEllipsizeMode"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getIconSize]("GI.Gtk.Interfaces.ToolShell#g:method:getIconSize"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getOrientation]("GI.Gtk.Interfaces.ToolShell#g:method:getOrientation"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getReliefStyle]("GI.Gtk.Interfaces.ToolShell#g:method:getReliefStyle"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Interfaces.ToolShell#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTextAlignment]("GI.Gtk.Interfaces.ToolShell#g:method:getTextAlignment"), [getTextOrientation]("GI.Gtk.Interfaces.ToolShell#g:method:getTextOrientation"), [getTextSizeGroup]("GI.Gtk.Interfaces.ToolShell#g:method:getTextSizeGroup"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveToolShellMethod                  ,
#endif

-- ** getEllipsizeMode #method:getEllipsizeMode#

#if defined(ENABLE_OVERLOADING)
    ToolShellGetEllipsizeModeMethodInfo     ,
#endif
    toolShellGetEllipsizeMode               ,


-- ** getIconSize #method:getIconSize#

#if defined(ENABLE_OVERLOADING)
    ToolShellGetIconSizeMethodInfo          ,
#endif
    toolShellGetIconSize                    ,


-- ** getOrientation #method:getOrientation#

#if defined(ENABLE_OVERLOADING)
    ToolShellGetOrientationMethodInfo       ,
#endif
    toolShellGetOrientation                 ,


-- ** getReliefStyle #method:getReliefStyle#

#if defined(ENABLE_OVERLOADING)
    ToolShellGetReliefStyleMethodInfo       ,
#endif
    toolShellGetReliefStyle                 ,


-- ** getStyle #method:getStyle#

#if defined(ENABLE_OVERLOADING)
    ToolShellGetStyleMethodInfo             ,
#endif
    toolShellGetStyle                       ,


-- ** getTextAlignment #method:getTextAlignment#

#if defined(ENABLE_OVERLOADING)
    ToolShellGetTextAlignmentMethodInfo     ,
#endif
    toolShellGetTextAlignment               ,


-- ** getTextOrientation #method:getTextOrientation#

#if defined(ENABLE_OVERLOADING)
    ToolShellGetTextOrientationMethodInfo   ,
#endif
    toolShellGetTextOrientation             ,


-- ** getTextSizeGroup #method:getTextSizeGroup#

#if defined(ENABLE_OVERLOADING)
    ToolShellGetTextSizeGroupMethodInfo     ,
#endif
    toolShellGetTextSizeGroup               ,


-- ** rebuildMenu #method:rebuildMenu#

#if defined(ENABLE_OVERLOADING)
    ToolShellRebuildMenuMethodInfo          ,
#endif
    toolShellRebuildMenu                    ,




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
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Objects.SizeGroup as Gtk.SizeGroup
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget
import qualified GI.Pango.Enums as Pango.Enums

-- interface ToolShell 
-- | Memory-managed wrapper type.
newtype ToolShell = ToolShell (SP.ManagedPtr ToolShell)
    deriving (Eq)

instance SP.ManagedPtrNewtype ToolShell where
    toManagedPtr (ToolShell p) = p

foreign import ccall "gtk_tool_shell_get_type"
    c_gtk_tool_shell_get_type :: IO B.Types.GType

instance B.Types.TypedObject ToolShell where
    glibType = c_gtk_tool_shell_get_type

instance B.Types.GObject ToolShell

-- | Type class for types which can be safely cast to `ToolShell`, for instance with `toToolShell`.
class (SP.GObject o, O.IsDescendantOf ToolShell o) => IsToolShell o
instance (SP.GObject o, O.IsDescendantOf ToolShell o) => IsToolShell o

instance O.HasParentTypes ToolShell
type instance O.ParentTypes ToolShell = '[Gtk.Widget.Widget, GObject.Object.Object]

-- | Cast to `ToolShell`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toToolShell :: (MIO.MonadIO m, IsToolShell o) => o -> m ToolShell
toToolShell = MIO.liftIO . B.ManagedPtr.unsafeCastTo ToolShell

-- | Convert 'ToolShell' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe ToolShell) where
    gvalueGType_ = c_gtk_tool_shell_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr ToolShell)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr ToolShell)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject ToolShell ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList ToolShell
type instance O.AttributeList ToolShell = ToolShellAttributeList
type ToolShellAttributeList = ('[ '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveToolShellMethod (t :: Symbol) (o :: *) :: * where
    ResolveToolShellMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveToolShellMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveToolShellMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveToolShellMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveToolShellMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveToolShellMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveToolShellMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveToolShellMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveToolShellMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveToolShellMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveToolShellMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveToolShellMethod "childNotify" o = Gtk.Widget.WidgetChildNotifyMethodInfo
    ResolveToolShellMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveToolShellMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveToolShellMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveToolShellMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveToolShellMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveToolShellMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveToolShellMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveToolShellMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveToolShellMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveToolShellMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveToolShellMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveToolShellMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveToolShellMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveToolShellMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveToolShellMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveToolShellMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveToolShellMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveToolShellMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveToolShellMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveToolShellMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveToolShellMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveToolShellMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveToolShellMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveToolShellMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveToolShellMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveToolShellMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveToolShellMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveToolShellMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveToolShellMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveToolShellMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveToolShellMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveToolShellMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveToolShellMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveToolShellMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveToolShellMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveToolShellMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveToolShellMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveToolShellMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveToolShellMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveToolShellMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveToolShellMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveToolShellMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveToolShellMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveToolShellMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveToolShellMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveToolShellMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveToolShellMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveToolShellMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveToolShellMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveToolShellMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveToolShellMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveToolShellMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveToolShellMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveToolShellMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveToolShellMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveToolShellMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveToolShellMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveToolShellMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveToolShellMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveToolShellMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveToolShellMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveToolShellMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveToolShellMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveToolShellMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveToolShellMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveToolShellMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveToolShellMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveToolShellMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveToolShellMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveToolShellMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveToolShellMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveToolShellMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveToolShellMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveToolShellMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveToolShellMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveToolShellMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveToolShellMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveToolShellMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveToolShellMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveToolShellMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveToolShellMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveToolShellMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveToolShellMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveToolShellMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveToolShellMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveToolShellMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveToolShellMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveToolShellMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveToolShellMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveToolShellMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveToolShellMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveToolShellMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveToolShellMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveToolShellMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveToolShellMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveToolShellMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveToolShellMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveToolShellMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveToolShellMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveToolShellMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveToolShellMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveToolShellMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveToolShellMethod "rebuildMenu" o = ToolShellRebuildMenuMethodInfo
    ResolveToolShellMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveToolShellMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveToolShellMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveToolShellMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveToolShellMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveToolShellMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveToolShellMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveToolShellMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveToolShellMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveToolShellMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveToolShellMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveToolShellMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveToolShellMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveToolShellMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveToolShellMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveToolShellMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveToolShellMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveToolShellMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveToolShellMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveToolShellMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveToolShellMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveToolShellMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveToolShellMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveToolShellMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveToolShellMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveToolShellMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveToolShellMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveToolShellMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveToolShellMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveToolShellMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveToolShellMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveToolShellMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveToolShellMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveToolShellMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveToolShellMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveToolShellMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveToolShellMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveToolShellMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveToolShellMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveToolShellMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveToolShellMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveToolShellMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveToolShellMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveToolShellMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveToolShellMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveToolShellMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveToolShellMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveToolShellMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveToolShellMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveToolShellMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveToolShellMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveToolShellMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveToolShellMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveToolShellMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveToolShellMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveToolShellMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveToolShellMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveToolShellMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveToolShellMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveToolShellMethod "getEllipsizeMode" o = ToolShellGetEllipsizeModeMethodInfo
    ResolveToolShellMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveToolShellMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveToolShellMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveToolShellMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveToolShellMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveToolShellMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveToolShellMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveToolShellMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveToolShellMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveToolShellMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveToolShellMethod "getIconSize" o = ToolShellGetIconSizeMethodInfo
    ResolveToolShellMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveToolShellMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveToolShellMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveToolShellMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveToolShellMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveToolShellMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveToolShellMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveToolShellMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveToolShellMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveToolShellMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveToolShellMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveToolShellMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveToolShellMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveToolShellMethod "getOrientation" o = ToolShellGetOrientationMethodInfo
    ResolveToolShellMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveToolShellMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveToolShellMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveToolShellMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveToolShellMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveToolShellMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveToolShellMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveToolShellMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveToolShellMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveToolShellMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveToolShellMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveToolShellMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveToolShellMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveToolShellMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveToolShellMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveToolShellMethod "getReliefStyle" o = ToolShellGetReliefStyleMethodInfo
    ResolveToolShellMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveToolShellMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveToolShellMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveToolShellMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveToolShellMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveToolShellMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveToolShellMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveToolShellMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveToolShellMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveToolShellMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveToolShellMethod "getStyle" o = ToolShellGetStyleMethodInfo
    ResolveToolShellMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveToolShellMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveToolShellMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveToolShellMethod "getTextAlignment" o = ToolShellGetTextAlignmentMethodInfo
    ResolveToolShellMethod "getTextOrientation" o = ToolShellGetTextOrientationMethodInfo
    ResolveToolShellMethod "getTextSizeGroup" o = ToolShellGetTextSizeGroupMethodInfo
    ResolveToolShellMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveToolShellMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveToolShellMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveToolShellMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveToolShellMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveToolShellMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveToolShellMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveToolShellMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveToolShellMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveToolShellMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveToolShellMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveToolShellMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveToolShellMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveToolShellMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveToolShellMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveToolShellMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveToolShellMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveToolShellMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveToolShellMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveToolShellMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveToolShellMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveToolShellMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveToolShellMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveToolShellMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveToolShellMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveToolShellMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveToolShellMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveToolShellMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveToolShellMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveToolShellMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveToolShellMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveToolShellMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveToolShellMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveToolShellMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveToolShellMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveToolShellMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveToolShellMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveToolShellMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveToolShellMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveToolShellMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveToolShellMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveToolShellMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveToolShellMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveToolShellMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveToolShellMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveToolShellMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveToolShellMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveToolShellMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveToolShellMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveToolShellMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveToolShellMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveToolShellMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveToolShellMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveToolShellMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveToolShellMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveToolShellMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveToolShellMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveToolShellMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveToolShellMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveToolShellMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveToolShellMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveToolShellMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveToolShellMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveToolShellMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveToolShellMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveToolShellMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveToolShellMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveToolShellMethod t ToolShell, O.OverloadedMethod info ToolShell p) => OL.IsLabel t (ToolShell -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveToolShellMethod t ToolShell, O.OverloadedMethod info ToolShell p, R.HasField t ToolShell p) => R.HasField t ToolShell p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveToolShellMethod t ToolShell, O.OverloadedMethodInfo info ToolShell) => OL.IsLabel t (O.MethodProxy info ToolShell) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- method ToolShell::get_ellipsize_mode
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "shell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolShell" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToolShell" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Pango" , name = "EllipsizeMode" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tool_shell_get_ellipsize_mode" gtk_tool_shell_get_ellipsize_mode :: 
    Ptr ToolShell ->                        -- shell : TInterface (Name {namespace = "Gtk", name = "ToolShell"})
    IO CUInt

-- | Retrieves the current ellipsize mode for the tool shell. Tool items must not
-- call this function directly, but rely on 'GI.Gtk.Objects.ToolItem.toolItemGetEllipsizeMode'
-- instead.
-- 
-- /Since: 2.20/
toolShellGetEllipsizeMode ::
    (B.CallStack.HasCallStack, MonadIO m, IsToolShell a) =>
    a
    -- ^ /@shell@/: a t'GI.Gtk.Interfaces.ToolShell.ToolShell'
    -> m Pango.Enums.EllipsizeMode
    -- ^ __Returns:__ the current ellipsize mode of /@shell@/
toolShellGetEllipsizeMode shell = liftIO $ do
    shell' <- unsafeManagedPtrCastPtr shell
    result <- gtk_tool_shell_get_ellipsize_mode shell'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr shell
    return result'

#if defined(ENABLE_OVERLOADING)
data ToolShellGetEllipsizeModeMethodInfo
instance (signature ~ (m Pango.Enums.EllipsizeMode), MonadIO m, IsToolShell a) => O.OverloadedMethod ToolShellGetEllipsizeModeMethodInfo a signature where
    overloadedMethod = toolShellGetEllipsizeMode

instance O.OverloadedMethodInfo ToolShellGetEllipsizeModeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.ToolShell.toolShellGetEllipsizeMode",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-ToolShell.html#v:toolShellGetEllipsizeMode"
        })


#endif

-- method ToolShell::get_icon_size
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "shell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolShell" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToolShell" , sinceVersion = Nothing }
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

foreign import ccall "gtk_tool_shell_get_icon_size" gtk_tool_shell_get_icon_size :: 
    Ptr ToolShell ->                        -- shell : TInterface (Name {namespace = "Gtk", name = "ToolShell"})
    IO Int32

-- | Retrieves the icon size for the tool shell. Tool items must not call this
-- function directly, but rely on 'GI.Gtk.Objects.ToolItem.toolItemGetIconSize' instead.
-- 
-- /Since: 2.14/
toolShellGetIconSize ::
    (B.CallStack.HasCallStack, MonadIO m, IsToolShell a) =>
    a
    -- ^ /@shell@/: a t'GI.Gtk.Interfaces.ToolShell.ToolShell'
    -> m Int32
    -- ^ __Returns:__ the current size (t'GI.Gtk.Enums.IconSize') for icons of /@shell@/
toolShellGetIconSize shell = liftIO $ do
    shell' <- unsafeManagedPtrCastPtr shell
    result <- gtk_tool_shell_get_icon_size shell'
    touchManagedPtr shell
    return result

#if defined(ENABLE_OVERLOADING)
data ToolShellGetIconSizeMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsToolShell a) => O.OverloadedMethod ToolShellGetIconSizeMethodInfo a signature where
    overloadedMethod = toolShellGetIconSize

instance O.OverloadedMethodInfo ToolShellGetIconSizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.ToolShell.toolShellGetIconSize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-ToolShell.html#v:toolShellGetIconSize"
        })


#endif

-- method ToolShell::get_orientation
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "shell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolShell" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToolShell" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Orientation" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tool_shell_get_orientation" gtk_tool_shell_get_orientation :: 
    Ptr ToolShell ->                        -- shell : TInterface (Name {namespace = "Gtk", name = "ToolShell"})
    IO CUInt

-- | Retrieves the current orientation for the tool shell. Tool items must not
-- call this function directly, but rely on 'GI.Gtk.Objects.ToolItem.toolItemGetOrientation'
-- instead.
-- 
-- /Since: 2.14/
toolShellGetOrientation ::
    (B.CallStack.HasCallStack, MonadIO m, IsToolShell a) =>
    a
    -- ^ /@shell@/: a t'GI.Gtk.Interfaces.ToolShell.ToolShell'
    -> m Gtk.Enums.Orientation
    -- ^ __Returns:__ the current orientation of /@shell@/
toolShellGetOrientation shell = liftIO $ do
    shell' <- unsafeManagedPtrCastPtr shell
    result <- gtk_tool_shell_get_orientation shell'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr shell
    return result'

#if defined(ENABLE_OVERLOADING)
data ToolShellGetOrientationMethodInfo
instance (signature ~ (m Gtk.Enums.Orientation), MonadIO m, IsToolShell a) => O.OverloadedMethod ToolShellGetOrientationMethodInfo a signature where
    overloadedMethod = toolShellGetOrientation

instance O.OverloadedMethodInfo ToolShellGetOrientationMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.ToolShell.toolShellGetOrientation",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-ToolShell.html#v:toolShellGetOrientation"
        })


#endif

-- method ToolShell::get_relief_style
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "shell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolShell" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToolShell" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "ReliefStyle" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tool_shell_get_relief_style" gtk_tool_shell_get_relief_style :: 
    Ptr ToolShell ->                        -- shell : TInterface (Name {namespace = "Gtk", name = "ToolShell"})
    IO CUInt

-- | Returns the relief style of buttons on /@shell@/. Tool items must not call this
-- function directly, but rely on 'GI.Gtk.Objects.ToolItem.toolItemGetReliefStyle' instead.
-- 
-- /Since: 2.14/
toolShellGetReliefStyle ::
    (B.CallStack.HasCallStack, MonadIO m, IsToolShell a) =>
    a
    -- ^ /@shell@/: a t'GI.Gtk.Interfaces.ToolShell.ToolShell'
    -> m Gtk.Enums.ReliefStyle
    -- ^ __Returns:__ The relief style of buttons on /@shell@/.
toolShellGetReliefStyle shell = liftIO $ do
    shell' <- unsafeManagedPtrCastPtr shell
    result <- gtk_tool_shell_get_relief_style shell'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr shell
    return result'

#if defined(ENABLE_OVERLOADING)
data ToolShellGetReliefStyleMethodInfo
instance (signature ~ (m Gtk.Enums.ReliefStyle), MonadIO m, IsToolShell a) => O.OverloadedMethod ToolShellGetReliefStyleMethodInfo a signature where
    overloadedMethod = toolShellGetReliefStyle

instance O.OverloadedMethodInfo ToolShellGetReliefStyleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.ToolShell.toolShellGetReliefStyle",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-ToolShell.html#v:toolShellGetReliefStyle"
        })


#endif

-- method ToolShell::get_style
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "shell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolShell" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToolShell" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "ToolbarStyle" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tool_shell_get_style" gtk_tool_shell_get_style :: 
    Ptr ToolShell ->                        -- shell : TInterface (Name {namespace = "Gtk", name = "ToolShell"})
    IO CUInt

-- | Retrieves whether the tool shell has text, icons, or both. Tool items must
-- not call this function directly, but rely on 'GI.Gtk.Objects.ToolItem.toolItemGetToolbarStyle'
-- instead.
-- 
-- /Since: 2.14/
toolShellGetStyle ::
    (B.CallStack.HasCallStack, MonadIO m, IsToolShell a) =>
    a
    -- ^ /@shell@/: a t'GI.Gtk.Interfaces.ToolShell.ToolShell'
    -> m Gtk.Enums.ToolbarStyle
    -- ^ __Returns:__ the current style of /@shell@/
toolShellGetStyle shell = liftIO $ do
    shell' <- unsafeManagedPtrCastPtr shell
    result <- gtk_tool_shell_get_style shell'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr shell
    return result'

#if defined(ENABLE_OVERLOADING)
data ToolShellGetStyleMethodInfo
instance (signature ~ (m Gtk.Enums.ToolbarStyle), MonadIO m, IsToolShell a) => O.OverloadedMethod ToolShellGetStyleMethodInfo a signature where
    overloadedMethod = toolShellGetStyle

instance O.OverloadedMethodInfo ToolShellGetStyleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.ToolShell.toolShellGetStyle",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-ToolShell.html#v:toolShellGetStyle"
        })


#endif

-- method ToolShell::get_text_alignment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "shell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolShell" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToolShell" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TFloat)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tool_shell_get_text_alignment" gtk_tool_shell_get_text_alignment :: 
    Ptr ToolShell ->                        -- shell : TInterface (Name {namespace = "Gtk", name = "ToolShell"})
    IO CFloat

-- | Retrieves the current text alignment for the tool shell. Tool items must not
-- call this function directly, but rely on 'GI.Gtk.Objects.ToolItem.toolItemGetTextAlignment'
-- instead.
-- 
-- /Since: 2.20/
toolShellGetTextAlignment ::
    (B.CallStack.HasCallStack, MonadIO m, IsToolShell a) =>
    a
    -- ^ /@shell@/: a t'GI.Gtk.Interfaces.ToolShell.ToolShell'
    -> m Float
    -- ^ __Returns:__ the current text alignment of /@shell@/
toolShellGetTextAlignment shell = liftIO $ do
    shell' <- unsafeManagedPtrCastPtr shell
    result <- gtk_tool_shell_get_text_alignment shell'
    let result' = realToFrac result
    touchManagedPtr shell
    return result'

#if defined(ENABLE_OVERLOADING)
data ToolShellGetTextAlignmentMethodInfo
instance (signature ~ (m Float), MonadIO m, IsToolShell a) => O.OverloadedMethod ToolShellGetTextAlignmentMethodInfo a signature where
    overloadedMethod = toolShellGetTextAlignment

instance O.OverloadedMethodInfo ToolShellGetTextAlignmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.ToolShell.toolShellGetTextAlignment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-ToolShell.html#v:toolShellGetTextAlignment"
        })


#endif

-- method ToolShell::get_text_orientation
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "shell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolShell" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToolShell" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Orientation" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tool_shell_get_text_orientation" gtk_tool_shell_get_text_orientation :: 
    Ptr ToolShell ->                        -- shell : TInterface (Name {namespace = "Gtk", name = "ToolShell"})
    IO CUInt

-- | Retrieves the current text orientation for the tool shell. Tool items must not
-- call this function directly, but rely on 'GI.Gtk.Objects.ToolItem.toolItemGetTextOrientation'
-- instead.
-- 
-- /Since: 2.20/
toolShellGetTextOrientation ::
    (B.CallStack.HasCallStack, MonadIO m, IsToolShell a) =>
    a
    -- ^ /@shell@/: a t'GI.Gtk.Interfaces.ToolShell.ToolShell'
    -> m Gtk.Enums.Orientation
    -- ^ __Returns:__ the current text orientation of /@shell@/
toolShellGetTextOrientation shell = liftIO $ do
    shell' <- unsafeManagedPtrCastPtr shell
    result <- gtk_tool_shell_get_text_orientation shell'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr shell
    return result'

#if defined(ENABLE_OVERLOADING)
data ToolShellGetTextOrientationMethodInfo
instance (signature ~ (m Gtk.Enums.Orientation), MonadIO m, IsToolShell a) => O.OverloadedMethod ToolShellGetTextOrientationMethodInfo a signature where
    overloadedMethod = toolShellGetTextOrientation

instance O.OverloadedMethodInfo ToolShellGetTextOrientationMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.ToolShell.toolShellGetTextOrientation",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-ToolShell.html#v:toolShellGetTextOrientation"
        })


#endif

-- method ToolShell::get_text_size_group
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "shell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolShell" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToolShell" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "SizeGroup" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tool_shell_get_text_size_group" gtk_tool_shell_get_text_size_group :: 
    Ptr ToolShell ->                        -- shell : TInterface (Name {namespace = "Gtk", name = "ToolShell"})
    IO (Ptr Gtk.SizeGroup.SizeGroup)

-- | Retrieves the current text size group for the tool shell. Tool items must not
-- call this function directly, but rely on 'GI.Gtk.Objects.ToolItem.toolItemGetTextSizeGroup'
-- instead.
-- 
-- /Since: 2.20/
toolShellGetTextSizeGroup ::
    (B.CallStack.HasCallStack, MonadIO m, IsToolShell a) =>
    a
    -- ^ /@shell@/: a t'GI.Gtk.Interfaces.ToolShell.ToolShell'
    -> m Gtk.SizeGroup.SizeGroup
    -- ^ __Returns:__ the current text size group of /@shell@/
toolShellGetTextSizeGroup shell = liftIO $ do
    shell' <- unsafeManagedPtrCastPtr shell
    result <- gtk_tool_shell_get_text_size_group shell'
    checkUnexpectedReturnNULL "toolShellGetTextSizeGroup" result
    result' <- (newObject Gtk.SizeGroup.SizeGroup) result
    touchManagedPtr shell
    return result'

#if defined(ENABLE_OVERLOADING)
data ToolShellGetTextSizeGroupMethodInfo
instance (signature ~ (m Gtk.SizeGroup.SizeGroup), MonadIO m, IsToolShell a) => O.OverloadedMethod ToolShellGetTextSizeGroupMethodInfo a signature where
    overloadedMethod = toolShellGetTextSizeGroup

instance O.OverloadedMethodInfo ToolShellGetTextSizeGroupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.ToolShell.toolShellGetTextSizeGroup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-ToolShell.html#v:toolShellGetTextSizeGroup"
        })


#endif

-- method ToolShell::rebuild_menu
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "shell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolShell" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToolShell" , sinceVersion = Nothing }
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

foreign import ccall "gtk_tool_shell_rebuild_menu" gtk_tool_shell_rebuild_menu :: 
    Ptr ToolShell ->                        -- shell : TInterface (Name {namespace = "Gtk", name = "ToolShell"})
    IO ()

-- | Calling this function signals the tool shell that the overflow menu item for
-- tool items have changed. If there is an overflow menu and if it is visible
-- when this function it called, the menu will be rebuilt.
-- 
-- Tool items must not call this function directly, but rely on
-- 'GI.Gtk.Objects.ToolItem.toolItemRebuildMenu' instead.
-- 
-- /Since: 2.14/
toolShellRebuildMenu ::
    (B.CallStack.HasCallStack, MonadIO m, IsToolShell a) =>
    a
    -- ^ /@shell@/: a t'GI.Gtk.Interfaces.ToolShell.ToolShell'
    -> m ()
toolShellRebuildMenu shell = liftIO $ do
    shell' <- unsafeManagedPtrCastPtr shell
    gtk_tool_shell_rebuild_menu shell'
    touchManagedPtr shell
    return ()

#if defined(ENABLE_OVERLOADING)
data ToolShellRebuildMenuMethodInfo
instance (signature ~ (m ()), MonadIO m, IsToolShell a) => O.OverloadedMethod ToolShellRebuildMenuMethodInfo a signature where
    overloadedMethod = toolShellRebuildMenu

instance O.OverloadedMethodInfo ToolShellRebuildMenuMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.ToolShell.toolShellRebuildMenu",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-ToolShell.html#v:toolShellRebuildMenu"
        })


#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList ToolShell = ToolShellSignalList
type ToolShellSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif


