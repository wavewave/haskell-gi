{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Objects.HSV.HSV' is the “color wheel” part of a complete color selector widget.
-- It allows to select a color by determining its HSV components in an
-- intuitive way. Moving the selection around the outer ring changes the hue,
-- and moving the selection point inside the inner triangle changes value and
-- saturation.
-- 
-- t'GI.Gtk.Objects.HSV.HSV' has been deprecated together with t'GI.Gtk.Objects.ColorSelection.ColorSelection', where
-- it was used.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.HSV
    ( 

-- * Exported types
    HSV(..)                                 ,
    IsHSV                                   ,
    toHSV                                   ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childNotify]("GI.Gtk.Objects.Widget#g:method:childNotify"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAdjusting]("GI.Gtk.Objects.HSV#g:method:isAdjusting"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getColor]("GI.Gtk.Objects.HSV#g:method:getColor"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getMetrics]("GI.Gtk.Objects.HSV#g:method:getMetrics"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setColor]("GI.Gtk.Objects.HSV#g:method:setColor"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setMetrics]("GI.Gtk.Objects.HSV#g:method:setMetrics"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveHSVMethod                        ,
#endif

-- ** getColor #method:getColor#

#if defined(ENABLE_OVERLOADING)
    HSVGetColorMethodInfo                   ,
#endif
    hSVGetColor                             ,


-- ** getMetrics #method:getMetrics#

#if defined(ENABLE_OVERLOADING)
    HSVGetMetricsMethodInfo                 ,
#endif
    hSVGetMetrics                           ,


-- ** isAdjusting #method:isAdjusting#

#if defined(ENABLE_OVERLOADING)
    HSVIsAdjustingMethodInfo                ,
#endif
    hSVIsAdjusting                          ,


-- ** new #method:new#

    hSVNew                                  ,


-- ** setColor #method:setColor#

#if defined(ENABLE_OVERLOADING)
    HSVSetColorMethodInfo                   ,
#endif
    hSVSetColor                             ,


-- ** setMetrics #method:setMetrics#

#if defined(ENABLE_OVERLOADING)
    HSVSetMetricsMethodInfo                 ,
#endif
    hSVSetMetrics                           ,


-- ** toRgb #method:toRgb#

    hSVToRgb                                ,




 -- * Signals


-- ** changed #signal:changed#

    HSVChangedCallback                      ,
#if defined(ENABLE_OVERLOADING)
    HSVChangedSignalInfo                    ,
#endif
    afterHSVChanged                         ,
    onHSVChanged                            ,


-- ** move #signal:move#

    HSVMoveCallback                         ,
#if defined(ENABLE_OVERLOADING)
    HSVMoveSignalInfo                       ,
#endif
    afterHSVMove                            ,
    onHSVMove                               ,




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
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype HSV = HSV (SP.ManagedPtr HSV)
    deriving (Eq)

instance SP.ManagedPtrNewtype HSV where
    toManagedPtr (HSV p) = p

foreign import ccall "gtk_hsv_get_type"
    c_gtk_hsv_get_type :: IO B.Types.GType

instance B.Types.TypedObject HSV where
    glibType = c_gtk_hsv_get_type

instance B.Types.GObject HSV

-- | Type class for types which can be safely cast to `HSV`, for instance with `toHSV`.
class (SP.GObject o, O.IsDescendantOf HSV o) => IsHSV o
instance (SP.GObject o, O.IsDescendantOf HSV o) => IsHSV o

instance O.HasParentTypes HSV
type instance O.ParentTypes HSV = '[Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable]

-- | Cast to `HSV`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toHSV :: (MIO.MonadIO m, IsHSV o) => o -> m HSV
toHSV = MIO.liftIO . B.ManagedPtr.unsafeCastTo HSV

-- | Convert 'HSV' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe HSV) where
    gvalueGType_ = c_gtk_hsv_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr HSV)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr HSV)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject HSV ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveHSVMethod (t :: Symbol) (o :: *) :: * where
    ResolveHSVMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveHSVMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveHSVMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveHSVMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveHSVMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveHSVMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveHSVMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveHSVMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveHSVMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveHSVMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveHSVMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveHSVMethod "childNotify" o = Gtk.Widget.WidgetChildNotifyMethodInfo
    ResolveHSVMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveHSVMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveHSVMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveHSVMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveHSVMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveHSVMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveHSVMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveHSVMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveHSVMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveHSVMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveHSVMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveHSVMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveHSVMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveHSVMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveHSVMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveHSVMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveHSVMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveHSVMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveHSVMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveHSVMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveHSVMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveHSVMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveHSVMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveHSVMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveHSVMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveHSVMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveHSVMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveHSVMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveHSVMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveHSVMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveHSVMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveHSVMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveHSVMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveHSVMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveHSVMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveHSVMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveHSVMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveHSVMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveHSVMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveHSVMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveHSVMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveHSVMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveHSVMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveHSVMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveHSVMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveHSVMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveHSVMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveHSVMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveHSVMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveHSVMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveHSVMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveHSVMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveHSVMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveHSVMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveHSVMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveHSVMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveHSVMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveHSVMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveHSVMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveHSVMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveHSVMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveHSVMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveHSVMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveHSVMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveHSVMethod "isAdjusting" o = HSVIsAdjustingMethodInfo
    ResolveHSVMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveHSVMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveHSVMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveHSVMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveHSVMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveHSVMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveHSVMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveHSVMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveHSVMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveHSVMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveHSVMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveHSVMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveHSVMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveHSVMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveHSVMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveHSVMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveHSVMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveHSVMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveHSVMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveHSVMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveHSVMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveHSVMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveHSVMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveHSVMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveHSVMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveHSVMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveHSVMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveHSVMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveHSVMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveHSVMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveHSVMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveHSVMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveHSVMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveHSVMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveHSVMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveHSVMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveHSVMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveHSVMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveHSVMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveHSVMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveHSVMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveHSVMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveHSVMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveHSVMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveHSVMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveHSVMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveHSVMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveHSVMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveHSVMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveHSVMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveHSVMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveHSVMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveHSVMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveHSVMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveHSVMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveHSVMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveHSVMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveHSVMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveHSVMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveHSVMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveHSVMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveHSVMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveHSVMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveHSVMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveHSVMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveHSVMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveHSVMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveHSVMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveHSVMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveHSVMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveHSVMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveHSVMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveHSVMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveHSVMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveHSVMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveHSVMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveHSVMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveHSVMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveHSVMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveHSVMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveHSVMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveHSVMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveHSVMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveHSVMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveHSVMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveHSVMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveHSVMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveHSVMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveHSVMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveHSVMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveHSVMethod "getColor" o = HSVGetColorMethodInfo
    ResolveHSVMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveHSVMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveHSVMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveHSVMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveHSVMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveHSVMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveHSVMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveHSVMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveHSVMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveHSVMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveHSVMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveHSVMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveHSVMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveHSVMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveHSVMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveHSVMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveHSVMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveHSVMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveHSVMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveHSVMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveHSVMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveHSVMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveHSVMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveHSVMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveHSVMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveHSVMethod "getMetrics" o = HSVGetMetricsMethodInfo
    ResolveHSVMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveHSVMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveHSVMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveHSVMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveHSVMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveHSVMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveHSVMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveHSVMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveHSVMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveHSVMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveHSVMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveHSVMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveHSVMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveHSVMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveHSVMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveHSVMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveHSVMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveHSVMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveHSVMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveHSVMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveHSVMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveHSVMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveHSVMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveHSVMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveHSVMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveHSVMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveHSVMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveHSVMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveHSVMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveHSVMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveHSVMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveHSVMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveHSVMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveHSVMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveHSVMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveHSVMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveHSVMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveHSVMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveHSVMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveHSVMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveHSVMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveHSVMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveHSVMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveHSVMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveHSVMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveHSVMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveHSVMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveHSVMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveHSVMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveHSVMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveHSVMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveHSVMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveHSVMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveHSVMethod "setColor" o = HSVSetColorMethodInfo
    ResolveHSVMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveHSVMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveHSVMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveHSVMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveHSVMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveHSVMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveHSVMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveHSVMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveHSVMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveHSVMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveHSVMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveHSVMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveHSVMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveHSVMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveHSVMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveHSVMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveHSVMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveHSVMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveHSVMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveHSVMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveHSVMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveHSVMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveHSVMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveHSVMethod "setMetrics" o = HSVSetMetricsMethodInfo
    ResolveHSVMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveHSVMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveHSVMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveHSVMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveHSVMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveHSVMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveHSVMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveHSVMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveHSVMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveHSVMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveHSVMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveHSVMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveHSVMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveHSVMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveHSVMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveHSVMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveHSVMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveHSVMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveHSVMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveHSVMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveHSVMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveHSVMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveHSVMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveHSVMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveHSVMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveHSVMethod t HSV, O.OverloadedMethod info HSV p) => OL.IsLabel t (HSV -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveHSVMethod t HSV, O.OverloadedMethod info HSV p, R.HasField t HSV p) => R.HasField t HSV p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveHSVMethod t HSV, O.OverloadedMethodInfo info HSV) => OL.IsLabel t (O.MethodProxy info HSV) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal HSV::changed
-- | /No description available in the introspection data./
type HSVChangedCallback =
    IO ()

type C_HSVChangedCallback =
    Ptr HSV ->                              -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_HSVChangedCallback`.
foreign import ccall "wrapper"
    mk_HSVChangedCallback :: C_HSVChangedCallback -> IO (FunPtr C_HSVChangedCallback)

wrap_HSVChangedCallback :: 
    GObject a => (a -> HSVChangedCallback) ->
    C_HSVChangedCallback
wrap_HSVChangedCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [changed](#signal:changed) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' hSV #changed callback
-- @
-- 
-- 
onHSVChanged :: (IsHSV a, MonadIO m) => a -> ((?self :: a) => HSVChangedCallback) -> m SignalHandlerId
onHSVChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_HSVChangedCallback wrapped
    wrapped'' <- mk_HSVChangedCallback wrapped'
    connectSignalFunPtr obj "changed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [changed](#signal:changed) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' hSV #changed callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterHSVChanged :: (IsHSV a, MonadIO m) => a -> ((?self :: a) => HSVChangedCallback) -> m SignalHandlerId
afterHSVChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_HSVChangedCallback wrapped
    wrapped'' <- mk_HSVChangedCallback wrapped'
    connectSignalFunPtr obj "changed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data HSVChangedSignalInfo
instance SignalInfo HSVChangedSignalInfo where
    type HaskellCallbackType HSVChangedSignalInfo = HSVChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_HSVChangedCallback cb
        cb'' <- mk_HSVChangedCallback cb'
        connectSignalFunPtr obj "changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.HSV::changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-HSV.html#g:signal:changed"})

#endif

-- signal HSV::move
-- | /No description available in the introspection data./
type HSVMoveCallback =
    Gtk.Enums.DirectionType
    -> IO ()

type C_HSVMoveCallback =
    Ptr HSV ->                              -- object
    CUInt ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_HSVMoveCallback`.
foreign import ccall "wrapper"
    mk_HSVMoveCallback :: C_HSVMoveCallback -> IO (FunPtr C_HSVMoveCallback)

wrap_HSVMoveCallback :: 
    GObject a => (a -> HSVMoveCallback) ->
    C_HSVMoveCallback
wrap_HSVMoveCallback gi'cb gi'selfPtr object _ = do
    let object' = (toEnum . fromIntegral) object
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  object'


-- | Connect a signal handler for the [move](#signal:move) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' hSV #move callback
-- @
-- 
-- 
onHSVMove :: (IsHSV a, MonadIO m) => a -> ((?self :: a) => HSVMoveCallback) -> m SignalHandlerId
onHSVMove obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_HSVMoveCallback wrapped
    wrapped'' <- mk_HSVMoveCallback wrapped'
    connectSignalFunPtr obj "move" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [move](#signal:move) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' hSV #move callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterHSVMove :: (IsHSV a, MonadIO m) => a -> ((?self :: a) => HSVMoveCallback) -> m SignalHandlerId
afterHSVMove obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_HSVMoveCallback wrapped
    wrapped'' <- mk_HSVMoveCallback wrapped'
    connectSignalFunPtr obj "move" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data HSVMoveSignalInfo
instance SignalInfo HSVMoveSignalInfo where
    type HaskellCallbackType HSVMoveSignalInfo = HSVMoveCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_HSVMoveCallback cb
        cb'' <- mk_HSVMoveCallback cb'
        connectSignalFunPtr obj "move" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.HSV::move"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-HSV.html#g:signal:move"})

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList HSV
type instance O.AttributeList HSV = HSVAttributeList
type HSVAttributeList = ('[ '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList HSV = HSVSignalList
type HSVSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("changed", HSVChangedSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("move", HSVMoveSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method HSV::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "HSV" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_hsv_new" gtk_hsv_new :: 
    IO (Ptr HSV)

-- | Creates a new HSV color selector.
-- 
-- /Since: 2.14/
hSVNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m HSV
    -- ^ __Returns:__ A newly-created HSV color selector.
hSVNew  = liftIO $ do
    result <- gtk_hsv_new
    checkUnexpectedReturnNULL "hSVNew" result
    result' <- (newObject HSV) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method HSV::get_color
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "hsv"
--           , argType = TInterface Name { namespace = "Gtk" , name = "HSV" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "An HSV color selector"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "h"
--           , argType = TBasicType TDouble
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Return value for the hue"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "s"
--           , argType = TBasicType TDouble
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Return value for the saturation"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "v"
--           , argType = TBasicType TDouble
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Return value for the value"
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

foreign import ccall "gtk_hsv_get_color" gtk_hsv_get_color :: 
    Ptr HSV ->                              -- hsv : TInterface (Name {namespace = "Gtk", name = "HSV"})
    Ptr CDouble ->                          -- h : TBasicType TDouble
    Ptr CDouble ->                          -- s : TBasicType TDouble
    Ptr CDouble ->                          -- v : TBasicType TDouble
    IO ()

-- | Queries the current color in an HSV color selector.
-- Returned values will be in the [0.0, 1.0] range.
-- 
-- /Since: 2.14/
hSVGetColor ::
    (B.CallStack.HasCallStack, MonadIO m, IsHSV a) =>
    a
    -- ^ /@hsv@/: An HSV color selector
    -> m ((Double, Double, Double))
hSVGetColor hsv = liftIO $ do
    hsv' <- unsafeManagedPtrCastPtr hsv
    h <- allocMem :: IO (Ptr CDouble)
    s <- allocMem :: IO (Ptr CDouble)
    v <- allocMem :: IO (Ptr CDouble)
    gtk_hsv_get_color hsv' h s v
    h' <- peek h
    let h'' = realToFrac h'
    s' <- peek s
    let s'' = realToFrac s'
    v' <- peek v
    let v'' = realToFrac v'
    touchManagedPtr hsv
    freeMem h
    freeMem s
    freeMem v
    return (h'', s'', v'')

#if defined(ENABLE_OVERLOADING)
data HSVGetColorMethodInfo
instance (signature ~ (m ((Double, Double, Double))), MonadIO m, IsHSV a) => O.OverloadedMethod HSVGetColorMethodInfo a signature where
    overloadedMethod = hSVGetColor

instance O.OverloadedMethodInfo HSVGetColorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.HSV.hSVGetColor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-HSV.html#v:hSVGetColor"
        })


#endif

-- method HSV::get_metrics
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "hsv"
--           , argType = TInterface Name { namespace = "Gtk" , name = "HSV" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "An HSV color selector"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "size"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Return value for the diameter of the hue ring"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "ring_width"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Return value for the width of the hue ring"
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

foreign import ccall "gtk_hsv_get_metrics" gtk_hsv_get_metrics :: 
    Ptr HSV ->                              -- hsv : TInterface (Name {namespace = "Gtk", name = "HSV"})
    Ptr Int32 ->                            -- size : TBasicType TInt
    Ptr Int32 ->                            -- ring_width : TBasicType TInt
    IO ()

-- | Queries the size and ring width of an HSV color selector.
-- 
-- /Since: 2.14/
hSVGetMetrics ::
    (B.CallStack.HasCallStack, MonadIO m, IsHSV a) =>
    a
    -- ^ /@hsv@/: An HSV color selector
    -> m ((Int32, Int32))
hSVGetMetrics hsv = liftIO $ do
    hsv' <- unsafeManagedPtrCastPtr hsv
    size <- allocMem :: IO (Ptr Int32)
    ringWidth <- allocMem :: IO (Ptr Int32)
    gtk_hsv_get_metrics hsv' size ringWidth
    size' <- peek size
    ringWidth' <- peek ringWidth
    touchManagedPtr hsv
    freeMem size
    freeMem ringWidth
    return (size', ringWidth')

#if defined(ENABLE_OVERLOADING)
data HSVGetMetricsMethodInfo
instance (signature ~ (m ((Int32, Int32))), MonadIO m, IsHSV a) => O.OverloadedMethod HSVGetMetricsMethodInfo a signature where
    overloadedMethod = hSVGetMetrics

instance O.OverloadedMethodInfo HSVGetMetricsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.HSV.hSVGetMetrics",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-HSV.html#v:hSVGetMetrics"
        })


#endif

-- method HSV::is_adjusting
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "hsv"
--           , argType = TInterface Name { namespace = "Gtk" , name = "HSV" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkHSV" , sinceVersion = Nothing }
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

foreign import ccall "gtk_hsv_is_adjusting" gtk_hsv_is_adjusting :: 
    Ptr HSV ->                              -- hsv : TInterface (Name {namespace = "Gtk", name = "HSV"})
    IO CInt

-- | An HSV color selector can be said to be adjusting if multiple rapid
-- changes are being made to its value, for example, when the user is
-- adjusting the value with the mouse. This function queries whether
-- the HSV color selector is being adjusted or not.
-- 
-- /Since: 2.14/
hSVIsAdjusting ::
    (B.CallStack.HasCallStack, MonadIO m, IsHSV a) =>
    a
    -- ^ /@hsv@/: A t'GI.Gtk.Objects.HSV.HSV'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if clients can ignore changes to the color value,
    --     since they may be transitory, or 'P.False' if they should consider
    --     the color value status to be final.
hSVIsAdjusting hsv = liftIO $ do
    hsv' <- unsafeManagedPtrCastPtr hsv
    result <- gtk_hsv_is_adjusting hsv'
    let result' = (/= 0) result
    touchManagedPtr hsv
    return result'

#if defined(ENABLE_OVERLOADING)
data HSVIsAdjustingMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsHSV a) => O.OverloadedMethod HSVIsAdjustingMethodInfo a signature where
    overloadedMethod = hSVIsAdjusting

instance O.OverloadedMethodInfo HSVIsAdjustingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.HSV.hSVIsAdjusting",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-HSV.html#v:hSVIsAdjusting"
        })


#endif

-- method HSV::set_color
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "hsv"
--           , argType = TInterface Name { namespace = "Gtk" , name = "HSV" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "An HSV color selector"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "h"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation { rawDocText = Just "Hue" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "s"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Saturation" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "v"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Value" , sinceVersion = Nothing }
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

foreign import ccall "gtk_hsv_set_color" gtk_hsv_set_color :: 
    Ptr HSV ->                              -- hsv : TInterface (Name {namespace = "Gtk", name = "HSV"})
    CDouble ->                              -- h : TBasicType TDouble
    CDouble ->                              -- s : TBasicType TDouble
    CDouble ->                              -- v : TBasicType TDouble
    IO ()

-- | Sets the current color in an HSV color selector.
-- Color component values must be in the [0.0, 1.0] range.
-- 
-- /Since: 2.14/
hSVSetColor ::
    (B.CallStack.HasCallStack, MonadIO m, IsHSV a) =>
    a
    -- ^ /@hsv@/: An HSV color selector
    -> Double
    -- ^ /@h@/: Hue
    -> Double
    -- ^ /@s@/: Saturation
    -> Double
    -- ^ /@v@/: Value
    -> m ()
hSVSetColor hsv h s v = liftIO $ do
    hsv' <- unsafeManagedPtrCastPtr hsv
    let h' = realToFrac h
    let s' = realToFrac s
    let v' = realToFrac v
    gtk_hsv_set_color hsv' h' s' v'
    touchManagedPtr hsv
    return ()

#if defined(ENABLE_OVERLOADING)
data HSVSetColorMethodInfo
instance (signature ~ (Double -> Double -> Double -> m ()), MonadIO m, IsHSV a) => O.OverloadedMethod HSVSetColorMethodInfo a signature where
    overloadedMethod = hSVSetColor

instance O.OverloadedMethodInfo HSVSetColorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.HSV.hSVSetColor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-HSV.html#v:hSVSetColor"
        })


#endif

-- method HSV::set_metrics
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "hsv"
--           , argType = TInterface Name { namespace = "Gtk" , name = "HSV" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "An HSV color selector"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "size"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Diameter for the hue ring"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "ring_width"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Width of the hue ring"
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

foreign import ccall "gtk_hsv_set_metrics" gtk_hsv_set_metrics :: 
    Ptr HSV ->                              -- hsv : TInterface (Name {namespace = "Gtk", name = "HSV"})
    Int32 ->                                -- size : TBasicType TInt
    Int32 ->                                -- ring_width : TBasicType TInt
    IO ()

-- | Sets the size and ring width of an HSV color selector.
-- 
-- /Since: 2.14/
hSVSetMetrics ::
    (B.CallStack.HasCallStack, MonadIO m, IsHSV a) =>
    a
    -- ^ /@hsv@/: An HSV color selector
    -> Int32
    -- ^ /@size@/: Diameter for the hue ring
    -> Int32
    -- ^ /@ringWidth@/: Width of the hue ring
    -> m ()
hSVSetMetrics hsv size ringWidth = liftIO $ do
    hsv' <- unsafeManagedPtrCastPtr hsv
    gtk_hsv_set_metrics hsv' size ringWidth
    touchManagedPtr hsv
    return ()

#if defined(ENABLE_OVERLOADING)
data HSVSetMetricsMethodInfo
instance (signature ~ (Int32 -> Int32 -> m ()), MonadIO m, IsHSV a) => O.OverloadedMethod HSVSetMetricsMethodInfo a signature where
    overloadedMethod = hSVSetMetrics

instance O.OverloadedMethodInfo HSVSetMetricsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.HSV.hSVSetMetrics",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-HSV.html#v:hSVSetMetrics"
        })


#endif

-- method HSV::to_rgb
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "h"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation { rawDocText = Just "Hue" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "s"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Saturation" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "v"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Value" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "r"
--           , argType = TBasicType TDouble
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Return value for the red component"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "g"
--           , argType = TBasicType TDouble
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Return value for the green component"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "b"
--           , argType = TBasicType TDouble
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Return value for the blue component"
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

foreign import ccall "gtk_hsv_to_rgb" gtk_hsv_to_rgb :: 
    CDouble ->                              -- h : TBasicType TDouble
    CDouble ->                              -- s : TBasicType TDouble
    CDouble ->                              -- v : TBasicType TDouble
    Ptr CDouble ->                          -- r : TBasicType TDouble
    Ptr CDouble ->                          -- g : TBasicType TDouble
    Ptr CDouble ->                          -- b : TBasicType TDouble
    IO ()

-- | Converts a color from HSV space to RGB.
-- 
-- Input values must be in the [0.0, 1.0] range;
-- output values will be in the same range.
-- 
-- /Since: 2.14/
hSVToRgb ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Double
    -- ^ /@h@/: Hue
    -> Double
    -- ^ /@s@/: Saturation
    -> Double
    -- ^ /@v@/: Value
    -> m ((Double, Double, Double))
hSVToRgb h s v = liftIO $ do
    let h' = realToFrac h
    let s' = realToFrac s
    let v' = realToFrac v
    r <- allocMem :: IO (Ptr CDouble)
    g <- allocMem :: IO (Ptr CDouble)
    b <- allocMem :: IO (Ptr CDouble)
    gtk_hsv_to_rgb h' s' v' r g b
    r' <- peek r
    let r'' = realToFrac r'
    g' <- peek g
    let g'' = realToFrac g'
    b' <- peek b
    let b'' = realToFrac b'
    freeMem r
    freeMem g
    freeMem b
    return (r'', g'', b'')

#if defined(ENABLE_OVERLOADING)
#endif


