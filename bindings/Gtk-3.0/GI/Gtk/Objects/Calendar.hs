{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Objects.Calendar.Calendar' is a widget that displays a Gregorian calendar, one month
-- at a time. It can be created with 'GI.Gtk.Objects.Calendar.calendarNew'.
-- 
-- The month and year currently displayed can be altered with
-- 'GI.Gtk.Objects.Calendar.calendarSelectMonth'. The exact day can be selected from the
-- displayed month using 'GI.Gtk.Objects.Calendar.calendarSelectDay'.
-- 
-- To place a visual marker on a particular day, use 'GI.Gtk.Objects.Calendar.calendarMarkDay'
-- and to remove the marker, 'GI.Gtk.Objects.Calendar.calendarUnmarkDay'. Alternative, all
-- marks can be cleared with 'GI.Gtk.Objects.Calendar.calendarClearMarks'.
-- 
-- The way in which the calendar itself is displayed can be altered using
-- 'GI.Gtk.Objects.Calendar.calendarSetDisplayOptions'.
-- 
-- The selected date can be retrieved from a t'GI.Gtk.Objects.Calendar.Calendar' using
-- 'GI.Gtk.Objects.Calendar.calendarGetDate'.
-- 
-- Users should be aware that, although the Gregorian calendar is the
-- legal calendar in most countries, it was adopted progressively
-- between 1582 and 1929. Display before these dates is likely to be
-- historically incorrect.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.Calendar
    ( 

-- * Exported types
    Calendar(..)                            ,
    IsCalendar                              ,
    toCalendar                              ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childNotify]("GI.Gtk.Objects.Widget#g:method:childNotify"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [clearMarks]("GI.Gtk.Objects.Calendar#g:method:clearMarks"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [markDay]("GI.Gtk.Objects.Calendar#g:method:markDay"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [selectDay]("GI.Gtk.Objects.Calendar#g:method:selectDay"), [selectMonth]("GI.Gtk.Objects.Calendar#g:method:selectMonth"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unmarkDay]("GI.Gtk.Objects.Calendar#g:method:unmarkDay"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDate]("GI.Gtk.Objects.Calendar#g:method:getDate"), [getDayIsMarked]("GI.Gtk.Objects.Calendar#g:method:getDayIsMarked"), [getDetailHeightRows]("GI.Gtk.Objects.Calendar#g:method:getDetailHeightRows"), [getDetailWidthChars]("GI.Gtk.Objects.Calendar#g:method:getDetailWidthChars"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDisplayOptions]("GI.Gtk.Objects.Calendar#g:method:getDisplayOptions"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDetailFunc]("GI.Gtk.Objects.Calendar#g:method:setDetailFunc"), [setDetailHeightRows]("GI.Gtk.Objects.Calendar#g:method:setDetailHeightRows"), [setDetailWidthChars]("GI.Gtk.Objects.Calendar#g:method:setDetailWidthChars"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDisplayOptions]("GI.Gtk.Objects.Calendar#g:method:setDisplayOptions"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveCalendarMethod                   ,
#endif

-- ** clearMarks #method:clearMarks#

#if defined(ENABLE_OVERLOADING)
    CalendarClearMarksMethodInfo            ,
#endif
    calendarClearMarks                      ,


-- ** getDate #method:getDate#

#if defined(ENABLE_OVERLOADING)
    CalendarGetDateMethodInfo               ,
#endif
    calendarGetDate                         ,


-- ** getDayIsMarked #method:getDayIsMarked#

#if defined(ENABLE_OVERLOADING)
    CalendarGetDayIsMarkedMethodInfo        ,
#endif
    calendarGetDayIsMarked                  ,


-- ** getDetailHeightRows #method:getDetailHeightRows#

#if defined(ENABLE_OVERLOADING)
    CalendarGetDetailHeightRowsMethodInfo   ,
#endif
    calendarGetDetailHeightRows             ,


-- ** getDetailWidthChars #method:getDetailWidthChars#

#if defined(ENABLE_OVERLOADING)
    CalendarGetDetailWidthCharsMethodInfo   ,
#endif
    calendarGetDetailWidthChars             ,


-- ** getDisplayOptions #method:getDisplayOptions#

#if defined(ENABLE_OVERLOADING)
    CalendarGetDisplayOptionsMethodInfo     ,
#endif
    calendarGetDisplayOptions               ,


-- ** markDay #method:markDay#

#if defined(ENABLE_OVERLOADING)
    CalendarMarkDayMethodInfo               ,
#endif
    calendarMarkDay                         ,


-- ** new #method:new#

    calendarNew                             ,


-- ** selectDay #method:selectDay#

#if defined(ENABLE_OVERLOADING)
    CalendarSelectDayMethodInfo             ,
#endif
    calendarSelectDay                       ,


-- ** selectMonth #method:selectMonth#

#if defined(ENABLE_OVERLOADING)
    CalendarSelectMonthMethodInfo           ,
#endif
    calendarSelectMonth                     ,


-- ** setDetailFunc #method:setDetailFunc#

#if defined(ENABLE_OVERLOADING)
    CalendarSetDetailFuncMethodInfo         ,
#endif
    calendarSetDetailFunc                   ,


-- ** setDetailHeightRows #method:setDetailHeightRows#

#if defined(ENABLE_OVERLOADING)
    CalendarSetDetailHeightRowsMethodInfo   ,
#endif
    calendarSetDetailHeightRows             ,


-- ** setDetailWidthChars #method:setDetailWidthChars#

#if defined(ENABLE_OVERLOADING)
    CalendarSetDetailWidthCharsMethodInfo   ,
#endif
    calendarSetDetailWidthChars             ,


-- ** setDisplayOptions #method:setDisplayOptions#

#if defined(ENABLE_OVERLOADING)
    CalendarSetDisplayOptionsMethodInfo     ,
#endif
    calendarSetDisplayOptions               ,


-- ** unmarkDay #method:unmarkDay#

#if defined(ENABLE_OVERLOADING)
    CalendarUnmarkDayMethodInfo             ,
#endif
    calendarUnmarkDay                       ,




 -- * Properties


-- ** day #attr:day#
-- | The selected day (as a number between 1 and 31, or 0
-- to unselect the currently selected day).
-- This property gets initially set to the current day.

#if defined(ENABLE_OVERLOADING)
    CalendarDayPropertyInfo                 ,
#endif
#if defined(ENABLE_OVERLOADING)
    calendarDay                             ,
#endif
    constructCalendarDay                    ,
    getCalendarDay                          ,
    setCalendarDay                          ,


-- ** detailHeightRows #attr:detailHeightRows#
-- | Height of a detail cell, in rows.
-- A value of 0 allows any width. See 'GI.Gtk.Objects.Calendar.calendarSetDetailFunc'.
-- 
-- /Since: 2.14/

#if defined(ENABLE_OVERLOADING)
    CalendarDetailHeightRowsPropertyInfo    ,
#endif
#if defined(ENABLE_OVERLOADING)
    calendarDetailHeightRows                ,
#endif
    constructCalendarDetailHeightRows       ,
    getCalendarDetailHeightRows             ,
    setCalendarDetailHeightRows             ,


-- ** detailWidthChars #attr:detailWidthChars#
-- | Width of a detail cell, in characters.
-- A value of 0 allows any width. See 'GI.Gtk.Objects.Calendar.calendarSetDetailFunc'.
-- 
-- /Since: 2.14/

#if defined(ENABLE_OVERLOADING)
    CalendarDetailWidthCharsPropertyInfo    ,
#endif
#if defined(ENABLE_OVERLOADING)
    calendarDetailWidthChars                ,
#endif
    constructCalendarDetailWidthChars       ,
    getCalendarDetailWidthChars             ,
    setCalendarDetailWidthChars             ,


-- ** month #attr:month#
-- | The selected month (as a number between 0 and 11).
-- This property gets initially set to the current month.

#if defined(ENABLE_OVERLOADING)
    CalendarMonthPropertyInfo               ,
#endif
#if defined(ENABLE_OVERLOADING)
    calendarMonth                           ,
#endif
    constructCalendarMonth                  ,
    getCalendarMonth                        ,
    setCalendarMonth                        ,


-- ** noMonthChange #attr:noMonthChange#
-- | Determines whether the selected month can be changed.
-- 
-- /Since: 2.4/

#if defined(ENABLE_OVERLOADING)
    CalendarNoMonthChangePropertyInfo       ,
#endif
#if defined(ENABLE_OVERLOADING)
    calendarNoMonthChange                   ,
#endif
    constructCalendarNoMonthChange          ,
    getCalendarNoMonthChange                ,
    setCalendarNoMonthChange                ,


-- ** showDayNames #attr:showDayNames#
-- | Determines whether day names are displayed.
-- 
-- /Since: 2.4/

#if defined(ENABLE_OVERLOADING)
    CalendarShowDayNamesPropertyInfo        ,
#endif
#if defined(ENABLE_OVERLOADING)
    calendarShowDayNames                    ,
#endif
    constructCalendarShowDayNames           ,
    getCalendarShowDayNames                 ,
    setCalendarShowDayNames                 ,


-- ** showDetails #attr:showDetails#
-- | Determines whether details are shown directly in the widget, or if they are
-- available only as tooltip. When this property is set days with details are
-- marked.
-- 
-- /Since: 2.14/

#if defined(ENABLE_OVERLOADING)
    CalendarShowDetailsPropertyInfo         ,
#endif
#if defined(ENABLE_OVERLOADING)
    calendarShowDetails                     ,
#endif
    constructCalendarShowDetails            ,
    getCalendarShowDetails                  ,
    setCalendarShowDetails                  ,


-- ** showHeading #attr:showHeading#
-- | Determines whether a heading is displayed.
-- 
-- /Since: 2.4/

#if defined(ENABLE_OVERLOADING)
    CalendarShowHeadingPropertyInfo         ,
#endif
#if defined(ENABLE_OVERLOADING)
    calendarShowHeading                     ,
#endif
    constructCalendarShowHeading            ,
    getCalendarShowHeading                  ,
    setCalendarShowHeading                  ,


-- ** showWeekNumbers #attr:showWeekNumbers#
-- | Determines whether week numbers are displayed.
-- 
-- /Since: 2.4/

#if defined(ENABLE_OVERLOADING)
    CalendarShowWeekNumbersPropertyInfo     ,
#endif
#if defined(ENABLE_OVERLOADING)
    calendarShowWeekNumbers                 ,
#endif
    constructCalendarShowWeekNumbers        ,
    getCalendarShowWeekNumbers              ,
    setCalendarShowWeekNumbers              ,


-- ** year #attr:year#
-- | The selected year.
-- This property gets initially set to the current year.

#if defined(ENABLE_OVERLOADING)
    CalendarYearPropertyInfo                ,
#endif
#if defined(ENABLE_OVERLOADING)
    calendarYear                            ,
#endif
    constructCalendarYear                   ,
    getCalendarYear                         ,
    setCalendarYear                         ,




 -- * Signals


-- ** daySelected #signal:daySelected#

    CalendarDaySelectedCallback             ,
#if defined(ENABLE_OVERLOADING)
    CalendarDaySelectedSignalInfo           ,
#endif
    afterCalendarDaySelected                ,
    onCalendarDaySelected                   ,


-- ** daySelectedDoubleClick #signal:daySelectedDoubleClick#

    CalendarDaySelectedDoubleClickCallback  ,
#if defined(ENABLE_OVERLOADING)
    CalendarDaySelectedDoubleClickSignalInfo,
#endif
    afterCalendarDaySelectedDoubleClick     ,
    onCalendarDaySelectedDoubleClick        ,


-- ** monthChanged #signal:monthChanged#

    CalendarMonthChangedCallback            ,
#if defined(ENABLE_OVERLOADING)
    CalendarMonthChangedSignalInfo          ,
#endif
    afterCalendarMonthChanged               ,
    onCalendarMonthChanged                  ,


-- ** nextMonth #signal:nextMonth#

    CalendarNextMonthCallback               ,
#if defined(ENABLE_OVERLOADING)
    CalendarNextMonthSignalInfo             ,
#endif
    afterCalendarNextMonth                  ,
    onCalendarNextMonth                     ,


-- ** nextYear #signal:nextYear#

    CalendarNextYearCallback                ,
#if defined(ENABLE_OVERLOADING)
    CalendarNextYearSignalInfo              ,
#endif
    afterCalendarNextYear                   ,
    onCalendarNextYear                      ,


-- ** prevMonth #signal:prevMonth#

    CalendarPrevMonthCallback               ,
#if defined(ENABLE_OVERLOADING)
    CalendarPrevMonthSignalInfo             ,
#endif
    afterCalendarPrevMonth                  ,
    onCalendarPrevMonth                     ,


-- ** prevYear #signal:prevYear#

    CalendarPrevYearCallback                ,
#if defined(ENABLE_OVERLOADING)
    CalendarPrevYearSignalInfo              ,
#endif
    afterCalendarPrevYear                   ,
    onCalendarPrevYear                      ,




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
import qualified GI.GLib.Callbacks as GLib.Callbacks
import qualified GI.GObject.Objects.Object as GObject.Object
import qualified GI.Gtk.Callbacks as Gtk.Callbacks
import {-# SOURCE #-} qualified GI.Gtk.Flags as Gtk.Flags
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype Calendar = Calendar (SP.ManagedPtr Calendar)
    deriving (Eq)

instance SP.ManagedPtrNewtype Calendar where
    toManagedPtr (Calendar p) = p

foreign import ccall "gtk_calendar_get_type"
    c_gtk_calendar_get_type :: IO B.Types.GType

instance B.Types.TypedObject Calendar where
    glibType = c_gtk_calendar_get_type

instance B.Types.GObject Calendar

-- | Type class for types which can be safely cast to `Calendar`, for instance with `toCalendar`.
class (SP.GObject o, O.IsDescendantOf Calendar o) => IsCalendar o
instance (SP.GObject o, O.IsDescendantOf Calendar o) => IsCalendar o

instance O.HasParentTypes Calendar
type instance O.ParentTypes Calendar = '[Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable]

-- | Cast to `Calendar`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toCalendar :: (MIO.MonadIO m, IsCalendar o) => o -> m Calendar
toCalendar = MIO.liftIO . B.ManagedPtr.unsafeCastTo Calendar

-- | Convert 'Calendar' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe Calendar) where
    gvalueGType_ = c_gtk_calendar_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr Calendar)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr Calendar)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject Calendar ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveCalendarMethod (t :: Symbol) (o :: *) :: * where
    ResolveCalendarMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveCalendarMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveCalendarMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveCalendarMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveCalendarMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveCalendarMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveCalendarMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveCalendarMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveCalendarMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveCalendarMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveCalendarMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveCalendarMethod "childNotify" o = Gtk.Widget.WidgetChildNotifyMethodInfo
    ResolveCalendarMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveCalendarMethod "clearMarks" o = CalendarClearMarksMethodInfo
    ResolveCalendarMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveCalendarMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveCalendarMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveCalendarMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveCalendarMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveCalendarMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveCalendarMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveCalendarMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveCalendarMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveCalendarMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveCalendarMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveCalendarMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveCalendarMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveCalendarMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveCalendarMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveCalendarMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveCalendarMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveCalendarMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveCalendarMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveCalendarMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveCalendarMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveCalendarMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveCalendarMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveCalendarMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveCalendarMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveCalendarMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveCalendarMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveCalendarMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveCalendarMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveCalendarMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveCalendarMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveCalendarMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveCalendarMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveCalendarMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveCalendarMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveCalendarMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveCalendarMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveCalendarMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveCalendarMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveCalendarMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveCalendarMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveCalendarMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveCalendarMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveCalendarMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveCalendarMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveCalendarMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveCalendarMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveCalendarMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveCalendarMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveCalendarMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveCalendarMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveCalendarMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveCalendarMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveCalendarMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveCalendarMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveCalendarMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveCalendarMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveCalendarMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveCalendarMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveCalendarMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveCalendarMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveCalendarMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveCalendarMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveCalendarMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveCalendarMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveCalendarMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveCalendarMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveCalendarMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveCalendarMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveCalendarMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveCalendarMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveCalendarMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveCalendarMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveCalendarMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveCalendarMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveCalendarMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveCalendarMethod "markDay" o = CalendarMarkDayMethodInfo
    ResolveCalendarMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveCalendarMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveCalendarMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveCalendarMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveCalendarMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveCalendarMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveCalendarMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveCalendarMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveCalendarMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveCalendarMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveCalendarMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveCalendarMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveCalendarMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveCalendarMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveCalendarMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveCalendarMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveCalendarMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveCalendarMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveCalendarMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveCalendarMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveCalendarMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveCalendarMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveCalendarMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveCalendarMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveCalendarMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveCalendarMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveCalendarMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveCalendarMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveCalendarMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveCalendarMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveCalendarMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveCalendarMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveCalendarMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveCalendarMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveCalendarMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveCalendarMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveCalendarMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveCalendarMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveCalendarMethod "selectDay" o = CalendarSelectDayMethodInfo
    ResolveCalendarMethod "selectMonth" o = CalendarSelectMonthMethodInfo
    ResolveCalendarMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveCalendarMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveCalendarMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveCalendarMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveCalendarMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveCalendarMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveCalendarMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveCalendarMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveCalendarMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveCalendarMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveCalendarMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveCalendarMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveCalendarMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveCalendarMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveCalendarMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveCalendarMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveCalendarMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveCalendarMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveCalendarMethod "unmarkDay" o = CalendarUnmarkDayMethodInfo
    ResolveCalendarMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveCalendarMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveCalendarMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveCalendarMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveCalendarMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveCalendarMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveCalendarMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveCalendarMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveCalendarMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveCalendarMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveCalendarMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveCalendarMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveCalendarMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveCalendarMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveCalendarMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveCalendarMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveCalendarMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveCalendarMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveCalendarMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveCalendarMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveCalendarMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveCalendarMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveCalendarMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveCalendarMethod "getDate" o = CalendarGetDateMethodInfo
    ResolveCalendarMethod "getDayIsMarked" o = CalendarGetDayIsMarkedMethodInfo
    ResolveCalendarMethod "getDetailHeightRows" o = CalendarGetDetailHeightRowsMethodInfo
    ResolveCalendarMethod "getDetailWidthChars" o = CalendarGetDetailWidthCharsMethodInfo
    ResolveCalendarMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveCalendarMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveCalendarMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveCalendarMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveCalendarMethod "getDisplayOptions" o = CalendarGetDisplayOptionsMethodInfo
    ResolveCalendarMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveCalendarMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveCalendarMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveCalendarMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveCalendarMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveCalendarMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveCalendarMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveCalendarMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveCalendarMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveCalendarMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveCalendarMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveCalendarMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveCalendarMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveCalendarMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveCalendarMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveCalendarMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveCalendarMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveCalendarMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveCalendarMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveCalendarMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveCalendarMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveCalendarMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveCalendarMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveCalendarMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveCalendarMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveCalendarMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveCalendarMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveCalendarMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveCalendarMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveCalendarMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveCalendarMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveCalendarMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveCalendarMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveCalendarMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveCalendarMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveCalendarMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveCalendarMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveCalendarMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveCalendarMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveCalendarMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveCalendarMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveCalendarMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveCalendarMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveCalendarMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveCalendarMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveCalendarMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveCalendarMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveCalendarMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveCalendarMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveCalendarMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveCalendarMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveCalendarMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveCalendarMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveCalendarMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveCalendarMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveCalendarMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveCalendarMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveCalendarMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveCalendarMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveCalendarMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveCalendarMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveCalendarMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveCalendarMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveCalendarMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveCalendarMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveCalendarMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveCalendarMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveCalendarMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveCalendarMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveCalendarMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveCalendarMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveCalendarMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveCalendarMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveCalendarMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveCalendarMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveCalendarMethod "setDetailFunc" o = CalendarSetDetailFuncMethodInfo
    ResolveCalendarMethod "setDetailHeightRows" o = CalendarSetDetailHeightRowsMethodInfo
    ResolveCalendarMethod "setDetailWidthChars" o = CalendarSetDetailWidthCharsMethodInfo
    ResolveCalendarMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveCalendarMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveCalendarMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveCalendarMethod "setDisplayOptions" o = CalendarSetDisplayOptionsMethodInfo
    ResolveCalendarMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveCalendarMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveCalendarMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveCalendarMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveCalendarMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveCalendarMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveCalendarMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveCalendarMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveCalendarMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveCalendarMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveCalendarMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveCalendarMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveCalendarMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveCalendarMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveCalendarMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveCalendarMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveCalendarMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveCalendarMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveCalendarMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveCalendarMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveCalendarMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveCalendarMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveCalendarMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveCalendarMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveCalendarMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveCalendarMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveCalendarMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveCalendarMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveCalendarMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveCalendarMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveCalendarMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveCalendarMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveCalendarMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveCalendarMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveCalendarMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveCalendarMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveCalendarMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveCalendarMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveCalendarMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveCalendarMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveCalendarMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveCalendarMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveCalendarMethod t Calendar, O.OverloadedMethod info Calendar p) => OL.IsLabel t (Calendar -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveCalendarMethod t Calendar, O.OverloadedMethod info Calendar p, R.HasField t Calendar p) => R.HasField t Calendar p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveCalendarMethod t Calendar, O.OverloadedMethodInfo info Calendar) => OL.IsLabel t (O.MethodProxy info Calendar) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal Calendar::day-selected
-- | Emitted when the user selects a day.
type CalendarDaySelectedCallback =
    IO ()

type C_CalendarDaySelectedCallback =
    Ptr Calendar ->                         -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_CalendarDaySelectedCallback`.
foreign import ccall "wrapper"
    mk_CalendarDaySelectedCallback :: C_CalendarDaySelectedCallback -> IO (FunPtr C_CalendarDaySelectedCallback)

wrap_CalendarDaySelectedCallback :: 
    GObject a => (a -> CalendarDaySelectedCallback) ->
    C_CalendarDaySelectedCallback
wrap_CalendarDaySelectedCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [daySelected](#signal:daySelected) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' calendar #daySelected callback
-- @
-- 
-- 
onCalendarDaySelected :: (IsCalendar a, MonadIO m) => a -> ((?self :: a) => CalendarDaySelectedCallback) -> m SignalHandlerId
onCalendarDaySelected obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CalendarDaySelectedCallback wrapped
    wrapped'' <- mk_CalendarDaySelectedCallback wrapped'
    connectSignalFunPtr obj "day-selected" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [daySelected](#signal:daySelected) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' calendar #daySelected callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterCalendarDaySelected :: (IsCalendar a, MonadIO m) => a -> ((?self :: a) => CalendarDaySelectedCallback) -> m SignalHandlerId
afterCalendarDaySelected obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CalendarDaySelectedCallback wrapped
    wrapped'' <- mk_CalendarDaySelectedCallback wrapped'
    connectSignalFunPtr obj "day-selected" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data CalendarDaySelectedSignalInfo
instance SignalInfo CalendarDaySelectedSignalInfo where
    type HaskellCallbackType CalendarDaySelectedSignalInfo = CalendarDaySelectedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_CalendarDaySelectedCallback cb
        cb'' <- mk_CalendarDaySelectedCallback cb'
        connectSignalFunPtr obj "day-selected" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Calendar::day-selected"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Calendar.html#g:signal:daySelected"})

#endif

-- signal Calendar::day-selected-double-click
-- | Emitted when the user double-clicks a day.
type CalendarDaySelectedDoubleClickCallback =
    IO ()

type C_CalendarDaySelectedDoubleClickCallback =
    Ptr Calendar ->                         -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_CalendarDaySelectedDoubleClickCallback`.
foreign import ccall "wrapper"
    mk_CalendarDaySelectedDoubleClickCallback :: C_CalendarDaySelectedDoubleClickCallback -> IO (FunPtr C_CalendarDaySelectedDoubleClickCallback)

wrap_CalendarDaySelectedDoubleClickCallback :: 
    GObject a => (a -> CalendarDaySelectedDoubleClickCallback) ->
    C_CalendarDaySelectedDoubleClickCallback
wrap_CalendarDaySelectedDoubleClickCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [daySelectedDoubleClick](#signal:daySelectedDoubleClick) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' calendar #daySelectedDoubleClick callback
-- @
-- 
-- 
onCalendarDaySelectedDoubleClick :: (IsCalendar a, MonadIO m) => a -> ((?self :: a) => CalendarDaySelectedDoubleClickCallback) -> m SignalHandlerId
onCalendarDaySelectedDoubleClick obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CalendarDaySelectedDoubleClickCallback wrapped
    wrapped'' <- mk_CalendarDaySelectedDoubleClickCallback wrapped'
    connectSignalFunPtr obj "day-selected-double-click" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [daySelectedDoubleClick](#signal:daySelectedDoubleClick) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' calendar #daySelectedDoubleClick callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterCalendarDaySelectedDoubleClick :: (IsCalendar a, MonadIO m) => a -> ((?self :: a) => CalendarDaySelectedDoubleClickCallback) -> m SignalHandlerId
afterCalendarDaySelectedDoubleClick obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CalendarDaySelectedDoubleClickCallback wrapped
    wrapped'' <- mk_CalendarDaySelectedDoubleClickCallback wrapped'
    connectSignalFunPtr obj "day-selected-double-click" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data CalendarDaySelectedDoubleClickSignalInfo
instance SignalInfo CalendarDaySelectedDoubleClickSignalInfo where
    type HaskellCallbackType CalendarDaySelectedDoubleClickSignalInfo = CalendarDaySelectedDoubleClickCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_CalendarDaySelectedDoubleClickCallback cb
        cb'' <- mk_CalendarDaySelectedDoubleClickCallback cb'
        connectSignalFunPtr obj "day-selected-double-click" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Calendar::day-selected-double-click"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Calendar.html#g:signal:daySelectedDoubleClick"})

#endif

-- signal Calendar::month-changed
-- | Emitted when the user clicks a button to change the selected month on a
-- calendar.
type CalendarMonthChangedCallback =
    IO ()

type C_CalendarMonthChangedCallback =
    Ptr Calendar ->                         -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_CalendarMonthChangedCallback`.
foreign import ccall "wrapper"
    mk_CalendarMonthChangedCallback :: C_CalendarMonthChangedCallback -> IO (FunPtr C_CalendarMonthChangedCallback)

wrap_CalendarMonthChangedCallback :: 
    GObject a => (a -> CalendarMonthChangedCallback) ->
    C_CalendarMonthChangedCallback
wrap_CalendarMonthChangedCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [monthChanged](#signal:monthChanged) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' calendar #monthChanged callback
-- @
-- 
-- 
onCalendarMonthChanged :: (IsCalendar a, MonadIO m) => a -> ((?self :: a) => CalendarMonthChangedCallback) -> m SignalHandlerId
onCalendarMonthChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CalendarMonthChangedCallback wrapped
    wrapped'' <- mk_CalendarMonthChangedCallback wrapped'
    connectSignalFunPtr obj "month-changed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [monthChanged](#signal:monthChanged) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' calendar #monthChanged callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterCalendarMonthChanged :: (IsCalendar a, MonadIO m) => a -> ((?self :: a) => CalendarMonthChangedCallback) -> m SignalHandlerId
afterCalendarMonthChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CalendarMonthChangedCallback wrapped
    wrapped'' <- mk_CalendarMonthChangedCallback wrapped'
    connectSignalFunPtr obj "month-changed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data CalendarMonthChangedSignalInfo
instance SignalInfo CalendarMonthChangedSignalInfo where
    type HaskellCallbackType CalendarMonthChangedSignalInfo = CalendarMonthChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_CalendarMonthChangedCallback cb
        cb'' <- mk_CalendarMonthChangedCallback cb'
        connectSignalFunPtr obj "month-changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Calendar::month-changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Calendar.html#g:signal:monthChanged"})

#endif

-- signal Calendar::next-month
-- | Emitted when the user switched to the next month.
type CalendarNextMonthCallback =
    IO ()

type C_CalendarNextMonthCallback =
    Ptr Calendar ->                         -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_CalendarNextMonthCallback`.
foreign import ccall "wrapper"
    mk_CalendarNextMonthCallback :: C_CalendarNextMonthCallback -> IO (FunPtr C_CalendarNextMonthCallback)

wrap_CalendarNextMonthCallback :: 
    GObject a => (a -> CalendarNextMonthCallback) ->
    C_CalendarNextMonthCallback
wrap_CalendarNextMonthCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [nextMonth](#signal:nextMonth) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' calendar #nextMonth callback
-- @
-- 
-- 
onCalendarNextMonth :: (IsCalendar a, MonadIO m) => a -> ((?self :: a) => CalendarNextMonthCallback) -> m SignalHandlerId
onCalendarNextMonth obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CalendarNextMonthCallback wrapped
    wrapped'' <- mk_CalendarNextMonthCallback wrapped'
    connectSignalFunPtr obj "next-month" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [nextMonth](#signal:nextMonth) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' calendar #nextMonth callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterCalendarNextMonth :: (IsCalendar a, MonadIO m) => a -> ((?self :: a) => CalendarNextMonthCallback) -> m SignalHandlerId
afterCalendarNextMonth obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CalendarNextMonthCallback wrapped
    wrapped'' <- mk_CalendarNextMonthCallback wrapped'
    connectSignalFunPtr obj "next-month" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data CalendarNextMonthSignalInfo
instance SignalInfo CalendarNextMonthSignalInfo where
    type HaskellCallbackType CalendarNextMonthSignalInfo = CalendarNextMonthCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_CalendarNextMonthCallback cb
        cb'' <- mk_CalendarNextMonthCallback cb'
        connectSignalFunPtr obj "next-month" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Calendar::next-month"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Calendar.html#g:signal:nextMonth"})

#endif

-- signal Calendar::next-year
-- | Emitted when user switched to the next year.
type CalendarNextYearCallback =
    IO ()

type C_CalendarNextYearCallback =
    Ptr Calendar ->                         -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_CalendarNextYearCallback`.
foreign import ccall "wrapper"
    mk_CalendarNextYearCallback :: C_CalendarNextYearCallback -> IO (FunPtr C_CalendarNextYearCallback)

wrap_CalendarNextYearCallback :: 
    GObject a => (a -> CalendarNextYearCallback) ->
    C_CalendarNextYearCallback
wrap_CalendarNextYearCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [nextYear](#signal:nextYear) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' calendar #nextYear callback
-- @
-- 
-- 
onCalendarNextYear :: (IsCalendar a, MonadIO m) => a -> ((?self :: a) => CalendarNextYearCallback) -> m SignalHandlerId
onCalendarNextYear obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CalendarNextYearCallback wrapped
    wrapped'' <- mk_CalendarNextYearCallback wrapped'
    connectSignalFunPtr obj "next-year" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [nextYear](#signal:nextYear) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' calendar #nextYear callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterCalendarNextYear :: (IsCalendar a, MonadIO m) => a -> ((?self :: a) => CalendarNextYearCallback) -> m SignalHandlerId
afterCalendarNextYear obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CalendarNextYearCallback wrapped
    wrapped'' <- mk_CalendarNextYearCallback wrapped'
    connectSignalFunPtr obj "next-year" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data CalendarNextYearSignalInfo
instance SignalInfo CalendarNextYearSignalInfo where
    type HaskellCallbackType CalendarNextYearSignalInfo = CalendarNextYearCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_CalendarNextYearCallback cb
        cb'' <- mk_CalendarNextYearCallback cb'
        connectSignalFunPtr obj "next-year" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Calendar::next-year"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Calendar.html#g:signal:nextYear"})

#endif

-- signal Calendar::prev-month
-- | Emitted when the user switched to the previous month.
type CalendarPrevMonthCallback =
    IO ()

type C_CalendarPrevMonthCallback =
    Ptr Calendar ->                         -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_CalendarPrevMonthCallback`.
foreign import ccall "wrapper"
    mk_CalendarPrevMonthCallback :: C_CalendarPrevMonthCallback -> IO (FunPtr C_CalendarPrevMonthCallback)

wrap_CalendarPrevMonthCallback :: 
    GObject a => (a -> CalendarPrevMonthCallback) ->
    C_CalendarPrevMonthCallback
wrap_CalendarPrevMonthCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [prevMonth](#signal:prevMonth) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' calendar #prevMonth callback
-- @
-- 
-- 
onCalendarPrevMonth :: (IsCalendar a, MonadIO m) => a -> ((?self :: a) => CalendarPrevMonthCallback) -> m SignalHandlerId
onCalendarPrevMonth obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CalendarPrevMonthCallback wrapped
    wrapped'' <- mk_CalendarPrevMonthCallback wrapped'
    connectSignalFunPtr obj "prev-month" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [prevMonth](#signal:prevMonth) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' calendar #prevMonth callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterCalendarPrevMonth :: (IsCalendar a, MonadIO m) => a -> ((?self :: a) => CalendarPrevMonthCallback) -> m SignalHandlerId
afterCalendarPrevMonth obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CalendarPrevMonthCallback wrapped
    wrapped'' <- mk_CalendarPrevMonthCallback wrapped'
    connectSignalFunPtr obj "prev-month" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data CalendarPrevMonthSignalInfo
instance SignalInfo CalendarPrevMonthSignalInfo where
    type HaskellCallbackType CalendarPrevMonthSignalInfo = CalendarPrevMonthCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_CalendarPrevMonthCallback cb
        cb'' <- mk_CalendarPrevMonthCallback cb'
        connectSignalFunPtr obj "prev-month" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Calendar::prev-month"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Calendar.html#g:signal:prevMonth"})

#endif

-- signal Calendar::prev-year
-- | Emitted when user switched to the previous year.
type CalendarPrevYearCallback =
    IO ()

type C_CalendarPrevYearCallback =
    Ptr Calendar ->                         -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_CalendarPrevYearCallback`.
foreign import ccall "wrapper"
    mk_CalendarPrevYearCallback :: C_CalendarPrevYearCallback -> IO (FunPtr C_CalendarPrevYearCallback)

wrap_CalendarPrevYearCallback :: 
    GObject a => (a -> CalendarPrevYearCallback) ->
    C_CalendarPrevYearCallback
wrap_CalendarPrevYearCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [prevYear](#signal:prevYear) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' calendar #prevYear callback
-- @
-- 
-- 
onCalendarPrevYear :: (IsCalendar a, MonadIO m) => a -> ((?self :: a) => CalendarPrevYearCallback) -> m SignalHandlerId
onCalendarPrevYear obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CalendarPrevYearCallback wrapped
    wrapped'' <- mk_CalendarPrevYearCallback wrapped'
    connectSignalFunPtr obj "prev-year" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [prevYear](#signal:prevYear) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' calendar #prevYear callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterCalendarPrevYear :: (IsCalendar a, MonadIO m) => a -> ((?self :: a) => CalendarPrevYearCallback) -> m SignalHandlerId
afterCalendarPrevYear obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CalendarPrevYearCallback wrapped
    wrapped'' <- mk_CalendarPrevYearCallback wrapped'
    connectSignalFunPtr obj "prev-year" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data CalendarPrevYearSignalInfo
instance SignalInfo CalendarPrevYearSignalInfo where
    type HaskellCallbackType CalendarPrevYearSignalInfo = CalendarPrevYearCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_CalendarPrevYearCallback cb
        cb'' <- mk_CalendarPrevYearCallback cb'
        connectSignalFunPtr obj "prev-year" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Calendar::prev-year"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Calendar.html#g:signal:prevYear"})

#endif

-- VVV Prop "day"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@day@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' calendar #day
-- @
getCalendarDay :: (MonadIO m, IsCalendar o) => o -> m Int32
getCalendarDay obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "day"

-- | Set the value of the “@day@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' calendar [ #day 'Data.GI.Base.Attributes.:=' value ]
-- @
setCalendarDay :: (MonadIO m, IsCalendar o) => o -> Int32 -> m ()
setCalendarDay obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "day" val

-- | Construct a `GValueConstruct` with valid value for the “@day@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCalendarDay :: (IsCalendar o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructCalendarDay val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "day" val

#if defined(ENABLE_OVERLOADING)
data CalendarDayPropertyInfo
instance AttrInfo CalendarDayPropertyInfo where
    type AttrAllowedOps CalendarDayPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CalendarDayPropertyInfo = IsCalendar
    type AttrSetTypeConstraint CalendarDayPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint CalendarDayPropertyInfo = (~) Int32
    type AttrTransferType CalendarDayPropertyInfo = Int32
    type AttrGetType CalendarDayPropertyInfo = Int32
    type AttrLabel CalendarDayPropertyInfo = "day"
    type AttrOrigin CalendarDayPropertyInfo = Calendar
    attrGet = getCalendarDay
    attrSet = setCalendarDay
    attrTransfer _ v = do
        return v
    attrConstruct = constructCalendarDay
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Calendar.day"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Calendar.html#g:attr:day"
        })
#endif

-- VVV Prop "detail-height-rows"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@detail-height-rows@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' calendar #detailHeightRows
-- @
getCalendarDetailHeightRows :: (MonadIO m, IsCalendar o) => o -> m Int32
getCalendarDetailHeightRows obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "detail-height-rows"

-- | Set the value of the “@detail-height-rows@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' calendar [ #detailHeightRows 'Data.GI.Base.Attributes.:=' value ]
-- @
setCalendarDetailHeightRows :: (MonadIO m, IsCalendar o) => o -> Int32 -> m ()
setCalendarDetailHeightRows obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "detail-height-rows" val

-- | Construct a `GValueConstruct` with valid value for the “@detail-height-rows@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCalendarDetailHeightRows :: (IsCalendar o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructCalendarDetailHeightRows val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "detail-height-rows" val

#if defined(ENABLE_OVERLOADING)
data CalendarDetailHeightRowsPropertyInfo
instance AttrInfo CalendarDetailHeightRowsPropertyInfo where
    type AttrAllowedOps CalendarDetailHeightRowsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CalendarDetailHeightRowsPropertyInfo = IsCalendar
    type AttrSetTypeConstraint CalendarDetailHeightRowsPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint CalendarDetailHeightRowsPropertyInfo = (~) Int32
    type AttrTransferType CalendarDetailHeightRowsPropertyInfo = Int32
    type AttrGetType CalendarDetailHeightRowsPropertyInfo = Int32
    type AttrLabel CalendarDetailHeightRowsPropertyInfo = "detail-height-rows"
    type AttrOrigin CalendarDetailHeightRowsPropertyInfo = Calendar
    attrGet = getCalendarDetailHeightRows
    attrSet = setCalendarDetailHeightRows
    attrTransfer _ v = do
        return v
    attrConstruct = constructCalendarDetailHeightRows
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Calendar.detailHeightRows"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Calendar.html#g:attr:detailHeightRows"
        })
#endif

-- VVV Prop "detail-width-chars"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@detail-width-chars@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' calendar #detailWidthChars
-- @
getCalendarDetailWidthChars :: (MonadIO m, IsCalendar o) => o -> m Int32
getCalendarDetailWidthChars obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "detail-width-chars"

-- | Set the value of the “@detail-width-chars@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' calendar [ #detailWidthChars 'Data.GI.Base.Attributes.:=' value ]
-- @
setCalendarDetailWidthChars :: (MonadIO m, IsCalendar o) => o -> Int32 -> m ()
setCalendarDetailWidthChars obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "detail-width-chars" val

-- | Construct a `GValueConstruct` with valid value for the “@detail-width-chars@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCalendarDetailWidthChars :: (IsCalendar o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructCalendarDetailWidthChars val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "detail-width-chars" val

#if defined(ENABLE_OVERLOADING)
data CalendarDetailWidthCharsPropertyInfo
instance AttrInfo CalendarDetailWidthCharsPropertyInfo where
    type AttrAllowedOps CalendarDetailWidthCharsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CalendarDetailWidthCharsPropertyInfo = IsCalendar
    type AttrSetTypeConstraint CalendarDetailWidthCharsPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint CalendarDetailWidthCharsPropertyInfo = (~) Int32
    type AttrTransferType CalendarDetailWidthCharsPropertyInfo = Int32
    type AttrGetType CalendarDetailWidthCharsPropertyInfo = Int32
    type AttrLabel CalendarDetailWidthCharsPropertyInfo = "detail-width-chars"
    type AttrOrigin CalendarDetailWidthCharsPropertyInfo = Calendar
    attrGet = getCalendarDetailWidthChars
    attrSet = setCalendarDetailWidthChars
    attrTransfer _ v = do
        return v
    attrConstruct = constructCalendarDetailWidthChars
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Calendar.detailWidthChars"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Calendar.html#g:attr:detailWidthChars"
        })
#endif

-- VVV Prop "month"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@month@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' calendar #month
-- @
getCalendarMonth :: (MonadIO m, IsCalendar o) => o -> m Int32
getCalendarMonth obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "month"

-- | Set the value of the “@month@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' calendar [ #month 'Data.GI.Base.Attributes.:=' value ]
-- @
setCalendarMonth :: (MonadIO m, IsCalendar o) => o -> Int32 -> m ()
setCalendarMonth obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "month" val

-- | Construct a `GValueConstruct` with valid value for the “@month@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCalendarMonth :: (IsCalendar o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructCalendarMonth val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "month" val

#if defined(ENABLE_OVERLOADING)
data CalendarMonthPropertyInfo
instance AttrInfo CalendarMonthPropertyInfo where
    type AttrAllowedOps CalendarMonthPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CalendarMonthPropertyInfo = IsCalendar
    type AttrSetTypeConstraint CalendarMonthPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint CalendarMonthPropertyInfo = (~) Int32
    type AttrTransferType CalendarMonthPropertyInfo = Int32
    type AttrGetType CalendarMonthPropertyInfo = Int32
    type AttrLabel CalendarMonthPropertyInfo = "month"
    type AttrOrigin CalendarMonthPropertyInfo = Calendar
    attrGet = getCalendarMonth
    attrSet = setCalendarMonth
    attrTransfer _ v = do
        return v
    attrConstruct = constructCalendarMonth
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Calendar.month"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Calendar.html#g:attr:month"
        })
#endif

-- VVV Prop "no-month-change"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@no-month-change@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' calendar #noMonthChange
-- @
getCalendarNoMonthChange :: (MonadIO m, IsCalendar o) => o -> m Bool
getCalendarNoMonthChange obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "no-month-change"

-- | Set the value of the “@no-month-change@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' calendar [ #noMonthChange 'Data.GI.Base.Attributes.:=' value ]
-- @
setCalendarNoMonthChange :: (MonadIO m, IsCalendar o) => o -> Bool -> m ()
setCalendarNoMonthChange obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "no-month-change" val

-- | Construct a `GValueConstruct` with valid value for the “@no-month-change@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCalendarNoMonthChange :: (IsCalendar o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructCalendarNoMonthChange val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "no-month-change" val

#if defined(ENABLE_OVERLOADING)
data CalendarNoMonthChangePropertyInfo
instance AttrInfo CalendarNoMonthChangePropertyInfo where
    type AttrAllowedOps CalendarNoMonthChangePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CalendarNoMonthChangePropertyInfo = IsCalendar
    type AttrSetTypeConstraint CalendarNoMonthChangePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint CalendarNoMonthChangePropertyInfo = (~) Bool
    type AttrTransferType CalendarNoMonthChangePropertyInfo = Bool
    type AttrGetType CalendarNoMonthChangePropertyInfo = Bool
    type AttrLabel CalendarNoMonthChangePropertyInfo = "no-month-change"
    type AttrOrigin CalendarNoMonthChangePropertyInfo = Calendar
    attrGet = getCalendarNoMonthChange
    attrSet = setCalendarNoMonthChange
    attrTransfer _ v = do
        return v
    attrConstruct = constructCalendarNoMonthChange
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Calendar.noMonthChange"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Calendar.html#g:attr:noMonthChange"
        })
#endif

-- VVV Prop "show-day-names"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@show-day-names@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' calendar #showDayNames
-- @
getCalendarShowDayNames :: (MonadIO m, IsCalendar o) => o -> m Bool
getCalendarShowDayNames obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "show-day-names"

-- | Set the value of the “@show-day-names@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' calendar [ #showDayNames 'Data.GI.Base.Attributes.:=' value ]
-- @
setCalendarShowDayNames :: (MonadIO m, IsCalendar o) => o -> Bool -> m ()
setCalendarShowDayNames obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "show-day-names" val

-- | Construct a `GValueConstruct` with valid value for the “@show-day-names@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCalendarShowDayNames :: (IsCalendar o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructCalendarShowDayNames val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "show-day-names" val

#if defined(ENABLE_OVERLOADING)
data CalendarShowDayNamesPropertyInfo
instance AttrInfo CalendarShowDayNamesPropertyInfo where
    type AttrAllowedOps CalendarShowDayNamesPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CalendarShowDayNamesPropertyInfo = IsCalendar
    type AttrSetTypeConstraint CalendarShowDayNamesPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint CalendarShowDayNamesPropertyInfo = (~) Bool
    type AttrTransferType CalendarShowDayNamesPropertyInfo = Bool
    type AttrGetType CalendarShowDayNamesPropertyInfo = Bool
    type AttrLabel CalendarShowDayNamesPropertyInfo = "show-day-names"
    type AttrOrigin CalendarShowDayNamesPropertyInfo = Calendar
    attrGet = getCalendarShowDayNames
    attrSet = setCalendarShowDayNames
    attrTransfer _ v = do
        return v
    attrConstruct = constructCalendarShowDayNames
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Calendar.showDayNames"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Calendar.html#g:attr:showDayNames"
        })
#endif

-- VVV Prop "show-details"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@show-details@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' calendar #showDetails
-- @
getCalendarShowDetails :: (MonadIO m, IsCalendar o) => o -> m Bool
getCalendarShowDetails obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "show-details"

-- | Set the value of the “@show-details@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' calendar [ #showDetails 'Data.GI.Base.Attributes.:=' value ]
-- @
setCalendarShowDetails :: (MonadIO m, IsCalendar o) => o -> Bool -> m ()
setCalendarShowDetails obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "show-details" val

-- | Construct a `GValueConstruct` with valid value for the “@show-details@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCalendarShowDetails :: (IsCalendar o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructCalendarShowDetails val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "show-details" val

#if defined(ENABLE_OVERLOADING)
data CalendarShowDetailsPropertyInfo
instance AttrInfo CalendarShowDetailsPropertyInfo where
    type AttrAllowedOps CalendarShowDetailsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CalendarShowDetailsPropertyInfo = IsCalendar
    type AttrSetTypeConstraint CalendarShowDetailsPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint CalendarShowDetailsPropertyInfo = (~) Bool
    type AttrTransferType CalendarShowDetailsPropertyInfo = Bool
    type AttrGetType CalendarShowDetailsPropertyInfo = Bool
    type AttrLabel CalendarShowDetailsPropertyInfo = "show-details"
    type AttrOrigin CalendarShowDetailsPropertyInfo = Calendar
    attrGet = getCalendarShowDetails
    attrSet = setCalendarShowDetails
    attrTransfer _ v = do
        return v
    attrConstruct = constructCalendarShowDetails
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Calendar.showDetails"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Calendar.html#g:attr:showDetails"
        })
#endif

-- VVV Prop "show-heading"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@show-heading@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' calendar #showHeading
-- @
getCalendarShowHeading :: (MonadIO m, IsCalendar o) => o -> m Bool
getCalendarShowHeading obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "show-heading"

-- | Set the value of the “@show-heading@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' calendar [ #showHeading 'Data.GI.Base.Attributes.:=' value ]
-- @
setCalendarShowHeading :: (MonadIO m, IsCalendar o) => o -> Bool -> m ()
setCalendarShowHeading obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "show-heading" val

-- | Construct a `GValueConstruct` with valid value for the “@show-heading@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCalendarShowHeading :: (IsCalendar o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructCalendarShowHeading val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "show-heading" val

#if defined(ENABLE_OVERLOADING)
data CalendarShowHeadingPropertyInfo
instance AttrInfo CalendarShowHeadingPropertyInfo where
    type AttrAllowedOps CalendarShowHeadingPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CalendarShowHeadingPropertyInfo = IsCalendar
    type AttrSetTypeConstraint CalendarShowHeadingPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint CalendarShowHeadingPropertyInfo = (~) Bool
    type AttrTransferType CalendarShowHeadingPropertyInfo = Bool
    type AttrGetType CalendarShowHeadingPropertyInfo = Bool
    type AttrLabel CalendarShowHeadingPropertyInfo = "show-heading"
    type AttrOrigin CalendarShowHeadingPropertyInfo = Calendar
    attrGet = getCalendarShowHeading
    attrSet = setCalendarShowHeading
    attrTransfer _ v = do
        return v
    attrConstruct = constructCalendarShowHeading
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Calendar.showHeading"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Calendar.html#g:attr:showHeading"
        })
#endif

-- VVV Prop "show-week-numbers"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@show-week-numbers@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' calendar #showWeekNumbers
-- @
getCalendarShowWeekNumbers :: (MonadIO m, IsCalendar o) => o -> m Bool
getCalendarShowWeekNumbers obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "show-week-numbers"

-- | Set the value of the “@show-week-numbers@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' calendar [ #showWeekNumbers 'Data.GI.Base.Attributes.:=' value ]
-- @
setCalendarShowWeekNumbers :: (MonadIO m, IsCalendar o) => o -> Bool -> m ()
setCalendarShowWeekNumbers obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "show-week-numbers" val

-- | Construct a `GValueConstruct` with valid value for the “@show-week-numbers@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCalendarShowWeekNumbers :: (IsCalendar o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructCalendarShowWeekNumbers val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "show-week-numbers" val

#if defined(ENABLE_OVERLOADING)
data CalendarShowWeekNumbersPropertyInfo
instance AttrInfo CalendarShowWeekNumbersPropertyInfo where
    type AttrAllowedOps CalendarShowWeekNumbersPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CalendarShowWeekNumbersPropertyInfo = IsCalendar
    type AttrSetTypeConstraint CalendarShowWeekNumbersPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint CalendarShowWeekNumbersPropertyInfo = (~) Bool
    type AttrTransferType CalendarShowWeekNumbersPropertyInfo = Bool
    type AttrGetType CalendarShowWeekNumbersPropertyInfo = Bool
    type AttrLabel CalendarShowWeekNumbersPropertyInfo = "show-week-numbers"
    type AttrOrigin CalendarShowWeekNumbersPropertyInfo = Calendar
    attrGet = getCalendarShowWeekNumbers
    attrSet = setCalendarShowWeekNumbers
    attrTransfer _ v = do
        return v
    attrConstruct = constructCalendarShowWeekNumbers
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Calendar.showWeekNumbers"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Calendar.html#g:attr:showWeekNumbers"
        })
#endif

-- VVV Prop "year"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@year@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' calendar #year
-- @
getCalendarYear :: (MonadIO m, IsCalendar o) => o -> m Int32
getCalendarYear obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "year"

-- | Set the value of the “@year@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' calendar [ #year 'Data.GI.Base.Attributes.:=' value ]
-- @
setCalendarYear :: (MonadIO m, IsCalendar o) => o -> Int32 -> m ()
setCalendarYear obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "year" val

-- | Construct a `GValueConstruct` with valid value for the “@year@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCalendarYear :: (IsCalendar o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructCalendarYear val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "year" val

#if defined(ENABLE_OVERLOADING)
data CalendarYearPropertyInfo
instance AttrInfo CalendarYearPropertyInfo where
    type AttrAllowedOps CalendarYearPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CalendarYearPropertyInfo = IsCalendar
    type AttrSetTypeConstraint CalendarYearPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint CalendarYearPropertyInfo = (~) Int32
    type AttrTransferType CalendarYearPropertyInfo = Int32
    type AttrGetType CalendarYearPropertyInfo = Int32
    type AttrLabel CalendarYearPropertyInfo = "year"
    type AttrOrigin CalendarYearPropertyInfo = Calendar
    attrGet = getCalendarYear
    attrSet = setCalendarYear
    attrTransfer _ v = do
        return v
    attrConstruct = constructCalendarYear
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Calendar.year"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Calendar.html#g:attr:year"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList Calendar
type instance O.AttributeList Calendar = CalendarAttributeList
type CalendarAttributeList = ('[ '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("day", CalendarDayPropertyInfo), '("detailHeightRows", CalendarDetailHeightRowsPropertyInfo), '("detailWidthChars", CalendarDetailWidthCharsPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("month", CalendarMonthPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noMonthChange", CalendarNoMonthChangePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("showDayNames", CalendarShowDayNamesPropertyInfo), '("showDetails", CalendarShowDetailsPropertyInfo), '("showHeading", CalendarShowHeadingPropertyInfo), '("showWeekNumbers", CalendarShowWeekNumbersPropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo), '("year", CalendarYearPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
calendarDay :: AttrLabelProxy "day"
calendarDay = AttrLabelProxy

calendarDetailHeightRows :: AttrLabelProxy "detailHeightRows"
calendarDetailHeightRows = AttrLabelProxy

calendarDetailWidthChars :: AttrLabelProxy "detailWidthChars"
calendarDetailWidthChars = AttrLabelProxy

calendarMonth :: AttrLabelProxy "month"
calendarMonth = AttrLabelProxy

calendarNoMonthChange :: AttrLabelProxy "noMonthChange"
calendarNoMonthChange = AttrLabelProxy

calendarShowDayNames :: AttrLabelProxy "showDayNames"
calendarShowDayNames = AttrLabelProxy

calendarShowDetails :: AttrLabelProxy "showDetails"
calendarShowDetails = AttrLabelProxy

calendarShowHeading :: AttrLabelProxy "showHeading"
calendarShowHeading = AttrLabelProxy

calendarShowWeekNumbers :: AttrLabelProxy "showWeekNumbers"
calendarShowWeekNumbers = AttrLabelProxy

calendarYear :: AttrLabelProxy "year"
calendarYear = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList Calendar = CalendarSignalList
type CalendarSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("daySelected", CalendarDaySelectedSignalInfo), '("daySelectedDoubleClick", CalendarDaySelectedDoubleClickSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("monthChanged", CalendarMonthChangedSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("nextMonth", CalendarNextMonthSignalInfo), '("nextYear", CalendarNextYearSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("prevMonth", CalendarPrevMonthSignalInfo), '("prevYear", CalendarPrevYearSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method Calendar::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Calendar" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_calendar_new" gtk_calendar_new :: 
    IO (Ptr Calendar)

-- | Creates a new calendar, with the current date being selected.
calendarNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m Calendar
    -- ^ __Returns:__ a newly t'GI.Gtk.Objects.Calendar.Calendar' widget
calendarNew  = liftIO $ do
    result <- gtk_calendar_new
    checkUnexpectedReturnNULL "calendarNew" result
    result' <- (newObject Calendar) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Calendar::clear_marks
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "calendar"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Calendar" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCalendar" , sinceVersion = Nothing }
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

foreign import ccall "gtk_calendar_clear_marks" gtk_calendar_clear_marks :: 
    Ptr Calendar ->                         -- calendar : TInterface (Name {namespace = "Gtk", name = "Calendar"})
    IO ()

-- | Remove all visual markers.
calendarClearMarks ::
    (B.CallStack.HasCallStack, MonadIO m, IsCalendar a) =>
    a
    -- ^ /@calendar@/: a t'GI.Gtk.Objects.Calendar.Calendar'
    -> m ()
calendarClearMarks calendar = liftIO $ do
    calendar' <- unsafeManagedPtrCastPtr calendar
    gtk_calendar_clear_marks calendar'
    touchManagedPtr calendar
    return ()

#if defined(ENABLE_OVERLOADING)
data CalendarClearMarksMethodInfo
instance (signature ~ (m ()), MonadIO m, IsCalendar a) => O.OverloadedMethod CalendarClearMarksMethodInfo a signature where
    overloadedMethod = calendarClearMarks

instance O.OverloadedMethodInfo CalendarClearMarksMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Calendar.calendarClearMarks",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Calendar.html#v:calendarClearMarks"
        })


#endif

-- method Calendar::get_date
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "calendar"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Calendar" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCalendar" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "year"
--           , argType = TBasicType TUInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "location to store the year as a decimal\n    number (e.g. 2011), or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "month"
--           , argType = TBasicType TUInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "location to store the month number\n    (between 0 and 11), or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "day"
--           , argType = TBasicType TUInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "location to store the day number (between\n    1 and 31), or %NULL"
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

foreign import ccall "gtk_calendar_get_date" gtk_calendar_get_date :: 
    Ptr Calendar ->                         -- calendar : TInterface (Name {namespace = "Gtk", name = "Calendar"})
    Ptr Word32 ->                           -- year : TBasicType TUInt
    Ptr Word32 ->                           -- month : TBasicType TUInt
    Ptr Word32 ->                           -- day : TBasicType TUInt
    IO ()

-- | Obtains the selected date from a t'GI.Gtk.Objects.Calendar.Calendar'.
calendarGetDate ::
    (B.CallStack.HasCallStack, MonadIO m, IsCalendar a) =>
    a
    -- ^ /@calendar@/: a t'GI.Gtk.Objects.Calendar.Calendar'
    -> m ((Word32, Word32, Word32))
calendarGetDate calendar = liftIO $ do
    calendar' <- unsafeManagedPtrCastPtr calendar
    year <- allocMem :: IO (Ptr Word32)
    month <- allocMem :: IO (Ptr Word32)
    day <- allocMem :: IO (Ptr Word32)
    gtk_calendar_get_date calendar' year month day
    year' <- peek year
    month' <- peek month
    day' <- peek day
    touchManagedPtr calendar
    freeMem year
    freeMem month
    freeMem day
    return (year', month', day')

#if defined(ENABLE_OVERLOADING)
data CalendarGetDateMethodInfo
instance (signature ~ (m ((Word32, Word32, Word32))), MonadIO m, IsCalendar a) => O.OverloadedMethod CalendarGetDateMethodInfo a signature where
    overloadedMethod = calendarGetDate

instance O.OverloadedMethodInfo CalendarGetDateMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Calendar.calendarGetDate",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Calendar.html#v:calendarGetDate"
        })


#endif

-- method Calendar::get_day_is_marked
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "calendar"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Calendar" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCalendar" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "day"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the day number between 1 and 31."
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

foreign import ccall "gtk_calendar_get_day_is_marked" gtk_calendar_get_day_is_marked :: 
    Ptr Calendar ->                         -- calendar : TInterface (Name {namespace = "Gtk", name = "Calendar"})
    Word32 ->                               -- day : TBasicType TUInt
    IO CInt

-- | Returns if the /@day@/ of the /@calendar@/ is already marked.
-- 
-- /Since: 3.0/
calendarGetDayIsMarked ::
    (B.CallStack.HasCallStack, MonadIO m, IsCalendar a) =>
    a
    -- ^ /@calendar@/: a t'GI.Gtk.Objects.Calendar.Calendar'
    -> Word32
    -- ^ /@day@/: the day number between 1 and 31.
    -> m Bool
    -- ^ __Returns:__ whether the day is marked.
calendarGetDayIsMarked calendar day = liftIO $ do
    calendar' <- unsafeManagedPtrCastPtr calendar
    result <- gtk_calendar_get_day_is_marked calendar' day
    let result' = (/= 0) result
    touchManagedPtr calendar
    return result'

#if defined(ENABLE_OVERLOADING)
data CalendarGetDayIsMarkedMethodInfo
instance (signature ~ (Word32 -> m Bool), MonadIO m, IsCalendar a) => O.OverloadedMethod CalendarGetDayIsMarkedMethodInfo a signature where
    overloadedMethod = calendarGetDayIsMarked

instance O.OverloadedMethodInfo CalendarGetDayIsMarkedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Calendar.calendarGetDayIsMarked",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Calendar.html#v:calendarGetDayIsMarked"
        })


#endif

-- method Calendar::get_detail_height_rows
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "calendar"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Calendar" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCalendar." , sinceVersion = Nothing }
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

foreign import ccall "gtk_calendar_get_detail_height_rows" gtk_calendar_get_detail_height_rows :: 
    Ptr Calendar ->                         -- calendar : TInterface (Name {namespace = "Gtk", name = "Calendar"})
    IO Int32

-- | Queries the height of detail cells, in rows.
-- See [Calendar:detailWidthChars]("GI.Gtk.Objects.Calendar#g:attr:detailWidthChars").
-- 
-- /Since: 2.14/
calendarGetDetailHeightRows ::
    (B.CallStack.HasCallStack, MonadIO m, IsCalendar a) =>
    a
    -- ^ /@calendar@/: a t'GI.Gtk.Objects.Calendar.Calendar'.
    -> m Int32
    -- ^ __Returns:__ The height of detail cells, in rows.
calendarGetDetailHeightRows calendar = liftIO $ do
    calendar' <- unsafeManagedPtrCastPtr calendar
    result <- gtk_calendar_get_detail_height_rows calendar'
    touchManagedPtr calendar
    return result

#if defined(ENABLE_OVERLOADING)
data CalendarGetDetailHeightRowsMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsCalendar a) => O.OverloadedMethod CalendarGetDetailHeightRowsMethodInfo a signature where
    overloadedMethod = calendarGetDetailHeightRows

instance O.OverloadedMethodInfo CalendarGetDetailHeightRowsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Calendar.calendarGetDetailHeightRows",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Calendar.html#v:calendarGetDetailHeightRows"
        })


#endif

-- method Calendar::get_detail_width_chars
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "calendar"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Calendar" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCalendar." , sinceVersion = Nothing }
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

foreign import ccall "gtk_calendar_get_detail_width_chars" gtk_calendar_get_detail_width_chars :: 
    Ptr Calendar ->                         -- calendar : TInterface (Name {namespace = "Gtk", name = "Calendar"})
    IO Int32

-- | Queries the width of detail cells, in characters.
-- See [Calendar:detailWidthChars]("GI.Gtk.Objects.Calendar#g:attr:detailWidthChars").
-- 
-- /Since: 2.14/
calendarGetDetailWidthChars ::
    (B.CallStack.HasCallStack, MonadIO m, IsCalendar a) =>
    a
    -- ^ /@calendar@/: a t'GI.Gtk.Objects.Calendar.Calendar'.
    -> m Int32
    -- ^ __Returns:__ The width of detail cells, in characters.
calendarGetDetailWidthChars calendar = liftIO $ do
    calendar' <- unsafeManagedPtrCastPtr calendar
    result <- gtk_calendar_get_detail_width_chars calendar'
    touchManagedPtr calendar
    return result

#if defined(ENABLE_OVERLOADING)
data CalendarGetDetailWidthCharsMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsCalendar a) => O.OverloadedMethod CalendarGetDetailWidthCharsMethodInfo a signature where
    overloadedMethod = calendarGetDetailWidthChars

instance O.OverloadedMethodInfo CalendarGetDetailWidthCharsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Calendar.calendarGetDetailWidthChars",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Calendar.html#v:calendarGetDetailWidthChars"
        })


#endif

-- method Calendar::get_display_options
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "calendar"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Calendar" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCalendar" , sinceVersion = Nothing }
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
--                  Name { namespace = "Gtk" , name = "CalendarDisplayOptions" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_calendar_get_display_options" gtk_calendar_get_display_options :: 
    Ptr Calendar ->                         -- calendar : TInterface (Name {namespace = "Gtk", name = "Calendar"})
    IO CUInt

-- | Returns the current display options of /@calendar@/.
-- 
-- /Since: 2.4/
calendarGetDisplayOptions ::
    (B.CallStack.HasCallStack, MonadIO m, IsCalendar a) =>
    a
    -- ^ /@calendar@/: a t'GI.Gtk.Objects.Calendar.Calendar'
    -> m [Gtk.Flags.CalendarDisplayOptions]
    -- ^ __Returns:__ the display options.
calendarGetDisplayOptions calendar = liftIO $ do
    calendar' <- unsafeManagedPtrCastPtr calendar
    result <- gtk_calendar_get_display_options calendar'
    let result' = wordToGFlags result
    touchManagedPtr calendar
    return result'

#if defined(ENABLE_OVERLOADING)
data CalendarGetDisplayOptionsMethodInfo
instance (signature ~ (m [Gtk.Flags.CalendarDisplayOptions]), MonadIO m, IsCalendar a) => O.OverloadedMethod CalendarGetDisplayOptionsMethodInfo a signature where
    overloadedMethod = calendarGetDisplayOptions

instance O.OverloadedMethodInfo CalendarGetDisplayOptionsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Calendar.calendarGetDisplayOptions",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Calendar.html#v:calendarGetDisplayOptions"
        })


#endif

-- method Calendar::mark_day
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "calendar"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Calendar" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCalendar" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "day"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the day number to mark between 1 and 31."
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

foreign import ccall "gtk_calendar_mark_day" gtk_calendar_mark_day :: 
    Ptr Calendar ->                         -- calendar : TInterface (Name {namespace = "Gtk", name = "Calendar"})
    Word32 ->                               -- day : TBasicType TUInt
    IO ()

-- | Places a visual marker on a particular day.
calendarMarkDay ::
    (B.CallStack.HasCallStack, MonadIO m, IsCalendar a) =>
    a
    -- ^ /@calendar@/: a t'GI.Gtk.Objects.Calendar.Calendar'
    -> Word32
    -- ^ /@day@/: the day number to mark between 1 and 31.
    -> m ()
calendarMarkDay calendar day = liftIO $ do
    calendar' <- unsafeManagedPtrCastPtr calendar
    gtk_calendar_mark_day calendar' day
    touchManagedPtr calendar
    return ()

#if defined(ENABLE_OVERLOADING)
data CalendarMarkDayMethodInfo
instance (signature ~ (Word32 -> m ()), MonadIO m, IsCalendar a) => O.OverloadedMethod CalendarMarkDayMethodInfo a signature where
    overloadedMethod = calendarMarkDay

instance O.OverloadedMethodInfo CalendarMarkDayMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Calendar.calendarMarkDay",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Calendar.html#v:calendarMarkDay"
        })


#endif

-- method Calendar::select_day
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "calendar"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Calendar" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCalendar." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "day"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the day number between 1 and 31, or 0 to unselect\n  the currently selected day."
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

foreign import ccall "gtk_calendar_select_day" gtk_calendar_select_day :: 
    Ptr Calendar ->                         -- calendar : TInterface (Name {namespace = "Gtk", name = "Calendar"})
    Word32 ->                               -- day : TBasicType TUInt
    IO ()

-- | Selects a day from the current month.
calendarSelectDay ::
    (B.CallStack.HasCallStack, MonadIO m, IsCalendar a) =>
    a
    -- ^ /@calendar@/: a t'GI.Gtk.Objects.Calendar.Calendar'.
    -> Word32
    -- ^ /@day@/: the day number between 1 and 31, or 0 to unselect
    --   the currently selected day.
    -> m ()
calendarSelectDay calendar day = liftIO $ do
    calendar' <- unsafeManagedPtrCastPtr calendar
    gtk_calendar_select_day calendar' day
    touchManagedPtr calendar
    return ()

#if defined(ENABLE_OVERLOADING)
data CalendarSelectDayMethodInfo
instance (signature ~ (Word32 -> m ()), MonadIO m, IsCalendar a) => O.OverloadedMethod CalendarSelectDayMethodInfo a signature where
    overloadedMethod = calendarSelectDay

instance O.OverloadedMethodInfo CalendarSelectDayMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Calendar.calendarSelectDay",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Calendar.html#v:calendarSelectDay"
        })


#endif

-- method Calendar::select_month
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "calendar"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Calendar" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCalendar" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "month"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a month number between 0 and 11."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "year"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the year the month is in."
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

foreign import ccall "gtk_calendar_select_month" gtk_calendar_select_month :: 
    Ptr Calendar ->                         -- calendar : TInterface (Name {namespace = "Gtk", name = "Calendar"})
    Word32 ->                               -- month : TBasicType TUInt
    Word32 ->                               -- year : TBasicType TUInt
    IO ()

-- | Shifts the calendar to a different month.
calendarSelectMonth ::
    (B.CallStack.HasCallStack, MonadIO m, IsCalendar a) =>
    a
    -- ^ /@calendar@/: a t'GI.Gtk.Objects.Calendar.Calendar'
    -> Word32
    -- ^ /@month@/: a month number between 0 and 11.
    -> Word32
    -- ^ /@year@/: the year the month is in.
    -> m ()
calendarSelectMonth calendar month year = liftIO $ do
    calendar' <- unsafeManagedPtrCastPtr calendar
    gtk_calendar_select_month calendar' month year
    touchManagedPtr calendar
    return ()

#if defined(ENABLE_OVERLOADING)
data CalendarSelectMonthMethodInfo
instance (signature ~ (Word32 -> Word32 -> m ()), MonadIO m, IsCalendar a) => O.OverloadedMethod CalendarSelectMonthMethodInfo a signature where
    overloadedMethod = calendarSelectMonth

instance O.OverloadedMethodInfo CalendarSelectMonthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Calendar.calendarSelectMonth",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Calendar.html#v:calendarSelectMonth"
        })


#endif

-- method Calendar::set_detail_func
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "calendar"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Calendar" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCalendar." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "func"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CalendarDetailFunc" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a function providing details for each day."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeNotified
--           , argClosure = 2
--           , argDestroy = 3
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "data to pass to @func invokations."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "destroy"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "DestroyNotify" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a function for releasing @data."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeAsync
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

foreign import ccall "gtk_calendar_set_detail_func" gtk_calendar_set_detail_func :: 
    Ptr Calendar ->                         -- calendar : TInterface (Name {namespace = "Gtk", name = "Calendar"})
    FunPtr Gtk.Callbacks.C_CalendarDetailFunc -> -- func : TInterface (Name {namespace = "Gtk", name = "CalendarDetailFunc"})
    Ptr () ->                               -- data : TBasicType TPtr
    FunPtr GLib.Callbacks.C_DestroyNotify -> -- destroy : TInterface (Name {namespace = "GLib", name = "DestroyNotify"})
    IO ()

-- | Installs a function which provides Pango markup with detail information
-- for each day. Examples for such details are holidays or appointments. That
-- information is shown below each day when [Calendar:showDetails]("GI.Gtk.Objects.Calendar#g:attr:showDetails") is set.
-- A tooltip containing with full detail information is provided, if the entire
-- text should not fit into the details area, or if [Calendar:showDetails]("GI.Gtk.Objects.Calendar#g:attr:showDetails")
-- is not set.
-- 
-- The size of the details area can be restricted by setting the
-- [Calendar:detailWidthChars]("GI.Gtk.Objects.Calendar#g:attr:detailWidthChars") and [Calendar:detailHeightRows]("GI.Gtk.Objects.Calendar#g:attr:detailHeightRows")
-- properties.
-- 
-- /Since: 2.14/
calendarSetDetailFunc ::
    (B.CallStack.HasCallStack, MonadIO m, IsCalendar a) =>
    a
    -- ^ /@calendar@/: a t'GI.Gtk.Objects.Calendar.Calendar'.
    -> Gtk.Callbacks.CalendarDetailFunc
    -- ^ /@func@/: a function providing details for each day.
    -> m ()
calendarSetDetailFunc calendar func = liftIO $ do
    calendar' <- unsafeManagedPtrCastPtr calendar
    func' <- Gtk.Callbacks.mk_CalendarDetailFunc (Gtk.Callbacks.wrap_CalendarDetailFunc Nothing (Gtk.Callbacks.drop_closures_CalendarDetailFunc func))
    let data_ = castFunPtrToPtr func'
    let destroy = SP.safeFreeFunPtrPtr
    gtk_calendar_set_detail_func calendar' func' data_ destroy
    touchManagedPtr calendar
    return ()

#if defined(ENABLE_OVERLOADING)
data CalendarSetDetailFuncMethodInfo
instance (signature ~ (Gtk.Callbacks.CalendarDetailFunc -> m ()), MonadIO m, IsCalendar a) => O.OverloadedMethod CalendarSetDetailFuncMethodInfo a signature where
    overloadedMethod = calendarSetDetailFunc

instance O.OverloadedMethodInfo CalendarSetDetailFuncMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Calendar.calendarSetDetailFunc",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Calendar.html#v:calendarSetDetailFunc"
        })


#endif

-- method Calendar::set_detail_height_rows
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "calendar"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Calendar" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCalendar." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "rows"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "detail height in rows."
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

foreign import ccall "gtk_calendar_set_detail_height_rows" gtk_calendar_set_detail_height_rows :: 
    Ptr Calendar ->                         -- calendar : TInterface (Name {namespace = "Gtk", name = "Calendar"})
    Int32 ->                                -- rows : TBasicType TInt
    IO ()

-- | Updates the height of detail cells.
-- See [Calendar:detailHeightRows]("GI.Gtk.Objects.Calendar#g:attr:detailHeightRows").
-- 
-- /Since: 2.14/
calendarSetDetailHeightRows ::
    (B.CallStack.HasCallStack, MonadIO m, IsCalendar a) =>
    a
    -- ^ /@calendar@/: a t'GI.Gtk.Objects.Calendar.Calendar'.
    -> Int32
    -- ^ /@rows@/: detail height in rows.
    -> m ()
calendarSetDetailHeightRows calendar rows = liftIO $ do
    calendar' <- unsafeManagedPtrCastPtr calendar
    gtk_calendar_set_detail_height_rows calendar' rows
    touchManagedPtr calendar
    return ()

#if defined(ENABLE_OVERLOADING)
data CalendarSetDetailHeightRowsMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsCalendar a) => O.OverloadedMethod CalendarSetDetailHeightRowsMethodInfo a signature where
    overloadedMethod = calendarSetDetailHeightRows

instance O.OverloadedMethodInfo CalendarSetDetailHeightRowsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Calendar.calendarSetDetailHeightRows",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Calendar.html#v:calendarSetDetailHeightRows"
        })


#endif

-- method Calendar::set_detail_width_chars
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "calendar"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Calendar" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCalendar." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "chars"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "detail width in characters."
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

foreign import ccall "gtk_calendar_set_detail_width_chars" gtk_calendar_set_detail_width_chars :: 
    Ptr Calendar ->                         -- calendar : TInterface (Name {namespace = "Gtk", name = "Calendar"})
    Int32 ->                                -- chars : TBasicType TInt
    IO ()

-- | Updates the width of detail cells.
-- See [Calendar:detailWidthChars]("GI.Gtk.Objects.Calendar#g:attr:detailWidthChars").
-- 
-- /Since: 2.14/
calendarSetDetailWidthChars ::
    (B.CallStack.HasCallStack, MonadIO m, IsCalendar a) =>
    a
    -- ^ /@calendar@/: a t'GI.Gtk.Objects.Calendar.Calendar'.
    -> Int32
    -- ^ /@chars@/: detail width in characters.
    -> m ()
calendarSetDetailWidthChars calendar chars = liftIO $ do
    calendar' <- unsafeManagedPtrCastPtr calendar
    gtk_calendar_set_detail_width_chars calendar' chars
    touchManagedPtr calendar
    return ()

#if defined(ENABLE_OVERLOADING)
data CalendarSetDetailWidthCharsMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsCalendar a) => O.OverloadedMethod CalendarSetDetailWidthCharsMethodInfo a signature where
    overloadedMethod = calendarSetDetailWidthChars

instance O.OverloadedMethodInfo CalendarSetDetailWidthCharsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Calendar.calendarSetDetailWidthChars",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Calendar.html#v:calendarSetDetailWidthChars"
        })


#endif

-- method Calendar::set_display_options
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "calendar"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Calendar" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCalendar" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "flags"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "CalendarDisplayOptions" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the display options to set"
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

foreign import ccall "gtk_calendar_set_display_options" gtk_calendar_set_display_options :: 
    Ptr Calendar ->                         -- calendar : TInterface (Name {namespace = "Gtk", name = "Calendar"})
    CUInt ->                                -- flags : TInterface (Name {namespace = "Gtk", name = "CalendarDisplayOptions"})
    IO ()

-- | Sets display options (whether to display the heading and the month
-- headings).
-- 
-- /Since: 2.4/
calendarSetDisplayOptions ::
    (B.CallStack.HasCallStack, MonadIO m, IsCalendar a) =>
    a
    -- ^ /@calendar@/: a t'GI.Gtk.Objects.Calendar.Calendar'
    -> [Gtk.Flags.CalendarDisplayOptions]
    -- ^ /@flags@/: the display options to set
    -> m ()
calendarSetDisplayOptions calendar flags = liftIO $ do
    calendar' <- unsafeManagedPtrCastPtr calendar
    let flags' = gflagsToWord flags
    gtk_calendar_set_display_options calendar' flags'
    touchManagedPtr calendar
    return ()

#if defined(ENABLE_OVERLOADING)
data CalendarSetDisplayOptionsMethodInfo
instance (signature ~ ([Gtk.Flags.CalendarDisplayOptions] -> m ()), MonadIO m, IsCalendar a) => O.OverloadedMethod CalendarSetDisplayOptionsMethodInfo a signature where
    overloadedMethod = calendarSetDisplayOptions

instance O.OverloadedMethodInfo CalendarSetDisplayOptionsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Calendar.calendarSetDisplayOptions",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Calendar.html#v:calendarSetDisplayOptions"
        })


#endif

-- method Calendar::unmark_day
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "calendar"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Calendar" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCalendar." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "day"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the day number to unmark between 1 and 31."
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

foreign import ccall "gtk_calendar_unmark_day" gtk_calendar_unmark_day :: 
    Ptr Calendar ->                         -- calendar : TInterface (Name {namespace = "Gtk", name = "Calendar"})
    Word32 ->                               -- day : TBasicType TUInt
    IO ()

-- | Removes the visual marker from a particular day.
calendarUnmarkDay ::
    (B.CallStack.HasCallStack, MonadIO m, IsCalendar a) =>
    a
    -- ^ /@calendar@/: a t'GI.Gtk.Objects.Calendar.Calendar'.
    -> Word32
    -- ^ /@day@/: the day number to unmark between 1 and 31.
    -> m ()
calendarUnmarkDay calendar day = liftIO $ do
    calendar' <- unsafeManagedPtrCastPtr calendar
    gtk_calendar_unmark_day calendar' day
    touchManagedPtr calendar
    return ()

#if defined(ENABLE_OVERLOADING)
data CalendarUnmarkDayMethodInfo
instance (signature ~ (Word32 -> m ()), MonadIO m, IsCalendar a) => O.OverloadedMethod CalendarUnmarkDayMethodInfo a signature where
    overloadedMethod = calendarUnmarkDay

instance O.OverloadedMethodInfo CalendarUnmarkDayMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Calendar.calendarUnmarkDay",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Calendar.html#v:calendarUnmarkDay"
        })


#endif


