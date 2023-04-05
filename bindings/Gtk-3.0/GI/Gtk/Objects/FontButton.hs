{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- The t'GI.Gtk.Objects.FontButton.FontButton' is a button which displays the currently selected
-- font an allows to open a font chooser dialog to change the font.
-- It is suitable widget for selecting a font in a preference dialog.
-- 
-- = CSS nodes
-- 
-- GtkFontButton has a single CSS node with name button and style class .font.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.FontButton
    ( 

-- * Exported types
    FontButton(..)                          ,
    IsFontButton                            ,
    toFontButton                            ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [clicked]("GI.Gtk.Objects.Button#g:method:clicked"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [doSetRelatedAction]("GI.Gtk.Interfaces.Activatable#g:method:doSetRelatedAction"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [enter]("GI.Gtk.Objects.Button#g:method:enter"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [leave]("GI.Gtk.Objects.Button#g:method:leave"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [pressed]("GI.Gtk.Objects.Button#g:method:pressed"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [released]("GI.Gtk.Objects.Button#g:method:released"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [syncActionProperties]("GI.Gtk.Interfaces.Activatable#g:method:syncActionProperties"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getActionName]("GI.Gtk.Interfaces.Actionable#g:method:getActionName"), [getActionTargetValue]("GI.Gtk.Interfaces.Actionable#g:method:getActionTargetValue"), [getAlignment]("GI.Gtk.Objects.Button#g:method:getAlignment"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAlwaysShowImage]("GI.Gtk.Objects.Button#g:method:getAlwaysShowImage"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChild]("GI.Gtk.Objects.Bin#g:method:getChild"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEventWindow]("GI.Gtk.Objects.Button#g:method:getEventWindow"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Button#g:method:getFocusOnClick"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFont]("GI.Gtk.Interfaces.FontChooser#g:method:getFont"), [getFontDesc]("GI.Gtk.Interfaces.FontChooser#g:method:getFontDesc"), [getFontFace]("GI.Gtk.Interfaces.FontChooser#g:method:getFontFace"), [getFontFamily]("GI.Gtk.Interfaces.FontChooser#g:method:getFontFamily"), [getFontFeatures]("GI.Gtk.Interfaces.FontChooser#g:method:getFontFeatures"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontName]("GI.Gtk.Objects.FontButton#g:method:getFontName"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFontSize]("GI.Gtk.Interfaces.FontChooser#g:method:getFontSize"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getImage]("GI.Gtk.Objects.Button#g:method:getImage"), [getImagePosition]("GI.Gtk.Objects.Button#g:method:getImagePosition"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getLabel]("GI.Gtk.Objects.Button#g:method:getLabel"), [getLanguage]("GI.Gtk.Interfaces.FontChooser#g:method:getLanguage"), [getLevel]("GI.Gtk.Interfaces.FontChooser#g:method:getLevel"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getPreviewText]("GI.Gtk.Interfaces.FontChooser#g:method:getPreviewText"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRelatedAction]("GI.Gtk.Interfaces.Activatable#g:method:getRelatedAction"), [getRelief]("GI.Gtk.Objects.Button#g:method:getRelief"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getShowPreviewEntry]("GI.Gtk.Interfaces.FontChooser#g:method:getShowPreviewEntry"), [getShowSize]("GI.Gtk.Objects.FontButton#g:method:getShowSize"), [getShowStyle]("GI.Gtk.Objects.FontButton#g:method:getShowStyle"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTitle]("GI.Gtk.Objects.FontButton#g:method:getTitle"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getUseActionAppearance]("GI.Gtk.Interfaces.Activatable#g:method:getUseActionAppearance"), [getUseFont]("GI.Gtk.Objects.FontButton#g:method:getUseFont"), [getUseSize]("GI.Gtk.Objects.FontButton#g:method:getUseSize"), [getUseStock]("GI.Gtk.Objects.Button#g:method:getUseStock"), [getUseUnderline]("GI.Gtk.Objects.Button#g:method:getUseUnderline"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setActionName]("GI.Gtk.Interfaces.Actionable#g:method:setActionName"), [setActionTargetValue]("GI.Gtk.Interfaces.Actionable#g:method:setActionTargetValue"), [setAlignment]("GI.Gtk.Objects.Button#g:method:setAlignment"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAlwaysShowImage]("GI.Gtk.Objects.Button#g:method:setAlwaysShowImage"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDetailedActionName]("GI.Gtk.Interfaces.Actionable#g:method:setDetailedActionName"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFilterFunc]("GI.Gtk.Interfaces.FontChooser#g:method:setFilterFunc"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Button#g:method:setFocusOnClick"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFont]("GI.Gtk.Interfaces.FontChooser#g:method:setFont"), [setFontDesc]("GI.Gtk.Interfaces.FontChooser#g:method:setFontDesc"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontName]("GI.Gtk.Objects.FontButton#g:method:setFontName"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setImage]("GI.Gtk.Objects.Button#g:method:setImage"), [setImagePosition]("GI.Gtk.Objects.Button#g:method:setImagePosition"), [setLabel]("GI.Gtk.Objects.Button#g:method:setLabel"), [setLanguage]("GI.Gtk.Interfaces.FontChooser#g:method:setLanguage"), [setLevel]("GI.Gtk.Interfaces.FontChooser#g:method:setLevel"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setPreviewText]("GI.Gtk.Interfaces.FontChooser#g:method:setPreviewText"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setRelatedAction]("GI.Gtk.Interfaces.Activatable#g:method:setRelatedAction"), [setRelief]("GI.Gtk.Objects.Button#g:method:setRelief"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setShowPreviewEntry]("GI.Gtk.Interfaces.FontChooser#g:method:setShowPreviewEntry"), [setShowSize]("GI.Gtk.Objects.FontButton#g:method:setShowSize"), [setShowStyle]("GI.Gtk.Objects.FontButton#g:method:setShowStyle"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTitle]("GI.Gtk.Objects.FontButton#g:method:setTitle"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setUseActionAppearance]("GI.Gtk.Interfaces.Activatable#g:method:setUseActionAppearance"), [setUseFont]("GI.Gtk.Objects.FontButton#g:method:setUseFont"), [setUseSize]("GI.Gtk.Objects.FontButton#g:method:setUseSize"), [setUseStock]("GI.Gtk.Objects.Button#g:method:setUseStock"), [setUseUnderline]("GI.Gtk.Objects.Button#g:method:setUseUnderline"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveFontButtonMethod                 ,
#endif

-- ** getFontName #method:getFontName#

#if defined(ENABLE_OVERLOADING)
    FontButtonGetFontNameMethodInfo         ,
#endif
    fontButtonGetFontName                   ,


-- ** getShowSize #method:getShowSize#

#if defined(ENABLE_OVERLOADING)
    FontButtonGetShowSizeMethodInfo         ,
#endif
    fontButtonGetShowSize                   ,


-- ** getShowStyle #method:getShowStyle#

#if defined(ENABLE_OVERLOADING)
    FontButtonGetShowStyleMethodInfo        ,
#endif
    fontButtonGetShowStyle                  ,


-- ** getTitle #method:getTitle#

#if defined(ENABLE_OVERLOADING)
    FontButtonGetTitleMethodInfo            ,
#endif
    fontButtonGetTitle                      ,


-- ** getUseFont #method:getUseFont#

#if defined(ENABLE_OVERLOADING)
    FontButtonGetUseFontMethodInfo          ,
#endif
    fontButtonGetUseFont                    ,


-- ** getUseSize #method:getUseSize#

#if defined(ENABLE_OVERLOADING)
    FontButtonGetUseSizeMethodInfo          ,
#endif
    fontButtonGetUseSize                    ,


-- ** new #method:new#

    fontButtonNew                           ,


-- ** newWithFont #method:newWithFont#

    fontButtonNewWithFont                   ,


-- ** setFontName #method:setFontName#

#if defined(ENABLE_OVERLOADING)
    FontButtonSetFontNameMethodInfo         ,
#endif
    fontButtonSetFontName                   ,


-- ** setShowSize #method:setShowSize#

#if defined(ENABLE_OVERLOADING)
    FontButtonSetShowSizeMethodInfo         ,
#endif
    fontButtonSetShowSize                   ,


-- ** setShowStyle #method:setShowStyle#

#if defined(ENABLE_OVERLOADING)
    FontButtonSetShowStyleMethodInfo        ,
#endif
    fontButtonSetShowStyle                  ,


-- ** setTitle #method:setTitle#

#if defined(ENABLE_OVERLOADING)
    FontButtonSetTitleMethodInfo            ,
#endif
    fontButtonSetTitle                      ,


-- ** setUseFont #method:setUseFont#

#if defined(ENABLE_OVERLOADING)
    FontButtonSetUseFontMethodInfo          ,
#endif
    fontButtonSetUseFont                    ,


-- ** setUseSize #method:setUseSize#

#if defined(ENABLE_OVERLOADING)
    FontButtonSetUseSizeMethodInfo          ,
#endif
    fontButtonSetUseSize                    ,




 -- * Properties


-- ** fontName #attr:fontName#
-- | The name of the currently selected font.
-- 
-- /Since: 2.4/

#if defined(ENABLE_OVERLOADING)
    FontButtonFontNamePropertyInfo          ,
#endif
    clearFontButtonFontName                 ,
    constructFontButtonFontName             ,
#if defined(ENABLE_OVERLOADING)
    fontButtonFontName                      ,
#endif
    getFontButtonFontName                   ,
    setFontButtonFontName                   ,


-- ** showSize #attr:showSize#
-- | If this property is set to 'P.True', the selected font size will be shown
-- in the label. For a more WYSIWYG way to show the selected size, see the
-- [useSize](#g:signal:useSize) property.
-- 
-- /Since: 2.4/

#if defined(ENABLE_OVERLOADING)
    FontButtonShowSizePropertyInfo          ,
#endif
    constructFontButtonShowSize             ,
#if defined(ENABLE_OVERLOADING)
    fontButtonShowSize                      ,
#endif
    getFontButtonShowSize                   ,
    setFontButtonShowSize                   ,


-- ** showStyle #attr:showStyle#
-- | If this property is set to 'P.True', the name of the selected font style
-- will be shown in the label. For a more WYSIWYG way to show the selected
-- style, see the [useFont](#g:signal:useFont) property.
-- 
-- /Since: 2.4/

#if defined(ENABLE_OVERLOADING)
    FontButtonShowStylePropertyInfo         ,
#endif
    constructFontButtonShowStyle            ,
#if defined(ENABLE_OVERLOADING)
    fontButtonShowStyle                     ,
#endif
    getFontButtonShowStyle                  ,
    setFontButtonShowStyle                  ,


-- ** title #attr:title#
-- | The title of the font chooser dialog.
-- 
-- /Since: 2.4/

#if defined(ENABLE_OVERLOADING)
    FontButtonTitlePropertyInfo             ,
#endif
    constructFontButtonTitle                ,
#if defined(ENABLE_OVERLOADING)
    fontButtonTitle                         ,
#endif
    getFontButtonTitle                      ,
    setFontButtonTitle                      ,


-- ** useFont #attr:useFont#
-- | If this property is set to 'P.True', the label will be drawn
-- in the selected font.
-- 
-- /Since: 2.4/

#if defined(ENABLE_OVERLOADING)
    FontButtonUseFontPropertyInfo           ,
#endif
    constructFontButtonUseFont              ,
#if defined(ENABLE_OVERLOADING)
    fontButtonUseFont                       ,
#endif
    getFontButtonUseFont                    ,
    setFontButtonUseFont                    ,


-- ** useSize #attr:useSize#
-- | If this property is set to 'P.True', the label will be drawn
-- with the selected font size.
-- 
-- /Since: 2.4/

#if defined(ENABLE_OVERLOADING)
    FontButtonUseSizePropertyInfo           ,
#endif
    constructFontButtonUseSize              ,
#if defined(ENABLE_OVERLOADING)
    fontButtonUseSize                       ,
#endif
    getFontButtonUseSize                    ,
    setFontButtonUseSize                    ,




 -- * Signals


-- ** fontSet #signal:fontSet#

    FontButtonFontSetCallback               ,
#if defined(ENABLE_OVERLOADING)
    FontButtonFontSetSignalInfo             ,
#endif
    afterFontButtonFontSet                  ,
    onFontButtonFontSet                     ,




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
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Actionable as Gtk.Actionable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Activatable as Gtk.Activatable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.FontChooser as Gtk.FontChooser
import {-# SOURCE #-} qualified GI.Gtk.Objects.Bin as Gtk.Bin
import {-# SOURCE #-} qualified GI.Gtk.Objects.Button as Gtk.Button
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype FontButton = FontButton (SP.ManagedPtr FontButton)
    deriving (Eq)

instance SP.ManagedPtrNewtype FontButton where
    toManagedPtr (FontButton p) = p

foreign import ccall "gtk_font_button_get_type"
    c_gtk_font_button_get_type :: IO B.Types.GType

instance B.Types.TypedObject FontButton where
    glibType = c_gtk_font_button_get_type

instance B.Types.GObject FontButton

-- | Type class for types which can be safely cast to `FontButton`, for instance with `toFontButton`.
class (SP.GObject o, O.IsDescendantOf FontButton o) => IsFontButton o
instance (SP.GObject o, O.IsDescendantOf FontButton o) => IsFontButton o

instance O.HasParentTypes FontButton
type instance O.ParentTypes FontButton = '[Gtk.Button.Button, Gtk.Bin.Bin, Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Actionable.Actionable, Gtk.Activatable.Activatable, Gtk.Buildable.Buildable, Gtk.FontChooser.FontChooser]

-- | Cast to `FontButton`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toFontButton :: (MIO.MonadIO m, IsFontButton o) => o -> m FontButton
toFontButton = MIO.liftIO . B.ManagedPtr.unsafeCastTo FontButton

-- | Convert 'FontButton' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe FontButton) where
    gvalueGType_ = c_gtk_font_button_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr FontButton)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr FontButton)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject FontButton ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveFontButtonMethod (t :: Symbol) (o :: *) :: * where
    ResolveFontButtonMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveFontButtonMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveFontButtonMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveFontButtonMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveFontButtonMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveFontButtonMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveFontButtonMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveFontButtonMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveFontButtonMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveFontButtonMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveFontButtonMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveFontButtonMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveFontButtonMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveFontButtonMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveFontButtonMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveFontButtonMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveFontButtonMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveFontButtonMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveFontButtonMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveFontButtonMethod "clicked" o = Gtk.Button.ButtonClickedMethodInfo
    ResolveFontButtonMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveFontButtonMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveFontButtonMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveFontButtonMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveFontButtonMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveFontButtonMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveFontButtonMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveFontButtonMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveFontButtonMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveFontButtonMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveFontButtonMethod "doSetRelatedAction" o = Gtk.Activatable.ActivatableDoSetRelatedActionMethodInfo
    ResolveFontButtonMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveFontButtonMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveFontButtonMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveFontButtonMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveFontButtonMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveFontButtonMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveFontButtonMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveFontButtonMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveFontButtonMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveFontButtonMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveFontButtonMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveFontButtonMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveFontButtonMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveFontButtonMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveFontButtonMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveFontButtonMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveFontButtonMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveFontButtonMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveFontButtonMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveFontButtonMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveFontButtonMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveFontButtonMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveFontButtonMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveFontButtonMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveFontButtonMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveFontButtonMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveFontButtonMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveFontButtonMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveFontButtonMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveFontButtonMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveFontButtonMethod "enter" o = Gtk.Button.ButtonEnterMethodInfo
    ResolveFontButtonMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveFontButtonMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveFontButtonMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveFontButtonMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveFontButtonMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveFontButtonMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveFontButtonMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveFontButtonMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveFontButtonMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveFontButtonMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveFontButtonMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveFontButtonMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveFontButtonMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveFontButtonMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveFontButtonMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveFontButtonMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveFontButtonMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveFontButtonMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveFontButtonMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveFontButtonMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveFontButtonMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveFontButtonMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveFontButtonMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveFontButtonMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveFontButtonMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveFontButtonMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveFontButtonMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveFontButtonMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveFontButtonMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveFontButtonMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveFontButtonMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveFontButtonMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveFontButtonMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveFontButtonMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveFontButtonMethod "leave" o = Gtk.Button.ButtonLeaveMethodInfo
    ResolveFontButtonMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveFontButtonMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveFontButtonMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveFontButtonMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveFontButtonMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveFontButtonMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveFontButtonMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveFontButtonMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveFontButtonMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveFontButtonMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveFontButtonMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveFontButtonMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveFontButtonMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveFontButtonMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveFontButtonMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveFontButtonMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveFontButtonMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveFontButtonMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveFontButtonMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveFontButtonMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveFontButtonMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveFontButtonMethod "pressed" o = Gtk.Button.ButtonPressedMethodInfo
    ResolveFontButtonMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveFontButtonMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveFontButtonMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveFontButtonMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveFontButtonMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveFontButtonMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveFontButtonMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveFontButtonMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveFontButtonMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveFontButtonMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveFontButtonMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveFontButtonMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveFontButtonMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveFontButtonMethod "released" o = Gtk.Button.ButtonReleasedMethodInfo
    ResolveFontButtonMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolveFontButtonMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveFontButtonMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveFontButtonMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveFontButtonMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveFontButtonMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveFontButtonMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveFontButtonMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveFontButtonMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveFontButtonMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveFontButtonMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveFontButtonMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveFontButtonMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveFontButtonMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveFontButtonMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveFontButtonMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveFontButtonMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveFontButtonMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveFontButtonMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveFontButtonMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveFontButtonMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveFontButtonMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveFontButtonMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveFontButtonMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveFontButtonMethod "syncActionProperties" o = Gtk.Activatable.ActivatableSyncActionPropertiesMethodInfo
    ResolveFontButtonMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveFontButtonMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveFontButtonMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveFontButtonMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveFontButtonMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveFontButtonMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveFontButtonMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveFontButtonMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveFontButtonMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveFontButtonMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveFontButtonMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveFontButtonMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveFontButtonMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveFontButtonMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveFontButtonMethod "getActionName" o = Gtk.Actionable.ActionableGetActionNameMethodInfo
    ResolveFontButtonMethod "getActionTargetValue" o = Gtk.Actionable.ActionableGetActionTargetValueMethodInfo
    ResolveFontButtonMethod "getAlignment" o = Gtk.Button.ButtonGetAlignmentMethodInfo
    ResolveFontButtonMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveFontButtonMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveFontButtonMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveFontButtonMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveFontButtonMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveFontButtonMethod "getAlwaysShowImage" o = Gtk.Button.ButtonGetAlwaysShowImageMethodInfo
    ResolveFontButtonMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveFontButtonMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveFontButtonMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveFontButtonMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveFontButtonMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveFontButtonMethod "getChild" o = Gtk.Bin.BinGetChildMethodInfo
    ResolveFontButtonMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveFontButtonMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveFontButtonMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveFontButtonMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveFontButtonMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveFontButtonMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveFontButtonMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveFontButtonMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveFontButtonMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveFontButtonMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveFontButtonMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveFontButtonMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveFontButtonMethod "getEventWindow" o = Gtk.Button.ButtonGetEventWindowMethodInfo
    ResolveFontButtonMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveFontButtonMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveFontButtonMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveFontButtonMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveFontButtonMethod "getFocusOnClick" o = Gtk.Button.ButtonGetFocusOnClickMethodInfo
    ResolveFontButtonMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveFontButtonMethod "getFont" o = Gtk.FontChooser.FontChooserGetFontMethodInfo
    ResolveFontButtonMethod "getFontDesc" o = Gtk.FontChooser.FontChooserGetFontDescMethodInfo
    ResolveFontButtonMethod "getFontFace" o = Gtk.FontChooser.FontChooserGetFontFaceMethodInfo
    ResolveFontButtonMethod "getFontFamily" o = Gtk.FontChooser.FontChooserGetFontFamilyMethodInfo
    ResolveFontButtonMethod "getFontFeatures" o = Gtk.FontChooser.FontChooserGetFontFeaturesMethodInfo
    ResolveFontButtonMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveFontButtonMethod "getFontName" o = FontButtonGetFontNameMethodInfo
    ResolveFontButtonMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveFontButtonMethod "getFontSize" o = Gtk.FontChooser.FontChooserGetFontSizeMethodInfo
    ResolveFontButtonMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveFontButtonMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveFontButtonMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveFontButtonMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveFontButtonMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveFontButtonMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveFontButtonMethod "getImage" o = Gtk.Button.ButtonGetImageMethodInfo
    ResolveFontButtonMethod "getImagePosition" o = Gtk.Button.ButtonGetImagePositionMethodInfo
    ResolveFontButtonMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveFontButtonMethod "getLabel" o = Gtk.Button.ButtonGetLabelMethodInfo
    ResolveFontButtonMethod "getLanguage" o = Gtk.FontChooser.FontChooserGetLanguageMethodInfo
    ResolveFontButtonMethod "getLevel" o = Gtk.FontChooser.FontChooserGetLevelMethodInfo
    ResolveFontButtonMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveFontButtonMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveFontButtonMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveFontButtonMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveFontButtonMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveFontButtonMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveFontButtonMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveFontButtonMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveFontButtonMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveFontButtonMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveFontButtonMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveFontButtonMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveFontButtonMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveFontButtonMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveFontButtonMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveFontButtonMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveFontButtonMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveFontButtonMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveFontButtonMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveFontButtonMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveFontButtonMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveFontButtonMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveFontButtonMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveFontButtonMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveFontButtonMethod "getPreviewText" o = Gtk.FontChooser.FontChooserGetPreviewTextMethodInfo
    ResolveFontButtonMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveFontButtonMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveFontButtonMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveFontButtonMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveFontButtonMethod "getRelatedAction" o = Gtk.Activatable.ActivatableGetRelatedActionMethodInfo
    ResolveFontButtonMethod "getRelief" o = Gtk.Button.ButtonGetReliefMethodInfo
    ResolveFontButtonMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveFontButtonMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveFontButtonMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveFontButtonMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveFontButtonMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveFontButtonMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveFontButtonMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveFontButtonMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveFontButtonMethod "getShowPreviewEntry" o = Gtk.FontChooser.FontChooserGetShowPreviewEntryMethodInfo
    ResolveFontButtonMethod "getShowSize" o = FontButtonGetShowSizeMethodInfo
    ResolveFontButtonMethod "getShowStyle" o = FontButtonGetShowStyleMethodInfo
    ResolveFontButtonMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveFontButtonMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveFontButtonMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveFontButtonMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveFontButtonMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveFontButtonMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveFontButtonMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveFontButtonMethod "getTitle" o = FontButtonGetTitleMethodInfo
    ResolveFontButtonMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveFontButtonMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveFontButtonMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveFontButtonMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveFontButtonMethod "getUseActionAppearance" o = Gtk.Activatable.ActivatableGetUseActionAppearanceMethodInfo
    ResolveFontButtonMethod "getUseFont" o = FontButtonGetUseFontMethodInfo
    ResolveFontButtonMethod "getUseSize" o = FontButtonGetUseSizeMethodInfo
    ResolveFontButtonMethod "getUseStock" o = Gtk.Button.ButtonGetUseStockMethodInfo
    ResolveFontButtonMethod "getUseUnderline" o = Gtk.Button.ButtonGetUseUnderlineMethodInfo
    ResolveFontButtonMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveFontButtonMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveFontButtonMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveFontButtonMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveFontButtonMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveFontButtonMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveFontButtonMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveFontButtonMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveFontButtonMethod "setActionName" o = Gtk.Actionable.ActionableSetActionNameMethodInfo
    ResolveFontButtonMethod "setActionTargetValue" o = Gtk.Actionable.ActionableSetActionTargetValueMethodInfo
    ResolveFontButtonMethod "setAlignment" o = Gtk.Button.ButtonSetAlignmentMethodInfo
    ResolveFontButtonMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveFontButtonMethod "setAlwaysShowImage" o = Gtk.Button.ButtonSetAlwaysShowImageMethodInfo
    ResolveFontButtonMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveFontButtonMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveFontButtonMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveFontButtonMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveFontButtonMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveFontButtonMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveFontButtonMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveFontButtonMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveFontButtonMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveFontButtonMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveFontButtonMethod "setDetailedActionName" o = Gtk.Actionable.ActionableSetDetailedActionNameMethodInfo
    ResolveFontButtonMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveFontButtonMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveFontButtonMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveFontButtonMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveFontButtonMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveFontButtonMethod "setFilterFunc" o = Gtk.FontChooser.FontChooserSetFilterFuncMethodInfo
    ResolveFontButtonMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveFontButtonMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveFontButtonMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveFontButtonMethod "setFocusOnClick" o = Gtk.Button.ButtonSetFocusOnClickMethodInfo
    ResolveFontButtonMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveFontButtonMethod "setFont" o = Gtk.FontChooser.FontChooserSetFontMethodInfo
    ResolveFontButtonMethod "setFontDesc" o = Gtk.FontChooser.FontChooserSetFontDescMethodInfo
    ResolveFontButtonMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveFontButtonMethod "setFontName" o = FontButtonSetFontNameMethodInfo
    ResolveFontButtonMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveFontButtonMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveFontButtonMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveFontButtonMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveFontButtonMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveFontButtonMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveFontButtonMethod "setImage" o = Gtk.Button.ButtonSetImageMethodInfo
    ResolveFontButtonMethod "setImagePosition" o = Gtk.Button.ButtonSetImagePositionMethodInfo
    ResolveFontButtonMethod "setLabel" o = Gtk.Button.ButtonSetLabelMethodInfo
    ResolveFontButtonMethod "setLanguage" o = Gtk.FontChooser.FontChooserSetLanguageMethodInfo
    ResolveFontButtonMethod "setLevel" o = Gtk.FontChooser.FontChooserSetLevelMethodInfo
    ResolveFontButtonMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveFontButtonMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveFontButtonMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveFontButtonMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveFontButtonMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveFontButtonMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveFontButtonMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveFontButtonMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveFontButtonMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveFontButtonMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveFontButtonMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveFontButtonMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveFontButtonMethod "setPreviewText" o = Gtk.FontChooser.FontChooserSetPreviewTextMethodInfo
    ResolveFontButtonMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveFontButtonMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveFontButtonMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveFontButtonMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveFontButtonMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveFontButtonMethod "setRelatedAction" o = Gtk.Activatable.ActivatableSetRelatedActionMethodInfo
    ResolveFontButtonMethod "setRelief" o = Gtk.Button.ButtonSetReliefMethodInfo
    ResolveFontButtonMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveFontButtonMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveFontButtonMethod "setShowPreviewEntry" o = Gtk.FontChooser.FontChooserSetShowPreviewEntryMethodInfo
    ResolveFontButtonMethod "setShowSize" o = FontButtonSetShowSizeMethodInfo
    ResolveFontButtonMethod "setShowStyle" o = FontButtonSetShowStyleMethodInfo
    ResolveFontButtonMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveFontButtonMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveFontButtonMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveFontButtonMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveFontButtonMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveFontButtonMethod "setTitle" o = FontButtonSetTitleMethodInfo
    ResolveFontButtonMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveFontButtonMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveFontButtonMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveFontButtonMethod "setUseActionAppearance" o = Gtk.Activatable.ActivatableSetUseActionAppearanceMethodInfo
    ResolveFontButtonMethod "setUseFont" o = FontButtonSetUseFontMethodInfo
    ResolveFontButtonMethod "setUseSize" o = FontButtonSetUseSizeMethodInfo
    ResolveFontButtonMethod "setUseStock" o = Gtk.Button.ButtonSetUseStockMethodInfo
    ResolveFontButtonMethod "setUseUnderline" o = Gtk.Button.ButtonSetUseUnderlineMethodInfo
    ResolveFontButtonMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveFontButtonMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveFontButtonMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveFontButtonMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveFontButtonMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveFontButtonMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveFontButtonMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveFontButtonMethod t FontButton, O.OverloadedMethod info FontButton p) => OL.IsLabel t (FontButton -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveFontButtonMethod t FontButton, O.OverloadedMethod info FontButton p, R.HasField t FontButton p) => R.HasField t FontButton p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveFontButtonMethod t FontButton, O.OverloadedMethodInfo info FontButton) => OL.IsLabel t (O.MethodProxy info FontButton) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal FontButton::font-set
-- | The [fontSet](#g:signal:fontSet) signal is emitted when the user selects a font.
-- When handling this signal, use 'GI.Gtk.Interfaces.FontChooser.fontChooserGetFont'
-- to find out which font was just selected.
-- 
-- Note that this signal is only emitted when the user
-- changes the font. If you need to react to programmatic font changes
-- as well, use the notify[font](#g:signal:font) signal.
-- 
-- /Since: 2.4/
type FontButtonFontSetCallback =
    IO ()

type C_FontButtonFontSetCallback =
    Ptr FontButton ->                       -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_FontButtonFontSetCallback`.
foreign import ccall "wrapper"
    mk_FontButtonFontSetCallback :: C_FontButtonFontSetCallback -> IO (FunPtr C_FontButtonFontSetCallback)

wrap_FontButtonFontSetCallback :: 
    GObject a => (a -> FontButtonFontSetCallback) ->
    C_FontButtonFontSetCallback
wrap_FontButtonFontSetCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [fontSet](#signal:fontSet) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' fontButton #fontSet callback
-- @
-- 
-- 
onFontButtonFontSet :: (IsFontButton a, MonadIO m) => a -> ((?self :: a) => FontButtonFontSetCallback) -> m SignalHandlerId
onFontButtonFontSet obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_FontButtonFontSetCallback wrapped
    wrapped'' <- mk_FontButtonFontSetCallback wrapped'
    connectSignalFunPtr obj "font-set" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [fontSet](#signal:fontSet) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' fontButton #fontSet callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterFontButtonFontSet :: (IsFontButton a, MonadIO m) => a -> ((?self :: a) => FontButtonFontSetCallback) -> m SignalHandlerId
afterFontButtonFontSet obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_FontButtonFontSetCallback wrapped
    wrapped'' <- mk_FontButtonFontSetCallback wrapped'
    connectSignalFunPtr obj "font-set" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data FontButtonFontSetSignalInfo
instance SignalInfo FontButtonFontSetSignalInfo where
    type HaskellCallbackType FontButtonFontSetSignalInfo = FontButtonFontSetCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_FontButtonFontSetCallback cb
        cb'' <- mk_FontButtonFontSetCallback cb'
        connectSignalFunPtr obj "font-set" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FontButton::font-set"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FontButton.html#g:signal:fontSet"})

#endif

-- VVV Prop "font-name"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Nothing)

-- | Get the value of the “@font-name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' fontButton #fontName
-- @
getFontButtonFontName :: (MonadIO m, IsFontButton o) => o -> m T.Text
getFontButtonFontName obj = MIO.liftIO $ checkUnexpectedNothing "getFontButtonFontName" $ B.Properties.getObjectPropertyString obj "font-name"

-- | Set the value of the “@font-name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' fontButton [ #fontName 'Data.GI.Base.Attributes.:=' value ]
-- @
setFontButtonFontName :: (MonadIO m, IsFontButton o) => o -> T.Text -> m ()
setFontButtonFontName obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "font-name" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@font-name@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructFontButtonFontName :: (IsFontButton o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructFontButtonFontName val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "font-name" (P.Just val)

-- | Set the value of the “@font-name@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #fontName
-- @
clearFontButtonFontName :: (MonadIO m, IsFontButton o) => o -> m ()
clearFontButtonFontName obj = liftIO $ B.Properties.setObjectPropertyString obj "font-name" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data FontButtonFontNamePropertyInfo
instance AttrInfo FontButtonFontNamePropertyInfo where
    type AttrAllowedOps FontButtonFontNamePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint FontButtonFontNamePropertyInfo = IsFontButton
    type AttrSetTypeConstraint FontButtonFontNamePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint FontButtonFontNamePropertyInfo = (~) T.Text
    type AttrTransferType FontButtonFontNamePropertyInfo = T.Text
    type AttrGetType FontButtonFontNamePropertyInfo = T.Text
    type AttrLabel FontButtonFontNamePropertyInfo = "font-name"
    type AttrOrigin FontButtonFontNamePropertyInfo = FontButton
    attrGet = getFontButtonFontName
    attrSet = setFontButtonFontName
    attrTransfer _ v = do
        return v
    attrConstruct = constructFontButtonFontName
    attrClear = clearFontButtonFontName
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FontButton.fontName"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FontButton.html#g:attr:fontName"
        })
#endif

-- VVV Prop "show-size"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@show-size@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' fontButton #showSize
-- @
getFontButtonShowSize :: (MonadIO m, IsFontButton o) => o -> m Bool
getFontButtonShowSize obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "show-size"

-- | Set the value of the “@show-size@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' fontButton [ #showSize 'Data.GI.Base.Attributes.:=' value ]
-- @
setFontButtonShowSize :: (MonadIO m, IsFontButton o) => o -> Bool -> m ()
setFontButtonShowSize obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "show-size" val

-- | Construct a `GValueConstruct` with valid value for the “@show-size@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructFontButtonShowSize :: (IsFontButton o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructFontButtonShowSize val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "show-size" val

#if defined(ENABLE_OVERLOADING)
data FontButtonShowSizePropertyInfo
instance AttrInfo FontButtonShowSizePropertyInfo where
    type AttrAllowedOps FontButtonShowSizePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint FontButtonShowSizePropertyInfo = IsFontButton
    type AttrSetTypeConstraint FontButtonShowSizePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint FontButtonShowSizePropertyInfo = (~) Bool
    type AttrTransferType FontButtonShowSizePropertyInfo = Bool
    type AttrGetType FontButtonShowSizePropertyInfo = Bool
    type AttrLabel FontButtonShowSizePropertyInfo = "show-size"
    type AttrOrigin FontButtonShowSizePropertyInfo = FontButton
    attrGet = getFontButtonShowSize
    attrSet = setFontButtonShowSize
    attrTransfer _ v = do
        return v
    attrConstruct = constructFontButtonShowSize
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FontButton.showSize"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FontButton.html#g:attr:showSize"
        })
#endif

-- VVV Prop "show-style"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@show-style@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' fontButton #showStyle
-- @
getFontButtonShowStyle :: (MonadIO m, IsFontButton o) => o -> m Bool
getFontButtonShowStyle obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "show-style"

-- | Set the value of the “@show-style@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' fontButton [ #showStyle 'Data.GI.Base.Attributes.:=' value ]
-- @
setFontButtonShowStyle :: (MonadIO m, IsFontButton o) => o -> Bool -> m ()
setFontButtonShowStyle obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "show-style" val

-- | Construct a `GValueConstruct` with valid value for the “@show-style@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructFontButtonShowStyle :: (IsFontButton o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructFontButtonShowStyle val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "show-style" val

#if defined(ENABLE_OVERLOADING)
data FontButtonShowStylePropertyInfo
instance AttrInfo FontButtonShowStylePropertyInfo where
    type AttrAllowedOps FontButtonShowStylePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint FontButtonShowStylePropertyInfo = IsFontButton
    type AttrSetTypeConstraint FontButtonShowStylePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint FontButtonShowStylePropertyInfo = (~) Bool
    type AttrTransferType FontButtonShowStylePropertyInfo = Bool
    type AttrGetType FontButtonShowStylePropertyInfo = Bool
    type AttrLabel FontButtonShowStylePropertyInfo = "show-style"
    type AttrOrigin FontButtonShowStylePropertyInfo = FontButton
    attrGet = getFontButtonShowStyle
    attrSet = setFontButtonShowStyle
    attrTransfer _ v = do
        return v
    attrConstruct = constructFontButtonShowStyle
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FontButton.showStyle"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FontButton.html#g:attr:showStyle"
        })
#endif

-- VVV Prop "title"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@title@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' fontButton #title
-- @
getFontButtonTitle :: (MonadIO m, IsFontButton o) => o -> m T.Text
getFontButtonTitle obj = MIO.liftIO $ checkUnexpectedNothing "getFontButtonTitle" $ B.Properties.getObjectPropertyString obj "title"

-- | Set the value of the “@title@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' fontButton [ #title 'Data.GI.Base.Attributes.:=' value ]
-- @
setFontButtonTitle :: (MonadIO m, IsFontButton o) => o -> T.Text -> m ()
setFontButtonTitle obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "title" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@title@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructFontButtonTitle :: (IsFontButton o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructFontButtonTitle val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "title" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data FontButtonTitlePropertyInfo
instance AttrInfo FontButtonTitlePropertyInfo where
    type AttrAllowedOps FontButtonTitlePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint FontButtonTitlePropertyInfo = IsFontButton
    type AttrSetTypeConstraint FontButtonTitlePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint FontButtonTitlePropertyInfo = (~) T.Text
    type AttrTransferType FontButtonTitlePropertyInfo = T.Text
    type AttrGetType FontButtonTitlePropertyInfo = T.Text
    type AttrLabel FontButtonTitlePropertyInfo = "title"
    type AttrOrigin FontButtonTitlePropertyInfo = FontButton
    attrGet = getFontButtonTitle
    attrSet = setFontButtonTitle
    attrTransfer _ v = do
        return v
    attrConstruct = constructFontButtonTitle
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FontButton.title"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FontButton.html#g:attr:title"
        })
#endif

-- VVV Prop "use-font"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@use-font@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' fontButton #useFont
-- @
getFontButtonUseFont :: (MonadIO m, IsFontButton o) => o -> m Bool
getFontButtonUseFont obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "use-font"

-- | Set the value of the “@use-font@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' fontButton [ #useFont 'Data.GI.Base.Attributes.:=' value ]
-- @
setFontButtonUseFont :: (MonadIO m, IsFontButton o) => o -> Bool -> m ()
setFontButtonUseFont obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "use-font" val

-- | Construct a `GValueConstruct` with valid value for the “@use-font@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructFontButtonUseFont :: (IsFontButton o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructFontButtonUseFont val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "use-font" val

#if defined(ENABLE_OVERLOADING)
data FontButtonUseFontPropertyInfo
instance AttrInfo FontButtonUseFontPropertyInfo where
    type AttrAllowedOps FontButtonUseFontPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint FontButtonUseFontPropertyInfo = IsFontButton
    type AttrSetTypeConstraint FontButtonUseFontPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint FontButtonUseFontPropertyInfo = (~) Bool
    type AttrTransferType FontButtonUseFontPropertyInfo = Bool
    type AttrGetType FontButtonUseFontPropertyInfo = Bool
    type AttrLabel FontButtonUseFontPropertyInfo = "use-font"
    type AttrOrigin FontButtonUseFontPropertyInfo = FontButton
    attrGet = getFontButtonUseFont
    attrSet = setFontButtonUseFont
    attrTransfer _ v = do
        return v
    attrConstruct = constructFontButtonUseFont
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FontButton.useFont"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FontButton.html#g:attr:useFont"
        })
#endif

-- VVV Prop "use-size"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@use-size@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' fontButton #useSize
-- @
getFontButtonUseSize :: (MonadIO m, IsFontButton o) => o -> m Bool
getFontButtonUseSize obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "use-size"

-- | Set the value of the “@use-size@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' fontButton [ #useSize 'Data.GI.Base.Attributes.:=' value ]
-- @
setFontButtonUseSize :: (MonadIO m, IsFontButton o) => o -> Bool -> m ()
setFontButtonUseSize obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "use-size" val

-- | Construct a `GValueConstruct` with valid value for the “@use-size@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructFontButtonUseSize :: (IsFontButton o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructFontButtonUseSize val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "use-size" val

#if defined(ENABLE_OVERLOADING)
data FontButtonUseSizePropertyInfo
instance AttrInfo FontButtonUseSizePropertyInfo where
    type AttrAllowedOps FontButtonUseSizePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint FontButtonUseSizePropertyInfo = IsFontButton
    type AttrSetTypeConstraint FontButtonUseSizePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint FontButtonUseSizePropertyInfo = (~) Bool
    type AttrTransferType FontButtonUseSizePropertyInfo = Bool
    type AttrGetType FontButtonUseSizePropertyInfo = Bool
    type AttrLabel FontButtonUseSizePropertyInfo = "use-size"
    type AttrOrigin FontButtonUseSizePropertyInfo = FontButton
    attrGet = getFontButtonUseSize
    attrSet = setFontButtonUseSize
    attrTransfer _ v = do
        return v
    attrConstruct = constructFontButtonUseSize
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FontButton.useSize"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FontButton.html#g:attr:useSize"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList FontButton
type instance O.AttributeList FontButton = FontButtonAttributeList
type FontButtonAttributeList = ('[ '("actionName", Gtk.Actionable.ActionableActionNamePropertyInfo), '("actionTarget", Gtk.Actionable.ActionableActionTargetPropertyInfo), '("alwaysShowImage", Gtk.Button.ButtonAlwaysShowImagePropertyInfo), '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("font", Gtk.FontChooser.FontChooserFontPropertyInfo), '("fontDesc", Gtk.FontChooser.FontChooserFontDescPropertyInfo), '("fontFeatures", Gtk.FontChooser.FontChooserFontFeaturesPropertyInfo), '("fontName", FontButtonFontNamePropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("image", Gtk.Button.ButtonImagePropertyInfo), '("imagePosition", Gtk.Button.ButtonImagePositionPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("label", Gtk.Button.ButtonLabelPropertyInfo), '("language", Gtk.FontChooser.FontChooserLanguagePropertyInfo), '("level", Gtk.FontChooser.FontChooserLevelPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("previewText", Gtk.FontChooser.FontChooserPreviewTextPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("relatedAction", Gtk.Activatable.ActivatableRelatedActionPropertyInfo), '("relief", Gtk.Button.ButtonReliefPropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("showPreviewEntry", Gtk.FontChooser.FontChooserShowPreviewEntryPropertyInfo), '("showSize", FontButtonShowSizePropertyInfo), '("showStyle", FontButtonShowStylePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("title", FontButtonTitlePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("useActionAppearance", Gtk.Activatable.ActivatableUseActionAppearancePropertyInfo), '("useFont", FontButtonUseFontPropertyInfo), '("useSize", FontButtonUseSizePropertyInfo), '("useStock", Gtk.Button.ButtonUseStockPropertyInfo), '("useUnderline", Gtk.Button.ButtonUseUnderlinePropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo), '("xalign", Gtk.Button.ButtonXalignPropertyInfo), '("yalign", Gtk.Button.ButtonYalignPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
fontButtonFontName :: AttrLabelProxy "fontName"
fontButtonFontName = AttrLabelProxy

fontButtonShowSize :: AttrLabelProxy "showSize"
fontButtonShowSize = AttrLabelProxy

fontButtonShowStyle :: AttrLabelProxy "showStyle"
fontButtonShowStyle = AttrLabelProxy

fontButtonTitle :: AttrLabelProxy "title"
fontButtonTitle = AttrLabelProxy

fontButtonUseFont :: AttrLabelProxy "useFont"
fontButtonUseFont = AttrLabelProxy

fontButtonUseSize :: AttrLabelProxy "useSize"
fontButtonUseSize = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList FontButton = FontButtonSignalList
type FontButtonSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("activate", Gtk.Button.ButtonActivateSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("clicked", Gtk.Button.ButtonClickedSignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enter", Gtk.Button.ButtonEnterSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("fontActivated", Gtk.FontChooser.FontChooserFontActivatedSignalInfo), '("fontSet", FontButtonFontSetSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leave", Gtk.Button.ButtonLeaveSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("pressed", Gtk.Button.ButtonPressedSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("released", Gtk.Button.ButtonReleasedSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method FontButton::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "FontButton" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_font_button_new" gtk_font_button_new :: 
    IO (Ptr FontButton)

-- | Creates a new font picker widget.
-- 
-- /Since: 2.4/
fontButtonNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m FontButton
    -- ^ __Returns:__ a new font picker widget.
fontButtonNew  = liftIO $ do
    result <- gtk_font_button_new
    checkUnexpectedReturnNULL "fontButtonNew" result
    result' <- (newObject FontButton) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method FontButton::new_with_font
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "fontname"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "Name of font to display in font chooser dialog"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "FontButton" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_font_button_new_with_font" gtk_font_button_new_with_font :: 
    CString ->                              -- fontname : TBasicType TUTF8
    IO (Ptr FontButton)

-- | Creates a new font picker widget.
-- 
-- /Since: 2.4/
fontButtonNewWithFont ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@fontname@/: Name of font to display in font chooser dialog
    -> m FontButton
    -- ^ __Returns:__ a new font picker widget.
fontButtonNewWithFont fontname = liftIO $ do
    fontname' <- textToCString fontname
    result <- gtk_font_button_new_with_font fontname'
    checkUnexpectedReturnNULL "fontButtonNewWithFont" result
    result' <- (newObject FontButton) result
    freeMem fontname'
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method FontButton::get_font_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "font_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FontButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFontButton" , sinceVersion = Nothing }
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

foreign import ccall "gtk_font_button_get_font_name" gtk_font_button_get_font_name :: 
    Ptr FontButton ->                       -- font_button : TInterface (Name {namespace = "Gtk", name = "FontButton"})
    IO CString

{-# DEPRECATED fontButtonGetFontName ["(Since version 3.22)","Use 'GI.Gtk.Interfaces.FontChooser.fontChooserGetFont' instead"] #-}
-- | Retrieves the name of the currently selected font. This name includes
-- style and size information as well. If you want to render something
-- with the font, use this string with 'GI.Pango.Functions.fontDescriptionFromString' .
-- If you’re interested in peeking certain values (family name,
-- style, size, weight) just query these properties from the
-- t'GI.Pango.Structs.FontDescription.FontDescription' object.
-- 
-- /Since: 2.4/
fontButtonGetFontName ::
    (B.CallStack.HasCallStack, MonadIO m, IsFontButton a) =>
    a
    -- ^ /@fontButton@/: a t'GI.Gtk.Objects.FontButton.FontButton'
    -> m T.Text
    -- ^ __Returns:__ an internal copy of the font name which must not be freed.
fontButtonGetFontName fontButton = liftIO $ do
    fontButton' <- unsafeManagedPtrCastPtr fontButton
    result <- gtk_font_button_get_font_name fontButton'
    checkUnexpectedReturnNULL "fontButtonGetFontName" result
    result' <- cstringToText result
    touchManagedPtr fontButton
    return result'

#if defined(ENABLE_OVERLOADING)
data FontButtonGetFontNameMethodInfo
instance (signature ~ (m T.Text), MonadIO m, IsFontButton a) => O.OverloadedMethod FontButtonGetFontNameMethodInfo a signature where
    overloadedMethod = fontButtonGetFontName

instance O.OverloadedMethodInfo FontButtonGetFontNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FontButton.fontButtonGetFontName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FontButton.html#v:fontButtonGetFontName"
        })


#endif

-- method FontButton::get_show_size
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "font_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FontButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFontButton" , sinceVersion = Nothing }
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

foreign import ccall "gtk_font_button_get_show_size" gtk_font_button_get_show_size :: 
    Ptr FontButton ->                       -- font_button : TInterface (Name {namespace = "Gtk", name = "FontButton"})
    IO CInt

-- | Returns whether the font size will be shown in the label.
-- 
-- /Since: 2.4/
fontButtonGetShowSize ::
    (B.CallStack.HasCallStack, MonadIO m, IsFontButton a) =>
    a
    -- ^ /@fontButton@/: a t'GI.Gtk.Objects.FontButton.FontButton'
    -> m Bool
    -- ^ __Returns:__ whether the font size will be shown in the label.
fontButtonGetShowSize fontButton = liftIO $ do
    fontButton' <- unsafeManagedPtrCastPtr fontButton
    result <- gtk_font_button_get_show_size fontButton'
    let result' = (/= 0) result
    touchManagedPtr fontButton
    return result'

#if defined(ENABLE_OVERLOADING)
data FontButtonGetShowSizeMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsFontButton a) => O.OverloadedMethod FontButtonGetShowSizeMethodInfo a signature where
    overloadedMethod = fontButtonGetShowSize

instance O.OverloadedMethodInfo FontButtonGetShowSizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FontButton.fontButtonGetShowSize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FontButton.html#v:fontButtonGetShowSize"
        })


#endif

-- method FontButton::get_show_style
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "font_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FontButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFontButton" , sinceVersion = Nothing }
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

foreign import ccall "gtk_font_button_get_show_style" gtk_font_button_get_show_style :: 
    Ptr FontButton ->                       -- font_button : TInterface (Name {namespace = "Gtk", name = "FontButton"})
    IO CInt

-- | Returns whether the name of the font style will be shown in the label.
-- 
-- /Since: 2.4/
fontButtonGetShowStyle ::
    (B.CallStack.HasCallStack, MonadIO m, IsFontButton a) =>
    a
    -- ^ /@fontButton@/: a t'GI.Gtk.Objects.FontButton.FontButton'
    -> m Bool
    -- ^ __Returns:__ whether the font style will be shown in the label.
fontButtonGetShowStyle fontButton = liftIO $ do
    fontButton' <- unsafeManagedPtrCastPtr fontButton
    result <- gtk_font_button_get_show_style fontButton'
    let result' = (/= 0) result
    touchManagedPtr fontButton
    return result'

#if defined(ENABLE_OVERLOADING)
data FontButtonGetShowStyleMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsFontButton a) => O.OverloadedMethod FontButtonGetShowStyleMethodInfo a signature where
    overloadedMethod = fontButtonGetShowStyle

instance O.OverloadedMethodInfo FontButtonGetShowStyleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FontButton.fontButtonGetShowStyle",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FontButton.html#v:fontButtonGetShowStyle"
        })


#endif

-- method FontButton::get_title
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "font_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FontButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFontButton" , sinceVersion = Nothing }
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

foreign import ccall "gtk_font_button_get_title" gtk_font_button_get_title :: 
    Ptr FontButton ->                       -- font_button : TInterface (Name {namespace = "Gtk", name = "FontButton"})
    IO CString

-- | Retrieves the title of the font chooser dialog.
-- 
-- /Since: 2.4/
fontButtonGetTitle ::
    (B.CallStack.HasCallStack, MonadIO m, IsFontButton a) =>
    a
    -- ^ /@fontButton@/: a t'GI.Gtk.Objects.FontButton.FontButton'
    -> m T.Text
    -- ^ __Returns:__ an internal copy of the title string which must not be freed.
fontButtonGetTitle fontButton = liftIO $ do
    fontButton' <- unsafeManagedPtrCastPtr fontButton
    result <- gtk_font_button_get_title fontButton'
    checkUnexpectedReturnNULL "fontButtonGetTitle" result
    result' <- cstringToText result
    touchManagedPtr fontButton
    return result'

#if defined(ENABLE_OVERLOADING)
data FontButtonGetTitleMethodInfo
instance (signature ~ (m T.Text), MonadIO m, IsFontButton a) => O.OverloadedMethod FontButtonGetTitleMethodInfo a signature where
    overloadedMethod = fontButtonGetTitle

instance O.OverloadedMethodInfo FontButtonGetTitleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FontButton.fontButtonGetTitle",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FontButton.html#v:fontButtonGetTitle"
        })


#endif

-- method FontButton::get_use_font
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "font_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FontButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFontButton" , sinceVersion = Nothing }
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

foreign import ccall "gtk_font_button_get_use_font" gtk_font_button_get_use_font :: 
    Ptr FontButton ->                       -- font_button : TInterface (Name {namespace = "Gtk", name = "FontButton"})
    IO CInt

-- | Returns whether the selected font is used in the label.
-- 
-- /Since: 2.4/
fontButtonGetUseFont ::
    (B.CallStack.HasCallStack, MonadIO m, IsFontButton a) =>
    a
    -- ^ /@fontButton@/: a t'GI.Gtk.Objects.FontButton.FontButton'
    -> m Bool
    -- ^ __Returns:__ whether the selected font is used in the label.
fontButtonGetUseFont fontButton = liftIO $ do
    fontButton' <- unsafeManagedPtrCastPtr fontButton
    result <- gtk_font_button_get_use_font fontButton'
    let result' = (/= 0) result
    touchManagedPtr fontButton
    return result'

#if defined(ENABLE_OVERLOADING)
data FontButtonGetUseFontMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsFontButton a) => O.OverloadedMethod FontButtonGetUseFontMethodInfo a signature where
    overloadedMethod = fontButtonGetUseFont

instance O.OverloadedMethodInfo FontButtonGetUseFontMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FontButton.fontButtonGetUseFont",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FontButton.html#v:fontButtonGetUseFont"
        })


#endif

-- method FontButton::get_use_size
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "font_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FontButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFontButton" , sinceVersion = Nothing }
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

foreign import ccall "gtk_font_button_get_use_size" gtk_font_button_get_use_size :: 
    Ptr FontButton ->                       -- font_button : TInterface (Name {namespace = "Gtk", name = "FontButton"})
    IO CInt

-- | Returns whether the selected size is used in the label.
-- 
-- /Since: 2.4/
fontButtonGetUseSize ::
    (B.CallStack.HasCallStack, MonadIO m, IsFontButton a) =>
    a
    -- ^ /@fontButton@/: a t'GI.Gtk.Objects.FontButton.FontButton'
    -> m Bool
    -- ^ __Returns:__ whether the selected size is used in the label.
fontButtonGetUseSize fontButton = liftIO $ do
    fontButton' <- unsafeManagedPtrCastPtr fontButton
    result <- gtk_font_button_get_use_size fontButton'
    let result' = (/= 0) result
    touchManagedPtr fontButton
    return result'

#if defined(ENABLE_OVERLOADING)
data FontButtonGetUseSizeMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsFontButton a) => O.OverloadedMethod FontButtonGetUseSizeMethodInfo a signature where
    overloadedMethod = fontButtonGetUseSize

instance O.OverloadedMethodInfo FontButtonGetUseSizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FontButton.fontButtonGetUseSize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FontButton.html#v:fontButtonGetUseSize"
        })


#endif

-- method FontButton::set_font_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "font_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FontButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFontButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "fontname"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "Name of font to display in font chooser dialog"
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

foreign import ccall "gtk_font_button_set_font_name" gtk_font_button_set_font_name :: 
    Ptr FontButton ->                       -- font_button : TInterface (Name {namespace = "Gtk", name = "FontButton"})
    CString ->                              -- fontname : TBasicType TUTF8
    IO CInt

{-# DEPRECATED fontButtonSetFontName ["(Since version 3.22)","Use 'GI.Gtk.Interfaces.FontChooser.fontChooserSetFont' instead"] #-}
-- | Sets or updates the currently-displayed font in font picker dialog.
-- 
-- /Since: 2.4/
fontButtonSetFontName ::
    (B.CallStack.HasCallStack, MonadIO m, IsFontButton a) =>
    a
    -- ^ /@fontButton@/: a t'GI.Gtk.Objects.FontButton.FontButton'
    -> T.Text
    -- ^ /@fontname@/: Name of font to display in font chooser dialog
    -> m Bool
    -- ^ __Returns:__ 'P.True'
fontButtonSetFontName fontButton fontname = liftIO $ do
    fontButton' <- unsafeManagedPtrCastPtr fontButton
    fontname' <- textToCString fontname
    result <- gtk_font_button_set_font_name fontButton' fontname'
    let result' = (/= 0) result
    touchManagedPtr fontButton
    freeMem fontname'
    return result'

#if defined(ENABLE_OVERLOADING)
data FontButtonSetFontNameMethodInfo
instance (signature ~ (T.Text -> m Bool), MonadIO m, IsFontButton a) => O.OverloadedMethod FontButtonSetFontNameMethodInfo a signature where
    overloadedMethod = fontButtonSetFontName

instance O.OverloadedMethodInfo FontButtonSetFontNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FontButton.fontButtonSetFontName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FontButton.html#v:fontButtonSetFontName"
        })


#endif

-- method FontButton::set_show_size
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "font_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FontButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFontButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "show_size"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "%TRUE if font size should be displayed in dialog."
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

foreign import ccall "gtk_font_button_set_show_size" gtk_font_button_set_show_size :: 
    Ptr FontButton ->                       -- font_button : TInterface (Name {namespace = "Gtk", name = "FontButton"})
    CInt ->                                 -- show_size : TBasicType TBoolean
    IO ()

-- | If /@showSize@/ is 'P.True', the font size will be displayed along with the name of the selected font.
-- 
-- /Since: 2.4/
fontButtonSetShowSize ::
    (B.CallStack.HasCallStack, MonadIO m, IsFontButton a) =>
    a
    -- ^ /@fontButton@/: a t'GI.Gtk.Objects.FontButton.FontButton'
    -> Bool
    -- ^ /@showSize@/: 'P.True' if font size should be displayed in dialog.
    -> m ()
fontButtonSetShowSize fontButton showSize = liftIO $ do
    fontButton' <- unsafeManagedPtrCastPtr fontButton
    let showSize' = (fromIntegral . fromEnum) showSize
    gtk_font_button_set_show_size fontButton' showSize'
    touchManagedPtr fontButton
    return ()

#if defined(ENABLE_OVERLOADING)
data FontButtonSetShowSizeMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsFontButton a) => O.OverloadedMethod FontButtonSetShowSizeMethodInfo a signature where
    overloadedMethod = fontButtonSetShowSize

instance O.OverloadedMethodInfo FontButtonSetShowSizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FontButton.fontButtonSetShowSize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FontButton.html#v:fontButtonSetShowSize"
        })


#endif

-- method FontButton::set_show_style
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "font_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FontButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFontButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "show_style"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "%TRUE if font style should be displayed in label."
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

foreign import ccall "gtk_font_button_set_show_style" gtk_font_button_set_show_style :: 
    Ptr FontButton ->                       -- font_button : TInterface (Name {namespace = "Gtk", name = "FontButton"})
    CInt ->                                 -- show_style : TBasicType TBoolean
    IO ()

-- | If /@showStyle@/ is 'P.True', the font style will be displayed along with name of the selected font.
-- 
-- /Since: 2.4/
fontButtonSetShowStyle ::
    (B.CallStack.HasCallStack, MonadIO m, IsFontButton a) =>
    a
    -- ^ /@fontButton@/: a t'GI.Gtk.Objects.FontButton.FontButton'
    -> Bool
    -- ^ /@showStyle@/: 'P.True' if font style should be displayed in label.
    -> m ()
fontButtonSetShowStyle fontButton showStyle = liftIO $ do
    fontButton' <- unsafeManagedPtrCastPtr fontButton
    let showStyle' = (fromIntegral . fromEnum) showStyle
    gtk_font_button_set_show_style fontButton' showStyle'
    touchManagedPtr fontButton
    return ()

#if defined(ENABLE_OVERLOADING)
data FontButtonSetShowStyleMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsFontButton a) => O.OverloadedMethod FontButtonSetShowStyleMethodInfo a signature where
    overloadedMethod = fontButtonSetShowStyle

instance O.OverloadedMethodInfo FontButtonSetShowStyleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FontButton.fontButtonSetShowStyle",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FontButton.html#v:fontButtonSetShowStyle"
        })


#endif

-- method FontButton::set_title
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "font_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FontButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFontButton" , sinceVersion = Nothing }
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
--                 { rawDocText =
--                     Just "a string containing the font chooser dialog title"
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

foreign import ccall "gtk_font_button_set_title" gtk_font_button_set_title :: 
    Ptr FontButton ->                       -- font_button : TInterface (Name {namespace = "Gtk", name = "FontButton"})
    CString ->                              -- title : TBasicType TUTF8
    IO ()

-- | Sets the title for the font chooser dialog.
-- 
-- /Since: 2.4/
fontButtonSetTitle ::
    (B.CallStack.HasCallStack, MonadIO m, IsFontButton a) =>
    a
    -- ^ /@fontButton@/: a t'GI.Gtk.Objects.FontButton.FontButton'
    -> T.Text
    -- ^ /@title@/: a string containing the font chooser dialog title
    -> m ()
fontButtonSetTitle fontButton title = liftIO $ do
    fontButton' <- unsafeManagedPtrCastPtr fontButton
    title' <- textToCString title
    gtk_font_button_set_title fontButton' title'
    touchManagedPtr fontButton
    freeMem title'
    return ()

#if defined(ENABLE_OVERLOADING)
data FontButtonSetTitleMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsFontButton a) => O.OverloadedMethod FontButtonSetTitleMethodInfo a signature where
    overloadedMethod = fontButtonSetTitle

instance O.OverloadedMethodInfo FontButtonSetTitleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FontButton.fontButtonSetTitle",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FontButton.html#v:fontButtonSetTitle"
        })


#endif

-- method FontButton::set_use_font
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "font_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FontButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFontButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "use_font"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "If %TRUE, font name will be written using font chosen."
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

foreign import ccall "gtk_font_button_set_use_font" gtk_font_button_set_use_font :: 
    Ptr FontButton ->                       -- font_button : TInterface (Name {namespace = "Gtk", name = "FontButton"})
    CInt ->                                 -- use_font : TBasicType TBoolean
    IO ()

-- | If /@useFont@/ is 'P.True', the font name will be written using the selected font.
-- 
-- /Since: 2.4/
fontButtonSetUseFont ::
    (B.CallStack.HasCallStack, MonadIO m, IsFontButton a) =>
    a
    -- ^ /@fontButton@/: a t'GI.Gtk.Objects.FontButton.FontButton'
    -> Bool
    -- ^ /@useFont@/: If 'P.True', font name will be written using font chosen.
    -> m ()
fontButtonSetUseFont fontButton useFont = liftIO $ do
    fontButton' <- unsafeManagedPtrCastPtr fontButton
    let useFont' = (fromIntegral . fromEnum) useFont
    gtk_font_button_set_use_font fontButton' useFont'
    touchManagedPtr fontButton
    return ()

#if defined(ENABLE_OVERLOADING)
data FontButtonSetUseFontMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsFontButton a) => O.OverloadedMethod FontButtonSetUseFontMethodInfo a signature where
    overloadedMethod = fontButtonSetUseFont

instance O.OverloadedMethodInfo FontButtonSetUseFontMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FontButton.fontButtonSetUseFont",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FontButton.html#v:fontButtonSetUseFont"
        })


#endif

-- method FontButton::set_use_size
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "font_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FontButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFontButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "use_size"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "If %TRUE, font name will be written using the selected size."
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

foreign import ccall "gtk_font_button_set_use_size" gtk_font_button_set_use_size :: 
    Ptr FontButton ->                       -- font_button : TInterface (Name {namespace = "Gtk", name = "FontButton"})
    CInt ->                                 -- use_size : TBasicType TBoolean
    IO ()

-- | If /@useSize@/ is 'P.True', the font name will be written using the selected size.
-- 
-- /Since: 2.4/
fontButtonSetUseSize ::
    (B.CallStack.HasCallStack, MonadIO m, IsFontButton a) =>
    a
    -- ^ /@fontButton@/: a t'GI.Gtk.Objects.FontButton.FontButton'
    -> Bool
    -- ^ /@useSize@/: If 'P.True', font name will be written using the selected size.
    -> m ()
fontButtonSetUseSize fontButton useSize = liftIO $ do
    fontButton' <- unsafeManagedPtrCastPtr fontButton
    let useSize' = (fromIntegral . fromEnum) useSize
    gtk_font_button_set_use_size fontButton' useSize'
    touchManagedPtr fontButton
    return ()

#if defined(ENABLE_OVERLOADING)
data FontButtonSetUseSizeMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsFontButton a) => O.OverloadedMethod FontButtonSetUseSizeMethodInfo a signature where
    overloadedMethod = fontButtonSetUseSize

instance O.OverloadedMethodInfo FontButtonSetUseSizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FontButton.fontButtonSetUseSize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FontButton.html#v:fontButtonSetUseSize"
        })


#endif


