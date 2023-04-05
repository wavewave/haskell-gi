{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- The t'GI.Gtk.Objects.Button.Button' widget is generally used to trigger a callback function that is
-- called when the button is pressed.  The various signals and how to use them
-- are outlined below.
-- 
-- The t'GI.Gtk.Objects.Button.Button' widget can hold any valid child widget.  That is, it can hold
-- almost any other standard t'GI.Gtk.Objects.Widget.Widget'.  The most commonly used child is the
-- t'GI.Gtk.Objects.Label.Label'.
-- 
-- = CSS nodes
-- 
-- GtkButton has a single CSS node with name button. The node will get the
-- style classes .image-button or .text-button, if the content is just an
-- image or label, respectively. It may also receive the .flat style class.
-- 
-- Other style classes that are commonly used with GtkButton include
-- .suggested-action and .destructive-action. In special cases, buttons
-- can be made round by adding the .circular style class.
-- 
-- Button-like widgets like t'GI.Gtk.Objects.ToggleButton.ToggleButton', t'GI.Gtk.Objects.MenuButton.MenuButton', t'GI.Gtk.Objects.VolumeButton.VolumeButton',
-- t'GI.Gtk.Objects.LockButton.LockButton', t'GI.Gtk.Objects.ColorButton.ColorButton', t'GI.Gtk.Objects.FontButton.FontButton' or t'GI.Gtk.Objects.FileChooserButton.FileChooserButton' use
-- style classes such as .toggle, .popup, .scale, .lock, .color, .font, .file
-- to differentiate themselves from a plain GtkButton.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.Button
    ( 

-- * Exported types
    Button(..)                              ,
    IsButton                                ,
    toButton                                ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [clicked]("GI.Gtk.Objects.Button#g:method:clicked"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [doSetRelatedAction]("GI.Gtk.Interfaces.Activatable#g:method:doSetRelatedAction"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [enter]("GI.Gtk.Objects.Button#g:method:enter"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [leave]("GI.Gtk.Objects.Button#g:method:leave"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [pressed]("GI.Gtk.Objects.Button#g:method:pressed"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [released]("GI.Gtk.Objects.Button#g:method:released"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [syncActionProperties]("GI.Gtk.Interfaces.Activatable#g:method:syncActionProperties"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getActionName]("GI.Gtk.Interfaces.Actionable#g:method:getActionName"), [getActionTargetValue]("GI.Gtk.Interfaces.Actionable#g:method:getActionTargetValue"), [getAlignment]("GI.Gtk.Objects.Button#g:method:getAlignment"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAlwaysShowImage]("GI.Gtk.Objects.Button#g:method:getAlwaysShowImage"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChild]("GI.Gtk.Objects.Bin#g:method:getChild"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEventWindow]("GI.Gtk.Objects.Button#g:method:getEventWindow"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Button#g:method:getFocusOnClick"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getImage]("GI.Gtk.Objects.Button#g:method:getImage"), [getImagePosition]("GI.Gtk.Objects.Button#g:method:getImagePosition"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getLabel]("GI.Gtk.Objects.Button#g:method:getLabel"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRelatedAction]("GI.Gtk.Interfaces.Activatable#g:method:getRelatedAction"), [getRelief]("GI.Gtk.Objects.Button#g:method:getRelief"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getUseActionAppearance]("GI.Gtk.Interfaces.Activatable#g:method:getUseActionAppearance"), [getUseStock]("GI.Gtk.Objects.Button#g:method:getUseStock"), [getUseUnderline]("GI.Gtk.Objects.Button#g:method:getUseUnderline"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setActionName]("GI.Gtk.Interfaces.Actionable#g:method:setActionName"), [setActionTargetValue]("GI.Gtk.Interfaces.Actionable#g:method:setActionTargetValue"), [setAlignment]("GI.Gtk.Objects.Button#g:method:setAlignment"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAlwaysShowImage]("GI.Gtk.Objects.Button#g:method:setAlwaysShowImage"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDetailedActionName]("GI.Gtk.Interfaces.Actionable#g:method:setDetailedActionName"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Button#g:method:setFocusOnClick"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setImage]("GI.Gtk.Objects.Button#g:method:setImage"), [setImagePosition]("GI.Gtk.Objects.Button#g:method:setImagePosition"), [setLabel]("GI.Gtk.Objects.Button#g:method:setLabel"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setRelatedAction]("GI.Gtk.Interfaces.Activatable#g:method:setRelatedAction"), [setRelief]("GI.Gtk.Objects.Button#g:method:setRelief"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setUseActionAppearance]("GI.Gtk.Interfaces.Activatable#g:method:setUseActionAppearance"), [setUseStock]("GI.Gtk.Objects.Button#g:method:setUseStock"), [setUseUnderline]("GI.Gtk.Objects.Button#g:method:setUseUnderline"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveButtonMethod                     ,
#endif

-- ** clicked #method:clicked#

#if defined(ENABLE_OVERLOADING)
    ButtonClickedMethodInfo                 ,
#endif
    buttonClicked                           ,


-- ** enter #method:enter#

#if defined(ENABLE_OVERLOADING)
    ButtonEnterMethodInfo                   ,
#endif
    buttonEnter                             ,


-- ** getAlignment #method:getAlignment#

#if defined(ENABLE_OVERLOADING)
    ButtonGetAlignmentMethodInfo            ,
#endif
    buttonGetAlignment                      ,


-- ** getAlwaysShowImage #method:getAlwaysShowImage#

#if defined(ENABLE_OVERLOADING)
    ButtonGetAlwaysShowImageMethodInfo      ,
#endif
    buttonGetAlwaysShowImage                ,


-- ** getEventWindow #method:getEventWindow#

#if defined(ENABLE_OVERLOADING)
    ButtonGetEventWindowMethodInfo          ,
#endif
    buttonGetEventWindow                    ,


-- ** getFocusOnClick #method:getFocusOnClick#

#if defined(ENABLE_OVERLOADING)
    ButtonGetFocusOnClickMethodInfo         ,
#endif
    buttonGetFocusOnClick                   ,


-- ** getImage #method:getImage#

#if defined(ENABLE_OVERLOADING)
    ButtonGetImageMethodInfo                ,
#endif
    buttonGetImage                          ,


-- ** getImagePosition #method:getImagePosition#

#if defined(ENABLE_OVERLOADING)
    ButtonGetImagePositionMethodInfo        ,
#endif
    buttonGetImagePosition                  ,


-- ** getLabel #method:getLabel#

#if defined(ENABLE_OVERLOADING)
    ButtonGetLabelMethodInfo                ,
#endif
    buttonGetLabel                          ,


-- ** getRelief #method:getRelief#

#if defined(ENABLE_OVERLOADING)
    ButtonGetReliefMethodInfo               ,
#endif
    buttonGetRelief                         ,


-- ** getUseStock #method:getUseStock#

#if defined(ENABLE_OVERLOADING)
    ButtonGetUseStockMethodInfo             ,
#endif
    buttonGetUseStock                       ,


-- ** getUseUnderline #method:getUseUnderline#

#if defined(ENABLE_OVERLOADING)
    ButtonGetUseUnderlineMethodInfo         ,
#endif
    buttonGetUseUnderline                   ,


-- ** leave #method:leave#

#if defined(ENABLE_OVERLOADING)
    ButtonLeaveMethodInfo                   ,
#endif
    buttonLeave                             ,


-- ** new #method:new#

    buttonNew                               ,


-- ** newFromIconName #method:newFromIconName#

    buttonNewFromIconName                   ,


-- ** newFromStock #method:newFromStock#

    buttonNewFromStock                      ,


-- ** newWithLabel #method:newWithLabel#

    buttonNewWithLabel                      ,


-- ** newWithMnemonic #method:newWithMnemonic#

    buttonNewWithMnemonic                   ,


-- ** pressed #method:pressed#

#if defined(ENABLE_OVERLOADING)
    ButtonPressedMethodInfo                 ,
#endif
    buttonPressed                           ,


-- ** released #method:released#

#if defined(ENABLE_OVERLOADING)
    ButtonReleasedMethodInfo                ,
#endif
    buttonReleased                          ,


-- ** setAlignment #method:setAlignment#

#if defined(ENABLE_OVERLOADING)
    ButtonSetAlignmentMethodInfo            ,
#endif
    buttonSetAlignment                      ,


-- ** setAlwaysShowImage #method:setAlwaysShowImage#

#if defined(ENABLE_OVERLOADING)
    ButtonSetAlwaysShowImageMethodInfo      ,
#endif
    buttonSetAlwaysShowImage                ,


-- ** setFocusOnClick #method:setFocusOnClick#

#if defined(ENABLE_OVERLOADING)
    ButtonSetFocusOnClickMethodInfo         ,
#endif
    buttonSetFocusOnClick                   ,


-- ** setImage #method:setImage#

#if defined(ENABLE_OVERLOADING)
    ButtonSetImageMethodInfo                ,
#endif
    buttonSetImage                          ,


-- ** setImagePosition #method:setImagePosition#

#if defined(ENABLE_OVERLOADING)
    ButtonSetImagePositionMethodInfo        ,
#endif
    buttonSetImagePosition                  ,


-- ** setLabel #method:setLabel#

#if defined(ENABLE_OVERLOADING)
    ButtonSetLabelMethodInfo                ,
#endif
    buttonSetLabel                          ,


-- ** setRelief #method:setRelief#

#if defined(ENABLE_OVERLOADING)
    ButtonSetReliefMethodInfo               ,
#endif
    buttonSetRelief                         ,


-- ** setUseStock #method:setUseStock#

#if defined(ENABLE_OVERLOADING)
    ButtonSetUseStockMethodInfo             ,
#endif
    buttonSetUseStock                       ,


-- ** setUseUnderline #method:setUseUnderline#

#if defined(ENABLE_OVERLOADING)
    ButtonSetUseUnderlineMethodInfo         ,
#endif
    buttonSetUseUnderline                   ,




 -- * Properties


-- ** alwaysShowImage #attr:alwaysShowImage#
-- | If 'P.True', the button will ignore the [Settings:gtkButtonImages]("GI.Gtk.Objects.Settings#g:attr:gtkButtonImages")
-- setting and always show the image, if available.
-- 
-- Use this property if the button would be useless or hard to use
-- without the image.
-- 
-- /Since: 3.6/

#if defined(ENABLE_OVERLOADING)
    ButtonAlwaysShowImagePropertyInfo       ,
#endif
#if defined(ENABLE_OVERLOADING)
    buttonAlwaysShowImage                   ,
#endif
    constructButtonAlwaysShowImage          ,
    getButtonAlwaysShowImage                ,
    setButtonAlwaysShowImage                ,


-- ** image #attr:image#
-- | The child widget to appear next to the button text.
-- 
-- /Since: 2.6/

#if defined(ENABLE_OVERLOADING)
    ButtonImagePropertyInfo                 ,
#endif
#if defined(ENABLE_OVERLOADING)
    buttonImage                             ,
#endif
    clearButtonImage                        ,
    constructButtonImage                    ,
    getButtonImage                          ,
    setButtonImage                          ,


-- ** imagePosition #attr:imagePosition#
-- | The position of the image relative to the text inside the button.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    ButtonImagePositionPropertyInfo         ,
#endif
#if defined(ENABLE_OVERLOADING)
    buttonImagePosition                     ,
#endif
    constructButtonImagePosition            ,
    getButtonImagePosition                  ,
    setButtonImagePosition                  ,


-- ** label #attr:label#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ButtonLabelPropertyInfo                 ,
#endif
#if defined(ENABLE_OVERLOADING)
    buttonLabel                             ,
#endif
    constructButtonLabel                    ,
    getButtonLabel                          ,
    setButtonLabel                          ,


-- ** relief #attr:relief#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ButtonReliefPropertyInfo                ,
#endif
#if defined(ENABLE_OVERLOADING)
    buttonRelief                            ,
#endif
    constructButtonRelief                   ,
    getButtonRelief                         ,
    setButtonRelief                         ,


-- ** useStock #attr:useStock#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ButtonUseStockPropertyInfo              ,
#endif
#if defined(ENABLE_OVERLOADING)
    buttonUseStock                          ,
#endif
    constructButtonUseStock                 ,
    getButtonUseStock                       ,
    setButtonUseStock                       ,


-- ** useUnderline #attr:useUnderline#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ButtonUseUnderlinePropertyInfo          ,
#endif
#if defined(ENABLE_OVERLOADING)
    buttonUseUnderline                      ,
#endif
    constructButtonUseUnderline             ,
    getButtonUseUnderline                   ,
    setButtonUseUnderline                   ,


-- ** xalign #attr:xalign#
-- | If the child of the button is a t'GI.Gtk.Objects.Misc.Misc' or t'GI.Gtk.Objects.Alignment.Alignment', this property
-- can be used to control its horizontal alignment. 0.0 is left aligned,
-- 1.0 is right aligned.
-- 
-- /Since: 2.4/

#if defined(ENABLE_OVERLOADING)
    ButtonXalignPropertyInfo                ,
#endif
#if defined(ENABLE_OVERLOADING)
    buttonXalign                            ,
#endif
    constructButtonXalign                   ,
    getButtonXalign                         ,
    setButtonXalign                         ,


-- ** yalign #attr:yalign#
-- | If the child of the button is a t'GI.Gtk.Objects.Misc.Misc' or t'GI.Gtk.Objects.Alignment.Alignment', this property
-- can be used to control its vertical alignment. 0.0 is top aligned,
-- 1.0 is bottom aligned.
-- 
-- /Since: 2.4/

#if defined(ENABLE_OVERLOADING)
    ButtonYalignPropertyInfo                ,
#endif
#if defined(ENABLE_OVERLOADING)
    buttonYalign                            ,
#endif
    constructButtonYalign                   ,
    getButtonYalign                         ,
    setButtonYalign                         ,




 -- * Signals


-- ** activate #signal:activate#

    ButtonActivateCallback                  ,
#if defined(ENABLE_OVERLOADING)
    ButtonActivateSignalInfo                ,
#endif
    afterButtonActivate                     ,
    onButtonActivate                        ,


-- ** clicked #signal:clicked#

    ButtonClickedCallback                   ,
#if defined(ENABLE_OVERLOADING)
    ButtonClickedSignalInfo                 ,
#endif
    afterButtonClicked                      ,
    onButtonClicked                         ,


-- ** enter #signal:enter#

    ButtonEnterCallback                     ,
#if defined(ENABLE_OVERLOADING)
    ButtonEnterSignalInfo                   ,
#endif
    afterButtonEnter                        ,
    onButtonEnter                           ,


-- ** leave #signal:leave#

    ButtonLeaveCallback                     ,
#if defined(ENABLE_OVERLOADING)
    ButtonLeaveSignalInfo                   ,
#endif
    afterButtonLeave                        ,
    onButtonLeave                           ,


-- ** pressed #signal:pressed#

    ButtonPressedCallback                   ,
#if defined(ENABLE_OVERLOADING)
    ButtonPressedSignalInfo                 ,
#endif
    afterButtonPressed                      ,
    onButtonPressed                         ,


-- ** released #signal:released#

    ButtonReleasedCallback                  ,
#if defined(ENABLE_OVERLOADING)
    ButtonReleasedSignalInfo                ,
#endif
    afterButtonReleased                     ,
    onButtonReleased                        ,




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
import qualified GI.Gdk.Objects.Window as Gdk.Window
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Actionable as Gtk.Actionable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Activatable as Gtk.Activatable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Objects.Bin as Gtk.Bin
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype Button = Button (SP.ManagedPtr Button)
    deriving (Eq)

instance SP.ManagedPtrNewtype Button where
    toManagedPtr (Button p) = p

foreign import ccall "gtk_button_get_type"
    c_gtk_button_get_type :: IO B.Types.GType

instance B.Types.TypedObject Button where
    glibType = c_gtk_button_get_type

instance B.Types.GObject Button

-- | Type class for types which can be safely cast to `Button`, for instance with `toButton`.
class (SP.GObject o, O.IsDescendantOf Button o) => IsButton o
instance (SP.GObject o, O.IsDescendantOf Button o) => IsButton o

instance O.HasParentTypes Button
type instance O.ParentTypes Button = '[Gtk.Bin.Bin, Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Actionable.Actionable, Gtk.Activatable.Activatable, Gtk.Buildable.Buildable]

-- | Cast to `Button`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toButton :: (MIO.MonadIO m, IsButton o) => o -> m Button
toButton = MIO.liftIO . B.ManagedPtr.unsafeCastTo Button

-- | Convert 'Button' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe Button) where
    gvalueGType_ = c_gtk_button_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr Button)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr Button)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject Button ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveButtonMethod (t :: Symbol) (o :: *) :: * where
    ResolveButtonMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveButtonMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveButtonMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveButtonMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveButtonMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveButtonMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveButtonMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveButtonMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveButtonMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveButtonMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveButtonMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveButtonMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveButtonMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveButtonMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveButtonMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveButtonMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveButtonMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveButtonMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveButtonMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveButtonMethod "clicked" o = ButtonClickedMethodInfo
    ResolveButtonMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveButtonMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveButtonMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveButtonMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveButtonMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveButtonMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveButtonMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveButtonMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveButtonMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveButtonMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveButtonMethod "doSetRelatedAction" o = Gtk.Activatable.ActivatableDoSetRelatedActionMethodInfo
    ResolveButtonMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveButtonMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveButtonMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveButtonMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveButtonMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveButtonMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveButtonMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveButtonMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveButtonMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveButtonMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveButtonMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveButtonMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveButtonMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveButtonMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveButtonMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveButtonMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveButtonMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveButtonMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveButtonMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveButtonMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveButtonMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveButtonMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveButtonMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveButtonMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveButtonMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveButtonMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveButtonMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveButtonMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveButtonMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveButtonMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveButtonMethod "enter" o = ButtonEnterMethodInfo
    ResolveButtonMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveButtonMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveButtonMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveButtonMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveButtonMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveButtonMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveButtonMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveButtonMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveButtonMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveButtonMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveButtonMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveButtonMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveButtonMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveButtonMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveButtonMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveButtonMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveButtonMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveButtonMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveButtonMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveButtonMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveButtonMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveButtonMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveButtonMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveButtonMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveButtonMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveButtonMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveButtonMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveButtonMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveButtonMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveButtonMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveButtonMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveButtonMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveButtonMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveButtonMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveButtonMethod "leave" o = ButtonLeaveMethodInfo
    ResolveButtonMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveButtonMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveButtonMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveButtonMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveButtonMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveButtonMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveButtonMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveButtonMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveButtonMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveButtonMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveButtonMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveButtonMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveButtonMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveButtonMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveButtonMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveButtonMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveButtonMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveButtonMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveButtonMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveButtonMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveButtonMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveButtonMethod "pressed" o = ButtonPressedMethodInfo
    ResolveButtonMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveButtonMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveButtonMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveButtonMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveButtonMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveButtonMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveButtonMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveButtonMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveButtonMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveButtonMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveButtonMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveButtonMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveButtonMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveButtonMethod "released" o = ButtonReleasedMethodInfo
    ResolveButtonMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolveButtonMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveButtonMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveButtonMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveButtonMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveButtonMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveButtonMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveButtonMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveButtonMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveButtonMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveButtonMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveButtonMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveButtonMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveButtonMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveButtonMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveButtonMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveButtonMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveButtonMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveButtonMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveButtonMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveButtonMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveButtonMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveButtonMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveButtonMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveButtonMethod "syncActionProperties" o = Gtk.Activatable.ActivatableSyncActionPropertiesMethodInfo
    ResolveButtonMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveButtonMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveButtonMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveButtonMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveButtonMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveButtonMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveButtonMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveButtonMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveButtonMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveButtonMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveButtonMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveButtonMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveButtonMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveButtonMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveButtonMethod "getActionName" o = Gtk.Actionable.ActionableGetActionNameMethodInfo
    ResolveButtonMethod "getActionTargetValue" o = Gtk.Actionable.ActionableGetActionTargetValueMethodInfo
    ResolveButtonMethod "getAlignment" o = ButtonGetAlignmentMethodInfo
    ResolveButtonMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveButtonMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveButtonMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveButtonMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveButtonMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveButtonMethod "getAlwaysShowImage" o = ButtonGetAlwaysShowImageMethodInfo
    ResolveButtonMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveButtonMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveButtonMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveButtonMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveButtonMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveButtonMethod "getChild" o = Gtk.Bin.BinGetChildMethodInfo
    ResolveButtonMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveButtonMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveButtonMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveButtonMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveButtonMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveButtonMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveButtonMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveButtonMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveButtonMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveButtonMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveButtonMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveButtonMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveButtonMethod "getEventWindow" o = ButtonGetEventWindowMethodInfo
    ResolveButtonMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveButtonMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveButtonMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveButtonMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveButtonMethod "getFocusOnClick" o = ButtonGetFocusOnClickMethodInfo
    ResolveButtonMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveButtonMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveButtonMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveButtonMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveButtonMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveButtonMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveButtonMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveButtonMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveButtonMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveButtonMethod "getImage" o = ButtonGetImageMethodInfo
    ResolveButtonMethod "getImagePosition" o = ButtonGetImagePositionMethodInfo
    ResolveButtonMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveButtonMethod "getLabel" o = ButtonGetLabelMethodInfo
    ResolveButtonMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveButtonMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveButtonMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveButtonMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveButtonMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveButtonMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveButtonMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveButtonMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveButtonMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveButtonMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveButtonMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveButtonMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveButtonMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveButtonMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveButtonMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveButtonMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveButtonMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveButtonMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveButtonMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveButtonMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveButtonMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveButtonMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveButtonMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveButtonMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveButtonMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveButtonMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveButtonMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveButtonMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveButtonMethod "getRelatedAction" o = Gtk.Activatable.ActivatableGetRelatedActionMethodInfo
    ResolveButtonMethod "getRelief" o = ButtonGetReliefMethodInfo
    ResolveButtonMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveButtonMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveButtonMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveButtonMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveButtonMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveButtonMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveButtonMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveButtonMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveButtonMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveButtonMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveButtonMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveButtonMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveButtonMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveButtonMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveButtonMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveButtonMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveButtonMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveButtonMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveButtonMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveButtonMethod "getUseActionAppearance" o = Gtk.Activatable.ActivatableGetUseActionAppearanceMethodInfo
    ResolveButtonMethod "getUseStock" o = ButtonGetUseStockMethodInfo
    ResolveButtonMethod "getUseUnderline" o = ButtonGetUseUnderlineMethodInfo
    ResolveButtonMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveButtonMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveButtonMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveButtonMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveButtonMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveButtonMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveButtonMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveButtonMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveButtonMethod "setActionName" o = Gtk.Actionable.ActionableSetActionNameMethodInfo
    ResolveButtonMethod "setActionTargetValue" o = Gtk.Actionable.ActionableSetActionTargetValueMethodInfo
    ResolveButtonMethod "setAlignment" o = ButtonSetAlignmentMethodInfo
    ResolveButtonMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveButtonMethod "setAlwaysShowImage" o = ButtonSetAlwaysShowImageMethodInfo
    ResolveButtonMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveButtonMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveButtonMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveButtonMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveButtonMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveButtonMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveButtonMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveButtonMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveButtonMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveButtonMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveButtonMethod "setDetailedActionName" o = Gtk.Actionable.ActionableSetDetailedActionNameMethodInfo
    ResolveButtonMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveButtonMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveButtonMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveButtonMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveButtonMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveButtonMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveButtonMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveButtonMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveButtonMethod "setFocusOnClick" o = ButtonSetFocusOnClickMethodInfo
    ResolveButtonMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveButtonMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveButtonMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveButtonMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveButtonMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveButtonMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveButtonMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveButtonMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveButtonMethod "setImage" o = ButtonSetImageMethodInfo
    ResolveButtonMethod "setImagePosition" o = ButtonSetImagePositionMethodInfo
    ResolveButtonMethod "setLabel" o = ButtonSetLabelMethodInfo
    ResolveButtonMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveButtonMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveButtonMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveButtonMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveButtonMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveButtonMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveButtonMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveButtonMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveButtonMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveButtonMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveButtonMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveButtonMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveButtonMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveButtonMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveButtonMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveButtonMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveButtonMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveButtonMethod "setRelatedAction" o = Gtk.Activatable.ActivatableSetRelatedActionMethodInfo
    ResolveButtonMethod "setRelief" o = ButtonSetReliefMethodInfo
    ResolveButtonMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveButtonMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveButtonMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveButtonMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveButtonMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveButtonMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveButtonMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveButtonMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveButtonMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveButtonMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveButtonMethod "setUseActionAppearance" o = Gtk.Activatable.ActivatableSetUseActionAppearanceMethodInfo
    ResolveButtonMethod "setUseStock" o = ButtonSetUseStockMethodInfo
    ResolveButtonMethod "setUseUnderline" o = ButtonSetUseUnderlineMethodInfo
    ResolveButtonMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveButtonMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveButtonMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveButtonMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveButtonMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveButtonMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveButtonMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveButtonMethod t Button, O.OverloadedMethod info Button p) => OL.IsLabel t (Button -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveButtonMethod t Button, O.OverloadedMethod info Button p, R.HasField t Button p) => R.HasField t Button p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveButtonMethod t Button, O.OverloadedMethodInfo info Button) => OL.IsLabel t (O.MethodProxy info Button) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal Button::activate
-- | The [activate](#g:signal:activate) signal on GtkButton is an action signal and
-- emitting it causes the button to animate press then release.
-- Applications should never connect to this signal, but use the
-- [Button::clicked]("GI.Gtk.Objects.Button#g:signal:clicked") signal.
type ButtonActivateCallback =
    IO ()

type C_ButtonActivateCallback =
    Ptr Button ->                           -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ButtonActivateCallback`.
foreign import ccall "wrapper"
    mk_ButtonActivateCallback :: C_ButtonActivateCallback -> IO (FunPtr C_ButtonActivateCallback)

wrap_ButtonActivateCallback :: 
    GObject a => (a -> ButtonActivateCallback) ->
    C_ButtonActivateCallback
wrap_ButtonActivateCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [activate](#signal:activate) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' button #activate callback
-- @
-- 
-- 
onButtonActivate :: (IsButton a, MonadIO m) => a -> ((?self :: a) => ButtonActivateCallback) -> m SignalHandlerId
onButtonActivate obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ButtonActivateCallback wrapped
    wrapped'' <- mk_ButtonActivateCallback wrapped'
    connectSignalFunPtr obj "activate" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [activate](#signal:activate) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' button #activate callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterButtonActivate :: (IsButton a, MonadIO m) => a -> ((?self :: a) => ButtonActivateCallback) -> m SignalHandlerId
afterButtonActivate obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ButtonActivateCallback wrapped
    wrapped'' <- mk_ButtonActivateCallback wrapped'
    connectSignalFunPtr obj "activate" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ButtonActivateSignalInfo
instance SignalInfo ButtonActivateSignalInfo where
    type HaskellCallbackType ButtonActivateSignalInfo = ButtonActivateCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ButtonActivateCallback cb
        cb'' <- mk_ButtonActivateCallback cb'
        connectSignalFunPtr obj "activate" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button::activate"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#g:signal:activate"})

#endif

-- signal Button::clicked
-- | Emitted when the button has been activated (pressed and released).
type ButtonClickedCallback =
    IO ()

type C_ButtonClickedCallback =
    Ptr Button ->                           -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ButtonClickedCallback`.
foreign import ccall "wrapper"
    mk_ButtonClickedCallback :: C_ButtonClickedCallback -> IO (FunPtr C_ButtonClickedCallback)

wrap_ButtonClickedCallback :: 
    GObject a => (a -> ButtonClickedCallback) ->
    C_ButtonClickedCallback
wrap_ButtonClickedCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [clicked](#signal:clicked) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' button #clicked callback
-- @
-- 
-- 
onButtonClicked :: (IsButton a, MonadIO m) => a -> ((?self :: a) => ButtonClickedCallback) -> m SignalHandlerId
onButtonClicked obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ButtonClickedCallback wrapped
    wrapped'' <- mk_ButtonClickedCallback wrapped'
    connectSignalFunPtr obj "clicked" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [clicked](#signal:clicked) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' button #clicked callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterButtonClicked :: (IsButton a, MonadIO m) => a -> ((?self :: a) => ButtonClickedCallback) -> m SignalHandlerId
afterButtonClicked obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ButtonClickedCallback wrapped
    wrapped'' <- mk_ButtonClickedCallback wrapped'
    connectSignalFunPtr obj "clicked" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ButtonClickedSignalInfo
instance SignalInfo ButtonClickedSignalInfo where
    type HaskellCallbackType ButtonClickedSignalInfo = ButtonClickedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ButtonClickedCallback cb
        cb'' <- mk_ButtonClickedCallback cb'
        connectSignalFunPtr obj "clicked" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button::clicked"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#g:signal:clicked"})

#endif

-- signal Button::enter
{-# DEPRECATED ButtonEnterCallback ["(Since version 2.8)","Use the [Widget::enterNotifyEvent](\"GI.Gtk.Objects.Widget#g:signal:enterNotifyEvent\") signal."] #-}
-- | Emitted when the pointer enters the button.
type ButtonEnterCallback =
    IO ()

type C_ButtonEnterCallback =
    Ptr Button ->                           -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ButtonEnterCallback`.
foreign import ccall "wrapper"
    mk_ButtonEnterCallback :: C_ButtonEnterCallback -> IO (FunPtr C_ButtonEnterCallback)

wrap_ButtonEnterCallback :: 
    GObject a => (a -> ButtonEnterCallback) ->
    C_ButtonEnterCallback
wrap_ButtonEnterCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [enter](#signal:enter) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' button #enter callback
-- @
-- 
-- 
onButtonEnter :: (IsButton a, MonadIO m) => a -> ((?self :: a) => ButtonEnterCallback) -> m SignalHandlerId
onButtonEnter obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ButtonEnterCallback wrapped
    wrapped'' <- mk_ButtonEnterCallback wrapped'
    connectSignalFunPtr obj "enter" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [enter](#signal:enter) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' button #enter callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterButtonEnter :: (IsButton a, MonadIO m) => a -> ((?self :: a) => ButtonEnterCallback) -> m SignalHandlerId
afterButtonEnter obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ButtonEnterCallback wrapped
    wrapped'' <- mk_ButtonEnterCallback wrapped'
    connectSignalFunPtr obj "enter" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ButtonEnterSignalInfo
instance SignalInfo ButtonEnterSignalInfo where
    type HaskellCallbackType ButtonEnterSignalInfo = ButtonEnterCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ButtonEnterCallback cb
        cb'' <- mk_ButtonEnterCallback cb'
        connectSignalFunPtr obj "enter" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button::enter"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#g:signal:enter"})

#endif

-- signal Button::leave
{-# DEPRECATED ButtonLeaveCallback ["(Since version 2.8)","Use the [Widget::leaveNotifyEvent](\"GI.Gtk.Objects.Widget#g:signal:leaveNotifyEvent\") signal."] #-}
-- | Emitted when the pointer leaves the button.
type ButtonLeaveCallback =
    IO ()

type C_ButtonLeaveCallback =
    Ptr Button ->                           -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ButtonLeaveCallback`.
foreign import ccall "wrapper"
    mk_ButtonLeaveCallback :: C_ButtonLeaveCallback -> IO (FunPtr C_ButtonLeaveCallback)

wrap_ButtonLeaveCallback :: 
    GObject a => (a -> ButtonLeaveCallback) ->
    C_ButtonLeaveCallback
wrap_ButtonLeaveCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [leave](#signal:leave) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' button #leave callback
-- @
-- 
-- 
onButtonLeave :: (IsButton a, MonadIO m) => a -> ((?self :: a) => ButtonLeaveCallback) -> m SignalHandlerId
onButtonLeave obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ButtonLeaveCallback wrapped
    wrapped'' <- mk_ButtonLeaveCallback wrapped'
    connectSignalFunPtr obj "leave" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [leave](#signal:leave) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' button #leave callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterButtonLeave :: (IsButton a, MonadIO m) => a -> ((?self :: a) => ButtonLeaveCallback) -> m SignalHandlerId
afterButtonLeave obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ButtonLeaveCallback wrapped
    wrapped'' <- mk_ButtonLeaveCallback wrapped'
    connectSignalFunPtr obj "leave" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ButtonLeaveSignalInfo
instance SignalInfo ButtonLeaveSignalInfo where
    type HaskellCallbackType ButtonLeaveSignalInfo = ButtonLeaveCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ButtonLeaveCallback cb
        cb'' <- mk_ButtonLeaveCallback cb'
        connectSignalFunPtr obj "leave" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button::leave"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#g:signal:leave"})

#endif

-- signal Button::pressed
{-# DEPRECATED ButtonPressedCallback ["(Since version 2.8)","Use the [Widget::buttonPressEvent](\"GI.Gtk.Objects.Widget#g:signal:buttonPressEvent\") signal."] #-}
-- | Emitted when the button is pressed.
type ButtonPressedCallback =
    IO ()

type C_ButtonPressedCallback =
    Ptr Button ->                           -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ButtonPressedCallback`.
foreign import ccall "wrapper"
    mk_ButtonPressedCallback :: C_ButtonPressedCallback -> IO (FunPtr C_ButtonPressedCallback)

wrap_ButtonPressedCallback :: 
    GObject a => (a -> ButtonPressedCallback) ->
    C_ButtonPressedCallback
wrap_ButtonPressedCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [pressed](#signal:pressed) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' button #pressed callback
-- @
-- 
-- 
onButtonPressed :: (IsButton a, MonadIO m) => a -> ((?self :: a) => ButtonPressedCallback) -> m SignalHandlerId
onButtonPressed obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ButtonPressedCallback wrapped
    wrapped'' <- mk_ButtonPressedCallback wrapped'
    connectSignalFunPtr obj "pressed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [pressed](#signal:pressed) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' button #pressed callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterButtonPressed :: (IsButton a, MonadIO m) => a -> ((?self :: a) => ButtonPressedCallback) -> m SignalHandlerId
afterButtonPressed obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ButtonPressedCallback wrapped
    wrapped'' <- mk_ButtonPressedCallback wrapped'
    connectSignalFunPtr obj "pressed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ButtonPressedSignalInfo
instance SignalInfo ButtonPressedSignalInfo where
    type HaskellCallbackType ButtonPressedSignalInfo = ButtonPressedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ButtonPressedCallback cb
        cb'' <- mk_ButtonPressedCallback cb'
        connectSignalFunPtr obj "pressed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button::pressed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#g:signal:pressed"})

#endif

-- signal Button::released
{-# DEPRECATED ButtonReleasedCallback ["(Since version 2.8)","Use the [Widget::buttonReleaseEvent](\"GI.Gtk.Objects.Widget#g:signal:buttonReleaseEvent\") signal."] #-}
-- | Emitted when the button is released.
type ButtonReleasedCallback =
    IO ()

type C_ButtonReleasedCallback =
    Ptr Button ->                           -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ButtonReleasedCallback`.
foreign import ccall "wrapper"
    mk_ButtonReleasedCallback :: C_ButtonReleasedCallback -> IO (FunPtr C_ButtonReleasedCallback)

wrap_ButtonReleasedCallback :: 
    GObject a => (a -> ButtonReleasedCallback) ->
    C_ButtonReleasedCallback
wrap_ButtonReleasedCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [released](#signal:released) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' button #released callback
-- @
-- 
-- 
onButtonReleased :: (IsButton a, MonadIO m) => a -> ((?self :: a) => ButtonReleasedCallback) -> m SignalHandlerId
onButtonReleased obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ButtonReleasedCallback wrapped
    wrapped'' <- mk_ButtonReleasedCallback wrapped'
    connectSignalFunPtr obj "released" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [released](#signal:released) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' button #released callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterButtonReleased :: (IsButton a, MonadIO m) => a -> ((?self :: a) => ButtonReleasedCallback) -> m SignalHandlerId
afterButtonReleased obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ButtonReleasedCallback wrapped
    wrapped'' <- mk_ButtonReleasedCallback wrapped'
    connectSignalFunPtr obj "released" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ButtonReleasedSignalInfo
instance SignalInfo ButtonReleasedSignalInfo where
    type HaskellCallbackType ButtonReleasedSignalInfo = ButtonReleasedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ButtonReleasedCallback cb
        cb'' <- mk_ButtonReleasedCallback cb'
        connectSignalFunPtr obj "released" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button::released"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#g:signal:released"})

#endif

-- VVV Prop "always-show-image"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstruct]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@always-show-image@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' button #alwaysShowImage
-- @
getButtonAlwaysShowImage :: (MonadIO m, IsButton o) => o -> m Bool
getButtonAlwaysShowImage obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "always-show-image"

-- | Set the value of the “@always-show-image@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' button [ #alwaysShowImage 'Data.GI.Base.Attributes.:=' value ]
-- @
setButtonAlwaysShowImage :: (MonadIO m, IsButton o) => o -> Bool -> m ()
setButtonAlwaysShowImage obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "always-show-image" val

-- | Construct a `GValueConstruct` with valid value for the “@always-show-image@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructButtonAlwaysShowImage :: (IsButton o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructButtonAlwaysShowImage val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "always-show-image" val

#if defined(ENABLE_OVERLOADING)
data ButtonAlwaysShowImagePropertyInfo
instance AttrInfo ButtonAlwaysShowImagePropertyInfo where
    type AttrAllowedOps ButtonAlwaysShowImagePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ButtonAlwaysShowImagePropertyInfo = IsButton
    type AttrSetTypeConstraint ButtonAlwaysShowImagePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint ButtonAlwaysShowImagePropertyInfo = (~) Bool
    type AttrTransferType ButtonAlwaysShowImagePropertyInfo = Bool
    type AttrGetType ButtonAlwaysShowImagePropertyInfo = Bool
    type AttrLabel ButtonAlwaysShowImagePropertyInfo = "always-show-image"
    type AttrOrigin ButtonAlwaysShowImagePropertyInfo = Button
    attrGet = getButtonAlwaysShowImage
    attrSet = setButtonAlwaysShowImage
    attrTransfer _ v = do
        return v
    attrConstruct = constructButtonAlwaysShowImage
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button.alwaysShowImage"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#g:attr:alwaysShowImage"
        })
#endif

-- VVV Prop "image"
   -- Type: TInterface (Name {namespace = "Gtk", name = "Widget"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just True)

-- | Get the value of the “@image@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' button #image
-- @
getButtonImage :: (MonadIO m, IsButton o) => o -> m (Maybe Gtk.Widget.Widget)
getButtonImage obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "image" Gtk.Widget.Widget

-- | Set the value of the “@image@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' button [ #image 'Data.GI.Base.Attributes.:=' value ]
-- @
setButtonImage :: (MonadIO m, IsButton o, Gtk.Widget.IsWidget a) => o -> a -> m ()
setButtonImage obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "image" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@image@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructButtonImage :: (IsButton o, MIO.MonadIO m, Gtk.Widget.IsWidget a) => a -> m (GValueConstruct o)
constructButtonImage val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "image" (P.Just val)

-- | Set the value of the “@image@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #image
-- @
clearButtonImage :: (MonadIO m, IsButton o) => o -> m ()
clearButtonImage obj = liftIO $ B.Properties.setObjectPropertyObject obj "image" (Nothing :: Maybe Gtk.Widget.Widget)

#if defined(ENABLE_OVERLOADING)
data ButtonImagePropertyInfo
instance AttrInfo ButtonImagePropertyInfo where
    type AttrAllowedOps ButtonImagePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint ButtonImagePropertyInfo = IsButton
    type AttrSetTypeConstraint ButtonImagePropertyInfo = Gtk.Widget.IsWidget
    type AttrTransferTypeConstraint ButtonImagePropertyInfo = Gtk.Widget.IsWidget
    type AttrTransferType ButtonImagePropertyInfo = Gtk.Widget.Widget
    type AttrGetType ButtonImagePropertyInfo = (Maybe Gtk.Widget.Widget)
    type AttrLabel ButtonImagePropertyInfo = "image"
    type AttrOrigin ButtonImagePropertyInfo = Button
    attrGet = getButtonImage
    attrSet = setButtonImage
    attrTransfer _ v = do
        unsafeCastTo Gtk.Widget.Widget v
    attrConstruct = constructButtonImage
    attrClear = clearButtonImage
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button.image"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#g:attr:image"
        })
#endif

-- VVV Prop "image-position"
   -- Type: TInterface (Name {namespace = "Gtk", name = "PositionType"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@image-position@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' button #imagePosition
-- @
getButtonImagePosition :: (MonadIO m, IsButton o) => o -> m Gtk.Enums.PositionType
getButtonImagePosition obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "image-position"

-- | Set the value of the “@image-position@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' button [ #imagePosition 'Data.GI.Base.Attributes.:=' value ]
-- @
setButtonImagePosition :: (MonadIO m, IsButton o) => o -> Gtk.Enums.PositionType -> m ()
setButtonImagePosition obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "image-position" val

-- | Construct a `GValueConstruct` with valid value for the “@image-position@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructButtonImagePosition :: (IsButton o, MIO.MonadIO m) => Gtk.Enums.PositionType -> m (GValueConstruct o)
constructButtonImagePosition val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "image-position" val

#if defined(ENABLE_OVERLOADING)
data ButtonImagePositionPropertyInfo
instance AttrInfo ButtonImagePositionPropertyInfo where
    type AttrAllowedOps ButtonImagePositionPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ButtonImagePositionPropertyInfo = IsButton
    type AttrSetTypeConstraint ButtonImagePositionPropertyInfo = (~) Gtk.Enums.PositionType
    type AttrTransferTypeConstraint ButtonImagePositionPropertyInfo = (~) Gtk.Enums.PositionType
    type AttrTransferType ButtonImagePositionPropertyInfo = Gtk.Enums.PositionType
    type AttrGetType ButtonImagePositionPropertyInfo = Gtk.Enums.PositionType
    type AttrLabel ButtonImagePositionPropertyInfo = "image-position"
    type AttrOrigin ButtonImagePositionPropertyInfo = Button
    attrGet = getButtonImagePosition
    attrSet = setButtonImagePosition
    attrTransfer _ v = do
        return v
    attrConstruct = constructButtonImagePosition
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button.imagePosition"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#g:attr:imagePosition"
        })
#endif

-- VVV Prop "label"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstruct]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@label@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' button #label
-- @
getButtonLabel :: (MonadIO m, IsButton o) => o -> m T.Text
getButtonLabel obj = MIO.liftIO $ checkUnexpectedNothing "getButtonLabel" $ B.Properties.getObjectPropertyString obj "label"

-- | Set the value of the “@label@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' button [ #label 'Data.GI.Base.Attributes.:=' value ]
-- @
setButtonLabel :: (MonadIO m, IsButton o) => o -> T.Text -> m ()
setButtonLabel obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "label" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@label@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructButtonLabel :: (IsButton o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructButtonLabel val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "label" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data ButtonLabelPropertyInfo
instance AttrInfo ButtonLabelPropertyInfo where
    type AttrAllowedOps ButtonLabelPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ButtonLabelPropertyInfo = IsButton
    type AttrSetTypeConstraint ButtonLabelPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint ButtonLabelPropertyInfo = (~) T.Text
    type AttrTransferType ButtonLabelPropertyInfo = T.Text
    type AttrGetType ButtonLabelPropertyInfo = T.Text
    type AttrLabel ButtonLabelPropertyInfo = "label"
    type AttrOrigin ButtonLabelPropertyInfo = Button
    attrGet = getButtonLabel
    attrSet = setButtonLabel
    attrTransfer _ v = do
        return v
    attrConstruct = constructButtonLabel
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button.label"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#g:attr:label"
        })
#endif

-- VVV Prop "relief"
   -- Type: TInterface (Name {namespace = "Gtk", name = "ReliefStyle"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@relief@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' button #relief
-- @
getButtonRelief :: (MonadIO m, IsButton o) => o -> m Gtk.Enums.ReliefStyle
getButtonRelief obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "relief"

-- | Set the value of the “@relief@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' button [ #relief 'Data.GI.Base.Attributes.:=' value ]
-- @
setButtonRelief :: (MonadIO m, IsButton o) => o -> Gtk.Enums.ReliefStyle -> m ()
setButtonRelief obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "relief" val

-- | Construct a `GValueConstruct` with valid value for the “@relief@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructButtonRelief :: (IsButton o, MIO.MonadIO m) => Gtk.Enums.ReliefStyle -> m (GValueConstruct o)
constructButtonRelief val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "relief" val

#if defined(ENABLE_OVERLOADING)
data ButtonReliefPropertyInfo
instance AttrInfo ButtonReliefPropertyInfo where
    type AttrAllowedOps ButtonReliefPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ButtonReliefPropertyInfo = IsButton
    type AttrSetTypeConstraint ButtonReliefPropertyInfo = (~) Gtk.Enums.ReliefStyle
    type AttrTransferTypeConstraint ButtonReliefPropertyInfo = (~) Gtk.Enums.ReliefStyle
    type AttrTransferType ButtonReliefPropertyInfo = Gtk.Enums.ReliefStyle
    type AttrGetType ButtonReliefPropertyInfo = Gtk.Enums.ReliefStyle
    type AttrLabel ButtonReliefPropertyInfo = "relief"
    type AttrOrigin ButtonReliefPropertyInfo = Button
    attrGet = getButtonRelief
    attrSet = setButtonRelief
    attrTransfer _ v = do
        return v
    attrConstruct = constructButtonRelief
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button.relief"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#g:attr:relief"
        })
#endif

-- VVV Prop "use-stock"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstruct]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@use-stock@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' button #useStock
-- @
getButtonUseStock :: (MonadIO m, IsButton o) => o -> m Bool
getButtonUseStock obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "use-stock"

-- | Set the value of the “@use-stock@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' button [ #useStock 'Data.GI.Base.Attributes.:=' value ]
-- @
setButtonUseStock :: (MonadIO m, IsButton o) => o -> Bool -> m ()
setButtonUseStock obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "use-stock" val

-- | Construct a `GValueConstruct` with valid value for the “@use-stock@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructButtonUseStock :: (IsButton o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructButtonUseStock val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "use-stock" val

#if defined(ENABLE_OVERLOADING)
data ButtonUseStockPropertyInfo
instance AttrInfo ButtonUseStockPropertyInfo where
    type AttrAllowedOps ButtonUseStockPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ButtonUseStockPropertyInfo = IsButton
    type AttrSetTypeConstraint ButtonUseStockPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint ButtonUseStockPropertyInfo = (~) Bool
    type AttrTransferType ButtonUseStockPropertyInfo = Bool
    type AttrGetType ButtonUseStockPropertyInfo = Bool
    type AttrLabel ButtonUseStockPropertyInfo = "use-stock"
    type AttrOrigin ButtonUseStockPropertyInfo = Button
    attrGet = getButtonUseStock
    attrSet = setButtonUseStock
    attrTransfer _ v = do
        return v
    attrConstruct = constructButtonUseStock
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button.useStock"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#g:attr:useStock"
        })
#endif

-- VVV Prop "use-underline"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstruct]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@use-underline@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' button #useUnderline
-- @
getButtonUseUnderline :: (MonadIO m, IsButton o) => o -> m Bool
getButtonUseUnderline obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "use-underline"

-- | Set the value of the “@use-underline@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' button [ #useUnderline 'Data.GI.Base.Attributes.:=' value ]
-- @
setButtonUseUnderline :: (MonadIO m, IsButton o) => o -> Bool -> m ()
setButtonUseUnderline obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "use-underline" val

-- | Construct a `GValueConstruct` with valid value for the “@use-underline@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructButtonUseUnderline :: (IsButton o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructButtonUseUnderline val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "use-underline" val

#if defined(ENABLE_OVERLOADING)
data ButtonUseUnderlinePropertyInfo
instance AttrInfo ButtonUseUnderlinePropertyInfo where
    type AttrAllowedOps ButtonUseUnderlinePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ButtonUseUnderlinePropertyInfo = IsButton
    type AttrSetTypeConstraint ButtonUseUnderlinePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint ButtonUseUnderlinePropertyInfo = (~) Bool
    type AttrTransferType ButtonUseUnderlinePropertyInfo = Bool
    type AttrGetType ButtonUseUnderlinePropertyInfo = Bool
    type AttrLabel ButtonUseUnderlinePropertyInfo = "use-underline"
    type AttrOrigin ButtonUseUnderlinePropertyInfo = Button
    attrGet = getButtonUseUnderline
    attrSet = setButtonUseUnderline
    attrTransfer _ v = do
        return v
    attrConstruct = constructButtonUseUnderline
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button.useUnderline"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#g:attr:useUnderline"
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
-- 'Data.GI.Base.Attributes.get' button #xalign
-- @
getButtonXalign :: (MonadIO m, IsButton o) => o -> m Float
getButtonXalign obj = MIO.liftIO $ B.Properties.getObjectPropertyFloat obj "xalign"

-- | Set the value of the “@xalign@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' button [ #xalign 'Data.GI.Base.Attributes.:=' value ]
-- @
setButtonXalign :: (MonadIO m, IsButton o) => o -> Float -> m ()
setButtonXalign obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyFloat obj "xalign" val

-- | Construct a `GValueConstruct` with valid value for the “@xalign@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructButtonXalign :: (IsButton o, MIO.MonadIO m) => Float -> m (GValueConstruct o)
constructButtonXalign val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyFloat "xalign" val

#if defined(ENABLE_OVERLOADING)
data ButtonXalignPropertyInfo
instance AttrInfo ButtonXalignPropertyInfo where
    type AttrAllowedOps ButtonXalignPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ButtonXalignPropertyInfo = IsButton
    type AttrSetTypeConstraint ButtonXalignPropertyInfo = (~) Float
    type AttrTransferTypeConstraint ButtonXalignPropertyInfo = (~) Float
    type AttrTransferType ButtonXalignPropertyInfo = Float
    type AttrGetType ButtonXalignPropertyInfo = Float
    type AttrLabel ButtonXalignPropertyInfo = "xalign"
    type AttrOrigin ButtonXalignPropertyInfo = Button
    attrGet = getButtonXalign
    attrSet = setButtonXalign
    attrTransfer _ v = do
        return v
    attrConstruct = constructButtonXalign
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button.xalign"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#g:attr:xalign"
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
-- 'Data.GI.Base.Attributes.get' button #yalign
-- @
getButtonYalign :: (MonadIO m, IsButton o) => o -> m Float
getButtonYalign obj = MIO.liftIO $ B.Properties.getObjectPropertyFloat obj "yalign"

-- | Set the value of the “@yalign@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' button [ #yalign 'Data.GI.Base.Attributes.:=' value ]
-- @
setButtonYalign :: (MonadIO m, IsButton o) => o -> Float -> m ()
setButtonYalign obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyFloat obj "yalign" val

-- | Construct a `GValueConstruct` with valid value for the “@yalign@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructButtonYalign :: (IsButton o, MIO.MonadIO m) => Float -> m (GValueConstruct o)
constructButtonYalign val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyFloat "yalign" val

#if defined(ENABLE_OVERLOADING)
data ButtonYalignPropertyInfo
instance AttrInfo ButtonYalignPropertyInfo where
    type AttrAllowedOps ButtonYalignPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ButtonYalignPropertyInfo = IsButton
    type AttrSetTypeConstraint ButtonYalignPropertyInfo = (~) Float
    type AttrTransferTypeConstraint ButtonYalignPropertyInfo = (~) Float
    type AttrTransferType ButtonYalignPropertyInfo = Float
    type AttrGetType ButtonYalignPropertyInfo = Float
    type AttrLabel ButtonYalignPropertyInfo = "yalign"
    type AttrOrigin ButtonYalignPropertyInfo = Button
    attrGet = getButtonYalign
    attrSet = setButtonYalign
    attrTransfer _ v = do
        return v
    attrConstruct = constructButtonYalign
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button.yalign"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#g:attr:yalign"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList Button
type instance O.AttributeList Button = ButtonAttributeList
type ButtonAttributeList = ('[ '("actionName", Gtk.Actionable.ActionableActionNamePropertyInfo), '("actionTarget", Gtk.Actionable.ActionableActionTargetPropertyInfo), '("alwaysShowImage", ButtonAlwaysShowImagePropertyInfo), '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("image", ButtonImagePropertyInfo), '("imagePosition", ButtonImagePositionPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("label", ButtonLabelPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("relatedAction", Gtk.Activatable.ActivatableRelatedActionPropertyInfo), '("relief", ButtonReliefPropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("useActionAppearance", Gtk.Activatable.ActivatableUseActionAppearancePropertyInfo), '("useStock", ButtonUseStockPropertyInfo), '("useUnderline", ButtonUseUnderlinePropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo), '("xalign", ButtonXalignPropertyInfo), '("yalign", ButtonYalignPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
buttonAlwaysShowImage :: AttrLabelProxy "alwaysShowImage"
buttonAlwaysShowImage = AttrLabelProxy

buttonImage :: AttrLabelProxy "image"
buttonImage = AttrLabelProxy

buttonImagePosition :: AttrLabelProxy "imagePosition"
buttonImagePosition = AttrLabelProxy

buttonLabel :: AttrLabelProxy "label"
buttonLabel = AttrLabelProxy

buttonRelief :: AttrLabelProxy "relief"
buttonRelief = AttrLabelProxy

buttonUseStock :: AttrLabelProxy "useStock"
buttonUseStock = AttrLabelProxy

buttonUseUnderline :: AttrLabelProxy "useUnderline"
buttonUseUnderline = AttrLabelProxy

buttonXalign :: AttrLabelProxy "xalign"
buttonXalign = AttrLabelProxy

buttonYalign :: AttrLabelProxy "yalign"
buttonYalign = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList Button = ButtonSignalList
type ButtonSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("activate", ButtonActivateSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("clicked", ButtonClickedSignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enter", ButtonEnterSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leave", ButtonLeaveSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("pressed", ButtonPressedSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("released", ButtonReleasedSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method Button::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Button" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_button_new" gtk_button_new :: 
    IO (Ptr Button)

-- | Creates a new t'GI.Gtk.Objects.Button.Button' widget. To add a child widget to the button,
-- use 'GI.Gtk.Objects.Container.containerAdd'.
buttonNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m Button
    -- ^ __Returns:__ The newly created t'GI.Gtk.Objects.Button.Button' widget.
buttonNew  = liftIO $ do
    result <- gtk_button_new
    checkUnexpectedReturnNULL "buttonNew" result
    result' <- (newObject Button) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Button::new_from_icon_name
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "icon_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an icon name or %NULL"
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
--                 { rawDocText = Just "an icon size (#GtkIconSize)"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Button" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_button_new_from_icon_name" gtk_button_new_from_icon_name :: 
    CString ->                              -- icon_name : TBasicType TUTF8
    Int32 ->                                -- size : TBasicType TInt
    IO (Ptr Button)

-- | Creates a new button containing an icon from the current icon theme.
-- 
-- If the icon name isn’t known, a “broken image” icon will be
-- displayed instead. If the current icon theme is changed, the icon
-- will be updated appropriately.
-- 
-- This function is a convenience wrapper around 'GI.Gtk.Objects.Button.buttonNew' and
-- 'GI.Gtk.Objects.Button.buttonSetImage'.
-- 
-- /Since: 3.10/
buttonNewFromIconName ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Maybe (T.Text)
    -- ^ /@iconName@/: an icon name or 'P.Nothing'
    -> Int32
    -- ^ /@size@/: an icon size (t'GI.Gtk.Enums.IconSize')
    -> m Button
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.Button.Button' displaying the themed icon
buttonNewFromIconName iconName size = liftIO $ do
    maybeIconName <- case iconName of
        Nothing -> return nullPtr
        Just jIconName -> do
            jIconName' <- textToCString jIconName
            return jIconName'
    result <- gtk_button_new_from_icon_name maybeIconName size
    checkUnexpectedReturnNULL "buttonNewFromIconName" result
    result' <- (newObject Button) result
    freeMem maybeIconName
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Button::new_from_stock
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "stock_id"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the name of the stock item"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Button" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_button_new_from_stock" gtk_button_new_from_stock :: 
    CString ->                              -- stock_id : TBasicType TUTF8
    IO (Ptr Button)

{-# DEPRECATED buttonNewFromStock ["(Since version 3.10)","Stock items are deprecated. Use 'GI.Gtk.Objects.Button.buttonNewWithLabel'","instead."] #-}
-- | Creates a new t'GI.Gtk.Objects.Button.Button' containing the image and text from a
-- [stock item][gtkstock].
-- Some stock ids have preprocessor macros like 'GI.Gtk.Constants.STOCK_OK' and
-- 'GI.Gtk.Constants.STOCK_APPLY'.
-- 
-- If /@stockId@/ is unknown, then it will be treated as a mnemonic
-- label (as for 'GI.Gtk.Objects.Button.buttonNewWithMnemonic').
buttonNewFromStock ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@stockId@/: the name of the stock item
    -> m Button
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.Button.Button'
buttonNewFromStock stockId = liftIO $ do
    stockId' <- textToCString stockId
    result <- gtk_button_new_from_stock stockId'
    checkUnexpectedReturnNULL "buttonNewFromStock" result
    result' <- (newObject Button) result
    freeMem stockId'
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Button::new_with_label
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The text you want the #GtkLabel to hold."
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Button" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_button_new_with_label" gtk_button_new_with_label :: 
    CString ->                              -- label : TBasicType TUTF8
    IO (Ptr Button)

-- | Creates a t'GI.Gtk.Objects.Button.Button' widget with a t'GI.Gtk.Objects.Label.Label' child containing the given
-- text.
buttonNewWithLabel ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@label@/: The text you want the t'GI.Gtk.Objects.Label.Label' to hold.
    -> m Button
    -- ^ __Returns:__ The newly created t'GI.Gtk.Objects.Button.Button' widget.
buttonNewWithLabel label = liftIO $ do
    label' <- textToCString label
    result <- gtk_button_new_with_label label'
    checkUnexpectedReturnNULL "buttonNewWithLabel" result
    result' <- (newObject Button) result
    freeMem label'
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Button::new_with_mnemonic
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "The text of the button, with an underscore in front of the\n        mnemonic character"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Button" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_button_new_with_mnemonic" gtk_button_new_with_mnemonic :: 
    CString ->                              -- label : TBasicType TUTF8
    IO (Ptr Button)

-- | Creates a new t'GI.Gtk.Objects.Button.Button' containing a label.
-- If characters in /@label@/ are preceded by an underscore, they are underlined.
-- If you need a literal underscore character in a label, use “__” (two
-- underscores). The first underlined character represents a keyboard
-- accelerator called a mnemonic.
-- Pressing Alt and that key activates the button.
buttonNewWithMnemonic ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@label@/: The text of the button, with an underscore in front of the
    --         mnemonic character
    -> m Button
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.Button.Button'
buttonNewWithMnemonic label = liftIO $ do
    label' <- textToCString label
    result <- gtk_button_new_with_mnemonic label'
    checkUnexpectedReturnNULL "buttonNewWithMnemonic" result
    result' <- (newObject Button) result
    freeMem label'
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Button::clicked
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "button"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Button" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The #GtkButton you want to send the signal to."
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

foreign import ccall "gtk_button_clicked" gtk_button_clicked :: 
    Ptr Button ->                           -- button : TInterface (Name {namespace = "Gtk", name = "Button"})
    IO ()

-- | Emits a [Button::clicked]("GI.Gtk.Objects.Button#g:signal:clicked") signal to the given t'GI.Gtk.Objects.Button.Button'.
buttonClicked ::
    (B.CallStack.HasCallStack, MonadIO m, IsButton a) =>
    a
    -- ^ /@button@/: The t'GI.Gtk.Objects.Button.Button' you want to send the signal to.
    -> m ()
buttonClicked button = liftIO $ do
    button' <- unsafeManagedPtrCastPtr button
    gtk_button_clicked button'
    touchManagedPtr button
    return ()

#if defined(ENABLE_OVERLOADING)
data ButtonClickedMethodInfo
instance (signature ~ (m ()), MonadIO m, IsButton a) => O.OverloadedMethod ButtonClickedMethodInfo a signature where
    overloadedMethod = buttonClicked

instance O.OverloadedMethodInfo ButtonClickedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button.buttonClicked",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#v:buttonClicked"
        })


#endif

-- method Button::enter
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "button"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Button" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The #GtkButton you want to send the signal to."
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

foreign import ccall "gtk_button_enter" gtk_button_enter :: 
    Ptr Button ->                           -- button : TInterface (Name {namespace = "Gtk", name = "Button"})
    IO ()

{-# DEPRECATED buttonEnter ["(Since version 2.20)","Use the [Widget::enterNotifyEvent](\"GI.Gtk.Objects.Widget#g:signal:enterNotifyEvent\") signal."] #-}
-- | Emits a [Button::enter]("GI.Gtk.Objects.Button#g:signal:enter") signal to the given t'GI.Gtk.Objects.Button.Button'.
buttonEnter ::
    (B.CallStack.HasCallStack, MonadIO m, IsButton a) =>
    a
    -- ^ /@button@/: The t'GI.Gtk.Objects.Button.Button' you want to send the signal to.
    -> m ()
buttonEnter button = liftIO $ do
    button' <- unsafeManagedPtrCastPtr button
    gtk_button_enter button'
    touchManagedPtr button
    return ()

#if defined(ENABLE_OVERLOADING)
data ButtonEnterMethodInfo
instance (signature ~ (m ()), MonadIO m, IsButton a) => O.OverloadedMethod ButtonEnterMethodInfo a signature where
    overloadedMethod = buttonEnter

instance O.OverloadedMethodInfo ButtonEnterMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button.buttonEnter",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#v:buttonEnter"
        })


#endif

-- method Button::get_alignment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "button"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Button" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "xalign"
--           , argType = TBasicType TFloat
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return location for horizontal alignment"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "yalign"
--           , argType = TBasicType TFloat
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return location for vertical alignment"
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

foreign import ccall "gtk_button_get_alignment" gtk_button_get_alignment :: 
    Ptr Button ->                           -- button : TInterface (Name {namespace = "Gtk", name = "Button"})
    Ptr CFloat ->                           -- xalign : TBasicType TFloat
    Ptr CFloat ->                           -- yalign : TBasicType TFloat
    IO ()

{-# DEPRECATED buttonGetAlignment ["(Since version 3.14)","Access the child widget directly if you need to control","its alignment."] #-}
-- | Gets the alignment of the child in the button.
-- 
-- /Since: 2.4/
buttonGetAlignment ::
    (B.CallStack.HasCallStack, MonadIO m, IsButton a) =>
    a
    -- ^ /@button@/: a t'GI.Gtk.Objects.Button.Button'
    -> m ((Float, Float))
buttonGetAlignment button = liftIO $ do
    button' <- unsafeManagedPtrCastPtr button
    xalign <- allocMem :: IO (Ptr CFloat)
    yalign <- allocMem :: IO (Ptr CFloat)
    gtk_button_get_alignment button' xalign yalign
    xalign' <- peek xalign
    let xalign'' = realToFrac xalign'
    yalign' <- peek yalign
    let yalign'' = realToFrac yalign'
    touchManagedPtr button
    freeMem xalign
    freeMem yalign
    return (xalign'', yalign'')

#if defined(ENABLE_OVERLOADING)
data ButtonGetAlignmentMethodInfo
instance (signature ~ (m ((Float, Float))), MonadIO m, IsButton a) => O.OverloadedMethod ButtonGetAlignmentMethodInfo a signature where
    overloadedMethod = buttonGetAlignment

instance O.OverloadedMethodInfo ButtonGetAlignmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button.buttonGetAlignment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#v:buttonGetAlignment"
        })


#endif

-- method Button::get_always_show_image
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "button"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Button" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkButton" , sinceVersion = Nothing }
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

foreign import ccall "gtk_button_get_always_show_image" gtk_button_get_always_show_image :: 
    Ptr Button ->                           -- button : TInterface (Name {namespace = "Gtk", name = "Button"})
    IO CInt

-- | Returns whether the button will ignore the [Settings:gtkButtonImages]("GI.Gtk.Objects.Settings#g:attr:gtkButtonImages")
-- setting and always show the image, if available.
-- 
-- /Since: 3.6/
buttonGetAlwaysShowImage ::
    (B.CallStack.HasCallStack, MonadIO m, IsButton a) =>
    a
    -- ^ /@button@/: a t'GI.Gtk.Objects.Button.Button'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the button will always show the image
buttonGetAlwaysShowImage button = liftIO $ do
    button' <- unsafeManagedPtrCastPtr button
    result <- gtk_button_get_always_show_image button'
    let result' = (/= 0) result
    touchManagedPtr button
    return result'

#if defined(ENABLE_OVERLOADING)
data ButtonGetAlwaysShowImageMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsButton a) => O.OverloadedMethod ButtonGetAlwaysShowImageMethodInfo a signature where
    overloadedMethod = buttonGetAlwaysShowImage

instance O.OverloadedMethodInfo ButtonGetAlwaysShowImageMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button.buttonGetAlwaysShowImage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#v:buttonGetAlwaysShowImage"
        })


#endif

-- method Button::get_event_window
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "button"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Button" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gdk" , name = "Window" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_button_get_event_window" gtk_button_get_event_window :: 
    Ptr Button ->                           -- button : TInterface (Name {namespace = "Gtk", name = "Button"})
    IO (Ptr Gdk.Window.Window)

-- | Returns the button’s event window if it is realized, 'P.Nothing' otherwise.
-- This function should be rarely needed.
-- 
-- /Since: 2.22/
buttonGetEventWindow ::
    (B.CallStack.HasCallStack, MonadIO m, IsButton a) =>
    a
    -- ^ /@button@/: a t'GI.Gtk.Objects.Button.Button'
    -> m Gdk.Window.Window
    -- ^ __Returns:__ /@button@/’s event window.
buttonGetEventWindow button = liftIO $ do
    button' <- unsafeManagedPtrCastPtr button
    result <- gtk_button_get_event_window button'
    checkUnexpectedReturnNULL "buttonGetEventWindow" result
    result' <- (newObject Gdk.Window.Window) result
    touchManagedPtr button
    return result'

#if defined(ENABLE_OVERLOADING)
data ButtonGetEventWindowMethodInfo
instance (signature ~ (m Gdk.Window.Window), MonadIO m, IsButton a) => O.OverloadedMethod ButtonGetEventWindowMethodInfo a signature where
    overloadedMethod = buttonGetEventWindow

instance O.OverloadedMethodInfo ButtonGetEventWindowMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button.buttonGetEventWindow",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#v:buttonGetEventWindow"
        })


#endif

-- method Button::get_focus_on_click
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "button"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Button" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkButton" , sinceVersion = Nothing }
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

foreign import ccall "gtk_button_get_focus_on_click" gtk_button_get_focus_on_click :: 
    Ptr Button ->                           -- button : TInterface (Name {namespace = "Gtk", name = "Button"})
    IO CInt

{-# DEPRECATED buttonGetFocusOnClick ["(Since version 3.20)","Use 'GI.Gtk.Objects.Widget.widgetGetFocusOnClick' instead"] #-}
-- | Returns whether the button grabs focus when it is clicked with the mouse.
-- See 'GI.Gtk.Objects.Button.buttonSetFocusOnClick'.
-- 
-- /Since: 2.4/
buttonGetFocusOnClick ::
    (B.CallStack.HasCallStack, MonadIO m, IsButton a) =>
    a
    -- ^ /@button@/: a t'GI.Gtk.Objects.Button.Button'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the button grabs focus when it is clicked with
    --               the mouse.
buttonGetFocusOnClick button = liftIO $ do
    button' <- unsafeManagedPtrCastPtr button
    result <- gtk_button_get_focus_on_click button'
    let result' = (/= 0) result
    touchManagedPtr button
    return result'

#if defined(ENABLE_OVERLOADING)
data ButtonGetFocusOnClickMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsButton a) => O.OverloadedMethod ButtonGetFocusOnClickMethodInfo a signature where
    overloadedMethod = buttonGetFocusOnClick

instance O.OverloadedMethodInfo ButtonGetFocusOnClickMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button.buttonGetFocusOnClick",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#v:buttonGetFocusOnClick"
        })


#endif

-- method Button::get_image
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "button"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Button" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkButton" , sinceVersion = Nothing }
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

foreign import ccall "gtk_button_get_image" gtk_button_get_image :: 
    Ptr Button ->                           -- button : TInterface (Name {namespace = "Gtk", name = "Button"})
    IO (Ptr Gtk.Widget.Widget)

-- | Gets the widget that is currenty set as the image of /@button@/.
-- This may have been explicitly set by 'GI.Gtk.Objects.Button.buttonSetImage'
-- or constructed by 'GI.Gtk.Objects.Button.buttonNewFromStock'.
-- 
-- /Since: 2.6/
buttonGetImage ::
    (B.CallStack.HasCallStack, MonadIO m, IsButton a) =>
    a
    -- ^ /@button@/: a t'GI.Gtk.Objects.Button.Button'
    -> m (Maybe Gtk.Widget.Widget)
    -- ^ __Returns:__ a t'GI.Gtk.Objects.Widget.Widget' or 'P.Nothing' in case
    --     there is no image
buttonGetImage button = liftIO $ do
    button' <- unsafeManagedPtrCastPtr button
    result <- gtk_button_get_image button'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Widget.Widget) result'
        return result''
    touchManagedPtr button
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data ButtonGetImageMethodInfo
instance (signature ~ (m (Maybe Gtk.Widget.Widget)), MonadIO m, IsButton a) => O.OverloadedMethod ButtonGetImageMethodInfo a signature where
    overloadedMethod = buttonGetImage

instance O.OverloadedMethodInfo ButtonGetImageMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button.buttonGetImage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#v:buttonGetImage"
        })


#endif

-- method Button::get_image_position
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "button"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Button" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "PositionType" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_button_get_image_position" gtk_button_get_image_position :: 
    Ptr Button ->                           -- button : TInterface (Name {namespace = "Gtk", name = "Button"})
    IO CUInt

-- | Gets the position of the image relative to the text
-- inside the button.
-- 
-- /Since: 2.10/
buttonGetImagePosition ::
    (B.CallStack.HasCallStack, MonadIO m, IsButton a) =>
    a
    -- ^ /@button@/: a t'GI.Gtk.Objects.Button.Button'
    -> m Gtk.Enums.PositionType
    -- ^ __Returns:__ the position
buttonGetImagePosition button = liftIO $ do
    button' <- unsafeManagedPtrCastPtr button
    result <- gtk_button_get_image_position button'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr button
    return result'

#if defined(ENABLE_OVERLOADING)
data ButtonGetImagePositionMethodInfo
instance (signature ~ (m Gtk.Enums.PositionType), MonadIO m, IsButton a) => O.OverloadedMethod ButtonGetImagePositionMethodInfo a signature where
    overloadedMethod = buttonGetImagePosition

instance O.OverloadedMethodInfo ButtonGetImagePositionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button.buttonGetImagePosition",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#v:buttonGetImagePosition"
        })


#endif

-- method Button::get_label
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "button"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Button" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkButton" , sinceVersion = Nothing }
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

foreign import ccall "gtk_button_get_label" gtk_button_get_label :: 
    Ptr Button ->                           -- button : TInterface (Name {namespace = "Gtk", name = "Button"})
    IO CString

-- | Fetches the text from the label of the button, as set by
-- 'GI.Gtk.Objects.Button.buttonSetLabel'. If the label text has not
-- been set the return value will be 'P.Nothing'. This will be the
-- case if you create an empty button with 'GI.Gtk.Objects.Button.buttonNew' to
-- use as a container.
buttonGetLabel ::
    (B.CallStack.HasCallStack, MonadIO m, IsButton a) =>
    a
    -- ^ /@button@/: a t'GI.Gtk.Objects.Button.Button'
    -> m T.Text
    -- ^ __Returns:__ The text of the label widget. This string is owned
    -- by the widget and must not be modified or freed.
buttonGetLabel button = liftIO $ do
    button' <- unsafeManagedPtrCastPtr button
    result <- gtk_button_get_label button'
    checkUnexpectedReturnNULL "buttonGetLabel" result
    result' <- cstringToText result
    touchManagedPtr button
    return result'

#if defined(ENABLE_OVERLOADING)
data ButtonGetLabelMethodInfo
instance (signature ~ (m T.Text), MonadIO m, IsButton a) => O.OverloadedMethod ButtonGetLabelMethodInfo a signature where
    overloadedMethod = buttonGetLabel

instance O.OverloadedMethodInfo ButtonGetLabelMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button.buttonGetLabel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#v:buttonGetLabel"
        })


#endif

-- method Button::get_relief
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "button"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Button" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The #GtkButton you want the #GtkReliefStyle from."
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "ReliefStyle" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_button_get_relief" gtk_button_get_relief :: 
    Ptr Button ->                           -- button : TInterface (Name {namespace = "Gtk", name = "Button"})
    IO CUInt

-- | Returns the current relief style of the given t'GI.Gtk.Objects.Button.Button'.
buttonGetRelief ::
    (B.CallStack.HasCallStack, MonadIO m, IsButton a) =>
    a
    -- ^ /@button@/: The t'GI.Gtk.Objects.Button.Button' you want the t'GI.Gtk.Enums.ReliefStyle' from.
    -> m Gtk.Enums.ReliefStyle
    -- ^ __Returns:__ The current t'GI.Gtk.Enums.ReliefStyle'
buttonGetRelief button = liftIO $ do
    button' <- unsafeManagedPtrCastPtr button
    result <- gtk_button_get_relief button'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr button
    return result'

#if defined(ENABLE_OVERLOADING)
data ButtonGetReliefMethodInfo
instance (signature ~ (m Gtk.Enums.ReliefStyle), MonadIO m, IsButton a) => O.OverloadedMethod ButtonGetReliefMethodInfo a signature where
    overloadedMethod = buttonGetRelief

instance O.OverloadedMethodInfo ButtonGetReliefMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button.buttonGetRelief",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#v:buttonGetRelief"
        })


#endif

-- method Button::get_use_stock
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "button"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Button" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkButton" , sinceVersion = Nothing }
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

foreign import ccall "gtk_button_get_use_stock" gtk_button_get_use_stock :: 
    Ptr Button ->                           -- button : TInterface (Name {namespace = "Gtk", name = "Button"})
    IO CInt

{-# DEPRECATED buttonGetUseStock ["(Since version 3.10)"] #-}
-- | Returns whether the button label is a stock item.
buttonGetUseStock ::
    (B.CallStack.HasCallStack, MonadIO m, IsButton a) =>
    a
    -- ^ /@button@/: a t'GI.Gtk.Objects.Button.Button'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the button label is used to
    --               select a stock item instead of being
    --               used directly as the label text.
buttonGetUseStock button = liftIO $ do
    button' <- unsafeManagedPtrCastPtr button
    result <- gtk_button_get_use_stock button'
    let result' = (/= 0) result
    touchManagedPtr button
    return result'

#if defined(ENABLE_OVERLOADING)
data ButtonGetUseStockMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsButton a) => O.OverloadedMethod ButtonGetUseStockMethodInfo a signature where
    overloadedMethod = buttonGetUseStock

instance O.OverloadedMethodInfo ButtonGetUseStockMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button.buttonGetUseStock",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#v:buttonGetUseStock"
        })


#endif

-- method Button::get_use_underline
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "button"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Button" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkButton" , sinceVersion = Nothing }
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

foreign import ccall "gtk_button_get_use_underline" gtk_button_get_use_underline :: 
    Ptr Button ->                           -- button : TInterface (Name {namespace = "Gtk", name = "Button"})
    IO CInt

-- | Returns whether an embedded underline in the button label indicates a
-- mnemonic. See gtk_button_set_use_underline ().
buttonGetUseUnderline ::
    (B.CallStack.HasCallStack, MonadIO m, IsButton a) =>
    a
    -- ^ /@button@/: a t'GI.Gtk.Objects.Button.Button'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if an embedded underline in the button label
    --               indicates the mnemonic accelerator keys.
buttonGetUseUnderline button = liftIO $ do
    button' <- unsafeManagedPtrCastPtr button
    result <- gtk_button_get_use_underline button'
    let result' = (/= 0) result
    touchManagedPtr button
    return result'

#if defined(ENABLE_OVERLOADING)
data ButtonGetUseUnderlineMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsButton a) => O.OverloadedMethod ButtonGetUseUnderlineMethodInfo a signature where
    overloadedMethod = buttonGetUseUnderline

instance O.OverloadedMethodInfo ButtonGetUseUnderlineMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button.buttonGetUseUnderline",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#v:buttonGetUseUnderline"
        })


#endif

-- method Button::leave
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "button"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Button" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The #GtkButton you want to send the signal to."
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

foreign import ccall "gtk_button_leave" gtk_button_leave :: 
    Ptr Button ->                           -- button : TInterface (Name {namespace = "Gtk", name = "Button"})
    IO ()

{-# DEPRECATED buttonLeave ["(Since version 2.20)","Use the [Widget::leaveNotifyEvent](\"GI.Gtk.Objects.Widget#g:signal:leaveNotifyEvent\") signal."] #-}
-- | Emits a [Button::leave]("GI.Gtk.Objects.Button#g:signal:leave") signal to the given t'GI.Gtk.Objects.Button.Button'.
buttonLeave ::
    (B.CallStack.HasCallStack, MonadIO m, IsButton a) =>
    a
    -- ^ /@button@/: The t'GI.Gtk.Objects.Button.Button' you want to send the signal to.
    -> m ()
buttonLeave button = liftIO $ do
    button' <- unsafeManagedPtrCastPtr button
    gtk_button_leave button'
    touchManagedPtr button
    return ()

#if defined(ENABLE_OVERLOADING)
data ButtonLeaveMethodInfo
instance (signature ~ (m ()), MonadIO m, IsButton a) => O.OverloadedMethod ButtonLeaveMethodInfo a signature where
    overloadedMethod = buttonLeave

instance O.OverloadedMethodInfo ButtonLeaveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button.buttonLeave",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#v:buttonLeave"
        })


#endif

-- method Button::pressed
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "button"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Button" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The #GtkButton you want to send the signal to."
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

foreign import ccall "gtk_button_pressed" gtk_button_pressed :: 
    Ptr Button ->                           -- button : TInterface (Name {namespace = "Gtk", name = "Button"})
    IO ()

{-# DEPRECATED buttonPressed ["(Since version 2.20)","Use the [Widget::buttonPressEvent](\"GI.Gtk.Objects.Widget#g:signal:buttonPressEvent\") signal."] #-}
-- | Emits a [Button::pressed]("GI.Gtk.Objects.Button#g:signal:pressed") signal to the given t'GI.Gtk.Objects.Button.Button'.
buttonPressed ::
    (B.CallStack.HasCallStack, MonadIO m, IsButton a) =>
    a
    -- ^ /@button@/: The t'GI.Gtk.Objects.Button.Button' you want to send the signal to.
    -> m ()
buttonPressed button = liftIO $ do
    button' <- unsafeManagedPtrCastPtr button
    gtk_button_pressed button'
    touchManagedPtr button
    return ()

#if defined(ENABLE_OVERLOADING)
data ButtonPressedMethodInfo
instance (signature ~ (m ()), MonadIO m, IsButton a) => O.OverloadedMethod ButtonPressedMethodInfo a signature where
    overloadedMethod = buttonPressed

instance O.OverloadedMethodInfo ButtonPressedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button.buttonPressed",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#v:buttonPressed"
        })


#endif

-- method Button::released
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "button"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Button" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The #GtkButton you want to send the signal to."
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

foreign import ccall "gtk_button_released" gtk_button_released :: 
    Ptr Button ->                           -- button : TInterface (Name {namespace = "Gtk", name = "Button"})
    IO ()

{-# DEPRECATED buttonReleased ["(Since version 2.20)","Use the [Widget::buttonReleaseEvent](\"GI.Gtk.Objects.Widget#g:signal:buttonReleaseEvent\") signal."] #-}
-- | Emits a [Button::released]("GI.Gtk.Objects.Button#g:signal:released") signal to the given t'GI.Gtk.Objects.Button.Button'.
buttonReleased ::
    (B.CallStack.HasCallStack, MonadIO m, IsButton a) =>
    a
    -- ^ /@button@/: The t'GI.Gtk.Objects.Button.Button' you want to send the signal to.
    -> m ()
buttonReleased button = liftIO $ do
    button' <- unsafeManagedPtrCastPtr button
    gtk_button_released button'
    touchManagedPtr button
    return ()

#if defined(ENABLE_OVERLOADING)
data ButtonReleasedMethodInfo
instance (signature ~ (m ()), MonadIO m, IsButton a) => O.OverloadedMethod ButtonReleasedMethodInfo a signature where
    overloadedMethod = buttonReleased

instance O.OverloadedMethodInfo ButtonReleasedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button.buttonReleased",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#v:buttonReleased"
        })


#endif

-- method Button::set_alignment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "button"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Button" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkButton" , sinceVersion = Nothing }
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
--                       "the horizontal position of the child, 0.0 is left aligned,\n  1.0 is right aligned"
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
--                       "the vertical position of the child, 0.0 is top aligned,\n  1.0 is bottom aligned"
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

foreign import ccall "gtk_button_set_alignment" gtk_button_set_alignment :: 
    Ptr Button ->                           -- button : TInterface (Name {namespace = "Gtk", name = "Button"})
    CFloat ->                               -- xalign : TBasicType TFloat
    CFloat ->                               -- yalign : TBasicType TFloat
    IO ()

{-# DEPRECATED buttonSetAlignment ["(Since version 3.14)","Access the child widget directly if you need to control","its alignment."] #-}
-- | Sets the alignment of the child. This property has no effect unless
-- the child is a t'GI.Gtk.Objects.Misc.Misc' or a t'GI.Gtk.Objects.Alignment.Alignment'.
-- 
-- /Since: 2.4/
buttonSetAlignment ::
    (B.CallStack.HasCallStack, MonadIO m, IsButton a) =>
    a
    -- ^ /@button@/: a t'GI.Gtk.Objects.Button.Button'
    -> Float
    -- ^ /@xalign@/: the horizontal position of the child, 0.0 is left aligned,
    --   1.0 is right aligned
    -> Float
    -- ^ /@yalign@/: the vertical position of the child, 0.0 is top aligned,
    --   1.0 is bottom aligned
    -> m ()
buttonSetAlignment button xalign yalign = liftIO $ do
    button' <- unsafeManagedPtrCastPtr button
    let xalign' = realToFrac xalign
    let yalign' = realToFrac yalign
    gtk_button_set_alignment button' xalign' yalign'
    touchManagedPtr button
    return ()

#if defined(ENABLE_OVERLOADING)
data ButtonSetAlignmentMethodInfo
instance (signature ~ (Float -> Float -> m ()), MonadIO m, IsButton a) => O.OverloadedMethod ButtonSetAlignmentMethodInfo a signature where
    overloadedMethod = buttonSetAlignment

instance O.OverloadedMethodInfo ButtonSetAlignmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button.buttonSetAlignment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#v:buttonSetAlignment"
        })


#endif

-- method Button::set_always_show_image
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "button"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Button" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "always_show"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "%TRUE if the menuitem should always show the image"
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

foreign import ccall "gtk_button_set_always_show_image" gtk_button_set_always_show_image :: 
    Ptr Button ->                           -- button : TInterface (Name {namespace = "Gtk", name = "Button"})
    CInt ->                                 -- always_show : TBasicType TBoolean
    IO ()

-- | If 'P.True', the button will ignore the [Settings:gtkButtonImages]("GI.Gtk.Objects.Settings#g:attr:gtkButtonImages")
-- setting and always show the image, if available.
-- 
-- Use this property if the button  would be useless or hard to use
-- without the image.
-- 
-- /Since: 3.6/
buttonSetAlwaysShowImage ::
    (B.CallStack.HasCallStack, MonadIO m, IsButton a) =>
    a
    -- ^ /@button@/: a t'GI.Gtk.Objects.Button.Button'
    -> Bool
    -- ^ /@alwaysShow@/: 'P.True' if the menuitem should always show the image
    -> m ()
buttonSetAlwaysShowImage button alwaysShow = liftIO $ do
    button' <- unsafeManagedPtrCastPtr button
    let alwaysShow' = (fromIntegral . fromEnum) alwaysShow
    gtk_button_set_always_show_image button' alwaysShow'
    touchManagedPtr button
    return ()

#if defined(ENABLE_OVERLOADING)
data ButtonSetAlwaysShowImageMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsButton a) => O.OverloadedMethod ButtonSetAlwaysShowImageMethodInfo a signature where
    overloadedMethod = buttonSetAlwaysShowImage

instance O.OverloadedMethodInfo ButtonSetAlwaysShowImageMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button.buttonSetAlwaysShowImage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#v:buttonSetAlwaysShowImage"
        })


#endif

-- method Button::set_focus_on_click
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "button"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Button" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "focus_on_click"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "whether the button grabs focus when clicked with the mouse"
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

foreign import ccall "gtk_button_set_focus_on_click" gtk_button_set_focus_on_click :: 
    Ptr Button ->                           -- button : TInterface (Name {namespace = "Gtk", name = "Button"})
    CInt ->                                 -- focus_on_click : TBasicType TBoolean
    IO ()

{-# DEPRECATED buttonSetFocusOnClick ["(Since version 3.20)","Use 'GI.Gtk.Objects.Widget.widgetSetFocusOnClick' instead"] #-}
-- | Sets whether the button will grab focus when it is clicked with the mouse.
-- Making mouse clicks not grab focus is useful in places like toolbars where
-- you don’t want the keyboard focus removed from the main area of the
-- application.
-- 
-- /Since: 2.4/
buttonSetFocusOnClick ::
    (B.CallStack.HasCallStack, MonadIO m, IsButton a) =>
    a
    -- ^ /@button@/: a t'GI.Gtk.Objects.Button.Button'
    -> Bool
    -- ^ /@focusOnClick@/: whether the button grabs focus when clicked with the mouse
    -> m ()
buttonSetFocusOnClick button focusOnClick = liftIO $ do
    button' <- unsafeManagedPtrCastPtr button
    let focusOnClick' = (fromIntegral . fromEnum) focusOnClick
    gtk_button_set_focus_on_click button' focusOnClick'
    touchManagedPtr button
    return ()

#if defined(ENABLE_OVERLOADING)
data ButtonSetFocusOnClickMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsButton a) => O.OverloadedMethod ButtonSetFocusOnClickMethodInfo a signature where
    overloadedMethod = buttonSetFocusOnClick

instance O.OverloadedMethodInfo ButtonSetFocusOnClickMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button.buttonSetFocusOnClick",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#v:buttonSetFocusOnClick"
        })


#endif

-- method Button::set_image
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "button"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Button" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "image"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a widget to set as the image for the button, or %NULL to unset"
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

foreign import ccall "gtk_button_set_image" gtk_button_set_image :: 
    Ptr Button ->                           -- button : TInterface (Name {namespace = "Gtk", name = "Button"})
    Ptr Gtk.Widget.Widget ->                -- image : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | Set the image of /@button@/ to the given widget. The image will be
-- displayed if the label text is 'P.Nothing' or if
-- [Button:alwaysShowImage]("GI.Gtk.Objects.Button#g:attr:alwaysShowImage") is 'P.True'. You don’t have to call
-- 'GI.Gtk.Objects.Widget.widgetShow' on /@image@/ yourself.
-- 
-- /Since: 2.6/
buttonSetImage ::
    (B.CallStack.HasCallStack, MonadIO m, IsButton a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@button@/: a t'GI.Gtk.Objects.Button.Button'
    -> Maybe (b)
    -- ^ /@image@/: a widget to set as the image for the button, or 'P.Nothing' to unset
    -> m ()
buttonSetImage button image = liftIO $ do
    button' <- unsafeManagedPtrCastPtr button
    maybeImage <- case image of
        Nothing -> return nullPtr
        Just jImage -> do
            jImage' <- unsafeManagedPtrCastPtr jImage
            return jImage'
    gtk_button_set_image button' maybeImage
    touchManagedPtr button
    whenJust image touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data ButtonSetImageMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsButton a, Gtk.Widget.IsWidget b) => O.OverloadedMethod ButtonSetImageMethodInfo a signature where
    overloadedMethod = buttonSetImage

instance O.OverloadedMethodInfo ButtonSetImageMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button.buttonSetImage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#v:buttonSetImage"
        })


#endif

-- method Button::set_image_position
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "button"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Button" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "position"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PositionType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the position" , sinceVersion = Nothing }
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

foreign import ccall "gtk_button_set_image_position" gtk_button_set_image_position :: 
    Ptr Button ->                           -- button : TInterface (Name {namespace = "Gtk", name = "Button"})
    CUInt ->                                -- position : TInterface (Name {namespace = "Gtk", name = "PositionType"})
    IO ()

-- | Sets the position of the image relative to the text
-- inside the button.
-- 
-- /Since: 2.10/
buttonSetImagePosition ::
    (B.CallStack.HasCallStack, MonadIO m, IsButton a) =>
    a
    -- ^ /@button@/: a t'GI.Gtk.Objects.Button.Button'
    -> Gtk.Enums.PositionType
    -- ^ /@position@/: the position
    -> m ()
buttonSetImagePosition button position = liftIO $ do
    button' <- unsafeManagedPtrCastPtr button
    let position' = (fromIntegral . fromEnum) position
    gtk_button_set_image_position button' position'
    touchManagedPtr button
    return ()

#if defined(ENABLE_OVERLOADING)
data ButtonSetImagePositionMethodInfo
instance (signature ~ (Gtk.Enums.PositionType -> m ()), MonadIO m, IsButton a) => O.OverloadedMethod ButtonSetImagePositionMethodInfo a signature where
    overloadedMethod = buttonSetImagePosition

instance O.OverloadedMethodInfo ButtonSetImagePositionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button.buttonSetImagePosition",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#v:buttonSetImagePosition"
        })


#endif

-- method Button::set_label
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "button"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Button" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "label"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a string" , sinceVersion = Nothing }
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

foreign import ccall "gtk_button_set_label" gtk_button_set_label :: 
    Ptr Button ->                           -- button : TInterface (Name {namespace = "Gtk", name = "Button"})
    CString ->                              -- label : TBasicType TUTF8
    IO ()

-- | Sets the text of the label of the button to /@str@/. This text is
-- also used to select the stock item if 'GI.Gtk.Objects.Button.buttonSetUseStock'
-- is used.
-- 
-- This will also clear any previously set labels.
buttonSetLabel ::
    (B.CallStack.HasCallStack, MonadIO m, IsButton a) =>
    a
    -- ^ /@button@/: a t'GI.Gtk.Objects.Button.Button'
    -> T.Text
    -- ^ /@label@/: a string
    -> m ()
buttonSetLabel button label = liftIO $ do
    button' <- unsafeManagedPtrCastPtr button
    label' <- textToCString label
    gtk_button_set_label button' label'
    touchManagedPtr button
    freeMem label'
    return ()

#if defined(ENABLE_OVERLOADING)
data ButtonSetLabelMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsButton a) => O.OverloadedMethod ButtonSetLabelMethodInfo a signature where
    overloadedMethod = buttonSetLabel

instance O.OverloadedMethodInfo ButtonSetLabelMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button.buttonSetLabel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#v:buttonSetLabel"
        })


#endif

-- method Button::set_relief
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "button"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Button" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The #GtkButton you want to set relief styles of"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "relief"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ReliefStyle" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The GtkReliefStyle as described above"
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

foreign import ccall "gtk_button_set_relief" gtk_button_set_relief :: 
    Ptr Button ->                           -- button : TInterface (Name {namespace = "Gtk", name = "Button"})
    CUInt ->                                -- relief : TInterface (Name {namespace = "Gtk", name = "ReliefStyle"})
    IO ()

-- | Sets the relief style of the edges of the given t'GI.Gtk.Objects.Button.Button' widget.
-- Two styles exist, 'GI.Gtk.Enums.ReliefStyleNormal' and 'GI.Gtk.Enums.ReliefStyleNone'.
-- The default style is, as one can guess, 'GI.Gtk.Enums.ReliefStyleNormal'.
-- The deprecated value 'GI.Gtk.Enums.ReliefStyleHalf' behaves the same as
-- 'GI.Gtk.Enums.ReliefStyleNormal'.
buttonSetRelief ::
    (B.CallStack.HasCallStack, MonadIO m, IsButton a) =>
    a
    -- ^ /@button@/: The t'GI.Gtk.Objects.Button.Button' you want to set relief styles of
    -> Gtk.Enums.ReliefStyle
    -- ^ /@relief@/: The GtkReliefStyle as described above
    -> m ()
buttonSetRelief button relief = liftIO $ do
    button' <- unsafeManagedPtrCastPtr button
    let relief' = (fromIntegral . fromEnum) relief
    gtk_button_set_relief button' relief'
    touchManagedPtr button
    return ()

#if defined(ENABLE_OVERLOADING)
data ButtonSetReliefMethodInfo
instance (signature ~ (Gtk.Enums.ReliefStyle -> m ()), MonadIO m, IsButton a) => O.OverloadedMethod ButtonSetReliefMethodInfo a signature where
    overloadedMethod = buttonSetRelief

instance O.OverloadedMethodInfo ButtonSetReliefMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button.buttonSetRelief",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#v:buttonSetRelief"
        })


#endif

-- method Button::set_use_stock
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "button"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Button" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "use_stock"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE if the button should use a stock item"
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

foreign import ccall "gtk_button_set_use_stock" gtk_button_set_use_stock :: 
    Ptr Button ->                           -- button : TInterface (Name {namespace = "Gtk", name = "Button"})
    CInt ->                                 -- use_stock : TBasicType TBoolean
    IO ()

{-# DEPRECATED buttonSetUseStock ["(Since version 3.10)"] #-}
-- | If 'P.True', the label set on the button is used as a
-- stock id to select the stock item for the button.
buttonSetUseStock ::
    (B.CallStack.HasCallStack, MonadIO m, IsButton a) =>
    a
    -- ^ /@button@/: a t'GI.Gtk.Objects.Button.Button'
    -> Bool
    -- ^ /@useStock@/: 'P.True' if the button should use a stock item
    -> m ()
buttonSetUseStock button useStock = liftIO $ do
    button' <- unsafeManagedPtrCastPtr button
    let useStock' = (fromIntegral . fromEnum) useStock
    gtk_button_set_use_stock button' useStock'
    touchManagedPtr button
    return ()

#if defined(ENABLE_OVERLOADING)
data ButtonSetUseStockMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsButton a) => O.OverloadedMethod ButtonSetUseStockMethodInfo a signature where
    overloadedMethod = buttonSetUseStock

instance O.OverloadedMethodInfo ButtonSetUseStockMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button.buttonSetUseStock",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#v:buttonSetUseStock"
        })


#endif

-- method Button::set_use_underline
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "button"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Button" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "use_underline"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "%TRUE if underlines in the text indicate mnemonics"
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

foreign import ccall "gtk_button_set_use_underline" gtk_button_set_use_underline :: 
    Ptr Button ->                           -- button : TInterface (Name {namespace = "Gtk", name = "Button"})
    CInt ->                                 -- use_underline : TBasicType TBoolean
    IO ()

-- | If true, an underline in the text of the button label indicates
-- the next character should be used for the mnemonic accelerator key.
buttonSetUseUnderline ::
    (B.CallStack.HasCallStack, MonadIO m, IsButton a) =>
    a
    -- ^ /@button@/: a t'GI.Gtk.Objects.Button.Button'
    -> Bool
    -- ^ /@useUnderline@/: 'P.True' if underlines in the text indicate mnemonics
    -> m ()
buttonSetUseUnderline button useUnderline = liftIO $ do
    button' <- unsafeManagedPtrCastPtr button
    let useUnderline' = (fromIntegral . fromEnum) useUnderline
    gtk_button_set_use_underline button' useUnderline'
    touchManagedPtr button
    return ()

#if defined(ENABLE_OVERLOADING)
data ButtonSetUseUnderlineMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsButton a) => O.OverloadedMethod ButtonSetUseUnderlineMethodInfo a signature where
    overloadedMethod = buttonSetUseUnderline

instance O.OverloadedMethodInfo ButtonSetUseUnderlineMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Button.buttonSetUseUnderline",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Button.html#v:buttonSetUseUnderline"
        })


#endif


