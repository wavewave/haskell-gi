{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- GtkLockButton is a widget that can be used in control panels or
-- preference dialogs to allow users to obtain and revoke authorizations
-- needed to operate the controls. The required authorization is represented
-- by a t'GI.Gio.Objects.Permission.Permission' object. Concrete implementations of t'GI.Gio.Objects.Permission.Permission' may use
-- PolicyKit or some other authorization framework. To obtain a PolicyKit-based
-- t'GI.Gio.Objects.Permission.Permission', use @/polkit_permission_new()/@.
-- 
-- If the user is not currently allowed to perform the action, but can obtain
-- the permission, the widget looks like this:
-- 
-- <<https://developer.gnome.org/gtk3/stable/lockbutton-locked.png>>
-- 
-- and the user can click the button to request the permission. Depending
-- on the platform, this may pop up an authentication dialog or ask the user
-- to authenticate in some other way. Once the user has obtained the permission,
-- the widget changes to this:
-- 
-- <<https://developer.gnome.org/gtk3/stable/lockbutton-unlocked.png>>
-- 
-- and the permission can be dropped again by clicking the button. If the user
-- is not able to obtain the permission at all, the widget looks like this:
-- 
-- <<https://developer.gnome.org/gtk3/stable/lockbutton-sorry.png>>
-- 
-- If the user has the permission and cannot drop it, the button is hidden.
-- 
-- The text (and tooltips) that are shown in the various cases can be adjusted
-- with the [LockButton:textLock]("GI.Gtk.Objects.LockButton#g:attr:textLock"), [LockButton:textUnlock]("GI.Gtk.Objects.LockButton#g:attr:textUnlock"),
-- [LockButton:tooltipLock]("GI.Gtk.Objects.LockButton#g:attr:tooltipLock"), [LockButton:tooltipUnlock]("GI.Gtk.Objects.LockButton#g:attr:tooltipUnlock") and
-- [LockButton:tooltipNotAuthorized]("GI.Gtk.Objects.LockButton#g:attr:tooltipNotAuthorized") properties.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.LockButton
    ( 

-- * Exported types
    LockButton(..)                          ,
    IsLockButton                            ,
    toLockButton                            ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [clicked]("GI.Gtk.Objects.Button#g:method:clicked"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [doSetRelatedAction]("GI.Gtk.Interfaces.Activatable#g:method:doSetRelatedAction"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [enter]("GI.Gtk.Objects.Button#g:method:enter"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [leave]("GI.Gtk.Objects.Button#g:method:leave"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [pressed]("GI.Gtk.Objects.Button#g:method:pressed"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [released]("GI.Gtk.Objects.Button#g:method:released"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [syncActionProperties]("GI.Gtk.Interfaces.Activatable#g:method:syncActionProperties"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getActionName]("GI.Gtk.Interfaces.Actionable#g:method:getActionName"), [getActionTargetValue]("GI.Gtk.Interfaces.Actionable#g:method:getActionTargetValue"), [getAlignment]("GI.Gtk.Objects.Button#g:method:getAlignment"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAlwaysShowImage]("GI.Gtk.Objects.Button#g:method:getAlwaysShowImage"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChild]("GI.Gtk.Objects.Bin#g:method:getChild"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEventWindow]("GI.Gtk.Objects.Button#g:method:getEventWindow"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Button#g:method:getFocusOnClick"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getImage]("GI.Gtk.Objects.Button#g:method:getImage"), [getImagePosition]("GI.Gtk.Objects.Button#g:method:getImagePosition"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getLabel]("GI.Gtk.Objects.Button#g:method:getLabel"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPermission]("GI.Gtk.Objects.LockButton#g:method:getPermission"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRelatedAction]("GI.Gtk.Interfaces.Activatable#g:method:getRelatedAction"), [getRelief]("GI.Gtk.Objects.Button#g:method:getRelief"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getUseActionAppearance]("GI.Gtk.Interfaces.Activatable#g:method:getUseActionAppearance"), [getUseStock]("GI.Gtk.Objects.Button#g:method:getUseStock"), [getUseUnderline]("GI.Gtk.Objects.Button#g:method:getUseUnderline"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setActionName]("GI.Gtk.Interfaces.Actionable#g:method:setActionName"), [setActionTargetValue]("GI.Gtk.Interfaces.Actionable#g:method:setActionTargetValue"), [setAlignment]("GI.Gtk.Objects.Button#g:method:setAlignment"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAlwaysShowImage]("GI.Gtk.Objects.Button#g:method:setAlwaysShowImage"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDetailedActionName]("GI.Gtk.Interfaces.Actionable#g:method:setDetailedActionName"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Button#g:method:setFocusOnClick"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setImage]("GI.Gtk.Objects.Button#g:method:setImage"), [setImagePosition]("GI.Gtk.Objects.Button#g:method:setImagePosition"), [setLabel]("GI.Gtk.Objects.Button#g:method:setLabel"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setPermission]("GI.Gtk.Objects.LockButton#g:method:setPermission"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setRelatedAction]("GI.Gtk.Interfaces.Activatable#g:method:setRelatedAction"), [setRelief]("GI.Gtk.Objects.Button#g:method:setRelief"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setUseActionAppearance]("GI.Gtk.Interfaces.Activatable#g:method:setUseActionAppearance"), [setUseStock]("GI.Gtk.Objects.Button#g:method:setUseStock"), [setUseUnderline]("GI.Gtk.Objects.Button#g:method:setUseUnderline"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveLockButtonMethod                 ,
#endif

-- ** getPermission #method:getPermission#

#if defined(ENABLE_OVERLOADING)
    LockButtonGetPermissionMethodInfo       ,
#endif
    lockButtonGetPermission                 ,


-- ** new #method:new#

    lockButtonNew                           ,


-- ** setPermission #method:setPermission#

#if defined(ENABLE_OVERLOADING)
    LockButtonSetPermissionMethodInfo       ,
#endif
    lockButtonSetPermission                 ,




 -- * Properties


-- ** permission #attr:permission#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    LockButtonPermissionPropertyInfo        ,
#endif
    clearLockButtonPermission               ,
    constructLockButtonPermission           ,
    getLockButtonPermission                 ,
#if defined(ENABLE_OVERLOADING)
    lockButtonPermission                    ,
#endif
    setLockButtonPermission                 ,


-- ** textLock #attr:textLock#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    LockButtonTextLockPropertyInfo          ,
#endif
    clearLockButtonTextLock                 ,
    constructLockButtonTextLock             ,
    getLockButtonTextLock                   ,
#if defined(ENABLE_OVERLOADING)
    lockButtonTextLock                      ,
#endif
    setLockButtonTextLock                   ,


-- ** textUnlock #attr:textUnlock#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    LockButtonTextUnlockPropertyInfo        ,
#endif
    clearLockButtonTextUnlock               ,
    constructLockButtonTextUnlock           ,
    getLockButtonTextUnlock                 ,
#if defined(ENABLE_OVERLOADING)
    lockButtonTextUnlock                    ,
#endif
    setLockButtonTextUnlock                 ,


-- ** tooltipLock #attr:tooltipLock#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    LockButtonTooltipLockPropertyInfo       ,
#endif
    clearLockButtonTooltipLock              ,
    constructLockButtonTooltipLock          ,
    getLockButtonTooltipLock                ,
#if defined(ENABLE_OVERLOADING)
    lockButtonTooltipLock                   ,
#endif
    setLockButtonTooltipLock                ,


-- ** tooltipNotAuthorized #attr:tooltipNotAuthorized#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    LockButtonTooltipNotAuthorizedPropertyInfo,
#endif
    clearLockButtonTooltipNotAuthorized     ,
    constructLockButtonTooltipNotAuthorized ,
    getLockButtonTooltipNotAuthorized       ,
#if defined(ENABLE_OVERLOADING)
    lockButtonTooltipNotAuthorized          ,
#endif
    setLockButtonTooltipNotAuthorized       ,


-- ** tooltipUnlock #attr:tooltipUnlock#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    LockButtonTooltipUnlockPropertyInfo     ,
#endif
    clearLockButtonTooltipUnlock            ,
    constructLockButtonTooltipUnlock        ,
    getLockButtonTooltipUnlock              ,
#if defined(ENABLE_OVERLOADING)
    lockButtonTooltipUnlock                 ,
#endif
    setLockButtonTooltipUnlock              ,




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
import qualified GI.Gio.Objects.Permission as Gio.Permission
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Actionable as Gtk.Actionable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Activatable as Gtk.Activatable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Objects.Bin as Gtk.Bin
import {-# SOURCE #-} qualified GI.Gtk.Objects.Button as Gtk.Button
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype LockButton = LockButton (SP.ManagedPtr LockButton)
    deriving (Eq)

instance SP.ManagedPtrNewtype LockButton where
    toManagedPtr (LockButton p) = p

foreign import ccall "gtk_lock_button_get_type"
    c_gtk_lock_button_get_type :: IO B.Types.GType

instance B.Types.TypedObject LockButton where
    glibType = c_gtk_lock_button_get_type

instance B.Types.GObject LockButton

-- | Type class for types which can be safely cast to `LockButton`, for instance with `toLockButton`.
class (SP.GObject o, O.IsDescendantOf LockButton o) => IsLockButton o
instance (SP.GObject o, O.IsDescendantOf LockButton o) => IsLockButton o

instance O.HasParentTypes LockButton
type instance O.ParentTypes LockButton = '[Gtk.Button.Button, Gtk.Bin.Bin, Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Actionable.Actionable, Gtk.Activatable.Activatable, Gtk.Buildable.Buildable]

-- | Cast to `LockButton`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toLockButton :: (MIO.MonadIO m, IsLockButton o) => o -> m LockButton
toLockButton = MIO.liftIO . B.ManagedPtr.unsafeCastTo LockButton

-- | Convert 'LockButton' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe LockButton) where
    gvalueGType_ = c_gtk_lock_button_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr LockButton)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr LockButton)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject LockButton ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveLockButtonMethod (t :: Symbol) (o :: *) :: * where
    ResolveLockButtonMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveLockButtonMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveLockButtonMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveLockButtonMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveLockButtonMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveLockButtonMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveLockButtonMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveLockButtonMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveLockButtonMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveLockButtonMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveLockButtonMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveLockButtonMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveLockButtonMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveLockButtonMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveLockButtonMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveLockButtonMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveLockButtonMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveLockButtonMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveLockButtonMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveLockButtonMethod "clicked" o = Gtk.Button.ButtonClickedMethodInfo
    ResolveLockButtonMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveLockButtonMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveLockButtonMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveLockButtonMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveLockButtonMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveLockButtonMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveLockButtonMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveLockButtonMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveLockButtonMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveLockButtonMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveLockButtonMethod "doSetRelatedAction" o = Gtk.Activatable.ActivatableDoSetRelatedActionMethodInfo
    ResolveLockButtonMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveLockButtonMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveLockButtonMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveLockButtonMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveLockButtonMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveLockButtonMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveLockButtonMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveLockButtonMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveLockButtonMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveLockButtonMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveLockButtonMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveLockButtonMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveLockButtonMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveLockButtonMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveLockButtonMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveLockButtonMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveLockButtonMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveLockButtonMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveLockButtonMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveLockButtonMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveLockButtonMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveLockButtonMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveLockButtonMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveLockButtonMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveLockButtonMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveLockButtonMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveLockButtonMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveLockButtonMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveLockButtonMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveLockButtonMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveLockButtonMethod "enter" o = Gtk.Button.ButtonEnterMethodInfo
    ResolveLockButtonMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveLockButtonMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveLockButtonMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveLockButtonMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveLockButtonMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveLockButtonMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveLockButtonMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveLockButtonMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveLockButtonMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveLockButtonMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveLockButtonMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveLockButtonMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveLockButtonMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveLockButtonMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveLockButtonMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveLockButtonMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveLockButtonMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveLockButtonMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveLockButtonMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveLockButtonMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveLockButtonMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveLockButtonMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveLockButtonMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveLockButtonMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveLockButtonMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveLockButtonMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveLockButtonMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveLockButtonMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveLockButtonMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveLockButtonMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveLockButtonMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveLockButtonMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveLockButtonMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveLockButtonMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveLockButtonMethod "leave" o = Gtk.Button.ButtonLeaveMethodInfo
    ResolveLockButtonMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveLockButtonMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveLockButtonMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveLockButtonMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveLockButtonMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveLockButtonMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveLockButtonMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveLockButtonMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveLockButtonMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveLockButtonMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveLockButtonMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveLockButtonMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveLockButtonMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveLockButtonMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveLockButtonMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveLockButtonMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveLockButtonMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveLockButtonMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveLockButtonMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveLockButtonMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveLockButtonMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveLockButtonMethod "pressed" o = Gtk.Button.ButtonPressedMethodInfo
    ResolveLockButtonMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveLockButtonMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveLockButtonMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveLockButtonMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveLockButtonMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveLockButtonMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveLockButtonMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveLockButtonMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveLockButtonMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveLockButtonMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveLockButtonMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveLockButtonMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveLockButtonMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveLockButtonMethod "released" o = Gtk.Button.ButtonReleasedMethodInfo
    ResolveLockButtonMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolveLockButtonMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveLockButtonMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveLockButtonMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveLockButtonMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveLockButtonMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveLockButtonMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveLockButtonMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveLockButtonMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveLockButtonMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveLockButtonMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveLockButtonMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveLockButtonMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveLockButtonMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveLockButtonMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveLockButtonMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveLockButtonMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveLockButtonMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveLockButtonMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveLockButtonMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveLockButtonMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveLockButtonMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveLockButtonMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveLockButtonMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveLockButtonMethod "syncActionProperties" o = Gtk.Activatable.ActivatableSyncActionPropertiesMethodInfo
    ResolveLockButtonMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveLockButtonMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveLockButtonMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveLockButtonMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveLockButtonMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveLockButtonMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveLockButtonMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveLockButtonMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveLockButtonMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveLockButtonMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveLockButtonMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveLockButtonMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveLockButtonMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveLockButtonMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveLockButtonMethod "getActionName" o = Gtk.Actionable.ActionableGetActionNameMethodInfo
    ResolveLockButtonMethod "getActionTargetValue" o = Gtk.Actionable.ActionableGetActionTargetValueMethodInfo
    ResolveLockButtonMethod "getAlignment" o = Gtk.Button.ButtonGetAlignmentMethodInfo
    ResolveLockButtonMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveLockButtonMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveLockButtonMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveLockButtonMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveLockButtonMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveLockButtonMethod "getAlwaysShowImage" o = Gtk.Button.ButtonGetAlwaysShowImageMethodInfo
    ResolveLockButtonMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveLockButtonMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveLockButtonMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveLockButtonMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveLockButtonMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveLockButtonMethod "getChild" o = Gtk.Bin.BinGetChildMethodInfo
    ResolveLockButtonMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveLockButtonMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveLockButtonMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveLockButtonMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveLockButtonMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveLockButtonMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveLockButtonMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveLockButtonMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveLockButtonMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveLockButtonMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveLockButtonMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveLockButtonMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveLockButtonMethod "getEventWindow" o = Gtk.Button.ButtonGetEventWindowMethodInfo
    ResolveLockButtonMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveLockButtonMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveLockButtonMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveLockButtonMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveLockButtonMethod "getFocusOnClick" o = Gtk.Button.ButtonGetFocusOnClickMethodInfo
    ResolveLockButtonMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveLockButtonMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveLockButtonMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveLockButtonMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveLockButtonMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveLockButtonMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveLockButtonMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveLockButtonMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveLockButtonMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveLockButtonMethod "getImage" o = Gtk.Button.ButtonGetImageMethodInfo
    ResolveLockButtonMethod "getImagePosition" o = Gtk.Button.ButtonGetImagePositionMethodInfo
    ResolveLockButtonMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveLockButtonMethod "getLabel" o = Gtk.Button.ButtonGetLabelMethodInfo
    ResolveLockButtonMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveLockButtonMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveLockButtonMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveLockButtonMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveLockButtonMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveLockButtonMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveLockButtonMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveLockButtonMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveLockButtonMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveLockButtonMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveLockButtonMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveLockButtonMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveLockButtonMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveLockButtonMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveLockButtonMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveLockButtonMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveLockButtonMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveLockButtonMethod "getPermission" o = LockButtonGetPermissionMethodInfo
    ResolveLockButtonMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveLockButtonMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveLockButtonMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveLockButtonMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveLockButtonMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveLockButtonMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveLockButtonMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveLockButtonMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveLockButtonMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveLockButtonMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveLockButtonMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveLockButtonMethod "getRelatedAction" o = Gtk.Activatable.ActivatableGetRelatedActionMethodInfo
    ResolveLockButtonMethod "getRelief" o = Gtk.Button.ButtonGetReliefMethodInfo
    ResolveLockButtonMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveLockButtonMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveLockButtonMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveLockButtonMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveLockButtonMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveLockButtonMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveLockButtonMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveLockButtonMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveLockButtonMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveLockButtonMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveLockButtonMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveLockButtonMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveLockButtonMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveLockButtonMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveLockButtonMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveLockButtonMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveLockButtonMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveLockButtonMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveLockButtonMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveLockButtonMethod "getUseActionAppearance" o = Gtk.Activatable.ActivatableGetUseActionAppearanceMethodInfo
    ResolveLockButtonMethod "getUseStock" o = Gtk.Button.ButtonGetUseStockMethodInfo
    ResolveLockButtonMethod "getUseUnderline" o = Gtk.Button.ButtonGetUseUnderlineMethodInfo
    ResolveLockButtonMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveLockButtonMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveLockButtonMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveLockButtonMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveLockButtonMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveLockButtonMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveLockButtonMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveLockButtonMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveLockButtonMethod "setActionName" o = Gtk.Actionable.ActionableSetActionNameMethodInfo
    ResolveLockButtonMethod "setActionTargetValue" o = Gtk.Actionable.ActionableSetActionTargetValueMethodInfo
    ResolveLockButtonMethod "setAlignment" o = Gtk.Button.ButtonSetAlignmentMethodInfo
    ResolveLockButtonMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveLockButtonMethod "setAlwaysShowImage" o = Gtk.Button.ButtonSetAlwaysShowImageMethodInfo
    ResolveLockButtonMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveLockButtonMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveLockButtonMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveLockButtonMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveLockButtonMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveLockButtonMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveLockButtonMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveLockButtonMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveLockButtonMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveLockButtonMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveLockButtonMethod "setDetailedActionName" o = Gtk.Actionable.ActionableSetDetailedActionNameMethodInfo
    ResolveLockButtonMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveLockButtonMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveLockButtonMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveLockButtonMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveLockButtonMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveLockButtonMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveLockButtonMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveLockButtonMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveLockButtonMethod "setFocusOnClick" o = Gtk.Button.ButtonSetFocusOnClickMethodInfo
    ResolveLockButtonMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveLockButtonMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveLockButtonMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveLockButtonMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveLockButtonMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveLockButtonMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveLockButtonMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveLockButtonMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveLockButtonMethod "setImage" o = Gtk.Button.ButtonSetImageMethodInfo
    ResolveLockButtonMethod "setImagePosition" o = Gtk.Button.ButtonSetImagePositionMethodInfo
    ResolveLockButtonMethod "setLabel" o = Gtk.Button.ButtonSetLabelMethodInfo
    ResolveLockButtonMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveLockButtonMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveLockButtonMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveLockButtonMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveLockButtonMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveLockButtonMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveLockButtonMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveLockButtonMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveLockButtonMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveLockButtonMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveLockButtonMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveLockButtonMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveLockButtonMethod "setPermission" o = LockButtonSetPermissionMethodInfo
    ResolveLockButtonMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveLockButtonMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveLockButtonMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveLockButtonMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveLockButtonMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveLockButtonMethod "setRelatedAction" o = Gtk.Activatable.ActivatableSetRelatedActionMethodInfo
    ResolveLockButtonMethod "setRelief" o = Gtk.Button.ButtonSetReliefMethodInfo
    ResolveLockButtonMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveLockButtonMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveLockButtonMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveLockButtonMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveLockButtonMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveLockButtonMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveLockButtonMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveLockButtonMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveLockButtonMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveLockButtonMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveLockButtonMethod "setUseActionAppearance" o = Gtk.Activatable.ActivatableSetUseActionAppearanceMethodInfo
    ResolveLockButtonMethod "setUseStock" o = Gtk.Button.ButtonSetUseStockMethodInfo
    ResolveLockButtonMethod "setUseUnderline" o = Gtk.Button.ButtonSetUseUnderlineMethodInfo
    ResolveLockButtonMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveLockButtonMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveLockButtonMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveLockButtonMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveLockButtonMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveLockButtonMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveLockButtonMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveLockButtonMethod t LockButton, O.OverloadedMethod info LockButton p) => OL.IsLabel t (LockButton -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveLockButtonMethod t LockButton, O.OverloadedMethod info LockButton p, R.HasField t LockButton p) => R.HasField t LockButton p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveLockButtonMethod t LockButton, O.OverloadedMethodInfo info LockButton) => OL.IsLabel t (O.MethodProxy info LockButton) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- VVV Prop "permission"
   -- Type: TInterface (Name {namespace = "Gio", name = "Permission"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just True)

-- | Get the value of the “@permission@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' lockButton #permission
-- @
getLockButtonPermission :: (MonadIO m, IsLockButton o) => o -> m Gio.Permission.Permission
getLockButtonPermission obj = MIO.liftIO $ checkUnexpectedNothing "getLockButtonPermission" $ B.Properties.getObjectPropertyObject obj "permission" Gio.Permission.Permission

-- | Set the value of the “@permission@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' lockButton [ #permission 'Data.GI.Base.Attributes.:=' value ]
-- @
setLockButtonPermission :: (MonadIO m, IsLockButton o, Gio.Permission.IsPermission a) => o -> a -> m ()
setLockButtonPermission obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "permission" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@permission@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructLockButtonPermission :: (IsLockButton o, MIO.MonadIO m, Gio.Permission.IsPermission a) => a -> m (GValueConstruct o)
constructLockButtonPermission val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "permission" (P.Just val)

-- | Set the value of the “@permission@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #permission
-- @
clearLockButtonPermission :: (MonadIO m, IsLockButton o) => o -> m ()
clearLockButtonPermission obj = liftIO $ B.Properties.setObjectPropertyObject obj "permission" (Nothing :: Maybe Gio.Permission.Permission)

#if defined(ENABLE_OVERLOADING)
data LockButtonPermissionPropertyInfo
instance AttrInfo LockButtonPermissionPropertyInfo where
    type AttrAllowedOps LockButtonPermissionPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint LockButtonPermissionPropertyInfo = IsLockButton
    type AttrSetTypeConstraint LockButtonPermissionPropertyInfo = Gio.Permission.IsPermission
    type AttrTransferTypeConstraint LockButtonPermissionPropertyInfo = Gio.Permission.IsPermission
    type AttrTransferType LockButtonPermissionPropertyInfo = Gio.Permission.Permission
    type AttrGetType LockButtonPermissionPropertyInfo = Gio.Permission.Permission
    type AttrLabel LockButtonPermissionPropertyInfo = "permission"
    type AttrOrigin LockButtonPermissionPropertyInfo = LockButton
    attrGet = getLockButtonPermission
    attrSet = setLockButtonPermission
    attrTransfer _ v = do
        unsafeCastTo Gio.Permission.Permission v
    attrConstruct = constructLockButtonPermission
    attrClear = clearLockButtonPermission
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.LockButton.permission"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-LockButton.html#g:attr:permission"
        })
#endif

-- VVV Prop "text-lock"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstruct]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@text-lock@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' lockButton #textLock
-- @
getLockButtonTextLock :: (MonadIO m, IsLockButton o) => o -> m (Maybe T.Text)
getLockButtonTextLock obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "text-lock"

-- | Set the value of the “@text-lock@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' lockButton [ #textLock 'Data.GI.Base.Attributes.:=' value ]
-- @
setLockButtonTextLock :: (MonadIO m, IsLockButton o) => o -> T.Text -> m ()
setLockButtonTextLock obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "text-lock" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@text-lock@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructLockButtonTextLock :: (IsLockButton o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructLockButtonTextLock val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "text-lock" (P.Just val)

-- | Set the value of the “@text-lock@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #textLock
-- @
clearLockButtonTextLock :: (MonadIO m, IsLockButton o) => o -> m ()
clearLockButtonTextLock obj = liftIO $ B.Properties.setObjectPropertyString obj "text-lock" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data LockButtonTextLockPropertyInfo
instance AttrInfo LockButtonTextLockPropertyInfo where
    type AttrAllowedOps LockButtonTextLockPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint LockButtonTextLockPropertyInfo = IsLockButton
    type AttrSetTypeConstraint LockButtonTextLockPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint LockButtonTextLockPropertyInfo = (~) T.Text
    type AttrTransferType LockButtonTextLockPropertyInfo = T.Text
    type AttrGetType LockButtonTextLockPropertyInfo = (Maybe T.Text)
    type AttrLabel LockButtonTextLockPropertyInfo = "text-lock"
    type AttrOrigin LockButtonTextLockPropertyInfo = LockButton
    attrGet = getLockButtonTextLock
    attrSet = setLockButtonTextLock
    attrTransfer _ v = do
        return v
    attrConstruct = constructLockButtonTextLock
    attrClear = clearLockButtonTextLock
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.LockButton.textLock"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-LockButton.html#g:attr:textLock"
        })
#endif

-- VVV Prop "text-unlock"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstruct]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@text-unlock@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' lockButton #textUnlock
-- @
getLockButtonTextUnlock :: (MonadIO m, IsLockButton o) => o -> m (Maybe T.Text)
getLockButtonTextUnlock obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "text-unlock"

-- | Set the value of the “@text-unlock@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' lockButton [ #textUnlock 'Data.GI.Base.Attributes.:=' value ]
-- @
setLockButtonTextUnlock :: (MonadIO m, IsLockButton o) => o -> T.Text -> m ()
setLockButtonTextUnlock obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "text-unlock" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@text-unlock@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructLockButtonTextUnlock :: (IsLockButton o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructLockButtonTextUnlock val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "text-unlock" (P.Just val)

-- | Set the value of the “@text-unlock@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #textUnlock
-- @
clearLockButtonTextUnlock :: (MonadIO m, IsLockButton o) => o -> m ()
clearLockButtonTextUnlock obj = liftIO $ B.Properties.setObjectPropertyString obj "text-unlock" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data LockButtonTextUnlockPropertyInfo
instance AttrInfo LockButtonTextUnlockPropertyInfo where
    type AttrAllowedOps LockButtonTextUnlockPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint LockButtonTextUnlockPropertyInfo = IsLockButton
    type AttrSetTypeConstraint LockButtonTextUnlockPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint LockButtonTextUnlockPropertyInfo = (~) T.Text
    type AttrTransferType LockButtonTextUnlockPropertyInfo = T.Text
    type AttrGetType LockButtonTextUnlockPropertyInfo = (Maybe T.Text)
    type AttrLabel LockButtonTextUnlockPropertyInfo = "text-unlock"
    type AttrOrigin LockButtonTextUnlockPropertyInfo = LockButton
    attrGet = getLockButtonTextUnlock
    attrSet = setLockButtonTextUnlock
    attrTransfer _ v = do
        return v
    attrConstruct = constructLockButtonTextUnlock
    attrClear = clearLockButtonTextUnlock
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.LockButton.textUnlock"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-LockButton.html#g:attr:textUnlock"
        })
#endif

-- VVV Prop "tooltip-lock"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstruct]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@tooltip-lock@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' lockButton #tooltipLock
-- @
getLockButtonTooltipLock :: (MonadIO m, IsLockButton o) => o -> m (Maybe T.Text)
getLockButtonTooltipLock obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "tooltip-lock"

-- | Set the value of the “@tooltip-lock@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' lockButton [ #tooltipLock 'Data.GI.Base.Attributes.:=' value ]
-- @
setLockButtonTooltipLock :: (MonadIO m, IsLockButton o) => o -> T.Text -> m ()
setLockButtonTooltipLock obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "tooltip-lock" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@tooltip-lock@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructLockButtonTooltipLock :: (IsLockButton o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructLockButtonTooltipLock val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "tooltip-lock" (P.Just val)

-- | Set the value of the “@tooltip-lock@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #tooltipLock
-- @
clearLockButtonTooltipLock :: (MonadIO m, IsLockButton o) => o -> m ()
clearLockButtonTooltipLock obj = liftIO $ B.Properties.setObjectPropertyString obj "tooltip-lock" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data LockButtonTooltipLockPropertyInfo
instance AttrInfo LockButtonTooltipLockPropertyInfo where
    type AttrAllowedOps LockButtonTooltipLockPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint LockButtonTooltipLockPropertyInfo = IsLockButton
    type AttrSetTypeConstraint LockButtonTooltipLockPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint LockButtonTooltipLockPropertyInfo = (~) T.Text
    type AttrTransferType LockButtonTooltipLockPropertyInfo = T.Text
    type AttrGetType LockButtonTooltipLockPropertyInfo = (Maybe T.Text)
    type AttrLabel LockButtonTooltipLockPropertyInfo = "tooltip-lock"
    type AttrOrigin LockButtonTooltipLockPropertyInfo = LockButton
    attrGet = getLockButtonTooltipLock
    attrSet = setLockButtonTooltipLock
    attrTransfer _ v = do
        return v
    attrConstruct = constructLockButtonTooltipLock
    attrClear = clearLockButtonTooltipLock
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.LockButton.tooltipLock"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-LockButton.html#g:attr:tooltipLock"
        })
#endif

-- VVV Prop "tooltip-not-authorized"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstruct]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@tooltip-not-authorized@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' lockButton #tooltipNotAuthorized
-- @
getLockButtonTooltipNotAuthorized :: (MonadIO m, IsLockButton o) => o -> m (Maybe T.Text)
getLockButtonTooltipNotAuthorized obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "tooltip-not-authorized"

-- | Set the value of the “@tooltip-not-authorized@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' lockButton [ #tooltipNotAuthorized 'Data.GI.Base.Attributes.:=' value ]
-- @
setLockButtonTooltipNotAuthorized :: (MonadIO m, IsLockButton o) => o -> T.Text -> m ()
setLockButtonTooltipNotAuthorized obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "tooltip-not-authorized" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@tooltip-not-authorized@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructLockButtonTooltipNotAuthorized :: (IsLockButton o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructLockButtonTooltipNotAuthorized val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "tooltip-not-authorized" (P.Just val)

-- | Set the value of the “@tooltip-not-authorized@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #tooltipNotAuthorized
-- @
clearLockButtonTooltipNotAuthorized :: (MonadIO m, IsLockButton o) => o -> m ()
clearLockButtonTooltipNotAuthorized obj = liftIO $ B.Properties.setObjectPropertyString obj "tooltip-not-authorized" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data LockButtonTooltipNotAuthorizedPropertyInfo
instance AttrInfo LockButtonTooltipNotAuthorizedPropertyInfo where
    type AttrAllowedOps LockButtonTooltipNotAuthorizedPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint LockButtonTooltipNotAuthorizedPropertyInfo = IsLockButton
    type AttrSetTypeConstraint LockButtonTooltipNotAuthorizedPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint LockButtonTooltipNotAuthorizedPropertyInfo = (~) T.Text
    type AttrTransferType LockButtonTooltipNotAuthorizedPropertyInfo = T.Text
    type AttrGetType LockButtonTooltipNotAuthorizedPropertyInfo = (Maybe T.Text)
    type AttrLabel LockButtonTooltipNotAuthorizedPropertyInfo = "tooltip-not-authorized"
    type AttrOrigin LockButtonTooltipNotAuthorizedPropertyInfo = LockButton
    attrGet = getLockButtonTooltipNotAuthorized
    attrSet = setLockButtonTooltipNotAuthorized
    attrTransfer _ v = do
        return v
    attrConstruct = constructLockButtonTooltipNotAuthorized
    attrClear = clearLockButtonTooltipNotAuthorized
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.LockButton.tooltipNotAuthorized"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-LockButton.html#g:attr:tooltipNotAuthorized"
        })
#endif

-- VVV Prop "tooltip-unlock"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstruct]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@tooltip-unlock@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' lockButton #tooltipUnlock
-- @
getLockButtonTooltipUnlock :: (MonadIO m, IsLockButton o) => o -> m (Maybe T.Text)
getLockButtonTooltipUnlock obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "tooltip-unlock"

-- | Set the value of the “@tooltip-unlock@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' lockButton [ #tooltipUnlock 'Data.GI.Base.Attributes.:=' value ]
-- @
setLockButtonTooltipUnlock :: (MonadIO m, IsLockButton o) => o -> T.Text -> m ()
setLockButtonTooltipUnlock obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "tooltip-unlock" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@tooltip-unlock@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructLockButtonTooltipUnlock :: (IsLockButton o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructLockButtonTooltipUnlock val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "tooltip-unlock" (P.Just val)

-- | Set the value of the “@tooltip-unlock@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #tooltipUnlock
-- @
clearLockButtonTooltipUnlock :: (MonadIO m, IsLockButton o) => o -> m ()
clearLockButtonTooltipUnlock obj = liftIO $ B.Properties.setObjectPropertyString obj "tooltip-unlock" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data LockButtonTooltipUnlockPropertyInfo
instance AttrInfo LockButtonTooltipUnlockPropertyInfo where
    type AttrAllowedOps LockButtonTooltipUnlockPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint LockButtonTooltipUnlockPropertyInfo = IsLockButton
    type AttrSetTypeConstraint LockButtonTooltipUnlockPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint LockButtonTooltipUnlockPropertyInfo = (~) T.Text
    type AttrTransferType LockButtonTooltipUnlockPropertyInfo = T.Text
    type AttrGetType LockButtonTooltipUnlockPropertyInfo = (Maybe T.Text)
    type AttrLabel LockButtonTooltipUnlockPropertyInfo = "tooltip-unlock"
    type AttrOrigin LockButtonTooltipUnlockPropertyInfo = LockButton
    attrGet = getLockButtonTooltipUnlock
    attrSet = setLockButtonTooltipUnlock
    attrTransfer _ v = do
        return v
    attrConstruct = constructLockButtonTooltipUnlock
    attrClear = clearLockButtonTooltipUnlock
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.LockButton.tooltipUnlock"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-LockButton.html#g:attr:tooltipUnlock"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList LockButton
type instance O.AttributeList LockButton = LockButtonAttributeList
type LockButtonAttributeList = ('[ '("actionName", Gtk.Actionable.ActionableActionNamePropertyInfo), '("actionTarget", Gtk.Actionable.ActionableActionTargetPropertyInfo), '("alwaysShowImage", Gtk.Button.ButtonAlwaysShowImagePropertyInfo), '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("image", Gtk.Button.ButtonImagePropertyInfo), '("imagePosition", Gtk.Button.ButtonImagePositionPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("label", Gtk.Button.ButtonLabelPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("permission", LockButtonPermissionPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("relatedAction", Gtk.Activatable.ActivatableRelatedActionPropertyInfo), '("relief", Gtk.Button.ButtonReliefPropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("textLock", LockButtonTextLockPropertyInfo), '("textUnlock", LockButtonTextUnlockPropertyInfo), '("tooltipLock", LockButtonTooltipLockPropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipNotAuthorized", LockButtonTooltipNotAuthorizedPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("tooltipUnlock", LockButtonTooltipUnlockPropertyInfo), '("useActionAppearance", Gtk.Activatable.ActivatableUseActionAppearancePropertyInfo), '("useStock", Gtk.Button.ButtonUseStockPropertyInfo), '("useUnderline", Gtk.Button.ButtonUseUnderlinePropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo), '("xalign", Gtk.Button.ButtonXalignPropertyInfo), '("yalign", Gtk.Button.ButtonYalignPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
lockButtonPermission :: AttrLabelProxy "permission"
lockButtonPermission = AttrLabelProxy

lockButtonTextLock :: AttrLabelProxy "textLock"
lockButtonTextLock = AttrLabelProxy

lockButtonTextUnlock :: AttrLabelProxy "textUnlock"
lockButtonTextUnlock = AttrLabelProxy

lockButtonTooltipLock :: AttrLabelProxy "tooltipLock"
lockButtonTooltipLock = AttrLabelProxy

lockButtonTooltipNotAuthorized :: AttrLabelProxy "tooltipNotAuthorized"
lockButtonTooltipNotAuthorized = AttrLabelProxy

lockButtonTooltipUnlock :: AttrLabelProxy "tooltipUnlock"
lockButtonTooltipUnlock = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList LockButton = LockButtonSignalList
type LockButtonSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("activate", Gtk.Button.ButtonActivateSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("clicked", Gtk.Button.ButtonClickedSignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enter", Gtk.Button.ButtonEnterSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leave", Gtk.Button.ButtonLeaveSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("pressed", Gtk.Button.ButtonPressedSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("released", Gtk.Button.ButtonReleasedSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method LockButton::new
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "permission"
--           , argType =
--               TInterface Name { namespace = "Gio" , name = "Permission" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GPermission" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "LockButton" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_lock_button_new" gtk_lock_button_new :: 
    Ptr Gio.Permission.Permission ->        -- permission : TInterface (Name {namespace = "Gio", name = "Permission"})
    IO (Ptr LockButton)

-- | Creates a new lock button which reflects the /@permission@/.
-- 
-- /Since: 3.2/
lockButtonNew ::
    (B.CallStack.HasCallStack, MonadIO m, Gio.Permission.IsPermission a) =>
    Maybe (a)
    -- ^ /@permission@/: a t'GI.Gio.Objects.Permission.Permission'
    -> m LockButton
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.LockButton.LockButton'
lockButtonNew permission = liftIO $ do
    maybePermission <- case permission of
        Nothing -> return nullPtr
        Just jPermission -> do
            jPermission' <- unsafeManagedPtrCastPtr jPermission
            return jPermission'
    result <- gtk_lock_button_new maybePermission
    checkUnexpectedReturnNULL "lockButtonNew" result
    result' <- (newObject LockButton) result
    whenJust permission touchManagedPtr
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method LockButton::get_permission
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "LockButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLockButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gio" , name = "Permission" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_lock_button_get_permission" gtk_lock_button_get_permission :: 
    Ptr LockButton ->                       -- button : TInterface (Name {namespace = "Gtk", name = "LockButton"})
    IO (Ptr Gio.Permission.Permission)

-- | Obtains the t'GI.Gio.Objects.Permission.Permission' object that controls /@button@/.
-- 
-- /Since: 3.2/
lockButtonGetPermission ::
    (B.CallStack.HasCallStack, MonadIO m, IsLockButton a) =>
    a
    -- ^ /@button@/: a t'GI.Gtk.Objects.LockButton.LockButton'
    -> m Gio.Permission.Permission
    -- ^ __Returns:__ the t'GI.Gio.Objects.Permission.Permission' of /@button@/
lockButtonGetPermission button = liftIO $ do
    button' <- unsafeManagedPtrCastPtr button
    result <- gtk_lock_button_get_permission button'
    checkUnexpectedReturnNULL "lockButtonGetPermission" result
    result' <- (newObject Gio.Permission.Permission) result
    touchManagedPtr button
    return result'

#if defined(ENABLE_OVERLOADING)
data LockButtonGetPermissionMethodInfo
instance (signature ~ (m Gio.Permission.Permission), MonadIO m, IsLockButton a) => O.OverloadedMethod LockButtonGetPermissionMethodInfo a signature where
    overloadedMethod = lockButtonGetPermission

instance O.OverloadedMethodInfo LockButtonGetPermissionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.LockButton.lockButtonGetPermission",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-LockButton.html#v:lockButtonGetPermission"
        })


#endif

-- method LockButton::set_permission
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "LockButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLockButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "permission"
--           , argType =
--               TInterface Name { namespace = "Gio" , name = "Permission" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GPermission object, or %NULL"
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

foreign import ccall "gtk_lock_button_set_permission" gtk_lock_button_set_permission :: 
    Ptr LockButton ->                       -- button : TInterface (Name {namespace = "Gtk", name = "LockButton"})
    Ptr Gio.Permission.Permission ->        -- permission : TInterface (Name {namespace = "Gio", name = "Permission"})
    IO ()

-- | Sets the t'GI.Gio.Objects.Permission.Permission' object that controls /@button@/.
-- 
-- /Since: 3.2/
lockButtonSetPermission ::
    (B.CallStack.HasCallStack, MonadIO m, IsLockButton a, Gio.Permission.IsPermission b) =>
    a
    -- ^ /@button@/: a t'GI.Gtk.Objects.LockButton.LockButton'
    -> Maybe (b)
    -- ^ /@permission@/: a t'GI.Gio.Objects.Permission.Permission' object, or 'P.Nothing'
    -> m ()
lockButtonSetPermission button permission = liftIO $ do
    button' <- unsafeManagedPtrCastPtr button
    maybePermission <- case permission of
        Nothing -> return nullPtr
        Just jPermission -> do
            jPermission' <- unsafeManagedPtrCastPtr jPermission
            return jPermission'
    gtk_lock_button_set_permission button' maybePermission
    touchManagedPtr button
    whenJust permission touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data LockButtonSetPermissionMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsLockButton a, Gio.Permission.IsPermission b) => O.OverloadedMethod LockButtonSetPermissionMethodInfo a signature where
    overloadedMethod = lockButtonSetPermission

instance O.OverloadedMethodInfo LockButtonSetPermissionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.LockButton.lockButtonSetPermission",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-LockButton.html#v:lockButtonSetPermission"
        })


#endif


