{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- The t'GI.Gtk.Objects.MenuButton.MenuButton' widget is used to display a popup when clicked on.
-- This popup can be provided either as a t'GI.Gtk.Objects.Menu.Menu', a t'GI.Gtk.Objects.Popover.Popover' or an
-- abstract t'GI.Gio.Objects.MenuModel.MenuModel'.
-- 
-- The t'GI.Gtk.Objects.MenuButton.MenuButton' widget can hold any valid child widget. That is, it
-- can hold almost any other standard t'GI.Gtk.Objects.Widget.Widget'. The most commonly used
-- child is t'GI.Gtk.Objects.Image.Image'. If no widget is explicitely added to the t'GI.Gtk.Objects.MenuButton.MenuButton',
-- a t'GI.Gtk.Objects.Image.Image' is automatically created, using an arrow image oriented
-- according to [MenuButton:direction]("GI.Gtk.Objects.MenuButton#g:attr:direction") or the generic “open-menu-symbolic”
-- icon if the direction is not set.
-- 
-- The positioning of the popup is determined by the [MenuButton:direction]("GI.Gtk.Objects.MenuButton#g:attr:direction")
-- property of the menu button.
-- 
-- For menus, the [Widget:halign]("GI.Gtk.Objects.Widget#g:attr:halign") and [Widget:valign]("GI.Gtk.Objects.Widget#g:attr:valign") properties of the
-- menu are also taken into account. For example, when the direction is
-- 'GI.Gtk.Enums.ArrowTypeDown' and the horizontal alignment is 'GI.Gtk.Enums.AlignStart', the
-- menu will be positioned below the button, with the starting edge
-- (depending on the text direction) of the menu aligned with the starting
-- edge of the button. If there is not enough space below the button, the
-- menu is popped up above the button instead. If the alignment would move
-- part of the menu offscreen, it is “pushed in”.
-- 
-- == Direction = Down
-- 
-- 
-- * halign = start
-- 
-- 
--     <<https://developer.gnome.org/gtk3/stable/down-start.png>>
-- 
-- * halign = center
-- 
-- 
--     <<https://developer.gnome.org/gtk3/stable/down-center.png>>
-- 
-- * halign = end
-- 
-- 
--     <<https://developer.gnome.org/gtk3/stable/down-end.png>>
-- 
-- == Direction = Up
-- 
-- 
-- * halign = start
-- 
-- 
--     <<https://developer.gnome.org/gtk3/stable/up-start.png>>
-- 
-- * halign = center
-- 
-- 
--     <<https://developer.gnome.org/gtk3/stable/up-center.png>>
-- 
-- * halign = end
-- 
-- 
--     <<https://developer.gnome.org/gtk3/stable/up-end.png>>
-- 
-- == Direction = Left
-- 
-- 
-- * valign = start
-- 
-- 
--     <<https://developer.gnome.org/gtk3/stable/left-start.png>>
-- 
-- * valign = center
-- 
-- 
--     <<https://developer.gnome.org/gtk3/stable/left-center.png>>
-- 
-- * valign = end
-- 
-- 
--     <<https://developer.gnome.org/gtk3/stable/left-end.png>>
-- 
-- == Direction = Right
-- 
-- 
-- * valign = start
-- 
-- 
--     <<https://developer.gnome.org/gtk3/stable/right-start.png>>
-- 
-- * valign = center
-- 
-- 
--     <<https://developer.gnome.org/gtk3/stable/right-center.png>>
-- 
-- * valign = end
-- 
-- 
--     <<https://developer.gnome.org/gtk3/stable/right-end.png>>
-- 
-- = CSS nodes
-- 
-- GtkMenuButton has a single CSS node with name button. To differentiate
-- it from a plain t'GI.Gtk.Objects.Button.Button', it gets the .popup style class.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.MenuButton
    ( 

-- * Exported types
    MenuButton(..)                          ,
    IsMenuButton                            ,
    toMenuButton                            ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [clicked]("GI.Gtk.Objects.Button#g:method:clicked"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [doSetRelatedAction]("GI.Gtk.Interfaces.Activatable#g:method:doSetRelatedAction"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [enter]("GI.Gtk.Objects.Button#g:method:enter"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [leave]("GI.Gtk.Objects.Button#g:method:leave"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [pressed]("GI.Gtk.Objects.Button#g:method:pressed"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [released]("GI.Gtk.Objects.Button#g:method:released"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [syncActionProperties]("GI.Gtk.Interfaces.Activatable#g:method:syncActionProperties"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [toggled]("GI.Gtk.Objects.ToggleButton#g:method:toggled"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getActionName]("GI.Gtk.Interfaces.Actionable#g:method:getActionName"), [getActionTargetValue]("GI.Gtk.Interfaces.Actionable#g:method:getActionTargetValue"), [getActive]("GI.Gtk.Objects.ToggleButton#g:method:getActive"), [getAlignWidget]("GI.Gtk.Objects.MenuButton#g:method:getAlignWidget"), [getAlignment]("GI.Gtk.Objects.Button#g:method:getAlignment"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAlwaysShowImage]("GI.Gtk.Objects.Button#g:method:getAlwaysShowImage"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChild]("GI.Gtk.Objects.Bin#g:method:getChild"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.MenuButton#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEventWindow]("GI.Gtk.Objects.Button#g:method:getEventWindow"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Button#g:method:getFocusOnClick"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getImage]("GI.Gtk.Objects.Button#g:method:getImage"), [getImagePosition]("GI.Gtk.Objects.Button#g:method:getImagePosition"), [getInconsistent]("GI.Gtk.Objects.ToggleButton#g:method:getInconsistent"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getLabel]("GI.Gtk.Objects.Button#g:method:getLabel"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getMenuModel]("GI.Gtk.Objects.MenuButton#g:method:getMenuModel"), [getMode]("GI.Gtk.Objects.ToggleButton#g:method:getMode"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPopover]("GI.Gtk.Objects.MenuButton#g:method:getPopover"), [getPopup]("GI.Gtk.Objects.MenuButton#g:method:getPopup"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRelatedAction]("GI.Gtk.Interfaces.Activatable#g:method:getRelatedAction"), [getRelief]("GI.Gtk.Objects.Button#g:method:getRelief"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getUseActionAppearance]("GI.Gtk.Interfaces.Activatable#g:method:getUseActionAppearance"), [getUsePopover]("GI.Gtk.Objects.MenuButton#g:method:getUsePopover"), [getUseStock]("GI.Gtk.Objects.Button#g:method:getUseStock"), [getUseUnderline]("GI.Gtk.Objects.Button#g:method:getUseUnderline"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setActionName]("GI.Gtk.Interfaces.Actionable#g:method:setActionName"), [setActionTargetValue]("GI.Gtk.Interfaces.Actionable#g:method:setActionTargetValue"), [setActive]("GI.Gtk.Objects.ToggleButton#g:method:setActive"), [setAlignWidget]("GI.Gtk.Objects.MenuButton#g:method:setAlignWidget"), [setAlignment]("GI.Gtk.Objects.Button#g:method:setAlignment"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAlwaysShowImage]("GI.Gtk.Objects.Button#g:method:setAlwaysShowImage"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDetailedActionName]("GI.Gtk.Interfaces.Actionable#g:method:setDetailedActionName"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.MenuButton#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Button#g:method:setFocusOnClick"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setImage]("GI.Gtk.Objects.Button#g:method:setImage"), [setImagePosition]("GI.Gtk.Objects.Button#g:method:setImagePosition"), [setInconsistent]("GI.Gtk.Objects.ToggleButton#g:method:setInconsistent"), [setLabel]("GI.Gtk.Objects.Button#g:method:setLabel"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setMenuModel]("GI.Gtk.Objects.MenuButton#g:method:setMenuModel"), [setMode]("GI.Gtk.Objects.ToggleButton#g:method:setMode"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setPopover]("GI.Gtk.Objects.MenuButton#g:method:setPopover"), [setPopup]("GI.Gtk.Objects.MenuButton#g:method:setPopup"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setRelatedAction]("GI.Gtk.Interfaces.Activatable#g:method:setRelatedAction"), [setRelief]("GI.Gtk.Objects.Button#g:method:setRelief"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setUseActionAppearance]("GI.Gtk.Interfaces.Activatable#g:method:setUseActionAppearance"), [setUsePopover]("GI.Gtk.Objects.MenuButton#g:method:setUsePopover"), [setUseStock]("GI.Gtk.Objects.Button#g:method:setUseStock"), [setUseUnderline]("GI.Gtk.Objects.Button#g:method:setUseUnderline"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveMenuButtonMethod                 ,
#endif

-- ** getAlignWidget #method:getAlignWidget#

#if defined(ENABLE_OVERLOADING)
    MenuButtonGetAlignWidgetMethodInfo      ,
#endif
    menuButtonGetAlignWidget                ,


-- ** getDirection #method:getDirection#

#if defined(ENABLE_OVERLOADING)
    MenuButtonGetDirectionMethodInfo        ,
#endif
    menuButtonGetDirection                  ,


-- ** getMenuModel #method:getMenuModel#

#if defined(ENABLE_OVERLOADING)
    MenuButtonGetMenuModelMethodInfo        ,
#endif
    menuButtonGetMenuModel                  ,


-- ** getPopover #method:getPopover#

#if defined(ENABLE_OVERLOADING)
    MenuButtonGetPopoverMethodInfo          ,
#endif
    menuButtonGetPopover                    ,


-- ** getPopup #method:getPopup#

#if defined(ENABLE_OVERLOADING)
    MenuButtonGetPopupMethodInfo            ,
#endif
    menuButtonGetPopup                      ,


-- ** getUsePopover #method:getUsePopover#

#if defined(ENABLE_OVERLOADING)
    MenuButtonGetUsePopoverMethodInfo       ,
#endif
    menuButtonGetUsePopover                 ,


-- ** new #method:new#

    menuButtonNew                           ,


-- ** setAlignWidget #method:setAlignWidget#

#if defined(ENABLE_OVERLOADING)
    MenuButtonSetAlignWidgetMethodInfo      ,
#endif
    menuButtonSetAlignWidget                ,


-- ** setDirection #method:setDirection#

#if defined(ENABLE_OVERLOADING)
    MenuButtonSetDirectionMethodInfo        ,
#endif
    menuButtonSetDirection                  ,


-- ** setMenuModel #method:setMenuModel#

#if defined(ENABLE_OVERLOADING)
    MenuButtonSetMenuModelMethodInfo        ,
#endif
    menuButtonSetMenuModel                  ,


-- ** setPopover #method:setPopover#

#if defined(ENABLE_OVERLOADING)
    MenuButtonSetPopoverMethodInfo          ,
#endif
    menuButtonSetPopover                    ,


-- ** setPopup #method:setPopup#

#if defined(ENABLE_OVERLOADING)
    MenuButtonSetPopupMethodInfo            ,
#endif
    menuButtonSetPopup                      ,


-- ** setUsePopover #method:setUsePopover#

#if defined(ENABLE_OVERLOADING)
    MenuButtonSetUsePopoverMethodInfo       ,
#endif
    menuButtonSetUsePopover                 ,




 -- * Properties


-- ** alignWidget #attr:alignWidget#
-- | The t'GI.Gtk.Objects.Widget.Widget' to use to align the menu with.
-- 
-- /Since: 3.6/

#if defined(ENABLE_OVERLOADING)
    MenuButtonAlignWidgetPropertyInfo       ,
#endif
    clearMenuButtonAlignWidget              ,
    constructMenuButtonAlignWidget          ,
    getMenuButtonAlignWidget                ,
#if defined(ENABLE_OVERLOADING)
    menuButtonAlignWidget                   ,
#endif
    setMenuButtonAlignWidget                ,


-- ** direction #attr:direction#
-- | The t'GI.Gtk.Enums.ArrowType' representing the direction in which the
-- menu or popover will be popped out.
-- 
-- /Since: 3.6/

#if defined(ENABLE_OVERLOADING)
    MenuButtonDirectionPropertyInfo         ,
#endif
    constructMenuButtonDirection            ,
    getMenuButtonDirection                  ,
#if defined(ENABLE_OVERLOADING)
    menuButtonDirection                     ,
#endif
    setMenuButtonDirection                  ,


-- ** menuModel #attr:menuModel#
-- | The t'GI.Gio.Objects.MenuModel.MenuModel' from which the popup will be created.
-- Depending on the [MenuButton:usePopover]("GI.Gtk.Objects.MenuButton#g:attr:usePopover") property, that may
-- be a menu or a popover.
-- 
-- See 'GI.Gtk.Objects.MenuButton.menuButtonSetMenuModel' for the interaction with the
-- [MenuButton:popup]("GI.Gtk.Objects.MenuButton#g:attr:popup") property.
-- 
-- /Since: 3.6/

#if defined(ENABLE_OVERLOADING)
    MenuButtonMenuModelPropertyInfo         ,
#endif
    clearMenuButtonMenuModel                ,
    constructMenuButtonMenuModel            ,
    getMenuButtonMenuModel                  ,
#if defined(ENABLE_OVERLOADING)
    menuButtonMenuModel                     ,
#endif
    setMenuButtonMenuModel                  ,


-- ** popover #attr:popover#
-- | The t'GI.Gtk.Objects.Popover.Popover' that will be popped up when the button is clicked.
-- 
-- /Since: 3.12/

#if defined(ENABLE_OVERLOADING)
    MenuButtonPopoverPropertyInfo           ,
#endif
    clearMenuButtonPopover                  ,
    constructMenuButtonPopover              ,
    getMenuButtonPopover                    ,
#if defined(ENABLE_OVERLOADING)
    menuButtonPopover                       ,
#endif
    setMenuButtonPopover                    ,


-- ** popup #attr:popup#
-- | The t'GI.Gtk.Objects.Menu.Menu' that will be popped up when the button is clicked.
-- 
-- /Since: 3.6/

#if defined(ENABLE_OVERLOADING)
    MenuButtonPopupPropertyInfo             ,
#endif
    clearMenuButtonPopup                    ,
    constructMenuButtonPopup                ,
    getMenuButtonPopup                      ,
#if defined(ENABLE_OVERLOADING)
    menuButtonPopup                         ,
#endif
    setMenuButtonPopup                      ,


-- ** usePopover #attr:usePopover#
-- | Whether to construct a t'GI.Gtk.Objects.Popover.Popover' from the menu model,
-- or a t'GI.Gtk.Objects.Menu.Menu'.
-- 
-- /Since: 3.12/

#if defined(ENABLE_OVERLOADING)
    MenuButtonUsePopoverPropertyInfo        ,
#endif
    constructMenuButtonUsePopover           ,
    getMenuButtonUsePopover                 ,
#if defined(ENABLE_OVERLOADING)
    menuButtonUsePopover                    ,
#endif
    setMenuButtonUsePopover                 ,




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
import qualified GI.Gio.Objects.MenuModel as Gio.MenuModel
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Actionable as Gtk.Actionable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Activatable as Gtk.Activatable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Objects.Bin as Gtk.Bin
import {-# SOURCE #-} qualified GI.Gtk.Objects.Button as Gtk.Button
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.Menu as Gtk.Menu
import {-# SOURCE #-} qualified GI.Gtk.Objects.Popover as Gtk.Popover
import {-# SOURCE #-} qualified GI.Gtk.Objects.ToggleButton as Gtk.ToggleButton
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype MenuButton = MenuButton (SP.ManagedPtr MenuButton)
    deriving (Eq)

instance SP.ManagedPtrNewtype MenuButton where
    toManagedPtr (MenuButton p) = p

foreign import ccall "gtk_menu_button_get_type"
    c_gtk_menu_button_get_type :: IO B.Types.GType

instance B.Types.TypedObject MenuButton where
    glibType = c_gtk_menu_button_get_type

instance B.Types.GObject MenuButton

-- | Type class for types which can be safely cast to `MenuButton`, for instance with `toMenuButton`.
class (SP.GObject o, O.IsDescendantOf MenuButton o) => IsMenuButton o
instance (SP.GObject o, O.IsDescendantOf MenuButton o) => IsMenuButton o

instance O.HasParentTypes MenuButton
type instance O.ParentTypes MenuButton = '[Gtk.ToggleButton.ToggleButton, Gtk.Button.Button, Gtk.Bin.Bin, Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Actionable.Actionable, Gtk.Activatable.Activatable, Gtk.Buildable.Buildable]

-- | Cast to `MenuButton`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toMenuButton :: (MIO.MonadIO m, IsMenuButton o) => o -> m MenuButton
toMenuButton = MIO.liftIO . B.ManagedPtr.unsafeCastTo MenuButton

-- | Convert 'MenuButton' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe MenuButton) where
    gvalueGType_ = c_gtk_menu_button_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr MenuButton)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr MenuButton)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject MenuButton ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveMenuButtonMethod (t :: Symbol) (o :: *) :: * where
    ResolveMenuButtonMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveMenuButtonMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveMenuButtonMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveMenuButtonMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveMenuButtonMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveMenuButtonMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveMenuButtonMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveMenuButtonMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveMenuButtonMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveMenuButtonMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveMenuButtonMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveMenuButtonMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveMenuButtonMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveMenuButtonMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveMenuButtonMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveMenuButtonMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveMenuButtonMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveMenuButtonMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveMenuButtonMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveMenuButtonMethod "clicked" o = Gtk.Button.ButtonClickedMethodInfo
    ResolveMenuButtonMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveMenuButtonMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveMenuButtonMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveMenuButtonMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveMenuButtonMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveMenuButtonMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveMenuButtonMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveMenuButtonMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveMenuButtonMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveMenuButtonMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveMenuButtonMethod "doSetRelatedAction" o = Gtk.Activatable.ActivatableDoSetRelatedActionMethodInfo
    ResolveMenuButtonMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveMenuButtonMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveMenuButtonMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveMenuButtonMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveMenuButtonMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveMenuButtonMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveMenuButtonMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveMenuButtonMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveMenuButtonMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveMenuButtonMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveMenuButtonMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveMenuButtonMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveMenuButtonMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveMenuButtonMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveMenuButtonMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveMenuButtonMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveMenuButtonMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveMenuButtonMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveMenuButtonMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveMenuButtonMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveMenuButtonMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveMenuButtonMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveMenuButtonMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveMenuButtonMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveMenuButtonMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveMenuButtonMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveMenuButtonMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveMenuButtonMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveMenuButtonMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveMenuButtonMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveMenuButtonMethod "enter" o = Gtk.Button.ButtonEnterMethodInfo
    ResolveMenuButtonMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveMenuButtonMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveMenuButtonMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveMenuButtonMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveMenuButtonMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveMenuButtonMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveMenuButtonMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveMenuButtonMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveMenuButtonMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveMenuButtonMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveMenuButtonMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveMenuButtonMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveMenuButtonMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveMenuButtonMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveMenuButtonMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveMenuButtonMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveMenuButtonMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveMenuButtonMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveMenuButtonMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveMenuButtonMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveMenuButtonMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveMenuButtonMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveMenuButtonMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveMenuButtonMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveMenuButtonMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveMenuButtonMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveMenuButtonMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveMenuButtonMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveMenuButtonMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveMenuButtonMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveMenuButtonMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveMenuButtonMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveMenuButtonMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveMenuButtonMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveMenuButtonMethod "leave" o = Gtk.Button.ButtonLeaveMethodInfo
    ResolveMenuButtonMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveMenuButtonMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveMenuButtonMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveMenuButtonMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveMenuButtonMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveMenuButtonMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveMenuButtonMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveMenuButtonMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveMenuButtonMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveMenuButtonMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveMenuButtonMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveMenuButtonMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveMenuButtonMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveMenuButtonMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveMenuButtonMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveMenuButtonMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveMenuButtonMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveMenuButtonMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveMenuButtonMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveMenuButtonMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveMenuButtonMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveMenuButtonMethod "pressed" o = Gtk.Button.ButtonPressedMethodInfo
    ResolveMenuButtonMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveMenuButtonMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveMenuButtonMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveMenuButtonMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveMenuButtonMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveMenuButtonMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveMenuButtonMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveMenuButtonMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveMenuButtonMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveMenuButtonMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveMenuButtonMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveMenuButtonMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveMenuButtonMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveMenuButtonMethod "released" o = Gtk.Button.ButtonReleasedMethodInfo
    ResolveMenuButtonMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolveMenuButtonMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveMenuButtonMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveMenuButtonMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveMenuButtonMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveMenuButtonMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveMenuButtonMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveMenuButtonMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveMenuButtonMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveMenuButtonMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveMenuButtonMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveMenuButtonMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveMenuButtonMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveMenuButtonMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveMenuButtonMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveMenuButtonMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveMenuButtonMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveMenuButtonMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveMenuButtonMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveMenuButtonMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveMenuButtonMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveMenuButtonMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveMenuButtonMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveMenuButtonMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveMenuButtonMethod "syncActionProperties" o = Gtk.Activatable.ActivatableSyncActionPropertiesMethodInfo
    ResolveMenuButtonMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveMenuButtonMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveMenuButtonMethod "toggled" o = Gtk.ToggleButton.ToggleButtonToggledMethodInfo
    ResolveMenuButtonMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveMenuButtonMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveMenuButtonMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveMenuButtonMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveMenuButtonMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveMenuButtonMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveMenuButtonMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveMenuButtonMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveMenuButtonMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveMenuButtonMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveMenuButtonMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveMenuButtonMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveMenuButtonMethod "getActionName" o = Gtk.Actionable.ActionableGetActionNameMethodInfo
    ResolveMenuButtonMethod "getActionTargetValue" o = Gtk.Actionable.ActionableGetActionTargetValueMethodInfo
    ResolveMenuButtonMethod "getActive" o = Gtk.ToggleButton.ToggleButtonGetActiveMethodInfo
    ResolveMenuButtonMethod "getAlignWidget" o = MenuButtonGetAlignWidgetMethodInfo
    ResolveMenuButtonMethod "getAlignment" o = Gtk.Button.ButtonGetAlignmentMethodInfo
    ResolveMenuButtonMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveMenuButtonMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveMenuButtonMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveMenuButtonMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveMenuButtonMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveMenuButtonMethod "getAlwaysShowImage" o = Gtk.Button.ButtonGetAlwaysShowImageMethodInfo
    ResolveMenuButtonMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveMenuButtonMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveMenuButtonMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveMenuButtonMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveMenuButtonMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveMenuButtonMethod "getChild" o = Gtk.Bin.BinGetChildMethodInfo
    ResolveMenuButtonMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveMenuButtonMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveMenuButtonMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveMenuButtonMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveMenuButtonMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveMenuButtonMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveMenuButtonMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveMenuButtonMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveMenuButtonMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveMenuButtonMethod "getDirection" o = MenuButtonGetDirectionMethodInfo
    ResolveMenuButtonMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveMenuButtonMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveMenuButtonMethod "getEventWindow" o = Gtk.Button.ButtonGetEventWindowMethodInfo
    ResolveMenuButtonMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveMenuButtonMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveMenuButtonMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveMenuButtonMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveMenuButtonMethod "getFocusOnClick" o = Gtk.Button.ButtonGetFocusOnClickMethodInfo
    ResolveMenuButtonMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveMenuButtonMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveMenuButtonMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveMenuButtonMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveMenuButtonMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveMenuButtonMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveMenuButtonMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveMenuButtonMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveMenuButtonMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveMenuButtonMethod "getImage" o = Gtk.Button.ButtonGetImageMethodInfo
    ResolveMenuButtonMethod "getImagePosition" o = Gtk.Button.ButtonGetImagePositionMethodInfo
    ResolveMenuButtonMethod "getInconsistent" o = Gtk.ToggleButton.ToggleButtonGetInconsistentMethodInfo
    ResolveMenuButtonMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveMenuButtonMethod "getLabel" o = Gtk.Button.ButtonGetLabelMethodInfo
    ResolveMenuButtonMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveMenuButtonMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveMenuButtonMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveMenuButtonMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveMenuButtonMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveMenuButtonMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveMenuButtonMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveMenuButtonMethod "getMenuModel" o = MenuButtonGetMenuModelMethodInfo
    ResolveMenuButtonMethod "getMode" o = Gtk.ToggleButton.ToggleButtonGetModeMethodInfo
    ResolveMenuButtonMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveMenuButtonMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveMenuButtonMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveMenuButtonMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveMenuButtonMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveMenuButtonMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveMenuButtonMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveMenuButtonMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveMenuButtonMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveMenuButtonMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveMenuButtonMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveMenuButtonMethod "getPopover" o = MenuButtonGetPopoverMethodInfo
    ResolveMenuButtonMethod "getPopup" o = MenuButtonGetPopupMethodInfo
    ResolveMenuButtonMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveMenuButtonMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveMenuButtonMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveMenuButtonMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveMenuButtonMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveMenuButtonMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveMenuButtonMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveMenuButtonMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveMenuButtonMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveMenuButtonMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveMenuButtonMethod "getRelatedAction" o = Gtk.Activatable.ActivatableGetRelatedActionMethodInfo
    ResolveMenuButtonMethod "getRelief" o = Gtk.Button.ButtonGetReliefMethodInfo
    ResolveMenuButtonMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveMenuButtonMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveMenuButtonMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveMenuButtonMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveMenuButtonMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveMenuButtonMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveMenuButtonMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveMenuButtonMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveMenuButtonMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveMenuButtonMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveMenuButtonMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveMenuButtonMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveMenuButtonMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveMenuButtonMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveMenuButtonMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveMenuButtonMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveMenuButtonMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveMenuButtonMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveMenuButtonMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveMenuButtonMethod "getUseActionAppearance" o = Gtk.Activatable.ActivatableGetUseActionAppearanceMethodInfo
    ResolveMenuButtonMethod "getUsePopover" o = MenuButtonGetUsePopoverMethodInfo
    ResolveMenuButtonMethod "getUseStock" o = Gtk.Button.ButtonGetUseStockMethodInfo
    ResolveMenuButtonMethod "getUseUnderline" o = Gtk.Button.ButtonGetUseUnderlineMethodInfo
    ResolveMenuButtonMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveMenuButtonMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveMenuButtonMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveMenuButtonMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveMenuButtonMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveMenuButtonMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveMenuButtonMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveMenuButtonMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveMenuButtonMethod "setActionName" o = Gtk.Actionable.ActionableSetActionNameMethodInfo
    ResolveMenuButtonMethod "setActionTargetValue" o = Gtk.Actionable.ActionableSetActionTargetValueMethodInfo
    ResolveMenuButtonMethod "setActive" o = Gtk.ToggleButton.ToggleButtonSetActiveMethodInfo
    ResolveMenuButtonMethod "setAlignWidget" o = MenuButtonSetAlignWidgetMethodInfo
    ResolveMenuButtonMethod "setAlignment" o = Gtk.Button.ButtonSetAlignmentMethodInfo
    ResolveMenuButtonMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveMenuButtonMethod "setAlwaysShowImage" o = Gtk.Button.ButtonSetAlwaysShowImageMethodInfo
    ResolveMenuButtonMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveMenuButtonMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveMenuButtonMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveMenuButtonMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveMenuButtonMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveMenuButtonMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveMenuButtonMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveMenuButtonMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveMenuButtonMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveMenuButtonMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveMenuButtonMethod "setDetailedActionName" o = Gtk.Actionable.ActionableSetDetailedActionNameMethodInfo
    ResolveMenuButtonMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveMenuButtonMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveMenuButtonMethod "setDirection" o = MenuButtonSetDirectionMethodInfo
    ResolveMenuButtonMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveMenuButtonMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveMenuButtonMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveMenuButtonMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveMenuButtonMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveMenuButtonMethod "setFocusOnClick" o = Gtk.Button.ButtonSetFocusOnClickMethodInfo
    ResolveMenuButtonMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveMenuButtonMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveMenuButtonMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveMenuButtonMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveMenuButtonMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveMenuButtonMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveMenuButtonMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveMenuButtonMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveMenuButtonMethod "setImage" o = Gtk.Button.ButtonSetImageMethodInfo
    ResolveMenuButtonMethod "setImagePosition" o = Gtk.Button.ButtonSetImagePositionMethodInfo
    ResolveMenuButtonMethod "setInconsistent" o = Gtk.ToggleButton.ToggleButtonSetInconsistentMethodInfo
    ResolveMenuButtonMethod "setLabel" o = Gtk.Button.ButtonSetLabelMethodInfo
    ResolveMenuButtonMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveMenuButtonMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveMenuButtonMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveMenuButtonMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveMenuButtonMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveMenuButtonMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveMenuButtonMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveMenuButtonMethod "setMenuModel" o = MenuButtonSetMenuModelMethodInfo
    ResolveMenuButtonMethod "setMode" o = Gtk.ToggleButton.ToggleButtonSetModeMethodInfo
    ResolveMenuButtonMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveMenuButtonMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveMenuButtonMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveMenuButtonMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveMenuButtonMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveMenuButtonMethod "setPopover" o = MenuButtonSetPopoverMethodInfo
    ResolveMenuButtonMethod "setPopup" o = MenuButtonSetPopupMethodInfo
    ResolveMenuButtonMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveMenuButtonMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveMenuButtonMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveMenuButtonMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveMenuButtonMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveMenuButtonMethod "setRelatedAction" o = Gtk.Activatable.ActivatableSetRelatedActionMethodInfo
    ResolveMenuButtonMethod "setRelief" o = Gtk.Button.ButtonSetReliefMethodInfo
    ResolveMenuButtonMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveMenuButtonMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveMenuButtonMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveMenuButtonMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveMenuButtonMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveMenuButtonMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveMenuButtonMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveMenuButtonMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveMenuButtonMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveMenuButtonMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveMenuButtonMethod "setUseActionAppearance" o = Gtk.Activatable.ActivatableSetUseActionAppearanceMethodInfo
    ResolveMenuButtonMethod "setUsePopover" o = MenuButtonSetUsePopoverMethodInfo
    ResolveMenuButtonMethod "setUseStock" o = Gtk.Button.ButtonSetUseStockMethodInfo
    ResolveMenuButtonMethod "setUseUnderline" o = Gtk.Button.ButtonSetUseUnderlineMethodInfo
    ResolveMenuButtonMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveMenuButtonMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveMenuButtonMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveMenuButtonMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveMenuButtonMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveMenuButtonMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveMenuButtonMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveMenuButtonMethod t MenuButton, O.OverloadedMethod info MenuButton p) => OL.IsLabel t (MenuButton -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveMenuButtonMethod t MenuButton, O.OverloadedMethod info MenuButton p, R.HasField t MenuButton p) => R.HasField t MenuButton p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveMenuButtonMethod t MenuButton, O.OverloadedMethodInfo info MenuButton) => OL.IsLabel t (O.MethodProxy info MenuButton) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- VVV Prop "align-widget"
   -- Type: TInterface (Name {namespace = "Gtk", name = "Container"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@align-widget@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' menuButton #alignWidget
-- @
getMenuButtonAlignWidget :: (MonadIO m, IsMenuButton o) => o -> m (Maybe Gtk.Container.Container)
getMenuButtonAlignWidget obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "align-widget" Gtk.Container.Container

-- | Set the value of the “@align-widget@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' menuButton [ #alignWidget 'Data.GI.Base.Attributes.:=' value ]
-- @
setMenuButtonAlignWidget :: (MonadIO m, IsMenuButton o, Gtk.Container.IsContainer a) => o -> a -> m ()
setMenuButtonAlignWidget obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "align-widget" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@align-widget@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructMenuButtonAlignWidget :: (IsMenuButton o, MIO.MonadIO m, Gtk.Container.IsContainer a) => a -> m (GValueConstruct o)
constructMenuButtonAlignWidget val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "align-widget" (P.Just val)

-- | Set the value of the “@align-widget@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #alignWidget
-- @
clearMenuButtonAlignWidget :: (MonadIO m, IsMenuButton o) => o -> m ()
clearMenuButtonAlignWidget obj = liftIO $ B.Properties.setObjectPropertyObject obj "align-widget" (Nothing :: Maybe Gtk.Container.Container)

#if defined(ENABLE_OVERLOADING)
data MenuButtonAlignWidgetPropertyInfo
instance AttrInfo MenuButtonAlignWidgetPropertyInfo where
    type AttrAllowedOps MenuButtonAlignWidgetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint MenuButtonAlignWidgetPropertyInfo = IsMenuButton
    type AttrSetTypeConstraint MenuButtonAlignWidgetPropertyInfo = Gtk.Container.IsContainer
    type AttrTransferTypeConstraint MenuButtonAlignWidgetPropertyInfo = Gtk.Container.IsContainer
    type AttrTransferType MenuButtonAlignWidgetPropertyInfo = Gtk.Container.Container
    type AttrGetType MenuButtonAlignWidgetPropertyInfo = (Maybe Gtk.Container.Container)
    type AttrLabel MenuButtonAlignWidgetPropertyInfo = "align-widget"
    type AttrOrigin MenuButtonAlignWidgetPropertyInfo = MenuButton
    attrGet = getMenuButtonAlignWidget
    attrSet = setMenuButtonAlignWidget
    attrTransfer _ v = do
        unsafeCastTo Gtk.Container.Container v
    attrConstruct = constructMenuButtonAlignWidget
    attrClear = clearMenuButtonAlignWidget
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuButton.alignWidget"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuButton.html#g:attr:alignWidget"
        })
#endif

-- VVV Prop "direction"
   -- Type: TInterface (Name {namespace = "Gtk", name = "ArrowType"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@direction@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' menuButton #direction
-- @
getMenuButtonDirection :: (MonadIO m, IsMenuButton o) => o -> m Gtk.Enums.ArrowType
getMenuButtonDirection obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "direction"

-- | Set the value of the “@direction@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' menuButton [ #direction 'Data.GI.Base.Attributes.:=' value ]
-- @
setMenuButtonDirection :: (MonadIO m, IsMenuButton o) => o -> Gtk.Enums.ArrowType -> m ()
setMenuButtonDirection obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "direction" val

-- | Construct a `GValueConstruct` with valid value for the “@direction@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructMenuButtonDirection :: (IsMenuButton o, MIO.MonadIO m) => Gtk.Enums.ArrowType -> m (GValueConstruct o)
constructMenuButtonDirection val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "direction" val

#if defined(ENABLE_OVERLOADING)
data MenuButtonDirectionPropertyInfo
instance AttrInfo MenuButtonDirectionPropertyInfo where
    type AttrAllowedOps MenuButtonDirectionPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint MenuButtonDirectionPropertyInfo = IsMenuButton
    type AttrSetTypeConstraint MenuButtonDirectionPropertyInfo = (~) Gtk.Enums.ArrowType
    type AttrTransferTypeConstraint MenuButtonDirectionPropertyInfo = (~) Gtk.Enums.ArrowType
    type AttrTransferType MenuButtonDirectionPropertyInfo = Gtk.Enums.ArrowType
    type AttrGetType MenuButtonDirectionPropertyInfo = Gtk.Enums.ArrowType
    type AttrLabel MenuButtonDirectionPropertyInfo = "direction"
    type AttrOrigin MenuButtonDirectionPropertyInfo = MenuButton
    attrGet = getMenuButtonDirection
    attrSet = setMenuButtonDirection
    attrTransfer _ v = do
        return v
    attrConstruct = constructMenuButtonDirection
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuButton.direction"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuButton.html#g:attr:direction"
        })
#endif

-- VVV Prop "menu-model"
   -- Type: TInterface (Name {namespace = "Gio", name = "MenuModel"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just True)

-- | Get the value of the “@menu-model@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' menuButton #menuModel
-- @
getMenuButtonMenuModel :: (MonadIO m, IsMenuButton o) => o -> m (Maybe Gio.MenuModel.MenuModel)
getMenuButtonMenuModel obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "menu-model" Gio.MenuModel.MenuModel

-- | Set the value of the “@menu-model@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' menuButton [ #menuModel 'Data.GI.Base.Attributes.:=' value ]
-- @
setMenuButtonMenuModel :: (MonadIO m, IsMenuButton o, Gio.MenuModel.IsMenuModel a) => o -> a -> m ()
setMenuButtonMenuModel obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "menu-model" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@menu-model@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructMenuButtonMenuModel :: (IsMenuButton o, MIO.MonadIO m, Gio.MenuModel.IsMenuModel a) => a -> m (GValueConstruct o)
constructMenuButtonMenuModel val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "menu-model" (P.Just val)

-- | Set the value of the “@menu-model@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #menuModel
-- @
clearMenuButtonMenuModel :: (MonadIO m, IsMenuButton o) => o -> m ()
clearMenuButtonMenuModel obj = liftIO $ B.Properties.setObjectPropertyObject obj "menu-model" (Nothing :: Maybe Gio.MenuModel.MenuModel)

#if defined(ENABLE_OVERLOADING)
data MenuButtonMenuModelPropertyInfo
instance AttrInfo MenuButtonMenuModelPropertyInfo where
    type AttrAllowedOps MenuButtonMenuModelPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint MenuButtonMenuModelPropertyInfo = IsMenuButton
    type AttrSetTypeConstraint MenuButtonMenuModelPropertyInfo = Gio.MenuModel.IsMenuModel
    type AttrTransferTypeConstraint MenuButtonMenuModelPropertyInfo = Gio.MenuModel.IsMenuModel
    type AttrTransferType MenuButtonMenuModelPropertyInfo = Gio.MenuModel.MenuModel
    type AttrGetType MenuButtonMenuModelPropertyInfo = (Maybe Gio.MenuModel.MenuModel)
    type AttrLabel MenuButtonMenuModelPropertyInfo = "menu-model"
    type AttrOrigin MenuButtonMenuModelPropertyInfo = MenuButton
    attrGet = getMenuButtonMenuModel
    attrSet = setMenuButtonMenuModel
    attrTransfer _ v = do
        unsafeCastTo Gio.MenuModel.MenuModel v
    attrConstruct = constructMenuButtonMenuModel
    attrClear = clearMenuButtonMenuModel
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuButton.menuModel"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuButton.html#g:attr:menuModel"
        })
#endif

-- VVV Prop "popover"
   -- Type: TInterface (Name {namespace = "Gtk", name = "Popover"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Nothing)

-- | Get the value of the “@popover@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' menuButton #popover
-- @
getMenuButtonPopover :: (MonadIO m, IsMenuButton o) => o -> m (Maybe Gtk.Popover.Popover)
getMenuButtonPopover obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "popover" Gtk.Popover.Popover

-- | Set the value of the “@popover@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' menuButton [ #popover 'Data.GI.Base.Attributes.:=' value ]
-- @
setMenuButtonPopover :: (MonadIO m, IsMenuButton o, Gtk.Popover.IsPopover a) => o -> a -> m ()
setMenuButtonPopover obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "popover" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@popover@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructMenuButtonPopover :: (IsMenuButton o, MIO.MonadIO m, Gtk.Popover.IsPopover a) => a -> m (GValueConstruct o)
constructMenuButtonPopover val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "popover" (P.Just val)

-- | Set the value of the “@popover@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #popover
-- @
clearMenuButtonPopover :: (MonadIO m, IsMenuButton o) => o -> m ()
clearMenuButtonPopover obj = liftIO $ B.Properties.setObjectPropertyObject obj "popover" (Nothing :: Maybe Gtk.Popover.Popover)

#if defined(ENABLE_OVERLOADING)
data MenuButtonPopoverPropertyInfo
instance AttrInfo MenuButtonPopoverPropertyInfo where
    type AttrAllowedOps MenuButtonPopoverPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint MenuButtonPopoverPropertyInfo = IsMenuButton
    type AttrSetTypeConstraint MenuButtonPopoverPropertyInfo = Gtk.Popover.IsPopover
    type AttrTransferTypeConstraint MenuButtonPopoverPropertyInfo = Gtk.Popover.IsPopover
    type AttrTransferType MenuButtonPopoverPropertyInfo = Gtk.Popover.Popover
    type AttrGetType MenuButtonPopoverPropertyInfo = (Maybe Gtk.Popover.Popover)
    type AttrLabel MenuButtonPopoverPropertyInfo = "popover"
    type AttrOrigin MenuButtonPopoverPropertyInfo = MenuButton
    attrGet = getMenuButtonPopover
    attrSet = setMenuButtonPopover
    attrTransfer _ v = do
        unsafeCastTo Gtk.Popover.Popover v
    attrConstruct = constructMenuButtonPopover
    attrClear = clearMenuButtonPopover
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuButton.popover"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuButton.html#g:attr:popover"
        })
#endif

-- VVV Prop "popup"
   -- Type: TInterface (Name {namespace = "Gtk", name = "Menu"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Nothing)

-- | Get the value of the “@popup@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' menuButton #popup
-- @
getMenuButtonPopup :: (MonadIO m, IsMenuButton o) => o -> m (Maybe Gtk.Menu.Menu)
getMenuButtonPopup obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "popup" Gtk.Menu.Menu

-- | Set the value of the “@popup@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' menuButton [ #popup 'Data.GI.Base.Attributes.:=' value ]
-- @
setMenuButtonPopup :: (MonadIO m, IsMenuButton o, Gtk.Menu.IsMenu a) => o -> a -> m ()
setMenuButtonPopup obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "popup" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@popup@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructMenuButtonPopup :: (IsMenuButton o, MIO.MonadIO m, Gtk.Menu.IsMenu a) => a -> m (GValueConstruct o)
constructMenuButtonPopup val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "popup" (P.Just val)

-- | Set the value of the “@popup@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #popup
-- @
clearMenuButtonPopup :: (MonadIO m, IsMenuButton o) => o -> m ()
clearMenuButtonPopup obj = liftIO $ B.Properties.setObjectPropertyObject obj "popup" (Nothing :: Maybe Gtk.Menu.Menu)

#if defined(ENABLE_OVERLOADING)
data MenuButtonPopupPropertyInfo
instance AttrInfo MenuButtonPopupPropertyInfo where
    type AttrAllowedOps MenuButtonPopupPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint MenuButtonPopupPropertyInfo = IsMenuButton
    type AttrSetTypeConstraint MenuButtonPopupPropertyInfo = Gtk.Menu.IsMenu
    type AttrTransferTypeConstraint MenuButtonPopupPropertyInfo = Gtk.Menu.IsMenu
    type AttrTransferType MenuButtonPopupPropertyInfo = Gtk.Menu.Menu
    type AttrGetType MenuButtonPopupPropertyInfo = (Maybe Gtk.Menu.Menu)
    type AttrLabel MenuButtonPopupPropertyInfo = "popup"
    type AttrOrigin MenuButtonPopupPropertyInfo = MenuButton
    attrGet = getMenuButtonPopup
    attrSet = setMenuButtonPopup
    attrTransfer _ v = do
        unsafeCastTo Gtk.Menu.Menu v
    attrConstruct = constructMenuButtonPopup
    attrClear = clearMenuButtonPopup
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuButton.popup"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuButton.html#g:attr:popup"
        })
#endif

-- VVV Prop "use-popover"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@use-popover@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' menuButton #usePopover
-- @
getMenuButtonUsePopover :: (MonadIO m, IsMenuButton o) => o -> m Bool
getMenuButtonUsePopover obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "use-popover"

-- | Set the value of the “@use-popover@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' menuButton [ #usePopover 'Data.GI.Base.Attributes.:=' value ]
-- @
setMenuButtonUsePopover :: (MonadIO m, IsMenuButton o) => o -> Bool -> m ()
setMenuButtonUsePopover obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "use-popover" val

-- | Construct a `GValueConstruct` with valid value for the “@use-popover@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructMenuButtonUsePopover :: (IsMenuButton o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructMenuButtonUsePopover val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "use-popover" val

#if defined(ENABLE_OVERLOADING)
data MenuButtonUsePopoverPropertyInfo
instance AttrInfo MenuButtonUsePopoverPropertyInfo where
    type AttrAllowedOps MenuButtonUsePopoverPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint MenuButtonUsePopoverPropertyInfo = IsMenuButton
    type AttrSetTypeConstraint MenuButtonUsePopoverPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint MenuButtonUsePopoverPropertyInfo = (~) Bool
    type AttrTransferType MenuButtonUsePopoverPropertyInfo = Bool
    type AttrGetType MenuButtonUsePopoverPropertyInfo = Bool
    type AttrLabel MenuButtonUsePopoverPropertyInfo = "use-popover"
    type AttrOrigin MenuButtonUsePopoverPropertyInfo = MenuButton
    attrGet = getMenuButtonUsePopover
    attrSet = setMenuButtonUsePopover
    attrTransfer _ v = do
        return v
    attrConstruct = constructMenuButtonUsePopover
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuButton.usePopover"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuButton.html#g:attr:usePopover"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList MenuButton
type instance O.AttributeList MenuButton = MenuButtonAttributeList
type MenuButtonAttributeList = ('[ '("actionName", Gtk.Actionable.ActionableActionNamePropertyInfo), '("actionTarget", Gtk.Actionable.ActionableActionTargetPropertyInfo), '("active", Gtk.ToggleButton.ToggleButtonActivePropertyInfo), '("alignWidget", MenuButtonAlignWidgetPropertyInfo), '("alwaysShowImage", Gtk.Button.ButtonAlwaysShowImagePropertyInfo), '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("direction", MenuButtonDirectionPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("drawIndicator", Gtk.ToggleButton.ToggleButtonDrawIndicatorPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("image", Gtk.Button.ButtonImagePropertyInfo), '("imagePosition", Gtk.Button.ButtonImagePositionPropertyInfo), '("inconsistent", Gtk.ToggleButton.ToggleButtonInconsistentPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("label", Gtk.Button.ButtonLabelPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("menuModel", MenuButtonMenuModelPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("popover", MenuButtonPopoverPropertyInfo), '("popup", MenuButtonPopupPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("relatedAction", Gtk.Activatable.ActivatableRelatedActionPropertyInfo), '("relief", Gtk.Button.ButtonReliefPropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("useActionAppearance", Gtk.Activatable.ActivatableUseActionAppearancePropertyInfo), '("usePopover", MenuButtonUsePopoverPropertyInfo), '("useStock", Gtk.Button.ButtonUseStockPropertyInfo), '("useUnderline", Gtk.Button.ButtonUseUnderlinePropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo), '("xalign", Gtk.Button.ButtonXalignPropertyInfo), '("yalign", Gtk.Button.ButtonYalignPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
menuButtonAlignWidget :: AttrLabelProxy "alignWidget"
menuButtonAlignWidget = AttrLabelProxy

menuButtonDirection :: AttrLabelProxy "direction"
menuButtonDirection = AttrLabelProxy

menuButtonMenuModel :: AttrLabelProxy "menuModel"
menuButtonMenuModel = AttrLabelProxy

menuButtonPopover :: AttrLabelProxy "popover"
menuButtonPopover = AttrLabelProxy

menuButtonPopup :: AttrLabelProxy "popup"
menuButtonPopup = AttrLabelProxy

menuButtonUsePopover :: AttrLabelProxy "usePopover"
menuButtonUsePopover = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList MenuButton = MenuButtonSignalList
type MenuButtonSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("activate", Gtk.Button.ButtonActivateSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("clicked", Gtk.Button.ButtonClickedSignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enter", Gtk.Button.ButtonEnterSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leave", Gtk.Button.ButtonLeaveSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("pressed", Gtk.Button.ButtonPressedSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("released", Gtk.Button.ButtonReleasedSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("toggled", Gtk.ToggleButton.ToggleButtonToggledSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method MenuButton::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "MenuButton" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_menu_button_new" gtk_menu_button_new :: 
    IO (Ptr MenuButton)

-- | Creates a new t'GI.Gtk.Objects.MenuButton.MenuButton' widget with downwards-pointing
-- arrow as the only child. You can replace the child widget
-- with another t'GI.Gtk.Objects.Widget.Widget' should you wish to.
-- 
-- /Since: 3.6/
menuButtonNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m MenuButton
    -- ^ __Returns:__ The newly created t'GI.Gtk.Objects.MenuButton.MenuButton' widget
menuButtonNew  = liftIO $ do
    result <- gtk_menu_button_new
    checkUnexpectedReturnNULL "menuButtonNew" result
    result' <- (newObject MenuButton) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method MenuButton::get_align_widget
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MenuButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenuButton" , sinceVersion = Nothing }
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

foreign import ccall "gtk_menu_button_get_align_widget" gtk_menu_button_get_align_widget :: 
    Ptr MenuButton ->                       -- menu_button : TInterface (Name {namespace = "Gtk", name = "MenuButton"})
    IO (Ptr Gtk.Widget.Widget)

-- | Returns the parent t'GI.Gtk.Objects.Widget.Widget' to use to line up with menu.
-- 
-- /Since: 3.6/
menuButtonGetAlignWidget ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenuButton a) =>
    a
    -- ^ /@menuButton@/: a t'GI.Gtk.Objects.MenuButton.MenuButton'
    -> m (Maybe Gtk.Widget.Widget)
    -- ^ __Returns:__ a t'GI.Gtk.Objects.Widget.Widget' value or 'P.Nothing'
menuButtonGetAlignWidget menuButton = liftIO $ do
    menuButton' <- unsafeManagedPtrCastPtr menuButton
    result <- gtk_menu_button_get_align_widget menuButton'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Widget.Widget) result'
        return result''
    touchManagedPtr menuButton
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data MenuButtonGetAlignWidgetMethodInfo
instance (signature ~ (m (Maybe Gtk.Widget.Widget)), MonadIO m, IsMenuButton a) => O.OverloadedMethod MenuButtonGetAlignWidgetMethodInfo a signature where
    overloadedMethod = menuButtonGetAlignWidget

instance O.OverloadedMethodInfo MenuButtonGetAlignWidgetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuButton.menuButtonGetAlignWidget",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuButton.html#v:menuButtonGetAlignWidget"
        })


#endif

-- method MenuButton::get_direction
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MenuButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenuButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "ArrowType" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_menu_button_get_direction" gtk_menu_button_get_direction :: 
    Ptr MenuButton ->                       -- menu_button : TInterface (Name {namespace = "Gtk", name = "MenuButton"})
    IO CUInt

-- | Returns the direction the popup will be pointing at when popped up.
-- 
-- /Since: 3.6/
menuButtonGetDirection ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenuButton a) =>
    a
    -- ^ /@menuButton@/: a t'GI.Gtk.Objects.MenuButton.MenuButton'
    -> m Gtk.Enums.ArrowType
    -- ^ __Returns:__ a t'GI.Gtk.Enums.ArrowType' value
menuButtonGetDirection menuButton = liftIO $ do
    menuButton' <- unsafeManagedPtrCastPtr menuButton
    result <- gtk_menu_button_get_direction menuButton'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr menuButton
    return result'

#if defined(ENABLE_OVERLOADING)
data MenuButtonGetDirectionMethodInfo
instance (signature ~ (m Gtk.Enums.ArrowType), MonadIO m, IsMenuButton a) => O.OverloadedMethod MenuButtonGetDirectionMethodInfo a signature where
    overloadedMethod = menuButtonGetDirection

instance O.OverloadedMethodInfo MenuButtonGetDirectionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuButton.menuButtonGetDirection",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuButton.html#v:menuButtonGetDirection"
        })


#endif

-- method MenuButton::get_menu_model
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MenuButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenuButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gio" , name = "MenuModel" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_menu_button_get_menu_model" gtk_menu_button_get_menu_model :: 
    Ptr MenuButton ->                       -- menu_button : TInterface (Name {namespace = "Gtk", name = "MenuButton"})
    IO (Ptr Gio.MenuModel.MenuModel)

-- | Returns the t'GI.Gio.Objects.MenuModel.MenuModel' used to generate the popup.
-- 
-- /Since: 3.6/
menuButtonGetMenuModel ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenuButton a) =>
    a
    -- ^ /@menuButton@/: a t'GI.Gtk.Objects.MenuButton.MenuButton'
    -> m (Maybe Gio.MenuModel.MenuModel)
    -- ^ __Returns:__ a t'GI.Gio.Objects.MenuModel.MenuModel' or 'P.Nothing'
menuButtonGetMenuModel menuButton = liftIO $ do
    menuButton' <- unsafeManagedPtrCastPtr menuButton
    result <- gtk_menu_button_get_menu_model menuButton'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gio.MenuModel.MenuModel) result'
        return result''
    touchManagedPtr menuButton
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data MenuButtonGetMenuModelMethodInfo
instance (signature ~ (m (Maybe Gio.MenuModel.MenuModel)), MonadIO m, IsMenuButton a) => O.OverloadedMethod MenuButtonGetMenuModelMethodInfo a signature where
    overloadedMethod = menuButtonGetMenuModel

instance O.OverloadedMethodInfo MenuButtonGetMenuModelMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuButton.menuButtonGetMenuModel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuButton.html#v:menuButtonGetMenuModel"
        })


#endif

-- method MenuButton::get_popover
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MenuButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenuButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Popover" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_menu_button_get_popover" gtk_menu_button_get_popover :: 
    Ptr MenuButton ->                       -- menu_button : TInterface (Name {namespace = "Gtk", name = "MenuButton"})
    IO (Ptr Gtk.Popover.Popover)

-- | Returns the t'GI.Gtk.Objects.Popover.Popover' that pops out of the button.
-- If the button is not using a t'GI.Gtk.Objects.Popover.Popover', this function
-- returns 'P.Nothing'.
-- 
-- /Since: 3.12/
menuButtonGetPopover ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenuButton a) =>
    a
    -- ^ /@menuButton@/: a t'GI.Gtk.Objects.MenuButton.MenuButton'
    -> m (Maybe Gtk.Popover.Popover)
    -- ^ __Returns:__ a t'GI.Gtk.Objects.Popover.Popover' or 'P.Nothing'
menuButtonGetPopover menuButton = liftIO $ do
    menuButton' <- unsafeManagedPtrCastPtr menuButton
    result <- gtk_menu_button_get_popover menuButton'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Popover.Popover) result'
        return result''
    touchManagedPtr menuButton
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data MenuButtonGetPopoverMethodInfo
instance (signature ~ (m (Maybe Gtk.Popover.Popover)), MonadIO m, IsMenuButton a) => O.OverloadedMethod MenuButtonGetPopoverMethodInfo a signature where
    overloadedMethod = menuButtonGetPopover

instance O.OverloadedMethodInfo MenuButtonGetPopoverMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuButton.menuButtonGetPopover",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuButton.html#v:menuButtonGetPopover"
        })


#endif

-- method MenuButton::get_popup
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MenuButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenuButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Menu" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_menu_button_get_popup" gtk_menu_button_get_popup :: 
    Ptr MenuButton ->                       -- menu_button : TInterface (Name {namespace = "Gtk", name = "MenuButton"})
    IO (Ptr Gtk.Menu.Menu)

-- | Returns the t'GI.Gtk.Objects.Menu.Menu' that pops out of the button.
-- If the button does not use a t'GI.Gtk.Objects.Menu.Menu', this function
-- returns 'P.Nothing'.
-- 
-- /Since: 3.6/
menuButtonGetPopup ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenuButton a) =>
    a
    -- ^ /@menuButton@/: a t'GI.Gtk.Objects.MenuButton.MenuButton'
    -> m (Maybe Gtk.Menu.Menu)
    -- ^ __Returns:__ a t'GI.Gtk.Objects.Menu.Menu' or 'P.Nothing'
menuButtonGetPopup menuButton = liftIO $ do
    menuButton' <- unsafeManagedPtrCastPtr menuButton
    result <- gtk_menu_button_get_popup menuButton'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Menu.Menu) result'
        return result''
    touchManagedPtr menuButton
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data MenuButtonGetPopupMethodInfo
instance (signature ~ (m (Maybe Gtk.Menu.Menu)), MonadIO m, IsMenuButton a) => O.OverloadedMethod MenuButtonGetPopupMethodInfo a signature where
    overloadedMethod = menuButtonGetPopup

instance O.OverloadedMethodInfo MenuButtonGetPopupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuButton.menuButtonGetPopup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuButton.html#v:menuButtonGetPopup"
        })


#endif

-- method MenuButton::get_use_popover
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MenuButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenuButton" , sinceVersion = Nothing }
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

foreign import ccall "gtk_menu_button_get_use_popover" gtk_menu_button_get_use_popover :: 
    Ptr MenuButton ->                       -- menu_button : TInterface (Name {namespace = "Gtk", name = "MenuButton"})
    IO CInt

-- | Returns whether a t'GI.Gtk.Objects.Popover.Popover' or a t'GI.Gtk.Objects.Menu.Menu' will be constructed
-- from the menu model.
-- 
-- /Since: 3.12/
menuButtonGetUsePopover ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenuButton a) =>
    a
    -- ^ /@menuButton@/: a t'GI.Gtk.Objects.MenuButton.MenuButton'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if using a t'GI.Gtk.Objects.Popover.Popover'
menuButtonGetUsePopover menuButton = liftIO $ do
    menuButton' <- unsafeManagedPtrCastPtr menuButton
    result <- gtk_menu_button_get_use_popover menuButton'
    let result' = (/= 0) result
    touchManagedPtr menuButton
    return result'

#if defined(ENABLE_OVERLOADING)
data MenuButtonGetUsePopoverMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsMenuButton a) => O.OverloadedMethod MenuButtonGetUsePopoverMethodInfo a signature where
    overloadedMethod = menuButtonGetUsePopover

instance O.OverloadedMethodInfo MenuButtonGetUsePopoverMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuButton.menuButtonGetUsePopover",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuButton.html#v:menuButtonGetUsePopover"
        })


#endif

-- method MenuButton::set_align_widget
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MenuButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenuButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "align_widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidget" , sinceVersion = Nothing }
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

foreign import ccall "gtk_menu_button_set_align_widget" gtk_menu_button_set_align_widget :: 
    Ptr MenuButton ->                       -- menu_button : TInterface (Name {namespace = "Gtk", name = "MenuButton"})
    Ptr Gtk.Widget.Widget ->                -- align_widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | Sets the t'GI.Gtk.Objects.Widget.Widget' to use to line the menu with when popped up.
-- Note that the /@alignWidget@/ must contain the t'GI.Gtk.Objects.MenuButton.MenuButton' itself.
-- 
-- Setting it to 'P.Nothing' means that the menu will be aligned with the
-- button itself.
-- 
-- Note that this property is only used with menus currently,
-- and not for popovers.
-- 
-- /Since: 3.6/
menuButtonSetAlignWidget ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenuButton a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@menuButton@/: a t'GI.Gtk.Objects.MenuButton.MenuButton'
    -> Maybe (b)
    -- ^ /@alignWidget@/: a t'GI.Gtk.Objects.Widget.Widget'
    -> m ()
menuButtonSetAlignWidget menuButton alignWidget = liftIO $ do
    menuButton' <- unsafeManagedPtrCastPtr menuButton
    maybeAlignWidget <- case alignWidget of
        Nothing -> return nullPtr
        Just jAlignWidget -> do
            jAlignWidget' <- unsafeManagedPtrCastPtr jAlignWidget
            return jAlignWidget'
    gtk_menu_button_set_align_widget menuButton' maybeAlignWidget
    touchManagedPtr menuButton
    whenJust alignWidget touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuButtonSetAlignWidgetMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsMenuButton a, Gtk.Widget.IsWidget b) => O.OverloadedMethod MenuButtonSetAlignWidgetMethodInfo a signature where
    overloadedMethod = menuButtonSetAlignWidget

instance O.OverloadedMethodInfo MenuButtonSetAlignWidgetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuButton.menuButtonSetAlignWidget",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuButton.html#v:menuButtonSetAlignWidget"
        })


#endif

-- method MenuButton::set_direction
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MenuButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenuButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "direction"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ArrowType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkArrowType" , sinceVersion = Nothing }
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

foreign import ccall "gtk_menu_button_set_direction" gtk_menu_button_set_direction :: 
    Ptr MenuButton ->                       -- menu_button : TInterface (Name {namespace = "Gtk", name = "MenuButton"})
    CUInt ->                                -- direction : TInterface (Name {namespace = "Gtk", name = "ArrowType"})
    IO ()

-- | Sets the direction in which the popup will be popped up, as
-- well as changing the arrow’s direction. The child will not
-- be changed to an arrow if it was customized.
-- 
-- If the does not fit in the available space in the given direction,
-- GTK+ will its best to keep it inside the screen and fully visible.
-- 
-- If you pass 'GI.Gtk.Enums.ArrowTypeNone' for a /@direction@/, the popup will behave
-- as if you passed 'GI.Gtk.Enums.ArrowTypeDown' (although you won’t see any arrows).
-- 
-- /Since: 3.6/
menuButtonSetDirection ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenuButton a) =>
    a
    -- ^ /@menuButton@/: a t'GI.Gtk.Objects.MenuButton.MenuButton'
    -> Gtk.Enums.ArrowType
    -- ^ /@direction@/: a t'GI.Gtk.Enums.ArrowType'
    -> m ()
menuButtonSetDirection menuButton direction = liftIO $ do
    menuButton' <- unsafeManagedPtrCastPtr menuButton
    let direction' = (fromIntegral . fromEnum) direction
    gtk_menu_button_set_direction menuButton' direction'
    touchManagedPtr menuButton
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuButtonSetDirectionMethodInfo
instance (signature ~ (Gtk.Enums.ArrowType -> m ()), MonadIO m, IsMenuButton a) => O.OverloadedMethod MenuButtonSetDirectionMethodInfo a signature where
    overloadedMethod = menuButtonSetDirection

instance O.OverloadedMethodInfo MenuButtonSetDirectionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuButton.menuButtonSetDirection",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuButton.html#v:menuButtonSetDirection"
        })


#endif

-- method MenuButton::set_menu_model
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MenuButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenuButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "menu_model"
--           , argType =
--               TInterface Name { namespace = "Gio" , name = "MenuModel" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GMenuModel, or %NULL to unset and disable the\n  button"
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

foreign import ccall "gtk_menu_button_set_menu_model" gtk_menu_button_set_menu_model :: 
    Ptr MenuButton ->                       -- menu_button : TInterface (Name {namespace = "Gtk", name = "MenuButton"})
    Ptr Gio.MenuModel.MenuModel ->          -- menu_model : TInterface (Name {namespace = "Gio", name = "MenuModel"})
    IO ()

-- | Sets the t'GI.Gio.Objects.MenuModel.MenuModel' from which the popup will be constructed,
-- or 'P.Nothing' to dissociate any existing menu model and disable the button.
-- 
-- Depending on the value of [MenuButton:usePopover]("GI.Gtk.Objects.MenuButton#g:attr:usePopover"), either a
-- t'GI.Gtk.Objects.Menu.Menu' will be created with 'GI.Gtk.Objects.Menu.menuNewFromModel', or a
-- t'GI.Gtk.Objects.Popover.Popover' with 'GI.Gtk.Objects.Popover.popoverNewFromModel'. In either case,
-- actions will be connected as documented for these functions.
-- 
-- If [MenuButton:popup]("GI.Gtk.Objects.MenuButton#g:attr:popup") or [MenuButton:popover]("GI.Gtk.Objects.MenuButton#g:attr:popover") are already set, those
-- widgets are dissociated from the /@menuButton@/, and those properties are set
-- to 'P.Nothing'.
-- 
-- /Since: 3.6/
menuButtonSetMenuModel ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenuButton a, Gio.MenuModel.IsMenuModel b) =>
    a
    -- ^ /@menuButton@/: a t'GI.Gtk.Objects.MenuButton.MenuButton'
    -> Maybe (b)
    -- ^ /@menuModel@/: a t'GI.Gio.Objects.MenuModel.MenuModel', or 'P.Nothing' to unset and disable the
    --   button
    -> m ()
menuButtonSetMenuModel menuButton menuModel = liftIO $ do
    menuButton' <- unsafeManagedPtrCastPtr menuButton
    maybeMenuModel <- case menuModel of
        Nothing -> return nullPtr
        Just jMenuModel -> do
            jMenuModel' <- unsafeManagedPtrCastPtr jMenuModel
            return jMenuModel'
    gtk_menu_button_set_menu_model menuButton' maybeMenuModel
    touchManagedPtr menuButton
    whenJust menuModel touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuButtonSetMenuModelMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsMenuButton a, Gio.MenuModel.IsMenuModel b) => O.OverloadedMethod MenuButtonSetMenuModelMethodInfo a signature where
    overloadedMethod = menuButtonSetMenuModel

instance O.OverloadedMethodInfo MenuButtonSetMenuModelMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuButton.menuButtonSetMenuModel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuButton.html#v:menuButtonSetMenuModel"
        })


#endif

-- method MenuButton::set_popover
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MenuButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenuButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "popover"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GtkPopover, or %NULL to unset and disable the button"
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

foreign import ccall "gtk_menu_button_set_popover" gtk_menu_button_set_popover :: 
    Ptr MenuButton ->                       -- menu_button : TInterface (Name {namespace = "Gtk", name = "MenuButton"})
    Ptr Gtk.Widget.Widget ->                -- popover : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | Sets the t'GI.Gtk.Objects.Popover.Popover' that will be popped up when the /@menuButton@/ is clicked,
-- or 'P.Nothing' to dissociate any existing popover and disable the button.
-- 
-- If [MenuButton:menuModel]("GI.Gtk.Objects.MenuButton#g:attr:menuModel") or [MenuButton:popup]("GI.Gtk.Objects.MenuButton#g:attr:popup") are set, those objects
-- are dissociated from the /@menuButton@/, and those properties are set to 'P.Nothing'.
-- 
-- /Since: 3.12/
menuButtonSetPopover ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenuButton a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@menuButton@/: a t'GI.Gtk.Objects.MenuButton.MenuButton'
    -> Maybe (b)
    -- ^ /@popover@/: a t'GI.Gtk.Objects.Popover.Popover', or 'P.Nothing' to unset and disable the button
    -> m ()
menuButtonSetPopover menuButton popover = liftIO $ do
    menuButton' <- unsafeManagedPtrCastPtr menuButton
    maybePopover <- case popover of
        Nothing -> return nullPtr
        Just jPopover -> do
            jPopover' <- unsafeManagedPtrCastPtr jPopover
            return jPopover'
    gtk_menu_button_set_popover menuButton' maybePopover
    touchManagedPtr menuButton
    whenJust popover touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuButtonSetPopoverMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsMenuButton a, Gtk.Widget.IsWidget b) => O.OverloadedMethod MenuButtonSetPopoverMethodInfo a signature where
    overloadedMethod = menuButtonSetPopover

instance O.OverloadedMethodInfo MenuButtonSetPopoverMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuButton.menuButtonSetPopover",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuButton.html#v:menuButtonSetPopover"
        })


#endif

-- method MenuButton::set_popup
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MenuButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenuButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "menu"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GtkMenu, or %NULL to unset and disable the button"
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

foreign import ccall "gtk_menu_button_set_popup" gtk_menu_button_set_popup :: 
    Ptr MenuButton ->                       -- menu_button : TInterface (Name {namespace = "Gtk", name = "MenuButton"})
    Ptr Gtk.Widget.Widget ->                -- menu : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | Sets the t'GI.Gtk.Objects.Menu.Menu' that will be popped up when the /@menuButton@/ is clicked, or
-- 'P.Nothing' to dissociate any existing menu and disable the button.
-- 
-- If [MenuButton:menuModel]("GI.Gtk.Objects.MenuButton#g:attr:menuModel") or [MenuButton:popover]("GI.Gtk.Objects.MenuButton#g:attr:popover") are set, those objects
-- are dissociated from the /@menuButton@/, and those properties are set to 'P.Nothing'.
-- 
-- /Since: 3.6/
menuButtonSetPopup ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenuButton a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@menuButton@/: a t'GI.Gtk.Objects.MenuButton.MenuButton'
    -> Maybe (b)
    -- ^ /@menu@/: a t'GI.Gtk.Objects.Menu.Menu', or 'P.Nothing' to unset and disable the button
    -> m ()
menuButtonSetPopup menuButton menu = liftIO $ do
    menuButton' <- unsafeManagedPtrCastPtr menuButton
    maybeMenu <- case menu of
        Nothing -> return nullPtr
        Just jMenu -> do
            jMenu' <- unsafeManagedPtrCastPtr jMenu
            return jMenu'
    gtk_menu_button_set_popup menuButton' maybeMenu
    touchManagedPtr menuButton
    whenJust menu touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuButtonSetPopupMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsMenuButton a, Gtk.Widget.IsWidget b) => O.OverloadedMethod MenuButtonSetPopupMethodInfo a signature where
    overloadedMethod = menuButtonSetPopup

instance O.OverloadedMethodInfo MenuButtonSetPopupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuButton.menuButtonSetPopup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuButton.html#v:menuButtonSetPopup"
        })


#endif

-- method MenuButton::set_use_popover
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MenuButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenuButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "use_popover"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "%TRUE to construct a popover from the menu model"
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

foreign import ccall "gtk_menu_button_set_use_popover" gtk_menu_button_set_use_popover :: 
    Ptr MenuButton ->                       -- menu_button : TInterface (Name {namespace = "Gtk", name = "MenuButton"})
    CInt ->                                 -- use_popover : TBasicType TBoolean
    IO ()

-- | Sets whether to construct a t'GI.Gtk.Objects.Popover.Popover' instead of t'GI.Gtk.Objects.Menu.Menu'
-- when 'GI.Gtk.Objects.MenuButton.menuButtonSetMenuModel' is called. Note that
-- this property is only consulted when a new menu model is set.
-- 
-- /Since: 3.12/
menuButtonSetUsePopover ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenuButton a) =>
    a
    -- ^ /@menuButton@/: a t'GI.Gtk.Objects.MenuButton.MenuButton'
    -> Bool
    -- ^ /@usePopover@/: 'P.True' to construct a popover from the menu model
    -> m ()
menuButtonSetUsePopover menuButton usePopover = liftIO $ do
    menuButton' <- unsafeManagedPtrCastPtr menuButton
    let usePopover' = (fromIntegral . fromEnum) usePopover
    gtk_menu_button_set_use_popover menuButton' usePopover'
    touchManagedPtr menuButton
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuButtonSetUsePopoverMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsMenuButton a) => O.OverloadedMethod MenuButtonSetUsePopoverMethodInfo a signature where
    overloadedMethod = menuButtonSetUsePopover

instance O.OverloadedMethodInfo MenuButtonSetUsePopoverMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuButton.menuButtonSetUsePopover",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuButton.html#v:menuButtonSetUsePopover"
        })


#endif


