{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A t'GI.Gtk.Objects.Menu.Menu' is a t'GI.Gtk.Objects.MenuShell.MenuShell' that implements a drop down menu
-- consisting of a list of t'GI.Gtk.Objects.MenuItem.MenuItem' objects which can be navigated
-- and activated by the user to perform application functions.
-- 
-- A t'GI.Gtk.Objects.Menu.Menu' is most commonly dropped down by activating a
-- t'GI.Gtk.Objects.MenuItem.MenuItem' in a t'GI.Gtk.Objects.MenuBar.MenuBar' or popped up by activating a
-- t'GI.Gtk.Objects.MenuItem.MenuItem' in another t'GI.Gtk.Objects.Menu.Menu'.
-- 
-- A t'GI.Gtk.Objects.Menu.Menu' can also be popped up by activating a t'GI.Gtk.Objects.ComboBox.ComboBox'.
-- Other composite widgets such as the t'GI.Gtk.Objects.Notebook.Notebook' can pop up a
-- t'GI.Gtk.Objects.Menu.Menu' as well.
-- 
-- Applications can display a t'GI.Gtk.Objects.Menu.Menu' as a popup menu by calling the
-- 'GI.Gtk.Objects.Menu.menuPopup' function.  The example below shows how an application
-- can pop up a menu when the 3rd mouse button is pressed.
-- 
-- == Connecting the popup signal handler.
-- 
-- 
-- === /C code/
-- >
-- >  // connect our handler which will popup the menu
-- >  g_signal_connect_swapped (window, "button_press_event",
-- >G_CALLBACK (my_popup_handler), menu);
-- 
-- 
-- == Signal handler which displays a popup menu.
-- 
-- 
-- === /C code/
-- >
-- >static gint
-- >my_popup_handler (GtkWidget *widget, GdkEvent *event)
-- >{
-- >  GtkMenu *menu;
-- >  GdkEventButton *event_button;
-- >
-- >  g_return_val_if_fail (widget != NULL, FALSE);
-- >  g_return_val_if_fail (GTK_IS_MENU (widget), FALSE);
-- >  g_return_val_if_fail (event != NULL, FALSE);
-- >
-- >  // The "widget" is the menu that was supplied when
-- >  // g_signal_connect_swapped() was called.
-- >  menu = GTK_MENU (widget);
-- >
-- >  if (event->type == GDK_BUTTON_PRESS)
-- >    {
-- >      event_button = (GdkEventButton *) event;
-- >      if (event_button->button == GDK_BUTTON_SECONDARY)
-- >        {
-- >          gtk_menu_popup (menu, NULL, NULL, NULL, NULL,
-- >                          event_button->button, event_button->time);
-- >          return TRUE;
-- >        }
-- >    }
-- >
-- >  return FALSE;
-- >}
-- 
-- 
-- = CSS nodes
-- 
-- 
-- === /plain code/
-- >
-- >menu
-- >├── arrow.top
-- >├── <child>
-- >┊
-- >├── <child>
-- >╰── arrow.bottom
-- 
-- 
-- The main CSS node of GtkMenu has name menu, and there are two subnodes
-- with name arrow, for scrolling menu arrows. These subnodes get the
-- .top and .bottom style classes.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.Menu
    ( 

-- * Exported types
    Menu(..)                                ,
    IsMenu                                  ,
    toMenu                                  ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [activateItem]("GI.Gtk.Objects.MenuShell#g:method:activateItem"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [append]("GI.Gtk.Objects.MenuShell#g:method:append"), [attach]("GI.Gtk.Objects.Menu#g:method:attach"), [attachToWidget]("GI.Gtk.Objects.Menu#g:method:attachToWidget"), [bindModel]("GI.Gtk.Objects.MenuShell#g:method:bindModel"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [cancel]("GI.Gtk.Objects.MenuShell#g:method:cancel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [deactivate]("GI.Gtk.Objects.MenuShell#g:method:deactivate"), [deselect]("GI.Gtk.Objects.MenuShell#g:method:deselect"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [detach]("GI.Gtk.Objects.Menu#g:method:detach"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insert]("GI.Gtk.Objects.MenuShell#g:method:insert"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [placeOnMonitor]("GI.Gtk.Objects.Menu#g:method:placeOnMonitor"), [popdown]("GI.Gtk.Objects.Menu#g:method:popdown"), [popup]("GI.Gtk.Objects.Menu#g:method:popup"), [popupAtPointer]("GI.Gtk.Objects.Menu#g:method:popupAtPointer"), [popupAtRect]("GI.Gtk.Objects.Menu#g:method:popupAtRect"), [popupAtWidget]("GI.Gtk.Objects.Menu#g:method:popupAtWidget"), [popupForDevice]("GI.Gtk.Objects.Menu#g:method:popupForDevice"), [prepend]("GI.Gtk.Objects.MenuShell#g:method:prepend"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reorderChild]("GI.Gtk.Objects.Menu#g:method:reorderChild"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [reposition]("GI.Gtk.Objects.Menu#g:method:reposition"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [selectFirst]("GI.Gtk.Objects.MenuShell#g:method:selectFirst"), [selectItem]("GI.Gtk.Objects.MenuShell#g:method:selectItem"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccelGroup]("GI.Gtk.Objects.Menu#g:method:getAccelGroup"), [getAccelPath]("GI.Gtk.Objects.Menu#g:method:getAccelPath"), [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getActive]("GI.Gtk.Objects.Menu#g:method:getActive"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getAttachWidget]("GI.Gtk.Objects.Menu#g:method:getAttachWidget"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getMonitor]("GI.Gtk.Objects.Menu#g:method:getMonitor"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentShell]("GI.Gtk.Objects.MenuShell#g:method:getParentShell"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getReserveToggleSize]("GI.Gtk.Objects.Menu#g:method:getReserveToggleSize"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSelectedItem]("GI.Gtk.Objects.MenuShell#g:method:getSelectedItem"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTakeFocus]("GI.Gtk.Objects.MenuShell#g:method:getTakeFocus"), [getTearoffState]("GI.Gtk.Objects.Menu#g:method:getTearoffState"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTitle]("GI.Gtk.Objects.Menu#g:method:getTitle"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelGroup]("GI.Gtk.Objects.Menu#g:method:setAccelGroup"), [setAccelPath]("GI.Gtk.Objects.Menu#g:method:setAccelPath"), [setActive]("GI.Gtk.Objects.Menu#g:method:setActive"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setMonitor]("GI.Gtk.Objects.Menu#g:method:setMonitor"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setReserveToggleSize]("GI.Gtk.Objects.Menu#g:method:setReserveToggleSize"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setScreen]("GI.Gtk.Objects.Menu#g:method:setScreen"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTakeFocus]("GI.Gtk.Objects.MenuShell#g:method:setTakeFocus"), [setTearoffState]("GI.Gtk.Objects.Menu#g:method:setTearoffState"), [setTitle]("GI.Gtk.Objects.Menu#g:method:setTitle"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveMenuMethod                       ,
#endif

-- ** attach #method:attach#

#if defined(ENABLE_OVERLOADING)
    MenuAttachMethodInfo                    ,
#endif
    menuAttach                              ,


-- ** attachToWidget #method:attachToWidget#

#if defined(ENABLE_OVERLOADING)
    MenuAttachToWidgetMethodInfo            ,
#endif
    menuAttachToWidget                      ,


-- ** detach #method:detach#

#if defined(ENABLE_OVERLOADING)
    MenuDetachMethodInfo                    ,
#endif
    menuDetach                              ,


-- ** getAccelGroup #method:getAccelGroup#

#if defined(ENABLE_OVERLOADING)
    MenuGetAccelGroupMethodInfo             ,
#endif
    menuGetAccelGroup                       ,


-- ** getAccelPath #method:getAccelPath#

#if defined(ENABLE_OVERLOADING)
    MenuGetAccelPathMethodInfo              ,
#endif
    menuGetAccelPath                        ,


-- ** getActive #method:getActive#

#if defined(ENABLE_OVERLOADING)
    MenuGetActiveMethodInfo                 ,
#endif
    menuGetActive                           ,


-- ** getAttachWidget #method:getAttachWidget#

#if defined(ENABLE_OVERLOADING)
    MenuGetAttachWidgetMethodInfo           ,
#endif
    menuGetAttachWidget                     ,


-- ** getForAttachWidget #method:getForAttachWidget#

    menuGetForAttachWidget                  ,


-- ** getMonitor #method:getMonitor#

#if defined(ENABLE_OVERLOADING)
    MenuGetMonitorMethodInfo                ,
#endif
    menuGetMonitor                          ,


-- ** getReserveToggleSize #method:getReserveToggleSize#

#if defined(ENABLE_OVERLOADING)
    MenuGetReserveToggleSizeMethodInfo      ,
#endif
    menuGetReserveToggleSize                ,


-- ** getTearoffState #method:getTearoffState#

#if defined(ENABLE_OVERLOADING)
    MenuGetTearoffStateMethodInfo           ,
#endif
    menuGetTearoffState                     ,


-- ** getTitle #method:getTitle#

#if defined(ENABLE_OVERLOADING)
    MenuGetTitleMethodInfo                  ,
#endif
    menuGetTitle                            ,


-- ** new #method:new#

    menuNew                                 ,


-- ** newFromModel #method:newFromModel#

    menuNewFromModel                        ,


-- ** placeOnMonitor #method:placeOnMonitor#

#if defined(ENABLE_OVERLOADING)
    MenuPlaceOnMonitorMethodInfo            ,
#endif
    menuPlaceOnMonitor                      ,


-- ** popdown #method:popdown#

#if defined(ENABLE_OVERLOADING)
    MenuPopdownMethodInfo                   ,
#endif
    menuPopdown                             ,


-- ** popup #method:popup#

#if defined(ENABLE_OVERLOADING)
    MenuPopupMethodInfo                     ,
#endif
    menuPopup                               ,


-- ** popupAtPointer #method:popupAtPointer#

#if defined(ENABLE_OVERLOADING)
    MenuPopupAtPointerMethodInfo            ,
#endif
    menuPopupAtPointer                      ,


-- ** popupAtRect #method:popupAtRect#

#if defined(ENABLE_OVERLOADING)
    MenuPopupAtRectMethodInfo               ,
#endif
    menuPopupAtRect                         ,


-- ** popupAtWidget #method:popupAtWidget#

#if defined(ENABLE_OVERLOADING)
    MenuPopupAtWidgetMethodInfo             ,
#endif
    menuPopupAtWidget                       ,


-- ** popupForDevice #method:popupForDevice#

#if defined(ENABLE_OVERLOADING)
    MenuPopupForDeviceMethodInfo            ,
#endif
    menuPopupForDevice                      ,


-- ** reorderChild #method:reorderChild#

#if defined(ENABLE_OVERLOADING)
    MenuReorderChildMethodInfo              ,
#endif
    menuReorderChild                        ,


-- ** reposition #method:reposition#

#if defined(ENABLE_OVERLOADING)
    MenuRepositionMethodInfo                ,
#endif
    menuReposition                          ,


-- ** setAccelGroup #method:setAccelGroup#

#if defined(ENABLE_OVERLOADING)
    MenuSetAccelGroupMethodInfo             ,
#endif
    menuSetAccelGroup                       ,


-- ** setAccelPath #method:setAccelPath#

#if defined(ENABLE_OVERLOADING)
    MenuSetAccelPathMethodInfo              ,
#endif
    menuSetAccelPath                        ,


-- ** setActive #method:setActive#

#if defined(ENABLE_OVERLOADING)
    MenuSetActiveMethodInfo                 ,
#endif
    menuSetActive                           ,


-- ** setMonitor #method:setMonitor#

#if defined(ENABLE_OVERLOADING)
    MenuSetMonitorMethodInfo                ,
#endif
    menuSetMonitor                          ,


-- ** setReserveToggleSize #method:setReserveToggleSize#

#if defined(ENABLE_OVERLOADING)
    MenuSetReserveToggleSizeMethodInfo      ,
#endif
    menuSetReserveToggleSize                ,


-- ** setScreen #method:setScreen#

#if defined(ENABLE_OVERLOADING)
    MenuSetScreenMethodInfo                 ,
#endif
    menuSetScreen                           ,


-- ** setTearoffState #method:setTearoffState#

#if defined(ENABLE_OVERLOADING)
    MenuSetTearoffStateMethodInfo           ,
#endif
    menuSetTearoffState                     ,


-- ** setTitle #method:setTitle#

#if defined(ENABLE_OVERLOADING)
    MenuSetTitleMethodInfo                  ,
#endif
    menuSetTitle                            ,




 -- * Properties


-- ** accelGroup #attr:accelGroup#
-- | The accel group holding accelerators for the menu.
-- 
-- /Since: 2.14/

#if defined(ENABLE_OVERLOADING)
    MenuAccelGroupPropertyInfo              ,
#endif
    clearMenuAccelGroup                     ,
    constructMenuAccelGroup                 ,
    getMenuAccelGroup                       ,
#if defined(ENABLE_OVERLOADING)
    menuAccelGroup                          ,
#endif
    setMenuAccelGroup                       ,


-- ** accelPath #attr:accelPath#
-- | An accel path used to conveniently construct accel paths of child items.
-- 
-- /Since: 2.14/

#if defined(ENABLE_OVERLOADING)
    MenuAccelPathPropertyInfo               ,
#endif
    clearMenuAccelPath                      ,
    constructMenuAccelPath                  ,
    getMenuAccelPath                        ,
#if defined(ENABLE_OVERLOADING)
    menuAccelPath                           ,
#endif
    setMenuAccelPath                        ,


-- ** active #attr:active#
-- | The index of the currently selected menu item, or -1 if no
-- menu item is selected.
-- 
-- /Since: 2.14/

#if defined(ENABLE_OVERLOADING)
    MenuActivePropertyInfo                  ,
#endif
    constructMenuActive                     ,
    getMenuActive                           ,
#if defined(ENABLE_OVERLOADING)
    menuActive                              ,
#endif
    setMenuActive                           ,


-- ** anchorHints #attr:anchorHints#
-- | Positioning hints for aligning the menu relative to a rectangle.
-- 
-- These hints determine how the menu should be positioned in the case that
-- the menu would fall off-screen if placed in its ideal position.
-- 
-- <<https://developer.gnome.org/gtk3/stable/popup-flip.png>>
-- 
-- For example, 'GI.Gdk.Flags.AnchorHintsFlipY' will replace 'GI.Gdk.Enums.GravityNorthWest' with
-- 'GI.Gdk.Enums.GravitySouthWest' and vice versa if the menu extends beyond the
-- bottom edge of the monitor.
-- 
-- See gtk_menu_popup_at_rect (), gtk_menu_popup_at_widget (),
-- gtk_menu_popup_at_pointer (), [Menu:rectAnchorDx]("GI.Gtk.Objects.Menu#g:attr:rectAnchorDx"),
-- [Menu:rectAnchorDy]("GI.Gtk.Objects.Menu#g:attr:rectAnchorDy"), [Menu:menuTypeHint]("GI.Gtk.Objects.Menu#g:attr:menuTypeHint"), and [Menu::poppedUp]("GI.Gtk.Objects.Menu#g:signal:poppedUp").
-- 
-- /Since: 3.22/

#if defined(ENABLE_OVERLOADING)
    MenuAnchorHintsPropertyInfo             ,
#endif
    constructMenuAnchorHints                ,
    getMenuAnchorHints                      ,
#if defined(ENABLE_OVERLOADING)
    menuAnchorHints                         ,
#endif
    setMenuAnchorHints                      ,


-- ** attachWidget #attr:attachWidget#
-- | The widget the menu is attached to. Setting this property attaches
-- the menu without a t'GI.Gtk.Callbacks.MenuDetachFunc'. If you need to use a detacher,
-- use 'GI.Gtk.Objects.Menu.menuAttachToWidget' directly.
-- 
-- /Since: 2.14/

#if defined(ENABLE_OVERLOADING)
    MenuAttachWidgetPropertyInfo            ,
#endif
    clearMenuAttachWidget                   ,
    constructMenuAttachWidget               ,
    getMenuAttachWidget                     ,
#if defined(ENABLE_OVERLOADING)
    menuAttachWidget                        ,
#endif
    setMenuAttachWidget                     ,


-- ** menuTypeHint #attr:menuTypeHint#
-- | The t'GI.Gdk.Enums.WindowTypeHint' to use for the menu\'s t'GI.Gdk.Objects.Window.Window'.
-- 
-- See gtk_menu_popup_at_rect (), gtk_menu_popup_at_widget (),
-- gtk_menu_popup_at_pointer (), [Menu:anchorHints]("GI.Gtk.Objects.Menu#g:attr:anchorHints"),
-- [Menu:rectAnchorDx]("GI.Gtk.Objects.Menu#g:attr:rectAnchorDx"), [Menu:rectAnchorDy]("GI.Gtk.Objects.Menu#g:attr:rectAnchorDy"), and [Menu::poppedUp]("GI.Gtk.Objects.Menu#g:signal:poppedUp").
-- 
-- /Since: 3.22/

#if defined(ENABLE_OVERLOADING)
    MenuMenuTypeHintPropertyInfo            ,
#endif
    constructMenuMenuTypeHint               ,
    getMenuMenuTypeHint                     ,
#if defined(ENABLE_OVERLOADING)
    menuMenuTypeHint                        ,
#endif
    setMenuMenuTypeHint                     ,


-- ** monitor #attr:monitor#
-- | The monitor the menu will be popped up on.
-- 
-- /Since: 2.14/

#if defined(ENABLE_OVERLOADING)
    MenuMonitorPropertyInfo                 ,
#endif
    constructMenuMonitor                    ,
    getMenuMonitor                          ,
#if defined(ENABLE_OVERLOADING)
    menuMonitor                             ,
#endif
    setMenuMonitor                          ,


-- ** rectAnchorDx #attr:rectAnchorDx#
-- | Horizontal offset to apply to the menu, i.e. the rectangle or widget
-- anchor.
-- 
-- See gtk_menu_popup_at_rect (), gtk_menu_popup_at_widget (),
-- gtk_menu_popup_at_pointer (), [Menu:anchorHints]("GI.Gtk.Objects.Menu#g:attr:anchorHints"),
-- [Menu:rectAnchorDy]("GI.Gtk.Objects.Menu#g:attr:rectAnchorDy"), [Menu:menuTypeHint]("GI.Gtk.Objects.Menu#g:attr:menuTypeHint"), and [Menu::poppedUp]("GI.Gtk.Objects.Menu#g:signal:poppedUp").
-- 
-- /Since: 3.22/

#if defined(ENABLE_OVERLOADING)
    MenuRectAnchorDxPropertyInfo            ,
#endif
    constructMenuRectAnchorDx               ,
    getMenuRectAnchorDx                     ,
#if defined(ENABLE_OVERLOADING)
    menuRectAnchorDx                        ,
#endif
    setMenuRectAnchorDx                     ,


-- ** rectAnchorDy #attr:rectAnchorDy#
-- | Vertical offset to apply to the menu, i.e. the rectangle or widget anchor.
-- 
-- See gtk_menu_popup_at_rect (), gtk_menu_popup_at_widget (),
-- gtk_menu_popup_at_pointer (), [Menu:anchorHints]("GI.Gtk.Objects.Menu#g:attr:anchorHints"),
-- [Menu:rectAnchorDx]("GI.Gtk.Objects.Menu#g:attr:rectAnchorDx"), [Menu:menuTypeHint]("GI.Gtk.Objects.Menu#g:attr:menuTypeHint"), and [Menu::poppedUp]("GI.Gtk.Objects.Menu#g:signal:poppedUp").
-- 
-- /Since: 3.22/

#if defined(ENABLE_OVERLOADING)
    MenuRectAnchorDyPropertyInfo            ,
#endif
    constructMenuRectAnchorDy               ,
    getMenuRectAnchorDy                     ,
#if defined(ENABLE_OVERLOADING)
    menuRectAnchorDy                        ,
#endif
    setMenuRectAnchorDy                     ,


-- ** reserveToggleSize #attr:reserveToggleSize#
-- | A boolean that indicates whether the menu reserves space for
-- toggles and icons, regardless of their actual presence.
-- 
-- This property should only be changed from its default value
-- for special-purposes such as tabular menus. Regular menus that
-- are connected to a menu bar or context menus should reserve
-- toggle space for consistency.
-- 
-- /Since: 2.18/

#if defined(ENABLE_OVERLOADING)
    MenuReserveToggleSizePropertyInfo       ,
#endif
    constructMenuReserveToggleSize          ,
    getMenuReserveToggleSize                ,
#if defined(ENABLE_OVERLOADING)
    menuReserveToggleSize                   ,
#endif
    setMenuReserveToggleSize                ,


-- ** tearoffState #attr:tearoffState#
-- | A boolean that indicates whether the menu is torn-off.
-- 
-- /Since: 2.6/

#if defined(ENABLE_OVERLOADING)
    MenuTearoffStatePropertyInfo            ,
#endif
    constructMenuTearoffState               ,
    getMenuTearoffState                     ,
#if defined(ENABLE_OVERLOADING)
    menuTearoffState                        ,
#endif
    setMenuTearoffState                     ,


-- ** tearoffTitle #attr:tearoffTitle#
-- | A title that may be displayed by the window manager when this
-- menu is torn-off.

#if defined(ENABLE_OVERLOADING)
    MenuTearoffTitlePropertyInfo            ,
#endif
    clearMenuTearoffTitle                   ,
    constructMenuTearoffTitle               ,
    getMenuTearoffTitle                     ,
#if defined(ENABLE_OVERLOADING)
    menuTearoffTitle                        ,
#endif
    setMenuTearoffTitle                     ,




 -- * Signals


-- ** moveScroll #signal:moveScroll#

    MenuMoveScrollCallback                  ,
#if defined(ENABLE_OVERLOADING)
    MenuMoveScrollSignalInfo                ,
#endif
    afterMenuMoveScroll                     ,
    onMenuMoveScroll                        ,


-- ** poppedUp #signal:poppedUp#

    MenuPoppedUpCallback                    ,
#if defined(ENABLE_OVERLOADING)
    MenuPoppedUpSignalInfo                  ,
#endif
    afterMenuPoppedUp                       ,
    onMenuPoppedUp                          ,




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
import qualified GI.Gdk.Enums as Gdk.Enums
import qualified GI.Gdk.Flags as Gdk.Flags
import qualified GI.Gdk.Objects.Device as Gdk.Device
import qualified GI.Gdk.Objects.Monitor as Gdk.Monitor
import qualified GI.Gdk.Objects.Screen as Gdk.Screen
import qualified GI.Gdk.Objects.Window as Gdk.Window
import qualified GI.Gdk.Structs.Rectangle as Gdk.Rectangle
import qualified GI.Gdk.Unions.Event as Gdk.Event
import qualified GI.Gio.Objects.MenuModel as Gio.MenuModel
import qualified GI.Gtk.Callbacks as Gtk.Callbacks
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Objects.AccelGroup as Gtk.AccelGroup
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.MenuShell as Gtk.MenuShell
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype Menu = Menu (SP.ManagedPtr Menu)
    deriving (Eq)

instance SP.ManagedPtrNewtype Menu where
    toManagedPtr (Menu p) = p

foreign import ccall "gtk_menu_get_type"
    c_gtk_menu_get_type :: IO B.Types.GType

instance B.Types.TypedObject Menu where
    glibType = c_gtk_menu_get_type

instance B.Types.GObject Menu

-- | Type class for types which can be safely cast to `Menu`, for instance with `toMenu`.
class (SP.GObject o, O.IsDescendantOf Menu o) => IsMenu o
instance (SP.GObject o, O.IsDescendantOf Menu o) => IsMenu o

instance O.HasParentTypes Menu
type instance O.ParentTypes Menu = '[Gtk.MenuShell.MenuShell, Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable]

-- | Cast to `Menu`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toMenu :: (MIO.MonadIO m, IsMenu o) => o -> m Menu
toMenu = MIO.liftIO . B.ManagedPtr.unsafeCastTo Menu

-- | Convert 'Menu' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe Menu) where
    gvalueGType_ = c_gtk_menu_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr Menu)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr Menu)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject Menu ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveMenuMethod (t :: Symbol) (o :: *) :: * where
    ResolveMenuMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveMenuMethod "activateItem" o = Gtk.MenuShell.MenuShellActivateItemMethodInfo
    ResolveMenuMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveMenuMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveMenuMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveMenuMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveMenuMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveMenuMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveMenuMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveMenuMethod "append" o = Gtk.MenuShell.MenuShellAppendMethodInfo
    ResolveMenuMethod "attach" o = MenuAttachMethodInfo
    ResolveMenuMethod "attachToWidget" o = MenuAttachToWidgetMethodInfo
    ResolveMenuMethod "bindModel" o = Gtk.MenuShell.MenuShellBindModelMethodInfo
    ResolveMenuMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveMenuMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveMenuMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveMenuMethod "cancel" o = Gtk.MenuShell.MenuShellCancelMethodInfo
    ResolveMenuMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveMenuMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveMenuMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveMenuMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveMenuMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveMenuMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveMenuMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveMenuMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveMenuMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveMenuMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveMenuMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveMenuMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveMenuMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveMenuMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveMenuMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveMenuMethod "deactivate" o = Gtk.MenuShell.MenuShellDeactivateMethodInfo
    ResolveMenuMethod "deselect" o = Gtk.MenuShell.MenuShellDeselectMethodInfo
    ResolveMenuMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveMenuMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveMenuMethod "detach" o = MenuDetachMethodInfo
    ResolveMenuMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveMenuMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveMenuMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveMenuMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveMenuMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveMenuMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveMenuMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveMenuMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveMenuMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveMenuMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveMenuMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveMenuMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveMenuMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveMenuMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveMenuMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveMenuMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveMenuMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveMenuMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveMenuMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveMenuMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveMenuMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveMenuMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveMenuMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveMenuMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveMenuMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveMenuMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveMenuMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveMenuMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveMenuMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveMenuMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveMenuMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveMenuMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveMenuMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveMenuMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveMenuMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveMenuMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveMenuMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveMenuMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveMenuMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveMenuMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveMenuMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveMenuMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveMenuMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveMenuMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveMenuMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveMenuMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveMenuMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveMenuMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveMenuMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveMenuMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveMenuMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveMenuMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveMenuMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveMenuMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveMenuMethod "insert" o = Gtk.MenuShell.MenuShellInsertMethodInfo
    ResolveMenuMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveMenuMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveMenuMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveMenuMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveMenuMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveMenuMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveMenuMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveMenuMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveMenuMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveMenuMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveMenuMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveMenuMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveMenuMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveMenuMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveMenuMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveMenuMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveMenuMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveMenuMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveMenuMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveMenuMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveMenuMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveMenuMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveMenuMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveMenuMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveMenuMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveMenuMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveMenuMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveMenuMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveMenuMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveMenuMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveMenuMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveMenuMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveMenuMethod "placeOnMonitor" o = MenuPlaceOnMonitorMethodInfo
    ResolveMenuMethod "popdown" o = MenuPopdownMethodInfo
    ResolveMenuMethod "popup" o = MenuPopupMethodInfo
    ResolveMenuMethod "popupAtPointer" o = MenuPopupAtPointerMethodInfo
    ResolveMenuMethod "popupAtRect" o = MenuPopupAtRectMethodInfo
    ResolveMenuMethod "popupAtWidget" o = MenuPopupAtWidgetMethodInfo
    ResolveMenuMethod "popupForDevice" o = MenuPopupForDeviceMethodInfo
    ResolveMenuMethod "prepend" o = Gtk.MenuShell.MenuShellPrependMethodInfo
    ResolveMenuMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveMenuMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveMenuMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveMenuMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveMenuMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveMenuMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveMenuMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveMenuMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveMenuMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveMenuMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveMenuMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveMenuMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveMenuMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveMenuMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolveMenuMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveMenuMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveMenuMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveMenuMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveMenuMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveMenuMethod "reorderChild" o = MenuReorderChildMethodInfo
    ResolveMenuMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveMenuMethod "reposition" o = MenuRepositionMethodInfo
    ResolveMenuMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveMenuMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveMenuMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveMenuMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveMenuMethod "selectFirst" o = Gtk.MenuShell.MenuShellSelectFirstMethodInfo
    ResolveMenuMethod "selectItem" o = Gtk.MenuShell.MenuShellSelectItemMethodInfo
    ResolveMenuMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveMenuMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveMenuMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveMenuMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveMenuMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveMenuMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveMenuMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveMenuMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveMenuMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveMenuMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveMenuMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveMenuMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveMenuMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveMenuMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveMenuMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveMenuMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveMenuMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveMenuMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveMenuMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveMenuMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveMenuMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveMenuMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveMenuMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveMenuMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveMenuMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveMenuMethod "getAccelGroup" o = MenuGetAccelGroupMethodInfo
    ResolveMenuMethod "getAccelPath" o = MenuGetAccelPathMethodInfo
    ResolveMenuMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveMenuMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveMenuMethod "getActive" o = MenuGetActiveMethodInfo
    ResolveMenuMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveMenuMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveMenuMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveMenuMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveMenuMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveMenuMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveMenuMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveMenuMethod "getAttachWidget" o = MenuGetAttachWidgetMethodInfo
    ResolveMenuMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveMenuMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveMenuMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveMenuMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveMenuMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveMenuMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveMenuMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveMenuMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveMenuMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveMenuMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveMenuMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveMenuMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveMenuMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveMenuMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveMenuMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveMenuMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveMenuMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveMenuMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveMenuMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveMenuMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveMenuMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveMenuMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveMenuMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveMenuMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveMenuMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveMenuMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveMenuMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveMenuMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveMenuMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveMenuMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveMenuMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveMenuMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveMenuMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveMenuMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveMenuMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveMenuMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveMenuMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveMenuMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveMenuMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveMenuMethod "getMonitor" o = MenuGetMonitorMethodInfo
    ResolveMenuMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveMenuMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveMenuMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveMenuMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveMenuMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveMenuMethod "getParentShell" o = Gtk.MenuShell.MenuShellGetParentShellMethodInfo
    ResolveMenuMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveMenuMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveMenuMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveMenuMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveMenuMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveMenuMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveMenuMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveMenuMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveMenuMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveMenuMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveMenuMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveMenuMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveMenuMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveMenuMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveMenuMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveMenuMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveMenuMethod "getReserveToggleSize" o = MenuGetReserveToggleSizeMethodInfo
    ResolveMenuMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveMenuMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveMenuMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveMenuMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveMenuMethod "getSelectedItem" o = Gtk.MenuShell.MenuShellGetSelectedItemMethodInfo
    ResolveMenuMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveMenuMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveMenuMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveMenuMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveMenuMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveMenuMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveMenuMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveMenuMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveMenuMethod "getTakeFocus" o = Gtk.MenuShell.MenuShellGetTakeFocusMethodInfo
    ResolveMenuMethod "getTearoffState" o = MenuGetTearoffStateMethodInfo
    ResolveMenuMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveMenuMethod "getTitle" o = MenuGetTitleMethodInfo
    ResolveMenuMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveMenuMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveMenuMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveMenuMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveMenuMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveMenuMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveMenuMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveMenuMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveMenuMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveMenuMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveMenuMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveMenuMethod "setAccelGroup" o = MenuSetAccelGroupMethodInfo
    ResolveMenuMethod "setAccelPath" o = MenuSetAccelPathMethodInfo
    ResolveMenuMethod "setActive" o = MenuSetActiveMethodInfo
    ResolveMenuMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveMenuMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveMenuMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveMenuMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveMenuMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveMenuMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveMenuMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveMenuMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveMenuMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveMenuMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveMenuMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveMenuMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveMenuMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveMenuMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveMenuMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveMenuMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveMenuMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveMenuMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveMenuMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveMenuMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveMenuMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveMenuMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveMenuMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveMenuMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveMenuMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveMenuMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveMenuMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveMenuMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveMenuMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveMenuMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveMenuMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveMenuMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveMenuMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveMenuMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveMenuMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveMenuMethod "setMonitor" o = MenuSetMonitorMethodInfo
    ResolveMenuMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveMenuMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveMenuMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveMenuMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveMenuMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveMenuMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveMenuMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveMenuMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveMenuMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveMenuMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveMenuMethod "setReserveToggleSize" o = MenuSetReserveToggleSizeMethodInfo
    ResolveMenuMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveMenuMethod "setScreen" o = MenuSetScreenMethodInfo
    ResolveMenuMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveMenuMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveMenuMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveMenuMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveMenuMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveMenuMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveMenuMethod "setTakeFocus" o = Gtk.MenuShell.MenuShellSetTakeFocusMethodInfo
    ResolveMenuMethod "setTearoffState" o = MenuSetTearoffStateMethodInfo
    ResolveMenuMethod "setTitle" o = MenuSetTitleMethodInfo
    ResolveMenuMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveMenuMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveMenuMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveMenuMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveMenuMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveMenuMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveMenuMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveMenuMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveMenuMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveMenuMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveMenuMethod t Menu, O.OverloadedMethod info Menu p) => OL.IsLabel t (Menu -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveMenuMethod t Menu, O.OverloadedMethod info Menu p, R.HasField t Menu p) => R.HasField t Menu p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveMenuMethod t Menu, O.OverloadedMethodInfo info Menu) => OL.IsLabel t (O.MethodProxy info Menu) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal Menu::move-scroll
-- | /No description available in the introspection data./
type MenuMoveScrollCallback =
    Gtk.Enums.ScrollType
    -- ^ /@scrollType@/: a t'GI.Gtk.Enums.ScrollType'
    -> IO ()

type C_MenuMoveScrollCallback =
    Ptr Menu ->                             -- object
    CUInt ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_MenuMoveScrollCallback`.
foreign import ccall "wrapper"
    mk_MenuMoveScrollCallback :: C_MenuMoveScrollCallback -> IO (FunPtr C_MenuMoveScrollCallback)

wrap_MenuMoveScrollCallback :: 
    GObject a => (a -> MenuMoveScrollCallback) ->
    C_MenuMoveScrollCallback
wrap_MenuMoveScrollCallback gi'cb gi'selfPtr scrollType _ = do
    let scrollType' = (toEnum . fromIntegral) scrollType
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  scrollType'


-- | Connect a signal handler for the [moveScroll](#signal:moveScroll) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' menu #moveScroll callback
-- @
-- 
-- 
onMenuMoveScroll :: (IsMenu a, MonadIO m) => a -> ((?self :: a) => MenuMoveScrollCallback) -> m SignalHandlerId
onMenuMoveScroll obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_MenuMoveScrollCallback wrapped
    wrapped'' <- mk_MenuMoveScrollCallback wrapped'
    connectSignalFunPtr obj "move-scroll" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [moveScroll](#signal:moveScroll) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' menu #moveScroll callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterMenuMoveScroll :: (IsMenu a, MonadIO m) => a -> ((?self :: a) => MenuMoveScrollCallback) -> m SignalHandlerId
afterMenuMoveScroll obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_MenuMoveScrollCallback wrapped
    wrapped'' <- mk_MenuMoveScrollCallback wrapped'
    connectSignalFunPtr obj "move-scroll" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data MenuMoveScrollSignalInfo
instance SignalInfo MenuMoveScrollSignalInfo where
    type HaskellCallbackType MenuMoveScrollSignalInfo = MenuMoveScrollCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_MenuMoveScrollCallback cb
        cb'' <- mk_MenuMoveScrollCallback cb'
        connectSignalFunPtr obj "move-scroll" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu::move-scroll"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#g:signal:moveScroll"})

#endif

-- signal Menu::popped-up
-- | Emitted when the position of /@menu@/ is finalized after being popped up
-- using gtk_menu_popup_at_rect (), gtk_menu_popup_at_widget (), or
-- gtk_menu_popup_at_pointer ().
-- 
-- /@menu@/ might be flipped over the anchor rectangle in order to keep it
-- on-screen, in which case /@flippedX@/ and /@flippedY@/ will be set to 'P.True'
-- accordingly.
-- 
-- /@flippedRect@/ is the ideal position of /@menu@/ after any possible flipping,
-- but before any possible sliding. /@finalRect@/ is /@flippedRect@/, but possibly
-- translated in the case that flipping is still ineffective in keeping /@menu@/
-- on-screen.
-- 
-- <<https://developer.gnome.org/gtk3/stable/popup-slide.png>>
-- 
-- The blue menu is /@menu@/\'s ideal position, the green menu is /@flippedRect@/,
-- and the red menu is /@finalRect@/.
-- 
-- See gtk_menu_popup_at_rect (), gtk_menu_popup_at_widget (),
-- gtk_menu_popup_at_pointer (), [Menu:anchorHints]("GI.Gtk.Objects.Menu#g:attr:anchorHints"),
-- [Menu:rectAnchorDx]("GI.Gtk.Objects.Menu#g:attr:rectAnchorDx"), [Menu:rectAnchorDy]("GI.Gtk.Objects.Menu#g:attr:rectAnchorDy"), and
-- [Menu:menuTypeHint]("GI.Gtk.Objects.Menu#g:attr:menuTypeHint").
-- 
-- /Since: 3.22/
type MenuPoppedUpCallback =
    Ptr ()
    -- ^ /@flippedRect@/: the position of /@menu@/ after any possible
    --                flipping or 'P.Nothing' if the backend can\'t obtain it
    -> Ptr ()
    -- ^ /@finalRect@/: the final position of /@menu@/ or 'P.Nothing' if the
    --              backend can\'t obtain it
    -> Bool
    -- ^ /@flippedX@/: 'P.True' if the anchors were flipped horizontally
    -> Bool
    -- ^ /@flippedY@/: 'P.True' if the anchors were flipped vertically
    -> IO ()

type C_MenuPoppedUpCallback =
    Ptr Menu ->                             -- object
    Ptr () ->
    Ptr () ->
    CInt ->
    CInt ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_MenuPoppedUpCallback`.
foreign import ccall "wrapper"
    mk_MenuPoppedUpCallback :: C_MenuPoppedUpCallback -> IO (FunPtr C_MenuPoppedUpCallback)

wrap_MenuPoppedUpCallback :: 
    GObject a => (a -> MenuPoppedUpCallback) ->
    C_MenuPoppedUpCallback
wrap_MenuPoppedUpCallback gi'cb gi'selfPtr flippedRect finalRect flippedX flippedY _ = do
    let flippedX' = (/= 0) flippedX
    let flippedY' = (/= 0) flippedY
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  flippedRect finalRect flippedX' flippedY'


-- | Connect a signal handler for the [poppedUp](#signal:poppedUp) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' menu #poppedUp callback
-- @
-- 
-- 
onMenuPoppedUp :: (IsMenu a, MonadIO m) => a -> ((?self :: a) => MenuPoppedUpCallback) -> m SignalHandlerId
onMenuPoppedUp obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_MenuPoppedUpCallback wrapped
    wrapped'' <- mk_MenuPoppedUpCallback wrapped'
    connectSignalFunPtr obj "popped-up" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [poppedUp](#signal:poppedUp) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' menu #poppedUp callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterMenuPoppedUp :: (IsMenu a, MonadIO m) => a -> ((?self :: a) => MenuPoppedUpCallback) -> m SignalHandlerId
afterMenuPoppedUp obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_MenuPoppedUpCallback wrapped
    wrapped'' <- mk_MenuPoppedUpCallback wrapped'
    connectSignalFunPtr obj "popped-up" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data MenuPoppedUpSignalInfo
instance SignalInfo MenuPoppedUpSignalInfo where
    type HaskellCallbackType MenuPoppedUpSignalInfo = MenuPoppedUpCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_MenuPoppedUpCallback cb
        cb'' <- mk_MenuPoppedUpCallback cb'
        connectSignalFunPtr obj "popped-up" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu::popped-up"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#g:signal:poppedUp"})

#endif

-- VVV Prop "accel-group"
   -- Type: TInterface (Name {namespace = "Gtk", name = "AccelGroup"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just True)

-- | Get the value of the “@accel-group@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' menu #accelGroup
-- @
getMenuAccelGroup :: (MonadIO m, IsMenu o) => o -> m Gtk.AccelGroup.AccelGroup
getMenuAccelGroup obj = MIO.liftIO $ checkUnexpectedNothing "getMenuAccelGroup" $ B.Properties.getObjectPropertyObject obj "accel-group" Gtk.AccelGroup.AccelGroup

-- | Set the value of the “@accel-group@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' menu [ #accelGroup 'Data.GI.Base.Attributes.:=' value ]
-- @
setMenuAccelGroup :: (MonadIO m, IsMenu o, Gtk.AccelGroup.IsAccelGroup a) => o -> a -> m ()
setMenuAccelGroup obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "accel-group" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@accel-group@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructMenuAccelGroup :: (IsMenu o, MIO.MonadIO m, Gtk.AccelGroup.IsAccelGroup a) => a -> m (GValueConstruct o)
constructMenuAccelGroup val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "accel-group" (P.Just val)

-- | Set the value of the “@accel-group@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #accelGroup
-- @
clearMenuAccelGroup :: (MonadIO m, IsMenu o) => o -> m ()
clearMenuAccelGroup obj = liftIO $ B.Properties.setObjectPropertyObject obj "accel-group" (Nothing :: Maybe Gtk.AccelGroup.AccelGroup)

#if defined(ENABLE_OVERLOADING)
data MenuAccelGroupPropertyInfo
instance AttrInfo MenuAccelGroupPropertyInfo where
    type AttrAllowedOps MenuAccelGroupPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint MenuAccelGroupPropertyInfo = IsMenu
    type AttrSetTypeConstraint MenuAccelGroupPropertyInfo = Gtk.AccelGroup.IsAccelGroup
    type AttrTransferTypeConstraint MenuAccelGroupPropertyInfo = Gtk.AccelGroup.IsAccelGroup
    type AttrTransferType MenuAccelGroupPropertyInfo = Gtk.AccelGroup.AccelGroup
    type AttrGetType MenuAccelGroupPropertyInfo = Gtk.AccelGroup.AccelGroup
    type AttrLabel MenuAccelGroupPropertyInfo = "accel-group"
    type AttrOrigin MenuAccelGroupPropertyInfo = Menu
    attrGet = getMenuAccelGroup
    attrSet = setMenuAccelGroup
    attrTransfer _ v = do
        unsafeCastTo Gtk.AccelGroup.AccelGroup v
    attrConstruct = constructMenuAccelGroup
    attrClear = clearMenuAccelGroup
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.accelGroup"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#g:attr:accelGroup"
        })
#endif

-- VVV Prop "accel-path"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just True)

-- | Get the value of the “@accel-path@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' menu #accelPath
-- @
getMenuAccelPath :: (MonadIO m, IsMenu o) => o -> m T.Text
getMenuAccelPath obj = MIO.liftIO $ checkUnexpectedNothing "getMenuAccelPath" $ B.Properties.getObjectPropertyString obj "accel-path"

-- | Set the value of the “@accel-path@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' menu [ #accelPath 'Data.GI.Base.Attributes.:=' value ]
-- @
setMenuAccelPath :: (MonadIO m, IsMenu o) => o -> T.Text -> m ()
setMenuAccelPath obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "accel-path" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@accel-path@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructMenuAccelPath :: (IsMenu o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructMenuAccelPath val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "accel-path" (P.Just val)

-- | Set the value of the “@accel-path@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #accelPath
-- @
clearMenuAccelPath :: (MonadIO m, IsMenu o) => o -> m ()
clearMenuAccelPath obj = liftIO $ B.Properties.setObjectPropertyString obj "accel-path" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data MenuAccelPathPropertyInfo
instance AttrInfo MenuAccelPathPropertyInfo where
    type AttrAllowedOps MenuAccelPathPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint MenuAccelPathPropertyInfo = IsMenu
    type AttrSetTypeConstraint MenuAccelPathPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint MenuAccelPathPropertyInfo = (~) T.Text
    type AttrTransferType MenuAccelPathPropertyInfo = T.Text
    type AttrGetType MenuAccelPathPropertyInfo = T.Text
    type AttrLabel MenuAccelPathPropertyInfo = "accel-path"
    type AttrOrigin MenuAccelPathPropertyInfo = Menu
    attrGet = getMenuAccelPath
    attrSet = setMenuAccelPath
    attrTransfer _ v = do
        return v
    attrConstruct = constructMenuAccelPath
    attrClear = clearMenuAccelPath
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.accelPath"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#g:attr:accelPath"
        })
#endif

-- VVV Prop "active"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@active@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' menu #active
-- @
getMenuActive :: (MonadIO m, IsMenu o) => o -> m Int32
getMenuActive obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "active"

-- | Set the value of the “@active@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' menu [ #active 'Data.GI.Base.Attributes.:=' value ]
-- @
setMenuActive :: (MonadIO m, IsMenu o) => o -> Int32 -> m ()
setMenuActive obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "active" val

-- | Construct a `GValueConstruct` with valid value for the “@active@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructMenuActive :: (IsMenu o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructMenuActive val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "active" val

#if defined(ENABLE_OVERLOADING)
data MenuActivePropertyInfo
instance AttrInfo MenuActivePropertyInfo where
    type AttrAllowedOps MenuActivePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint MenuActivePropertyInfo = IsMenu
    type AttrSetTypeConstraint MenuActivePropertyInfo = (~) Int32
    type AttrTransferTypeConstraint MenuActivePropertyInfo = (~) Int32
    type AttrTransferType MenuActivePropertyInfo = Int32
    type AttrGetType MenuActivePropertyInfo = Int32
    type AttrLabel MenuActivePropertyInfo = "active"
    type AttrOrigin MenuActivePropertyInfo = Menu
    attrGet = getMenuActive
    attrSet = setMenuActive
    attrTransfer _ v = do
        return v
    attrConstruct = constructMenuActive
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.active"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#g:attr:active"
        })
#endif

-- VVV Prop "anchor-hints"
   -- Type: TInterface (Name {namespace = "Gdk", name = "AnchorHints"})
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstruct]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@anchor-hints@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' menu #anchorHints
-- @
getMenuAnchorHints :: (MonadIO m, IsMenu o) => o -> m [Gdk.Flags.AnchorHints]
getMenuAnchorHints obj = MIO.liftIO $ B.Properties.getObjectPropertyFlags obj "anchor-hints"

-- | Set the value of the “@anchor-hints@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' menu [ #anchorHints 'Data.GI.Base.Attributes.:=' value ]
-- @
setMenuAnchorHints :: (MonadIO m, IsMenu o) => o -> [Gdk.Flags.AnchorHints] -> m ()
setMenuAnchorHints obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyFlags obj "anchor-hints" val

-- | Construct a `GValueConstruct` with valid value for the “@anchor-hints@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructMenuAnchorHints :: (IsMenu o, MIO.MonadIO m) => [Gdk.Flags.AnchorHints] -> m (GValueConstruct o)
constructMenuAnchorHints val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyFlags "anchor-hints" val

#if defined(ENABLE_OVERLOADING)
data MenuAnchorHintsPropertyInfo
instance AttrInfo MenuAnchorHintsPropertyInfo where
    type AttrAllowedOps MenuAnchorHintsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint MenuAnchorHintsPropertyInfo = IsMenu
    type AttrSetTypeConstraint MenuAnchorHintsPropertyInfo = (~) [Gdk.Flags.AnchorHints]
    type AttrTransferTypeConstraint MenuAnchorHintsPropertyInfo = (~) [Gdk.Flags.AnchorHints]
    type AttrTransferType MenuAnchorHintsPropertyInfo = [Gdk.Flags.AnchorHints]
    type AttrGetType MenuAnchorHintsPropertyInfo = [Gdk.Flags.AnchorHints]
    type AttrLabel MenuAnchorHintsPropertyInfo = "anchor-hints"
    type AttrOrigin MenuAnchorHintsPropertyInfo = Menu
    attrGet = getMenuAnchorHints
    attrSet = setMenuAnchorHints
    attrTransfer _ v = do
        return v
    attrConstruct = constructMenuAnchorHints
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.anchorHints"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#g:attr:anchorHints"
        })
#endif

-- VVV Prop "attach-widget"
   -- Type: TInterface (Name {namespace = "Gtk", name = "Widget"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Nothing)

-- | Get the value of the “@attach-widget@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' menu #attachWidget
-- @
getMenuAttachWidget :: (MonadIO m, IsMenu o) => o -> m Gtk.Widget.Widget
getMenuAttachWidget obj = MIO.liftIO $ checkUnexpectedNothing "getMenuAttachWidget" $ B.Properties.getObjectPropertyObject obj "attach-widget" Gtk.Widget.Widget

-- | Set the value of the “@attach-widget@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' menu [ #attachWidget 'Data.GI.Base.Attributes.:=' value ]
-- @
setMenuAttachWidget :: (MonadIO m, IsMenu o, Gtk.Widget.IsWidget a) => o -> a -> m ()
setMenuAttachWidget obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "attach-widget" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@attach-widget@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructMenuAttachWidget :: (IsMenu o, MIO.MonadIO m, Gtk.Widget.IsWidget a) => a -> m (GValueConstruct o)
constructMenuAttachWidget val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "attach-widget" (P.Just val)

-- | Set the value of the “@attach-widget@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #attachWidget
-- @
clearMenuAttachWidget :: (MonadIO m, IsMenu o) => o -> m ()
clearMenuAttachWidget obj = liftIO $ B.Properties.setObjectPropertyObject obj "attach-widget" (Nothing :: Maybe Gtk.Widget.Widget)

#if defined(ENABLE_OVERLOADING)
data MenuAttachWidgetPropertyInfo
instance AttrInfo MenuAttachWidgetPropertyInfo where
    type AttrAllowedOps MenuAttachWidgetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint MenuAttachWidgetPropertyInfo = IsMenu
    type AttrSetTypeConstraint MenuAttachWidgetPropertyInfo = Gtk.Widget.IsWidget
    type AttrTransferTypeConstraint MenuAttachWidgetPropertyInfo = Gtk.Widget.IsWidget
    type AttrTransferType MenuAttachWidgetPropertyInfo = Gtk.Widget.Widget
    type AttrGetType MenuAttachWidgetPropertyInfo = Gtk.Widget.Widget
    type AttrLabel MenuAttachWidgetPropertyInfo = "attach-widget"
    type AttrOrigin MenuAttachWidgetPropertyInfo = Menu
    attrGet = getMenuAttachWidget
    attrSet = setMenuAttachWidget
    attrTransfer _ v = do
        unsafeCastTo Gtk.Widget.Widget v
    attrConstruct = constructMenuAttachWidget
    attrClear = clearMenuAttachWidget
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.attachWidget"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#g:attr:attachWidget"
        })
#endif

-- VVV Prop "menu-type-hint"
   -- Type: TInterface (Name {namespace = "Gdk", name = "WindowTypeHint"})
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstruct]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@menu-type-hint@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' menu #menuTypeHint
-- @
getMenuMenuTypeHint :: (MonadIO m, IsMenu o) => o -> m Gdk.Enums.WindowTypeHint
getMenuMenuTypeHint obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "menu-type-hint"

-- | Set the value of the “@menu-type-hint@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' menu [ #menuTypeHint 'Data.GI.Base.Attributes.:=' value ]
-- @
setMenuMenuTypeHint :: (MonadIO m, IsMenu o) => o -> Gdk.Enums.WindowTypeHint -> m ()
setMenuMenuTypeHint obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "menu-type-hint" val

-- | Construct a `GValueConstruct` with valid value for the “@menu-type-hint@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructMenuMenuTypeHint :: (IsMenu o, MIO.MonadIO m) => Gdk.Enums.WindowTypeHint -> m (GValueConstruct o)
constructMenuMenuTypeHint val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "menu-type-hint" val

#if defined(ENABLE_OVERLOADING)
data MenuMenuTypeHintPropertyInfo
instance AttrInfo MenuMenuTypeHintPropertyInfo where
    type AttrAllowedOps MenuMenuTypeHintPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint MenuMenuTypeHintPropertyInfo = IsMenu
    type AttrSetTypeConstraint MenuMenuTypeHintPropertyInfo = (~) Gdk.Enums.WindowTypeHint
    type AttrTransferTypeConstraint MenuMenuTypeHintPropertyInfo = (~) Gdk.Enums.WindowTypeHint
    type AttrTransferType MenuMenuTypeHintPropertyInfo = Gdk.Enums.WindowTypeHint
    type AttrGetType MenuMenuTypeHintPropertyInfo = Gdk.Enums.WindowTypeHint
    type AttrLabel MenuMenuTypeHintPropertyInfo = "menu-type-hint"
    type AttrOrigin MenuMenuTypeHintPropertyInfo = Menu
    attrGet = getMenuMenuTypeHint
    attrSet = setMenuMenuTypeHint
    attrTransfer _ v = do
        return v
    attrConstruct = constructMenuMenuTypeHint
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.menuTypeHint"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#g:attr:menuTypeHint"
        })
#endif

-- VVV Prop "monitor"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@monitor@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' menu #monitor
-- @
getMenuMonitor :: (MonadIO m, IsMenu o) => o -> m Int32
getMenuMonitor obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "monitor"

-- | Set the value of the “@monitor@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' menu [ #monitor 'Data.GI.Base.Attributes.:=' value ]
-- @
setMenuMonitor :: (MonadIO m, IsMenu o) => o -> Int32 -> m ()
setMenuMonitor obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "monitor" val

-- | Construct a `GValueConstruct` with valid value for the “@monitor@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructMenuMonitor :: (IsMenu o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructMenuMonitor val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "monitor" val

#if defined(ENABLE_OVERLOADING)
data MenuMonitorPropertyInfo
instance AttrInfo MenuMonitorPropertyInfo where
    type AttrAllowedOps MenuMonitorPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint MenuMonitorPropertyInfo = IsMenu
    type AttrSetTypeConstraint MenuMonitorPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint MenuMonitorPropertyInfo = (~) Int32
    type AttrTransferType MenuMonitorPropertyInfo = Int32
    type AttrGetType MenuMonitorPropertyInfo = Int32
    type AttrLabel MenuMonitorPropertyInfo = "monitor"
    type AttrOrigin MenuMonitorPropertyInfo = Menu
    attrGet = getMenuMonitor
    attrSet = setMenuMonitor
    attrTransfer _ v = do
        return v
    attrConstruct = constructMenuMonitor
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.monitor"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#g:attr:monitor"
        })
#endif

-- VVV Prop "rect-anchor-dx"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstruct]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@rect-anchor-dx@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' menu #rectAnchorDx
-- @
getMenuRectAnchorDx :: (MonadIO m, IsMenu o) => o -> m Int32
getMenuRectAnchorDx obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "rect-anchor-dx"

-- | Set the value of the “@rect-anchor-dx@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' menu [ #rectAnchorDx 'Data.GI.Base.Attributes.:=' value ]
-- @
setMenuRectAnchorDx :: (MonadIO m, IsMenu o) => o -> Int32 -> m ()
setMenuRectAnchorDx obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "rect-anchor-dx" val

-- | Construct a `GValueConstruct` with valid value for the “@rect-anchor-dx@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructMenuRectAnchorDx :: (IsMenu o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructMenuRectAnchorDx val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "rect-anchor-dx" val

#if defined(ENABLE_OVERLOADING)
data MenuRectAnchorDxPropertyInfo
instance AttrInfo MenuRectAnchorDxPropertyInfo where
    type AttrAllowedOps MenuRectAnchorDxPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint MenuRectAnchorDxPropertyInfo = IsMenu
    type AttrSetTypeConstraint MenuRectAnchorDxPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint MenuRectAnchorDxPropertyInfo = (~) Int32
    type AttrTransferType MenuRectAnchorDxPropertyInfo = Int32
    type AttrGetType MenuRectAnchorDxPropertyInfo = Int32
    type AttrLabel MenuRectAnchorDxPropertyInfo = "rect-anchor-dx"
    type AttrOrigin MenuRectAnchorDxPropertyInfo = Menu
    attrGet = getMenuRectAnchorDx
    attrSet = setMenuRectAnchorDx
    attrTransfer _ v = do
        return v
    attrConstruct = constructMenuRectAnchorDx
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.rectAnchorDx"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#g:attr:rectAnchorDx"
        })
#endif

-- VVV Prop "rect-anchor-dy"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstruct]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@rect-anchor-dy@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' menu #rectAnchorDy
-- @
getMenuRectAnchorDy :: (MonadIO m, IsMenu o) => o -> m Int32
getMenuRectAnchorDy obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "rect-anchor-dy"

-- | Set the value of the “@rect-anchor-dy@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' menu [ #rectAnchorDy 'Data.GI.Base.Attributes.:=' value ]
-- @
setMenuRectAnchorDy :: (MonadIO m, IsMenu o) => o -> Int32 -> m ()
setMenuRectAnchorDy obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "rect-anchor-dy" val

-- | Construct a `GValueConstruct` with valid value for the “@rect-anchor-dy@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructMenuRectAnchorDy :: (IsMenu o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructMenuRectAnchorDy val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "rect-anchor-dy" val

#if defined(ENABLE_OVERLOADING)
data MenuRectAnchorDyPropertyInfo
instance AttrInfo MenuRectAnchorDyPropertyInfo where
    type AttrAllowedOps MenuRectAnchorDyPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint MenuRectAnchorDyPropertyInfo = IsMenu
    type AttrSetTypeConstraint MenuRectAnchorDyPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint MenuRectAnchorDyPropertyInfo = (~) Int32
    type AttrTransferType MenuRectAnchorDyPropertyInfo = Int32
    type AttrGetType MenuRectAnchorDyPropertyInfo = Int32
    type AttrLabel MenuRectAnchorDyPropertyInfo = "rect-anchor-dy"
    type AttrOrigin MenuRectAnchorDyPropertyInfo = Menu
    attrGet = getMenuRectAnchorDy
    attrSet = setMenuRectAnchorDy
    attrTransfer _ v = do
        return v
    attrConstruct = constructMenuRectAnchorDy
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.rectAnchorDy"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#g:attr:rectAnchorDy"
        })
#endif

-- VVV Prop "reserve-toggle-size"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@reserve-toggle-size@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' menu #reserveToggleSize
-- @
getMenuReserveToggleSize :: (MonadIO m, IsMenu o) => o -> m Bool
getMenuReserveToggleSize obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "reserve-toggle-size"

-- | Set the value of the “@reserve-toggle-size@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' menu [ #reserveToggleSize 'Data.GI.Base.Attributes.:=' value ]
-- @
setMenuReserveToggleSize :: (MonadIO m, IsMenu o) => o -> Bool -> m ()
setMenuReserveToggleSize obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "reserve-toggle-size" val

-- | Construct a `GValueConstruct` with valid value for the “@reserve-toggle-size@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructMenuReserveToggleSize :: (IsMenu o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructMenuReserveToggleSize val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "reserve-toggle-size" val

#if defined(ENABLE_OVERLOADING)
data MenuReserveToggleSizePropertyInfo
instance AttrInfo MenuReserveToggleSizePropertyInfo where
    type AttrAllowedOps MenuReserveToggleSizePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint MenuReserveToggleSizePropertyInfo = IsMenu
    type AttrSetTypeConstraint MenuReserveToggleSizePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint MenuReserveToggleSizePropertyInfo = (~) Bool
    type AttrTransferType MenuReserveToggleSizePropertyInfo = Bool
    type AttrGetType MenuReserveToggleSizePropertyInfo = Bool
    type AttrLabel MenuReserveToggleSizePropertyInfo = "reserve-toggle-size"
    type AttrOrigin MenuReserveToggleSizePropertyInfo = Menu
    attrGet = getMenuReserveToggleSize
    attrSet = setMenuReserveToggleSize
    attrTransfer _ v = do
        return v
    attrConstruct = constructMenuReserveToggleSize
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.reserveToggleSize"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#g:attr:reserveToggleSize"
        })
#endif

-- VVV Prop "tearoff-state"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@tearoff-state@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' menu #tearoffState
-- @
getMenuTearoffState :: (MonadIO m, IsMenu o) => o -> m Bool
getMenuTearoffState obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "tearoff-state"

-- | Set the value of the “@tearoff-state@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' menu [ #tearoffState 'Data.GI.Base.Attributes.:=' value ]
-- @
setMenuTearoffState :: (MonadIO m, IsMenu o) => o -> Bool -> m ()
setMenuTearoffState obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "tearoff-state" val

-- | Construct a `GValueConstruct` with valid value for the “@tearoff-state@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructMenuTearoffState :: (IsMenu o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructMenuTearoffState val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "tearoff-state" val

#if defined(ENABLE_OVERLOADING)
data MenuTearoffStatePropertyInfo
instance AttrInfo MenuTearoffStatePropertyInfo where
    type AttrAllowedOps MenuTearoffStatePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint MenuTearoffStatePropertyInfo = IsMenu
    type AttrSetTypeConstraint MenuTearoffStatePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint MenuTearoffStatePropertyInfo = (~) Bool
    type AttrTransferType MenuTearoffStatePropertyInfo = Bool
    type AttrGetType MenuTearoffStatePropertyInfo = Bool
    type AttrLabel MenuTearoffStatePropertyInfo = "tearoff-state"
    type AttrOrigin MenuTearoffStatePropertyInfo = Menu
    attrGet = getMenuTearoffState
    attrSet = setMenuTearoffState
    attrTransfer _ v = do
        return v
    attrConstruct = constructMenuTearoffState
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.tearoffState"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#g:attr:tearoffState"
        })
#endif

-- VVV Prop "tearoff-title"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@tearoff-title@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' menu #tearoffTitle
-- @
getMenuTearoffTitle :: (MonadIO m, IsMenu o) => o -> m (Maybe T.Text)
getMenuTearoffTitle obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "tearoff-title"

-- | Set the value of the “@tearoff-title@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' menu [ #tearoffTitle 'Data.GI.Base.Attributes.:=' value ]
-- @
setMenuTearoffTitle :: (MonadIO m, IsMenu o) => o -> T.Text -> m ()
setMenuTearoffTitle obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "tearoff-title" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@tearoff-title@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructMenuTearoffTitle :: (IsMenu o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructMenuTearoffTitle val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "tearoff-title" (P.Just val)

-- | Set the value of the “@tearoff-title@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #tearoffTitle
-- @
clearMenuTearoffTitle :: (MonadIO m, IsMenu o) => o -> m ()
clearMenuTearoffTitle obj = liftIO $ B.Properties.setObjectPropertyString obj "tearoff-title" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data MenuTearoffTitlePropertyInfo
instance AttrInfo MenuTearoffTitlePropertyInfo where
    type AttrAllowedOps MenuTearoffTitlePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint MenuTearoffTitlePropertyInfo = IsMenu
    type AttrSetTypeConstraint MenuTearoffTitlePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint MenuTearoffTitlePropertyInfo = (~) T.Text
    type AttrTransferType MenuTearoffTitlePropertyInfo = T.Text
    type AttrGetType MenuTearoffTitlePropertyInfo = (Maybe T.Text)
    type AttrLabel MenuTearoffTitlePropertyInfo = "tearoff-title"
    type AttrOrigin MenuTearoffTitlePropertyInfo = Menu
    attrGet = getMenuTearoffTitle
    attrSet = setMenuTearoffTitle
    attrTransfer _ v = do
        return v
    attrConstruct = constructMenuTearoffTitle
    attrClear = clearMenuTearoffTitle
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.tearoffTitle"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#g:attr:tearoffTitle"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList Menu
type instance O.AttributeList Menu = MenuAttributeList
type MenuAttributeList = ('[ '("accelGroup", MenuAccelGroupPropertyInfo), '("accelPath", MenuAccelPathPropertyInfo), '("active", MenuActivePropertyInfo), '("anchorHints", MenuAnchorHintsPropertyInfo), '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("attachWidget", MenuAttachWidgetPropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("menuTypeHint", MenuMenuTypeHintPropertyInfo), '("monitor", MenuMonitorPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("rectAnchorDx", MenuRectAnchorDxPropertyInfo), '("rectAnchorDy", MenuRectAnchorDyPropertyInfo), '("reserveToggleSize", MenuReserveToggleSizePropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("takeFocus", Gtk.MenuShell.MenuShellTakeFocusPropertyInfo), '("tearoffState", MenuTearoffStatePropertyInfo), '("tearoffTitle", MenuTearoffTitlePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
menuAccelGroup :: AttrLabelProxy "accelGroup"
menuAccelGroup = AttrLabelProxy

menuAccelPath :: AttrLabelProxy "accelPath"
menuAccelPath = AttrLabelProxy

menuActive :: AttrLabelProxy "active"
menuActive = AttrLabelProxy

menuAnchorHints :: AttrLabelProxy "anchorHints"
menuAnchorHints = AttrLabelProxy

menuAttachWidget :: AttrLabelProxy "attachWidget"
menuAttachWidget = AttrLabelProxy

menuMenuTypeHint :: AttrLabelProxy "menuTypeHint"
menuMenuTypeHint = AttrLabelProxy

menuMonitor :: AttrLabelProxy "monitor"
menuMonitor = AttrLabelProxy

menuRectAnchorDx :: AttrLabelProxy "rectAnchorDx"
menuRectAnchorDx = AttrLabelProxy

menuRectAnchorDy :: AttrLabelProxy "rectAnchorDy"
menuRectAnchorDy = AttrLabelProxy

menuReserveToggleSize :: AttrLabelProxy "reserveToggleSize"
menuReserveToggleSize = AttrLabelProxy

menuTearoffState :: AttrLabelProxy "tearoffState"
menuTearoffState = AttrLabelProxy

menuTearoffTitle :: AttrLabelProxy "tearoffTitle"
menuTearoffTitle = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList Menu = MenuSignalList
type MenuSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("activateCurrent", Gtk.MenuShell.MenuShellActivateCurrentSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("cancel", Gtk.MenuShell.MenuShellCancelSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("cycleFocus", Gtk.MenuShell.MenuShellCycleFocusSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deactivate", Gtk.MenuShell.MenuShellDeactivateSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("insert", Gtk.MenuShell.MenuShellInsertSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveCurrent", Gtk.MenuShell.MenuShellMoveCurrentSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("moveScroll", MenuMoveScrollSignalInfo), '("moveSelected", Gtk.MenuShell.MenuShellMoveSelectedSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("poppedUp", MenuPoppedUpSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionDone", Gtk.MenuShell.MenuShellSelectionDoneSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method Menu::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Menu" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_menu_new" gtk_menu_new :: 
    IO (Ptr Menu)

-- | Creates a new t'GI.Gtk.Objects.Menu.Menu'
menuNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m Menu
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.Menu.Menu'
menuNew  = liftIO $ do
    result <- gtk_menu_new
    checkUnexpectedReturnNULL "menuNew" result
    result' <- (newObject Menu) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Menu::new_from_model
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "model"
--           , argType =
--               TInterface Name { namespace = "Gio" , name = "MenuModel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GMenuModel" , sinceVersion = Nothing }
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

foreign import ccall "gtk_menu_new_from_model" gtk_menu_new_from_model :: 
    Ptr Gio.MenuModel.MenuModel ->          -- model : TInterface (Name {namespace = "Gio", name = "MenuModel"})
    IO (Ptr Menu)

-- | Creates a t'GI.Gtk.Objects.Menu.Menu' and populates it with menu items and
-- submenus according to /@model@/.
-- 
-- The created menu items are connected to actions found in the
-- t'GI.Gtk.Objects.ApplicationWindow.ApplicationWindow' to which the menu belongs - typically
-- by means of being attached to a widget (see 'GI.Gtk.Objects.Menu.menuAttachToWidget')
-- that is contained within the @/GtkApplicationWindows/@ widget hierarchy.
-- 
-- Actions can also be added using 'GI.Gtk.Objects.Widget.widgetInsertActionGroup' on the menu\'s
-- attach widget or on any of its parent widgets.
-- 
-- /Since: 3.4/
menuNewFromModel ::
    (B.CallStack.HasCallStack, MonadIO m, Gio.MenuModel.IsMenuModel a) =>
    a
    -- ^ /@model@/: a t'GI.Gio.Objects.MenuModel.MenuModel'
    -> m Menu
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.Menu.Menu'
menuNewFromModel model = liftIO $ do
    model' <- unsafeManagedPtrCastPtr model
    result <- gtk_menu_new_from_model model'
    checkUnexpectedReturnNULL "menuNewFromModel" result
    result' <- (newObject Menu) result
    touchManagedPtr model
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Menu::attach
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Menu" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenu" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "child"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenuItem" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "left_attach"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The column number to attach the left side of the item to"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "right_attach"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The column number to attach the right side of the item to"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "top_attach"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The row number to attach the top of the item to"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "bottom_attach"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The row number to attach the bottom of the item to"
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

foreign import ccall "gtk_menu_attach" gtk_menu_attach :: 
    Ptr Menu ->                             -- menu : TInterface (Name {namespace = "Gtk", name = "Menu"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Word32 ->                               -- left_attach : TBasicType TUInt
    Word32 ->                               -- right_attach : TBasicType TUInt
    Word32 ->                               -- top_attach : TBasicType TUInt
    Word32 ->                               -- bottom_attach : TBasicType TUInt
    IO ()

-- | Adds a new t'GI.Gtk.Objects.MenuItem.MenuItem' to a (table) menu. The number of “cells” that
-- an item will occupy is specified by /@leftAttach@/, /@rightAttach@/,
-- /@topAttach@/ and /@bottomAttach@/. These each represent the leftmost,
-- rightmost, uppermost and lower column and row numbers of the table.
-- (Columns and rows are indexed from zero).
-- 
-- Note that this function is not related to 'GI.Gtk.Objects.Menu.menuDetach'.
-- 
-- /Since: 2.4/
menuAttach ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenu a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@menu@/: a t'GI.Gtk.Objects.Menu.Menu'
    -> b
    -- ^ /@child@/: a t'GI.Gtk.Objects.MenuItem.MenuItem'
    -> Word32
    -- ^ /@leftAttach@/: The column number to attach the left side of the item to
    -> Word32
    -- ^ /@rightAttach@/: The column number to attach the right side of the item to
    -> Word32
    -- ^ /@topAttach@/: The row number to attach the top of the item to
    -> Word32
    -- ^ /@bottomAttach@/: The row number to attach the bottom of the item to
    -> m ()
menuAttach menu child leftAttach rightAttach topAttach bottomAttach = liftIO $ do
    menu' <- unsafeManagedPtrCastPtr menu
    child' <- unsafeManagedPtrCastPtr child
    gtk_menu_attach menu' child' leftAttach rightAttach topAttach bottomAttach
    touchManagedPtr menu
    touchManagedPtr child
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuAttachMethodInfo
instance (signature ~ (b -> Word32 -> Word32 -> Word32 -> Word32 -> m ()), MonadIO m, IsMenu a, Gtk.Widget.IsWidget b) => O.OverloadedMethod MenuAttachMethodInfo a signature where
    overloadedMethod = menuAttach

instance O.OverloadedMethodInfo MenuAttachMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.menuAttach",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#v:menuAttach"
        })


#endif

-- method Menu::attach_to_widget
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Menu" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenu" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "attach_widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the #GtkWidget that the menu will be attached to"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "detacher"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MenuDetachFunc" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the user supplied callback function\n            that will be called when the menu calls gtk_menu_detach()"
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

foreign import ccall "gtk_menu_attach_to_widget" gtk_menu_attach_to_widget :: 
    Ptr Menu ->                             -- menu : TInterface (Name {namespace = "Gtk", name = "Menu"})
    Ptr Gtk.Widget.Widget ->                -- attach_widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    FunPtr Gtk.Callbacks.C_MenuDetachFunc -> -- detacher : TInterface (Name {namespace = "Gtk", name = "MenuDetachFunc"})
    IO ()

-- | Attaches the menu to the widget and provides a callback function
-- that will be invoked when the menu calls 'GI.Gtk.Objects.Menu.menuDetach' during
-- its destruction.
-- 
-- If the menu is attached to the widget then it will be destroyed
-- when the widget is destroyed, as if it was a child widget.
-- An attached menu will also move between screens correctly if the
-- widgets moves between screens.
menuAttachToWidget ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenu a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@menu@/: a t'GI.Gtk.Objects.Menu.Menu'
    -> b
    -- ^ /@attachWidget@/: the t'GI.Gtk.Objects.Widget.Widget' that the menu will be attached to
    -> Maybe (Gtk.Callbacks.MenuDetachFunc)
    -- ^ /@detacher@/: the user supplied callback function
    --             that will be called when the menu calls 'GI.Gtk.Objects.Menu.menuDetach'
    -> m ()
menuAttachToWidget menu attachWidget detacher = liftIO $ do
    menu' <- unsafeManagedPtrCastPtr menu
    attachWidget' <- unsafeManagedPtrCastPtr attachWidget
    maybeDetacher <- case detacher of
        Nothing -> return (castPtrToFunPtr nullPtr)
        Just jDetacher -> do
            ptrdetacher <- callocMem :: IO (Ptr (FunPtr Gtk.Callbacks.C_MenuDetachFunc))
            jDetacher' <- Gtk.Callbacks.mk_MenuDetachFunc (Gtk.Callbacks.wrap_MenuDetachFunc (Just ptrdetacher) jDetacher)
            poke ptrdetacher jDetacher'
            return jDetacher'
    gtk_menu_attach_to_widget menu' attachWidget' maybeDetacher
    touchManagedPtr menu
    touchManagedPtr attachWidget
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuAttachToWidgetMethodInfo
instance (signature ~ (b -> Maybe (Gtk.Callbacks.MenuDetachFunc) -> m ()), MonadIO m, IsMenu a, Gtk.Widget.IsWidget b) => O.OverloadedMethod MenuAttachToWidgetMethodInfo a signature where
    overloadedMethod = menuAttachToWidget

instance O.OverloadedMethodInfo MenuAttachToWidgetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.menuAttachToWidget",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#v:menuAttachToWidget"
        })


#endif

-- method Menu::detach
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Menu" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenu" , sinceVersion = Nothing }
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

foreign import ccall "gtk_menu_detach" gtk_menu_detach :: 
    Ptr Menu ->                             -- menu : TInterface (Name {namespace = "Gtk", name = "Menu"})
    IO ()

-- | Detaches the menu from the widget to which it had been attached.
-- This function will call the callback function, /@detacher@/, provided
-- when the 'GI.Gtk.Objects.Menu.menuAttachToWidget' function was called.
menuDetach ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenu a) =>
    a
    -- ^ /@menu@/: a t'GI.Gtk.Objects.Menu.Menu'
    -> m ()
menuDetach menu = liftIO $ do
    menu' <- unsafeManagedPtrCastPtr menu
    gtk_menu_detach menu'
    touchManagedPtr menu
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuDetachMethodInfo
instance (signature ~ (m ()), MonadIO m, IsMenu a) => O.OverloadedMethod MenuDetachMethodInfo a signature where
    overloadedMethod = menuDetach

instance O.OverloadedMethodInfo MenuDetachMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.menuDetach",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#v:menuDetach"
        })


#endif

-- method Menu::get_accel_group
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Menu" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenu" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "AccelGroup" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_menu_get_accel_group" gtk_menu_get_accel_group :: 
    Ptr Menu ->                             -- menu : TInterface (Name {namespace = "Gtk", name = "Menu"})
    IO (Ptr Gtk.AccelGroup.AccelGroup)

-- | Gets the t'GI.Gtk.Objects.AccelGroup.AccelGroup' which holds global accelerators for the
-- menu. See 'GI.Gtk.Objects.Menu.menuSetAccelGroup'.
menuGetAccelGroup ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenu a) =>
    a
    -- ^ /@menu@/: a t'GI.Gtk.Objects.Menu.Menu'
    -> m Gtk.AccelGroup.AccelGroup
    -- ^ __Returns:__ the t'GI.Gtk.Objects.AccelGroup.AccelGroup' associated with the menu
menuGetAccelGroup menu = liftIO $ do
    menu' <- unsafeManagedPtrCastPtr menu
    result <- gtk_menu_get_accel_group menu'
    checkUnexpectedReturnNULL "menuGetAccelGroup" result
    result' <- (newObject Gtk.AccelGroup.AccelGroup) result
    touchManagedPtr menu
    return result'

#if defined(ENABLE_OVERLOADING)
data MenuGetAccelGroupMethodInfo
instance (signature ~ (m Gtk.AccelGroup.AccelGroup), MonadIO m, IsMenu a) => O.OverloadedMethod MenuGetAccelGroupMethodInfo a signature where
    overloadedMethod = menuGetAccelGroup

instance O.OverloadedMethodInfo MenuGetAccelGroupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.menuGetAccelGroup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#v:menuGetAccelGroup"
        })


#endif

-- method Menu::get_accel_path
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Menu" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a valid #GtkMenu" , sinceVersion = Nothing }
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

foreign import ccall "gtk_menu_get_accel_path" gtk_menu_get_accel_path :: 
    Ptr Menu ->                             -- menu : TInterface (Name {namespace = "Gtk", name = "Menu"})
    IO CString

-- | Retrieves the accelerator path set on the menu.
-- 
-- /Since: 2.14/
menuGetAccelPath ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenu a) =>
    a
    -- ^ /@menu@/: a valid t'GI.Gtk.Objects.Menu.Menu'
    -> m T.Text
    -- ^ __Returns:__ the accelerator path set on the menu.
menuGetAccelPath menu = liftIO $ do
    menu' <- unsafeManagedPtrCastPtr menu
    result <- gtk_menu_get_accel_path menu'
    checkUnexpectedReturnNULL "menuGetAccelPath" result
    result' <- cstringToText result
    touchManagedPtr menu
    return result'

#if defined(ENABLE_OVERLOADING)
data MenuGetAccelPathMethodInfo
instance (signature ~ (m T.Text), MonadIO m, IsMenu a) => O.OverloadedMethod MenuGetAccelPathMethodInfo a signature where
    overloadedMethod = menuGetAccelPath

instance O.OverloadedMethodInfo MenuGetAccelPathMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.menuGetAccelPath",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#v:menuGetAccelPath"
        })


#endif

-- method Menu::get_active
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Menu" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenu" , sinceVersion = Nothing }
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

foreign import ccall "gtk_menu_get_active" gtk_menu_get_active :: 
    Ptr Menu ->                             -- menu : TInterface (Name {namespace = "Gtk", name = "Menu"})
    IO (Ptr Gtk.Widget.Widget)

-- | Returns the selected menu item from the menu.  This is used by the
-- t'GI.Gtk.Objects.ComboBox.ComboBox'.
menuGetActive ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenu a) =>
    a
    -- ^ /@menu@/: a t'GI.Gtk.Objects.Menu.Menu'
    -> m Gtk.Widget.Widget
    -- ^ __Returns:__ the t'GI.Gtk.Objects.MenuItem.MenuItem' that was last selected
    --          in the menu.  If a selection has not yet been made, the
    --          first menu item is selected.
menuGetActive menu = liftIO $ do
    menu' <- unsafeManagedPtrCastPtr menu
    result <- gtk_menu_get_active menu'
    checkUnexpectedReturnNULL "menuGetActive" result
    result' <- (newObject Gtk.Widget.Widget) result
    touchManagedPtr menu
    return result'

#if defined(ENABLE_OVERLOADING)
data MenuGetActiveMethodInfo
instance (signature ~ (m Gtk.Widget.Widget), MonadIO m, IsMenu a) => O.OverloadedMethod MenuGetActiveMethodInfo a signature where
    overloadedMethod = menuGetActive

instance O.OverloadedMethodInfo MenuGetActiveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.menuGetActive",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#v:menuGetActive"
        })


#endif

-- method Menu::get_attach_widget
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Menu" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenu" , sinceVersion = Nothing }
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

foreign import ccall "gtk_menu_get_attach_widget" gtk_menu_get_attach_widget :: 
    Ptr Menu ->                             -- menu : TInterface (Name {namespace = "Gtk", name = "Menu"})
    IO (Ptr Gtk.Widget.Widget)

-- | Returns the t'GI.Gtk.Objects.Widget.Widget' that the menu is attached to.
menuGetAttachWidget ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenu a) =>
    a
    -- ^ /@menu@/: a t'GI.Gtk.Objects.Menu.Menu'
    -> m Gtk.Widget.Widget
    -- ^ __Returns:__ the t'GI.Gtk.Objects.Widget.Widget' that the menu is attached to
menuGetAttachWidget menu = liftIO $ do
    menu' <- unsafeManagedPtrCastPtr menu
    result <- gtk_menu_get_attach_widget menu'
    checkUnexpectedReturnNULL "menuGetAttachWidget" result
    result' <- (newObject Gtk.Widget.Widget) result
    touchManagedPtr menu
    return result'

#if defined(ENABLE_OVERLOADING)
data MenuGetAttachWidgetMethodInfo
instance (signature ~ (m Gtk.Widget.Widget), MonadIO m, IsMenu a) => O.OverloadedMethod MenuGetAttachWidgetMethodInfo a signature where
    overloadedMethod = menuGetAttachWidget

instance O.OverloadedMethodInfo MenuGetAttachWidgetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.menuGetAttachWidget",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#v:menuGetAttachWidget"
        })


#endif

-- method Menu::get_monitor
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Menu" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenu" , sinceVersion = Nothing }
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

foreign import ccall "gtk_menu_get_monitor" gtk_menu_get_monitor :: 
    Ptr Menu ->                             -- menu : TInterface (Name {namespace = "Gtk", name = "Menu"})
    IO Int32

-- | Retrieves the number of the monitor on which to show the menu.
-- 
-- /Since: 2.14/
menuGetMonitor ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenu a) =>
    a
    -- ^ /@menu@/: a t'GI.Gtk.Objects.Menu.Menu'
    -> m Int32
    -- ^ __Returns:__ the number of the monitor on which the menu should
    --    be popped up or -1, if no monitor has been set
menuGetMonitor menu = liftIO $ do
    menu' <- unsafeManagedPtrCastPtr menu
    result <- gtk_menu_get_monitor menu'
    touchManagedPtr menu
    return result

#if defined(ENABLE_OVERLOADING)
data MenuGetMonitorMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsMenu a) => O.OverloadedMethod MenuGetMonitorMethodInfo a signature where
    overloadedMethod = menuGetMonitor

instance O.OverloadedMethodInfo MenuGetMonitorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.menuGetMonitor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#v:menuGetMonitor"
        })


#endif

-- method Menu::get_reserve_toggle_size
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Menu" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenu" , sinceVersion = Nothing }
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

foreign import ccall "gtk_menu_get_reserve_toggle_size" gtk_menu_get_reserve_toggle_size :: 
    Ptr Menu ->                             -- menu : TInterface (Name {namespace = "Gtk", name = "Menu"})
    IO CInt

-- | Returns whether the menu reserves space for toggles and
-- icons, regardless of their actual presence.
-- 
-- /Since: 2.18/
menuGetReserveToggleSize ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenu a) =>
    a
    -- ^ /@menu@/: a t'GI.Gtk.Objects.Menu.Menu'
    -> m Bool
    -- ^ __Returns:__ Whether the menu reserves toggle space
menuGetReserveToggleSize menu = liftIO $ do
    menu' <- unsafeManagedPtrCastPtr menu
    result <- gtk_menu_get_reserve_toggle_size menu'
    let result' = (/= 0) result
    touchManagedPtr menu
    return result'

#if defined(ENABLE_OVERLOADING)
data MenuGetReserveToggleSizeMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsMenu a) => O.OverloadedMethod MenuGetReserveToggleSizeMethodInfo a signature where
    overloadedMethod = menuGetReserveToggleSize

instance O.OverloadedMethodInfo MenuGetReserveToggleSizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.menuGetReserveToggleSize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#v:menuGetReserveToggleSize"
        })


#endif

-- method Menu::get_tearoff_state
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Menu" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenu" , sinceVersion = Nothing }
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

foreign import ccall "gtk_menu_get_tearoff_state" gtk_menu_get_tearoff_state :: 
    Ptr Menu ->                             -- menu : TInterface (Name {namespace = "Gtk", name = "Menu"})
    IO CInt

{-# DEPRECATED menuGetTearoffState ["(Since version 3.10)"] #-}
-- | Returns whether the menu is torn off.
-- See 'GI.Gtk.Objects.Menu.menuSetTearoffState'.
menuGetTearoffState ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenu a) =>
    a
    -- ^ /@menu@/: a t'GI.Gtk.Objects.Menu.Menu'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the menu is currently torn off.
menuGetTearoffState menu = liftIO $ do
    menu' <- unsafeManagedPtrCastPtr menu
    result <- gtk_menu_get_tearoff_state menu'
    let result' = (/= 0) result
    touchManagedPtr menu
    return result'

#if defined(ENABLE_OVERLOADING)
data MenuGetTearoffStateMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsMenu a) => O.OverloadedMethod MenuGetTearoffStateMethodInfo a signature where
    overloadedMethod = menuGetTearoffState

instance O.OverloadedMethodInfo MenuGetTearoffStateMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.menuGetTearoffState",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#v:menuGetTearoffState"
        })


#endif

-- method Menu::get_title
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Menu" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenu" , sinceVersion = Nothing }
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

foreign import ccall "gtk_menu_get_title" gtk_menu_get_title :: 
    Ptr Menu ->                             -- menu : TInterface (Name {namespace = "Gtk", name = "Menu"})
    IO CString

{-# DEPRECATED menuGetTitle ["(Since version 3.10)"] #-}
-- | Returns the title of the menu. See 'GI.Gtk.Objects.Menu.menuSetTitle'.
menuGetTitle ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenu a) =>
    a
    -- ^ /@menu@/: a t'GI.Gtk.Objects.Menu.Menu'
    -> m T.Text
    -- ^ __Returns:__ the title of the menu, or 'P.Nothing' if the menu
    --     has no title set on it. This string is owned by GTK+
    --     and should not be modified or freed.
menuGetTitle menu = liftIO $ do
    menu' <- unsafeManagedPtrCastPtr menu
    result <- gtk_menu_get_title menu'
    checkUnexpectedReturnNULL "menuGetTitle" result
    result' <- cstringToText result
    touchManagedPtr menu
    return result'

#if defined(ENABLE_OVERLOADING)
data MenuGetTitleMethodInfo
instance (signature ~ (m T.Text), MonadIO m, IsMenu a) => O.OverloadedMethod MenuGetTitleMethodInfo a signature where
    overloadedMethod = menuGetTitle

instance O.OverloadedMethodInfo MenuGetTitleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.menuGetTitle",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#v:menuGetTitle"
        })


#endif

-- method Menu::place_on_monitor
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Menu" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenu" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "monitor"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Monitor" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the monitor to place the menu on"
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

foreign import ccall "gtk_menu_place_on_monitor" gtk_menu_place_on_monitor :: 
    Ptr Menu ->                             -- menu : TInterface (Name {namespace = "Gtk", name = "Menu"})
    Ptr Gdk.Monitor.Monitor ->              -- monitor : TInterface (Name {namespace = "Gdk", name = "Monitor"})
    IO ()

-- | Places /@menu@/ on the given monitor.
-- 
-- /Since: 3.22/
menuPlaceOnMonitor ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenu a, Gdk.Monitor.IsMonitor b) =>
    a
    -- ^ /@menu@/: a t'GI.Gtk.Objects.Menu.Menu'
    -> b
    -- ^ /@monitor@/: the monitor to place the menu on
    -> m ()
menuPlaceOnMonitor menu monitor = liftIO $ do
    menu' <- unsafeManagedPtrCastPtr menu
    monitor' <- unsafeManagedPtrCastPtr monitor
    gtk_menu_place_on_monitor menu' monitor'
    touchManagedPtr menu
    touchManagedPtr monitor
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuPlaceOnMonitorMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsMenu a, Gdk.Monitor.IsMonitor b) => O.OverloadedMethod MenuPlaceOnMonitorMethodInfo a signature where
    overloadedMethod = menuPlaceOnMonitor

instance O.OverloadedMethodInfo MenuPlaceOnMonitorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.menuPlaceOnMonitor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#v:menuPlaceOnMonitor"
        })


#endif

-- method Menu::popdown
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Menu" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenu" , sinceVersion = Nothing }
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

foreign import ccall "gtk_menu_popdown" gtk_menu_popdown :: 
    Ptr Menu ->                             -- menu : TInterface (Name {namespace = "Gtk", name = "Menu"})
    IO ()

-- | Removes the menu from the screen.
menuPopdown ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenu a) =>
    a
    -- ^ /@menu@/: a t'GI.Gtk.Objects.Menu.Menu'
    -> m ()
menuPopdown menu = liftIO $ do
    menu' <- unsafeManagedPtrCastPtr menu
    gtk_menu_popdown menu'
    touchManagedPtr menu
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuPopdownMethodInfo
instance (signature ~ (m ()), MonadIO m, IsMenu a) => O.OverloadedMethod MenuPopdownMethodInfo a signature where
    overloadedMethod = menuPopdown

instance O.OverloadedMethodInfo MenuPopdownMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.menuPopdown",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#v:menuPopdown"
        })


#endif

-- method Menu::popup
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Menu" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenu" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "parent_menu_shell"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the menu shell containing the\n    triggering menu item, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "parent_menu_item"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the menu item whose activation\n    triggered the popup, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "func"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MenuPositionFunc" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a user supplied function used to position\n    the menu, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeAsync
--           , argClosure = 4
--           , argDestroy = -1
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
--                 { rawDocText = Just "user supplied data to be passed to @func."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "button"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the mouse button which was pressed to initiate the event."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "activate_time"
--           , argType = TBasicType TUInt32
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the time at which the activation event occurred."
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

foreign import ccall "gtk_menu_popup" gtk_menu_popup :: 
    Ptr Menu ->                             -- menu : TInterface (Name {namespace = "Gtk", name = "Menu"})
    Ptr Gtk.Widget.Widget ->                -- parent_menu_shell : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gtk.Widget.Widget ->                -- parent_menu_item : TInterface (Name {namespace = "Gtk", name = "Widget"})
    FunPtr Gtk.Callbacks.C_MenuPositionFunc -> -- func : TInterface (Name {namespace = "Gtk", name = "MenuPositionFunc"})
    Ptr () ->                               -- data : TBasicType TPtr
    Word32 ->                               -- button : TBasicType TUInt
    Word32 ->                               -- activate_time : TBasicType TUInt32
    IO ()

{-# DEPRECATED menuPopup ["(Since version 3.22)","Please use 'GI.Gtk.Objects.Menu.menuPopupAtWidget',","    'GI.Gtk.Objects.Menu.menuPopupAtPointer'. or 'GI.Gtk.Objects.Menu.menuPopupAtRect' instead"] #-}
-- | Displays a menu and makes it available for selection.
-- 
-- Applications can use this function to display context-sensitive
-- menus, and will typically supply 'P.Nothing' for the /@parentMenuShell@/,
-- /@parentMenuItem@/, /@func@/ and /@data@/ parameters. The default menu
-- positioning function will position the menu at the current mouse
-- cursor position.
-- 
-- The /@button@/ parameter should be the mouse button pressed to initiate
-- the menu popup. If the menu popup was initiated by something other
-- than a mouse button press, such as a mouse button release or a keypress,
-- /@button@/ should be 0.
-- 
-- The /@activateTime@/ parameter is used to conflict-resolve initiation
-- of concurrent requests for mouse\/keyboard grab requests. To function
-- properly, this needs to be the timestamp of the user event (such as
-- a mouse click or key press) that caused the initiation of the popup.
-- Only if no such event is available, 'GI.Gtk.Functions.getCurrentEventTime' can
-- be used instead.
-- 
-- Note that this function does not work very well on GDK backends that
-- do not have global coordinates, such as Wayland or Mir. You should
-- probably use one of the gtk_menu_popup_at_ variants, which do not
-- have this problem.
menuPopup ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenu a, Gtk.Widget.IsWidget b, Gtk.Widget.IsWidget c) =>
    a
    -- ^ /@menu@/: a t'GI.Gtk.Objects.Menu.Menu'
    -> Maybe (b)
    -- ^ /@parentMenuShell@/: the menu shell containing the
    --     triggering menu item, or 'P.Nothing'
    -> Maybe (c)
    -- ^ /@parentMenuItem@/: the menu item whose activation
    --     triggered the popup, or 'P.Nothing'
    -> Maybe (Gtk.Callbacks.MenuPositionFunc)
    -- ^ /@func@/: a user supplied function used to position
    --     the menu, or 'P.Nothing'
    -> Word32
    -- ^ /@button@/: the mouse button which was pressed to initiate the event.
    -> Word32
    -- ^ /@activateTime@/: the time at which the activation event occurred.
    -> m ()
menuPopup menu parentMenuShell parentMenuItem func button activateTime = liftIO $ do
    menu' <- unsafeManagedPtrCastPtr menu
    maybeParentMenuShell <- case parentMenuShell of
        Nothing -> return nullPtr
        Just jParentMenuShell -> do
            jParentMenuShell' <- unsafeManagedPtrCastPtr jParentMenuShell
            return jParentMenuShell'
    maybeParentMenuItem <- case parentMenuItem of
        Nothing -> return nullPtr
        Just jParentMenuItem -> do
            jParentMenuItem' <- unsafeManagedPtrCastPtr jParentMenuItem
            return jParentMenuItem'
    maybeFunc <- case func of
        Nothing -> return (castPtrToFunPtr nullPtr)
        Just jFunc -> do
            ptrfunc <- callocMem :: IO (Ptr (FunPtr Gtk.Callbacks.C_MenuPositionFunc))
            jFunc' <- Gtk.Callbacks.mk_MenuPositionFunc (Gtk.Callbacks.wrap_MenuPositionFunc (Just ptrfunc) (Gtk.Callbacks.drop_closures_MenuPositionFunc jFunc))
            poke ptrfunc jFunc'
            return jFunc'
    let data_ = nullPtr
    gtk_menu_popup menu' maybeParentMenuShell maybeParentMenuItem maybeFunc data_ button activateTime
    touchManagedPtr menu
    whenJust parentMenuShell touchManagedPtr
    whenJust parentMenuItem touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuPopupMethodInfo
instance (signature ~ (Maybe (b) -> Maybe (c) -> Maybe (Gtk.Callbacks.MenuPositionFunc) -> Word32 -> Word32 -> m ()), MonadIO m, IsMenu a, Gtk.Widget.IsWidget b, Gtk.Widget.IsWidget c) => O.OverloadedMethod MenuPopupMethodInfo a signature where
    overloadedMethod = menuPopup

instance O.OverloadedMethodInfo MenuPopupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.menuPopup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#v:menuPopup"
        })


#endif

-- method Menu::popup_at_pointer
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Menu" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkMenu to pop up"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "trigger_event"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Event" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the #GdkEvent that initiated this request or\n                %NULL if it's the current event"
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

foreign import ccall "gtk_menu_popup_at_pointer" gtk_menu_popup_at_pointer :: 
    Ptr Menu ->                             -- menu : TInterface (Name {namespace = "Gtk", name = "Menu"})
    Ptr Gdk.Event.Event ->                  -- trigger_event : TInterface (Name {namespace = "Gdk", name = "Event"})
    IO ()

-- | Displays /@menu@/ and makes it available for selection.
-- 
-- See gtk_menu_popup_at_widget () to pop up a menu at a widget.
-- gtk_menu_popup_at_rect () also allows you to position a menu at an arbitrary
-- rectangle.
-- 
-- /@menu@/ will be positioned at the pointer associated with /@triggerEvent@/.
-- 
-- Properties that influence the behaviour of this function are
-- [Menu:anchorHints]("GI.Gtk.Objects.Menu#g:attr:anchorHints"), [Menu:rectAnchorDx]("GI.Gtk.Objects.Menu#g:attr:rectAnchorDx"), [Menu:rectAnchorDy]("GI.Gtk.Objects.Menu#g:attr:rectAnchorDy"), and
-- [Menu:menuTypeHint]("GI.Gtk.Objects.Menu#g:attr:menuTypeHint"). Connect to the [Menu::poppedUp]("GI.Gtk.Objects.Menu#g:signal:poppedUp") signal to find
-- out how it was actually positioned.
-- 
-- /Since: 3.22/
menuPopupAtPointer ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenu a) =>
    a
    -- ^ /@menu@/: the t'GI.Gtk.Objects.Menu.Menu' to pop up
    -> Maybe (Gdk.Event.Event)
    -- ^ /@triggerEvent@/: the t'GI.Gdk.Unions.Event.Event' that initiated this request or
    --                 'P.Nothing' if it\'s the current event
    -> m ()
menuPopupAtPointer menu triggerEvent = liftIO $ do
    menu' <- unsafeManagedPtrCastPtr menu
    maybeTriggerEvent <- case triggerEvent of
        Nothing -> return nullPtr
        Just jTriggerEvent -> do
            jTriggerEvent' <- unsafeManagedPtrGetPtr jTriggerEvent
            return jTriggerEvent'
    gtk_menu_popup_at_pointer menu' maybeTriggerEvent
    touchManagedPtr menu
    whenJust triggerEvent touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuPopupAtPointerMethodInfo
instance (signature ~ (Maybe (Gdk.Event.Event) -> m ()), MonadIO m, IsMenu a) => O.OverloadedMethod MenuPopupAtPointerMethodInfo a signature where
    overloadedMethod = menuPopupAtPointer

instance O.OverloadedMethodInfo MenuPopupAtPointerMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.menuPopupAtPointer",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#v:menuPopupAtPointer"
        })


#endif

-- method Menu::popup_at_rect
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Menu" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkMenu to pop up"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "rect_window"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GdkWindow @rect is relative to"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "rect"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GdkRectangle to align @menu with"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "rect_anchor"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Gravity" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the point on @rect to align with @menu's anchor point"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "menu_anchor"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Gravity" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the point on @menu to align with @rect's anchor point"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "trigger_event"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Event" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the #GdkEvent that initiated this request or\n                %NULL if it's the current event"
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

foreign import ccall "gtk_menu_popup_at_rect" gtk_menu_popup_at_rect :: 
    Ptr Menu ->                             -- menu : TInterface (Name {namespace = "Gtk", name = "Menu"})
    Ptr Gdk.Window.Window ->                -- rect_window : TInterface (Name {namespace = "Gdk", name = "Window"})
    Ptr Gdk.Rectangle.Rectangle ->          -- rect : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    CUInt ->                                -- rect_anchor : TInterface (Name {namespace = "Gdk", name = "Gravity"})
    CUInt ->                                -- menu_anchor : TInterface (Name {namespace = "Gdk", name = "Gravity"})
    Ptr Gdk.Event.Event ->                  -- trigger_event : TInterface (Name {namespace = "Gdk", name = "Event"})
    IO ()

-- | Displays /@menu@/ and makes it available for selection.
-- 
-- See gtk_menu_popup_at_widget () and gtk_menu_popup_at_pointer (), which
-- handle more common cases for popping up menus.
-- 
-- /@menu@/ will be positioned at /@rect@/, aligning their anchor points. /@rect@/ is
-- relative to the top-left corner of /@rectWindow@/. /@rectAnchor@/ and
-- /@menuAnchor@/ determine anchor points on /@rect@/ and /@menu@/ to pin together.
-- /@menu@/ can optionally be offset by [Menu:rectAnchorDx]("GI.Gtk.Objects.Menu#g:attr:rectAnchorDx") and
-- [Menu:rectAnchorDy]("GI.Gtk.Objects.Menu#g:attr:rectAnchorDy").
-- 
-- Anchors should be specified under the assumption that the text direction is
-- left-to-right; they will be flipped horizontally automatically if the text
-- direction is right-to-left.
-- 
-- Other properties that influence the behaviour of this function are
-- [Menu:anchorHints]("GI.Gtk.Objects.Menu#g:attr:anchorHints") and [Menu:menuTypeHint]("GI.Gtk.Objects.Menu#g:attr:menuTypeHint"). Connect to the
-- [Menu::poppedUp]("GI.Gtk.Objects.Menu#g:signal:poppedUp") signal to find out how it was actually positioned.
-- 
-- /Since: 3.22/
menuPopupAtRect ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenu a, Gdk.Window.IsWindow b) =>
    a
    -- ^ /@menu@/: the t'GI.Gtk.Objects.Menu.Menu' to pop up
    -> b
    -- ^ /@rectWindow@/: the t'GI.Gdk.Objects.Window.Window' /@rect@/ is relative to
    -> Gdk.Rectangle.Rectangle
    -- ^ /@rect@/: the t'GI.Gdk.Structs.Rectangle.Rectangle' to align /@menu@/ with
    -> Gdk.Enums.Gravity
    -- ^ /@rectAnchor@/: the point on /@rect@/ to align with /@menu@/\'s anchor point
    -> Gdk.Enums.Gravity
    -- ^ /@menuAnchor@/: the point on /@menu@/ to align with /@rect@/\'s anchor point
    -> Maybe (Gdk.Event.Event)
    -- ^ /@triggerEvent@/: the t'GI.Gdk.Unions.Event.Event' that initiated this request or
    --                 'P.Nothing' if it\'s the current event
    -> m ()
menuPopupAtRect menu rectWindow rect rectAnchor menuAnchor triggerEvent = liftIO $ do
    menu' <- unsafeManagedPtrCastPtr menu
    rectWindow' <- unsafeManagedPtrCastPtr rectWindow
    rect' <- unsafeManagedPtrGetPtr rect
    let rectAnchor' = (fromIntegral . fromEnum) rectAnchor
    let menuAnchor' = (fromIntegral . fromEnum) menuAnchor
    maybeTriggerEvent <- case triggerEvent of
        Nothing -> return nullPtr
        Just jTriggerEvent -> do
            jTriggerEvent' <- unsafeManagedPtrGetPtr jTriggerEvent
            return jTriggerEvent'
    gtk_menu_popup_at_rect menu' rectWindow' rect' rectAnchor' menuAnchor' maybeTriggerEvent
    touchManagedPtr menu
    touchManagedPtr rectWindow
    touchManagedPtr rect
    whenJust triggerEvent touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuPopupAtRectMethodInfo
instance (signature ~ (b -> Gdk.Rectangle.Rectangle -> Gdk.Enums.Gravity -> Gdk.Enums.Gravity -> Maybe (Gdk.Event.Event) -> m ()), MonadIO m, IsMenu a, Gdk.Window.IsWindow b) => O.OverloadedMethod MenuPopupAtRectMethodInfo a signature where
    overloadedMethod = menuPopupAtRect

instance O.OverloadedMethodInfo MenuPopupAtRectMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.menuPopupAtRect",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#v:menuPopupAtRect"
        })


#endif

-- method Menu::popup_at_widget
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Menu" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkMenu to pop up"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkWidget to align @menu with"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "widget_anchor"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Gravity" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the point on @widget to align with @menu's anchor point"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "menu_anchor"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Gravity" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the point on @menu to align with @widget's anchor point"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "trigger_event"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Event" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the #GdkEvent that initiated this request or\n                %NULL if it's the current event"
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

foreign import ccall "gtk_menu_popup_at_widget" gtk_menu_popup_at_widget :: 
    Ptr Menu ->                             -- menu : TInterface (Name {namespace = "Gtk", name = "Menu"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CUInt ->                                -- widget_anchor : TInterface (Name {namespace = "Gdk", name = "Gravity"})
    CUInt ->                                -- menu_anchor : TInterface (Name {namespace = "Gdk", name = "Gravity"})
    Ptr Gdk.Event.Event ->                  -- trigger_event : TInterface (Name {namespace = "Gdk", name = "Event"})
    IO ()

-- | Displays /@menu@/ and makes it available for selection.
-- 
-- See gtk_menu_popup_at_pointer () to pop up a menu at the master pointer.
-- gtk_menu_popup_at_rect () also allows you to position a menu at an arbitrary
-- rectangle.
-- 
-- <<https://developer.gnome.org/gtk3/stable/popup-anchors.png>>
-- 
-- /@menu@/ will be positioned at /@widget@/, aligning their anchor points.
-- /@widgetAnchor@/ and /@menuAnchor@/ determine anchor points on /@widget@/ and /@menu@/
-- to pin together. /@menu@/ can optionally be offset by [Menu:rectAnchorDx]("GI.Gtk.Objects.Menu#g:attr:rectAnchorDx")
-- and [Menu:rectAnchorDy]("GI.Gtk.Objects.Menu#g:attr:rectAnchorDy").
-- 
-- Anchors should be specified under the assumption that the text direction is
-- left-to-right; they will be flipped horizontally automatically if the text
-- direction is right-to-left.
-- 
-- Other properties that influence the behaviour of this function are
-- [Menu:anchorHints]("GI.Gtk.Objects.Menu#g:attr:anchorHints") and [Menu:menuTypeHint]("GI.Gtk.Objects.Menu#g:attr:menuTypeHint"). Connect to the
-- [Menu::poppedUp]("GI.Gtk.Objects.Menu#g:signal:poppedUp") signal to find out how it was actually positioned.
-- 
-- /Since: 3.22/
menuPopupAtWidget ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenu a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@menu@/: the t'GI.Gtk.Objects.Menu.Menu' to pop up
    -> b
    -- ^ /@widget@/: the t'GI.Gtk.Objects.Widget.Widget' to align /@menu@/ with
    -> Gdk.Enums.Gravity
    -- ^ /@widgetAnchor@/: the point on /@widget@/ to align with /@menu@/\'s anchor point
    -> Gdk.Enums.Gravity
    -- ^ /@menuAnchor@/: the point on /@menu@/ to align with /@widget@/\'s anchor point
    -> Maybe (Gdk.Event.Event)
    -- ^ /@triggerEvent@/: the t'GI.Gdk.Unions.Event.Event' that initiated this request or
    --                 'P.Nothing' if it\'s the current event
    -> m ()
menuPopupAtWidget menu widget widgetAnchor menuAnchor triggerEvent = liftIO $ do
    menu' <- unsafeManagedPtrCastPtr menu
    widget' <- unsafeManagedPtrCastPtr widget
    let widgetAnchor' = (fromIntegral . fromEnum) widgetAnchor
    let menuAnchor' = (fromIntegral . fromEnum) menuAnchor
    maybeTriggerEvent <- case triggerEvent of
        Nothing -> return nullPtr
        Just jTriggerEvent -> do
            jTriggerEvent' <- unsafeManagedPtrGetPtr jTriggerEvent
            return jTriggerEvent'
    gtk_menu_popup_at_widget menu' widget' widgetAnchor' menuAnchor' maybeTriggerEvent
    touchManagedPtr menu
    touchManagedPtr widget
    whenJust triggerEvent touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuPopupAtWidgetMethodInfo
instance (signature ~ (b -> Gdk.Enums.Gravity -> Gdk.Enums.Gravity -> Maybe (Gdk.Event.Event) -> m ()), MonadIO m, IsMenu a, Gtk.Widget.IsWidget b) => O.OverloadedMethod MenuPopupAtWidgetMethodInfo a signature where
    overloadedMethod = menuPopupAtWidget

instance O.OverloadedMethodInfo MenuPopupAtWidgetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.menuPopupAtWidget",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#v:menuPopupAtWidget"
        })


#endif

-- method Menu::popup_for_device
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Menu" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenu" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "device"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Device" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkDevice" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "parent_menu_shell"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the menu shell containing the triggering\n    menu item, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "parent_menu_item"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the menu item whose activation triggered\n    the popup, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "func"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MenuPositionFunc" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a user supplied function used to position the menu,\n    or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeNotified
--           , argClosure = 5
--           , argDestroy = 6
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
--                 { rawDocText = Just "user supplied data to be passed to @func"
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "destroy notify for @data"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeAsync
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "button"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the mouse button which was pressed to initiate the event"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "activate_time"
--           , argType = TBasicType TUInt32
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the time at which the activation event occurred"
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

foreign import ccall "gtk_menu_popup_for_device" gtk_menu_popup_for_device :: 
    Ptr Menu ->                             -- menu : TInterface (Name {namespace = "Gtk", name = "Menu"})
    Ptr Gdk.Device.Device ->                -- device : TInterface (Name {namespace = "Gdk", name = "Device"})
    Ptr Gtk.Widget.Widget ->                -- parent_menu_shell : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gtk.Widget.Widget ->                -- parent_menu_item : TInterface (Name {namespace = "Gtk", name = "Widget"})
    FunPtr Gtk.Callbacks.C_MenuPositionFunc -> -- func : TInterface (Name {namespace = "Gtk", name = "MenuPositionFunc"})
    Ptr () ->                               -- data : TBasicType TPtr
    FunPtr GLib.Callbacks.C_DestroyNotify -> -- destroy : TInterface (Name {namespace = "GLib", name = "DestroyNotify"})
    Word32 ->                               -- button : TBasicType TUInt
    Word32 ->                               -- activate_time : TBasicType TUInt32
    IO ()

{-# DEPRECATED menuPopupForDevice ["(Since version 3.22)","Please use 'GI.Gtk.Objects.Menu.menuPopupAtWidget',","    'GI.Gtk.Objects.Menu.menuPopupAtPointer'. or 'GI.Gtk.Objects.Menu.menuPopupAtRect' instead"] #-}
-- | Displays a menu and makes it available for selection.
-- 
-- Applications can use this function to display context-sensitive menus,
-- and will typically supply 'P.Nothing' for the /@parentMenuShell@/,
-- /@parentMenuItem@/, /@func@/, /@data@/ and /@destroy@/ parameters. The default
-- menu positioning function will position the menu at the current position
-- of /@device@/ (or its corresponding pointer).
-- 
-- The /@button@/ parameter should be the mouse button pressed to initiate
-- the menu popup. If the menu popup was initiated by something other than
-- a mouse button press, such as a mouse button release or a keypress,
-- /@button@/ should be 0.
-- 
-- The /@activateTime@/ parameter is used to conflict-resolve initiation of
-- concurrent requests for mouse\/keyboard grab requests. To function
-- properly, this needs to be the time stamp of the user event (such as
-- a mouse click or key press) that caused the initiation of the popup.
-- Only if no such event is available, 'GI.Gtk.Functions.getCurrentEventTime' can
-- be used instead.
-- 
-- Note that this function does not work very well on GDK backends that
-- do not have global coordinates, such as Wayland or Mir. You should
-- probably use one of the gtk_menu_popup_at_ variants, which do not
-- have this problem.
-- 
-- /Since: 3.0/
menuPopupForDevice ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenu a, Gdk.Device.IsDevice b, Gtk.Widget.IsWidget c, Gtk.Widget.IsWidget d) =>
    a
    -- ^ /@menu@/: a t'GI.Gtk.Objects.Menu.Menu'
    -> Maybe (b)
    -- ^ /@device@/: a t'GI.Gdk.Objects.Device.Device'
    -> Maybe (c)
    -- ^ /@parentMenuShell@/: the menu shell containing the triggering
    --     menu item, or 'P.Nothing'
    -> Maybe (d)
    -- ^ /@parentMenuItem@/: the menu item whose activation triggered
    --     the popup, or 'P.Nothing'
    -> Maybe (Gtk.Callbacks.MenuPositionFunc)
    -- ^ /@func@/: a user supplied function used to position the menu,
    --     or 'P.Nothing'
    -> Word32
    -- ^ /@button@/: the mouse button which was pressed to initiate the event
    -> Word32
    -- ^ /@activateTime@/: the time at which the activation event occurred
    -> m ()
menuPopupForDevice menu device parentMenuShell parentMenuItem func button activateTime = liftIO $ do
    menu' <- unsafeManagedPtrCastPtr menu
    maybeDevice <- case device of
        Nothing -> return nullPtr
        Just jDevice -> do
            jDevice' <- unsafeManagedPtrCastPtr jDevice
            return jDevice'
    maybeParentMenuShell <- case parentMenuShell of
        Nothing -> return nullPtr
        Just jParentMenuShell -> do
            jParentMenuShell' <- unsafeManagedPtrCastPtr jParentMenuShell
            return jParentMenuShell'
    maybeParentMenuItem <- case parentMenuItem of
        Nothing -> return nullPtr
        Just jParentMenuItem -> do
            jParentMenuItem' <- unsafeManagedPtrCastPtr jParentMenuItem
            return jParentMenuItem'
    maybeFunc <- case func of
        Nothing -> return (castPtrToFunPtr nullPtr)
        Just jFunc -> do
            jFunc' <- Gtk.Callbacks.mk_MenuPositionFunc (Gtk.Callbacks.wrap_MenuPositionFunc Nothing (Gtk.Callbacks.drop_closures_MenuPositionFunc jFunc))
            return jFunc'
    let data_ = castFunPtrToPtr maybeFunc
    let destroy = SP.safeFreeFunPtrPtr
    gtk_menu_popup_for_device menu' maybeDevice maybeParentMenuShell maybeParentMenuItem maybeFunc data_ destroy button activateTime
    touchManagedPtr menu
    whenJust device touchManagedPtr
    whenJust parentMenuShell touchManagedPtr
    whenJust parentMenuItem touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuPopupForDeviceMethodInfo
instance (signature ~ (Maybe (b) -> Maybe (c) -> Maybe (d) -> Maybe (Gtk.Callbacks.MenuPositionFunc) -> Word32 -> Word32 -> m ()), MonadIO m, IsMenu a, Gdk.Device.IsDevice b, Gtk.Widget.IsWidget c, Gtk.Widget.IsWidget d) => O.OverloadedMethod MenuPopupForDeviceMethodInfo a signature where
    overloadedMethod = menuPopupForDevice

instance O.OverloadedMethodInfo MenuPopupForDeviceMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.menuPopupForDevice",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#v:menuPopupForDevice"
        })


#endif

-- method Menu::reorder_child
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Menu" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenu" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "child"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkMenuItem to move"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "position"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the new position to place @child.\n    Positions are numbered from 0 to n - 1"
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

foreign import ccall "gtk_menu_reorder_child" gtk_menu_reorder_child :: 
    Ptr Menu ->                             -- menu : TInterface (Name {namespace = "Gtk", name = "Menu"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Int32 ->                                -- position : TBasicType TInt
    IO ()

-- | Moves /@child@/ to a new /@position@/ in the list of /@menu@/
-- children.
menuReorderChild ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenu a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@menu@/: a t'GI.Gtk.Objects.Menu.Menu'
    -> b
    -- ^ /@child@/: the t'GI.Gtk.Objects.MenuItem.MenuItem' to move
    -> Int32
    -- ^ /@position@/: the new position to place /@child@/.
    --     Positions are numbered from 0 to n - 1
    -> m ()
menuReorderChild menu child position = liftIO $ do
    menu' <- unsafeManagedPtrCastPtr menu
    child' <- unsafeManagedPtrCastPtr child
    gtk_menu_reorder_child menu' child' position
    touchManagedPtr menu
    touchManagedPtr child
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuReorderChildMethodInfo
instance (signature ~ (b -> Int32 -> m ()), MonadIO m, IsMenu a, Gtk.Widget.IsWidget b) => O.OverloadedMethod MenuReorderChildMethodInfo a signature where
    overloadedMethod = menuReorderChild

instance O.OverloadedMethodInfo MenuReorderChildMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.menuReorderChild",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#v:menuReorderChild"
        })


#endif

-- method Menu::reposition
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Menu" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenu" , sinceVersion = Nothing }
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

foreign import ccall "gtk_menu_reposition" gtk_menu_reposition :: 
    Ptr Menu ->                             -- menu : TInterface (Name {namespace = "Gtk", name = "Menu"})
    IO ()

-- | Repositions the menu according to its position function.
menuReposition ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenu a) =>
    a
    -- ^ /@menu@/: a t'GI.Gtk.Objects.Menu.Menu'
    -> m ()
menuReposition menu = liftIO $ do
    menu' <- unsafeManagedPtrCastPtr menu
    gtk_menu_reposition menu'
    touchManagedPtr menu
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuRepositionMethodInfo
instance (signature ~ (m ()), MonadIO m, IsMenu a) => O.OverloadedMethod MenuRepositionMethodInfo a signature where
    overloadedMethod = menuReposition

instance O.OverloadedMethodInfo MenuRepositionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.menuReposition",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#v:menuReposition"
        })


#endif

-- method Menu::set_accel_group
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Menu" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenu" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "accel_group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AccelGroup" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the #GtkAccelGroup to be associated\n              with the menu."
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

foreign import ccall "gtk_menu_set_accel_group" gtk_menu_set_accel_group :: 
    Ptr Menu ->                             -- menu : TInterface (Name {namespace = "Gtk", name = "Menu"})
    Ptr Gtk.AccelGroup.AccelGroup ->        -- accel_group : TInterface (Name {namespace = "Gtk", name = "AccelGroup"})
    IO ()

-- | Set the t'GI.Gtk.Objects.AccelGroup.AccelGroup' which holds global accelerators for the
-- menu.  This accelerator group needs to also be added to all windows
-- that this menu is being used in with 'GI.Gtk.Objects.Window.windowAddAccelGroup',
-- in order for those windows to support all the accelerators
-- contained in this group.
menuSetAccelGroup ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenu a, Gtk.AccelGroup.IsAccelGroup b) =>
    a
    -- ^ /@menu@/: a t'GI.Gtk.Objects.Menu.Menu'
    -> Maybe (b)
    -- ^ /@accelGroup@/: the t'GI.Gtk.Objects.AccelGroup.AccelGroup' to be associated
    --               with the menu.
    -> m ()
menuSetAccelGroup menu accelGroup = liftIO $ do
    menu' <- unsafeManagedPtrCastPtr menu
    maybeAccelGroup <- case accelGroup of
        Nothing -> return nullPtr
        Just jAccelGroup -> do
            jAccelGroup' <- unsafeManagedPtrCastPtr jAccelGroup
            return jAccelGroup'
    gtk_menu_set_accel_group menu' maybeAccelGroup
    touchManagedPtr menu
    whenJust accelGroup touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuSetAccelGroupMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsMenu a, Gtk.AccelGroup.IsAccelGroup b) => O.OverloadedMethod MenuSetAccelGroupMethodInfo a signature where
    overloadedMethod = menuSetAccelGroup

instance O.OverloadedMethodInfo MenuSetAccelGroupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.menuSetAccelGroup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#v:menuSetAccelGroup"
        })


#endif

-- method Menu::set_accel_path
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Menu" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a valid #GtkMenu" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "accel_path"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a valid accelerator path, or %NULL to unset the path"
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

foreign import ccall "gtk_menu_set_accel_path" gtk_menu_set_accel_path :: 
    Ptr Menu ->                             -- menu : TInterface (Name {namespace = "Gtk", name = "Menu"})
    CString ->                              -- accel_path : TBasicType TUTF8
    IO ()

-- | Sets an accelerator path for this menu from which accelerator paths
-- for its immediate children, its menu items, can be constructed.
-- The main purpose of this function is to spare the programmer the
-- inconvenience of having to call 'GI.Gtk.Objects.MenuItem.menuItemSetAccelPath' on
-- each menu item that should support runtime user changable accelerators.
-- Instead, by just calling 'GI.Gtk.Objects.Menu.menuSetAccelPath' on their parent,
-- each menu item of this menu, that contains a label describing its
-- purpose, automatically gets an accel path assigned.
-- 
-- For example, a menu containing menu items “New” and “Exit”, will, after
-- @gtk_menu_set_accel_path (menu, \"\<Gnumeric-Sheet>\/File\");@ has been
-- called, assign its items the accel paths: @\"\<Gnumeric-Sheet>\/File\/New\"@
-- and @\"\<Gnumeric-Sheet>\/File\/Exit\"@.
-- 
-- Assigning accel paths to menu items then enables the user to change
-- their accelerators at runtime. More details about accelerator paths
-- and their default setups can be found at 'GI.Gtk.Objects.AccelMap.accelMapAddEntry'.
-- 
-- Note that /@accelPath@/ string will be stored in a @/GQuark/@. Therefore,
-- if you pass a static string, you can save some memory by interning
-- it first with 'GI.GLib.Functions.internStaticString'.
menuSetAccelPath ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenu a) =>
    a
    -- ^ /@menu@/: a valid t'GI.Gtk.Objects.Menu.Menu'
    -> Maybe (T.Text)
    -- ^ /@accelPath@/: a valid accelerator path, or 'P.Nothing' to unset the path
    -> m ()
menuSetAccelPath menu accelPath = liftIO $ do
    menu' <- unsafeManagedPtrCastPtr menu
    maybeAccelPath <- case accelPath of
        Nothing -> return nullPtr
        Just jAccelPath -> do
            jAccelPath' <- textToCString jAccelPath
            return jAccelPath'
    gtk_menu_set_accel_path menu' maybeAccelPath
    touchManagedPtr menu
    freeMem maybeAccelPath
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuSetAccelPathMethodInfo
instance (signature ~ (Maybe (T.Text) -> m ()), MonadIO m, IsMenu a) => O.OverloadedMethod MenuSetAccelPathMethodInfo a signature where
    overloadedMethod = menuSetAccelPath

instance O.OverloadedMethodInfo MenuSetAccelPathMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.menuSetAccelPath",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#v:menuSetAccelPath"
        })


#endif

-- method Menu::set_active
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Menu" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenu" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "index"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the index of the menu item to select.  Index values are\n        from 0 to n-1"
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

foreign import ccall "gtk_menu_set_active" gtk_menu_set_active :: 
    Ptr Menu ->                             -- menu : TInterface (Name {namespace = "Gtk", name = "Menu"})
    Word32 ->                               -- index : TBasicType TUInt
    IO ()

-- | Selects the specified menu item within the menu.  This is used by
-- the t'GI.Gtk.Objects.ComboBox.ComboBox' and should not be used by anyone else.
menuSetActive ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenu a) =>
    a
    -- ^ /@menu@/: a t'GI.Gtk.Objects.Menu.Menu'
    -> Word32
    -- ^ /@index@/: the index of the menu item to select.  Index values are
    --         from 0 to n-1
    -> m ()
menuSetActive menu index = liftIO $ do
    menu' <- unsafeManagedPtrCastPtr menu
    gtk_menu_set_active menu' index
    touchManagedPtr menu
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuSetActiveMethodInfo
instance (signature ~ (Word32 -> m ()), MonadIO m, IsMenu a) => O.OverloadedMethod MenuSetActiveMethodInfo a signature where
    overloadedMethod = menuSetActive

instance O.OverloadedMethodInfo MenuSetActiveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.menuSetActive",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#v:menuSetActive"
        })


#endif

-- method Menu::set_monitor
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Menu" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenu" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "monitor_num"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the number of the monitor on which the menu should\n   be popped up"
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

foreign import ccall "gtk_menu_set_monitor" gtk_menu_set_monitor :: 
    Ptr Menu ->                             -- menu : TInterface (Name {namespace = "Gtk", name = "Menu"})
    Int32 ->                                -- monitor_num : TBasicType TInt
    IO ()

-- | Informs GTK+ on which monitor a menu should be popped up.
-- See 'GI.Gdk.Objects.Monitor.monitorGetGeometry'.
-- 
-- This function should be called from a t'GI.Gtk.Callbacks.MenuPositionFunc'
-- if the menu should not appear on the same monitor as the pointer.
-- This information can’t be reliably inferred from the coordinates
-- returned by a t'GI.Gtk.Callbacks.MenuPositionFunc', since, for very long menus,
-- these coordinates may extend beyond the monitor boundaries or even
-- the screen boundaries.
-- 
-- /Since: 2.4/
menuSetMonitor ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenu a) =>
    a
    -- ^ /@menu@/: a t'GI.Gtk.Objects.Menu.Menu'
    -> Int32
    -- ^ /@monitorNum@/: the number of the monitor on which the menu should
    --    be popped up
    -> m ()
menuSetMonitor menu monitorNum = liftIO $ do
    menu' <- unsafeManagedPtrCastPtr menu
    gtk_menu_set_monitor menu' monitorNum
    touchManagedPtr menu
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuSetMonitorMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsMenu a) => O.OverloadedMethod MenuSetMonitorMethodInfo a signature where
    overloadedMethod = menuSetMonitor

instance O.OverloadedMethodInfo MenuSetMonitorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.menuSetMonitor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#v:menuSetMonitor"
        })


#endif

-- method Menu::set_reserve_toggle_size
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Menu" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenu" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "reserve_toggle_size"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "whether to reserve size for toggles"
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

foreign import ccall "gtk_menu_set_reserve_toggle_size" gtk_menu_set_reserve_toggle_size :: 
    Ptr Menu ->                             -- menu : TInterface (Name {namespace = "Gtk", name = "Menu"})
    CInt ->                                 -- reserve_toggle_size : TBasicType TBoolean
    IO ()

-- | Sets whether the menu should reserve space for drawing toggles
-- or icons, regardless of their actual presence.
-- 
-- /Since: 2.18/
menuSetReserveToggleSize ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenu a) =>
    a
    -- ^ /@menu@/: a t'GI.Gtk.Objects.Menu.Menu'
    -> Bool
    -- ^ /@reserveToggleSize@/: whether to reserve size for toggles
    -> m ()
menuSetReserveToggleSize menu reserveToggleSize = liftIO $ do
    menu' <- unsafeManagedPtrCastPtr menu
    let reserveToggleSize' = (fromIntegral . fromEnum) reserveToggleSize
    gtk_menu_set_reserve_toggle_size menu' reserveToggleSize'
    touchManagedPtr menu
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuSetReserveToggleSizeMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsMenu a) => O.OverloadedMethod MenuSetReserveToggleSizeMethodInfo a signature where
    overloadedMethod = menuSetReserveToggleSize

instance O.OverloadedMethodInfo MenuSetReserveToggleSizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.menuSetReserveToggleSize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#v:menuSetReserveToggleSize"
        })


#endif

-- method Menu::set_screen
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Menu" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenu" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "screen"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Screen" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a #GdkScreen, or %NULL if the screen should be\n         determined by the widget the menu is attached to"
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

foreign import ccall "gtk_menu_set_screen" gtk_menu_set_screen :: 
    Ptr Menu ->                             -- menu : TInterface (Name {namespace = "Gtk", name = "Menu"})
    Ptr Gdk.Screen.Screen ->                -- screen : TInterface (Name {namespace = "Gdk", name = "Screen"})
    IO ()

-- | Sets the t'GI.Gdk.Objects.Screen.Screen' on which the menu will be displayed.
-- 
-- /Since: 2.2/
menuSetScreen ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenu a, Gdk.Screen.IsScreen b) =>
    a
    -- ^ /@menu@/: a t'GI.Gtk.Objects.Menu.Menu'
    -> Maybe (b)
    -- ^ /@screen@/: a t'GI.Gdk.Objects.Screen.Screen', or 'P.Nothing' if the screen should be
    --          determined by the widget the menu is attached to
    -> m ()
menuSetScreen menu screen = liftIO $ do
    menu' <- unsafeManagedPtrCastPtr menu
    maybeScreen <- case screen of
        Nothing -> return nullPtr
        Just jScreen -> do
            jScreen' <- unsafeManagedPtrCastPtr jScreen
            return jScreen'
    gtk_menu_set_screen menu' maybeScreen
    touchManagedPtr menu
    whenJust screen touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuSetScreenMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsMenu a, Gdk.Screen.IsScreen b) => O.OverloadedMethod MenuSetScreenMethodInfo a signature where
    overloadedMethod = menuSetScreen

instance O.OverloadedMethodInfo MenuSetScreenMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.menuSetScreen",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#v:menuSetScreen"
        })


#endif

-- method Menu::set_tearoff_state
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Menu" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenu" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "torn_off"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "If %TRUE, menu is displayed as a tearoff menu."
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

foreign import ccall "gtk_menu_set_tearoff_state" gtk_menu_set_tearoff_state :: 
    Ptr Menu ->                             -- menu : TInterface (Name {namespace = "Gtk", name = "Menu"})
    CInt ->                                 -- torn_off : TBasicType TBoolean
    IO ()

{-# DEPRECATED menuSetTearoffState ["(Since version 3.10)"] #-}
-- | Changes the tearoff state of the menu.  A menu is normally
-- displayed as drop down menu which persists as long as the menu is
-- active.  It can also be displayed as a tearoff menu which persists
-- until it is closed or reattached.
menuSetTearoffState ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenu a) =>
    a
    -- ^ /@menu@/: a t'GI.Gtk.Objects.Menu.Menu'
    -> Bool
    -- ^ /@tornOff@/: If 'P.True', menu is displayed as a tearoff menu.
    -> m ()
menuSetTearoffState menu tornOff = liftIO $ do
    menu' <- unsafeManagedPtrCastPtr menu
    let tornOff' = (fromIntegral . fromEnum) tornOff
    gtk_menu_set_tearoff_state menu' tornOff'
    touchManagedPtr menu
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuSetTearoffStateMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsMenu a) => O.OverloadedMethod MenuSetTearoffStateMethodInfo a signature where
    overloadedMethod = menuSetTearoffState

instance O.OverloadedMethodInfo MenuSetTearoffStateMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.menuSetTearoffState",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#v:menuSetTearoffState"
        })


#endif

-- method Menu::set_title
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Menu" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenu" , sinceVersion = Nothing }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a string containing the title for the menu, or %NULL to\n  inherit the title of the parent menu item, if any"
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

foreign import ccall "gtk_menu_set_title" gtk_menu_set_title :: 
    Ptr Menu ->                             -- menu : TInterface (Name {namespace = "Gtk", name = "Menu"})
    CString ->                              -- title : TBasicType TUTF8
    IO ()

{-# DEPRECATED menuSetTitle ["(Since version 3.10)"] #-}
-- | Sets the title string for the menu.
-- 
-- The title is displayed when the menu is shown as a tearoff
-- menu. If /@title@/ is 'P.Nothing', the menu will see if it is attached
-- to a parent menu item, and if so it will try to use the same
-- text as that menu item’s label.
menuSetTitle ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenu a) =>
    a
    -- ^ /@menu@/: a t'GI.Gtk.Objects.Menu.Menu'
    -> Maybe (T.Text)
    -- ^ /@title@/: a string containing the title for the menu, or 'P.Nothing' to
    --   inherit the title of the parent menu item, if any
    -> m ()
menuSetTitle menu title = liftIO $ do
    menu' <- unsafeManagedPtrCastPtr menu
    maybeTitle <- case title of
        Nothing -> return nullPtr
        Just jTitle -> do
            jTitle' <- textToCString jTitle
            return jTitle'
    gtk_menu_set_title menu' maybeTitle
    touchManagedPtr menu
    freeMem maybeTitle
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuSetTitleMethodInfo
instance (signature ~ (Maybe (T.Text) -> m ()), MonadIO m, IsMenu a) => O.OverloadedMethod MenuSetTitleMethodInfo a signature where
    overloadedMethod = menuSetTitle

instance O.OverloadedMethodInfo MenuSetTitleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Menu.menuSetTitle",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Menu.html#v:menuSetTitle"
        })


#endif

-- method Menu::get_for_attach_widget
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
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
-- returnType: Just
--               (TGList (TInterface Name { namespace = "Gtk" , name = "Widget" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_menu_get_for_attach_widget" gtk_menu_get_for_attach_widget :: 
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO (Ptr (GList (Ptr Gtk.Widget.Widget)))

-- | Returns a list of the menus which are attached to this widget.
-- This list is owned by GTK+ and must not be modified.
-- 
-- /Since: 2.6/
menuGetForAttachWidget ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Widget.IsWidget a) =>
    a
    -- ^ /@widget@/: a t'GI.Gtk.Objects.Widget.Widget'
    -> m [Gtk.Widget.Widget]
    -- ^ __Returns:__ the list
    --     of menus attached to his widget.
menuGetForAttachWidget widget = liftIO $ do
    widget' <- unsafeManagedPtrCastPtr widget
    result <- gtk_menu_get_for_attach_widget widget'
    result' <- unpackGList result
    result'' <- mapM (newObject Gtk.Widget.Widget) result'
    touchManagedPtr widget
    return result''

#if defined(ENABLE_OVERLOADING)
#endif


