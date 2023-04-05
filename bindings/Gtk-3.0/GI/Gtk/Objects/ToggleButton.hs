{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A t'GI.Gtk.Objects.ToggleButton.ToggleButton' is a t'GI.Gtk.Objects.Button.Button' which will remain “pressed-in” when
-- clicked. Clicking again will cause the toggle button to return to its
-- normal state.
-- 
-- A toggle button is created by calling either 'GI.Gtk.Objects.ToggleButton.toggleButtonNew' or
-- 'GI.Gtk.Objects.ToggleButton.toggleButtonNewWithLabel'. If using the former, it is advisable to
-- pack a widget, (such as a t'GI.Gtk.Objects.Label.Label' and\/or a t'GI.Gtk.Objects.Image.Image'), into the toggle
-- button’s container. (See t'GI.Gtk.Objects.Button.Button' for more information).
-- 
-- The state of a t'GI.Gtk.Objects.ToggleButton.ToggleButton' can be set specifically using
-- 'GI.Gtk.Objects.ToggleButton.toggleButtonSetActive', and retrieved using
-- 'GI.Gtk.Objects.ToggleButton.toggleButtonGetActive'.
-- 
-- To simply switch the state of a toggle button, use 'GI.Gtk.Objects.ToggleButton.toggleButtonToggled'.
-- 
-- = CSS nodes
-- 
-- GtkToggleButton has a single CSS node with name button. To differentiate
-- it from a plain t'GI.Gtk.Objects.Button.Button', it gets the .toggle style class.
-- 
-- ## Creating two t'GI.Gtk.Objects.ToggleButton.ToggleButton' widgets.
-- 
-- 
-- === /C code/
-- >
-- >static void output_state (GtkToggleButton *source, gpointer user_data) {
-- >  printf ("Active: %d\n", gtk_toggle_button_get_active (source));
-- >}
-- >
-- >void make_toggles (void) {
-- >  GtkWidget *window, *toggle1, *toggle2;
-- >  GtkWidget *box;
-- >  const char *text;
-- >
-- >  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
-- >  box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 12);
-- >
-- >  text = "Hi, I’m a toggle button.";
-- >  toggle1 = gtk_toggle_button_new_with_label (text);
-- >
-- >  // Makes this toggle button invisible
-- >  gtk_toggle_button_set_mode (GTK_TOGGLE_BUTTON (toggle1),
-- >                              TRUE);
-- >
-- >  g_signal_connect (toggle1, "toggled",
-- >                    G_CALLBACK (output_state),
-- >                    NULL);
-- >  gtk_container_add (GTK_CONTAINER (box), toggle1);
-- >
-- >  text = "Hi, I’m a toggle button.";
-- >  toggle2 = gtk_toggle_button_new_with_label (text);
-- >  gtk_toggle_button_set_mode (GTK_TOGGLE_BUTTON (toggle2),
-- >                              FALSE);
-- >  g_signal_connect (toggle2, "toggled",
-- >                    G_CALLBACK (output_state),
-- >                    NULL);
-- >  gtk_container_add (GTK_CONTAINER (box), toggle2);
-- >
-- >  gtk_container_add (GTK_CONTAINER (window), box);
-- >  gtk_widget_show_all (window);
-- >}
-- 

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.ToggleButton
    ( 

-- * Exported types
    ToggleButton(..)                        ,
    IsToggleButton                          ,
    toToggleButton                          ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [clicked]("GI.Gtk.Objects.Button#g:method:clicked"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [doSetRelatedAction]("GI.Gtk.Interfaces.Activatable#g:method:doSetRelatedAction"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [enter]("GI.Gtk.Objects.Button#g:method:enter"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [leave]("GI.Gtk.Objects.Button#g:method:leave"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [pressed]("GI.Gtk.Objects.Button#g:method:pressed"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [released]("GI.Gtk.Objects.Button#g:method:released"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [syncActionProperties]("GI.Gtk.Interfaces.Activatable#g:method:syncActionProperties"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [toggled]("GI.Gtk.Objects.ToggleButton#g:method:toggled"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getActionName]("GI.Gtk.Interfaces.Actionable#g:method:getActionName"), [getActionTargetValue]("GI.Gtk.Interfaces.Actionable#g:method:getActionTargetValue"), [getActive]("GI.Gtk.Objects.ToggleButton#g:method:getActive"), [getAlignment]("GI.Gtk.Objects.Button#g:method:getAlignment"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAlwaysShowImage]("GI.Gtk.Objects.Button#g:method:getAlwaysShowImage"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChild]("GI.Gtk.Objects.Bin#g:method:getChild"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEventWindow]("GI.Gtk.Objects.Button#g:method:getEventWindow"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Button#g:method:getFocusOnClick"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getImage]("GI.Gtk.Objects.Button#g:method:getImage"), [getImagePosition]("GI.Gtk.Objects.Button#g:method:getImagePosition"), [getInconsistent]("GI.Gtk.Objects.ToggleButton#g:method:getInconsistent"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getLabel]("GI.Gtk.Objects.Button#g:method:getLabel"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getMode]("GI.Gtk.Objects.ToggleButton#g:method:getMode"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRelatedAction]("GI.Gtk.Interfaces.Activatable#g:method:getRelatedAction"), [getRelief]("GI.Gtk.Objects.Button#g:method:getRelief"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getUseActionAppearance]("GI.Gtk.Interfaces.Activatable#g:method:getUseActionAppearance"), [getUseStock]("GI.Gtk.Objects.Button#g:method:getUseStock"), [getUseUnderline]("GI.Gtk.Objects.Button#g:method:getUseUnderline"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setActionName]("GI.Gtk.Interfaces.Actionable#g:method:setActionName"), [setActionTargetValue]("GI.Gtk.Interfaces.Actionable#g:method:setActionTargetValue"), [setActive]("GI.Gtk.Objects.ToggleButton#g:method:setActive"), [setAlignment]("GI.Gtk.Objects.Button#g:method:setAlignment"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAlwaysShowImage]("GI.Gtk.Objects.Button#g:method:setAlwaysShowImage"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDetailedActionName]("GI.Gtk.Interfaces.Actionable#g:method:setDetailedActionName"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Button#g:method:setFocusOnClick"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setImage]("GI.Gtk.Objects.Button#g:method:setImage"), [setImagePosition]("GI.Gtk.Objects.Button#g:method:setImagePosition"), [setInconsistent]("GI.Gtk.Objects.ToggleButton#g:method:setInconsistent"), [setLabel]("GI.Gtk.Objects.Button#g:method:setLabel"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setMode]("GI.Gtk.Objects.ToggleButton#g:method:setMode"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setRelatedAction]("GI.Gtk.Interfaces.Activatable#g:method:setRelatedAction"), [setRelief]("GI.Gtk.Objects.Button#g:method:setRelief"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setUseActionAppearance]("GI.Gtk.Interfaces.Activatable#g:method:setUseActionAppearance"), [setUseStock]("GI.Gtk.Objects.Button#g:method:setUseStock"), [setUseUnderline]("GI.Gtk.Objects.Button#g:method:setUseUnderline"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveToggleButtonMethod               ,
#endif

-- ** getActive #method:getActive#

#if defined(ENABLE_OVERLOADING)
    ToggleButtonGetActiveMethodInfo         ,
#endif
    toggleButtonGetActive                   ,


-- ** getInconsistent #method:getInconsistent#

#if defined(ENABLE_OVERLOADING)
    ToggleButtonGetInconsistentMethodInfo   ,
#endif
    toggleButtonGetInconsistent             ,


-- ** getMode #method:getMode#

#if defined(ENABLE_OVERLOADING)
    ToggleButtonGetModeMethodInfo           ,
#endif
    toggleButtonGetMode                     ,


-- ** new #method:new#

    toggleButtonNew                         ,


-- ** newWithLabel #method:newWithLabel#

    toggleButtonNewWithLabel                ,


-- ** newWithMnemonic #method:newWithMnemonic#

    toggleButtonNewWithMnemonic             ,


-- ** setActive #method:setActive#

#if defined(ENABLE_OVERLOADING)
    ToggleButtonSetActiveMethodInfo         ,
#endif
    toggleButtonSetActive                   ,


-- ** setInconsistent #method:setInconsistent#

#if defined(ENABLE_OVERLOADING)
    ToggleButtonSetInconsistentMethodInfo   ,
#endif
    toggleButtonSetInconsistent             ,


-- ** setMode #method:setMode#

#if defined(ENABLE_OVERLOADING)
    ToggleButtonSetModeMethodInfo           ,
#endif
    toggleButtonSetMode                     ,


-- ** toggled #method:toggled#

#if defined(ENABLE_OVERLOADING)
    ToggleButtonToggledMethodInfo           ,
#endif
    toggleButtonToggled                     ,




 -- * Properties


-- ** active #attr:active#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ToggleButtonActivePropertyInfo          ,
#endif
    constructToggleButtonActive             ,
    getToggleButtonActive                   ,
    setToggleButtonActive                   ,
#if defined(ENABLE_OVERLOADING)
    toggleButtonActive                      ,
#endif


-- ** drawIndicator #attr:drawIndicator#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ToggleButtonDrawIndicatorPropertyInfo   ,
#endif
    constructToggleButtonDrawIndicator      ,
    getToggleButtonDrawIndicator            ,
    setToggleButtonDrawIndicator            ,
#if defined(ENABLE_OVERLOADING)
    toggleButtonDrawIndicator               ,
#endif


-- ** inconsistent #attr:inconsistent#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ToggleButtonInconsistentPropertyInfo    ,
#endif
    constructToggleButtonInconsistent       ,
    getToggleButtonInconsistent             ,
    setToggleButtonInconsistent             ,
#if defined(ENABLE_OVERLOADING)
    toggleButtonInconsistent                ,
#endif




 -- * Signals


-- ** toggled #signal:toggled#

    ToggleButtonToggledCallback             ,
#if defined(ENABLE_OVERLOADING)
    ToggleButtonToggledSignalInfo           ,
#endif
    afterToggleButtonToggled                ,
    onToggleButtonToggled                   ,




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
import {-# SOURCE #-} qualified GI.Gtk.Objects.Bin as Gtk.Bin
import {-# SOURCE #-} qualified GI.Gtk.Objects.Button as Gtk.Button
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype ToggleButton = ToggleButton (SP.ManagedPtr ToggleButton)
    deriving (Eq)

instance SP.ManagedPtrNewtype ToggleButton where
    toManagedPtr (ToggleButton p) = p

foreign import ccall "gtk_toggle_button_get_type"
    c_gtk_toggle_button_get_type :: IO B.Types.GType

instance B.Types.TypedObject ToggleButton where
    glibType = c_gtk_toggle_button_get_type

instance B.Types.GObject ToggleButton

-- | Type class for types which can be safely cast to `ToggleButton`, for instance with `toToggleButton`.
class (SP.GObject o, O.IsDescendantOf ToggleButton o) => IsToggleButton o
instance (SP.GObject o, O.IsDescendantOf ToggleButton o) => IsToggleButton o

instance O.HasParentTypes ToggleButton
type instance O.ParentTypes ToggleButton = '[Gtk.Button.Button, Gtk.Bin.Bin, Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Actionable.Actionable, Gtk.Activatable.Activatable, Gtk.Buildable.Buildable]

-- | Cast to `ToggleButton`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toToggleButton :: (MIO.MonadIO m, IsToggleButton o) => o -> m ToggleButton
toToggleButton = MIO.liftIO . B.ManagedPtr.unsafeCastTo ToggleButton

-- | Convert 'ToggleButton' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe ToggleButton) where
    gvalueGType_ = c_gtk_toggle_button_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr ToggleButton)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr ToggleButton)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject ToggleButton ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveToggleButtonMethod (t :: Symbol) (o :: *) :: * where
    ResolveToggleButtonMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveToggleButtonMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveToggleButtonMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveToggleButtonMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveToggleButtonMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveToggleButtonMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveToggleButtonMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveToggleButtonMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveToggleButtonMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveToggleButtonMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveToggleButtonMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveToggleButtonMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveToggleButtonMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveToggleButtonMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveToggleButtonMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveToggleButtonMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveToggleButtonMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveToggleButtonMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveToggleButtonMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveToggleButtonMethod "clicked" o = Gtk.Button.ButtonClickedMethodInfo
    ResolveToggleButtonMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveToggleButtonMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveToggleButtonMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveToggleButtonMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveToggleButtonMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveToggleButtonMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveToggleButtonMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveToggleButtonMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveToggleButtonMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveToggleButtonMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveToggleButtonMethod "doSetRelatedAction" o = Gtk.Activatable.ActivatableDoSetRelatedActionMethodInfo
    ResolveToggleButtonMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveToggleButtonMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveToggleButtonMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveToggleButtonMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveToggleButtonMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveToggleButtonMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveToggleButtonMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveToggleButtonMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveToggleButtonMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveToggleButtonMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveToggleButtonMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveToggleButtonMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveToggleButtonMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveToggleButtonMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveToggleButtonMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveToggleButtonMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveToggleButtonMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveToggleButtonMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveToggleButtonMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveToggleButtonMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveToggleButtonMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveToggleButtonMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveToggleButtonMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveToggleButtonMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveToggleButtonMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveToggleButtonMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveToggleButtonMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveToggleButtonMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveToggleButtonMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveToggleButtonMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveToggleButtonMethod "enter" o = Gtk.Button.ButtonEnterMethodInfo
    ResolveToggleButtonMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveToggleButtonMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveToggleButtonMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveToggleButtonMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveToggleButtonMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveToggleButtonMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveToggleButtonMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveToggleButtonMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveToggleButtonMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveToggleButtonMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveToggleButtonMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveToggleButtonMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveToggleButtonMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveToggleButtonMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveToggleButtonMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveToggleButtonMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveToggleButtonMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveToggleButtonMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveToggleButtonMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveToggleButtonMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveToggleButtonMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveToggleButtonMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveToggleButtonMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveToggleButtonMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveToggleButtonMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveToggleButtonMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveToggleButtonMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveToggleButtonMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveToggleButtonMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveToggleButtonMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveToggleButtonMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveToggleButtonMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveToggleButtonMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveToggleButtonMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveToggleButtonMethod "leave" o = Gtk.Button.ButtonLeaveMethodInfo
    ResolveToggleButtonMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveToggleButtonMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveToggleButtonMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveToggleButtonMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveToggleButtonMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveToggleButtonMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveToggleButtonMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveToggleButtonMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveToggleButtonMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveToggleButtonMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveToggleButtonMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveToggleButtonMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveToggleButtonMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveToggleButtonMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveToggleButtonMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveToggleButtonMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveToggleButtonMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveToggleButtonMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveToggleButtonMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveToggleButtonMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveToggleButtonMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveToggleButtonMethod "pressed" o = Gtk.Button.ButtonPressedMethodInfo
    ResolveToggleButtonMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveToggleButtonMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveToggleButtonMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveToggleButtonMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveToggleButtonMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveToggleButtonMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveToggleButtonMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveToggleButtonMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveToggleButtonMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveToggleButtonMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveToggleButtonMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveToggleButtonMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveToggleButtonMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveToggleButtonMethod "released" o = Gtk.Button.ButtonReleasedMethodInfo
    ResolveToggleButtonMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolveToggleButtonMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveToggleButtonMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveToggleButtonMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveToggleButtonMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveToggleButtonMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveToggleButtonMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveToggleButtonMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveToggleButtonMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveToggleButtonMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveToggleButtonMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveToggleButtonMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveToggleButtonMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveToggleButtonMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveToggleButtonMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveToggleButtonMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveToggleButtonMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveToggleButtonMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveToggleButtonMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveToggleButtonMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveToggleButtonMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveToggleButtonMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveToggleButtonMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveToggleButtonMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveToggleButtonMethod "syncActionProperties" o = Gtk.Activatable.ActivatableSyncActionPropertiesMethodInfo
    ResolveToggleButtonMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveToggleButtonMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveToggleButtonMethod "toggled" o = ToggleButtonToggledMethodInfo
    ResolveToggleButtonMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveToggleButtonMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveToggleButtonMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveToggleButtonMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveToggleButtonMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveToggleButtonMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveToggleButtonMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveToggleButtonMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveToggleButtonMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveToggleButtonMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveToggleButtonMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveToggleButtonMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveToggleButtonMethod "getActionName" o = Gtk.Actionable.ActionableGetActionNameMethodInfo
    ResolveToggleButtonMethod "getActionTargetValue" o = Gtk.Actionable.ActionableGetActionTargetValueMethodInfo
    ResolveToggleButtonMethod "getActive" o = ToggleButtonGetActiveMethodInfo
    ResolveToggleButtonMethod "getAlignment" o = Gtk.Button.ButtonGetAlignmentMethodInfo
    ResolveToggleButtonMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveToggleButtonMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveToggleButtonMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveToggleButtonMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveToggleButtonMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveToggleButtonMethod "getAlwaysShowImage" o = Gtk.Button.ButtonGetAlwaysShowImageMethodInfo
    ResolveToggleButtonMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveToggleButtonMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveToggleButtonMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveToggleButtonMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveToggleButtonMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveToggleButtonMethod "getChild" o = Gtk.Bin.BinGetChildMethodInfo
    ResolveToggleButtonMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveToggleButtonMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveToggleButtonMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveToggleButtonMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveToggleButtonMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveToggleButtonMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveToggleButtonMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveToggleButtonMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveToggleButtonMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveToggleButtonMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveToggleButtonMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveToggleButtonMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveToggleButtonMethod "getEventWindow" o = Gtk.Button.ButtonGetEventWindowMethodInfo
    ResolveToggleButtonMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveToggleButtonMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveToggleButtonMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveToggleButtonMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveToggleButtonMethod "getFocusOnClick" o = Gtk.Button.ButtonGetFocusOnClickMethodInfo
    ResolveToggleButtonMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveToggleButtonMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveToggleButtonMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveToggleButtonMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveToggleButtonMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveToggleButtonMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveToggleButtonMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveToggleButtonMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveToggleButtonMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveToggleButtonMethod "getImage" o = Gtk.Button.ButtonGetImageMethodInfo
    ResolveToggleButtonMethod "getImagePosition" o = Gtk.Button.ButtonGetImagePositionMethodInfo
    ResolveToggleButtonMethod "getInconsistent" o = ToggleButtonGetInconsistentMethodInfo
    ResolveToggleButtonMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveToggleButtonMethod "getLabel" o = Gtk.Button.ButtonGetLabelMethodInfo
    ResolveToggleButtonMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveToggleButtonMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveToggleButtonMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveToggleButtonMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveToggleButtonMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveToggleButtonMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveToggleButtonMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveToggleButtonMethod "getMode" o = ToggleButtonGetModeMethodInfo
    ResolveToggleButtonMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveToggleButtonMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveToggleButtonMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveToggleButtonMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveToggleButtonMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveToggleButtonMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveToggleButtonMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveToggleButtonMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveToggleButtonMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveToggleButtonMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveToggleButtonMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveToggleButtonMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveToggleButtonMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveToggleButtonMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveToggleButtonMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveToggleButtonMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveToggleButtonMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveToggleButtonMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveToggleButtonMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveToggleButtonMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveToggleButtonMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveToggleButtonMethod "getRelatedAction" o = Gtk.Activatable.ActivatableGetRelatedActionMethodInfo
    ResolveToggleButtonMethod "getRelief" o = Gtk.Button.ButtonGetReliefMethodInfo
    ResolveToggleButtonMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveToggleButtonMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveToggleButtonMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveToggleButtonMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveToggleButtonMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveToggleButtonMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveToggleButtonMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveToggleButtonMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveToggleButtonMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveToggleButtonMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveToggleButtonMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveToggleButtonMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveToggleButtonMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveToggleButtonMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveToggleButtonMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveToggleButtonMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveToggleButtonMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveToggleButtonMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveToggleButtonMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveToggleButtonMethod "getUseActionAppearance" o = Gtk.Activatable.ActivatableGetUseActionAppearanceMethodInfo
    ResolveToggleButtonMethod "getUseStock" o = Gtk.Button.ButtonGetUseStockMethodInfo
    ResolveToggleButtonMethod "getUseUnderline" o = Gtk.Button.ButtonGetUseUnderlineMethodInfo
    ResolveToggleButtonMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveToggleButtonMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveToggleButtonMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveToggleButtonMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveToggleButtonMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveToggleButtonMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveToggleButtonMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveToggleButtonMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveToggleButtonMethod "setActionName" o = Gtk.Actionable.ActionableSetActionNameMethodInfo
    ResolveToggleButtonMethod "setActionTargetValue" o = Gtk.Actionable.ActionableSetActionTargetValueMethodInfo
    ResolveToggleButtonMethod "setActive" o = ToggleButtonSetActiveMethodInfo
    ResolveToggleButtonMethod "setAlignment" o = Gtk.Button.ButtonSetAlignmentMethodInfo
    ResolveToggleButtonMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveToggleButtonMethod "setAlwaysShowImage" o = Gtk.Button.ButtonSetAlwaysShowImageMethodInfo
    ResolveToggleButtonMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveToggleButtonMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveToggleButtonMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveToggleButtonMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveToggleButtonMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveToggleButtonMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveToggleButtonMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveToggleButtonMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveToggleButtonMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveToggleButtonMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveToggleButtonMethod "setDetailedActionName" o = Gtk.Actionable.ActionableSetDetailedActionNameMethodInfo
    ResolveToggleButtonMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveToggleButtonMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveToggleButtonMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveToggleButtonMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveToggleButtonMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveToggleButtonMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveToggleButtonMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveToggleButtonMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveToggleButtonMethod "setFocusOnClick" o = Gtk.Button.ButtonSetFocusOnClickMethodInfo
    ResolveToggleButtonMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveToggleButtonMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveToggleButtonMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveToggleButtonMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveToggleButtonMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveToggleButtonMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveToggleButtonMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveToggleButtonMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveToggleButtonMethod "setImage" o = Gtk.Button.ButtonSetImageMethodInfo
    ResolveToggleButtonMethod "setImagePosition" o = Gtk.Button.ButtonSetImagePositionMethodInfo
    ResolveToggleButtonMethod "setInconsistent" o = ToggleButtonSetInconsistentMethodInfo
    ResolveToggleButtonMethod "setLabel" o = Gtk.Button.ButtonSetLabelMethodInfo
    ResolveToggleButtonMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveToggleButtonMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveToggleButtonMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveToggleButtonMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveToggleButtonMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveToggleButtonMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveToggleButtonMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveToggleButtonMethod "setMode" o = ToggleButtonSetModeMethodInfo
    ResolveToggleButtonMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveToggleButtonMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveToggleButtonMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveToggleButtonMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveToggleButtonMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveToggleButtonMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveToggleButtonMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveToggleButtonMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveToggleButtonMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveToggleButtonMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveToggleButtonMethod "setRelatedAction" o = Gtk.Activatable.ActivatableSetRelatedActionMethodInfo
    ResolveToggleButtonMethod "setRelief" o = Gtk.Button.ButtonSetReliefMethodInfo
    ResolveToggleButtonMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveToggleButtonMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveToggleButtonMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveToggleButtonMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveToggleButtonMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveToggleButtonMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveToggleButtonMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveToggleButtonMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveToggleButtonMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveToggleButtonMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveToggleButtonMethod "setUseActionAppearance" o = Gtk.Activatable.ActivatableSetUseActionAppearanceMethodInfo
    ResolveToggleButtonMethod "setUseStock" o = Gtk.Button.ButtonSetUseStockMethodInfo
    ResolveToggleButtonMethod "setUseUnderline" o = Gtk.Button.ButtonSetUseUnderlineMethodInfo
    ResolveToggleButtonMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveToggleButtonMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveToggleButtonMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveToggleButtonMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveToggleButtonMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveToggleButtonMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveToggleButtonMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveToggleButtonMethod t ToggleButton, O.OverloadedMethod info ToggleButton p) => OL.IsLabel t (ToggleButton -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveToggleButtonMethod t ToggleButton, O.OverloadedMethod info ToggleButton p, R.HasField t ToggleButton p) => R.HasField t ToggleButton p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveToggleButtonMethod t ToggleButton, O.OverloadedMethodInfo info ToggleButton) => OL.IsLabel t (O.MethodProxy info ToggleButton) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal ToggleButton::toggled
-- | Should be connected if you wish to perform an action whenever the
-- t'GI.Gtk.Objects.ToggleButton.ToggleButton'\'s state is changed.
type ToggleButtonToggledCallback =
    IO ()

type C_ToggleButtonToggledCallback =
    Ptr ToggleButton ->                     -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ToggleButtonToggledCallback`.
foreign import ccall "wrapper"
    mk_ToggleButtonToggledCallback :: C_ToggleButtonToggledCallback -> IO (FunPtr C_ToggleButtonToggledCallback)

wrap_ToggleButtonToggledCallback :: 
    GObject a => (a -> ToggleButtonToggledCallback) ->
    C_ToggleButtonToggledCallback
wrap_ToggleButtonToggledCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [toggled](#signal:toggled) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' toggleButton #toggled callback
-- @
-- 
-- 
onToggleButtonToggled :: (IsToggleButton a, MonadIO m) => a -> ((?self :: a) => ToggleButtonToggledCallback) -> m SignalHandlerId
onToggleButtonToggled obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ToggleButtonToggledCallback wrapped
    wrapped'' <- mk_ToggleButtonToggledCallback wrapped'
    connectSignalFunPtr obj "toggled" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [toggled](#signal:toggled) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' toggleButton #toggled callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterToggleButtonToggled :: (IsToggleButton a, MonadIO m) => a -> ((?self :: a) => ToggleButtonToggledCallback) -> m SignalHandlerId
afterToggleButtonToggled obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ToggleButtonToggledCallback wrapped
    wrapped'' <- mk_ToggleButtonToggledCallback wrapped'
    connectSignalFunPtr obj "toggled" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ToggleButtonToggledSignalInfo
instance SignalInfo ToggleButtonToggledSignalInfo where
    type HaskellCallbackType ToggleButtonToggledSignalInfo = ToggleButtonToggledCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ToggleButtonToggledCallback cb
        cb'' <- mk_ToggleButtonToggledCallback cb'
        connectSignalFunPtr obj "toggled" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToggleButton::toggled"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToggleButton.html#g:signal:toggled"})

#endif

-- VVV Prop "active"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@active@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' toggleButton #active
-- @
getToggleButtonActive :: (MonadIO m, IsToggleButton o) => o -> m Bool
getToggleButtonActive obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "active"

-- | Set the value of the “@active@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' toggleButton [ #active 'Data.GI.Base.Attributes.:=' value ]
-- @
setToggleButtonActive :: (MonadIO m, IsToggleButton o) => o -> Bool -> m ()
setToggleButtonActive obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "active" val

-- | Construct a `GValueConstruct` with valid value for the “@active@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructToggleButtonActive :: (IsToggleButton o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructToggleButtonActive val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "active" val

#if defined(ENABLE_OVERLOADING)
data ToggleButtonActivePropertyInfo
instance AttrInfo ToggleButtonActivePropertyInfo where
    type AttrAllowedOps ToggleButtonActivePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ToggleButtonActivePropertyInfo = IsToggleButton
    type AttrSetTypeConstraint ToggleButtonActivePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint ToggleButtonActivePropertyInfo = (~) Bool
    type AttrTransferType ToggleButtonActivePropertyInfo = Bool
    type AttrGetType ToggleButtonActivePropertyInfo = Bool
    type AttrLabel ToggleButtonActivePropertyInfo = "active"
    type AttrOrigin ToggleButtonActivePropertyInfo = ToggleButton
    attrGet = getToggleButtonActive
    attrSet = setToggleButtonActive
    attrTransfer _ v = do
        return v
    attrConstruct = constructToggleButtonActive
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToggleButton.active"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToggleButton.html#g:attr:active"
        })
#endif

-- VVV Prop "draw-indicator"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@draw-indicator@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' toggleButton #drawIndicator
-- @
getToggleButtonDrawIndicator :: (MonadIO m, IsToggleButton o) => o -> m Bool
getToggleButtonDrawIndicator obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "draw-indicator"

-- | Set the value of the “@draw-indicator@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' toggleButton [ #drawIndicator 'Data.GI.Base.Attributes.:=' value ]
-- @
setToggleButtonDrawIndicator :: (MonadIO m, IsToggleButton o) => o -> Bool -> m ()
setToggleButtonDrawIndicator obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "draw-indicator" val

-- | Construct a `GValueConstruct` with valid value for the “@draw-indicator@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructToggleButtonDrawIndicator :: (IsToggleButton o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructToggleButtonDrawIndicator val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "draw-indicator" val

#if defined(ENABLE_OVERLOADING)
data ToggleButtonDrawIndicatorPropertyInfo
instance AttrInfo ToggleButtonDrawIndicatorPropertyInfo where
    type AttrAllowedOps ToggleButtonDrawIndicatorPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ToggleButtonDrawIndicatorPropertyInfo = IsToggleButton
    type AttrSetTypeConstraint ToggleButtonDrawIndicatorPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint ToggleButtonDrawIndicatorPropertyInfo = (~) Bool
    type AttrTransferType ToggleButtonDrawIndicatorPropertyInfo = Bool
    type AttrGetType ToggleButtonDrawIndicatorPropertyInfo = Bool
    type AttrLabel ToggleButtonDrawIndicatorPropertyInfo = "draw-indicator"
    type AttrOrigin ToggleButtonDrawIndicatorPropertyInfo = ToggleButton
    attrGet = getToggleButtonDrawIndicator
    attrSet = setToggleButtonDrawIndicator
    attrTransfer _ v = do
        return v
    attrConstruct = constructToggleButtonDrawIndicator
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToggleButton.drawIndicator"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToggleButton.html#g:attr:drawIndicator"
        })
#endif

-- VVV Prop "inconsistent"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@inconsistent@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' toggleButton #inconsistent
-- @
getToggleButtonInconsistent :: (MonadIO m, IsToggleButton o) => o -> m Bool
getToggleButtonInconsistent obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "inconsistent"

-- | Set the value of the “@inconsistent@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' toggleButton [ #inconsistent 'Data.GI.Base.Attributes.:=' value ]
-- @
setToggleButtonInconsistent :: (MonadIO m, IsToggleButton o) => o -> Bool -> m ()
setToggleButtonInconsistent obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "inconsistent" val

-- | Construct a `GValueConstruct` with valid value for the “@inconsistent@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructToggleButtonInconsistent :: (IsToggleButton o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructToggleButtonInconsistent val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "inconsistent" val

#if defined(ENABLE_OVERLOADING)
data ToggleButtonInconsistentPropertyInfo
instance AttrInfo ToggleButtonInconsistentPropertyInfo where
    type AttrAllowedOps ToggleButtonInconsistentPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ToggleButtonInconsistentPropertyInfo = IsToggleButton
    type AttrSetTypeConstraint ToggleButtonInconsistentPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint ToggleButtonInconsistentPropertyInfo = (~) Bool
    type AttrTransferType ToggleButtonInconsistentPropertyInfo = Bool
    type AttrGetType ToggleButtonInconsistentPropertyInfo = Bool
    type AttrLabel ToggleButtonInconsistentPropertyInfo = "inconsistent"
    type AttrOrigin ToggleButtonInconsistentPropertyInfo = ToggleButton
    attrGet = getToggleButtonInconsistent
    attrSet = setToggleButtonInconsistent
    attrTransfer _ v = do
        return v
    attrConstruct = constructToggleButtonInconsistent
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToggleButton.inconsistent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToggleButton.html#g:attr:inconsistent"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList ToggleButton
type instance O.AttributeList ToggleButton = ToggleButtonAttributeList
type ToggleButtonAttributeList = ('[ '("actionName", Gtk.Actionable.ActionableActionNamePropertyInfo), '("actionTarget", Gtk.Actionable.ActionableActionTargetPropertyInfo), '("active", ToggleButtonActivePropertyInfo), '("alwaysShowImage", Gtk.Button.ButtonAlwaysShowImagePropertyInfo), '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("drawIndicator", ToggleButtonDrawIndicatorPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("image", Gtk.Button.ButtonImagePropertyInfo), '("imagePosition", Gtk.Button.ButtonImagePositionPropertyInfo), '("inconsistent", ToggleButtonInconsistentPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("label", Gtk.Button.ButtonLabelPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("relatedAction", Gtk.Activatable.ActivatableRelatedActionPropertyInfo), '("relief", Gtk.Button.ButtonReliefPropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("useActionAppearance", Gtk.Activatable.ActivatableUseActionAppearancePropertyInfo), '("useStock", Gtk.Button.ButtonUseStockPropertyInfo), '("useUnderline", Gtk.Button.ButtonUseUnderlinePropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo), '("xalign", Gtk.Button.ButtonXalignPropertyInfo), '("yalign", Gtk.Button.ButtonYalignPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
toggleButtonActive :: AttrLabelProxy "active"
toggleButtonActive = AttrLabelProxy

toggleButtonDrawIndicator :: AttrLabelProxy "drawIndicator"
toggleButtonDrawIndicator = AttrLabelProxy

toggleButtonInconsistent :: AttrLabelProxy "inconsistent"
toggleButtonInconsistent = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList ToggleButton = ToggleButtonSignalList
type ToggleButtonSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("activate", Gtk.Button.ButtonActivateSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("clicked", Gtk.Button.ButtonClickedSignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enter", Gtk.Button.ButtonEnterSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leave", Gtk.Button.ButtonLeaveSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("pressed", Gtk.Button.ButtonPressedSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("released", Gtk.Button.ButtonReleasedSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("toggled", ToggleButtonToggledSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method ToggleButton::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "ToggleButton" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_toggle_button_new" gtk_toggle_button_new :: 
    IO (Ptr ToggleButton)

-- | Creates a new toggle button. A widget should be packed into the button, as in 'GI.Gtk.Objects.Button.buttonNew'.
toggleButtonNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m ToggleButton
    -- ^ __Returns:__ a new toggle button.
toggleButtonNew  = liftIO $ do
    result <- gtk_toggle_button_new
    checkUnexpectedReturnNULL "toggleButtonNew" result
    result' <- (newObject ToggleButton) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method ToggleButton::new_with_label
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
--                       "a string containing the message to be placed in the toggle button."
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
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "ToggleButton" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_toggle_button_new_with_label" gtk_toggle_button_new_with_label :: 
    CString ->                              -- label : TBasicType TUTF8
    IO (Ptr ToggleButton)

-- | Creates a new toggle button with a text label.
toggleButtonNewWithLabel ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@label@/: a string containing the message to be placed in the toggle button.
    -> m ToggleButton
    -- ^ __Returns:__ a new toggle button.
toggleButtonNewWithLabel label = liftIO $ do
    label' <- textToCString label
    result <- gtk_toggle_button_new_with_label label'
    checkUnexpectedReturnNULL "toggleButtonNewWithLabel" result
    result' <- (newObject ToggleButton) result
    freeMem label'
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method ToggleButton::new_with_mnemonic
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
--                       "the text of the button, with an underscore in front of the\n        mnemonic character"
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
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "ToggleButton" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_toggle_button_new_with_mnemonic" gtk_toggle_button_new_with_mnemonic :: 
    CString ->                              -- label : TBasicType TUTF8
    IO (Ptr ToggleButton)

-- | Creates a new t'GI.Gtk.Objects.ToggleButton.ToggleButton' containing a label. The label
-- will be created using 'GI.Gtk.Objects.Label.labelNewWithMnemonic', so underscores
-- in /@label@/ indicate the mnemonic for the button.
toggleButtonNewWithMnemonic ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@label@/: the text of the button, with an underscore in front of the
    --         mnemonic character
    -> m ToggleButton
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.ToggleButton.ToggleButton'
toggleButtonNewWithMnemonic label = liftIO $ do
    label' <- textToCString label
    result <- gtk_toggle_button_new_with_mnemonic label'
    checkUnexpectedReturnNULL "toggleButtonNewWithMnemonic" result
    result' <- (newObject ToggleButton) result
    freeMem label'
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method ToggleButton::get_active
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "toggle_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToggleButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToggleButton."
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

foreign import ccall "gtk_toggle_button_get_active" gtk_toggle_button_get_active :: 
    Ptr ToggleButton ->                     -- toggle_button : TInterface (Name {namespace = "Gtk", name = "ToggleButton"})
    IO CInt

-- | Queries a t'GI.Gtk.Objects.ToggleButton.ToggleButton' and returns its current state. Returns 'P.True' if
-- the toggle button is pressed in and 'P.False' if it is raised.
toggleButtonGetActive ::
    (B.CallStack.HasCallStack, MonadIO m, IsToggleButton a) =>
    a
    -- ^ /@toggleButton@/: a t'GI.Gtk.Objects.ToggleButton.ToggleButton'.
    -> m Bool
    -- ^ __Returns:__ a t'P.Bool' value.
toggleButtonGetActive toggleButton = liftIO $ do
    toggleButton' <- unsafeManagedPtrCastPtr toggleButton
    result <- gtk_toggle_button_get_active toggleButton'
    let result' = (/= 0) result
    touchManagedPtr toggleButton
    return result'

#if defined(ENABLE_OVERLOADING)
data ToggleButtonGetActiveMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsToggleButton a) => O.OverloadedMethod ToggleButtonGetActiveMethodInfo a signature where
    overloadedMethod = toggleButtonGetActive

instance O.OverloadedMethodInfo ToggleButtonGetActiveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToggleButton.toggleButtonGetActive",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToggleButton.html#v:toggleButtonGetActive"
        })


#endif

-- method ToggleButton::get_inconsistent
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "toggle_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToggleButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToggleButton" , sinceVersion = Nothing }
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

foreign import ccall "gtk_toggle_button_get_inconsistent" gtk_toggle_button_get_inconsistent :: 
    Ptr ToggleButton ->                     -- toggle_button : TInterface (Name {namespace = "Gtk", name = "ToggleButton"})
    IO CInt

-- | Gets the value set by 'GI.Gtk.Objects.ToggleButton.toggleButtonSetInconsistent'.
toggleButtonGetInconsistent ::
    (B.CallStack.HasCallStack, MonadIO m, IsToggleButton a) =>
    a
    -- ^ /@toggleButton@/: a t'GI.Gtk.Objects.ToggleButton.ToggleButton'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the button is displayed as inconsistent, 'P.False' otherwise
toggleButtonGetInconsistent toggleButton = liftIO $ do
    toggleButton' <- unsafeManagedPtrCastPtr toggleButton
    result <- gtk_toggle_button_get_inconsistent toggleButton'
    let result' = (/= 0) result
    touchManagedPtr toggleButton
    return result'

#if defined(ENABLE_OVERLOADING)
data ToggleButtonGetInconsistentMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsToggleButton a) => O.OverloadedMethod ToggleButtonGetInconsistentMethodInfo a signature where
    overloadedMethod = toggleButtonGetInconsistent

instance O.OverloadedMethodInfo ToggleButtonGetInconsistentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToggleButton.toggleButtonGetInconsistent",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToggleButton.html#v:toggleButtonGetInconsistent"
        })


#endif

-- method ToggleButton::get_mode
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "toggle_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToggleButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToggleButton" , sinceVersion = Nothing }
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

foreign import ccall "gtk_toggle_button_get_mode" gtk_toggle_button_get_mode :: 
    Ptr ToggleButton ->                     -- toggle_button : TInterface (Name {namespace = "Gtk", name = "ToggleButton"})
    IO CInt

-- | Retrieves whether the button is displayed as a separate indicator
-- and label. See 'GI.Gtk.Objects.ToggleButton.toggleButtonSetMode'.
toggleButtonGetMode ::
    (B.CallStack.HasCallStack, MonadIO m, IsToggleButton a) =>
    a
    -- ^ /@toggleButton@/: a t'GI.Gtk.Objects.ToggleButton.ToggleButton'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the togglebutton is drawn as a separate indicator
    --   and label.
toggleButtonGetMode toggleButton = liftIO $ do
    toggleButton' <- unsafeManagedPtrCastPtr toggleButton
    result <- gtk_toggle_button_get_mode toggleButton'
    let result' = (/= 0) result
    touchManagedPtr toggleButton
    return result'

#if defined(ENABLE_OVERLOADING)
data ToggleButtonGetModeMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsToggleButton a) => O.OverloadedMethod ToggleButtonGetModeMethodInfo a signature where
    overloadedMethod = toggleButtonGetMode

instance O.OverloadedMethodInfo ToggleButtonGetModeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToggleButton.toggleButtonGetMode",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToggleButton.html#v:toggleButtonGetMode"
        })


#endif

-- method ToggleButton::set_active
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "toggle_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToggleButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToggleButton."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "is_active"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE or %FALSE." , sinceVersion = Nothing }
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

foreign import ccall "gtk_toggle_button_set_active" gtk_toggle_button_set_active :: 
    Ptr ToggleButton ->                     -- toggle_button : TInterface (Name {namespace = "Gtk", name = "ToggleButton"})
    CInt ->                                 -- is_active : TBasicType TBoolean
    IO ()

-- | Sets the status of the toggle button. Set to 'P.True' if you want the
-- GtkToggleButton to be “pressed in”, and 'P.False' to raise it.
-- This action causes the [ToggleButton::toggled]("GI.Gtk.Objects.ToggleButton#g:signal:toggled") signal and the
-- [Button::clicked]("GI.Gtk.Objects.Button#g:signal:clicked") signal to be emitted.
toggleButtonSetActive ::
    (B.CallStack.HasCallStack, MonadIO m, IsToggleButton a) =>
    a
    -- ^ /@toggleButton@/: a t'GI.Gtk.Objects.ToggleButton.ToggleButton'.
    -> Bool
    -- ^ /@isActive@/: 'P.True' or 'P.False'.
    -> m ()
toggleButtonSetActive toggleButton isActive = liftIO $ do
    toggleButton' <- unsafeManagedPtrCastPtr toggleButton
    let isActive' = (fromIntegral . fromEnum) isActive
    gtk_toggle_button_set_active toggleButton' isActive'
    touchManagedPtr toggleButton
    return ()

#if defined(ENABLE_OVERLOADING)
data ToggleButtonSetActiveMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsToggleButton a) => O.OverloadedMethod ToggleButtonSetActiveMethodInfo a signature where
    overloadedMethod = toggleButtonSetActive

instance O.OverloadedMethodInfo ToggleButtonSetActiveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToggleButton.toggleButtonSetActive",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToggleButton.html#v:toggleButtonSetActive"
        })


#endif

-- method ToggleButton::set_inconsistent
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "toggle_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToggleButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToggleButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "setting"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE if state is inconsistent"
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

foreign import ccall "gtk_toggle_button_set_inconsistent" gtk_toggle_button_set_inconsistent :: 
    Ptr ToggleButton ->                     -- toggle_button : TInterface (Name {namespace = "Gtk", name = "ToggleButton"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | If the user has selected a range of elements (such as some text or
-- spreadsheet cells) that are affected by a toggle button, and the
-- current values in that range are inconsistent, you may want to
-- display the toggle in an “in between” state. This function turns on
-- “in between” display.  Normally you would turn off the inconsistent
-- state again if the user toggles the toggle button. This has to be
-- done manually, 'GI.Gtk.Objects.ToggleButton.toggleButtonSetInconsistent' only affects
-- visual appearance, it doesn’t affect the semantics of the button.
toggleButtonSetInconsistent ::
    (B.CallStack.HasCallStack, MonadIO m, IsToggleButton a) =>
    a
    -- ^ /@toggleButton@/: a t'GI.Gtk.Objects.ToggleButton.ToggleButton'
    -> Bool
    -- ^ /@setting@/: 'P.True' if state is inconsistent
    -> m ()
toggleButtonSetInconsistent toggleButton setting = liftIO $ do
    toggleButton' <- unsafeManagedPtrCastPtr toggleButton
    let setting' = (fromIntegral . fromEnum) setting
    gtk_toggle_button_set_inconsistent toggleButton' setting'
    touchManagedPtr toggleButton
    return ()

#if defined(ENABLE_OVERLOADING)
data ToggleButtonSetInconsistentMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsToggleButton a) => O.OverloadedMethod ToggleButtonSetInconsistentMethodInfo a signature where
    overloadedMethod = toggleButtonSetInconsistent

instance O.OverloadedMethodInfo ToggleButtonSetInconsistentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToggleButton.toggleButtonSetInconsistent",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToggleButton.html#v:toggleButtonSetInconsistent"
        })


#endif

-- method ToggleButton::set_mode
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "toggle_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToggleButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToggleButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "draw_indicator"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "if %TRUE, draw the button as a separate indicator\nand label; if %FALSE, draw the button like a normal button"
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

foreign import ccall "gtk_toggle_button_set_mode" gtk_toggle_button_set_mode :: 
    Ptr ToggleButton ->                     -- toggle_button : TInterface (Name {namespace = "Gtk", name = "ToggleButton"})
    CInt ->                                 -- draw_indicator : TBasicType TBoolean
    IO ()

-- | Sets whether the button is displayed as a separate indicator and label.
-- You can call this function on a checkbutton or a radiobutton with
-- /@drawIndicator@/ = 'P.False' to make the button look like a normal button.
-- 
-- This can be used to create linked strip of buttons that work like
-- a t'GI.Gtk.Objects.StackSwitcher.StackSwitcher'.
-- 
-- This function only affects instances of classes like t'GI.Gtk.Objects.CheckButton.CheckButton'
-- and t'GI.Gtk.Objects.RadioButton.RadioButton' that derive from t'GI.Gtk.Objects.ToggleButton.ToggleButton',
-- not instances of t'GI.Gtk.Objects.ToggleButton.ToggleButton' itself.
toggleButtonSetMode ::
    (B.CallStack.HasCallStack, MonadIO m, IsToggleButton a) =>
    a
    -- ^ /@toggleButton@/: a t'GI.Gtk.Objects.ToggleButton.ToggleButton'
    -> Bool
    -- ^ /@drawIndicator@/: if 'P.True', draw the button as a separate indicator
    -- and label; if 'P.False', draw the button like a normal button
    -> m ()
toggleButtonSetMode toggleButton drawIndicator = liftIO $ do
    toggleButton' <- unsafeManagedPtrCastPtr toggleButton
    let drawIndicator' = (fromIntegral . fromEnum) drawIndicator
    gtk_toggle_button_set_mode toggleButton' drawIndicator'
    touchManagedPtr toggleButton
    return ()

#if defined(ENABLE_OVERLOADING)
data ToggleButtonSetModeMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsToggleButton a) => O.OverloadedMethod ToggleButtonSetModeMethodInfo a signature where
    overloadedMethod = toggleButtonSetMode

instance O.OverloadedMethodInfo ToggleButtonSetModeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToggleButton.toggleButtonSetMode",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToggleButton.html#v:toggleButtonSetMode"
        })


#endif

-- method ToggleButton::toggled
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "toggle_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToggleButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToggleButton."
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

foreign import ccall "gtk_toggle_button_toggled" gtk_toggle_button_toggled :: 
    Ptr ToggleButton ->                     -- toggle_button : TInterface (Name {namespace = "Gtk", name = "ToggleButton"})
    IO ()

-- | Emits the [ToggleButton::toggled]("GI.Gtk.Objects.ToggleButton#g:signal:toggled") signal on the
-- t'GI.Gtk.Objects.ToggleButton.ToggleButton'. There is no good reason for an
-- application ever to call this function.
toggleButtonToggled ::
    (B.CallStack.HasCallStack, MonadIO m, IsToggleButton a) =>
    a
    -- ^ /@toggleButton@/: a t'GI.Gtk.Objects.ToggleButton.ToggleButton'.
    -> m ()
toggleButtonToggled toggleButton = liftIO $ do
    toggleButton' <- unsafeManagedPtrCastPtr toggleButton
    gtk_toggle_button_toggled toggleButton'
    touchManagedPtr toggleButton
    return ()

#if defined(ENABLE_OVERLOADING)
data ToggleButtonToggledMethodInfo
instance (signature ~ (m ()), MonadIO m, IsToggleButton a) => O.OverloadedMethod ToggleButtonToggledMethodInfo a signature where
    overloadedMethod = toggleButtonToggled

instance O.OverloadedMethodInfo ToggleButtonToggledMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToggleButton.toggleButtonToggled",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToggleButton.html#v:toggleButtonToggled"
        })


#endif


