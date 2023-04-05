{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- The t'GI.Gtk.Objects.AccelLabel.AccelLabel' widget is a subclass of t'GI.Gtk.Objects.Label.Label' that also displays an
-- accelerator key on the right of the label text, e.g. “Ctrl+S”.
-- It is commonly used in menus to show the keyboard short-cuts for commands.
-- 
-- The accelerator key to display is typically not set explicitly (although it
-- can be, with 'GI.Gtk.Objects.AccelLabel.accelLabelSetAccel'). Instead, the t'GI.Gtk.Objects.AccelLabel.AccelLabel' displays
-- the accelerators which have been added to a particular widget. This widget is
-- set by calling 'GI.Gtk.Objects.AccelLabel.accelLabelSetAccelWidget'.
-- 
-- For example, a t'GI.Gtk.Objects.MenuItem.MenuItem' widget may have an accelerator added to emit
-- the “activate” signal when the “Ctrl+S” key combination is pressed.
-- A t'GI.Gtk.Objects.AccelLabel.AccelLabel' is created and added to the t'GI.Gtk.Objects.MenuItem.MenuItem', and
-- 'GI.Gtk.Objects.AccelLabel.accelLabelSetAccelWidget' is called with the t'GI.Gtk.Objects.MenuItem.MenuItem' as the
-- second argument. The t'GI.Gtk.Objects.AccelLabel.AccelLabel' will now display “Ctrl+S” after its label.
-- 
-- Note that creating a t'GI.Gtk.Objects.MenuItem.MenuItem' with 'GI.Gtk.Objects.MenuItem.menuItemNewWithLabel' (or
-- one of the similar functions for t'GI.Gtk.Objects.CheckMenuItem.CheckMenuItem' and t'GI.Gtk.Objects.RadioMenuItem.RadioMenuItem')
-- automatically adds a t'GI.Gtk.Objects.AccelLabel.AccelLabel' to the t'GI.Gtk.Objects.MenuItem.MenuItem' and calls
-- 'GI.Gtk.Objects.AccelLabel.accelLabelSetAccelWidget' to set it up for you.
-- 
-- A t'GI.Gtk.Objects.AccelLabel.AccelLabel' will only display accelerators which have 'GI.Gtk.Flags.AccelFlagsVisible'
-- set (see t'GI.Gtk.Flags.AccelFlags').
-- A t'GI.Gtk.Objects.AccelLabel.AccelLabel' can display multiple accelerators and even signal names,
-- though it is almost always used to display just one accelerator key.
-- 
-- == Creating a simple menu item with an accelerator key.
-- 
-- 
-- === /C code/
-- >
-- >  GtkWidget *window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
-- >  GtkWidget *menu = gtk_menu_new ();
-- >  GtkWidget *save_item;
-- >  GtkAccelGroup *accel_group;
-- >
-- >  // Create a GtkAccelGroup and add it to the window.
-- >  accel_group = gtk_accel_group_new ();
-- >  gtk_window_add_accel_group (GTK_WINDOW (window), accel_group);
-- >
-- >  // Create the menu item using the convenience function.
-- >  save_item = gtk_menu_item_new_with_label ("Save");
-- >  gtk_widget_show (save_item);
-- >  gtk_container_add (GTK_CONTAINER (menu), save_item);
-- >
-- >  // Now add the accelerator to the GtkMenuItem. Note that since we
-- >  // called gtk_menu_item_new_with_label() to create the GtkMenuItem
-- >  // the GtkAccelLabel is automatically set up to display the
-- >  // GtkMenuItem accelerators. We just need to make sure we use
-- >  // GTK_ACCEL_VISIBLE here.
-- >  gtk_widget_add_accelerator (save_item, "activate", accel_group,
-- >                              GDK_KEY_s, GDK_CONTROL_MASK, GTK_ACCEL_VISIBLE);
-- 
-- 
-- = CSS nodes
-- 
-- 
-- === /plain code/
-- >
-- >label
-- >╰── accelerator
-- 
-- 
-- Like t'GI.Gtk.Objects.Label.Label', GtkAccelLabel has a main CSS node with the name label.
-- It adds a subnode with name accelerator.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.AccelLabel
    ( 

-- * Exported types
    AccelLabel(..)                          ,
    IsAccelLabel                            ,
    toAccelLabel                            ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childNotify]("GI.Gtk.Objects.Widget#g:method:childNotify"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [refetch]("GI.Gtk.Objects.AccelLabel#g:method:refetch"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [selectRegion]("GI.Gtk.Objects.Label#g:method:selectRegion"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccel]("GI.Gtk.Objects.AccelLabel#g:method:getAccel"), [getAccelWidget]("GI.Gtk.Objects.AccelLabel#g:method:getAccelWidget"), [getAccelWidth]("GI.Gtk.Objects.AccelLabel#g:method:getAccelWidth"), [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getAlignment]("GI.Gtk.Objects.Misc#g:method:getAlignment"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAngle]("GI.Gtk.Objects.Label#g:method:getAngle"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getAttributes]("GI.Gtk.Objects.Label#g:method:getAttributes"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getCurrentUri]("GI.Gtk.Objects.Label#g:method:getCurrentUri"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEllipsize]("GI.Gtk.Objects.Label#g:method:getEllipsize"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getJustify]("GI.Gtk.Objects.Label#g:method:getJustify"), [getLabel]("GI.Gtk.Objects.Label#g:method:getLabel"), [getLayout]("GI.Gtk.Objects.Label#g:method:getLayout"), [getLayoutOffsets]("GI.Gtk.Objects.Label#g:method:getLayoutOffsets"), [getLineWrap]("GI.Gtk.Objects.Label#g:method:getLineWrap"), [getLineWrapMode]("GI.Gtk.Objects.Label#g:method:getLineWrapMode"), [getLines]("GI.Gtk.Objects.Label#g:method:getLines"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getMaxWidthChars]("GI.Gtk.Objects.Label#g:method:getMaxWidthChars"), [getMnemonicKeyval]("GI.Gtk.Objects.Label#g:method:getMnemonicKeyval"), [getMnemonicWidget]("GI.Gtk.Objects.Label#g:method:getMnemonicWidget"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getPadding]("GI.Gtk.Objects.Misc#g:method:getPadding"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSelectable]("GI.Gtk.Objects.Label#g:method:getSelectable"), [getSelectionBounds]("GI.Gtk.Objects.Label#g:method:getSelectionBounds"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSingleLineMode]("GI.Gtk.Objects.Label#g:method:getSingleLineMode"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getText]("GI.Gtk.Objects.Label#g:method:getText"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getTrackVisitedLinks]("GI.Gtk.Objects.Label#g:method:getTrackVisitedLinks"), [getUseMarkup]("GI.Gtk.Objects.Label#g:method:getUseMarkup"), [getUseUnderline]("GI.Gtk.Objects.Label#g:method:getUseUnderline"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWidthChars]("GI.Gtk.Objects.Label#g:method:getWidthChars"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow"), [getXalign]("GI.Gtk.Objects.Label#g:method:getXalign"), [getYalign]("GI.Gtk.Objects.Label#g:method:getYalign").
-- 
-- ==== Setters
-- [setAccel]("GI.Gtk.Objects.AccelLabel#g:method:setAccel"), [setAccelClosure]("GI.Gtk.Objects.AccelLabel#g:method:setAccelClosure"), [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setAccelWidget]("GI.Gtk.Objects.AccelLabel#g:method:setAccelWidget"), [setAlignment]("GI.Gtk.Objects.Misc#g:method:setAlignment"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAngle]("GI.Gtk.Objects.Label#g:method:setAngle"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setAttributes]("GI.Gtk.Objects.Label#g:method:setAttributes"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEllipsize]("GI.Gtk.Objects.Label#g:method:setEllipsize"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setJustify]("GI.Gtk.Objects.Label#g:method:setJustify"), [setLabel]("GI.Gtk.Objects.Label#g:method:setLabel"), [setLineWrap]("GI.Gtk.Objects.Label#g:method:setLineWrap"), [setLineWrapMode]("GI.Gtk.Objects.Label#g:method:setLineWrapMode"), [setLines]("GI.Gtk.Objects.Label#g:method:setLines"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setMarkup]("GI.Gtk.Objects.Label#g:method:setMarkup"), [setMarkupWithMnemonic]("GI.Gtk.Objects.Label#g:method:setMarkupWithMnemonic"), [setMaxWidthChars]("GI.Gtk.Objects.Label#g:method:setMaxWidthChars"), [setMnemonicWidget]("GI.Gtk.Objects.Label#g:method:setMnemonicWidget"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setPadding]("GI.Gtk.Objects.Misc#g:method:setPadding"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setPattern]("GI.Gtk.Objects.Label#g:method:setPattern"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setSelectable]("GI.Gtk.Objects.Label#g:method:setSelectable"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSingleLineMode]("GI.Gtk.Objects.Label#g:method:setSingleLineMode"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setText]("GI.Gtk.Objects.Label#g:method:setText"), [setTextWithMnemonic]("GI.Gtk.Objects.Label#g:method:setTextWithMnemonic"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setTrackVisitedLinks]("GI.Gtk.Objects.Label#g:method:setTrackVisitedLinks"), [setUseMarkup]("GI.Gtk.Objects.Label#g:method:setUseMarkup"), [setUseUnderline]("GI.Gtk.Objects.Label#g:method:setUseUnderline"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWidthChars]("GI.Gtk.Objects.Label#g:method:setWidthChars"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow"), [setXalign]("GI.Gtk.Objects.Label#g:method:setXalign"), [setYalign]("GI.Gtk.Objects.Label#g:method:setYalign").

#if defined(ENABLE_OVERLOADING)
    ResolveAccelLabelMethod                 ,
#endif

-- ** getAccel #method:getAccel#

#if defined(ENABLE_OVERLOADING)
    AccelLabelGetAccelMethodInfo            ,
#endif
    accelLabelGetAccel                      ,


-- ** getAccelWidget #method:getAccelWidget#

#if defined(ENABLE_OVERLOADING)
    AccelLabelGetAccelWidgetMethodInfo      ,
#endif
    accelLabelGetAccelWidget                ,


-- ** getAccelWidth #method:getAccelWidth#

#if defined(ENABLE_OVERLOADING)
    AccelLabelGetAccelWidthMethodInfo       ,
#endif
    accelLabelGetAccelWidth                 ,


-- ** new #method:new#

    accelLabelNew                           ,


-- ** refetch #method:refetch#

#if defined(ENABLE_OVERLOADING)
    AccelLabelRefetchMethodInfo             ,
#endif
    accelLabelRefetch                       ,


-- ** setAccel #method:setAccel#

#if defined(ENABLE_OVERLOADING)
    AccelLabelSetAccelMethodInfo            ,
#endif
    accelLabelSetAccel                      ,


-- ** setAccelClosure #method:setAccelClosure#

#if defined(ENABLE_OVERLOADING)
    AccelLabelSetAccelClosureMethodInfo     ,
#endif
    accelLabelSetAccelClosure               ,


-- ** setAccelWidget #method:setAccelWidget#

#if defined(ENABLE_OVERLOADING)
    AccelLabelSetAccelWidgetMethodInfo      ,
#endif
    accelLabelSetAccelWidget                ,




 -- * Properties


-- ** accelClosure #attr:accelClosure#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    AccelLabelAccelClosurePropertyInfo      ,
#endif
#if defined(ENABLE_OVERLOADING)
    accelLabelAccelClosure                  ,
#endif
    clearAccelLabelAccelClosure             ,
    constructAccelLabelAccelClosure         ,
    getAccelLabelAccelClosure               ,
    setAccelLabelAccelClosure               ,


-- ** accelWidget #attr:accelWidget#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    AccelLabelAccelWidgetPropertyInfo       ,
#endif
#if defined(ENABLE_OVERLOADING)
    accelLabelAccelWidget                   ,
#endif
    clearAccelLabelAccelWidget              ,
    constructAccelLabelAccelWidget          ,
    getAccelLabelAccelWidget                ,
    setAccelLabelAccelWidget                ,




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
import qualified GI.Gdk.Flags as Gdk.Flags
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Objects.Label as Gtk.Label
import {-# SOURCE #-} qualified GI.Gtk.Objects.Misc as Gtk.Misc
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype AccelLabel = AccelLabel (SP.ManagedPtr AccelLabel)
    deriving (Eq)

instance SP.ManagedPtrNewtype AccelLabel where
    toManagedPtr (AccelLabel p) = p

foreign import ccall "gtk_accel_label_get_type"
    c_gtk_accel_label_get_type :: IO B.Types.GType

instance B.Types.TypedObject AccelLabel where
    glibType = c_gtk_accel_label_get_type

instance B.Types.GObject AccelLabel

-- | Type class for types which can be safely cast to `AccelLabel`, for instance with `toAccelLabel`.
class (SP.GObject o, O.IsDescendantOf AccelLabel o) => IsAccelLabel o
instance (SP.GObject o, O.IsDescendantOf AccelLabel o) => IsAccelLabel o

instance O.HasParentTypes AccelLabel
type instance O.ParentTypes AccelLabel = '[Gtk.Label.Label, Gtk.Misc.Misc, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable]

-- | Cast to `AccelLabel`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toAccelLabel :: (MIO.MonadIO m, IsAccelLabel o) => o -> m AccelLabel
toAccelLabel = MIO.liftIO . B.ManagedPtr.unsafeCastTo AccelLabel

-- | Convert 'AccelLabel' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe AccelLabel) where
    gvalueGType_ = c_gtk_accel_label_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr AccelLabel)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr AccelLabel)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject AccelLabel ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveAccelLabelMethod (t :: Symbol) (o :: *) :: * where
    ResolveAccelLabelMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveAccelLabelMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveAccelLabelMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveAccelLabelMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveAccelLabelMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveAccelLabelMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveAccelLabelMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveAccelLabelMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveAccelLabelMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveAccelLabelMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveAccelLabelMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveAccelLabelMethod "childNotify" o = Gtk.Widget.WidgetChildNotifyMethodInfo
    ResolveAccelLabelMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveAccelLabelMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveAccelLabelMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveAccelLabelMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveAccelLabelMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveAccelLabelMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveAccelLabelMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveAccelLabelMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveAccelLabelMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveAccelLabelMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveAccelLabelMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveAccelLabelMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveAccelLabelMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveAccelLabelMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveAccelLabelMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveAccelLabelMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveAccelLabelMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveAccelLabelMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveAccelLabelMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveAccelLabelMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveAccelLabelMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveAccelLabelMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveAccelLabelMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveAccelLabelMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveAccelLabelMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveAccelLabelMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveAccelLabelMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveAccelLabelMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveAccelLabelMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveAccelLabelMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveAccelLabelMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveAccelLabelMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveAccelLabelMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveAccelLabelMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveAccelLabelMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveAccelLabelMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveAccelLabelMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveAccelLabelMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveAccelLabelMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveAccelLabelMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveAccelLabelMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveAccelLabelMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveAccelLabelMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveAccelLabelMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveAccelLabelMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveAccelLabelMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveAccelLabelMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveAccelLabelMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveAccelLabelMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveAccelLabelMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveAccelLabelMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveAccelLabelMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveAccelLabelMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveAccelLabelMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveAccelLabelMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveAccelLabelMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveAccelLabelMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveAccelLabelMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveAccelLabelMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveAccelLabelMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveAccelLabelMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveAccelLabelMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveAccelLabelMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveAccelLabelMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveAccelLabelMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveAccelLabelMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveAccelLabelMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveAccelLabelMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveAccelLabelMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveAccelLabelMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveAccelLabelMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveAccelLabelMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveAccelLabelMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveAccelLabelMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveAccelLabelMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveAccelLabelMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveAccelLabelMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveAccelLabelMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveAccelLabelMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveAccelLabelMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveAccelLabelMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveAccelLabelMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveAccelLabelMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveAccelLabelMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveAccelLabelMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveAccelLabelMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveAccelLabelMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveAccelLabelMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveAccelLabelMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveAccelLabelMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveAccelLabelMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveAccelLabelMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveAccelLabelMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveAccelLabelMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveAccelLabelMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveAccelLabelMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveAccelLabelMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveAccelLabelMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveAccelLabelMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveAccelLabelMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveAccelLabelMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveAccelLabelMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveAccelLabelMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveAccelLabelMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveAccelLabelMethod "refetch" o = AccelLabelRefetchMethodInfo
    ResolveAccelLabelMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveAccelLabelMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveAccelLabelMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveAccelLabelMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveAccelLabelMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveAccelLabelMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveAccelLabelMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveAccelLabelMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveAccelLabelMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveAccelLabelMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveAccelLabelMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveAccelLabelMethod "selectRegion" o = Gtk.Label.LabelSelectRegionMethodInfo
    ResolveAccelLabelMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveAccelLabelMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveAccelLabelMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveAccelLabelMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveAccelLabelMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveAccelLabelMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveAccelLabelMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveAccelLabelMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveAccelLabelMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveAccelLabelMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveAccelLabelMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveAccelLabelMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveAccelLabelMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveAccelLabelMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveAccelLabelMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveAccelLabelMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveAccelLabelMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveAccelLabelMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveAccelLabelMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveAccelLabelMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveAccelLabelMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveAccelLabelMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveAccelLabelMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveAccelLabelMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveAccelLabelMethod "getAccel" o = AccelLabelGetAccelMethodInfo
    ResolveAccelLabelMethod "getAccelWidget" o = AccelLabelGetAccelWidgetMethodInfo
    ResolveAccelLabelMethod "getAccelWidth" o = AccelLabelGetAccelWidthMethodInfo
    ResolveAccelLabelMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveAccelLabelMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveAccelLabelMethod "getAlignment" o = Gtk.Misc.MiscGetAlignmentMethodInfo
    ResolveAccelLabelMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveAccelLabelMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveAccelLabelMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveAccelLabelMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveAccelLabelMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveAccelLabelMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveAccelLabelMethod "getAngle" o = Gtk.Label.LabelGetAngleMethodInfo
    ResolveAccelLabelMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveAccelLabelMethod "getAttributes" o = Gtk.Label.LabelGetAttributesMethodInfo
    ResolveAccelLabelMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveAccelLabelMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveAccelLabelMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveAccelLabelMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveAccelLabelMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveAccelLabelMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveAccelLabelMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveAccelLabelMethod "getCurrentUri" o = Gtk.Label.LabelGetCurrentUriMethodInfo
    ResolveAccelLabelMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveAccelLabelMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveAccelLabelMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveAccelLabelMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveAccelLabelMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveAccelLabelMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveAccelLabelMethod "getEllipsize" o = Gtk.Label.LabelGetEllipsizeMethodInfo
    ResolveAccelLabelMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveAccelLabelMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveAccelLabelMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveAccelLabelMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveAccelLabelMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveAccelLabelMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveAccelLabelMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveAccelLabelMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveAccelLabelMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveAccelLabelMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveAccelLabelMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveAccelLabelMethod "getJustify" o = Gtk.Label.LabelGetJustifyMethodInfo
    ResolveAccelLabelMethod "getLabel" o = Gtk.Label.LabelGetLabelMethodInfo
    ResolveAccelLabelMethod "getLayout" o = Gtk.Label.LabelGetLayoutMethodInfo
    ResolveAccelLabelMethod "getLayoutOffsets" o = Gtk.Label.LabelGetLayoutOffsetsMethodInfo
    ResolveAccelLabelMethod "getLineWrap" o = Gtk.Label.LabelGetLineWrapMethodInfo
    ResolveAccelLabelMethod "getLineWrapMode" o = Gtk.Label.LabelGetLineWrapModeMethodInfo
    ResolveAccelLabelMethod "getLines" o = Gtk.Label.LabelGetLinesMethodInfo
    ResolveAccelLabelMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveAccelLabelMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveAccelLabelMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveAccelLabelMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveAccelLabelMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveAccelLabelMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveAccelLabelMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveAccelLabelMethod "getMaxWidthChars" o = Gtk.Label.LabelGetMaxWidthCharsMethodInfo
    ResolveAccelLabelMethod "getMnemonicKeyval" o = Gtk.Label.LabelGetMnemonicKeyvalMethodInfo
    ResolveAccelLabelMethod "getMnemonicWidget" o = Gtk.Label.LabelGetMnemonicWidgetMethodInfo
    ResolveAccelLabelMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveAccelLabelMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveAccelLabelMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveAccelLabelMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveAccelLabelMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveAccelLabelMethod "getPadding" o = Gtk.Misc.MiscGetPaddingMethodInfo
    ResolveAccelLabelMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveAccelLabelMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveAccelLabelMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveAccelLabelMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveAccelLabelMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveAccelLabelMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveAccelLabelMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveAccelLabelMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveAccelLabelMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveAccelLabelMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveAccelLabelMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveAccelLabelMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveAccelLabelMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveAccelLabelMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveAccelLabelMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveAccelLabelMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveAccelLabelMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveAccelLabelMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveAccelLabelMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveAccelLabelMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveAccelLabelMethod "getSelectable" o = Gtk.Label.LabelGetSelectableMethodInfo
    ResolveAccelLabelMethod "getSelectionBounds" o = Gtk.Label.LabelGetSelectionBoundsMethodInfo
    ResolveAccelLabelMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveAccelLabelMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveAccelLabelMethod "getSingleLineMode" o = Gtk.Label.LabelGetSingleLineModeMethodInfo
    ResolveAccelLabelMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveAccelLabelMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveAccelLabelMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveAccelLabelMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveAccelLabelMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveAccelLabelMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveAccelLabelMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveAccelLabelMethod "getText" o = Gtk.Label.LabelGetTextMethodInfo
    ResolveAccelLabelMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveAccelLabelMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveAccelLabelMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveAccelLabelMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveAccelLabelMethod "getTrackVisitedLinks" o = Gtk.Label.LabelGetTrackVisitedLinksMethodInfo
    ResolveAccelLabelMethod "getUseMarkup" o = Gtk.Label.LabelGetUseMarkupMethodInfo
    ResolveAccelLabelMethod "getUseUnderline" o = Gtk.Label.LabelGetUseUnderlineMethodInfo
    ResolveAccelLabelMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveAccelLabelMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveAccelLabelMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveAccelLabelMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveAccelLabelMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveAccelLabelMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveAccelLabelMethod "getWidthChars" o = Gtk.Label.LabelGetWidthCharsMethodInfo
    ResolveAccelLabelMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveAccelLabelMethod "getXalign" o = Gtk.Label.LabelGetXalignMethodInfo
    ResolveAccelLabelMethod "getYalign" o = Gtk.Label.LabelGetYalignMethodInfo
    ResolveAccelLabelMethod "setAccel" o = AccelLabelSetAccelMethodInfo
    ResolveAccelLabelMethod "setAccelClosure" o = AccelLabelSetAccelClosureMethodInfo
    ResolveAccelLabelMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveAccelLabelMethod "setAccelWidget" o = AccelLabelSetAccelWidgetMethodInfo
    ResolveAccelLabelMethod "setAlignment" o = Gtk.Misc.MiscSetAlignmentMethodInfo
    ResolveAccelLabelMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveAccelLabelMethod "setAngle" o = Gtk.Label.LabelSetAngleMethodInfo
    ResolveAccelLabelMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveAccelLabelMethod "setAttributes" o = Gtk.Label.LabelSetAttributesMethodInfo
    ResolveAccelLabelMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveAccelLabelMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveAccelLabelMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveAccelLabelMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveAccelLabelMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveAccelLabelMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveAccelLabelMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveAccelLabelMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveAccelLabelMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveAccelLabelMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveAccelLabelMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveAccelLabelMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveAccelLabelMethod "setEllipsize" o = Gtk.Label.LabelSetEllipsizeMethodInfo
    ResolveAccelLabelMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveAccelLabelMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveAccelLabelMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveAccelLabelMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveAccelLabelMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveAccelLabelMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveAccelLabelMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveAccelLabelMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveAccelLabelMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveAccelLabelMethod "setJustify" o = Gtk.Label.LabelSetJustifyMethodInfo
    ResolveAccelLabelMethod "setLabel" o = Gtk.Label.LabelSetLabelMethodInfo
    ResolveAccelLabelMethod "setLineWrap" o = Gtk.Label.LabelSetLineWrapMethodInfo
    ResolveAccelLabelMethod "setLineWrapMode" o = Gtk.Label.LabelSetLineWrapModeMethodInfo
    ResolveAccelLabelMethod "setLines" o = Gtk.Label.LabelSetLinesMethodInfo
    ResolveAccelLabelMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveAccelLabelMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveAccelLabelMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveAccelLabelMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveAccelLabelMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveAccelLabelMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveAccelLabelMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveAccelLabelMethod "setMarkup" o = Gtk.Label.LabelSetMarkupMethodInfo
    ResolveAccelLabelMethod "setMarkupWithMnemonic" o = Gtk.Label.LabelSetMarkupWithMnemonicMethodInfo
    ResolveAccelLabelMethod "setMaxWidthChars" o = Gtk.Label.LabelSetMaxWidthCharsMethodInfo
    ResolveAccelLabelMethod "setMnemonicWidget" o = Gtk.Label.LabelSetMnemonicWidgetMethodInfo
    ResolveAccelLabelMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveAccelLabelMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveAccelLabelMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveAccelLabelMethod "setPadding" o = Gtk.Misc.MiscSetPaddingMethodInfo
    ResolveAccelLabelMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveAccelLabelMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveAccelLabelMethod "setPattern" o = Gtk.Label.LabelSetPatternMethodInfo
    ResolveAccelLabelMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveAccelLabelMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveAccelLabelMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveAccelLabelMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveAccelLabelMethod "setSelectable" o = Gtk.Label.LabelSetSelectableMethodInfo
    ResolveAccelLabelMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveAccelLabelMethod "setSingleLineMode" o = Gtk.Label.LabelSetSingleLineModeMethodInfo
    ResolveAccelLabelMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveAccelLabelMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveAccelLabelMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveAccelLabelMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveAccelLabelMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveAccelLabelMethod "setText" o = Gtk.Label.LabelSetTextMethodInfo
    ResolveAccelLabelMethod "setTextWithMnemonic" o = Gtk.Label.LabelSetTextWithMnemonicMethodInfo
    ResolveAccelLabelMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveAccelLabelMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveAccelLabelMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveAccelLabelMethod "setTrackVisitedLinks" o = Gtk.Label.LabelSetTrackVisitedLinksMethodInfo
    ResolveAccelLabelMethod "setUseMarkup" o = Gtk.Label.LabelSetUseMarkupMethodInfo
    ResolveAccelLabelMethod "setUseUnderline" o = Gtk.Label.LabelSetUseUnderlineMethodInfo
    ResolveAccelLabelMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveAccelLabelMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveAccelLabelMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveAccelLabelMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveAccelLabelMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveAccelLabelMethod "setWidthChars" o = Gtk.Label.LabelSetWidthCharsMethodInfo
    ResolveAccelLabelMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveAccelLabelMethod "setXalign" o = Gtk.Label.LabelSetXalignMethodInfo
    ResolveAccelLabelMethod "setYalign" o = Gtk.Label.LabelSetYalignMethodInfo
    ResolveAccelLabelMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveAccelLabelMethod t AccelLabel, O.OverloadedMethod info AccelLabel p) => OL.IsLabel t (AccelLabel -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveAccelLabelMethod t AccelLabel, O.OverloadedMethod info AccelLabel p, R.HasField t AccelLabel p) => R.HasField t AccelLabel p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveAccelLabelMethod t AccelLabel, O.OverloadedMethodInfo info AccelLabel) => OL.IsLabel t (O.MethodProxy info AccelLabel) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

--- XXX Duplicated object with different types:
  --- Name {namespace = "Gtk", name = "Label"} -> Property {propName = "xalign", propType = TBasicType TFloat, propFlags = [PropertyReadable,PropertyWritable], propReadNullable = Just False, propWriteNullable = Just False, propTransfer = TransferNothing, propDoc = Documentation {rawDocText = Just "The xalign property determines the horizontal aligment of the label text\ninside the labels size allocation. Compare this to #GtkWidget:halign,\nwhich determines how the labels size allocation is positioned in the\nspace available for the label.", sinceVersion = Just "3.16"}, propDeprecated = Nothing}
  --- Name {namespace = "Gtk", name = "Misc"} -> Property {propName = "xalign", propType = TBasicType TFloat, propFlags = [PropertyReadable,PropertyWritable], propReadNullable = Nothing, propWriteNullable = Nothing, propTransfer = TransferNothing, propDoc = Documentation {rawDocText = Just "The horizontal alignment. A value of 0.0 means left alignment (or right\non RTL locales); a value of 1.0 means right alignment (or left on RTL\nlocales).", sinceVersion = Nothing}, propDeprecated = Just (DeprecationInfo {deprecatedSinceVersion = Just "3.14", deprecationMessage = Just "Use gtk_widget_set_halign() instead. If you are using\n  #GtkLabel, use #GtkLabel:xalign instead."})}
--- XXX Duplicated object with different types:
  --- Name {namespace = "Gtk", name = "Label"} -> Property {propName = "yalign", propType = TBasicType TFloat, propFlags = [PropertyReadable,PropertyWritable], propReadNullable = Just False, propWriteNullable = Just False, propTransfer = TransferNothing, propDoc = Documentation {rawDocText = Just "The yalign property determines the vertical aligment of the label text\ninside the labels size allocation. Compare this to #GtkWidget:valign,\nwhich determines how the labels size allocation is positioned in the\nspace available for the label.", sinceVersion = Just "3.16"}, propDeprecated = Nothing}
  --- Name {namespace = "Gtk", name = "Misc"} -> Property {propName = "yalign", propType = TBasicType TFloat, propFlags = [PropertyReadable,PropertyWritable], propReadNullable = Nothing, propWriteNullable = Nothing, propTransfer = TransferNothing, propDoc = Documentation {rawDocText = Just "The vertical alignment. A value of 0.0 means top alignment;\na value of 1.0 means bottom alignment.", sinceVersion = Nothing}, propDeprecated = Just (DeprecationInfo {deprecatedSinceVersion = Just "3.14", deprecationMessage = Just "Use gtk_widget_set_valign() instead. If you are using\n  #GtkLabel, use #GtkLabel:yalign instead."})}
-- VVV Prop "accel-closure"
   -- Type: TGClosure Nothing
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Just True)

-- | Get the value of the “@accel-closure@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' accelLabel #accelClosure
-- @
getAccelLabelAccelClosure :: (MonadIO m, IsAccelLabel o) => o -> m (Maybe (GClosure ()))
getAccelLabelAccelClosure obj = MIO.liftIO $ B.Properties.getObjectPropertyClosure obj "accel-closure"

-- | Set the value of the “@accel-closure@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' accelLabel [ #accelClosure 'Data.GI.Base.Attributes.:=' value ]
-- @
setAccelLabelAccelClosure :: (MonadIO m, IsAccelLabel o) => o -> GClosure a -> m ()
setAccelLabelAccelClosure obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyClosure obj "accel-closure" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@accel-closure@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructAccelLabelAccelClosure :: (IsAccelLabel o, MIO.MonadIO m) => GClosure a -> m (GValueConstruct o)
constructAccelLabelAccelClosure val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyClosure "accel-closure" (P.Just val)

-- | Set the value of the “@accel-closure@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #accelClosure
-- @
clearAccelLabelAccelClosure :: (MonadIO m, IsAccelLabel o) => o -> m ()
clearAccelLabelAccelClosure obj = liftIO $ B.Properties.setObjectPropertyClosure obj "accel-closure" (Nothing :: Maybe (GClosure a))

#if defined(ENABLE_OVERLOADING)
data AccelLabelAccelClosurePropertyInfo
instance AttrInfo AccelLabelAccelClosurePropertyInfo where
    type AttrAllowedOps AccelLabelAccelClosurePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint AccelLabelAccelClosurePropertyInfo = IsAccelLabel
    type AttrSetTypeConstraint AccelLabelAccelClosurePropertyInfo = (~) (GClosure ())
    type AttrTransferTypeConstraint AccelLabelAccelClosurePropertyInfo = (~) (GClosure ())
    type AttrTransferType AccelLabelAccelClosurePropertyInfo = GClosure ()
    type AttrGetType AccelLabelAccelClosurePropertyInfo = (Maybe (GClosure ()))
    type AttrLabel AccelLabelAccelClosurePropertyInfo = "accel-closure"
    type AttrOrigin AccelLabelAccelClosurePropertyInfo = AccelLabel
    attrGet = getAccelLabelAccelClosure
    attrSet = setAccelLabelAccelClosure
    attrTransfer _ v = do
        return v
    attrConstruct = constructAccelLabelAccelClosure
    attrClear = clearAccelLabelAccelClosure
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AccelLabel.accelClosure"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AccelLabel.html#g:attr:accelClosure"
        })
#endif

-- VVV Prop "accel-widget"
   -- Type: TInterface (Name {namespace = "Gtk", name = "Widget"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just True)

-- | Get the value of the “@accel-widget@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' accelLabel #accelWidget
-- @
getAccelLabelAccelWidget :: (MonadIO m, IsAccelLabel o) => o -> m (Maybe Gtk.Widget.Widget)
getAccelLabelAccelWidget obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "accel-widget" Gtk.Widget.Widget

-- | Set the value of the “@accel-widget@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' accelLabel [ #accelWidget 'Data.GI.Base.Attributes.:=' value ]
-- @
setAccelLabelAccelWidget :: (MonadIO m, IsAccelLabel o, Gtk.Widget.IsWidget a) => o -> a -> m ()
setAccelLabelAccelWidget obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "accel-widget" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@accel-widget@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructAccelLabelAccelWidget :: (IsAccelLabel o, MIO.MonadIO m, Gtk.Widget.IsWidget a) => a -> m (GValueConstruct o)
constructAccelLabelAccelWidget val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "accel-widget" (P.Just val)

-- | Set the value of the “@accel-widget@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #accelWidget
-- @
clearAccelLabelAccelWidget :: (MonadIO m, IsAccelLabel o) => o -> m ()
clearAccelLabelAccelWidget obj = liftIO $ B.Properties.setObjectPropertyObject obj "accel-widget" (Nothing :: Maybe Gtk.Widget.Widget)

#if defined(ENABLE_OVERLOADING)
data AccelLabelAccelWidgetPropertyInfo
instance AttrInfo AccelLabelAccelWidgetPropertyInfo where
    type AttrAllowedOps AccelLabelAccelWidgetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint AccelLabelAccelWidgetPropertyInfo = IsAccelLabel
    type AttrSetTypeConstraint AccelLabelAccelWidgetPropertyInfo = Gtk.Widget.IsWidget
    type AttrTransferTypeConstraint AccelLabelAccelWidgetPropertyInfo = Gtk.Widget.IsWidget
    type AttrTransferType AccelLabelAccelWidgetPropertyInfo = Gtk.Widget.Widget
    type AttrGetType AccelLabelAccelWidgetPropertyInfo = (Maybe Gtk.Widget.Widget)
    type AttrLabel AccelLabelAccelWidgetPropertyInfo = "accel-widget"
    type AttrOrigin AccelLabelAccelWidgetPropertyInfo = AccelLabel
    attrGet = getAccelLabelAccelWidget
    attrSet = setAccelLabelAccelWidget
    attrTransfer _ v = do
        unsafeCastTo Gtk.Widget.Widget v
    attrConstruct = constructAccelLabelAccelWidget
    attrClear = clearAccelLabelAccelWidget
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AccelLabel.accelWidget"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AccelLabel.html#g:attr:accelWidget"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList AccelLabel
type instance O.AttributeList AccelLabel = AccelLabelAttributeList
type AccelLabelAttributeList = ('[ '("accelClosure", AccelLabelAccelClosurePropertyInfo), '("accelWidget", AccelLabelAccelWidgetPropertyInfo), '("angle", Gtk.Label.LabelAnglePropertyInfo), '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("attributes", Gtk.Label.LabelAttributesPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("cursorPosition", Gtk.Label.LabelCursorPositionPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("ellipsize", Gtk.Label.LabelEllipsizePropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("justify", Gtk.Label.LabelJustifyPropertyInfo), '("label", Gtk.Label.LabelLabelPropertyInfo), '("lines", Gtk.Label.LabelLinesPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("maxWidthChars", Gtk.Label.LabelMaxWidthCharsPropertyInfo), '("mnemonicKeyval", Gtk.Label.LabelMnemonicKeyvalPropertyInfo), '("mnemonicWidget", Gtk.Label.LabelMnemonicWidgetPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("pattern", Gtk.Label.LabelPatternPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("selectable", Gtk.Label.LabelSelectablePropertyInfo), '("selectionBound", Gtk.Label.LabelSelectionBoundPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("singleLineMode", Gtk.Label.LabelSingleLineModePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("trackVisitedLinks", Gtk.Label.LabelTrackVisitedLinksPropertyInfo), '("useMarkup", Gtk.Label.LabelUseMarkupPropertyInfo), '("useUnderline", Gtk.Label.LabelUseUnderlinePropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthChars", Gtk.Label.LabelWidthCharsPropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo), '("wrap", Gtk.Label.LabelWrapPropertyInfo), '("wrapMode", Gtk.Label.LabelWrapModePropertyInfo), '("xalign", Gtk.Label.LabelXalignPropertyInfo), '("xpad", Gtk.Misc.MiscXpadPropertyInfo), '("yalign", Gtk.Label.LabelYalignPropertyInfo), '("ypad", Gtk.Misc.MiscYpadPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
accelLabelAccelClosure :: AttrLabelProxy "accelClosure"
accelLabelAccelClosure = AttrLabelProxy

accelLabelAccelWidget :: AttrLabelProxy "accelWidget"
accelLabelAccelWidget = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList AccelLabel = AccelLabelSignalList
type AccelLabelSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("activateCurrentLink", Gtk.Label.LabelActivateCurrentLinkSignalInfo), '("activateLink", Gtk.Label.LabelActivateLinkSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("copyClipboard", Gtk.Label.LabelCopyClipboardSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveCursor", Gtk.Label.LabelMoveCursorSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("populatePopup", Gtk.Label.LabelPopulatePopupSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method AccelLabel::new
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "string"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the label string. Must be non-%NULL."
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "AccelLabel" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_accel_label_new" gtk_accel_label_new :: 
    CString ->                              -- string : TBasicType TUTF8
    IO (Ptr AccelLabel)

-- | Creates a new t'GI.Gtk.Objects.AccelLabel.AccelLabel'.
accelLabelNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@string@/: the label string. Must be non-'P.Nothing'.
    -> m AccelLabel
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.AccelLabel.AccelLabel'.
accelLabelNew string = liftIO $ do
    string' <- textToCString string
    result <- gtk_accel_label_new string'
    checkUnexpectedReturnNULL "accelLabelNew" result
    result' <- (newObject AccelLabel) result
    freeMem string'
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method AccelLabel::get_accel
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "accel_label"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AccelLabel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAccelLabel" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "accelerator_key"
--           , argType = TBasicType TUInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return location for the keyval"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "accelerator_mods"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "ModifierType" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return location for the modifier mask"
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

foreign import ccall "gtk_accel_label_get_accel" gtk_accel_label_get_accel :: 
    Ptr AccelLabel ->                       -- accel_label : TInterface (Name {namespace = "Gtk", name = "AccelLabel"})
    Ptr Word32 ->                           -- accelerator_key : TBasicType TUInt
    Ptr CUInt ->                            -- accelerator_mods : TInterface (Name {namespace = "Gdk", name = "ModifierType"})
    IO ()

-- | Gets the keyval and modifier mask set with
-- 'GI.Gtk.Objects.AccelLabel.accelLabelSetAccel'.
-- 
-- /Since: 3.12/
accelLabelGetAccel ::
    (B.CallStack.HasCallStack, MonadIO m, IsAccelLabel a) =>
    a
    -- ^ /@accelLabel@/: a t'GI.Gtk.Objects.AccelLabel.AccelLabel'
    -> m ((Word32, [Gdk.Flags.ModifierType]))
accelLabelGetAccel accelLabel = liftIO $ do
    accelLabel' <- unsafeManagedPtrCastPtr accelLabel
    acceleratorKey <- allocMem :: IO (Ptr Word32)
    acceleratorMods <- allocMem :: IO (Ptr CUInt)
    gtk_accel_label_get_accel accelLabel' acceleratorKey acceleratorMods
    acceleratorKey' <- peek acceleratorKey
    acceleratorMods' <- peek acceleratorMods
    let acceleratorMods'' = wordToGFlags acceleratorMods'
    touchManagedPtr accelLabel
    freeMem acceleratorKey
    freeMem acceleratorMods
    return (acceleratorKey', acceleratorMods'')

#if defined(ENABLE_OVERLOADING)
data AccelLabelGetAccelMethodInfo
instance (signature ~ (m ((Word32, [Gdk.Flags.ModifierType]))), MonadIO m, IsAccelLabel a) => O.OverloadedMethod AccelLabelGetAccelMethodInfo a signature where
    overloadedMethod = accelLabelGetAccel

instance O.OverloadedMethodInfo AccelLabelGetAccelMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AccelLabel.accelLabelGetAccel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AccelLabel.html#v:accelLabelGetAccel"
        })


#endif

-- method AccelLabel::get_accel_widget
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "accel_label"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AccelLabel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAccelLabel" , sinceVersion = Nothing }
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

foreign import ccall "gtk_accel_label_get_accel_widget" gtk_accel_label_get_accel_widget :: 
    Ptr AccelLabel ->                       -- accel_label : TInterface (Name {namespace = "Gtk", name = "AccelLabel"})
    IO (Ptr Gtk.Widget.Widget)

-- | Fetches the widget monitored by this accelerator label. See
-- 'GI.Gtk.Objects.AccelLabel.accelLabelSetAccelWidget'.
accelLabelGetAccelWidget ::
    (B.CallStack.HasCallStack, MonadIO m, IsAccelLabel a) =>
    a
    -- ^ /@accelLabel@/: a t'GI.Gtk.Objects.AccelLabel.AccelLabel'
    -> m (Maybe Gtk.Widget.Widget)
    -- ^ __Returns:__ the object monitored by the accelerator label, or 'P.Nothing'.
accelLabelGetAccelWidget accelLabel = liftIO $ do
    accelLabel' <- unsafeManagedPtrCastPtr accelLabel
    result <- gtk_accel_label_get_accel_widget accelLabel'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Widget.Widget) result'
        return result''
    touchManagedPtr accelLabel
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data AccelLabelGetAccelWidgetMethodInfo
instance (signature ~ (m (Maybe Gtk.Widget.Widget)), MonadIO m, IsAccelLabel a) => O.OverloadedMethod AccelLabelGetAccelWidgetMethodInfo a signature where
    overloadedMethod = accelLabelGetAccelWidget

instance O.OverloadedMethodInfo AccelLabelGetAccelWidgetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AccelLabel.accelLabelGetAccelWidget",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AccelLabel.html#v:accelLabelGetAccelWidget"
        })


#endif

-- method AccelLabel::get_accel_width
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "accel_label"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AccelLabel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAccelLabel." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TUInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_accel_label_get_accel_width" gtk_accel_label_get_accel_width :: 
    Ptr AccelLabel ->                       -- accel_label : TInterface (Name {namespace = "Gtk", name = "AccelLabel"})
    IO Word32

-- | Returns the width needed to display the accelerator key(s).
-- This is used by menus to align all of the t'GI.Gtk.Objects.MenuItem.MenuItem' widgets, and shouldn\'t
-- be needed by applications.
accelLabelGetAccelWidth ::
    (B.CallStack.HasCallStack, MonadIO m, IsAccelLabel a) =>
    a
    -- ^ /@accelLabel@/: a t'GI.Gtk.Objects.AccelLabel.AccelLabel'.
    -> m Word32
    -- ^ __Returns:__ the width needed to display the accelerator key(s).
accelLabelGetAccelWidth accelLabel = liftIO $ do
    accelLabel' <- unsafeManagedPtrCastPtr accelLabel
    result <- gtk_accel_label_get_accel_width accelLabel'
    touchManagedPtr accelLabel
    return result

#if defined(ENABLE_OVERLOADING)
data AccelLabelGetAccelWidthMethodInfo
instance (signature ~ (m Word32), MonadIO m, IsAccelLabel a) => O.OverloadedMethod AccelLabelGetAccelWidthMethodInfo a signature where
    overloadedMethod = accelLabelGetAccelWidth

instance O.OverloadedMethodInfo AccelLabelGetAccelWidthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AccelLabel.accelLabelGetAccelWidth",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AccelLabel.html#v:accelLabelGetAccelWidth"
        })


#endif

-- method AccelLabel::refetch
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "accel_label"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AccelLabel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAccelLabel." , sinceVersion = Nothing }
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

foreign import ccall "gtk_accel_label_refetch" gtk_accel_label_refetch :: 
    Ptr AccelLabel ->                       -- accel_label : TInterface (Name {namespace = "Gtk", name = "AccelLabel"})
    IO CInt

-- | Recreates the string representing the accelerator keys.
-- This should not be needed since the string is automatically updated whenever
-- accelerators are added or removed from the associated widget.
accelLabelRefetch ::
    (B.CallStack.HasCallStack, MonadIO m, IsAccelLabel a) =>
    a
    -- ^ /@accelLabel@/: a t'GI.Gtk.Objects.AccelLabel.AccelLabel'.
    -> m Bool
    -- ^ __Returns:__ always returns 'P.False'.
accelLabelRefetch accelLabel = liftIO $ do
    accelLabel' <- unsafeManagedPtrCastPtr accelLabel
    result <- gtk_accel_label_refetch accelLabel'
    let result' = (/= 0) result
    touchManagedPtr accelLabel
    return result'

#if defined(ENABLE_OVERLOADING)
data AccelLabelRefetchMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsAccelLabel a) => O.OverloadedMethod AccelLabelRefetchMethodInfo a signature where
    overloadedMethod = accelLabelRefetch

instance O.OverloadedMethodInfo AccelLabelRefetchMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AccelLabel.accelLabelRefetch",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AccelLabel.html#v:accelLabelRefetch"
        })


#endif

-- method AccelLabel::set_accel
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "accel_label"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AccelLabel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAccelLabel" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "accelerator_key"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a keyval, or 0" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "accelerator_mods"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "ModifierType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the modifier mask for the accel"
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

foreign import ccall "gtk_accel_label_set_accel" gtk_accel_label_set_accel :: 
    Ptr AccelLabel ->                       -- accel_label : TInterface (Name {namespace = "Gtk", name = "AccelLabel"})
    Word32 ->                               -- accelerator_key : TBasicType TUInt
    CUInt ->                                -- accelerator_mods : TInterface (Name {namespace = "Gdk", name = "ModifierType"})
    IO ()

-- | Manually sets a keyval and modifier mask as the accelerator rendered
-- by /@accelLabel@/.
-- 
-- If a keyval and modifier are explicitly set then these values are
-- used regardless of any associated accel closure or widget.
-- 
-- Providing an /@acceleratorKey@/ of 0 removes the manual setting.
-- 
-- /Since: 3.6/
accelLabelSetAccel ::
    (B.CallStack.HasCallStack, MonadIO m, IsAccelLabel a) =>
    a
    -- ^ /@accelLabel@/: a t'GI.Gtk.Objects.AccelLabel.AccelLabel'
    -> Word32
    -- ^ /@acceleratorKey@/: a keyval, or 0
    -> [Gdk.Flags.ModifierType]
    -- ^ /@acceleratorMods@/: the modifier mask for the accel
    -> m ()
accelLabelSetAccel accelLabel acceleratorKey acceleratorMods = liftIO $ do
    accelLabel' <- unsafeManagedPtrCastPtr accelLabel
    let acceleratorMods' = gflagsToWord acceleratorMods
    gtk_accel_label_set_accel accelLabel' acceleratorKey acceleratorMods'
    touchManagedPtr accelLabel
    return ()

#if defined(ENABLE_OVERLOADING)
data AccelLabelSetAccelMethodInfo
instance (signature ~ (Word32 -> [Gdk.Flags.ModifierType] -> m ()), MonadIO m, IsAccelLabel a) => O.OverloadedMethod AccelLabelSetAccelMethodInfo a signature where
    overloadedMethod = accelLabelSetAccel

instance O.OverloadedMethodInfo AccelLabelSetAccelMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AccelLabel.accelLabelSetAccel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AccelLabel.html#v:accelLabelSetAccel"
        })


#endif

-- method AccelLabel::set_accel_closure
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "accel_label"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AccelLabel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAccelLabel" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "accel_closure"
--           , argType = TGClosure Nothing
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the closure to monitor for accelerator changes,\nor %NULL"
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

foreign import ccall "gtk_accel_label_set_accel_closure" gtk_accel_label_set_accel_closure :: 
    Ptr AccelLabel ->                       -- accel_label : TInterface (Name {namespace = "Gtk", name = "AccelLabel"})
    Ptr (GClosure ()) ->                    -- accel_closure : TGClosure Nothing
    IO ()

-- | Sets the closure to be monitored by this accelerator label. The closure
-- must be connected to an accelerator group; see 'GI.Gtk.Objects.AccelGroup.accelGroupConnect'.
-- Passing 'P.Nothing' for /@accelClosure@/ will dissociate /@accelLabel@/ from its
-- current closure, if any.
accelLabelSetAccelClosure ::
    (B.CallStack.HasCallStack, MonadIO m, IsAccelLabel a) =>
    a
    -- ^ /@accelLabel@/: a t'GI.Gtk.Objects.AccelLabel.AccelLabel'
    -> Maybe (GClosure b)
    -- ^ /@accelClosure@/: the closure to monitor for accelerator changes,
    -- or 'P.Nothing'
    -> m ()
accelLabelSetAccelClosure accelLabel accelClosure = liftIO $ do
    accelLabel' <- unsafeManagedPtrCastPtr accelLabel
    maybeAccelClosure <- case accelClosure of
        Nothing -> return nullPtr
        Just jAccelClosure -> do
            jAccelClosure' <- unsafeManagedPtrCastPtr jAccelClosure
            return jAccelClosure'
    gtk_accel_label_set_accel_closure accelLabel' maybeAccelClosure
    touchManagedPtr accelLabel
    whenJust accelClosure touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data AccelLabelSetAccelClosureMethodInfo
instance (signature ~ (Maybe (GClosure b) -> m ()), MonadIO m, IsAccelLabel a) => O.OverloadedMethod AccelLabelSetAccelClosureMethodInfo a signature where
    overloadedMethod = accelLabelSetAccelClosure

instance O.OverloadedMethodInfo AccelLabelSetAccelClosureMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AccelLabel.accelLabelSetAccelClosure",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AccelLabel.html#v:accelLabelSetAccelClosure"
        })


#endif

-- method AccelLabel::set_accel_widget
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "accel_label"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AccelLabel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAccelLabel" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "accel_widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the widget to be monitored, or %NULL"
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

foreign import ccall "gtk_accel_label_set_accel_widget" gtk_accel_label_set_accel_widget :: 
    Ptr AccelLabel ->                       -- accel_label : TInterface (Name {namespace = "Gtk", name = "AccelLabel"})
    Ptr Gtk.Widget.Widget ->                -- accel_widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | Sets the widget to be monitored by this accelerator label. Passing 'P.Nothing' for
-- /@accelWidget@/ will dissociate /@accelLabel@/ from its current widget, if any.
accelLabelSetAccelWidget ::
    (B.CallStack.HasCallStack, MonadIO m, IsAccelLabel a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@accelLabel@/: a t'GI.Gtk.Objects.AccelLabel.AccelLabel'
    -> Maybe (b)
    -- ^ /@accelWidget@/: the widget to be monitored, or 'P.Nothing'
    -> m ()
accelLabelSetAccelWidget accelLabel accelWidget = liftIO $ do
    accelLabel' <- unsafeManagedPtrCastPtr accelLabel
    maybeAccelWidget <- case accelWidget of
        Nothing -> return nullPtr
        Just jAccelWidget -> do
            jAccelWidget' <- unsafeManagedPtrCastPtr jAccelWidget
            return jAccelWidget'
    gtk_accel_label_set_accel_widget accelLabel' maybeAccelWidget
    touchManagedPtr accelLabel
    whenJust accelWidget touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data AccelLabelSetAccelWidgetMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsAccelLabel a, Gtk.Widget.IsWidget b) => O.OverloadedMethod AccelLabelSetAccelWidgetMethodInfo a signature where
    overloadedMethod = accelLabelSetAccelWidget

instance O.OverloadedMethodInfo AccelLabelSetAccelWidgetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AccelLabel.accelLabelSetAccelWidget",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AccelLabel.html#v:accelLabelSetAccelWidget"
        })


#endif


