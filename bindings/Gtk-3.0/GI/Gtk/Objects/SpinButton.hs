{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A t'GI.Gtk.Objects.SpinButton.SpinButton' is an ideal way to allow the user to set the value of
-- some attribute. Rather than having to directly type a number into a
-- t'GI.Gtk.Objects.Entry.Entry', GtkSpinButton allows the user to click on one of two arrows
-- to increment or decrement the displayed value. A value can still be
-- typed in, with the bonus that it can be checked to ensure it is in a
-- given range.
-- 
-- The main properties of a GtkSpinButton are through an adjustment.
-- See the t'GI.Gtk.Objects.Adjustment.Adjustment' section for more details about an adjustment\'s
-- properties. Note that GtkSpinButton will by default make its entry
-- large enough to accomodate the lower and upper bounds of the adjustment,
-- which can lead to surprising results. Best practice is to set both
-- the [Entry:widthChars]("GI.Gtk.Objects.Entry#g:attr:widthChars") and [Entry:maxWidthChars]("GI.Gtk.Objects.Entry#g:attr:maxWidthChars") poperties
-- to the desired number of characters to display in the entry.
-- 
-- = CSS nodes
-- 
-- 
-- === /plain code/
-- >
-- >spinbutton.horizontal
-- >├── undershoot.left
-- >├── undershoot.right
-- >├── entry
-- >│   ╰── ...
-- >├── button.down
-- >╰── button.up
-- 
-- 
-- 
-- === /plain code/
-- >
-- >spinbutton.vertical
-- >├── undershoot.left
-- >├── undershoot.right
-- >├── button.up
-- >├── entry
-- >│   ╰── ...
-- >╰── button.down
-- 
-- 
-- GtkSpinButtons main CSS node has the name spinbutton. It creates subnodes
-- for the entry and the two buttons, with these names. The button nodes have
-- the style classes .up and .down. The GtkEntry subnodes (if present) are put
-- below the entry node. The orientation of the spin button is reflected in
-- the .vertical or .horizontal style class on the main node.
-- 
-- == Using a GtkSpinButton to get an integer
-- 
-- 
-- === /C code/
-- >
-- >// Provides a function to retrieve an integer value from a GtkSpinButton
-- >// and creates a spin button to model percentage values.
-- >
-- >gint
-- >grab_int_value (GtkSpinButton *button,
-- >                gpointer       user_data)
-- >{
-- >  return gtk_spin_button_get_value_as_int (button);
-- >}
-- >
-- >void
-- >create_integer_spin_button (void)
-- >{
-- >
-- >  GtkWidget *window, *button;
-- >  GtkAdjustment *adjustment;
-- >
-- >  adjustment = gtk_adjustment_new (50.0, 0.0, 100.0, 1.0, 5.0, 0.0);
-- >
-- >  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
-- >  gtk_container_set_border_width (GTK_CONTAINER (window), 5);
-- >
-- >  // creates the spinbutton, with no decimal places
-- >  button = gtk_spin_button_new (adjustment, 1.0, 0);
-- >  gtk_container_add (GTK_CONTAINER (window), button);
-- >
-- >  gtk_widget_show_all (window);
-- >}
-- 
-- 
-- == Using a GtkSpinButton to get a floating point value
-- 
-- 
-- === /C code/
-- >
-- >// Provides a function to retrieve a floating point value from a
-- >// GtkSpinButton, and creates a high precision spin button.
-- >
-- >gfloat
-- >grab_float_value (GtkSpinButton *button,
-- >                  gpointer       user_data)
-- >{
-- >  return gtk_spin_button_get_value (button);
-- >}
-- >
-- >void
-- >create_floating_spin_button (void)
-- >{
-- >  GtkWidget *window, *button;
-- >  GtkAdjustment *adjustment;
-- >
-- >  adjustment = gtk_adjustment_new (2.500, 0.0, 5.0, 0.001, 0.1, 0.0);
-- >
-- >  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
-- >  gtk_container_set_border_width (GTK_CONTAINER (window), 5);
-- >
-- >  // creates the spinbutton, with three decimal places
-- >  button = gtk_spin_button_new (adjustment, 0.001, 3);
-- >  gtk_container_add (GTK_CONTAINER (window), button);
-- >
-- >  gtk_widget_show_all (window);
-- >}
-- 

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.SpinButton
    ( 

-- * Exported types
    SpinButton(..)                          ,
    IsSpinButton                            ,
    toSpinButton                            ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childNotify]("GI.Gtk.Objects.Widget#g:method:childNotify"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [configure]("GI.Gtk.Objects.SpinButton#g:method:configure"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [copyClipboard]("GI.Gtk.Interfaces.Editable#g:method:copyClipboard"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [cutClipboard]("GI.Gtk.Interfaces.Editable#g:method:cutClipboard"), [deleteSelection]("GI.Gtk.Interfaces.Editable#g:method:deleteSelection"), [deleteText]("GI.Gtk.Interfaces.Editable#g:method:deleteText"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [editingDone]("GI.Gtk.Interfaces.CellEditable#g:method:editingDone"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabFocusWithoutSelecting]("GI.Gtk.Objects.Entry#g:method:grabFocusWithoutSelecting"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [imContextFilterKeypress]("GI.Gtk.Objects.Entry#g:method:imContextFilterKeypress"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [insertText]("GI.Gtk.Interfaces.Editable#g:method:insertText"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [layoutIndexToTextIndex]("GI.Gtk.Objects.Entry#g:method:layoutIndexToTextIndex"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [pasteClipboard]("GI.Gtk.Interfaces.Editable#g:method:pasteClipboard"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [progressPulse]("GI.Gtk.Objects.Entry#g:method:progressPulse"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [removeWidget]("GI.Gtk.Interfaces.CellEditable#g:method:removeWidget"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetImContext]("GI.Gtk.Objects.Entry#g:method:resetImContext"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [selectRegion]("GI.Gtk.Interfaces.Editable#g:method:selectRegion"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [spin]("GI.Gtk.Objects.SpinButton#g:method:spin"), [startEditing]("GI.Gtk.Interfaces.CellEditable#g:method:startEditing"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [textIndexToLayoutIndex]("GI.Gtk.Objects.Entry#g:method:textIndexToLayoutIndex"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetInvisibleChar]("GI.Gtk.Objects.Entry#g:method:unsetInvisibleChar"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [update]("GI.Gtk.Objects.SpinButton#g:method:update"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getActivatesDefault]("GI.Gtk.Objects.Entry#g:method:getActivatesDefault"), [getAdjustment]("GI.Gtk.Objects.SpinButton#g:method:getAdjustment"), [getAlignment]("GI.Gtk.Objects.Entry#g:method:getAlignment"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getAttributes]("GI.Gtk.Objects.Entry#g:method:getAttributes"), [getBuffer]("GI.Gtk.Objects.Entry#g:method:getBuffer"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChars]("GI.Gtk.Interfaces.Editable#g:method:getChars"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompletion]("GI.Gtk.Objects.Entry#g:method:getCompletion"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getCurrentIconDragSource]("GI.Gtk.Objects.Entry#g:method:getCurrentIconDragSource"), [getCursorHadjustment]("GI.Gtk.Objects.Entry#g:method:getCursorHadjustment"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDigits]("GI.Gtk.Objects.SpinButton#g:method:getDigits"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEditable]("GI.Gtk.Interfaces.Editable#g:method:getEditable"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasFrame]("GI.Gtk.Objects.Entry#g:method:getHasFrame"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getIconActivatable]("GI.Gtk.Objects.Entry#g:method:getIconActivatable"), [getIconArea]("GI.Gtk.Objects.Entry#g:method:getIconArea"), [getIconAtPos]("GI.Gtk.Objects.Entry#g:method:getIconAtPos"), [getIconGicon]("GI.Gtk.Objects.Entry#g:method:getIconGicon"), [getIconName]("GI.Gtk.Objects.Entry#g:method:getIconName"), [getIconPixbuf]("GI.Gtk.Objects.Entry#g:method:getIconPixbuf"), [getIconSensitive]("GI.Gtk.Objects.Entry#g:method:getIconSensitive"), [getIconStock]("GI.Gtk.Objects.Entry#g:method:getIconStock"), [getIconStorageType]("GI.Gtk.Objects.Entry#g:method:getIconStorageType"), [getIconTooltipMarkup]("GI.Gtk.Objects.Entry#g:method:getIconTooltipMarkup"), [getIconTooltipText]("GI.Gtk.Objects.Entry#g:method:getIconTooltipText"), [getIncrements]("GI.Gtk.Objects.SpinButton#g:method:getIncrements"), [getInnerBorder]("GI.Gtk.Objects.Entry#g:method:getInnerBorder"), [getInputHints]("GI.Gtk.Objects.Entry#g:method:getInputHints"), [getInputPurpose]("GI.Gtk.Objects.Entry#g:method:getInputPurpose"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getInvisibleChar]("GI.Gtk.Objects.Entry#g:method:getInvisibleChar"), [getLayout]("GI.Gtk.Objects.Entry#g:method:getLayout"), [getLayoutOffsets]("GI.Gtk.Objects.Entry#g:method:getLayoutOffsets"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getMaxLength]("GI.Gtk.Objects.Entry#g:method:getMaxLength"), [getMaxWidthChars]("GI.Gtk.Objects.Entry#g:method:getMaxWidthChars"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getNumeric]("GI.Gtk.Objects.SpinButton#g:method:getNumeric"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getOrientation]("GI.Gtk.Interfaces.Orientable#g:method:getOrientation"), [getOverwriteMode]("GI.Gtk.Objects.Entry#g:method:getOverwriteMode"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPlaceholderText]("GI.Gtk.Objects.Entry#g:method:getPlaceholderText"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPosition]("GI.Gtk.Interfaces.Editable#g:method:getPosition"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProgressFraction]("GI.Gtk.Objects.Entry#g:method:getProgressFraction"), [getProgressPulseStep]("GI.Gtk.Objects.Entry#g:method:getProgressPulseStep"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRange]("GI.Gtk.Objects.SpinButton#g:method:getRange"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSelectionBounds]("GI.Gtk.Interfaces.Editable#g:method:getSelectionBounds"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getSnapToTicks]("GI.Gtk.Objects.SpinButton#g:method:getSnapToTicks"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTabs]("GI.Gtk.Objects.Entry#g:method:getTabs"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getText]("GI.Gtk.Objects.Entry#g:method:getText"), [getTextArea]("GI.Gtk.Objects.Entry#g:method:getTextArea"), [getTextLength]("GI.Gtk.Objects.Entry#g:method:getTextLength"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getUpdatePolicy]("GI.Gtk.Objects.SpinButton#g:method:getUpdatePolicy"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getValue]("GI.Gtk.Objects.SpinButton#g:method:getValue"), [getValueAsInt]("GI.Gtk.Objects.SpinButton#g:method:getValueAsInt"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisibility]("GI.Gtk.Objects.Entry#g:method:getVisibility"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWidthChars]("GI.Gtk.Objects.Entry#g:method:getWidthChars"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow"), [getWrap]("GI.Gtk.Objects.SpinButton#g:method:getWrap").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setActivatesDefault]("GI.Gtk.Objects.Entry#g:method:setActivatesDefault"), [setAdjustment]("GI.Gtk.Objects.SpinButton#g:method:setAdjustment"), [setAlignment]("GI.Gtk.Objects.Entry#g:method:setAlignment"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setAttributes]("GI.Gtk.Objects.Entry#g:method:setAttributes"), [setBuffer]("GI.Gtk.Objects.Entry#g:method:setBuffer"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompletion]("GI.Gtk.Objects.Entry#g:method:setCompletion"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setCursorHadjustment]("GI.Gtk.Objects.Entry#g:method:setCursorHadjustment"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDigits]("GI.Gtk.Objects.SpinButton#g:method:setDigits"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEditable]("GI.Gtk.Interfaces.Editable#g:method:setEditable"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasFrame]("GI.Gtk.Objects.Entry#g:method:setHasFrame"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setIconActivatable]("GI.Gtk.Objects.Entry#g:method:setIconActivatable"), [setIconDragSource]("GI.Gtk.Objects.Entry#g:method:setIconDragSource"), [setIconFromGicon]("GI.Gtk.Objects.Entry#g:method:setIconFromGicon"), [setIconFromIconName]("GI.Gtk.Objects.Entry#g:method:setIconFromIconName"), [setIconFromPixbuf]("GI.Gtk.Objects.Entry#g:method:setIconFromPixbuf"), [setIconFromStock]("GI.Gtk.Objects.Entry#g:method:setIconFromStock"), [setIconSensitive]("GI.Gtk.Objects.Entry#g:method:setIconSensitive"), [setIconTooltipMarkup]("GI.Gtk.Objects.Entry#g:method:setIconTooltipMarkup"), [setIconTooltipText]("GI.Gtk.Objects.Entry#g:method:setIconTooltipText"), [setIncrements]("GI.Gtk.Objects.SpinButton#g:method:setIncrements"), [setInnerBorder]("GI.Gtk.Objects.Entry#g:method:setInnerBorder"), [setInputHints]("GI.Gtk.Objects.Entry#g:method:setInputHints"), [setInputPurpose]("GI.Gtk.Objects.Entry#g:method:setInputPurpose"), [setInvisibleChar]("GI.Gtk.Objects.Entry#g:method:setInvisibleChar"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setMaxLength]("GI.Gtk.Objects.Entry#g:method:setMaxLength"), [setMaxWidthChars]("GI.Gtk.Objects.Entry#g:method:setMaxWidthChars"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setNumeric]("GI.Gtk.Objects.SpinButton#g:method:setNumeric"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setOrientation]("GI.Gtk.Interfaces.Orientable#g:method:setOrientation"), [setOverwriteMode]("GI.Gtk.Objects.Entry#g:method:setOverwriteMode"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setPlaceholderText]("GI.Gtk.Objects.Entry#g:method:setPlaceholderText"), [setPosition]("GI.Gtk.Interfaces.Editable#g:method:setPosition"), [setProgressFraction]("GI.Gtk.Objects.Entry#g:method:setProgressFraction"), [setProgressPulseStep]("GI.Gtk.Objects.Entry#g:method:setProgressPulseStep"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRange]("GI.Gtk.Objects.SpinButton#g:method:setRange"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setSnapToTicks]("GI.Gtk.Objects.SpinButton#g:method:setSnapToTicks"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTabs]("GI.Gtk.Objects.Entry#g:method:setTabs"), [setText]("GI.Gtk.Objects.Entry#g:method:setText"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setUpdatePolicy]("GI.Gtk.Objects.SpinButton#g:method:setUpdatePolicy"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setValue]("GI.Gtk.Objects.SpinButton#g:method:setValue"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisibility]("GI.Gtk.Objects.Entry#g:method:setVisibility"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWidthChars]("GI.Gtk.Objects.Entry#g:method:setWidthChars"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow"), [setWrap]("GI.Gtk.Objects.SpinButton#g:method:setWrap").

#if defined(ENABLE_OVERLOADING)
    ResolveSpinButtonMethod                 ,
#endif

-- ** configure #method:configure#

#if defined(ENABLE_OVERLOADING)
    SpinButtonConfigureMethodInfo           ,
#endif
    spinButtonConfigure                     ,


-- ** getAdjustment #method:getAdjustment#

#if defined(ENABLE_OVERLOADING)
    SpinButtonGetAdjustmentMethodInfo       ,
#endif
    spinButtonGetAdjustment                 ,


-- ** getDigits #method:getDigits#

#if defined(ENABLE_OVERLOADING)
    SpinButtonGetDigitsMethodInfo           ,
#endif
    spinButtonGetDigits                     ,


-- ** getIncrements #method:getIncrements#

#if defined(ENABLE_OVERLOADING)
    SpinButtonGetIncrementsMethodInfo       ,
#endif
    spinButtonGetIncrements                 ,


-- ** getNumeric #method:getNumeric#

#if defined(ENABLE_OVERLOADING)
    SpinButtonGetNumericMethodInfo          ,
#endif
    spinButtonGetNumeric                    ,


-- ** getRange #method:getRange#

#if defined(ENABLE_OVERLOADING)
    SpinButtonGetRangeMethodInfo            ,
#endif
    spinButtonGetRange                      ,


-- ** getSnapToTicks #method:getSnapToTicks#

#if defined(ENABLE_OVERLOADING)
    SpinButtonGetSnapToTicksMethodInfo      ,
#endif
    spinButtonGetSnapToTicks                ,


-- ** getUpdatePolicy #method:getUpdatePolicy#

#if defined(ENABLE_OVERLOADING)
    SpinButtonGetUpdatePolicyMethodInfo     ,
#endif
    spinButtonGetUpdatePolicy               ,


-- ** getValue #method:getValue#

#if defined(ENABLE_OVERLOADING)
    SpinButtonGetValueMethodInfo            ,
#endif
    spinButtonGetValue                      ,


-- ** getValueAsInt #method:getValueAsInt#

#if defined(ENABLE_OVERLOADING)
    SpinButtonGetValueAsIntMethodInfo       ,
#endif
    spinButtonGetValueAsInt                 ,


-- ** getWrap #method:getWrap#

#if defined(ENABLE_OVERLOADING)
    SpinButtonGetWrapMethodInfo             ,
#endif
    spinButtonGetWrap                       ,


-- ** new #method:new#

    spinButtonNew                           ,


-- ** newWithRange #method:newWithRange#

    spinButtonNewWithRange                  ,


-- ** setAdjustment #method:setAdjustment#

#if defined(ENABLE_OVERLOADING)
    SpinButtonSetAdjustmentMethodInfo       ,
#endif
    spinButtonSetAdjustment                 ,


-- ** setDigits #method:setDigits#

#if defined(ENABLE_OVERLOADING)
    SpinButtonSetDigitsMethodInfo           ,
#endif
    spinButtonSetDigits                     ,


-- ** setIncrements #method:setIncrements#

#if defined(ENABLE_OVERLOADING)
    SpinButtonSetIncrementsMethodInfo       ,
#endif
    spinButtonSetIncrements                 ,


-- ** setNumeric #method:setNumeric#

#if defined(ENABLE_OVERLOADING)
    SpinButtonSetNumericMethodInfo          ,
#endif
    spinButtonSetNumeric                    ,


-- ** setRange #method:setRange#

#if defined(ENABLE_OVERLOADING)
    SpinButtonSetRangeMethodInfo            ,
#endif
    spinButtonSetRange                      ,


-- ** setSnapToTicks #method:setSnapToTicks#

#if defined(ENABLE_OVERLOADING)
    SpinButtonSetSnapToTicksMethodInfo      ,
#endif
    spinButtonSetSnapToTicks                ,


-- ** setUpdatePolicy #method:setUpdatePolicy#

#if defined(ENABLE_OVERLOADING)
    SpinButtonSetUpdatePolicyMethodInfo     ,
#endif
    spinButtonSetUpdatePolicy               ,


-- ** setValue #method:setValue#

#if defined(ENABLE_OVERLOADING)
    SpinButtonSetValueMethodInfo            ,
#endif
    spinButtonSetValue                      ,


-- ** setWrap #method:setWrap#

#if defined(ENABLE_OVERLOADING)
    SpinButtonSetWrapMethodInfo             ,
#endif
    spinButtonSetWrap                       ,


-- ** spin #method:spin#

#if defined(ENABLE_OVERLOADING)
    SpinButtonSpinMethodInfo                ,
#endif
    spinButtonSpin                          ,


-- ** update #method:update#

#if defined(ENABLE_OVERLOADING)
    SpinButtonUpdateMethodInfo              ,
#endif
    spinButtonUpdate                        ,




 -- * Properties


-- ** adjustment #attr:adjustment#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SpinButtonAdjustmentPropertyInfo        ,
#endif
    constructSpinButtonAdjustment           ,
    getSpinButtonAdjustment                 ,
    setSpinButtonAdjustment                 ,
#if defined(ENABLE_OVERLOADING)
    spinButtonAdjustment                    ,
#endif


-- ** climbRate #attr:climbRate#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SpinButtonClimbRatePropertyInfo         ,
#endif
    constructSpinButtonClimbRate            ,
    getSpinButtonClimbRate                  ,
    setSpinButtonClimbRate                  ,
#if defined(ENABLE_OVERLOADING)
    spinButtonClimbRate                     ,
#endif


-- ** digits #attr:digits#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SpinButtonDigitsPropertyInfo            ,
#endif
    constructSpinButtonDigits               ,
    getSpinButtonDigits                     ,
    setSpinButtonDigits                     ,
#if defined(ENABLE_OVERLOADING)
    spinButtonDigits                        ,
#endif


-- ** numeric #attr:numeric#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SpinButtonNumericPropertyInfo           ,
#endif
    constructSpinButtonNumeric              ,
    getSpinButtonNumeric                    ,
    setSpinButtonNumeric                    ,
#if defined(ENABLE_OVERLOADING)
    spinButtonNumeric                       ,
#endif


-- ** snapToTicks #attr:snapToTicks#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SpinButtonSnapToTicksPropertyInfo       ,
#endif
    constructSpinButtonSnapToTicks          ,
    getSpinButtonSnapToTicks                ,
    setSpinButtonSnapToTicks                ,
#if defined(ENABLE_OVERLOADING)
    spinButtonSnapToTicks                   ,
#endif


-- ** updatePolicy #attr:updatePolicy#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SpinButtonUpdatePolicyPropertyInfo      ,
#endif
    constructSpinButtonUpdatePolicy         ,
    getSpinButtonUpdatePolicy               ,
    setSpinButtonUpdatePolicy               ,
#if defined(ENABLE_OVERLOADING)
    spinButtonUpdatePolicy                  ,
#endif


-- ** value #attr:value#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SpinButtonValuePropertyInfo             ,
#endif
    constructSpinButtonValue                ,
    getSpinButtonValue                      ,
    setSpinButtonValue                      ,
#if defined(ENABLE_OVERLOADING)
    spinButtonValue                         ,
#endif


-- ** wrap #attr:wrap#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SpinButtonWrapPropertyInfo              ,
#endif
    constructSpinButtonWrap                 ,
    getSpinButtonWrap                       ,
    setSpinButtonWrap                       ,
#if defined(ENABLE_OVERLOADING)
    spinButtonWrap                          ,
#endif




 -- * Signals


-- ** changeValue #signal:changeValue#

    SpinButtonChangeValueCallback           ,
#if defined(ENABLE_OVERLOADING)
    SpinButtonChangeValueSignalInfo         ,
#endif
    afterSpinButtonChangeValue              ,
    onSpinButtonChangeValue                 ,


-- ** input #signal:input#

    SpinButtonInputCallback                 ,
#if defined(ENABLE_OVERLOADING)
    SpinButtonInputSignalInfo               ,
#endif
    afterSpinButtonInput                    ,
    onSpinButtonInput                       ,


-- ** output #signal:output#

    SpinButtonOutputCallback                ,
#if defined(ENABLE_OVERLOADING)
    SpinButtonOutputSignalInfo              ,
#endif
    afterSpinButtonOutput                   ,
    onSpinButtonOutput                      ,


-- ** valueChanged #signal:valueChanged#

    SpinButtonValueChangedCallback          ,
#if defined(ENABLE_OVERLOADING)
    SpinButtonValueChangedSignalInfo        ,
#endif
    afterSpinButtonValueChanged             ,
    onSpinButtonValueChanged                ,


-- ** wrapped #signal:wrapped#

    SpinButtonWrappedCallback               ,
#if defined(ENABLE_OVERLOADING)
    SpinButtonWrappedSignalInfo             ,
#endif
    afterSpinButtonWrapped                  ,
    onSpinButtonWrapped                     ,




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
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.CellEditable as Gtk.CellEditable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Editable as Gtk.Editable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Orientable as Gtk.Orientable
import {-# SOURCE #-} qualified GI.Gtk.Objects.Adjustment as Gtk.Adjustment
import {-# SOURCE #-} qualified GI.Gtk.Objects.Entry as Gtk.Entry
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype SpinButton = SpinButton (SP.ManagedPtr SpinButton)
    deriving (Eq)

instance SP.ManagedPtrNewtype SpinButton where
    toManagedPtr (SpinButton p) = p

foreign import ccall "gtk_spin_button_get_type"
    c_gtk_spin_button_get_type :: IO B.Types.GType

instance B.Types.TypedObject SpinButton where
    glibType = c_gtk_spin_button_get_type

instance B.Types.GObject SpinButton

-- | Type class for types which can be safely cast to `SpinButton`, for instance with `toSpinButton`.
class (SP.GObject o, O.IsDescendantOf SpinButton o) => IsSpinButton o
instance (SP.GObject o, O.IsDescendantOf SpinButton o) => IsSpinButton o

instance O.HasParentTypes SpinButton
type instance O.ParentTypes SpinButton = '[Gtk.Entry.Entry, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable, Gtk.CellEditable.CellEditable, Gtk.Editable.Editable, Gtk.Orientable.Orientable]

-- | Cast to `SpinButton`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toSpinButton :: (MIO.MonadIO m, IsSpinButton o) => o -> m SpinButton
toSpinButton = MIO.liftIO . B.ManagedPtr.unsafeCastTo SpinButton

-- | Convert 'SpinButton' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe SpinButton) where
    gvalueGType_ = c_gtk_spin_button_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr SpinButton)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr SpinButton)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject SpinButton ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveSpinButtonMethod (t :: Symbol) (o :: *) :: * where
    ResolveSpinButtonMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveSpinButtonMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveSpinButtonMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveSpinButtonMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveSpinButtonMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveSpinButtonMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveSpinButtonMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveSpinButtonMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveSpinButtonMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveSpinButtonMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveSpinButtonMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveSpinButtonMethod "childNotify" o = Gtk.Widget.WidgetChildNotifyMethodInfo
    ResolveSpinButtonMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveSpinButtonMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveSpinButtonMethod "configure" o = SpinButtonConfigureMethodInfo
    ResolveSpinButtonMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveSpinButtonMethod "copyClipboard" o = Gtk.Editable.EditableCopyClipboardMethodInfo
    ResolveSpinButtonMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveSpinButtonMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveSpinButtonMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveSpinButtonMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveSpinButtonMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveSpinButtonMethod "cutClipboard" o = Gtk.Editable.EditableCutClipboardMethodInfo
    ResolveSpinButtonMethod "deleteSelection" o = Gtk.Editable.EditableDeleteSelectionMethodInfo
    ResolveSpinButtonMethod "deleteText" o = Gtk.Editable.EditableDeleteTextMethodInfo
    ResolveSpinButtonMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveSpinButtonMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveSpinButtonMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveSpinButtonMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveSpinButtonMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveSpinButtonMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveSpinButtonMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveSpinButtonMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveSpinButtonMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveSpinButtonMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveSpinButtonMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveSpinButtonMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveSpinButtonMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveSpinButtonMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveSpinButtonMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveSpinButtonMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveSpinButtonMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveSpinButtonMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveSpinButtonMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveSpinButtonMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveSpinButtonMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveSpinButtonMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveSpinButtonMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveSpinButtonMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveSpinButtonMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveSpinButtonMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveSpinButtonMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveSpinButtonMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveSpinButtonMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveSpinButtonMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveSpinButtonMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveSpinButtonMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveSpinButtonMethod "editingDone" o = Gtk.CellEditable.CellEditableEditingDoneMethodInfo
    ResolveSpinButtonMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveSpinButtonMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveSpinButtonMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveSpinButtonMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveSpinButtonMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveSpinButtonMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveSpinButtonMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveSpinButtonMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveSpinButtonMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveSpinButtonMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveSpinButtonMethod "grabFocusWithoutSelecting" o = Gtk.Entry.EntryGrabFocusWithoutSelectingMethodInfo
    ResolveSpinButtonMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveSpinButtonMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveSpinButtonMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveSpinButtonMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveSpinButtonMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveSpinButtonMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveSpinButtonMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveSpinButtonMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveSpinButtonMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveSpinButtonMethod "imContextFilterKeypress" o = Gtk.Entry.EntryImContextFilterKeypressMethodInfo
    ResolveSpinButtonMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveSpinButtonMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveSpinButtonMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveSpinButtonMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveSpinButtonMethod "insertText" o = Gtk.Editable.EditableInsertTextMethodInfo
    ResolveSpinButtonMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveSpinButtonMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveSpinButtonMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveSpinButtonMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveSpinButtonMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveSpinButtonMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveSpinButtonMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveSpinButtonMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveSpinButtonMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveSpinButtonMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveSpinButtonMethod "layoutIndexToTextIndex" o = Gtk.Entry.EntryLayoutIndexToTextIndexMethodInfo
    ResolveSpinButtonMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveSpinButtonMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveSpinButtonMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveSpinButtonMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveSpinButtonMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveSpinButtonMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveSpinButtonMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveSpinButtonMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveSpinButtonMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveSpinButtonMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveSpinButtonMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveSpinButtonMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveSpinButtonMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveSpinButtonMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveSpinButtonMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveSpinButtonMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveSpinButtonMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveSpinButtonMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveSpinButtonMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveSpinButtonMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveSpinButtonMethod "pasteClipboard" o = Gtk.Editable.EditablePasteClipboardMethodInfo
    ResolveSpinButtonMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveSpinButtonMethod "progressPulse" o = Gtk.Entry.EntryProgressPulseMethodInfo
    ResolveSpinButtonMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveSpinButtonMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveSpinButtonMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveSpinButtonMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveSpinButtonMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveSpinButtonMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveSpinButtonMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveSpinButtonMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveSpinButtonMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveSpinButtonMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveSpinButtonMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveSpinButtonMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveSpinButtonMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveSpinButtonMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveSpinButtonMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveSpinButtonMethod "removeWidget" o = Gtk.CellEditable.CellEditableRemoveWidgetMethodInfo
    ResolveSpinButtonMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveSpinButtonMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveSpinButtonMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveSpinButtonMethod "resetImContext" o = Gtk.Entry.EntryResetImContextMethodInfo
    ResolveSpinButtonMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveSpinButtonMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveSpinButtonMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveSpinButtonMethod "selectRegion" o = Gtk.Editable.EditableSelectRegionMethodInfo
    ResolveSpinButtonMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveSpinButtonMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveSpinButtonMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveSpinButtonMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveSpinButtonMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveSpinButtonMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveSpinButtonMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveSpinButtonMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveSpinButtonMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveSpinButtonMethod "spin" o = SpinButtonSpinMethodInfo
    ResolveSpinButtonMethod "startEditing" o = Gtk.CellEditable.CellEditableStartEditingMethodInfo
    ResolveSpinButtonMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveSpinButtonMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveSpinButtonMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveSpinButtonMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveSpinButtonMethod "textIndexToLayoutIndex" o = Gtk.Entry.EntryTextIndexToLayoutIndexMethodInfo
    ResolveSpinButtonMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveSpinButtonMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveSpinButtonMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveSpinButtonMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveSpinButtonMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveSpinButtonMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveSpinButtonMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveSpinButtonMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveSpinButtonMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveSpinButtonMethod "unsetInvisibleChar" o = Gtk.Entry.EntryUnsetInvisibleCharMethodInfo
    ResolveSpinButtonMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveSpinButtonMethod "update" o = SpinButtonUpdateMethodInfo
    ResolveSpinButtonMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveSpinButtonMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveSpinButtonMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveSpinButtonMethod "getActivatesDefault" o = Gtk.Entry.EntryGetActivatesDefaultMethodInfo
    ResolveSpinButtonMethod "getAdjustment" o = SpinButtonGetAdjustmentMethodInfo
    ResolveSpinButtonMethod "getAlignment" o = Gtk.Entry.EntryGetAlignmentMethodInfo
    ResolveSpinButtonMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveSpinButtonMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveSpinButtonMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveSpinButtonMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveSpinButtonMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveSpinButtonMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveSpinButtonMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveSpinButtonMethod "getAttributes" o = Gtk.Entry.EntryGetAttributesMethodInfo
    ResolveSpinButtonMethod "getBuffer" o = Gtk.Entry.EntryGetBufferMethodInfo
    ResolveSpinButtonMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveSpinButtonMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveSpinButtonMethod "getChars" o = Gtk.Editable.EditableGetCharsMethodInfo
    ResolveSpinButtonMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveSpinButtonMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveSpinButtonMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveSpinButtonMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveSpinButtonMethod "getCompletion" o = Gtk.Entry.EntryGetCompletionMethodInfo
    ResolveSpinButtonMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveSpinButtonMethod "getCurrentIconDragSource" o = Gtk.Entry.EntryGetCurrentIconDragSourceMethodInfo
    ResolveSpinButtonMethod "getCursorHadjustment" o = Gtk.Entry.EntryGetCursorHadjustmentMethodInfo
    ResolveSpinButtonMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveSpinButtonMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveSpinButtonMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveSpinButtonMethod "getDigits" o = SpinButtonGetDigitsMethodInfo
    ResolveSpinButtonMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveSpinButtonMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveSpinButtonMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveSpinButtonMethod "getEditable" o = Gtk.Editable.EditableGetEditableMethodInfo
    ResolveSpinButtonMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveSpinButtonMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveSpinButtonMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveSpinButtonMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveSpinButtonMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveSpinButtonMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveSpinButtonMethod "getHasFrame" o = Gtk.Entry.EntryGetHasFrameMethodInfo
    ResolveSpinButtonMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveSpinButtonMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveSpinButtonMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveSpinButtonMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveSpinButtonMethod "getIconActivatable" o = Gtk.Entry.EntryGetIconActivatableMethodInfo
    ResolveSpinButtonMethod "getIconArea" o = Gtk.Entry.EntryGetIconAreaMethodInfo
    ResolveSpinButtonMethod "getIconAtPos" o = Gtk.Entry.EntryGetIconAtPosMethodInfo
    ResolveSpinButtonMethod "getIconGicon" o = Gtk.Entry.EntryGetIconGiconMethodInfo
    ResolveSpinButtonMethod "getIconName" o = Gtk.Entry.EntryGetIconNameMethodInfo
    ResolveSpinButtonMethod "getIconPixbuf" o = Gtk.Entry.EntryGetIconPixbufMethodInfo
    ResolveSpinButtonMethod "getIconSensitive" o = Gtk.Entry.EntryGetIconSensitiveMethodInfo
    ResolveSpinButtonMethod "getIconStock" o = Gtk.Entry.EntryGetIconStockMethodInfo
    ResolveSpinButtonMethod "getIconStorageType" o = Gtk.Entry.EntryGetIconStorageTypeMethodInfo
    ResolveSpinButtonMethod "getIconTooltipMarkup" o = Gtk.Entry.EntryGetIconTooltipMarkupMethodInfo
    ResolveSpinButtonMethod "getIconTooltipText" o = Gtk.Entry.EntryGetIconTooltipTextMethodInfo
    ResolveSpinButtonMethod "getIncrements" o = SpinButtonGetIncrementsMethodInfo
    ResolveSpinButtonMethod "getInnerBorder" o = Gtk.Entry.EntryGetInnerBorderMethodInfo
    ResolveSpinButtonMethod "getInputHints" o = Gtk.Entry.EntryGetInputHintsMethodInfo
    ResolveSpinButtonMethod "getInputPurpose" o = Gtk.Entry.EntryGetInputPurposeMethodInfo
    ResolveSpinButtonMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveSpinButtonMethod "getInvisibleChar" o = Gtk.Entry.EntryGetInvisibleCharMethodInfo
    ResolveSpinButtonMethod "getLayout" o = Gtk.Entry.EntryGetLayoutMethodInfo
    ResolveSpinButtonMethod "getLayoutOffsets" o = Gtk.Entry.EntryGetLayoutOffsetsMethodInfo
    ResolveSpinButtonMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveSpinButtonMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveSpinButtonMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveSpinButtonMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveSpinButtonMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveSpinButtonMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveSpinButtonMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveSpinButtonMethod "getMaxLength" o = Gtk.Entry.EntryGetMaxLengthMethodInfo
    ResolveSpinButtonMethod "getMaxWidthChars" o = Gtk.Entry.EntryGetMaxWidthCharsMethodInfo
    ResolveSpinButtonMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveSpinButtonMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveSpinButtonMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveSpinButtonMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveSpinButtonMethod "getNumeric" o = SpinButtonGetNumericMethodInfo
    ResolveSpinButtonMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveSpinButtonMethod "getOrientation" o = Gtk.Orientable.OrientableGetOrientationMethodInfo
    ResolveSpinButtonMethod "getOverwriteMode" o = Gtk.Entry.EntryGetOverwriteModeMethodInfo
    ResolveSpinButtonMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveSpinButtonMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveSpinButtonMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveSpinButtonMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveSpinButtonMethod "getPlaceholderText" o = Gtk.Entry.EntryGetPlaceholderTextMethodInfo
    ResolveSpinButtonMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveSpinButtonMethod "getPosition" o = Gtk.Editable.EditableGetPositionMethodInfo
    ResolveSpinButtonMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveSpinButtonMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveSpinButtonMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveSpinButtonMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveSpinButtonMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveSpinButtonMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveSpinButtonMethod "getProgressFraction" o = Gtk.Entry.EntryGetProgressFractionMethodInfo
    ResolveSpinButtonMethod "getProgressPulseStep" o = Gtk.Entry.EntryGetProgressPulseStepMethodInfo
    ResolveSpinButtonMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveSpinButtonMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveSpinButtonMethod "getRange" o = SpinButtonGetRangeMethodInfo
    ResolveSpinButtonMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveSpinButtonMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveSpinButtonMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveSpinButtonMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveSpinButtonMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveSpinButtonMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveSpinButtonMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveSpinButtonMethod "getSelectionBounds" o = Gtk.Editable.EditableGetSelectionBoundsMethodInfo
    ResolveSpinButtonMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveSpinButtonMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveSpinButtonMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveSpinButtonMethod "getSnapToTicks" o = SpinButtonGetSnapToTicksMethodInfo
    ResolveSpinButtonMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveSpinButtonMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveSpinButtonMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveSpinButtonMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveSpinButtonMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveSpinButtonMethod "getTabs" o = Gtk.Entry.EntryGetTabsMethodInfo
    ResolveSpinButtonMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveSpinButtonMethod "getText" o = Gtk.Entry.EntryGetTextMethodInfo
    ResolveSpinButtonMethod "getTextArea" o = Gtk.Entry.EntryGetTextAreaMethodInfo
    ResolveSpinButtonMethod "getTextLength" o = Gtk.Entry.EntryGetTextLengthMethodInfo
    ResolveSpinButtonMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveSpinButtonMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveSpinButtonMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveSpinButtonMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveSpinButtonMethod "getUpdatePolicy" o = SpinButtonGetUpdatePolicyMethodInfo
    ResolveSpinButtonMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveSpinButtonMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveSpinButtonMethod "getValue" o = SpinButtonGetValueMethodInfo
    ResolveSpinButtonMethod "getValueAsInt" o = SpinButtonGetValueAsIntMethodInfo
    ResolveSpinButtonMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveSpinButtonMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveSpinButtonMethod "getVisibility" o = Gtk.Entry.EntryGetVisibilityMethodInfo
    ResolveSpinButtonMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveSpinButtonMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveSpinButtonMethod "getWidthChars" o = Gtk.Entry.EntryGetWidthCharsMethodInfo
    ResolveSpinButtonMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveSpinButtonMethod "getWrap" o = SpinButtonGetWrapMethodInfo
    ResolveSpinButtonMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveSpinButtonMethod "setActivatesDefault" o = Gtk.Entry.EntrySetActivatesDefaultMethodInfo
    ResolveSpinButtonMethod "setAdjustment" o = SpinButtonSetAdjustmentMethodInfo
    ResolveSpinButtonMethod "setAlignment" o = Gtk.Entry.EntrySetAlignmentMethodInfo
    ResolveSpinButtonMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveSpinButtonMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveSpinButtonMethod "setAttributes" o = Gtk.Entry.EntrySetAttributesMethodInfo
    ResolveSpinButtonMethod "setBuffer" o = Gtk.Entry.EntrySetBufferMethodInfo
    ResolveSpinButtonMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveSpinButtonMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveSpinButtonMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveSpinButtonMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveSpinButtonMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveSpinButtonMethod "setCompletion" o = Gtk.Entry.EntrySetCompletionMethodInfo
    ResolveSpinButtonMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveSpinButtonMethod "setCursorHadjustment" o = Gtk.Entry.EntrySetCursorHadjustmentMethodInfo
    ResolveSpinButtonMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveSpinButtonMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveSpinButtonMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveSpinButtonMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveSpinButtonMethod "setDigits" o = SpinButtonSetDigitsMethodInfo
    ResolveSpinButtonMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveSpinButtonMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveSpinButtonMethod "setEditable" o = Gtk.Editable.EditableSetEditableMethodInfo
    ResolveSpinButtonMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveSpinButtonMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveSpinButtonMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveSpinButtonMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveSpinButtonMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveSpinButtonMethod "setHasFrame" o = Gtk.Entry.EntrySetHasFrameMethodInfo
    ResolveSpinButtonMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveSpinButtonMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveSpinButtonMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveSpinButtonMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveSpinButtonMethod "setIconActivatable" o = Gtk.Entry.EntrySetIconActivatableMethodInfo
    ResolveSpinButtonMethod "setIconDragSource" o = Gtk.Entry.EntrySetIconDragSourceMethodInfo
    ResolveSpinButtonMethod "setIconFromGicon" o = Gtk.Entry.EntrySetIconFromGiconMethodInfo
    ResolveSpinButtonMethod "setIconFromIconName" o = Gtk.Entry.EntrySetIconFromIconNameMethodInfo
    ResolveSpinButtonMethod "setIconFromPixbuf" o = Gtk.Entry.EntrySetIconFromPixbufMethodInfo
    ResolveSpinButtonMethod "setIconFromStock" o = Gtk.Entry.EntrySetIconFromStockMethodInfo
    ResolveSpinButtonMethod "setIconSensitive" o = Gtk.Entry.EntrySetIconSensitiveMethodInfo
    ResolveSpinButtonMethod "setIconTooltipMarkup" o = Gtk.Entry.EntrySetIconTooltipMarkupMethodInfo
    ResolveSpinButtonMethod "setIconTooltipText" o = Gtk.Entry.EntrySetIconTooltipTextMethodInfo
    ResolveSpinButtonMethod "setIncrements" o = SpinButtonSetIncrementsMethodInfo
    ResolveSpinButtonMethod "setInnerBorder" o = Gtk.Entry.EntrySetInnerBorderMethodInfo
    ResolveSpinButtonMethod "setInputHints" o = Gtk.Entry.EntrySetInputHintsMethodInfo
    ResolveSpinButtonMethod "setInputPurpose" o = Gtk.Entry.EntrySetInputPurposeMethodInfo
    ResolveSpinButtonMethod "setInvisibleChar" o = Gtk.Entry.EntrySetInvisibleCharMethodInfo
    ResolveSpinButtonMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveSpinButtonMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveSpinButtonMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveSpinButtonMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveSpinButtonMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveSpinButtonMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveSpinButtonMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveSpinButtonMethod "setMaxLength" o = Gtk.Entry.EntrySetMaxLengthMethodInfo
    ResolveSpinButtonMethod "setMaxWidthChars" o = Gtk.Entry.EntrySetMaxWidthCharsMethodInfo
    ResolveSpinButtonMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveSpinButtonMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveSpinButtonMethod "setNumeric" o = SpinButtonSetNumericMethodInfo
    ResolveSpinButtonMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveSpinButtonMethod "setOrientation" o = Gtk.Orientable.OrientableSetOrientationMethodInfo
    ResolveSpinButtonMethod "setOverwriteMode" o = Gtk.Entry.EntrySetOverwriteModeMethodInfo
    ResolveSpinButtonMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveSpinButtonMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveSpinButtonMethod "setPlaceholderText" o = Gtk.Entry.EntrySetPlaceholderTextMethodInfo
    ResolveSpinButtonMethod "setPosition" o = Gtk.Editable.EditableSetPositionMethodInfo
    ResolveSpinButtonMethod "setProgressFraction" o = Gtk.Entry.EntrySetProgressFractionMethodInfo
    ResolveSpinButtonMethod "setProgressPulseStep" o = Gtk.Entry.EntrySetProgressPulseStepMethodInfo
    ResolveSpinButtonMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveSpinButtonMethod "setRange" o = SpinButtonSetRangeMethodInfo
    ResolveSpinButtonMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveSpinButtonMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveSpinButtonMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveSpinButtonMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveSpinButtonMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveSpinButtonMethod "setSnapToTicks" o = SpinButtonSetSnapToTicksMethodInfo
    ResolveSpinButtonMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveSpinButtonMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveSpinButtonMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveSpinButtonMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveSpinButtonMethod "setTabs" o = Gtk.Entry.EntrySetTabsMethodInfo
    ResolveSpinButtonMethod "setText" o = Gtk.Entry.EntrySetTextMethodInfo
    ResolveSpinButtonMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveSpinButtonMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveSpinButtonMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveSpinButtonMethod "setUpdatePolicy" o = SpinButtonSetUpdatePolicyMethodInfo
    ResolveSpinButtonMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveSpinButtonMethod "setValue" o = SpinButtonSetValueMethodInfo
    ResolveSpinButtonMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveSpinButtonMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveSpinButtonMethod "setVisibility" o = Gtk.Entry.EntrySetVisibilityMethodInfo
    ResolveSpinButtonMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveSpinButtonMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveSpinButtonMethod "setWidthChars" o = Gtk.Entry.EntrySetWidthCharsMethodInfo
    ResolveSpinButtonMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveSpinButtonMethod "setWrap" o = SpinButtonSetWrapMethodInfo
    ResolveSpinButtonMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveSpinButtonMethod t SpinButton, O.OverloadedMethod info SpinButton p) => OL.IsLabel t (SpinButton -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveSpinButtonMethod t SpinButton, O.OverloadedMethod info SpinButton p, R.HasField t SpinButton p) => R.HasField t SpinButton p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveSpinButtonMethod t SpinButton, O.OverloadedMethodInfo info SpinButton) => OL.IsLabel t (O.MethodProxy info SpinButton) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal SpinButton::change-value
-- | The [changeValue](#g:signal:changeValue) signal is a [keybinding signal][GtkBindingSignal]
-- which gets emitted when the user initiates a value change.
-- 
-- Applications should not connect to it, but may emit it with
-- @/g_signal_emit_by_name()/@ if they need to control the cursor
-- programmatically.
-- 
-- The default bindings for this signal are Up\/Down and PageUp and\/PageDown.
type SpinButtonChangeValueCallback =
    Gtk.Enums.ScrollType
    -- ^ /@scroll@/: a t'GI.Gtk.Enums.ScrollType' to specify the speed and amount of change
    -> IO ()

type C_SpinButtonChangeValueCallback =
    Ptr SpinButton ->                       -- object
    CUInt ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_SpinButtonChangeValueCallback`.
foreign import ccall "wrapper"
    mk_SpinButtonChangeValueCallback :: C_SpinButtonChangeValueCallback -> IO (FunPtr C_SpinButtonChangeValueCallback)

wrap_SpinButtonChangeValueCallback :: 
    GObject a => (a -> SpinButtonChangeValueCallback) ->
    C_SpinButtonChangeValueCallback
wrap_SpinButtonChangeValueCallback gi'cb gi'selfPtr scroll _ = do
    let scroll' = (toEnum . fromIntegral) scroll
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  scroll'


-- | Connect a signal handler for the [changeValue](#signal:changeValue) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' spinButton #changeValue callback
-- @
-- 
-- 
onSpinButtonChangeValue :: (IsSpinButton a, MonadIO m) => a -> ((?self :: a) => SpinButtonChangeValueCallback) -> m SignalHandlerId
onSpinButtonChangeValue obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_SpinButtonChangeValueCallback wrapped
    wrapped'' <- mk_SpinButtonChangeValueCallback wrapped'
    connectSignalFunPtr obj "change-value" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [changeValue](#signal:changeValue) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' spinButton #changeValue callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterSpinButtonChangeValue :: (IsSpinButton a, MonadIO m) => a -> ((?self :: a) => SpinButtonChangeValueCallback) -> m SignalHandlerId
afterSpinButtonChangeValue obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_SpinButtonChangeValueCallback wrapped
    wrapped'' <- mk_SpinButtonChangeValueCallback wrapped'
    connectSignalFunPtr obj "change-value" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data SpinButtonChangeValueSignalInfo
instance SignalInfo SpinButtonChangeValueSignalInfo where
    type HaskellCallbackType SpinButtonChangeValueSignalInfo = SpinButtonChangeValueCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_SpinButtonChangeValueCallback cb
        cb'' <- mk_SpinButtonChangeValueCallback cb'
        connectSignalFunPtr obj "change-value" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton::change-value"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#g:signal:changeValue"})

#endif

-- signal SpinButton::input
-- | The [input](#g:signal:input) signal can be used to influence the conversion of
-- the users input into a double value. The signal handler is
-- expected to use 'GI.Gtk.Objects.Entry.entryGetText' to retrieve the text of
-- the entry and set /@newValue@/ to the new value.
-- 
-- The default conversion uses 'GI.GLib.Functions.strtod'.
type SpinButtonInputCallback =
    IO ((Int32, Double))
    -- ^ __Returns:__ 'P.True' for a successful conversion, 'P.False' if the input
    --     was not handled, and 'GI.Gtk.Constants.INPUT_ERROR' if the conversion failed.

type C_SpinButtonInputCallback =
    Ptr SpinButton ->                       -- object
    Ptr CDouble ->
    Ptr () ->                               -- user_data
    IO Int32

-- | Generate a function pointer callable from C code, from a `C_SpinButtonInputCallback`.
foreign import ccall "wrapper"
    mk_SpinButtonInputCallback :: C_SpinButtonInputCallback -> IO (FunPtr C_SpinButtonInputCallback)

wrap_SpinButtonInputCallback :: 
    GObject a => (a -> SpinButtonInputCallback) ->
    C_SpinButtonInputCallback
wrap_SpinButtonInputCallback gi'cb gi'selfPtr newValue _ = do
    (result, outnewValue) <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 
    let outnewValue' = realToFrac outnewValue
    poke newValue outnewValue'
    return result


-- | Connect a signal handler for the [input](#signal:input) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' spinButton #input callback
-- @
-- 
-- 
onSpinButtonInput :: (IsSpinButton a, MonadIO m) => a -> ((?self :: a) => SpinButtonInputCallback) -> m SignalHandlerId
onSpinButtonInput obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_SpinButtonInputCallback wrapped
    wrapped'' <- mk_SpinButtonInputCallback wrapped'
    connectSignalFunPtr obj "input" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [input](#signal:input) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' spinButton #input callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterSpinButtonInput :: (IsSpinButton a, MonadIO m) => a -> ((?self :: a) => SpinButtonInputCallback) -> m SignalHandlerId
afterSpinButtonInput obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_SpinButtonInputCallback wrapped
    wrapped'' <- mk_SpinButtonInputCallback wrapped'
    connectSignalFunPtr obj "input" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data SpinButtonInputSignalInfo
instance SignalInfo SpinButtonInputSignalInfo where
    type HaskellCallbackType SpinButtonInputSignalInfo = SpinButtonInputCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_SpinButtonInputCallback cb
        cb'' <- mk_SpinButtonInputCallback cb'
        connectSignalFunPtr obj "input" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton::input"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#g:signal:input"})

#endif

-- signal SpinButton::output
-- | The [output](#g:signal:output) signal can be used to change to formatting
-- of the value that is displayed in the spin buttons entry.
-- 
-- === /C code/
-- >
-- >// show leading zeros
-- >static gboolean
-- >on_output (GtkSpinButton *spin,
-- >           gpointer       data)
-- >{
-- >   GtkAdjustment *adjustment;
-- >   gchar *text;
-- >   int value;
-- >
-- >   adjustment = gtk_spin_button_get_adjustment (spin);
-- >   value = (int)gtk_adjustment_get_value (adjustment);
-- >   text = g_strdup_printf ("%02d", value);
-- >   gtk_entry_set_text (GTK_ENTRY (spin), text);
-- >   g_free (text);
-- >
-- >   return TRUE;
-- >}
type SpinButtonOutputCallback =
    IO Bool
    -- ^ __Returns:__ 'P.True' if the value has been displayed

type C_SpinButtonOutputCallback =
    Ptr SpinButton ->                       -- object
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_SpinButtonOutputCallback`.
foreign import ccall "wrapper"
    mk_SpinButtonOutputCallback :: C_SpinButtonOutputCallback -> IO (FunPtr C_SpinButtonOutputCallback)

wrap_SpinButtonOutputCallback :: 
    GObject a => (a -> SpinButtonOutputCallback) ->
    C_SpinButtonOutputCallback
wrap_SpinButtonOutputCallback gi'cb gi'selfPtr _ = do
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 
    let result' = (fromIntegral . fromEnum) result
    return result'


-- | Connect a signal handler for the [output](#signal:output) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' spinButton #output callback
-- @
-- 
-- 
onSpinButtonOutput :: (IsSpinButton a, MonadIO m) => a -> ((?self :: a) => SpinButtonOutputCallback) -> m SignalHandlerId
onSpinButtonOutput obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_SpinButtonOutputCallback wrapped
    wrapped'' <- mk_SpinButtonOutputCallback wrapped'
    connectSignalFunPtr obj "output" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [output](#signal:output) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' spinButton #output callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterSpinButtonOutput :: (IsSpinButton a, MonadIO m) => a -> ((?self :: a) => SpinButtonOutputCallback) -> m SignalHandlerId
afterSpinButtonOutput obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_SpinButtonOutputCallback wrapped
    wrapped'' <- mk_SpinButtonOutputCallback wrapped'
    connectSignalFunPtr obj "output" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data SpinButtonOutputSignalInfo
instance SignalInfo SpinButtonOutputSignalInfo where
    type HaskellCallbackType SpinButtonOutputSignalInfo = SpinButtonOutputCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_SpinButtonOutputCallback cb
        cb'' <- mk_SpinButtonOutputCallback cb'
        connectSignalFunPtr obj "output" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton::output"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#g:signal:output"})

#endif

-- signal SpinButton::value-changed
-- | The [valueChanged](#g:signal:valueChanged) signal is emitted when the value represented by
-- /@spinbutton@/ changes. Also see the [SpinButton::output]("GI.Gtk.Objects.SpinButton#g:signal:output") signal.
type SpinButtonValueChangedCallback =
    IO ()

type C_SpinButtonValueChangedCallback =
    Ptr SpinButton ->                       -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_SpinButtonValueChangedCallback`.
foreign import ccall "wrapper"
    mk_SpinButtonValueChangedCallback :: C_SpinButtonValueChangedCallback -> IO (FunPtr C_SpinButtonValueChangedCallback)

wrap_SpinButtonValueChangedCallback :: 
    GObject a => (a -> SpinButtonValueChangedCallback) ->
    C_SpinButtonValueChangedCallback
wrap_SpinButtonValueChangedCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [valueChanged](#signal:valueChanged) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' spinButton #valueChanged callback
-- @
-- 
-- 
onSpinButtonValueChanged :: (IsSpinButton a, MonadIO m) => a -> ((?self :: a) => SpinButtonValueChangedCallback) -> m SignalHandlerId
onSpinButtonValueChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_SpinButtonValueChangedCallback wrapped
    wrapped'' <- mk_SpinButtonValueChangedCallback wrapped'
    connectSignalFunPtr obj "value-changed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [valueChanged](#signal:valueChanged) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' spinButton #valueChanged callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterSpinButtonValueChanged :: (IsSpinButton a, MonadIO m) => a -> ((?self :: a) => SpinButtonValueChangedCallback) -> m SignalHandlerId
afterSpinButtonValueChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_SpinButtonValueChangedCallback wrapped
    wrapped'' <- mk_SpinButtonValueChangedCallback wrapped'
    connectSignalFunPtr obj "value-changed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data SpinButtonValueChangedSignalInfo
instance SignalInfo SpinButtonValueChangedSignalInfo where
    type HaskellCallbackType SpinButtonValueChangedSignalInfo = SpinButtonValueChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_SpinButtonValueChangedCallback cb
        cb'' <- mk_SpinButtonValueChangedCallback cb'
        connectSignalFunPtr obj "value-changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton::value-changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#g:signal:valueChanged"})

#endif

-- signal SpinButton::wrapped
-- | The [wrapped](#g:signal:wrapped) signal is emitted right after the spinbutton wraps
-- from its maximum to minimum value or vice-versa.
-- 
-- /Since: 2.10/
type SpinButtonWrappedCallback =
    IO ()

type C_SpinButtonWrappedCallback =
    Ptr SpinButton ->                       -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_SpinButtonWrappedCallback`.
foreign import ccall "wrapper"
    mk_SpinButtonWrappedCallback :: C_SpinButtonWrappedCallback -> IO (FunPtr C_SpinButtonWrappedCallback)

wrap_SpinButtonWrappedCallback :: 
    GObject a => (a -> SpinButtonWrappedCallback) ->
    C_SpinButtonWrappedCallback
wrap_SpinButtonWrappedCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [wrapped](#signal:wrapped) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' spinButton #wrapped callback
-- @
-- 
-- 
onSpinButtonWrapped :: (IsSpinButton a, MonadIO m) => a -> ((?self :: a) => SpinButtonWrappedCallback) -> m SignalHandlerId
onSpinButtonWrapped obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_SpinButtonWrappedCallback wrapped
    wrapped'' <- mk_SpinButtonWrappedCallback wrapped'
    connectSignalFunPtr obj "wrapped" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [wrapped](#signal:wrapped) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' spinButton #wrapped callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterSpinButtonWrapped :: (IsSpinButton a, MonadIO m) => a -> ((?self :: a) => SpinButtonWrappedCallback) -> m SignalHandlerId
afterSpinButtonWrapped obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_SpinButtonWrappedCallback wrapped
    wrapped'' <- mk_SpinButtonWrappedCallback wrapped'
    connectSignalFunPtr obj "wrapped" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data SpinButtonWrappedSignalInfo
instance SignalInfo SpinButtonWrappedSignalInfo where
    type HaskellCallbackType SpinButtonWrappedSignalInfo = SpinButtonWrappedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_SpinButtonWrappedCallback cb
        cb'' <- mk_SpinButtonWrappedCallback cb'
        connectSignalFunPtr obj "wrapped" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton::wrapped"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#g:signal:wrapped"})

#endif

-- VVV Prop "adjustment"
   -- Type: TInterface (Name {namespace = "Gtk", name = "Adjustment"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@adjustment@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' spinButton #adjustment
-- @
getSpinButtonAdjustment :: (MonadIO m, IsSpinButton o) => o -> m Gtk.Adjustment.Adjustment
getSpinButtonAdjustment obj = MIO.liftIO $ checkUnexpectedNothing "getSpinButtonAdjustment" $ B.Properties.getObjectPropertyObject obj "adjustment" Gtk.Adjustment.Adjustment

-- | Set the value of the “@adjustment@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' spinButton [ #adjustment 'Data.GI.Base.Attributes.:=' value ]
-- @
setSpinButtonAdjustment :: (MonadIO m, IsSpinButton o, Gtk.Adjustment.IsAdjustment a) => o -> a -> m ()
setSpinButtonAdjustment obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "adjustment" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@adjustment@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSpinButtonAdjustment :: (IsSpinButton o, MIO.MonadIO m, Gtk.Adjustment.IsAdjustment a) => a -> m (GValueConstruct o)
constructSpinButtonAdjustment val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "adjustment" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data SpinButtonAdjustmentPropertyInfo
instance AttrInfo SpinButtonAdjustmentPropertyInfo where
    type AttrAllowedOps SpinButtonAdjustmentPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SpinButtonAdjustmentPropertyInfo = IsSpinButton
    type AttrSetTypeConstraint SpinButtonAdjustmentPropertyInfo = Gtk.Adjustment.IsAdjustment
    type AttrTransferTypeConstraint SpinButtonAdjustmentPropertyInfo = Gtk.Adjustment.IsAdjustment
    type AttrTransferType SpinButtonAdjustmentPropertyInfo = Gtk.Adjustment.Adjustment
    type AttrGetType SpinButtonAdjustmentPropertyInfo = Gtk.Adjustment.Adjustment
    type AttrLabel SpinButtonAdjustmentPropertyInfo = "adjustment"
    type AttrOrigin SpinButtonAdjustmentPropertyInfo = SpinButton
    attrGet = getSpinButtonAdjustment
    attrSet = setSpinButtonAdjustment
    attrTransfer _ v = do
        unsafeCastTo Gtk.Adjustment.Adjustment v
    attrConstruct = constructSpinButtonAdjustment
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton.adjustment"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#g:attr:adjustment"
        })
#endif

-- VVV Prop "climb-rate"
   -- Type: TBasicType TDouble
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@climb-rate@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' spinButton #climbRate
-- @
getSpinButtonClimbRate :: (MonadIO m, IsSpinButton o) => o -> m Double
getSpinButtonClimbRate obj = MIO.liftIO $ B.Properties.getObjectPropertyDouble obj "climb-rate"

-- | Set the value of the “@climb-rate@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' spinButton [ #climbRate 'Data.GI.Base.Attributes.:=' value ]
-- @
setSpinButtonClimbRate :: (MonadIO m, IsSpinButton o) => o -> Double -> m ()
setSpinButtonClimbRate obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyDouble obj "climb-rate" val

-- | Construct a `GValueConstruct` with valid value for the “@climb-rate@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSpinButtonClimbRate :: (IsSpinButton o, MIO.MonadIO m) => Double -> m (GValueConstruct o)
constructSpinButtonClimbRate val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyDouble "climb-rate" val

#if defined(ENABLE_OVERLOADING)
data SpinButtonClimbRatePropertyInfo
instance AttrInfo SpinButtonClimbRatePropertyInfo where
    type AttrAllowedOps SpinButtonClimbRatePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SpinButtonClimbRatePropertyInfo = IsSpinButton
    type AttrSetTypeConstraint SpinButtonClimbRatePropertyInfo = (~) Double
    type AttrTransferTypeConstraint SpinButtonClimbRatePropertyInfo = (~) Double
    type AttrTransferType SpinButtonClimbRatePropertyInfo = Double
    type AttrGetType SpinButtonClimbRatePropertyInfo = Double
    type AttrLabel SpinButtonClimbRatePropertyInfo = "climb-rate"
    type AttrOrigin SpinButtonClimbRatePropertyInfo = SpinButton
    attrGet = getSpinButtonClimbRate
    attrSet = setSpinButtonClimbRate
    attrTransfer _ v = do
        return v
    attrConstruct = constructSpinButtonClimbRate
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton.climbRate"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#g:attr:climbRate"
        })
#endif

-- VVV Prop "digits"
   -- Type: TBasicType TUInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@digits@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' spinButton #digits
-- @
getSpinButtonDigits :: (MonadIO m, IsSpinButton o) => o -> m Word32
getSpinButtonDigits obj = MIO.liftIO $ B.Properties.getObjectPropertyUInt32 obj "digits"

-- | Set the value of the “@digits@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' spinButton [ #digits 'Data.GI.Base.Attributes.:=' value ]
-- @
setSpinButtonDigits :: (MonadIO m, IsSpinButton o) => o -> Word32 -> m ()
setSpinButtonDigits obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyUInt32 obj "digits" val

-- | Construct a `GValueConstruct` with valid value for the “@digits@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSpinButtonDigits :: (IsSpinButton o, MIO.MonadIO m) => Word32 -> m (GValueConstruct o)
constructSpinButtonDigits val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyUInt32 "digits" val

#if defined(ENABLE_OVERLOADING)
data SpinButtonDigitsPropertyInfo
instance AttrInfo SpinButtonDigitsPropertyInfo where
    type AttrAllowedOps SpinButtonDigitsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SpinButtonDigitsPropertyInfo = IsSpinButton
    type AttrSetTypeConstraint SpinButtonDigitsPropertyInfo = (~) Word32
    type AttrTransferTypeConstraint SpinButtonDigitsPropertyInfo = (~) Word32
    type AttrTransferType SpinButtonDigitsPropertyInfo = Word32
    type AttrGetType SpinButtonDigitsPropertyInfo = Word32
    type AttrLabel SpinButtonDigitsPropertyInfo = "digits"
    type AttrOrigin SpinButtonDigitsPropertyInfo = SpinButton
    attrGet = getSpinButtonDigits
    attrSet = setSpinButtonDigits
    attrTransfer _ v = do
        return v
    attrConstruct = constructSpinButtonDigits
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton.digits"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#g:attr:digits"
        })
#endif

-- VVV Prop "numeric"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@numeric@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' spinButton #numeric
-- @
getSpinButtonNumeric :: (MonadIO m, IsSpinButton o) => o -> m Bool
getSpinButtonNumeric obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "numeric"

-- | Set the value of the “@numeric@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' spinButton [ #numeric 'Data.GI.Base.Attributes.:=' value ]
-- @
setSpinButtonNumeric :: (MonadIO m, IsSpinButton o) => o -> Bool -> m ()
setSpinButtonNumeric obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "numeric" val

-- | Construct a `GValueConstruct` with valid value for the “@numeric@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSpinButtonNumeric :: (IsSpinButton o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSpinButtonNumeric val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "numeric" val

#if defined(ENABLE_OVERLOADING)
data SpinButtonNumericPropertyInfo
instance AttrInfo SpinButtonNumericPropertyInfo where
    type AttrAllowedOps SpinButtonNumericPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SpinButtonNumericPropertyInfo = IsSpinButton
    type AttrSetTypeConstraint SpinButtonNumericPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SpinButtonNumericPropertyInfo = (~) Bool
    type AttrTransferType SpinButtonNumericPropertyInfo = Bool
    type AttrGetType SpinButtonNumericPropertyInfo = Bool
    type AttrLabel SpinButtonNumericPropertyInfo = "numeric"
    type AttrOrigin SpinButtonNumericPropertyInfo = SpinButton
    attrGet = getSpinButtonNumeric
    attrSet = setSpinButtonNumeric
    attrTransfer _ v = do
        return v
    attrConstruct = constructSpinButtonNumeric
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton.numeric"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#g:attr:numeric"
        })
#endif

-- VVV Prop "snap-to-ticks"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@snap-to-ticks@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' spinButton #snapToTicks
-- @
getSpinButtonSnapToTicks :: (MonadIO m, IsSpinButton o) => o -> m Bool
getSpinButtonSnapToTicks obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "snap-to-ticks"

-- | Set the value of the “@snap-to-ticks@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' spinButton [ #snapToTicks 'Data.GI.Base.Attributes.:=' value ]
-- @
setSpinButtonSnapToTicks :: (MonadIO m, IsSpinButton o) => o -> Bool -> m ()
setSpinButtonSnapToTicks obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "snap-to-ticks" val

-- | Construct a `GValueConstruct` with valid value for the “@snap-to-ticks@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSpinButtonSnapToTicks :: (IsSpinButton o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSpinButtonSnapToTicks val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "snap-to-ticks" val

#if defined(ENABLE_OVERLOADING)
data SpinButtonSnapToTicksPropertyInfo
instance AttrInfo SpinButtonSnapToTicksPropertyInfo where
    type AttrAllowedOps SpinButtonSnapToTicksPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SpinButtonSnapToTicksPropertyInfo = IsSpinButton
    type AttrSetTypeConstraint SpinButtonSnapToTicksPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SpinButtonSnapToTicksPropertyInfo = (~) Bool
    type AttrTransferType SpinButtonSnapToTicksPropertyInfo = Bool
    type AttrGetType SpinButtonSnapToTicksPropertyInfo = Bool
    type AttrLabel SpinButtonSnapToTicksPropertyInfo = "snap-to-ticks"
    type AttrOrigin SpinButtonSnapToTicksPropertyInfo = SpinButton
    attrGet = getSpinButtonSnapToTicks
    attrSet = setSpinButtonSnapToTicks
    attrTransfer _ v = do
        return v
    attrConstruct = constructSpinButtonSnapToTicks
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton.snapToTicks"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#g:attr:snapToTicks"
        })
#endif

-- VVV Prop "update-policy"
   -- Type: TInterface (Name {namespace = "Gtk", name = "SpinButtonUpdatePolicy"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@update-policy@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' spinButton #updatePolicy
-- @
getSpinButtonUpdatePolicy :: (MonadIO m, IsSpinButton o) => o -> m Gtk.Enums.SpinButtonUpdatePolicy
getSpinButtonUpdatePolicy obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "update-policy"

-- | Set the value of the “@update-policy@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' spinButton [ #updatePolicy 'Data.GI.Base.Attributes.:=' value ]
-- @
setSpinButtonUpdatePolicy :: (MonadIO m, IsSpinButton o) => o -> Gtk.Enums.SpinButtonUpdatePolicy -> m ()
setSpinButtonUpdatePolicy obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "update-policy" val

-- | Construct a `GValueConstruct` with valid value for the “@update-policy@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSpinButtonUpdatePolicy :: (IsSpinButton o, MIO.MonadIO m) => Gtk.Enums.SpinButtonUpdatePolicy -> m (GValueConstruct o)
constructSpinButtonUpdatePolicy val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "update-policy" val

#if defined(ENABLE_OVERLOADING)
data SpinButtonUpdatePolicyPropertyInfo
instance AttrInfo SpinButtonUpdatePolicyPropertyInfo where
    type AttrAllowedOps SpinButtonUpdatePolicyPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SpinButtonUpdatePolicyPropertyInfo = IsSpinButton
    type AttrSetTypeConstraint SpinButtonUpdatePolicyPropertyInfo = (~) Gtk.Enums.SpinButtonUpdatePolicy
    type AttrTransferTypeConstraint SpinButtonUpdatePolicyPropertyInfo = (~) Gtk.Enums.SpinButtonUpdatePolicy
    type AttrTransferType SpinButtonUpdatePolicyPropertyInfo = Gtk.Enums.SpinButtonUpdatePolicy
    type AttrGetType SpinButtonUpdatePolicyPropertyInfo = Gtk.Enums.SpinButtonUpdatePolicy
    type AttrLabel SpinButtonUpdatePolicyPropertyInfo = "update-policy"
    type AttrOrigin SpinButtonUpdatePolicyPropertyInfo = SpinButton
    attrGet = getSpinButtonUpdatePolicy
    attrSet = setSpinButtonUpdatePolicy
    attrTransfer _ v = do
        return v
    attrConstruct = constructSpinButtonUpdatePolicy
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton.updatePolicy"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#g:attr:updatePolicy"
        })
#endif

-- VVV Prop "value"
   -- Type: TBasicType TDouble
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@value@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' spinButton #value
-- @
getSpinButtonValue :: (MonadIO m, IsSpinButton o) => o -> m Double
getSpinButtonValue obj = MIO.liftIO $ B.Properties.getObjectPropertyDouble obj "value"

-- | Set the value of the “@value@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' spinButton [ #value 'Data.GI.Base.Attributes.:=' value ]
-- @
setSpinButtonValue :: (MonadIO m, IsSpinButton o) => o -> Double -> m ()
setSpinButtonValue obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyDouble obj "value" val

-- | Construct a `GValueConstruct` with valid value for the “@value@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSpinButtonValue :: (IsSpinButton o, MIO.MonadIO m) => Double -> m (GValueConstruct o)
constructSpinButtonValue val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyDouble "value" val

#if defined(ENABLE_OVERLOADING)
data SpinButtonValuePropertyInfo
instance AttrInfo SpinButtonValuePropertyInfo where
    type AttrAllowedOps SpinButtonValuePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SpinButtonValuePropertyInfo = IsSpinButton
    type AttrSetTypeConstraint SpinButtonValuePropertyInfo = (~) Double
    type AttrTransferTypeConstraint SpinButtonValuePropertyInfo = (~) Double
    type AttrTransferType SpinButtonValuePropertyInfo = Double
    type AttrGetType SpinButtonValuePropertyInfo = Double
    type AttrLabel SpinButtonValuePropertyInfo = "value"
    type AttrOrigin SpinButtonValuePropertyInfo = SpinButton
    attrGet = getSpinButtonValue
    attrSet = setSpinButtonValue
    attrTransfer _ v = do
        return v
    attrConstruct = constructSpinButtonValue
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton.value"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#g:attr:value"
        })
#endif

-- VVV Prop "wrap"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@wrap@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' spinButton #wrap
-- @
getSpinButtonWrap :: (MonadIO m, IsSpinButton o) => o -> m Bool
getSpinButtonWrap obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "wrap"

-- | Set the value of the “@wrap@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' spinButton [ #wrap 'Data.GI.Base.Attributes.:=' value ]
-- @
setSpinButtonWrap :: (MonadIO m, IsSpinButton o) => o -> Bool -> m ()
setSpinButtonWrap obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "wrap" val

-- | Construct a `GValueConstruct` with valid value for the “@wrap@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSpinButtonWrap :: (IsSpinButton o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSpinButtonWrap val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "wrap" val

#if defined(ENABLE_OVERLOADING)
data SpinButtonWrapPropertyInfo
instance AttrInfo SpinButtonWrapPropertyInfo where
    type AttrAllowedOps SpinButtonWrapPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SpinButtonWrapPropertyInfo = IsSpinButton
    type AttrSetTypeConstraint SpinButtonWrapPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SpinButtonWrapPropertyInfo = (~) Bool
    type AttrTransferType SpinButtonWrapPropertyInfo = Bool
    type AttrGetType SpinButtonWrapPropertyInfo = Bool
    type AttrLabel SpinButtonWrapPropertyInfo = "wrap"
    type AttrOrigin SpinButtonWrapPropertyInfo = SpinButton
    attrGet = getSpinButtonWrap
    attrSet = setSpinButtonWrap
    attrTransfer _ v = do
        return v
    attrConstruct = constructSpinButtonWrap
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton.wrap"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#g:attr:wrap"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList SpinButton
type instance O.AttributeList SpinButton = SpinButtonAttributeList
type SpinButtonAttributeList = ('[ '("activatesDefault", Gtk.Entry.EntryActivatesDefaultPropertyInfo), '("adjustment", SpinButtonAdjustmentPropertyInfo), '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("attributes", Gtk.Entry.EntryAttributesPropertyInfo), '("buffer", Gtk.Entry.EntryBufferPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("capsLockWarning", Gtk.Entry.EntryCapsLockWarningPropertyInfo), '("climbRate", SpinButtonClimbRatePropertyInfo), '("completion", Gtk.Entry.EntryCompletionPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("cursorPosition", Gtk.Entry.EntryCursorPositionPropertyInfo), '("digits", SpinButtonDigitsPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("editable", Gtk.Entry.EntryEditablePropertyInfo), '("editingCanceled", Gtk.CellEditable.CellEditableEditingCanceledPropertyInfo), '("enableEmojiCompletion", Gtk.Entry.EntryEnableEmojiCompletionPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasFrame", Gtk.Entry.EntryHasFramePropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("imModule", Gtk.Entry.EntryImModulePropertyInfo), '("innerBorder", Gtk.Entry.EntryInnerBorderPropertyInfo), '("inputHints", Gtk.Entry.EntryInputHintsPropertyInfo), '("inputPurpose", Gtk.Entry.EntryInputPurposePropertyInfo), '("invisibleChar", Gtk.Entry.EntryInvisibleCharPropertyInfo), '("invisibleCharSet", Gtk.Entry.EntryInvisibleCharSetPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("maxLength", Gtk.Entry.EntryMaxLengthPropertyInfo), '("maxWidthChars", Gtk.Entry.EntryMaxWidthCharsPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("numeric", SpinButtonNumericPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("orientation", Gtk.Orientable.OrientableOrientationPropertyInfo), '("overwriteMode", Gtk.Entry.EntryOverwriteModePropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("placeholderText", Gtk.Entry.EntryPlaceholderTextPropertyInfo), '("populateAll", Gtk.Entry.EntryPopulateAllPropertyInfo), '("primaryIconActivatable", Gtk.Entry.EntryPrimaryIconActivatablePropertyInfo), '("primaryIconGicon", Gtk.Entry.EntryPrimaryIconGiconPropertyInfo), '("primaryIconName", Gtk.Entry.EntryPrimaryIconNamePropertyInfo), '("primaryIconPixbuf", Gtk.Entry.EntryPrimaryIconPixbufPropertyInfo), '("primaryIconSensitive", Gtk.Entry.EntryPrimaryIconSensitivePropertyInfo), '("primaryIconStock", Gtk.Entry.EntryPrimaryIconStockPropertyInfo), '("primaryIconStorageType", Gtk.Entry.EntryPrimaryIconStorageTypePropertyInfo), '("primaryIconTooltipMarkup", Gtk.Entry.EntryPrimaryIconTooltipMarkupPropertyInfo), '("primaryIconTooltipText", Gtk.Entry.EntryPrimaryIconTooltipTextPropertyInfo), '("progressFraction", Gtk.Entry.EntryProgressFractionPropertyInfo), '("progressPulseStep", Gtk.Entry.EntryProgressPulseStepPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("scrollOffset", Gtk.Entry.EntryScrollOffsetPropertyInfo), '("secondaryIconActivatable", Gtk.Entry.EntrySecondaryIconActivatablePropertyInfo), '("secondaryIconGicon", Gtk.Entry.EntrySecondaryIconGiconPropertyInfo), '("secondaryIconName", Gtk.Entry.EntrySecondaryIconNamePropertyInfo), '("secondaryIconPixbuf", Gtk.Entry.EntrySecondaryIconPixbufPropertyInfo), '("secondaryIconSensitive", Gtk.Entry.EntrySecondaryIconSensitivePropertyInfo), '("secondaryIconStock", Gtk.Entry.EntrySecondaryIconStockPropertyInfo), '("secondaryIconStorageType", Gtk.Entry.EntrySecondaryIconStorageTypePropertyInfo), '("secondaryIconTooltipMarkup", Gtk.Entry.EntrySecondaryIconTooltipMarkupPropertyInfo), '("secondaryIconTooltipText", Gtk.Entry.EntrySecondaryIconTooltipTextPropertyInfo), '("selectionBound", Gtk.Entry.EntrySelectionBoundPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("shadowType", Gtk.Entry.EntryShadowTypePropertyInfo), '("showEmojiIcon", Gtk.Entry.EntryShowEmojiIconPropertyInfo), '("snapToTicks", SpinButtonSnapToTicksPropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tabs", Gtk.Entry.EntryTabsPropertyInfo), '("text", Gtk.Entry.EntryTextPropertyInfo), '("textLength", Gtk.Entry.EntryTextLengthPropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("truncateMultiline", Gtk.Entry.EntryTruncateMultilinePropertyInfo), '("updatePolicy", SpinButtonUpdatePolicyPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("value", SpinButtonValuePropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visibility", Gtk.Entry.EntryVisibilityPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthChars", Gtk.Entry.EntryWidthCharsPropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo), '("wrap", SpinButtonWrapPropertyInfo), '("xalign", Gtk.Entry.EntryXalignPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
spinButtonAdjustment :: AttrLabelProxy "adjustment"
spinButtonAdjustment = AttrLabelProxy

spinButtonClimbRate :: AttrLabelProxy "climbRate"
spinButtonClimbRate = AttrLabelProxy

spinButtonDigits :: AttrLabelProxy "digits"
spinButtonDigits = AttrLabelProxy

spinButtonNumeric :: AttrLabelProxy "numeric"
spinButtonNumeric = AttrLabelProxy

spinButtonSnapToTicks :: AttrLabelProxy "snapToTicks"
spinButtonSnapToTicks = AttrLabelProxy

spinButtonUpdatePolicy :: AttrLabelProxy "updatePolicy"
spinButtonUpdatePolicy = AttrLabelProxy

spinButtonValue :: AttrLabelProxy "value"
spinButtonValue = AttrLabelProxy

spinButtonWrap :: AttrLabelProxy "wrap"
spinButtonWrap = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList SpinButton = SpinButtonSignalList
type SpinButtonSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("activate", Gtk.Entry.EntryActivateSignalInfo), '("backspace", Gtk.Entry.EntryBackspaceSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("changeValue", SpinButtonChangeValueSignalInfo), '("changed", Gtk.Editable.EditableChangedSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("copyClipboard", Gtk.Entry.EntryCopyClipboardSignalInfo), '("cutClipboard", Gtk.Entry.EntryCutClipboardSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("deleteFromCursor", Gtk.Entry.EntryDeleteFromCursorSignalInfo), '("deleteText", Gtk.Editable.EditableDeleteTextSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("editingDone", Gtk.CellEditable.CellEditableEditingDoneSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("iconPress", Gtk.Entry.EntryIconPressSignalInfo), '("iconRelease", Gtk.Entry.EntryIconReleaseSignalInfo), '("input", SpinButtonInputSignalInfo), '("insertAtCursor", Gtk.Entry.EntryInsertAtCursorSignalInfo), '("insertEmoji", Gtk.Entry.EntryInsertEmojiSignalInfo), '("insertText", Gtk.Editable.EditableInsertTextSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveCursor", Gtk.Entry.EntryMoveCursorSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("output", SpinButtonOutputSignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("pasteClipboard", Gtk.Entry.EntryPasteClipboardSignalInfo), '("populatePopup", Gtk.Entry.EntryPopulatePopupSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("preeditChanged", Gtk.Entry.EntryPreeditChangedSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("removeWidget", Gtk.CellEditable.CellEditableRemoveWidgetSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("toggleOverwrite", Gtk.Entry.EntryToggleOverwriteSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("valueChanged", SpinButtonValueChangedSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo), '("wrapped", SpinButtonWrappedSignalInfo)] :: [(Symbol, *)])

#endif

-- method SpinButton::new
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "adjustment"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Adjustment" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the #GtkAdjustment object that this spin\n    button should use, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "climb_rate"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "specifies by how much the rate of change in the value will\n    accelerate if you continue to hold down an up/down button or arrow key"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "digits"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the number of decimal places to display"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "SpinButton" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_spin_button_new" gtk_spin_button_new :: 
    Ptr Gtk.Adjustment.Adjustment ->        -- adjustment : TInterface (Name {namespace = "Gtk", name = "Adjustment"})
    CDouble ->                              -- climb_rate : TBasicType TDouble
    Word32 ->                               -- digits : TBasicType TUInt
    IO (Ptr SpinButton)

-- | Creates a new t'GI.Gtk.Objects.SpinButton.SpinButton'.
spinButtonNew ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Adjustment.IsAdjustment a) =>
    Maybe (a)
    -- ^ /@adjustment@/: the t'GI.Gtk.Objects.Adjustment.Adjustment' object that this spin
    --     button should use, or 'P.Nothing'
    -> Double
    -- ^ /@climbRate@/: specifies by how much the rate of change in the value will
    --     accelerate if you continue to hold down an up\/down button or arrow key
    -> Word32
    -- ^ /@digits@/: the number of decimal places to display
    -> m SpinButton
    -- ^ __Returns:__ The new spin button as a t'GI.Gtk.Objects.Widget.Widget'
spinButtonNew adjustment climbRate digits = liftIO $ do
    maybeAdjustment <- case adjustment of
        Nothing -> return nullPtr
        Just jAdjustment -> do
            jAdjustment' <- unsafeManagedPtrCastPtr jAdjustment
            return jAdjustment'
    let climbRate' = realToFrac climbRate
    result <- gtk_spin_button_new maybeAdjustment climbRate' digits
    checkUnexpectedReturnNULL "spinButtonNew" result
    result' <- (newObject SpinButton) result
    whenJust adjustment touchManagedPtr
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method SpinButton::new_with_range
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "min"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Minimum allowable value"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "max"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Maximum allowable value"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "step"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "Increment added or subtracted by spinning the widget"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "SpinButton" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_spin_button_new_with_range" gtk_spin_button_new_with_range :: 
    CDouble ->                              -- min : TBasicType TDouble
    CDouble ->                              -- max : TBasicType TDouble
    CDouble ->                              -- step : TBasicType TDouble
    IO (Ptr SpinButton)

-- | This is a convenience constructor that allows creation of a numeric
-- t'GI.Gtk.Objects.SpinButton.SpinButton' without manually creating an adjustment. The value is
-- initially set to the minimum value and a page increment of 10 * /@step@/
-- is the default. The precision of the spin button is equivalent to the
-- precision of /@step@/.
-- 
-- Note that the way in which the precision is derived works best if /@step@/
-- is a power of ten. If the resulting precision is not suitable for your
-- needs, use 'GI.Gtk.Objects.SpinButton.spinButtonSetDigits' to correct it.
spinButtonNewWithRange ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Double
    -- ^ /@min@/: Minimum allowable value
    -> Double
    -- ^ /@max@/: Maximum allowable value
    -> Double
    -- ^ /@step@/: Increment added or subtracted by spinning the widget
    -> m SpinButton
    -- ^ __Returns:__ The new spin button as a t'GI.Gtk.Objects.Widget.Widget'
spinButtonNewWithRange min max step = liftIO $ do
    let min' = realToFrac min
    let max' = realToFrac max
    let step' = realToFrac step
    result <- gtk_spin_button_new_with_range min' max' step'
    checkUnexpectedReturnNULL "spinButtonNewWithRange" result
    result' <- (newObject SpinButton) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method SpinButton::configure
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "spin_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SpinButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSpinButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "adjustment"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Adjustment" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a #GtkAdjustment to replace the spin button\8217s\n    existing adjustment, or %NULL to leave its current adjustment unchanged"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "climb_rate"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the new climb rate" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "digits"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the number of decimal places to display in the spin button"
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

foreign import ccall "gtk_spin_button_configure" gtk_spin_button_configure :: 
    Ptr SpinButton ->                       -- spin_button : TInterface (Name {namespace = "Gtk", name = "SpinButton"})
    Ptr Gtk.Adjustment.Adjustment ->        -- adjustment : TInterface (Name {namespace = "Gtk", name = "Adjustment"})
    CDouble ->                              -- climb_rate : TBasicType TDouble
    Word32 ->                               -- digits : TBasicType TUInt
    IO ()

-- | Changes the properties of an existing spin button. The adjustment,
-- climb rate, and number of decimal places are updated accordingly.
spinButtonConfigure ::
    (B.CallStack.HasCallStack, MonadIO m, IsSpinButton a, Gtk.Adjustment.IsAdjustment b) =>
    a
    -- ^ /@spinButton@/: a t'GI.Gtk.Objects.SpinButton.SpinButton'
    -> Maybe (b)
    -- ^ /@adjustment@/: a t'GI.Gtk.Objects.Adjustment.Adjustment' to replace the spin button’s
    --     existing adjustment, or 'P.Nothing' to leave its current adjustment unchanged
    -> Double
    -- ^ /@climbRate@/: the new climb rate
    -> Word32
    -- ^ /@digits@/: the number of decimal places to display in the spin button
    -> m ()
spinButtonConfigure spinButton adjustment climbRate digits = liftIO $ do
    spinButton' <- unsafeManagedPtrCastPtr spinButton
    maybeAdjustment <- case adjustment of
        Nothing -> return nullPtr
        Just jAdjustment -> do
            jAdjustment' <- unsafeManagedPtrCastPtr jAdjustment
            return jAdjustment'
    let climbRate' = realToFrac climbRate
    gtk_spin_button_configure spinButton' maybeAdjustment climbRate' digits
    touchManagedPtr spinButton
    whenJust adjustment touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data SpinButtonConfigureMethodInfo
instance (signature ~ (Maybe (b) -> Double -> Word32 -> m ()), MonadIO m, IsSpinButton a, Gtk.Adjustment.IsAdjustment b) => O.OverloadedMethod SpinButtonConfigureMethodInfo a signature where
    overloadedMethod = spinButtonConfigure

instance O.OverloadedMethodInfo SpinButtonConfigureMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton.spinButtonConfigure",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#v:spinButtonConfigure"
        })


#endif

-- method SpinButton::get_adjustment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "spin_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SpinButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSpinButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Adjustment" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_spin_button_get_adjustment" gtk_spin_button_get_adjustment :: 
    Ptr SpinButton ->                       -- spin_button : TInterface (Name {namespace = "Gtk", name = "SpinButton"})
    IO (Ptr Gtk.Adjustment.Adjustment)

-- | Get the adjustment associated with a t'GI.Gtk.Objects.SpinButton.SpinButton'
spinButtonGetAdjustment ::
    (B.CallStack.HasCallStack, MonadIO m, IsSpinButton a) =>
    a
    -- ^ /@spinButton@/: a t'GI.Gtk.Objects.SpinButton.SpinButton'
    -> m Gtk.Adjustment.Adjustment
    -- ^ __Returns:__ the t'GI.Gtk.Objects.Adjustment.Adjustment' of /@spinButton@/
spinButtonGetAdjustment spinButton = liftIO $ do
    spinButton' <- unsafeManagedPtrCastPtr spinButton
    result <- gtk_spin_button_get_adjustment spinButton'
    checkUnexpectedReturnNULL "spinButtonGetAdjustment" result
    result' <- (newObject Gtk.Adjustment.Adjustment) result
    touchManagedPtr spinButton
    return result'

#if defined(ENABLE_OVERLOADING)
data SpinButtonGetAdjustmentMethodInfo
instance (signature ~ (m Gtk.Adjustment.Adjustment), MonadIO m, IsSpinButton a) => O.OverloadedMethod SpinButtonGetAdjustmentMethodInfo a signature where
    overloadedMethod = spinButtonGetAdjustment

instance O.OverloadedMethodInfo SpinButtonGetAdjustmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton.spinButtonGetAdjustment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#v:spinButtonGetAdjustment"
        })


#endif

-- method SpinButton::get_digits
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "spin_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SpinButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSpinButton" , sinceVersion = Nothing }
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

foreign import ccall "gtk_spin_button_get_digits" gtk_spin_button_get_digits :: 
    Ptr SpinButton ->                       -- spin_button : TInterface (Name {namespace = "Gtk", name = "SpinButton"})
    IO Word32

-- | Fetches the precision of /@spinButton@/. See 'GI.Gtk.Objects.SpinButton.spinButtonSetDigits'.
spinButtonGetDigits ::
    (B.CallStack.HasCallStack, MonadIO m, IsSpinButton a) =>
    a
    -- ^ /@spinButton@/: a t'GI.Gtk.Objects.SpinButton.SpinButton'
    -> m Word32
    -- ^ __Returns:__ the current precision
spinButtonGetDigits spinButton = liftIO $ do
    spinButton' <- unsafeManagedPtrCastPtr spinButton
    result <- gtk_spin_button_get_digits spinButton'
    touchManagedPtr spinButton
    return result

#if defined(ENABLE_OVERLOADING)
data SpinButtonGetDigitsMethodInfo
instance (signature ~ (m Word32), MonadIO m, IsSpinButton a) => O.OverloadedMethod SpinButtonGetDigitsMethodInfo a signature where
    overloadedMethod = spinButtonGetDigits

instance O.OverloadedMethodInfo SpinButtonGetDigitsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton.spinButtonGetDigits",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#v:spinButtonGetDigits"
        })


#endif

-- method SpinButton::get_increments
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "spin_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SpinButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSpinButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "step"
--           , argType = TBasicType TDouble
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "location to store step increment, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "page"
--           , argType = TBasicType TDouble
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "location to store page increment, or %NULL"
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

foreign import ccall "gtk_spin_button_get_increments" gtk_spin_button_get_increments :: 
    Ptr SpinButton ->                       -- spin_button : TInterface (Name {namespace = "Gtk", name = "SpinButton"})
    Ptr CDouble ->                          -- step : TBasicType TDouble
    Ptr CDouble ->                          -- page : TBasicType TDouble
    IO ()

-- | Gets the current step and page the increments used by /@spinButton@/. See
-- 'GI.Gtk.Objects.SpinButton.spinButtonSetIncrements'.
spinButtonGetIncrements ::
    (B.CallStack.HasCallStack, MonadIO m, IsSpinButton a) =>
    a
    -- ^ /@spinButton@/: a t'GI.Gtk.Objects.SpinButton.SpinButton'
    -> m ((Double, Double))
spinButtonGetIncrements spinButton = liftIO $ do
    spinButton' <- unsafeManagedPtrCastPtr spinButton
    step <- allocMem :: IO (Ptr CDouble)
    page <- allocMem :: IO (Ptr CDouble)
    gtk_spin_button_get_increments spinButton' step page
    step' <- peek step
    let step'' = realToFrac step'
    page' <- peek page
    let page'' = realToFrac page'
    touchManagedPtr spinButton
    freeMem step
    freeMem page
    return (step'', page'')

#if defined(ENABLE_OVERLOADING)
data SpinButtonGetIncrementsMethodInfo
instance (signature ~ (m ((Double, Double))), MonadIO m, IsSpinButton a) => O.OverloadedMethod SpinButtonGetIncrementsMethodInfo a signature where
    overloadedMethod = spinButtonGetIncrements

instance O.OverloadedMethodInfo SpinButtonGetIncrementsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton.spinButtonGetIncrements",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#v:spinButtonGetIncrements"
        })


#endif

-- method SpinButton::get_numeric
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "spin_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SpinButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSpinButton" , sinceVersion = Nothing }
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

foreign import ccall "gtk_spin_button_get_numeric" gtk_spin_button_get_numeric :: 
    Ptr SpinButton ->                       -- spin_button : TInterface (Name {namespace = "Gtk", name = "SpinButton"})
    IO CInt

-- | Returns whether non-numeric text can be typed into the spin button.
-- See 'GI.Gtk.Objects.SpinButton.spinButtonSetNumeric'.
spinButtonGetNumeric ::
    (B.CallStack.HasCallStack, MonadIO m, IsSpinButton a) =>
    a
    -- ^ /@spinButton@/: a t'GI.Gtk.Objects.SpinButton.SpinButton'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if only numeric text can be entered
spinButtonGetNumeric spinButton = liftIO $ do
    spinButton' <- unsafeManagedPtrCastPtr spinButton
    result <- gtk_spin_button_get_numeric spinButton'
    let result' = (/= 0) result
    touchManagedPtr spinButton
    return result'

#if defined(ENABLE_OVERLOADING)
data SpinButtonGetNumericMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsSpinButton a) => O.OverloadedMethod SpinButtonGetNumericMethodInfo a signature where
    overloadedMethod = spinButtonGetNumeric

instance O.OverloadedMethodInfo SpinButtonGetNumericMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton.spinButtonGetNumeric",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#v:spinButtonGetNumeric"
        })


#endif

-- method SpinButton::get_range
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "spin_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SpinButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSpinButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "min"
--           , argType = TBasicType TDouble
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location to store minimum allowed value, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "max"
--           , argType = TBasicType TDouble
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location to store maximum allowed value, or %NULL"
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

foreign import ccall "gtk_spin_button_get_range" gtk_spin_button_get_range :: 
    Ptr SpinButton ->                       -- spin_button : TInterface (Name {namespace = "Gtk", name = "SpinButton"})
    Ptr CDouble ->                          -- min : TBasicType TDouble
    Ptr CDouble ->                          -- max : TBasicType TDouble
    IO ()

-- | Gets the range allowed for /@spinButton@/.
-- See 'GI.Gtk.Objects.SpinButton.spinButtonSetRange'.
spinButtonGetRange ::
    (B.CallStack.HasCallStack, MonadIO m, IsSpinButton a) =>
    a
    -- ^ /@spinButton@/: a t'GI.Gtk.Objects.SpinButton.SpinButton'
    -> m ((Double, Double))
spinButtonGetRange spinButton = liftIO $ do
    spinButton' <- unsafeManagedPtrCastPtr spinButton
    min <- allocMem :: IO (Ptr CDouble)
    max <- allocMem :: IO (Ptr CDouble)
    gtk_spin_button_get_range spinButton' min max
    min' <- peek min
    let min'' = realToFrac min'
    max' <- peek max
    let max'' = realToFrac max'
    touchManagedPtr spinButton
    freeMem min
    freeMem max
    return (min'', max'')

#if defined(ENABLE_OVERLOADING)
data SpinButtonGetRangeMethodInfo
instance (signature ~ (m ((Double, Double))), MonadIO m, IsSpinButton a) => O.OverloadedMethod SpinButtonGetRangeMethodInfo a signature where
    overloadedMethod = spinButtonGetRange

instance O.OverloadedMethodInfo SpinButtonGetRangeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton.spinButtonGetRange",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#v:spinButtonGetRange"
        })


#endif

-- method SpinButton::get_snap_to_ticks
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "spin_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SpinButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSpinButton" , sinceVersion = Nothing }
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

foreign import ccall "gtk_spin_button_get_snap_to_ticks" gtk_spin_button_get_snap_to_ticks :: 
    Ptr SpinButton ->                       -- spin_button : TInterface (Name {namespace = "Gtk", name = "SpinButton"})
    IO CInt

-- | Returns whether the values are corrected to the nearest step.
-- See 'GI.Gtk.Objects.SpinButton.spinButtonSetSnapToTicks'.
spinButtonGetSnapToTicks ::
    (B.CallStack.HasCallStack, MonadIO m, IsSpinButton a) =>
    a
    -- ^ /@spinButton@/: a t'GI.Gtk.Objects.SpinButton.SpinButton'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if values are snapped to the nearest step
spinButtonGetSnapToTicks spinButton = liftIO $ do
    spinButton' <- unsafeManagedPtrCastPtr spinButton
    result <- gtk_spin_button_get_snap_to_ticks spinButton'
    let result' = (/= 0) result
    touchManagedPtr spinButton
    return result'

#if defined(ENABLE_OVERLOADING)
data SpinButtonGetSnapToTicksMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsSpinButton a) => O.OverloadedMethod SpinButtonGetSnapToTicksMethodInfo a signature where
    overloadedMethod = spinButtonGetSnapToTicks

instance O.OverloadedMethodInfo SpinButtonGetSnapToTicksMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton.spinButtonGetSnapToTicks",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#v:spinButtonGetSnapToTicks"
        })


#endif

-- method SpinButton::get_update_policy
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "spin_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SpinButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSpinButton" , sinceVersion = Nothing }
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
--                  Name { namespace = "Gtk" , name = "SpinButtonUpdatePolicy" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_spin_button_get_update_policy" gtk_spin_button_get_update_policy :: 
    Ptr SpinButton ->                       -- spin_button : TInterface (Name {namespace = "Gtk", name = "SpinButton"})
    IO CUInt

-- | Gets the update behavior of a spin button.
-- See 'GI.Gtk.Objects.SpinButton.spinButtonSetUpdatePolicy'.
spinButtonGetUpdatePolicy ::
    (B.CallStack.HasCallStack, MonadIO m, IsSpinButton a) =>
    a
    -- ^ /@spinButton@/: a t'GI.Gtk.Objects.SpinButton.SpinButton'
    -> m Gtk.Enums.SpinButtonUpdatePolicy
    -- ^ __Returns:__ the current update policy
spinButtonGetUpdatePolicy spinButton = liftIO $ do
    spinButton' <- unsafeManagedPtrCastPtr spinButton
    result <- gtk_spin_button_get_update_policy spinButton'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr spinButton
    return result'

#if defined(ENABLE_OVERLOADING)
data SpinButtonGetUpdatePolicyMethodInfo
instance (signature ~ (m Gtk.Enums.SpinButtonUpdatePolicy), MonadIO m, IsSpinButton a) => O.OverloadedMethod SpinButtonGetUpdatePolicyMethodInfo a signature where
    overloadedMethod = spinButtonGetUpdatePolicy

instance O.OverloadedMethodInfo SpinButtonGetUpdatePolicyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton.spinButtonGetUpdatePolicy",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#v:spinButtonGetUpdatePolicy"
        })


#endif

-- method SpinButton::get_value
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "spin_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SpinButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSpinButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TDouble)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_spin_button_get_value" gtk_spin_button_get_value :: 
    Ptr SpinButton ->                       -- spin_button : TInterface (Name {namespace = "Gtk", name = "SpinButton"})
    IO CDouble

-- | Get the value in the /@spinButton@/.
spinButtonGetValue ::
    (B.CallStack.HasCallStack, MonadIO m, IsSpinButton a) =>
    a
    -- ^ /@spinButton@/: a t'GI.Gtk.Objects.SpinButton.SpinButton'
    -> m Double
    -- ^ __Returns:__ the value of /@spinButton@/
spinButtonGetValue spinButton = liftIO $ do
    spinButton' <- unsafeManagedPtrCastPtr spinButton
    result <- gtk_spin_button_get_value spinButton'
    let result' = realToFrac result
    touchManagedPtr spinButton
    return result'

#if defined(ENABLE_OVERLOADING)
data SpinButtonGetValueMethodInfo
instance (signature ~ (m Double), MonadIO m, IsSpinButton a) => O.OverloadedMethod SpinButtonGetValueMethodInfo a signature where
    overloadedMethod = spinButtonGetValue

instance O.OverloadedMethodInfo SpinButtonGetValueMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton.spinButtonGetValue",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#v:spinButtonGetValue"
        })


#endif

-- method SpinButton::get_value_as_int
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "spin_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SpinButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSpinButton" , sinceVersion = Nothing }
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

foreign import ccall "gtk_spin_button_get_value_as_int" gtk_spin_button_get_value_as_int :: 
    Ptr SpinButton ->                       -- spin_button : TInterface (Name {namespace = "Gtk", name = "SpinButton"})
    IO Int32

-- | Get the value /@spinButton@/ represented as an integer.
spinButtonGetValueAsInt ::
    (B.CallStack.HasCallStack, MonadIO m, IsSpinButton a) =>
    a
    -- ^ /@spinButton@/: a t'GI.Gtk.Objects.SpinButton.SpinButton'
    -> m Int32
    -- ^ __Returns:__ the value of /@spinButton@/
spinButtonGetValueAsInt spinButton = liftIO $ do
    spinButton' <- unsafeManagedPtrCastPtr spinButton
    result <- gtk_spin_button_get_value_as_int spinButton'
    touchManagedPtr spinButton
    return result

#if defined(ENABLE_OVERLOADING)
data SpinButtonGetValueAsIntMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsSpinButton a) => O.OverloadedMethod SpinButtonGetValueAsIntMethodInfo a signature where
    overloadedMethod = spinButtonGetValueAsInt

instance O.OverloadedMethodInfo SpinButtonGetValueAsIntMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton.spinButtonGetValueAsInt",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#v:spinButtonGetValueAsInt"
        })


#endif

-- method SpinButton::get_wrap
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "spin_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SpinButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSpinButton" , sinceVersion = Nothing }
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

foreign import ccall "gtk_spin_button_get_wrap" gtk_spin_button_get_wrap :: 
    Ptr SpinButton ->                       -- spin_button : TInterface (Name {namespace = "Gtk", name = "SpinButton"})
    IO CInt

-- | Returns whether the spin button’s value wraps around to the
-- opposite limit when the upper or lower limit of the range is
-- exceeded. See 'GI.Gtk.Objects.SpinButton.spinButtonSetWrap'.
spinButtonGetWrap ::
    (B.CallStack.HasCallStack, MonadIO m, IsSpinButton a) =>
    a
    -- ^ /@spinButton@/: a t'GI.Gtk.Objects.SpinButton.SpinButton'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the spin button wraps around
spinButtonGetWrap spinButton = liftIO $ do
    spinButton' <- unsafeManagedPtrCastPtr spinButton
    result <- gtk_spin_button_get_wrap spinButton'
    let result' = (/= 0) result
    touchManagedPtr spinButton
    return result'

#if defined(ENABLE_OVERLOADING)
data SpinButtonGetWrapMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsSpinButton a) => O.OverloadedMethod SpinButtonGetWrapMethodInfo a signature where
    overloadedMethod = spinButtonGetWrap

instance O.OverloadedMethodInfo SpinButtonGetWrapMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton.spinButtonGetWrap",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#v:spinButtonGetWrap"
        })


#endif

-- method SpinButton::set_adjustment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "spin_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SpinButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSpinButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "adjustment"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Adjustment" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GtkAdjustment to replace the existing adjustment"
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

foreign import ccall "gtk_spin_button_set_adjustment" gtk_spin_button_set_adjustment :: 
    Ptr SpinButton ->                       -- spin_button : TInterface (Name {namespace = "Gtk", name = "SpinButton"})
    Ptr Gtk.Adjustment.Adjustment ->        -- adjustment : TInterface (Name {namespace = "Gtk", name = "Adjustment"})
    IO ()

-- | Replaces the t'GI.Gtk.Objects.Adjustment.Adjustment' associated with /@spinButton@/.
spinButtonSetAdjustment ::
    (B.CallStack.HasCallStack, MonadIO m, IsSpinButton a, Gtk.Adjustment.IsAdjustment b) =>
    a
    -- ^ /@spinButton@/: a t'GI.Gtk.Objects.SpinButton.SpinButton'
    -> b
    -- ^ /@adjustment@/: a t'GI.Gtk.Objects.Adjustment.Adjustment' to replace the existing adjustment
    -> m ()
spinButtonSetAdjustment spinButton adjustment = liftIO $ do
    spinButton' <- unsafeManagedPtrCastPtr spinButton
    adjustment' <- unsafeManagedPtrCastPtr adjustment
    gtk_spin_button_set_adjustment spinButton' adjustment'
    touchManagedPtr spinButton
    touchManagedPtr adjustment
    return ()

#if defined(ENABLE_OVERLOADING)
data SpinButtonSetAdjustmentMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsSpinButton a, Gtk.Adjustment.IsAdjustment b) => O.OverloadedMethod SpinButtonSetAdjustmentMethodInfo a signature where
    overloadedMethod = spinButtonSetAdjustment

instance O.OverloadedMethodInfo SpinButtonSetAdjustmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton.spinButtonSetAdjustment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#v:spinButtonSetAdjustment"
        })


#endif

-- method SpinButton::set_digits
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "spin_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SpinButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSpinButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "digits"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the number of digits after the decimal point to be displayed for the spin button\8217s value"
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

foreign import ccall "gtk_spin_button_set_digits" gtk_spin_button_set_digits :: 
    Ptr SpinButton ->                       -- spin_button : TInterface (Name {namespace = "Gtk", name = "SpinButton"})
    Word32 ->                               -- digits : TBasicType TUInt
    IO ()

-- | Set the precision to be displayed by /@spinButton@/. Up to 20 digit precision
-- is allowed.
spinButtonSetDigits ::
    (B.CallStack.HasCallStack, MonadIO m, IsSpinButton a) =>
    a
    -- ^ /@spinButton@/: a t'GI.Gtk.Objects.SpinButton.SpinButton'
    -> Word32
    -- ^ /@digits@/: the number of digits after the decimal point to be displayed for the spin button’s value
    -> m ()
spinButtonSetDigits spinButton digits = liftIO $ do
    spinButton' <- unsafeManagedPtrCastPtr spinButton
    gtk_spin_button_set_digits spinButton' digits
    touchManagedPtr spinButton
    return ()

#if defined(ENABLE_OVERLOADING)
data SpinButtonSetDigitsMethodInfo
instance (signature ~ (Word32 -> m ()), MonadIO m, IsSpinButton a) => O.OverloadedMethod SpinButtonSetDigitsMethodInfo a signature where
    overloadedMethod = spinButtonSetDigits

instance O.OverloadedMethodInfo SpinButtonSetDigitsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton.spinButtonSetDigits",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#v:spinButtonSetDigits"
        })


#endif

-- method SpinButton::set_increments
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "spin_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SpinButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSpinButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "step"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "increment applied for a button 1 press."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "page"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "increment applied for a button 2 press."
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

foreign import ccall "gtk_spin_button_set_increments" gtk_spin_button_set_increments :: 
    Ptr SpinButton ->                       -- spin_button : TInterface (Name {namespace = "Gtk", name = "SpinButton"})
    CDouble ->                              -- step : TBasicType TDouble
    CDouble ->                              -- page : TBasicType TDouble
    IO ()

-- | Sets the step and page increments for spin_button.  This affects how
-- quickly the value changes when the spin button’s arrows are activated.
spinButtonSetIncrements ::
    (B.CallStack.HasCallStack, MonadIO m, IsSpinButton a) =>
    a
    -- ^ /@spinButton@/: a t'GI.Gtk.Objects.SpinButton.SpinButton'
    -> Double
    -- ^ /@step@/: increment applied for a button 1 press.
    -> Double
    -- ^ /@page@/: increment applied for a button 2 press.
    -> m ()
spinButtonSetIncrements spinButton step page = liftIO $ do
    spinButton' <- unsafeManagedPtrCastPtr spinButton
    let step' = realToFrac step
    let page' = realToFrac page
    gtk_spin_button_set_increments spinButton' step' page'
    touchManagedPtr spinButton
    return ()

#if defined(ENABLE_OVERLOADING)
data SpinButtonSetIncrementsMethodInfo
instance (signature ~ (Double -> Double -> m ()), MonadIO m, IsSpinButton a) => O.OverloadedMethod SpinButtonSetIncrementsMethodInfo a signature where
    overloadedMethod = spinButtonSetIncrements

instance O.OverloadedMethodInfo SpinButtonSetIncrementsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton.spinButtonSetIncrements",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#v:spinButtonSetIncrements"
        })


#endif

-- method SpinButton::set_numeric
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "spin_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SpinButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSpinButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "numeric"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "flag indicating if only numeric entry is allowed"
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

foreign import ccall "gtk_spin_button_set_numeric" gtk_spin_button_set_numeric :: 
    Ptr SpinButton ->                       -- spin_button : TInterface (Name {namespace = "Gtk", name = "SpinButton"})
    CInt ->                                 -- numeric : TBasicType TBoolean
    IO ()

-- | Sets the flag that determines if non-numeric text can be typed
-- into the spin button.
spinButtonSetNumeric ::
    (B.CallStack.HasCallStack, MonadIO m, IsSpinButton a) =>
    a
    -- ^ /@spinButton@/: a t'GI.Gtk.Objects.SpinButton.SpinButton'
    -> Bool
    -- ^ /@numeric@/: flag indicating if only numeric entry is allowed
    -> m ()
spinButtonSetNumeric spinButton numeric = liftIO $ do
    spinButton' <- unsafeManagedPtrCastPtr spinButton
    let numeric' = (fromIntegral . fromEnum) numeric
    gtk_spin_button_set_numeric spinButton' numeric'
    touchManagedPtr spinButton
    return ()

#if defined(ENABLE_OVERLOADING)
data SpinButtonSetNumericMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsSpinButton a) => O.OverloadedMethod SpinButtonSetNumericMethodInfo a signature where
    overloadedMethod = spinButtonSetNumeric

instance O.OverloadedMethodInfo SpinButtonSetNumericMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton.spinButtonSetNumeric",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#v:spinButtonSetNumeric"
        })


#endif

-- method SpinButton::set_range
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "spin_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SpinButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSpinButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "min"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "minimum allowable value"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "max"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "maximum allowable value"
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

foreign import ccall "gtk_spin_button_set_range" gtk_spin_button_set_range :: 
    Ptr SpinButton ->                       -- spin_button : TInterface (Name {namespace = "Gtk", name = "SpinButton"})
    CDouble ->                              -- min : TBasicType TDouble
    CDouble ->                              -- max : TBasicType TDouble
    IO ()

-- | Sets the minimum and maximum allowable values for /@spinButton@/.
-- 
-- If the current value is outside this range, it will be adjusted
-- to fit within the range, otherwise it will remain unchanged.
spinButtonSetRange ::
    (B.CallStack.HasCallStack, MonadIO m, IsSpinButton a) =>
    a
    -- ^ /@spinButton@/: a t'GI.Gtk.Objects.SpinButton.SpinButton'
    -> Double
    -- ^ /@min@/: minimum allowable value
    -> Double
    -- ^ /@max@/: maximum allowable value
    -> m ()
spinButtonSetRange spinButton min max = liftIO $ do
    spinButton' <- unsafeManagedPtrCastPtr spinButton
    let min' = realToFrac min
    let max' = realToFrac max
    gtk_spin_button_set_range spinButton' min' max'
    touchManagedPtr spinButton
    return ()

#if defined(ENABLE_OVERLOADING)
data SpinButtonSetRangeMethodInfo
instance (signature ~ (Double -> Double -> m ()), MonadIO m, IsSpinButton a) => O.OverloadedMethod SpinButtonSetRangeMethodInfo a signature where
    overloadedMethod = spinButtonSetRange

instance O.OverloadedMethodInfo SpinButtonSetRangeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton.spinButtonSetRange",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#v:spinButtonSetRange"
        })


#endif

-- method SpinButton::set_snap_to_ticks
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "spin_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SpinButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSpinButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "snap_to_ticks"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a flag indicating if invalid values should be corrected"
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

foreign import ccall "gtk_spin_button_set_snap_to_ticks" gtk_spin_button_set_snap_to_ticks :: 
    Ptr SpinButton ->                       -- spin_button : TInterface (Name {namespace = "Gtk", name = "SpinButton"})
    CInt ->                                 -- snap_to_ticks : TBasicType TBoolean
    IO ()

-- | Sets the policy as to whether values are corrected to the
-- nearest step increment when a spin button is activated after
-- providing an invalid value.
spinButtonSetSnapToTicks ::
    (B.CallStack.HasCallStack, MonadIO m, IsSpinButton a) =>
    a
    -- ^ /@spinButton@/: a t'GI.Gtk.Objects.SpinButton.SpinButton'
    -> Bool
    -- ^ /@snapToTicks@/: a flag indicating if invalid values should be corrected
    -> m ()
spinButtonSetSnapToTicks spinButton snapToTicks = liftIO $ do
    spinButton' <- unsafeManagedPtrCastPtr spinButton
    let snapToTicks' = (fromIntegral . fromEnum) snapToTicks
    gtk_spin_button_set_snap_to_ticks spinButton' snapToTicks'
    touchManagedPtr spinButton
    return ()

#if defined(ENABLE_OVERLOADING)
data SpinButtonSetSnapToTicksMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsSpinButton a) => O.OverloadedMethod SpinButtonSetSnapToTicksMethodInfo a signature where
    overloadedMethod = spinButtonSetSnapToTicks

instance O.OverloadedMethodInfo SpinButtonSetSnapToTicksMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton.spinButtonSetSnapToTicks",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#v:spinButtonSetSnapToTicks"
        })


#endif

-- method SpinButton::set_update_policy
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "spin_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SpinButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSpinButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "policy"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "SpinButtonUpdatePolicy" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSpinButtonUpdatePolicy value"
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

foreign import ccall "gtk_spin_button_set_update_policy" gtk_spin_button_set_update_policy :: 
    Ptr SpinButton ->                       -- spin_button : TInterface (Name {namespace = "Gtk", name = "SpinButton"})
    CUInt ->                                -- policy : TInterface (Name {namespace = "Gtk", name = "SpinButtonUpdatePolicy"})
    IO ()

-- | Sets the update behavior of a spin button.
-- This determines whether the spin button is always updated
-- or only when a valid value is set.
spinButtonSetUpdatePolicy ::
    (B.CallStack.HasCallStack, MonadIO m, IsSpinButton a) =>
    a
    -- ^ /@spinButton@/: a t'GI.Gtk.Objects.SpinButton.SpinButton'
    -> Gtk.Enums.SpinButtonUpdatePolicy
    -- ^ /@policy@/: a t'GI.Gtk.Enums.SpinButtonUpdatePolicy' value
    -> m ()
spinButtonSetUpdatePolicy spinButton policy = liftIO $ do
    spinButton' <- unsafeManagedPtrCastPtr spinButton
    let policy' = (fromIntegral . fromEnum) policy
    gtk_spin_button_set_update_policy spinButton' policy'
    touchManagedPtr spinButton
    return ()

#if defined(ENABLE_OVERLOADING)
data SpinButtonSetUpdatePolicyMethodInfo
instance (signature ~ (Gtk.Enums.SpinButtonUpdatePolicy -> m ()), MonadIO m, IsSpinButton a) => O.OverloadedMethod SpinButtonSetUpdatePolicyMethodInfo a signature where
    overloadedMethod = spinButtonSetUpdatePolicy

instance O.OverloadedMethodInfo SpinButtonSetUpdatePolicyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton.spinButtonSetUpdatePolicy",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#v:spinButtonSetUpdatePolicy"
        })


#endif

-- method SpinButton::set_value
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "spin_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SpinButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSpinButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "value"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the new value" , sinceVersion = Nothing }
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

foreign import ccall "gtk_spin_button_set_value" gtk_spin_button_set_value :: 
    Ptr SpinButton ->                       -- spin_button : TInterface (Name {namespace = "Gtk", name = "SpinButton"})
    CDouble ->                              -- value : TBasicType TDouble
    IO ()

-- | Sets the value of /@spinButton@/.
spinButtonSetValue ::
    (B.CallStack.HasCallStack, MonadIO m, IsSpinButton a) =>
    a
    -- ^ /@spinButton@/: a t'GI.Gtk.Objects.SpinButton.SpinButton'
    -> Double
    -- ^ /@value@/: the new value
    -> m ()
spinButtonSetValue spinButton value = liftIO $ do
    spinButton' <- unsafeManagedPtrCastPtr spinButton
    let value' = realToFrac value
    gtk_spin_button_set_value spinButton' value'
    touchManagedPtr spinButton
    return ()

#if defined(ENABLE_OVERLOADING)
data SpinButtonSetValueMethodInfo
instance (signature ~ (Double -> m ()), MonadIO m, IsSpinButton a) => O.OverloadedMethod SpinButtonSetValueMethodInfo a signature where
    overloadedMethod = spinButtonSetValue

instance O.OverloadedMethodInfo SpinButtonSetValueMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton.spinButtonSetValue",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#v:spinButtonSetValue"
        })


#endif

-- method SpinButton::set_wrap
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "spin_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SpinButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSpinButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "wrap"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a flag indicating if wrapping behavior is performed"
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

foreign import ccall "gtk_spin_button_set_wrap" gtk_spin_button_set_wrap :: 
    Ptr SpinButton ->                       -- spin_button : TInterface (Name {namespace = "Gtk", name = "SpinButton"})
    CInt ->                                 -- wrap : TBasicType TBoolean
    IO ()

-- | Sets the flag that determines if a spin button value wraps
-- around to the opposite limit when the upper or lower limit
-- of the range is exceeded.
spinButtonSetWrap ::
    (B.CallStack.HasCallStack, MonadIO m, IsSpinButton a) =>
    a
    -- ^ /@spinButton@/: a t'GI.Gtk.Objects.SpinButton.SpinButton'
    -> Bool
    -- ^ /@wrap@/: a flag indicating if wrapping behavior is performed
    -> m ()
spinButtonSetWrap spinButton wrap = liftIO $ do
    spinButton' <- unsafeManagedPtrCastPtr spinButton
    let wrap' = (fromIntegral . fromEnum) wrap
    gtk_spin_button_set_wrap spinButton' wrap'
    touchManagedPtr spinButton
    return ()

#if defined(ENABLE_OVERLOADING)
data SpinButtonSetWrapMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsSpinButton a) => O.OverloadedMethod SpinButtonSetWrapMethodInfo a signature where
    overloadedMethod = spinButtonSetWrap

instance O.OverloadedMethodInfo SpinButtonSetWrapMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton.spinButtonSetWrap",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#v:spinButtonSetWrap"
        })


#endif

-- method SpinButton::spin
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "spin_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SpinButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSpinButton" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "direction"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SpinType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GtkSpinType indicating the direction to spin"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "increment"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "step increment to apply in the specified direction"
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

foreign import ccall "gtk_spin_button_spin" gtk_spin_button_spin :: 
    Ptr SpinButton ->                       -- spin_button : TInterface (Name {namespace = "Gtk", name = "SpinButton"})
    CUInt ->                                -- direction : TInterface (Name {namespace = "Gtk", name = "SpinType"})
    CDouble ->                              -- increment : TBasicType TDouble
    IO ()

-- | Increment or decrement a spin button’s value in a specified
-- direction by a specified amount.
spinButtonSpin ::
    (B.CallStack.HasCallStack, MonadIO m, IsSpinButton a) =>
    a
    -- ^ /@spinButton@/: a t'GI.Gtk.Objects.SpinButton.SpinButton'
    -> Gtk.Enums.SpinType
    -- ^ /@direction@/: a t'GI.Gtk.Enums.SpinType' indicating the direction to spin
    -> Double
    -- ^ /@increment@/: step increment to apply in the specified direction
    -> m ()
spinButtonSpin spinButton direction increment = liftIO $ do
    spinButton' <- unsafeManagedPtrCastPtr spinButton
    let direction' = (fromIntegral . fromEnum) direction
    let increment' = realToFrac increment
    gtk_spin_button_spin spinButton' direction' increment'
    touchManagedPtr spinButton
    return ()

#if defined(ENABLE_OVERLOADING)
data SpinButtonSpinMethodInfo
instance (signature ~ (Gtk.Enums.SpinType -> Double -> m ()), MonadIO m, IsSpinButton a) => O.OverloadedMethod SpinButtonSpinMethodInfo a signature where
    overloadedMethod = spinButtonSpin

instance O.OverloadedMethodInfo SpinButtonSpinMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton.spinButtonSpin",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#v:spinButtonSpin"
        })


#endif

-- method SpinButton::update
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "spin_button"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SpinButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSpinButton" , sinceVersion = Nothing }
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

foreign import ccall "gtk_spin_button_update" gtk_spin_button_update :: 
    Ptr SpinButton ->                       -- spin_button : TInterface (Name {namespace = "Gtk", name = "SpinButton"})
    IO ()

-- | Manually force an update of the spin button.
spinButtonUpdate ::
    (B.CallStack.HasCallStack, MonadIO m, IsSpinButton a) =>
    a
    -- ^ /@spinButton@/: a t'GI.Gtk.Objects.SpinButton.SpinButton'
    -> m ()
spinButtonUpdate spinButton = liftIO $ do
    spinButton' <- unsafeManagedPtrCastPtr spinButton
    gtk_spin_button_update spinButton'
    touchManagedPtr spinButton
    return ()

#if defined(ENABLE_OVERLOADING)
data SpinButtonUpdateMethodInfo
instance (signature ~ (m ()), MonadIO m, IsSpinButton a) => O.OverloadedMethod SpinButtonUpdateMethodInfo a signature where
    overloadedMethod = spinButtonUpdate

instance O.OverloadedMethodInfo SpinButtonUpdateMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SpinButton.spinButtonUpdate",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SpinButton.html#v:spinButtonUpdate"
        })


#endif


