{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- The t'GI.Gtk.Objects.Image.Image' widget displays an image. Various kinds of object
-- can be displayed as an image; most typically, you would load a
-- t'GI.GdkPixbuf.Objects.Pixbuf.Pixbuf' (\"pixel buffer\") from a file, and then display that.
-- There’s a convenience function to do this, 'GI.Gtk.Objects.Image.imageNewFromFile',
-- used as follows:
-- 
-- === /C code/
-- >
-- >  GtkWidget *image;
-- >  image = gtk_image_new_from_file ("myfile.png");
-- 
-- If the file isn’t loaded successfully, the image will contain a
-- “broken image” icon similar to that used in many web browsers.
-- If you want to handle errors in loading the file yourself,
-- for example by displaying an error message, then load the image with
-- 'GI.GdkPixbuf.Objects.Pixbuf.pixbufNewFromFile', then create the t'GI.Gtk.Objects.Image.Image' with
-- 'GI.Gtk.Objects.Image.imageNewFromPixbuf'.
-- 
-- The image file may contain an animation, if so the t'GI.Gtk.Objects.Image.Image' will
-- display an animation (t'GI.GdkPixbuf.Objects.PixbufAnimation.PixbufAnimation') instead of a static image.
-- 
-- t'GI.Gtk.Objects.Image.Image' is a subclass of t'GI.Gtk.Objects.Misc.Misc', which implies that you can
-- align it (center, left, right) and add padding to it, using
-- t'GI.Gtk.Objects.Misc.Misc' methods.
-- 
-- t'GI.Gtk.Objects.Image.Image' is a “no window” widget (has no t'GI.Gdk.Objects.Window.Window' of its own),
-- so by default does not receive events. If you want to receive events
-- on the image, such as button clicks, place the image inside a
-- t'GI.Gtk.Objects.EventBox.EventBox', then connect to the event signals on the event box.
-- 
-- ## Handling button press events on a t'GI.Gtk.Objects.Image.Image'.
-- 
-- 
-- === /C code/
-- >
-- >  static gboolean
-- >  button_press_callback (GtkWidget      *event_box,
-- >                         GdkEventButton *event,
-- >                         gpointer        data)
-- >  {
-- >    g_print ("Event box clicked at coordinates %f,%f\n",
-- >             event->x, event->y);
-- >
-- >    // Returning TRUE means we handled the event, so the signal
-- >    // emission should be stopped (don’t call any further callbacks
-- >    // that may be connected). Return FALSE to continue invoking callbacks.
-- >    return TRUE;
-- >  }
-- >
-- >  static GtkWidget*
-- >  create_image (void)
-- >  {
-- >    GtkWidget *image;
-- >    GtkWidget *event_box;
-- >
-- >    image = gtk_image_new_from_file ("myfile.png");
-- >
-- >    event_box = gtk_event_box_new ();
-- >
-- >    gtk_container_add (GTK_CONTAINER (event_box), image);
-- >
-- >    g_signal_connect (G_OBJECT (event_box),
-- >                      "button_press_event",
-- >                      G_CALLBACK (button_press_callback),
-- >                      image);
-- >
-- >    return image;
-- >  }
-- 
-- 
-- When handling events on the event box, keep in mind that coordinates
-- in the image may be different from event box coordinates due to
-- the alignment and padding settings on the image (see t'GI.Gtk.Objects.Misc.Misc').
-- The simplest way to solve this is to set the alignment to 0.0
-- (left\/top), and set the padding to zero. Then the origin of
-- the image will be the same as the origin of the event box.
-- 
-- Sometimes an application will want to avoid depending on external data
-- files, such as image files. GTK+ comes with a program to avoid this,
-- called “gdk-pixbuf-csource”. This library
-- allows you to convert an image into a C variable declaration, which
-- can then be loaded into a t'GI.GdkPixbuf.Objects.Pixbuf.Pixbuf' using
-- 'GI.GdkPixbuf.Objects.Pixbuf.pixbufNewFromInline'.
-- 
-- = CSS nodes
-- 
-- GtkImage has a single CSS node with the name image. The style classes
-- may appear on image CSS nodes: .icon-dropshadow, .lowres-icon.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.Image
    ( 

-- * Exported types
    Image(..)                               ,
    IsImage                                 ,
    toImage                                 ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childNotify]("GI.Gtk.Objects.Widget#g:method:childNotify"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [clear]("GI.Gtk.Objects.Image#g:method:clear"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getAlignment]("GI.Gtk.Objects.Misc#g:method:getAlignment"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAnimation]("GI.Gtk.Objects.Image#g:method:getAnimation"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getGicon]("GI.Gtk.Objects.Image#g:method:getGicon"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getIconName]("GI.Gtk.Objects.Image#g:method:getIconName"), [getIconSet]("GI.Gtk.Objects.Image#g:method:getIconSet"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getPadding]("GI.Gtk.Objects.Misc#g:method:getPadding"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPixbuf]("GI.Gtk.Objects.Image#g:method:getPixbuf"), [getPixelSize]("GI.Gtk.Objects.Image#g:method:getPixelSize"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStock]("GI.Gtk.Objects.Image#g:method:getStock"), [getStorageType]("GI.Gtk.Objects.Image#g:method:getStorageType"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setAlignment]("GI.Gtk.Objects.Misc#g:method:setAlignment"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setFromAnimation]("GI.Gtk.Objects.Image#g:method:setFromAnimation"), [setFromFile]("GI.Gtk.Objects.Image#g:method:setFromFile"), [setFromGicon]("GI.Gtk.Objects.Image#g:method:setFromGicon"), [setFromIconName]("GI.Gtk.Objects.Image#g:method:setFromIconName"), [setFromIconSet]("GI.Gtk.Objects.Image#g:method:setFromIconSet"), [setFromPixbuf]("GI.Gtk.Objects.Image#g:method:setFromPixbuf"), [setFromResource]("GI.Gtk.Objects.Image#g:method:setFromResource"), [setFromStock]("GI.Gtk.Objects.Image#g:method:setFromStock"), [setFromSurface]("GI.Gtk.Objects.Image#g:method:setFromSurface"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setPadding]("GI.Gtk.Objects.Misc#g:method:setPadding"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setPixelSize]("GI.Gtk.Objects.Image#g:method:setPixelSize"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveImageMethod                      ,
#endif

-- ** clear #method:clear#

#if defined(ENABLE_OVERLOADING)
    ImageClearMethodInfo                    ,
#endif
    imageClear                              ,


-- ** getAnimation #method:getAnimation#

#if defined(ENABLE_OVERLOADING)
    ImageGetAnimationMethodInfo             ,
#endif
    imageGetAnimation                       ,


-- ** getGicon #method:getGicon#

#if defined(ENABLE_OVERLOADING)
    ImageGetGiconMethodInfo                 ,
#endif
    imageGetGicon                           ,


-- ** getIconName #method:getIconName#

#if defined(ENABLE_OVERLOADING)
    ImageGetIconNameMethodInfo              ,
#endif
    imageGetIconName                        ,


-- ** getIconSet #method:getIconSet#

#if defined(ENABLE_OVERLOADING)
    ImageGetIconSetMethodInfo               ,
#endif
    imageGetIconSet                         ,


-- ** getPixbuf #method:getPixbuf#

#if defined(ENABLE_OVERLOADING)
    ImageGetPixbufMethodInfo                ,
#endif
    imageGetPixbuf                          ,


-- ** getPixelSize #method:getPixelSize#

#if defined(ENABLE_OVERLOADING)
    ImageGetPixelSizeMethodInfo             ,
#endif
    imageGetPixelSize                       ,


-- ** getStock #method:getStock#

#if defined(ENABLE_OVERLOADING)
    ImageGetStockMethodInfo                 ,
#endif
    imageGetStock                           ,


-- ** getStorageType #method:getStorageType#

#if defined(ENABLE_OVERLOADING)
    ImageGetStorageTypeMethodInfo           ,
#endif
    imageGetStorageType                     ,


-- ** new #method:new#

    imageNew                                ,


-- ** newFromAnimation #method:newFromAnimation#

    imageNewFromAnimation                   ,


-- ** newFromFile #method:newFromFile#

    imageNewFromFile                        ,


-- ** newFromGicon #method:newFromGicon#

    imageNewFromGicon                       ,


-- ** newFromIconName #method:newFromIconName#

    imageNewFromIconName                    ,


-- ** newFromIconSet #method:newFromIconSet#

    imageNewFromIconSet                     ,


-- ** newFromPixbuf #method:newFromPixbuf#

    imageNewFromPixbuf                      ,


-- ** newFromResource #method:newFromResource#

    imageNewFromResource                    ,


-- ** newFromStock #method:newFromStock#

    imageNewFromStock                       ,


-- ** newFromSurface #method:newFromSurface#

    imageNewFromSurface                     ,


-- ** setFromAnimation #method:setFromAnimation#

#if defined(ENABLE_OVERLOADING)
    ImageSetFromAnimationMethodInfo         ,
#endif
    imageSetFromAnimation                   ,


-- ** setFromFile #method:setFromFile#

#if defined(ENABLE_OVERLOADING)
    ImageSetFromFileMethodInfo              ,
#endif
    imageSetFromFile                        ,


-- ** setFromGicon #method:setFromGicon#

#if defined(ENABLE_OVERLOADING)
    ImageSetFromGiconMethodInfo             ,
#endif
    imageSetFromGicon                       ,


-- ** setFromIconName #method:setFromIconName#

#if defined(ENABLE_OVERLOADING)
    ImageSetFromIconNameMethodInfo          ,
#endif
    imageSetFromIconName                    ,


-- ** setFromIconSet #method:setFromIconSet#

#if defined(ENABLE_OVERLOADING)
    ImageSetFromIconSetMethodInfo           ,
#endif
    imageSetFromIconSet                     ,


-- ** setFromPixbuf #method:setFromPixbuf#

#if defined(ENABLE_OVERLOADING)
    ImageSetFromPixbufMethodInfo            ,
#endif
    imageSetFromPixbuf                      ,


-- ** setFromResource #method:setFromResource#

#if defined(ENABLE_OVERLOADING)
    ImageSetFromResourceMethodInfo          ,
#endif
    imageSetFromResource                    ,


-- ** setFromStock #method:setFromStock#

#if defined(ENABLE_OVERLOADING)
    ImageSetFromStockMethodInfo             ,
#endif
    imageSetFromStock                       ,


-- ** setFromSurface #method:setFromSurface#

#if defined(ENABLE_OVERLOADING)
    ImageSetFromSurfaceMethodInfo           ,
#endif
    imageSetFromSurface                     ,


-- ** setPixelSize #method:setPixelSize#

#if defined(ENABLE_OVERLOADING)
    ImageSetPixelSizeMethodInfo             ,
#endif
    imageSetPixelSize                       ,




 -- * Properties


-- ** file #attr:file#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ImageFilePropertyInfo                   ,
#endif
    clearImageFile                          ,
    constructImageFile                      ,
    getImageFile                            ,
#if defined(ENABLE_OVERLOADING)
    imageFile                               ,
#endif
    setImageFile                            ,


-- ** gicon #attr:gicon#
-- | The GIcon displayed in the GtkImage. For themed icons,
-- If the icon theme is changed, the image will be updated
-- automatically.
-- 
-- /Since: 2.14/

#if defined(ENABLE_OVERLOADING)
    ImageGiconPropertyInfo                  ,
#endif
    clearImageGicon                         ,
    constructImageGicon                     ,
    getImageGicon                           ,
#if defined(ENABLE_OVERLOADING)
    imageGicon                              ,
#endif
    setImageGicon                           ,


-- ** iconName #attr:iconName#
-- | The name of the icon in the icon theme. If the icon theme is
-- changed, the image will be updated automatically.
-- 
-- /Since: 2.6/

#if defined(ENABLE_OVERLOADING)
    ImageIconNamePropertyInfo               ,
#endif
    clearImageIconName                      ,
    constructImageIconName                  ,
    getImageIconName                        ,
#if defined(ENABLE_OVERLOADING)
    imageIconName                           ,
#endif
    setImageIconName                        ,


-- ** iconSet #attr:iconSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ImageIconSetPropertyInfo                ,
#endif
    clearImageIconSet                       ,
    constructImageIconSet                   ,
    getImageIconSet                         ,
#if defined(ENABLE_OVERLOADING)
    imageIconSet                            ,
#endif
    setImageIconSet                         ,


-- ** iconSize #attr:iconSize#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ImageIconSizePropertyInfo               ,
#endif
    constructImageIconSize                  ,
    getImageIconSize                        ,
#if defined(ENABLE_OVERLOADING)
    imageIconSize                           ,
#endif
    setImageIconSize                        ,


-- ** pixbuf #attr:pixbuf#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ImagePixbufPropertyInfo                 ,
#endif
    clearImagePixbuf                        ,
    constructImagePixbuf                    ,
    getImagePixbuf                          ,
#if defined(ENABLE_OVERLOADING)
    imagePixbuf                             ,
#endif
    setImagePixbuf                          ,


-- ** pixbufAnimation #attr:pixbufAnimation#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ImagePixbufAnimationPropertyInfo        ,
#endif
    clearImagePixbufAnimation               ,
    constructImagePixbufAnimation           ,
    getImagePixbufAnimation                 ,
#if defined(ENABLE_OVERLOADING)
    imagePixbufAnimation                    ,
#endif
    setImagePixbufAnimation                 ,


-- ** pixelSize #attr:pixelSize#
-- | The \"pixel-size\" property can be used to specify a fixed size
-- overriding the [Image:iconSize]("GI.Gtk.Objects.Image#g:attr:iconSize") property for images of type
-- 'GI.Gtk.Enums.ImageTypeIconName'.
-- 
-- /Since: 2.6/

#if defined(ENABLE_OVERLOADING)
    ImagePixelSizePropertyInfo              ,
#endif
    constructImagePixelSize                 ,
    getImagePixelSize                       ,
#if defined(ENABLE_OVERLOADING)
    imagePixelSize                          ,
#endif
    setImagePixelSize                       ,


-- ** resource #attr:resource#
-- | A path to a resource file to display.
-- 
-- /Since: 3.8/

#if defined(ENABLE_OVERLOADING)
    ImageResourcePropertyInfo               ,
#endif
    clearImageResource                      ,
    constructImageResource                  ,
    getImageResource                        ,
#if defined(ENABLE_OVERLOADING)
    imageResource                           ,
#endif
    setImageResource                        ,


-- ** stock #attr:stock#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ImageStockPropertyInfo                  ,
#endif
    clearImageStock                         ,
    constructImageStock                     ,
    getImageStock                           ,
#if defined(ENABLE_OVERLOADING)
    imageStock                              ,
#endif
    setImageStock                           ,


-- ** storageType #attr:storageType#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ImageStorageTypePropertyInfo            ,
#endif
    getImageStorageType                     ,
#if defined(ENABLE_OVERLOADING)
    imageStorageType                        ,
#endif


-- ** surface #attr:surface#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ImageSurfacePropertyInfo                ,
#endif
    clearImageSurface                       ,
    constructImageSurface                   ,
    getImageSurface                         ,
#if defined(ENABLE_OVERLOADING)
    imageSurface                            ,
#endif
    setImageSurface                         ,


-- ** useFallback #attr:useFallback#
-- | Whether the icon displayed in the GtkImage will use
-- standard icon names fallback. The value of this property
-- is only relevant for images of type 'GI.Gtk.Enums.ImageTypeIconName'
-- and 'GI.Gtk.Enums.ImageTypeGicon'.
-- 
-- /Since: 3.0/

#if defined(ENABLE_OVERLOADING)
    ImageUseFallbackPropertyInfo            ,
#endif
    constructImageUseFallback               ,
    getImageUseFallback                     ,
#if defined(ENABLE_OVERLOADING)
    imageUseFallback                        ,
#endif
    setImageUseFallback                     ,




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
import qualified GI.Cairo.Structs.Surface as Cairo.Surface
import qualified GI.GObject.Objects.Object as GObject.Object
import qualified GI.GdkPixbuf.Objects.Pixbuf as GdkPixbuf.Pixbuf
import qualified GI.GdkPixbuf.Objects.PixbufAnimation as GdkPixbuf.PixbufAnimation
import qualified GI.Gio.Interfaces.Icon as Gio.Icon
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Objects.Misc as Gtk.Misc
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget
import {-# SOURCE #-} qualified GI.Gtk.Structs.IconSet as Gtk.IconSet

-- | Memory-managed wrapper type.
newtype Image = Image (SP.ManagedPtr Image)
    deriving (Eq)

instance SP.ManagedPtrNewtype Image where
    toManagedPtr (Image p) = p

foreign import ccall "gtk_image_get_type"
    c_gtk_image_get_type :: IO B.Types.GType

instance B.Types.TypedObject Image where
    glibType = c_gtk_image_get_type

instance B.Types.GObject Image

-- | Type class for types which can be safely cast to `Image`, for instance with `toImage`.
class (SP.GObject o, O.IsDescendantOf Image o) => IsImage o
instance (SP.GObject o, O.IsDescendantOf Image o) => IsImage o

instance O.HasParentTypes Image
type instance O.ParentTypes Image = '[Gtk.Misc.Misc, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable]

-- | Cast to `Image`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toImage :: (MIO.MonadIO m, IsImage o) => o -> m Image
toImage = MIO.liftIO . B.ManagedPtr.unsafeCastTo Image

-- | Convert 'Image' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe Image) where
    gvalueGType_ = c_gtk_image_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr Image)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr Image)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject Image ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveImageMethod (t :: Symbol) (o :: *) :: * where
    ResolveImageMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveImageMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveImageMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveImageMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveImageMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveImageMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveImageMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveImageMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveImageMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveImageMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveImageMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveImageMethod "childNotify" o = Gtk.Widget.WidgetChildNotifyMethodInfo
    ResolveImageMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveImageMethod "clear" o = ImageClearMethodInfo
    ResolveImageMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveImageMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveImageMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveImageMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveImageMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveImageMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveImageMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveImageMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveImageMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveImageMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveImageMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveImageMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveImageMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveImageMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveImageMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveImageMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveImageMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveImageMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveImageMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveImageMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveImageMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveImageMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveImageMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveImageMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveImageMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveImageMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveImageMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveImageMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveImageMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveImageMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveImageMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveImageMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveImageMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveImageMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveImageMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveImageMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveImageMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveImageMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveImageMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveImageMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveImageMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveImageMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveImageMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveImageMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveImageMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveImageMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveImageMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveImageMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveImageMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveImageMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveImageMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveImageMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveImageMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveImageMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveImageMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveImageMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveImageMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveImageMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveImageMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveImageMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveImageMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveImageMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveImageMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveImageMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveImageMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveImageMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveImageMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveImageMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveImageMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveImageMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveImageMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveImageMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveImageMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveImageMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveImageMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveImageMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveImageMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveImageMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveImageMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveImageMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveImageMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveImageMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveImageMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveImageMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveImageMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveImageMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveImageMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveImageMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveImageMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveImageMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveImageMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveImageMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveImageMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveImageMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveImageMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveImageMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveImageMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveImageMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveImageMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveImageMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveImageMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveImageMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveImageMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveImageMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveImageMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveImageMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveImageMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveImageMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveImageMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveImageMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveImageMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveImageMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveImageMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveImageMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveImageMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveImageMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveImageMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveImageMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveImageMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveImageMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveImageMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveImageMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveImageMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveImageMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveImageMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveImageMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveImageMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveImageMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveImageMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveImageMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveImageMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveImageMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveImageMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveImageMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveImageMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveImageMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveImageMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveImageMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveImageMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveImageMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveImageMethod "getAlignment" o = Gtk.Misc.MiscGetAlignmentMethodInfo
    ResolveImageMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveImageMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveImageMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveImageMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveImageMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveImageMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveImageMethod "getAnimation" o = ImageGetAnimationMethodInfo
    ResolveImageMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveImageMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveImageMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveImageMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveImageMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveImageMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveImageMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveImageMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveImageMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveImageMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveImageMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveImageMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveImageMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveImageMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveImageMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveImageMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveImageMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveImageMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveImageMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveImageMethod "getGicon" o = ImageGetGiconMethodInfo
    ResolveImageMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveImageMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveImageMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveImageMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveImageMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveImageMethod "getIconName" o = ImageGetIconNameMethodInfo
    ResolveImageMethod "getIconSet" o = ImageGetIconSetMethodInfo
    ResolveImageMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveImageMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveImageMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveImageMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveImageMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveImageMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveImageMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveImageMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveImageMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveImageMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveImageMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveImageMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveImageMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveImageMethod "getPadding" o = Gtk.Misc.MiscGetPaddingMethodInfo
    ResolveImageMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveImageMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveImageMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveImageMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveImageMethod "getPixbuf" o = ImageGetPixbufMethodInfo
    ResolveImageMethod "getPixelSize" o = ImageGetPixelSizeMethodInfo
    ResolveImageMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveImageMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveImageMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveImageMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveImageMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveImageMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveImageMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveImageMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveImageMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveImageMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveImageMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveImageMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveImageMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveImageMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveImageMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveImageMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveImageMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveImageMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveImageMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveImageMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveImageMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveImageMethod "getStock" o = ImageGetStockMethodInfo
    ResolveImageMethod "getStorageType" o = ImageGetStorageTypeMethodInfo
    ResolveImageMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveImageMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveImageMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveImageMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveImageMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveImageMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveImageMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveImageMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveImageMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveImageMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveImageMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveImageMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveImageMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveImageMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveImageMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveImageMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveImageMethod "setAlignment" o = Gtk.Misc.MiscSetAlignmentMethodInfo
    ResolveImageMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveImageMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveImageMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveImageMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveImageMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveImageMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveImageMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveImageMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveImageMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveImageMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveImageMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveImageMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveImageMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveImageMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveImageMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveImageMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveImageMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveImageMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveImageMethod "setFromAnimation" o = ImageSetFromAnimationMethodInfo
    ResolveImageMethod "setFromFile" o = ImageSetFromFileMethodInfo
    ResolveImageMethod "setFromGicon" o = ImageSetFromGiconMethodInfo
    ResolveImageMethod "setFromIconName" o = ImageSetFromIconNameMethodInfo
    ResolveImageMethod "setFromIconSet" o = ImageSetFromIconSetMethodInfo
    ResolveImageMethod "setFromPixbuf" o = ImageSetFromPixbufMethodInfo
    ResolveImageMethod "setFromResource" o = ImageSetFromResourceMethodInfo
    ResolveImageMethod "setFromStock" o = ImageSetFromStockMethodInfo
    ResolveImageMethod "setFromSurface" o = ImageSetFromSurfaceMethodInfo
    ResolveImageMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveImageMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveImageMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveImageMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveImageMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveImageMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveImageMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveImageMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveImageMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveImageMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveImageMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveImageMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveImageMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveImageMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveImageMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveImageMethod "setPadding" o = Gtk.Misc.MiscSetPaddingMethodInfo
    ResolveImageMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveImageMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveImageMethod "setPixelSize" o = ImageSetPixelSizeMethodInfo
    ResolveImageMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveImageMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveImageMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveImageMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveImageMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveImageMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveImageMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveImageMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveImageMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveImageMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveImageMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveImageMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveImageMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveImageMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveImageMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveImageMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveImageMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveImageMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveImageMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveImageMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveImageMethod t Image, O.OverloadedMethod info Image p) => OL.IsLabel t (Image -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveImageMethod t Image, O.OverloadedMethod info Image p, R.HasField t Image p) => R.HasField t Image p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveImageMethod t Image, O.OverloadedMethodInfo info Image) => OL.IsLabel t (O.MethodProxy info Image) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- VVV Prop "file"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@file@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' image #file
-- @
getImageFile :: (MonadIO m, IsImage o) => o -> m (Maybe T.Text)
getImageFile obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "file"

-- | Set the value of the “@file@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' image [ #file 'Data.GI.Base.Attributes.:=' value ]
-- @
setImageFile :: (MonadIO m, IsImage o) => o -> T.Text -> m ()
setImageFile obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "file" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@file@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructImageFile :: (IsImage o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructImageFile val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "file" (P.Just val)

-- | Set the value of the “@file@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #file
-- @
clearImageFile :: (MonadIO m, IsImage o) => o -> m ()
clearImageFile obj = liftIO $ B.Properties.setObjectPropertyString obj "file" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data ImageFilePropertyInfo
instance AttrInfo ImageFilePropertyInfo where
    type AttrAllowedOps ImageFilePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint ImageFilePropertyInfo = IsImage
    type AttrSetTypeConstraint ImageFilePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint ImageFilePropertyInfo = (~) T.Text
    type AttrTransferType ImageFilePropertyInfo = T.Text
    type AttrGetType ImageFilePropertyInfo = (Maybe T.Text)
    type AttrLabel ImageFilePropertyInfo = "file"
    type AttrOrigin ImageFilePropertyInfo = Image
    attrGet = getImageFile
    attrSet = setImageFile
    attrTransfer _ v = do
        return v
    attrConstruct = constructImageFile
    attrClear = clearImageFile
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Image.file"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Image.html#g:attr:file"
        })
#endif

-- VVV Prop "gicon"
   -- Type: TInterface (Name {namespace = "Gio", name = "Icon"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gicon@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' image #gicon
-- @
getImageGicon :: (MonadIO m, IsImage o) => o -> m (Maybe Gio.Icon.Icon)
getImageGicon obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "gicon" Gio.Icon.Icon

-- | Set the value of the “@gicon@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' image [ #gicon 'Data.GI.Base.Attributes.:=' value ]
-- @
setImageGicon :: (MonadIO m, IsImage o, Gio.Icon.IsIcon a) => o -> a -> m ()
setImageGicon obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "gicon" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@gicon@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructImageGicon :: (IsImage o, MIO.MonadIO m, Gio.Icon.IsIcon a) => a -> m (GValueConstruct o)
constructImageGicon val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "gicon" (P.Just val)

-- | Set the value of the “@gicon@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #gicon
-- @
clearImageGicon :: (MonadIO m, IsImage o) => o -> m ()
clearImageGicon obj = liftIO $ B.Properties.setObjectPropertyObject obj "gicon" (Nothing :: Maybe Gio.Icon.Icon)

#if defined(ENABLE_OVERLOADING)
data ImageGiconPropertyInfo
instance AttrInfo ImageGiconPropertyInfo where
    type AttrAllowedOps ImageGiconPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint ImageGiconPropertyInfo = IsImage
    type AttrSetTypeConstraint ImageGiconPropertyInfo = Gio.Icon.IsIcon
    type AttrTransferTypeConstraint ImageGiconPropertyInfo = Gio.Icon.IsIcon
    type AttrTransferType ImageGiconPropertyInfo = Gio.Icon.Icon
    type AttrGetType ImageGiconPropertyInfo = (Maybe Gio.Icon.Icon)
    type AttrLabel ImageGiconPropertyInfo = "gicon"
    type AttrOrigin ImageGiconPropertyInfo = Image
    attrGet = getImageGicon
    attrSet = setImageGicon
    attrTransfer _ v = do
        unsafeCastTo Gio.Icon.Icon v
    attrConstruct = constructImageGicon
    attrClear = clearImageGicon
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Image.gicon"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Image.html#g:attr:gicon"
        })
#endif

-- VVV Prop "icon-name"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@icon-name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' image #iconName
-- @
getImageIconName :: (MonadIO m, IsImage o) => o -> m (Maybe T.Text)
getImageIconName obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "icon-name"

-- | Set the value of the “@icon-name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' image [ #iconName 'Data.GI.Base.Attributes.:=' value ]
-- @
setImageIconName :: (MonadIO m, IsImage o) => o -> T.Text -> m ()
setImageIconName obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "icon-name" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@icon-name@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructImageIconName :: (IsImage o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructImageIconName val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "icon-name" (P.Just val)

-- | Set the value of the “@icon-name@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #iconName
-- @
clearImageIconName :: (MonadIO m, IsImage o) => o -> m ()
clearImageIconName obj = liftIO $ B.Properties.setObjectPropertyString obj "icon-name" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data ImageIconNamePropertyInfo
instance AttrInfo ImageIconNamePropertyInfo where
    type AttrAllowedOps ImageIconNamePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint ImageIconNamePropertyInfo = IsImage
    type AttrSetTypeConstraint ImageIconNamePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint ImageIconNamePropertyInfo = (~) T.Text
    type AttrTransferType ImageIconNamePropertyInfo = T.Text
    type AttrGetType ImageIconNamePropertyInfo = (Maybe T.Text)
    type AttrLabel ImageIconNamePropertyInfo = "icon-name"
    type AttrOrigin ImageIconNamePropertyInfo = Image
    attrGet = getImageIconName
    attrSet = setImageIconName
    attrTransfer _ v = do
        return v
    attrConstruct = constructImageIconName
    attrClear = clearImageIconName
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Image.iconName"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Image.html#g:attr:iconName"
        })
#endif

-- VVV Prop "icon-set"
   -- Type: TInterface (Name {namespace = "Gtk", name = "IconSet"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@icon-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' image #iconSet
-- @
getImageIconSet :: (MonadIO m, IsImage o) => o -> m (Maybe Gtk.IconSet.IconSet)
getImageIconSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBoxed obj "icon-set" Gtk.IconSet.IconSet

-- | Set the value of the “@icon-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' image [ #iconSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setImageIconSet :: (MonadIO m, IsImage o) => o -> Gtk.IconSet.IconSet -> m ()
setImageIconSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBoxed obj "icon-set" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@icon-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructImageIconSet :: (IsImage o, MIO.MonadIO m) => Gtk.IconSet.IconSet -> m (GValueConstruct o)
constructImageIconSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBoxed "icon-set" (P.Just val)

-- | Set the value of the “@icon-set@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #iconSet
-- @
clearImageIconSet :: (MonadIO m, IsImage o) => o -> m ()
clearImageIconSet obj = liftIO $ B.Properties.setObjectPropertyBoxed obj "icon-set" (Nothing :: Maybe Gtk.IconSet.IconSet)

#if defined(ENABLE_OVERLOADING)
data ImageIconSetPropertyInfo
instance AttrInfo ImageIconSetPropertyInfo where
    type AttrAllowedOps ImageIconSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint ImageIconSetPropertyInfo = IsImage
    type AttrSetTypeConstraint ImageIconSetPropertyInfo = (~) Gtk.IconSet.IconSet
    type AttrTransferTypeConstraint ImageIconSetPropertyInfo = (~) Gtk.IconSet.IconSet
    type AttrTransferType ImageIconSetPropertyInfo = Gtk.IconSet.IconSet
    type AttrGetType ImageIconSetPropertyInfo = (Maybe Gtk.IconSet.IconSet)
    type AttrLabel ImageIconSetPropertyInfo = "icon-set"
    type AttrOrigin ImageIconSetPropertyInfo = Image
    attrGet = getImageIconSet
    attrSet = setImageIconSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructImageIconSet
    attrClear = clearImageIconSet
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Image.iconSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Image.html#g:attr:iconSet"
        })
#endif

-- VVV Prop "icon-size"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@icon-size@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' image #iconSize
-- @
getImageIconSize :: (MonadIO m, IsImage o) => o -> m Int32
getImageIconSize obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "icon-size"

-- | Set the value of the “@icon-size@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' image [ #iconSize 'Data.GI.Base.Attributes.:=' value ]
-- @
setImageIconSize :: (MonadIO m, IsImage o) => o -> Int32 -> m ()
setImageIconSize obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "icon-size" val

-- | Construct a `GValueConstruct` with valid value for the “@icon-size@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructImageIconSize :: (IsImage o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructImageIconSize val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "icon-size" val

#if defined(ENABLE_OVERLOADING)
data ImageIconSizePropertyInfo
instance AttrInfo ImageIconSizePropertyInfo where
    type AttrAllowedOps ImageIconSizePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ImageIconSizePropertyInfo = IsImage
    type AttrSetTypeConstraint ImageIconSizePropertyInfo = (~) Int32
    type AttrTransferTypeConstraint ImageIconSizePropertyInfo = (~) Int32
    type AttrTransferType ImageIconSizePropertyInfo = Int32
    type AttrGetType ImageIconSizePropertyInfo = Int32
    type AttrLabel ImageIconSizePropertyInfo = "icon-size"
    type AttrOrigin ImageIconSizePropertyInfo = Image
    attrGet = getImageIconSize
    attrSet = setImageIconSize
    attrTransfer _ v = do
        return v
    attrConstruct = constructImageIconSize
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Image.iconSize"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Image.html#g:attr:iconSize"
        })
#endif

-- VVV Prop "pixbuf"
   -- Type: TInterface (Name {namespace = "GdkPixbuf", name = "Pixbuf"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Nothing)

-- | Get the value of the “@pixbuf@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' image #pixbuf
-- @
getImagePixbuf :: (MonadIO m, IsImage o) => o -> m (Maybe GdkPixbuf.Pixbuf.Pixbuf)
getImagePixbuf obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "pixbuf" GdkPixbuf.Pixbuf.Pixbuf

-- | Set the value of the “@pixbuf@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' image [ #pixbuf 'Data.GI.Base.Attributes.:=' value ]
-- @
setImagePixbuf :: (MonadIO m, IsImage o, GdkPixbuf.Pixbuf.IsPixbuf a) => o -> a -> m ()
setImagePixbuf obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "pixbuf" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@pixbuf@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructImagePixbuf :: (IsImage o, MIO.MonadIO m, GdkPixbuf.Pixbuf.IsPixbuf a) => a -> m (GValueConstruct o)
constructImagePixbuf val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "pixbuf" (P.Just val)

-- | Set the value of the “@pixbuf@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #pixbuf
-- @
clearImagePixbuf :: (MonadIO m, IsImage o) => o -> m ()
clearImagePixbuf obj = liftIO $ B.Properties.setObjectPropertyObject obj "pixbuf" (Nothing :: Maybe GdkPixbuf.Pixbuf.Pixbuf)

#if defined(ENABLE_OVERLOADING)
data ImagePixbufPropertyInfo
instance AttrInfo ImagePixbufPropertyInfo where
    type AttrAllowedOps ImagePixbufPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint ImagePixbufPropertyInfo = IsImage
    type AttrSetTypeConstraint ImagePixbufPropertyInfo = GdkPixbuf.Pixbuf.IsPixbuf
    type AttrTransferTypeConstraint ImagePixbufPropertyInfo = GdkPixbuf.Pixbuf.IsPixbuf
    type AttrTransferType ImagePixbufPropertyInfo = GdkPixbuf.Pixbuf.Pixbuf
    type AttrGetType ImagePixbufPropertyInfo = (Maybe GdkPixbuf.Pixbuf.Pixbuf)
    type AttrLabel ImagePixbufPropertyInfo = "pixbuf"
    type AttrOrigin ImagePixbufPropertyInfo = Image
    attrGet = getImagePixbuf
    attrSet = setImagePixbuf
    attrTransfer _ v = do
        unsafeCastTo GdkPixbuf.Pixbuf.Pixbuf v
    attrConstruct = constructImagePixbuf
    attrClear = clearImagePixbuf
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Image.pixbuf"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Image.html#g:attr:pixbuf"
        })
#endif

-- VVV Prop "pixbuf-animation"
   -- Type: TInterface (Name {namespace = "GdkPixbuf", name = "PixbufAnimation"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@pixbuf-animation@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' image #pixbufAnimation
-- @
getImagePixbufAnimation :: (MonadIO m, IsImage o) => o -> m (Maybe GdkPixbuf.PixbufAnimation.PixbufAnimation)
getImagePixbufAnimation obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "pixbuf-animation" GdkPixbuf.PixbufAnimation.PixbufAnimation

-- | Set the value of the “@pixbuf-animation@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' image [ #pixbufAnimation 'Data.GI.Base.Attributes.:=' value ]
-- @
setImagePixbufAnimation :: (MonadIO m, IsImage o, GdkPixbuf.PixbufAnimation.IsPixbufAnimation a) => o -> a -> m ()
setImagePixbufAnimation obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "pixbuf-animation" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@pixbuf-animation@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructImagePixbufAnimation :: (IsImage o, MIO.MonadIO m, GdkPixbuf.PixbufAnimation.IsPixbufAnimation a) => a -> m (GValueConstruct o)
constructImagePixbufAnimation val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "pixbuf-animation" (P.Just val)

-- | Set the value of the “@pixbuf-animation@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #pixbufAnimation
-- @
clearImagePixbufAnimation :: (MonadIO m, IsImage o) => o -> m ()
clearImagePixbufAnimation obj = liftIO $ B.Properties.setObjectPropertyObject obj "pixbuf-animation" (Nothing :: Maybe GdkPixbuf.PixbufAnimation.PixbufAnimation)

#if defined(ENABLE_OVERLOADING)
data ImagePixbufAnimationPropertyInfo
instance AttrInfo ImagePixbufAnimationPropertyInfo where
    type AttrAllowedOps ImagePixbufAnimationPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint ImagePixbufAnimationPropertyInfo = IsImage
    type AttrSetTypeConstraint ImagePixbufAnimationPropertyInfo = GdkPixbuf.PixbufAnimation.IsPixbufAnimation
    type AttrTransferTypeConstraint ImagePixbufAnimationPropertyInfo = GdkPixbuf.PixbufAnimation.IsPixbufAnimation
    type AttrTransferType ImagePixbufAnimationPropertyInfo = GdkPixbuf.PixbufAnimation.PixbufAnimation
    type AttrGetType ImagePixbufAnimationPropertyInfo = (Maybe GdkPixbuf.PixbufAnimation.PixbufAnimation)
    type AttrLabel ImagePixbufAnimationPropertyInfo = "pixbuf-animation"
    type AttrOrigin ImagePixbufAnimationPropertyInfo = Image
    attrGet = getImagePixbufAnimation
    attrSet = setImagePixbufAnimation
    attrTransfer _ v = do
        unsafeCastTo GdkPixbuf.PixbufAnimation.PixbufAnimation v
    attrConstruct = constructImagePixbufAnimation
    attrClear = clearImagePixbufAnimation
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Image.pixbufAnimation"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Image.html#g:attr:pixbufAnimation"
        })
#endif

-- VVV Prop "pixel-size"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@pixel-size@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' image #pixelSize
-- @
getImagePixelSize :: (MonadIO m, IsImage o) => o -> m Int32
getImagePixelSize obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "pixel-size"

-- | Set the value of the “@pixel-size@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' image [ #pixelSize 'Data.GI.Base.Attributes.:=' value ]
-- @
setImagePixelSize :: (MonadIO m, IsImage o) => o -> Int32 -> m ()
setImagePixelSize obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "pixel-size" val

-- | Construct a `GValueConstruct` with valid value for the “@pixel-size@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructImagePixelSize :: (IsImage o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructImagePixelSize val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "pixel-size" val

#if defined(ENABLE_OVERLOADING)
data ImagePixelSizePropertyInfo
instance AttrInfo ImagePixelSizePropertyInfo where
    type AttrAllowedOps ImagePixelSizePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ImagePixelSizePropertyInfo = IsImage
    type AttrSetTypeConstraint ImagePixelSizePropertyInfo = (~) Int32
    type AttrTransferTypeConstraint ImagePixelSizePropertyInfo = (~) Int32
    type AttrTransferType ImagePixelSizePropertyInfo = Int32
    type AttrGetType ImagePixelSizePropertyInfo = Int32
    type AttrLabel ImagePixelSizePropertyInfo = "pixel-size"
    type AttrOrigin ImagePixelSizePropertyInfo = Image
    attrGet = getImagePixelSize
    attrSet = setImagePixelSize
    attrTransfer _ v = do
        return v
    attrConstruct = constructImagePixelSize
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Image.pixelSize"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Image.html#g:attr:pixelSize"
        })
#endif

-- VVV Prop "resource"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@resource@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' image #resource
-- @
getImageResource :: (MonadIO m, IsImage o) => o -> m (Maybe T.Text)
getImageResource obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "resource"

-- | Set the value of the “@resource@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' image [ #resource 'Data.GI.Base.Attributes.:=' value ]
-- @
setImageResource :: (MonadIO m, IsImage o) => o -> T.Text -> m ()
setImageResource obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "resource" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@resource@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructImageResource :: (IsImage o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructImageResource val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "resource" (P.Just val)

-- | Set the value of the “@resource@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #resource
-- @
clearImageResource :: (MonadIO m, IsImage o) => o -> m ()
clearImageResource obj = liftIO $ B.Properties.setObjectPropertyString obj "resource" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data ImageResourcePropertyInfo
instance AttrInfo ImageResourcePropertyInfo where
    type AttrAllowedOps ImageResourcePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint ImageResourcePropertyInfo = IsImage
    type AttrSetTypeConstraint ImageResourcePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint ImageResourcePropertyInfo = (~) T.Text
    type AttrTransferType ImageResourcePropertyInfo = T.Text
    type AttrGetType ImageResourcePropertyInfo = (Maybe T.Text)
    type AttrLabel ImageResourcePropertyInfo = "resource"
    type AttrOrigin ImageResourcePropertyInfo = Image
    attrGet = getImageResource
    attrSet = setImageResource
    attrTransfer _ v = do
        return v
    attrConstruct = constructImageResource
    attrClear = clearImageResource
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Image.resource"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Image.html#g:attr:resource"
        })
#endif

-- VVV Prop "stock"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@stock@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' image #stock
-- @
getImageStock :: (MonadIO m, IsImage o) => o -> m (Maybe T.Text)
getImageStock obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "stock"

-- | Set the value of the “@stock@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' image [ #stock 'Data.GI.Base.Attributes.:=' value ]
-- @
setImageStock :: (MonadIO m, IsImage o) => o -> T.Text -> m ()
setImageStock obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "stock" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@stock@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructImageStock :: (IsImage o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructImageStock val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "stock" (P.Just val)

-- | Set the value of the “@stock@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #stock
-- @
clearImageStock :: (MonadIO m, IsImage o) => o -> m ()
clearImageStock obj = liftIO $ B.Properties.setObjectPropertyString obj "stock" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data ImageStockPropertyInfo
instance AttrInfo ImageStockPropertyInfo where
    type AttrAllowedOps ImageStockPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint ImageStockPropertyInfo = IsImage
    type AttrSetTypeConstraint ImageStockPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint ImageStockPropertyInfo = (~) T.Text
    type AttrTransferType ImageStockPropertyInfo = T.Text
    type AttrGetType ImageStockPropertyInfo = (Maybe T.Text)
    type AttrLabel ImageStockPropertyInfo = "stock"
    type AttrOrigin ImageStockPropertyInfo = Image
    attrGet = getImageStock
    attrSet = setImageStock
    attrTransfer _ v = do
        return v
    attrConstruct = constructImageStock
    attrClear = clearImageStock
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Image.stock"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Image.html#g:attr:stock"
        })
#endif

-- VVV Prop "storage-type"
   -- Type: TInterface (Name {namespace = "Gtk", name = "ImageType"})
   -- Flags: [PropertyReadable]
   -- Nullable: (Just False,Nothing)

-- | Get the value of the “@storage-type@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' image #storageType
-- @
getImageStorageType :: (MonadIO m, IsImage o) => o -> m Gtk.Enums.ImageType
getImageStorageType obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "storage-type"

#if defined(ENABLE_OVERLOADING)
data ImageStorageTypePropertyInfo
instance AttrInfo ImageStorageTypePropertyInfo where
    type AttrAllowedOps ImageStorageTypePropertyInfo = '[ 'AttrGet]
    type AttrBaseTypeConstraint ImageStorageTypePropertyInfo = IsImage
    type AttrSetTypeConstraint ImageStorageTypePropertyInfo = (~) ()
    type AttrTransferTypeConstraint ImageStorageTypePropertyInfo = (~) ()
    type AttrTransferType ImageStorageTypePropertyInfo = ()
    type AttrGetType ImageStorageTypePropertyInfo = Gtk.Enums.ImageType
    type AttrLabel ImageStorageTypePropertyInfo = "storage-type"
    type AttrOrigin ImageStorageTypePropertyInfo = Image
    attrGet = getImageStorageType
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Image.storageType"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Image.html#g:attr:storageType"
        })
#endif

-- VVV Prop "surface"
   -- Type: TInterface (Name {namespace = "cairo", name = "Surface"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@surface@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' image #surface
-- @
getImageSurface :: (MonadIO m, IsImage o) => o -> m (Maybe Cairo.Surface.Surface)
getImageSurface obj = MIO.liftIO $ B.Properties.getObjectPropertyBoxed obj "surface" Cairo.Surface.Surface

-- | Set the value of the “@surface@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' image [ #surface 'Data.GI.Base.Attributes.:=' value ]
-- @
setImageSurface :: (MonadIO m, IsImage o) => o -> Cairo.Surface.Surface -> m ()
setImageSurface obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBoxed obj "surface" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@surface@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructImageSurface :: (IsImage o, MIO.MonadIO m) => Cairo.Surface.Surface -> m (GValueConstruct o)
constructImageSurface val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBoxed "surface" (P.Just val)

-- | Set the value of the “@surface@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #surface
-- @
clearImageSurface :: (MonadIO m, IsImage o) => o -> m ()
clearImageSurface obj = liftIO $ B.Properties.setObjectPropertyBoxed obj "surface" (Nothing :: Maybe Cairo.Surface.Surface)

#if defined(ENABLE_OVERLOADING)
data ImageSurfacePropertyInfo
instance AttrInfo ImageSurfacePropertyInfo where
    type AttrAllowedOps ImageSurfacePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint ImageSurfacePropertyInfo = IsImage
    type AttrSetTypeConstraint ImageSurfacePropertyInfo = (~) Cairo.Surface.Surface
    type AttrTransferTypeConstraint ImageSurfacePropertyInfo = (~) Cairo.Surface.Surface
    type AttrTransferType ImageSurfacePropertyInfo = Cairo.Surface.Surface
    type AttrGetType ImageSurfacePropertyInfo = (Maybe Cairo.Surface.Surface)
    type AttrLabel ImageSurfacePropertyInfo = "surface"
    type AttrOrigin ImageSurfacePropertyInfo = Image
    attrGet = getImageSurface
    attrSet = setImageSurface
    attrTransfer _ v = do
        return v
    attrConstruct = constructImageSurface
    attrClear = clearImageSurface
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Image.surface"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Image.html#g:attr:surface"
        })
#endif

-- VVV Prop "use-fallback"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@use-fallback@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' image #useFallback
-- @
getImageUseFallback :: (MonadIO m, IsImage o) => o -> m Bool
getImageUseFallback obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "use-fallback"

-- | Set the value of the “@use-fallback@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' image [ #useFallback 'Data.GI.Base.Attributes.:=' value ]
-- @
setImageUseFallback :: (MonadIO m, IsImage o) => o -> Bool -> m ()
setImageUseFallback obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "use-fallback" val

-- | Construct a `GValueConstruct` with valid value for the “@use-fallback@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructImageUseFallback :: (IsImage o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructImageUseFallback val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "use-fallback" val

#if defined(ENABLE_OVERLOADING)
data ImageUseFallbackPropertyInfo
instance AttrInfo ImageUseFallbackPropertyInfo where
    type AttrAllowedOps ImageUseFallbackPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ImageUseFallbackPropertyInfo = IsImage
    type AttrSetTypeConstraint ImageUseFallbackPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint ImageUseFallbackPropertyInfo = (~) Bool
    type AttrTransferType ImageUseFallbackPropertyInfo = Bool
    type AttrGetType ImageUseFallbackPropertyInfo = Bool
    type AttrLabel ImageUseFallbackPropertyInfo = "use-fallback"
    type AttrOrigin ImageUseFallbackPropertyInfo = Image
    attrGet = getImageUseFallback
    attrSet = setImageUseFallback
    attrTransfer _ v = do
        return v
    attrConstruct = constructImageUseFallback
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Image.useFallback"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Image.html#g:attr:useFallback"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList Image
type instance O.AttributeList Image = ImageAttributeList
type ImageAttributeList = ('[ '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("file", ImageFilePropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("gicon", ImageGiconPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("iconName", ImageIconNamePropertyInfo), '("iconSet", ImageIconSetPropertyInfo), '("iconSize", ImageIconSizePropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("pixbuf", ImagePixbufPropertyInfo), '("pixbufAnimation", ImagePixbufAnimationPropertyInfo), '("pixelSize", ImagePixelSizePropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("resource", ImageResourcePropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("stock", ImageStockPropertyInfo), '("storageType", ImageStorageTypePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("surface", ImageSurfacePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("useFallback", ImageUseFallbackPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo), '("xalign", Gtk.Misc.MiscXalignPropertyInfo), '("xpad", Gtk.Misc.MiscXpadPropertyInfo), '("yalign", Gtk.Misc.MiscYalignPropertyInfo), '("ypad", Gtk.Misc.MiscYpadPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
imageFile :: AttrLabelProxy "file"
imageFile = AttrLabelProxy

imageGicon :: AttrLabelProxy "gicon"
imageGicon = AttrLabelProxy

imageIconName :: AttrLabelProxy "iconName"
imageIconName = AttrLabelProxy

imageIconSet :: AttrLabelProxy "iconSet"
imageIconSet = AttrLabelProxy

imageIconSize :: AttrLabelProxy "iconSize"
imageIconSize = AttrLabelProxy

imagePixbuf :: AttrLabelProxy "pixbuf"
imagePixbuf = AttrLabelProxy

imagePixbufAnimation :: AttrLabelProxy "pixbufAnimation"
imagePixbufAnimation = AttrLabelProxy

imagePixelSize :: AttrLabelProxy "pixelSize"
imagePixelSize = AttrLabelProxy

imageResource :: AttrLabelProxy "resource"
imageResource = AttrLabelProxy

imageStock :: AttrLabelProxy "stock"
imageStock = AttrLabelProxy

imageStorageType :: AttrLabelProxy "storageType"
imageStorageType = AttrLabelProxy

imageSurface :: AttrLabelProxy "surface"
imageSurface = AttrLabelProxy

imageUseFallback :: AttrLabelProxy "useFallback"
imageUseFallback = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList Image = ImageSignalList
type ImageSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method Image::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Image" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_image_new" gtk_image_new :: 
    IO (Ptr Image)

-- | Creates a new empty t'GI.Gtk.Objects.Image.Image' widget.
imageNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m Image
    -- ^ __Returns:__ a newly created t'GI.Gtk.Objects.Image.Image' widget.
imageNew  = liftIO $ do
    result <- gtk_image_new
    checkUnexpectedReturnNULL "imageNew" result
    result' <- (newObject Image) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Image::new_from_animation
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "animation"
--           , argType =
--               TInterface
--                 Name { namespace = "GdkPixbuf" , name = "PixbufAnimation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an animation" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Image" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_image_new_from_animation" gtk_image_new_from_animation :: 
    Ptr GdkPixbuf.PixbufAnimation.PixbufAnimation -> -- animation : TInterface (Name {namespace = "GdkPixbuf", name = "PixbufAnimation"})
    IO (Ptr Image)

-- | Creates a t'GI.Gtk.Objects.Image.Image' displaying the given animation.
-- The t'GI.Gtk.Objects.Image.Image' does not assume a reference to the
-- animation; you still need to unref it if you own references.
-- t'GI.Gtk.Objects.Image.Image' will add its own reference rather than adopting yours.
-- 
-- Note that the animation frames are shown using a timeout with
-- 'GI.GLib.Constants.PRIORITY_DEFAULT'. When using animations to indicate busyness,
-- keep in mind that the animation will only be shown if the main loop
-- is not busy with something that has a higher priority.
imageNewFromAnimation ::
    (B.CallStack.HasCallStack, MonadIO m, GdkPixbuf.PixbufAnimation.IsPixbufAnimation a) =>
    a
    -- ^ /@animation@/: an animation
    -> m Image
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.Image.Image' widget
imageNewFromAnimation animation = liftIO $ do
    animation' <- unsafeManagedPtrCastPtr animation
    result <- gtk_image_new_from_animation animation'
    checkUnexpectedReturnNULL "imageNewFromAnimation" result
    result' <- (newObject Image) result
    touchManagedPtr animation
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Image::new_from_file
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "filename"
--           , argType = TBasicType TFileName
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a filename" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Image" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_image_new_from_file" gtk_image_new_from_file :: 
    CString ->                              -- filename : TBasicType TFileName
    IO (Ptr Image)

-- | Creates a new t'GI.Gtk.Objects.Image.Image' displaying the file /@filename@/. If the file
-- isn’t found or can’t be loaded, the resulting t'GI.Gtk.Objects.Image.Image' will
-- display a “broken image” icon. This function never returns 'P.Nothing',
-- it always returns a valid t'GI.Gtk.Objects.Image.Image' widget.
-- 
-- If the file contains an animation, the image will contain an
-- animation.
-- 
-- If you need to detect failures to load the file, use
-- 'GI.GdkPixbuf.Objects.Pixbuf.pixbufNewFromFile' to load the file yourself, then create
-- the t'GI.Gtk.Objects.Image.Image' from the pixbuf. (Or for animations, use
-- 'GI.GdkPixbuf.Objects.PixbufAnimation.pixbufAnimationNewFromFile').
-- 
-- The storage type ('GI.Gtk.Objects.Image.imageGetStorageType') of the returned
-- image is not defined, it will be whatever is appropriate for
-- displaying the file.
imageNewFromFile ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    [Char]
    -- ^ /@filename@/: a filename
    -> m Image
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.Image.Image'
imageNewFromFile filename = liftIO $ do
    filename' <- stringToCString filename
    result <- gtk_image_new_from_file filename'
    checkUnexpectedReturnNULL "imageNewFromFile" result
    result' <- (newObject Image) result
    freeMem filename'
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Image::new_from_gicon
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "icon"
--           , argType = TInterface Name { namespace = "Gio" , name = "Icon" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an icon" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a stock icon size (#GtkIconSize)"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Image" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_image_new_from_gicon" gtk_image_new_from_gicon :: 
    Ptr Gio.Icon.Icon ->                    -- icon : TInterface (Name {namespace = "Gio", name = "Icon"})
    Int32 ->                                -- size : TBasicType TInt
    IO (Ptr Image)

-- | Creates a t'GI.Gtk.Objects.Image.Image' displaying an icon from the current icon theme.
-- If the icon name isn’t known, a “broken image” icon will be
-- displayed instead.  If the current icon theme is changed, the icon
-- will be updated appropriately.
-- 
-- /Since: 2.14/
imageNewFromGicon ::
    (B.CallStack.HasCallStack, MonadIO m, Gio.Icon.IsIcon a) =>
    a
    -- ^ /@icon@/: an icon
    -> Int32
    -- ^ /@size@/: a stock icon size (t'GI.Gtk.Enums.IconSize')
    -> m Image
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.Image.Image' displaying the themed icon
imageNewFromGicon icon size = liftIO $ do
    icon' <- unsafeManagedPtrCastPtr icon
    result <- gtk_image_new_from_gicon icon' size
    checkUnexpectedReturnNULL "imageNewFromGicon" result
    result' <- (newObject Image) result
    touchManagedPtr icon
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Image::new_from_icon_name
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
--                 { rawDocText = Just "a stock icon size (#GtkIconSize)"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Image" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_image_new_from_icon_name" gtk_image_new_from_icon_name :: 
    CString ->                              -- icon_name : TBasicType TUTF8
    Int32 ->                                -- size : TBasicType TInt
    IO (Ptr Image)

-- | Creates a t'GI.Gtk.Objects.Image.Image' displaying an icon from the current icon theme.
-- If the icon name isn’t known, a “broken image” icon will be
-- displayed instead.  If the current icon theme is changed, the icon
-- will be updated appropriately.
-- 
-- /Since: 2.6/
imageNewFromIconName ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Maybe (T.Text)
    -- ^ /@iconName@/: an icon name or 'P.Nothing'
    -> Int32
    -- ^ /@size@/: a stock icon size (t'GI.Gtk.Enums.IconSize')
    -> m Image
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.Image.Image' displaying the themed icon
imageNewFromIconName iconName size = liftIO $ do
    maybeIconName <- case iconName of
        Nothing -> return nullPtr
        Just jIconName -> do
            jIconName' <- textToCString jIconName
            return jIconName'
    result <- gtk_image_new_from_icon_name maybeIconName size
    checkUnexpectedReturnNULL "imageNewFromIconName" result
    result' <- (newObject Image) result
    freeMem maybeIconName
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Image::new_from_icon_set
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "icon_set"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconSet" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkIconSet" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a stock icon size (#GtkIconSize)"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Image" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_image_new_from_icon_set" gtk_image_new_from_icon_set :: 
    Ptr Gtk.IconSet.IconSet ->              -- icon_set : TInterface (Name {namespace = "Gtk", name = "IconSet"})
    Int32 ->                                -- size : TBasicType TInt
    IO (Ptr Image)

{-# DEPRECATED imageNewFromIconSet ["(Since version 3.10)","Use 'GI.Gtk.Objects.Image.imageNewFromIconName' instead."] #-}
-- | Creates a t'GI.Gtk.Objects.Image.Image' displaying an icon set. Sample stock sizes are
-- @/GTK_ICON_SIZE_MENU/@, @/GTK_ICON_SIZE_SMALL_TOOLBAR/@. Instead of using
-- this function, usually it’s better to create a t'GI.Gtk.Objects.IconFactory.IconFactory', put
-- your icon sets in the icon factory, add the icon factory to the
-- list of default factories with 'GI.Gtk.Objects.IconFactory.iconFactoryAddDefault', and
-- then use 'GI.Gtk.Objects.Image.imageNewFromStock'. This will allow themes to
-- override the icon you ship with your application.
-- 
-- The t'GI.Gtk.Objects.Image.Image' does not assume a reference to the
-- icon set; you still need to unref it if you own references.
-- t'GI.Gtk.Objects.Image.Image' will add its own reference rather than adopting yours.
imageNewFromIconSet ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Gtk.IconSet.IconSet
    -- ^ /@iconSet@/: a t'GI.Gtk.Structs.IconSet.IconSet'
    -> Int32
    -- ^ /@size@/: a stock icon size (t'GI.Gtk.Enums.IconSize')
    -> m Image
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.Image.Image'
imageNewFromIconSet iconSet size = liftIO $ do
    iconSet' <- unsafeManagedPtrGetPtr iconSet
    result <- gtk_image_new_from_icon_set iconSet' size
    checkUnexpectedReturnNULL "imageNewFromIconSet" result
    result' <- (newObject Image) result
    touchManagedPtr iconSet
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Image::new_from_pixbuf
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "pixbuf"
--           , argType =
--               TInterface Name { namespace = "GdkPixbuf" , name = "Pixbuf" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkPixbuf, or %NULL"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Image" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_image_new_from_pixbuf" gtk_image_new_from_pixbuf :: 
    Ptr GdkPixbuf.Pixbuf.Pixbuf ->          -- pixbuf : TInterface (Name {namespace = "GdkPixbuf", name = "Pixbuf"})
    IO (Ptr Image)

-- | Creates a new t'GI.Gtk.Objects.Image.Image' displaying /@pixbuf@/.
-- The t'GI.Gtk.Objects.Image.Image' does not assume a reference to the
-- pixbuf; you still need to unref it if you own references.
-- t'GI.Gtk.Objects.Image.Image' will add its own reference rather than adopting yours.
-- 
-- Note that this function just creates an t'GI.Gtk.Objects.Image.Image' from the pixbuf. The
-- t'GI.Gtk.Objects.Image.Image' created will not react to state changes. Should you want that,
-- you should use 'GI.Gtk.Objects.Image.imageNewFromIconName'.
imageNewFromPixbuf ::
    (B.CallStack.HasCallStack, MonadIO m, GdkPixbuf.Pixbuf.IsPixbuf a) =>
    Maybe (a)
    -- ^ /@pixbuf@/: a t'GI.GdkPixbuf.Objects.Pixbuf.Pixbuf', or 'P.Nothing'
    -> m Image
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.Image.Image'
imageNewFromPixbuf pixbuf = liftIO $ do
    maybePixbuf <- case pixbuf of
        Nothing -> return nullPtr
        Just jPixbuf -> do
            jPixbuf' <- unsafeManagedPtrCastPtr jPixbuf
            return jPixbuf'
    result <- gtk_image_new_from_pixbuf maybePixbuf
    checkUnexpectedReturnNULL "imageNewFromPixbuf" result
    result' <- (newObject Image) result
    whenJust pixbuf touchManagedPtr
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Image::new_from_resource
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "resource_path"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a resource path" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Image" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_image_new_from_resource" gtk_image_new_from_resource :: 
    CString ->                              -- resource_path : TBasicType TUTF8
    IO (Ptr Image)

-- | Creates a new t'GI.Gtk.Objects.Image.Image' displaying the resource file /@resourcePath@/. If the file
-- isn’t found or can’t be loaded, the resulting t'GI.Gtk.Objects.Image.Image' will
-- display a “broken image” icon. This function never returns 'P.Nothing',
-- it always returns a valid t'GI.Gtk.Objects.Image.Image' widget.
-- 
-- If the file contains an animation, the image will contain an
-- animation.
-- 
-- If you need to detect failures to load the file, use
-- 'GI.GdkPixbuf.Objects.Pixbuf.pixbufNewFromFile' to load the file yourself, then create
-- the t'GI.Gtk.Objects.Image.Image' from the pixbuf. (Or for animations, use
-- 'GI.GdkPixbuf.Objects.PixbufAnimation.pixbufAnimationNewFromFile').
-- 
-- The storage type ('GI.Gtk.Objects.Image.imageGetStorageType') of the returned
-- image is not defined, it will be whatever is appropriate for
-- displaying the file.
-- 
-- /Since: 3.4/
imageNewFromResource ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@resourcePath@/: a resource path
    -> m Image
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.Image.Image'
imageNewFromResource resourcePath = liftIO $ do
    resourcePath' <- textToCString resourcePath
    result <- gtk_image_new_from_resource resourcePath'
    checkUnexpectedReturnNULL "imageNewFromResource" result
    result' <- (newObject Image) result
    freeMem resourcePath'
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Image::new_from_stock
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "stock_id"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a stock icon name" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a stock icon size (#GtkIconSize)"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Image" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_image_new_from_stock" gtk_image_new_from_stock :: 
    CString ->                              -- stock_id : TBasicType TUTF8
    Int32 ->                                -- size : TBasicType TInt
    IO (Ptr Image)

{-# DEPRECATED imageNewFromStock ["(Since version 3.10)","Use 'GI.Gtk.Objects.Image.imageNewFromIconName' instead."] #-}
-- | Creates a t'GI.Gtk.Objects.Image.Image' displaying a stock icon. Sample stock icon
-- names are 'GI.Gtk.Constants.STOCK_OPEN', 'GI.Gtk.Constants.STOCK_QUIT'. Sample stock sizes
-- are @/GTK_ICON_SIZE_MENU/@, @/GTK_ICON_SIZE_SMALL_TOOLBAR/@. If the stock
-- icon name isn’t known, the image will be empty.
-- You can register your own stock icon names, see
-- 'GI.Gtk.Objects.IconFactory.iconFactoryAddDefault' and 'GI.Gtk.Objects.IconFactory.iconFactoryAdd'.
imageNewFromStock ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@stockId@/: a stock icon name
    -> Int32
    -- ^ /@size@/: a stock icon size (t'GI.Gtk.Enums.IconSize')
    -> m Image
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.Image.Image' displaying the stock icon
imageNewFromStock stockId size = liftIO $ do
    stockId' <- textToCString stockId
    result <- gtk_image_new_from_stock stockId' size
    checkUnexpectedReturnNULL "imageNewFromStock" result
    result' <- (newObject Image) result
    freeMem stockId'
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Image::new_from_surface
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "surface"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Surface" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #cairo_surface_t, or %NULL"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Image" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_image_new_from_surface" gtk_image_new_from_surface :: 
    Ptr Cairo.Surface.Surface ->            -- surface : TInterface (Name {namespace = "cairo", name = "Surface"})
    IO (Ptr Image)

-- | Creates a new t'GI.Gtk.Objects.Image.Image' displaying /@surface@/.
-- The t'GI.Gtk.Objects.Image.Image' does not assume a reference to the
-- surface; you still need to unref it if you own references.
-- t'GI.Gtk.Objects.Image.Image' will add its own reference rather than adopting yours.
-- 
-- /Since: 3.10/
imageNewFromSurface ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Maybe (Cairo.Surface.Surface)
    -- ^ /@surface@/: a t'GI.Cairo.Structs.Surface.Surface', or 'P.Nothing'
    -> m Image
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.Image.Image'
imageNewFromSurface surface = liftIO $ do
    maybeSurface <- case surface of
        Nothing -> return nullPtr
        Just jSurface -> do
            jSurface' <- unsafeManagedPtrGetPtr jSurface
            return jSurface'
    result <- gtk_image_new_from_surface maybeSurface
    checkUnexpectedReturnNULL "imageNewFromSurface" result
    result' <- (newObject Image) result
    whenJust surface touchManagedPtr
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Image::clear
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "image"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Image" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkImage" , sinceVersion = Nothing }
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

foreign import ccall "gtk_image_clear" gtk_image_clear :: 
    Ptr Image ->                            -- image : TInterface (Name {namespace = "Gtk", name = "Image"})
    IO ()

-- | Resets the image to be empty.
-- 
-- /Since: 2.8/
imageClear ::
    (B.CallStack.HasCallStack, MonadIO m, IsImage a) =>
    a
    -- ^ /@image@/: a t'GI.Gtk.Objects.Image.Image'
    -> m ()
imageClear image = liftIO $ do
    image' <- unsafeManagedPtrCastPtr image
    gtk_image_clear image'
    touchManagedPtr image
    return ()

#if defined(ENABLE_OVERLOADING)
data ImageClearMethodInfo
instance (signature ~ (m ()), MonadIO m, IsImage a) => O.OverloadedMethod ImageClearMethodInfo a signature where
    overloadedMethod = imageClear

instance O.OverloadedMethodInfo ImageClearMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Image.imageClear",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Image.html#v:imageClear"
        })


#endif

-- method Image::get_animation
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "image"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Image" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkImage" , sinceVersion = Nothing }
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
--                  Name { namespace = "GdkPixbuf" , name = "PixbufAnimation" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_image_get_animation" gtk_image_get_animation :: 
    Ptr Image ->                            -- image : TInterface (Name {namespace = "Gtk", name = "Image"})
    IO (Ptr GdkPixbuf.PixbufAnimation.PixbufAnimation)

-- | Gets the t'GI.GdkPixbuf.Objects.PixbufAnimation.PixbufAnimation' being displayed by the t'GI.Gtk.Objects.Image.Image'.
-- The storage type of the image must be 'GI.Gtk.Enums.ImageTypeEmpty' or
-- 'GI.Gtk.Enums.ImageTypeAnimation' (see 'GI.Gtk.Objects.Image.imageGetStorageType').
-- The caller of this function does not own a reference to the
-- returned animation.
imageGetAnimation ::
    (B.CallStack.HasCallStack, MonadIO m, IsImage a) =>
    a
    -- ^ /@image@/: a t'GI.Gtk.Objects.Image.Image'
    -> m (Maybe GdkPixbuf.PixbufAnimation.PixbufAnimation)
    -- ^ __Returns:__ the displayed animation, or 'P.Nothing' if
    -- the image is empty
imageGetAnimation image = liftIO $ do
    image' <- unsafeManagedPtrCastPtr image
    result <- gtk_image_get_animation image'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject GdkPixbuf.PixbufAnimation.PixbufAnimation) result'
        return result''
    touchManagedPtr image
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data ImageGetAnimationMethodInfo
instance (signature ~ (m (Maybe GdkPixbuf.PixbufAnimation.PixbufAnimation)), MonadIO m, IsImage a) => O.OverloadedMethod ImageGetAnimationMethodInfo a signature where
    overloadedMethod = imageGetAnimation

instance O.OverloadedMethodInfo ImageGetAnimationMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Image.imageGetAnimation",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Image.html#v:imageGetAnimation"
        })


#endif

-- method Image::get_gicon
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "image"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Image" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkImage" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "gicon"
--           , argType = TInterface Name { namespace = "Gio" , name = "Icon" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "place to store a\n    #GIcon, or %NULL"
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
--                 { rawDocText =
--                     Just "place to store an icon size\n    (#GtkIconSize), or %NULL"
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

foreign import ccall "gtk_image_get_gicon" gtk_image_get_gicon :: 
    Ptr Image ->                            -- image : TInterface (Name {namespace = "Gtk", name = "Image"})
    Ptr (Ptr Gio.Icon.Icon) ->              -- gicon : TInterface (Name {namespace = "Gio", name = "Icon"})
    Ptr Int32 ->                            -- size : TBasicType TInt
    IO ()

-- | Gets the t'GI.Gio.Interfaces.Icon.Icon' and size being displayed by the t'GI.Gtk.Objects.Image.Image'.
-- The storage type of the image must be 'GI.Gtk.Enums.ImageTypeEmpty' or
-- 'GI.Gtk.Enums.ImageTypeGicon' (see 'GI.Gtk.Objects.Image.imageGetStorageType').
-- The caller of this function does not own a reference to the
-- returned t'GI.Gio.Interfaces.Icon.Icon'.
-- 
-- /Since: 2.14/
imageGetGicon ::
    (B.CallStack.HasCallStack, MonadIO m, IsImage a) =>
    a
    -- ^ /@image@/: a t'GI.Gtk.Objects.Image.Image'
    -> m ((Gio.Icon.Icon, Int32))
imageGetGicon image = liftIO $ do
    image' <- unsafeManagedPtrCastPtr image
    gicon <- callocMem :: IO (Ptr (Ptr Gio.Icon.Icon))
    size <- allocMem :: IO (Ptr Int32)
    gtk_image_get_gicon image' gicon size
    gicon' <- peek gicon
    gicon'' <- (newObject Gio.Icon.Icon) gicon'
    size' <- peek size
    touchManagedPtr image
    freeMem gicon
    freeMem size
    return (gicon'', size')

#if defined(ENABLE_OVERLOADING)
data ImageGetGiconMethodInfo
instance (signature ~ (m ((Gio.Icon.Icon, Int32))), MonadIO m, IsImage a) => O.OverloadedMethod ImageGetGiconMethodInfo a signature where
    overloadedMethod = imageGetGicon

instance O.OverloadedMethodInfo ImageGetGiconMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Image.imageGetGicon",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Image.html#v:imageGetGicon"
        })


#endif

-- method Image::get_icon_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "image"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Image" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkImage" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icon_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "place to store an\n    icon name, or %NULL"
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
--                 { rawDocText =
--                     Just "place to store an icon size\n    (#GtkIconSize), or %NULL"
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

foreign import ccall "gtk_image_get_icon_name" gtk_image_get_icon_name :: 
    Ptr Image ->                            -- image : TInterface (Name {namespace = "Gtk", name = "Image"})
    Ptr CString ->                          -- icon_name : TBasicType TUTF8
    Ptr Int32 ->                            -- size : TBasicType TInt
    IO ()

-- | Gets the icon name and size being displayed by the t'GI.Gtk.Objects.Image.Image'.
-- The storage type of the image must be 'GI.Gtk.Enums.ImageTypeEmpty' or
-- 'GI.Gtk.Enums.ImageTypeIconName' (see 'GI.Gtk.Objects.Image.imageGetStorageType').
-- The returned string is owned by the t'GI.Gtk.Objects.Image.Image' and should not
-- be freed.
-- 
-- /Since: 2.6/
imageGetIconName ::
    (B.CallStack.HasCallStack, MonadIO m, IsImage a) =>
    a
    -- ^ /@image@/: a t'GI.Gtk.Objects.Image.Image'
    -> m ((T.Text, Int32))
imageGetIconName image = liftIO $ do
    image' <- unsafeManagedPtrCastPtr image
    iconName <- callocMem :: IO (Ptr CString)
    size <- allocMem :: IO (Ptr Int32)
    gtk_image_get_icon_name image' iconName size
    iconName' <- peek iconName
    iconName'' <- cstringToText iconName'
    size' <- peek size
    touchManagedPtr image
    freeMem iconName
    freeMem size
    return (iconName'', size')

#if defined(ENABLE_OVERLOADING)
data ImageGetIconNameMethodInfo
instance (signature ~ (m ((T.Text, Int32))), MonadIO m, IsImage a) => O.OverloadedMethod ImageGetIconNameMethodInfo a signature where
    overloadedMethod = imageGetIconName

instance O.OverloadedMethodInfo ImageGetIconNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Image.imageGetIconName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Image.html#v:imageGetIconName"
        })


#endif

-- method Image::get_icon_set
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "image"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Image" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkImage" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icon_set"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconSet" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location to store a\n    #GtkIconSet, or %NULL"
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
--                 { rawDocText =
--                     Just
--                       "location to store a stock\n    icon size (#GtkIconSize), or %NULL"
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

foreign import ccall "gtk_image_get_icon_set" gtk_image_get_icon_set :: 
    Ptr Image ->                            -- image : TInterface (Name {namespace = "Gtk", name = "Image"})
    Ptr (Ptr Gtk.IconSet.IconSet) ->        -- icon_set : TInterface (Name {namespace = "Gtk", name = "IconSet"})
    Ptr Int32 ->                            -- size : TBasicType TInt
    IO ()

{-# DEPRECATED imageGetIconSet ["(Since version 3.10)","Use 'GI.Gtk.Objects.Image.imageGetIconName' instead."] #-}
-- | Gets the icon set and size being displayed by the t'GI.Gtk.Objects.Image.Image'.
-- The storage type of the image must be 'GI.Gtk.Enums.ImageTypeEmpty' or
-- 'GI.Gtk.Enums.ImageTypeIconSet' (see 'GI.Gtk.Objects.Image.imageGetStorageType').
imageGetIconSet ::
    (B.CallStack.HasCallStack, MonadIO m, IsImage a) =>
    a
    -- ^ /@image@/: a t'GI.Gtk.Objects.Image.Image'
    -> m ((Gtk.IconSet.IconSet, Int32))
imageGetIconSet image = liftIO $ do
    image' <- unsafeManagedPtrCastPtr image
    iconSet <- callocMem :: IO (Ptr (Ptr Gtk.IconSet.IconSet))
    size <- allocMem :: IO (Ptr Int32)
    gtk_image_get_icon_set image' iconSet size
    iconSet' <- peek iconSet
    iconSet'' <- (newBoxed Gtk.IconSet.IconSet) iconSet'
    size' <- peek size
    touchManagedPtr image
    freeMem iconSet
    freeMem size
    return (iconSet'', size')

#if defined(ENABLE_OVERLOADING)
data ImageGetIconSetMethodInfo
instance (signature ~ (m ((Gtk.IconSet.IconSet, Int32))), MonadIO m, IsImage a) => O.OverloadedMethod ImageGetIconSetMethodInfo a signature where
    overloadedMethod = imageGetIconSet

instance O.OverloadedMethodInfo ImageGetIconSetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Image.imageGetIconSet",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Image.html#v:imageGetIconSet"
        })


#endif

-- method Image::get_pixbuf
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "image"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Image" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkImage" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "GdkPixbuf" , name = "Pixbuf" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_image_get_pixbuf" gtk_image_get_pixbuf :: 
    Ptr Image ->                            -- image : TInterface (Name {namespace = "Gtk", name = "Image"})
    IO (Ptr GdkPixbuf.Pixbuf.Pixbuf)

-- | Gets the t'GI.GdkPixbuf.Objects.Pixbuf.Pixbuf' being displayed by the t'GI.Gtk.Objects.Image.Image'.
-- The storage type of the image must be 'GI.Gtk.Enums.ImageTypeEmpty' or
-- 'GI.Gtk.Enums.ImageTypePixbuf' (see 'GI.Gtk.Objects.Image.imageGetStorageType').
-- The caller of this function does not own a reference to the
-- returned pixbuf.
imageGetPixbuf ::
    (B.CallStack.HasCallStack, MonadIO m, IsImage a) =>
    a
    -- ^ /@image@/: a t'GI.Gtk.Objects.Image.Image'
    -> m (Maybe GdkPixbuf.Pixbuf.Pixbuf)
    -- ^ __Returns:__ the displayed pixbuf, or 'P.Nothing' if
    -- the image is empty
imageGetPixbuf image = liftIO $ do
    image' <- unsafeManagedPtrCastPtr image
    result <- gtk_image_get_pixbuf image'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject GdkPixbuf.Pixbuf.Pixbuf) result'
        return result''
    touchManagedPtr image
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data ImageGetPixbufMethodInfo
instance (signature ~ (m (Maybe GdkPixbuf.Pixbuf.Pixbuf)), MonadIO m, IsImage a) => O.OverloadedMethod ImageGetPixbufMethodInfo a signature where
    overloadedMethod = imageGetPixbuf

instance O.OverloadedMethodInfo ImageGetPixbufMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Image.imageGetPixbuf",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Image.html#v:imageGetPixbuf"
        })


#endif

-- method Image::get_pixel_size
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "image"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Image" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkImage" , sinceVersion = Nothing }
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

foreign import ccall "gtk_image_get_pixel_size" gtk_image_get_pixel_size :: 
    Ptr Image ->                            -- image : TInterface (Name {namespace = "Gtk", name = "Image"})
    IO Int32

-- | Gets the pixel size used for named icons.
-- 
-- /Since: 2.6/
imageGetPixelSize ::
    (B.CallStack.HasCallStack, MonadIO m, IsImage a) =>
    a
    -- ^ /@image@/: a t'GI.Gtk.Objects.Image.Image'
    -> m Int32
    -- ^ __Returns:__ the pixel size used for named icons.
imageGetPixelSize image = liftIO $ do
    image' <- unsafeManagedPtrCastPtr image
    result <- gtk_image_get_pixel_size image'
    touchManagedPtr image
    return result

#if defined(ENABLE_OVERLOADING)
data ImageGetPixelSizeMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsImage a) => O.OverloadedMethod ImageGetPixelSizeMethodInfo a signature where
    overloadedMethod = imageGetPixelSize

instance O.OverloadedMethodInfo ImageGetPixelSizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Image.imageGetPixelSize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Image.html#v:imageGetPixelSize"
        })


#endif

-- method Image::get_stock
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "image"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Image" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkImage" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "stock_id"
--           , argType = TBasicType TUTF8
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "place to store a\n    stock icon name, or %NULL"
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
--                 { rawDocText =
--                     Just
--                       "place to store a stock icon\n    size (#GtkIconSize), or %NULL"
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

foreign import ccall "gtk_image_get_stock" gtk_image_get_stock :: 
    Ptr Image ->                            -- image : TInterface (Name {namespace = "Gtk", name = "Image"})
    Ptr CString ->                          -- stock_id : TBasicType TUTF8
    Ptr Int32 ->                            -- size : TBasicType TInt
    IO ()

{-# DEPRECATED imageGetStock ["(Since version 3.10)","Use 'GI.Gtk.Objects.Image.imageGetIconName' instead."] #-}
-- | Gets the stock icon name and size being displayed by the t'GI.Gtk.Objects.Image.Image'.
-- The storage type of the image must be 'GI.Gtk.Enums.ImageTypeEmpty' or
-- 'GI.Gtk.Enums.ImageTypeStock' (see 'GI.Gtk.Objects.Image.imageGetStorageType').
-- The returned string is owned by the t'GI.Gtk.Objects.Image.Image' and should not
-- be freed.
imageGetStock ::
    (B.CallStack.HasCallStack, MonadIO m, IsImage a) =>
    a
    -- ^ /@image@/: a t'GI.Gtk.Objects.Image.Image'
    -> m ((T.Text, Int32))
imageGetStock image = liftIO $ do
    image' <- unsafeManagedPtrCastPtr image
    stockId <- callocMem :: IO (Ptr CString)
    size <- allocMem :: IO (Ptr Int32)
    gtk_image_get_stock image' stockId size
    stockId' <- peek stockId
    stockId'' <- cstringToText stockId'
    size' <- peek size
    touchManagedPtr image
    freeMem stockId
    freeMem size
    return (stockId'', size')

#if defined(ENABLE_OVERLOADING)
data ImageGetStockMethodInfo
instance (signature ~ (m ((T.Text, Int32))), MonadIO m, IsImage a) => O.OverloadedMethod ImageGetStockMethodInfo a signature where
    overloadedMethod = imageGetStock

instance O.OverloadedMethodInfo ImageGetStockMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Image.imageGetStock",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Image.html#v:imageGetStock"
        })


#endif

-- method Image::get_storage_type
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "image"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Image" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkImage" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "ImageType" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_image_get_storage_type" gtk_image_get_storage_type :: 
    Ptr Image ->                            -- image : TInterface (Name {namespace = "Gtk", name = "Image"})
    IO CUInt

-- | Gets the type of representation being used by the t'GI.Gtk.Objects.Image.Image'
-- to store image data. If the t'GI.Gtk.Objects.Image.Image' has no image data,
-- the return value will be 'GI.Gtk.Enums.ImageTypeEmpty'.
imageGetStorageType ::
    (B.CallStack.HasCallStack, MonadIO m, IsImage a) =>
    a
    -- ^ /@image@/: a t'GI.Gtk.Objects.Image.Image'
    -> m Gtk.Enums.ImageType
    -- ^ __Returns:__ image representation being used
imageGetStorageType image = liftIO $ do
    image' <- unsafeManagedPtrCastPtr image
    result <- gtk_image_get_storage_type image'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr image
    return result'

#if defined(ENABLE_OVERLOADING)
data ImageGetStorageTypeMethodInfo
instance (signature ~ (m Gtk.Enums.ImageType), MonadIO m, IsImage a) => O.OverloadedMethod ImageGetStorageTypeMethodInfo a signature where
    overloadedMethod = imageGetStorageType

instance O.OverloadedMethodInfo ImageGetStorageTypeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Image.imageGetStorageType",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Image.html#v:imageGetStorageType"
        })


#endif

-- method Image::set_from_animation
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "image"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Image" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkImage" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "animation"
--           , argType =
--               TInterface
--                 Name { namespace = "GdkPixbuf" , name = "PixbufAnimation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GdkPixbufAnimation"
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

foreign import ccall "gtk_image_set_from_animation" gtk_image_set_from_animation :: 
    Ptr Image ->                            -- image : TInterface (Name {namespace = "Gtk", name = "Image"})
    Ptr GdkPixbuf.PixbufAnimation.PixbufAnimation -> -- animation : TInterface (Name {namespace = "GdkPixbuf", name = "PixbufAnimation"})
    IO ()

-- | Causes the t'GI.Gtk.Objects.Image.Image' to display the given animation (or display
-- nothing, if you set the animation to 'P.Nothing').
imageSetFromAnimation ::
    (B.CallStack.HasCallStack, MonadIO m, IsImage a, GdkPixbuf.PixbufAnimation.IsPixbufAnimation b) =>
    a
    -- ^ /@image@/: a t'GI.Gtk.Objects.Image.Image'
    -> b
    -- ^ /@animation@/: the t'GI.GdkPixbuf.Objects.PixbufAnimation.PixbufAnimation'
    -> m ()
imageSetFromAnimation image animation = liftIO $ do
    image' <- unsafeManagedPtrCastPtr image
    animation' <- unsafeManagedPtrCastPtr animation
    gtk_image_set_from_animation image' animation'
    touchManagedPtr image
    touchManagedPtr animation
    return ()

#if defined(ENABLE_OVERLOADING)
data ImageSetFromAnimationMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsImage a, GdkPixbuf.PixbufAnimation.IsPixbufAnimation b) => O.OverloadedMethod ImageSetFromAnimationMethodInfo a signature where
    overloadedMethod = imageSetFromAnimation

instance O.OverloadedMethodInfo ImageSetFromAnimationMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Image.imageSetFromAnimation",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Image.html#v:imageSetFromAnimation"
        })


#endif

-- method Image::set_from_file
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "image"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Image" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkImage" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "filename"
--           , argType = TBasicType TFileName
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a filename or %NULL"
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

foreign import ccall "gtk_image_set_from_file" gtk_image_set_from_file :: 
    Ptr Image ->                            -- image : TInterface (Name {namespace = "Gtk", name = "Image"})
    CString ->                              -- filename : TBasicType TFileName
    IO ()

-- | See 'GI.Gtk.Objects.Image.imageNewFromFile' for details.
imageSetFromFile ::
    (B.CallStack.HasCallStack, MonadIO m, IsImage a) =>
    a
    -- ^ /@image@/: a t'GI.Gtk.Objects.Image.Image'
    -> Maybe ([Char])
    -- ^ /@filename@/: a filename or 'P.Nothing'
    -> m ()
imageSetFromFile image filename = liftIO $ do
    image' <- unsafeManagedPtrCastPtr image
    maybeFilename <- case filename of
        Nothing -> return nullPtr
        Just jFilename -> do
            jFilename' <- stringToCString jFilename
            return jFilename'
    gtk_image_set_from_file image' maybeFilename
    touchManagedPtr image
    freeMem maybeFilename
    return ()

#if defined(ENABLE_OVERLOADING)
data ImageSetFromFileMethodInfo
instance (signature ~ (Maybe ([Char]) -> m ()), MonadIO m, IsImage a) => O.OverloadedMethod ImageSetFromFileMethodInfo a signature where
    overloadedMethod = imageSetFromFile

instance O.OverloadedMethodInfo ImageSetFromFileMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Image.imageSetFromFile",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Image.html#v:imageSetFromFile"
        })


#endif

-- method Image::set_from_gicon
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "image"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Image" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkImage" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icon"
--           , argType = TInterface Name { namespace = "Gio" , name = "Icon" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an icon" , sinceVersion = Nothing }
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
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_image_set_from_gicon" gtk_image_set_from_gicon :: 
    Ptr Image ->                            -- image : TInterface (Name {namespace = "Gtk", name = "Image"})
    Ptr Gio.Icon.Icon ->                    -- icon : TInterface (Name {namespace = "Gio", name = "Icon"})
    Int32 ->                                -- size : TBasicType TInt
    IO ()

-- | See 'GI.Gtk.Objects.Image.imageNewFromGicon' for details.
-- 
-- /Since: 2.14/
imageSetFromGicon ::
    (B.CallStack.HasCallStack, MonadIO m, IsImage a, Gio.Icon.IsIcon b) =>
    a
    -- ^ /@image@/: a t'GI.Gtk.Objects.Image.Image'
    -> b
    -- ^ /@icon@/: an icon
    -> Int32
    -- ^ /@size@/: an icon size (t'GI.Gtk.Enums.IconSize')
    -> m ()
imageSetFromGicon image icon size = liftIO $ do
    image' <- unsafeManagedPtrCastPtr image
    icon' <- unsafeManagedPtrCastPtr icon
    gtk_image_set_from_gicon image' icon' size
    touchManagedPtr image
    touchManagedPtr icon
    return ()

#if defined(ENABLE_OVERLOADING)
data ImageSetFromGiconMethodInfo
instance (signature ~ (b -> Int32 -> m ()), MonadIO m, IsImage a, Gio.Icon.IsIcon b) => O.OverloadedMethod ImageSetFromGiconMethodInfo a signature where
    overloadedMethod = imageSetFromGicon

instance O.OverloadedMethodInfo ImageSetFromGiconMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Image.imageSetFromGicon",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Image.html#v:imageSetFromGicon"
        })


#endif

-- method Image::set_from_icon_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "image"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Image" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkImage" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
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
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_image_set_from_icon_name" gtk_image_set_from_icon_name :: 
    Ptr Image ->                            -- image : TInterface (Name {namespace = "Gtk", name = "Image"})
    CString ->                              -- icon_name : TBasicType TUTF8
    Int32 ->                                -- size : TBasicType TInt
    IO ()

-- | See 'GI.Gtk.Objects.Image.imageNewFromIconName' for details.
-- 
-- /Since: 2.6/
imageSetFromIconName ::
    (B.CallStack.HasCallStack, MonadIO m, IsImage a) =>
    a
    -- ^ /@image@/: a t'GI.Gtk.Objects.Image.Image'
    -> Maybe (T.Text)
    -- ^ /@iconName@/: an icon name or 'P.Nothing'
    -> Int32
    -- ^ /@size@/: an icon size (t'GI.Gtk.Enums.IconSize')
    -> m ()
imageSetFromIconName image iconName size = liftIO $ do
    image' <- unsafeManagedPtrCastPtr image
    maybeIconName <- case iconName of
        Nothing -> return nullPtr
        Just jIconName -> do
            jIconName' <- textToCString jIconName
            return jIconName'
    gtk_image_set_from_icon_name image' maybeIconName size
    touchManagedPtr image
    freeMem maybeIconName
    return ()

#if defined(ENABLE_OVERLOADING)
data ImageSetFromIconNameMethodInfo
instance (signature ~ (Maybe (T.Text) -> Int32 -> m ()), MonadIO m, IsImage a) => O.OverloadedMethod ImageSetFromIconNameMethodInfo a signature where
    overloadedMethod = imageSetFromIconName

instance O.OverloadedMethodInfo ImageSetFromIconNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Image.imageSetFromIconName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Image.html#v:imageSetFromIconName"
        })


#endif

-- method Image::set_from_icon_set
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "image"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Image" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkImage" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icon_set"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconSet" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkIconSet" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a stock icon size (#GtkIconSize)"
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

foreign import ccall "gtk_image_set_from_icon_set" gtk_image_set_from_icon_set :: 
    Ptr Image ->                            -- image : TInterface (Name {namespace = "Gtk", name = "Image"})
    Ptr Gtk.IconSet.IconSet ->              -- icon_set : TInterface (Name {namespace = "Gtk", name = "IconSet"})
    Int32 ->                                -- size : TBasicType TInt
    IO ()

{-# DEPRECATED imageSetFromIconSet ["(Since version 3.10)","Use 'GI.Gtk.Objects.Image.imageSetFromIconName' instead."] #-}
-- | See 'GI.Gtk.Objects.Image.imageNewFromIconSet' for details.
imageSetFromIconSet ::
    (B.CallStack.HasCallStack, MonadIO m, IsImage a) =>
    a
    -- ^ /@image@/: a t'GI.Gtk.Objects.Image.Image'
    -> Gtk.IconSet.IconSet
    -- ^ /@iconSet@/: a t'GI.Gtk.Structs.IconSet.IconSet'
    -> Int32
    -- ^ /@size@/: a stock icon size (t'GI.Gtk.Enums.IconSize')
    -> m ()
imageSetFromIconSet image iconSet size = liftIO $ do
    image' <- unsafeManagedPtrCastPtr image
    iconSet' <- unsafeManagedPtrGetPtr iconSet
    gtk_image_set_from_icon_set image' iconSet' size
    touchManagedPtr image
    touchManagedPtr iconSet
    return ()

#if defined(ENABLE_OVERLOADING)
data ImageSetFromIconSetMethodInfo
instance (signature ~ (Gtk.IconSet.IconSet -> Int32 -> m ()), MonadIO m, IsImage a) => O.OverloadedMethod ImageSetFromIconSetMethodInfo a signature where
    overloadedMethod = imageSetFromIconSet

instance O.OverloadedMethodInfo ImageSetFromIconSetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Image.imageSetFromIconSet",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Image.html#v:imageSetFromIconSet"
        })


#endif

-- method Image::set_from_pixbuf
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "image"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Image" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkImage" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pixbuf"
--           , argType =
--               TInterface Name { namespace = "GdkPixbuf" , name = "Pixbuf" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkPixbuf or %NULL"
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

foreign import ccall "gtk_image_set_from_pixbuf" gtk_image_set_from_pixbuf :: 
    Ptr Image ->                            -- image : TInterface (Name {namespace = "Gtk", name = "Image"})
    Ptr GdkPixbuf.Pixbuf.Pixbuf ->          -- pixbuf : TInterface (Name {namespace = "GdkPixbuf", name = "Pixbuf"})
    IO ()

-- | See 'GI.Gtk.Objects.Image.imageNewFromPixbuf' for details.
imageSetFromPixbuf ::
    (B.CallStack.HasCallStack, MonadIO m, IsImage a, GdkPixbuf.Pixbuf.IsPixbuf b) =>
    a
    -- ^ /@image@/: a t'GI.Gtk.Objects.Image.Image'
    -> Maybe (b)
    -- ^ /@pixbuf@/: a t'GI.GdkPixbuf.Objects.Pixbuf.Pixbuf' or 'P.Nothing'
    -> m ()
imageSetFromPixbuf image pixbuf = liftIO $ do
    image' <- unsafeManagedPtrCastPtr image
    maybePixbuf <- case pixbuf of
        Nothing -> return nullPtr
        Just jPixbuf -> do
            jPixbuf' <- unsafeManagedPtrCastPtr jPixbuf
            return jPixbuf'
    gtk_image_set_from_pixbuf image' maybePixbuf
    touchManagedPtr image
    whenJust pixbuf touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data ImageSetFromPixbufMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsImage a, GdkPixbuf.Pixbuf.IsPixbuf b) => O.OverloadedMethod ImageSetFromPixbufMethodInfo a signature where
    overloadedMethod = imageSetFromPixbuf

instance O.OverloadedMethodInfo ImageSetFromPixbufMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Image.imageSetFromPixbuf",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Image.html#v:imageSetFromPixbuf"
        })


#endif

-- method Image::set_from_resource
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "image"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Image" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkImage" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "resource_path"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a resource path or %NULL"
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

foreign import ccall "gtk_image_set_from_resource" gtk_image_set_from_resource :: 
    Ptr Image ->                            -- image : TInterface (Name {namespace = "Gtk", name = "Image"})
    CString ->                              -- resource_path : TBasicType TUTF8
    IO ()

-- | See 'GI.Gtk.Objects.Image.imageNewFromResource' for details.
imageSetFromResource ::
    (B.CallStack.HasCallStack, MonadIO m, IsImage a) =>
    a
    -- ^ /@image@/: a t'GI.Gtk.Objects.Image.Image'
    -> Maybe (T.Text)
    -- ^ /@resourcePath@/: a resource path or 'P.Nothing'
    -> m ()
imageSetFromResource image resourcePath = liftIO $ do
    image' <- unsafeManagedPtrCastPtr image
    maybeResourcePath <- case resourcePath of
        Nothing -> return nullPtr
        Just jResourcePath -> do
            jResourcePath' <- textToCString jResourcePath
            return jResourcePath'
    gtk_image_set_from_resource image' maybeResourcePath
    touchManagedPtr image
    freeMem maybeResourcePath
    return ()

#if defined(ENABLE_OVERLOADING)
data ImageSetFromResourceMethodInfo
instance (signature ~ (Maybe (T.Text) -> m ()), MonadIO m, IsImage a) => O.OverloadedMethod ImageSetFromResourceMethodInfo a signature where
    overloadedMethod = imageSetFromResource

instance O.OverloadedMethodInfo ImageSetFromResourceMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Image.imageSetFromResource",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Image.html#v:imageSetFromResource"
        })


#endif

-- method Image::set_from_stock
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "image"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Image" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkImage" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "stock_id"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a stock icon name" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a stock icon size (#GtkIconSize)"
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

foreign import ccall "gtk_image_set_from_stock" gtk_image_set_from_stock :: 
    Ptr Image ->                            -- image : TInterface (Name {namespace = "Gtk", name = "Image"})
    CString ->                              -- stock_id : TBasicType TUTF8
    Int32 ->                                -- size : TBasicType TInt
    IO ()

{-# DEPRECATED imageSetFromStock ["(Since version 3.10)","Use 'GI.Gtk.Objects.Image.imageSetFromIconName' instead."] #-}
-- | See 'GI.Gtk.Objects.Image.imageNewFromStock' for details.
imageSetFromStock ::
    (B.CallStack.HasCallStack, MonadIO m, IsImage a) =>
    a
    -- ^ /@image@/: a t'GI.Gtk.Objects.Image.Image'
    -> T.Text
    -- ^ /@stockId@/: a stock icon name
    -> Int32
    -- ^ /@size@/: a stock icon size (t'GI.Gtk.Enums.IconSize')
    -> m ()
imageSetFromStock image stockId size = liftIO $ do
    image' <- unsafeManagedPtrCastPtr image
    stockId' <- textToCString stockId
    gtk_image_set_from_stock image' stockId' size
    touchManagedPtr image
    freeMem stockId'
    return ()

#if defined(ENABLE_OVERLOADING)
data ImageSetFromStockMethodInfo
instance (signature ~ (T.Text -> Int32 -> m ()), MonadIO m, IsImage a) => O.OverloadedMethod ImageSetFromStockMethodInfo a signature where
    overloadedMethod = imageSetFromStock

instance O.OverloadedMethodInfo ImageSetFromStockMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Image.imageSetFromStock",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Image.html#v:imageSetFromStock"
        })


#endif

-- method Image::set_from_surface
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "image"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Image" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkImage" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "surface"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Surface" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a cairo_surface_t or %NULL"
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

foreign import ccall "gtk_image_set_from_surface" gtk_image_set_from_surface :: 
    Ptr Image ->                            -- image : TInterface (Name {namespace = "Gtk", name = "Image"})
    Ptr Cairo.Surface.Surface ->            -- surface : TInterface (Name {namespace = "cairo", name = "Surface"})
    IO ()

-- | See 'GI.Gtk.Objects.Image.imageNewFromSurface' for details.
-- 
-- /Since: 3.10/
imageSetFromSurface ::
    (B.CallStack.HasCallStack, MonadIO m, IsImage a) =>
    a
    -- ^ /@image@/: a t'GI.Gtk.Objects.Image.Image'
    -> Maybe (Cairo.Surface.Surface)
    -- ^ /@surface@/: a cairo_surface_t or 'P.Nothing'
    -> m ()
imageSetFromSurface image surface = liftIO $ do
    image' <- unsafeManagedPtrCastPtr image
    maybeSurface <- case surface of
        Nothing -> return nullPtr
        Just jSurface -> do
            jSurface' <- unsafeManagedPtrGetPtr jSurface
            return jSurface'
    gtk_image_set_from_surface image' maybeSurface
    touchManagedPtr image
    whenJust surface touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data ImageSetFromSurfaceMethodInfo
instance (signature ~ (Maybe (Cairo.Surface.Surface) -> m ()), MonadIO m, IsImage a) => O.OverloadedMethod ImageSetFromSurfaceMethodInfo a signature where
    overloadedMethod = imageSetFromSurface

instance O.OverloadedMethodInfo ImageSetFromSurfaceMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Image.imageSetFromSurface",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Image.html#v:imageSetFromSurface"
        })


#endif

-- method Image::set_pixel_size
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "image"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Image" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkImage" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pixel_size"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the new pixel size" , sinceVersion = Nothing }
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

foreign import ccall "gtk_image_set_pixel_size" gtk_image_set_pixel_size :: 
    Ptr Image ->                            -- image : TInterface (Name {namespace = "Gtk", name = "Image"})
    Int32 ->                                -- pixel_size : TBasicType TInt
    IO ()

-- | Sets the pixel size to use for named icons. If the pixel size is set
-- to a value != -1, it is used instead of the icon size set by
-- 'GI.Gtk.Objects.Image.imageSetFromIconName'.
-- 
-- /Since: 2.6/
imageSetPixelSize ::
    (B.CallStack.HasCallStack, MonadIO m, IsImage a) =>
    a
    -- ^ /@image@/: a t'GI.Gtk.Objects.Image.Image'
    -> Int32
    -- ^ /@pixelSize@/: the new pixel size
    -> m ()
imageSetPixelSize image pixelSize = liftIO $ do
    image' <- unsafeManagedPtrCastPtr image
    gtk_image_set_pixel_size image' pixelSize
    touchManagedPtr image
    return ()

#if defined(ENABLE_OVERLOADING)
data ImageSetPixelSizeMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsImage a) => O.OverloadedMethod ImageSetPixelSizeMethodInfo a signature where
    overloadedMethod = imageSetPixelSize

instance O.OverloadedMethodInfo ImageSetPixelSizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Image.imageSetPixelSize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Image.html#v:imageSetPixelSize"
        })


#endif


