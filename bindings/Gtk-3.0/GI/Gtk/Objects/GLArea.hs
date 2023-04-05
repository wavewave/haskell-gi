{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Objects.GLArea.GLArea' is a widget that allows drawing with OpenGL.
-- 
-- t'GI.Gtk.Objects.GLArea.GLArea' sets up its own t'GI.Gdk.Objects.GLContext.GLContext' for the window it creates, and
-- creates a custom GL framebuffer that the widget will do GL rendering onto.
-- It also ensures that this framebuffer is the default GL rendering target
-- when rendering.
-- 
-- In order to draw, you have to connect to the [GLArea::render]("GI.Gtk.Objects.GLArea#g:signal:render") signal,
-- or subclass t'GI.Gtk.Objects.GLArea.GLArea' and override the /@gtkGLAreaClass@/.@/render()/@ virtual
-- function.
-- 
-- The t'GI.Gtk.Objects.GLArea.GLArea' widget ensures that the t'GI.Gdk.Objects.GLContext.GLContext' is associated with
-- the widget\'s drawing area, and it is kept updated when the size and
-- position of the drawing area changes.
-- 
-- == Drawing with GtkGLArea 
-- 
-- The simplest way to draw using OpenGL commands in a t'GI.Gtk.Objects.GLArea.GLArea' is to
-- create a widget instance and connect to the [GLArea::render]("GI.Gtk.Objects.GLArea#g:signal:render") signal:
-- 
-- 
-- === /C code/
-- >
-- >  // create a GtkGLArea instance
-- >  GtkWidget *gl_area = gtk_gl_area_new ();
-- >
-- >  // connect to the "render" signal
-- >  g_signal_connect (gl_area, "render", G_CALLBACK (render), NULL);
-- 
-- 
-- The @render()@ function will be called when the t'GI.Gtk.Objects.GLArea.GLArea' is ready
-- for you to draw its content:
-- 
-- 
-- === /C code/
-- >
-- >  static gboolean
-- >  render (GtkGLArea *area, GdkGLContext *context)
-- >  {
-- >    // inside this function it's safe to use GL; the given
-- >    // #GdkGLContext has been made current to the drawable
-- >    // surface used by the #GtkGLArea and the viewport has
-- >    // already been set to be the size of the allocation
-- >
-- >    // we can start by clearing the buffer
-- >    glClearColor (0, 0, 0, 0);
-- >    glClear (GL_COLOR_BUFFER_BIT);
-- >
-- >    // draw your object
-- >    draw_an_object ();
-- >
-- >    // we completed our drawing; the draw commands will be
-- >    // flushed at the end of the signal emission chain, and
-- >    // the buffers will be drawn on the window
-- >    return TRUE;
-- >  }
-- 
-- 
-- If you need to initialize OpenGL state, e.g. buffer objects or
-- shaders, you should use the [Widget::realize]("GI.Gtk.Objects.Widget#g:signal:realize") signal; you
-- can use the [Widget::unrealize]("GI.Gtk.Objects.Widget#g:signal:unrealize") signal to clean up. Since the
-- t'GI.Gdk.Objects.GLContext.GLContext' creation and initialization may fail, you will
-- need to check for errors, using 'GI.Gtk.Objects.GLArea.gLAreaGetError'. An example
-- of how to safely initialize the GL state is:
-- 
-- 
-- === /C code/
-- >
-- >  static void
-- >  on_realize (GtkGLarea *area)
-- >  {
-- >    // We need to make the context current if we want to
-- >    // call GL API
-- >    gtk_gl_area_make_current (area);
-- >
-- >    // If there were errors during the initialization or
-- >    // when trying to make the context current, this
-- >    // function will return a #GError for you to catch
-- >    if (gtk_gl_area_get_error (area) != NULL)
-- >      return;
-- >
-- >    // You can also use gtk_gl_area_set_error() in order
-- >    // to show eventual initialization errors on the
-- >    // GtkGLArea widget itself
-- >    GError *internal_error = NULL;
-- >    init_buffer_objects (&error);
-- >    if (error != NULL)
-- >      {
-- >        gtk_gl_area_set_error (area, error);
-- >        g_error_free (error);
-- >        return;
-- >      }
-- >
-- >    init_shaders (&error);
-- >    if (error != NULL)
-- >      {
-- >        gtk_gl_area_set_error (area, error);
-- >        g_error_free (error);
-- >        return;
-- >      }
-- >  }
-- 
-- 
-- If you need to change the options for creating the t'GI.Gdk.Objects.GLContext.GLContext'
-- you should use the [GLArea::createContext]("GI.Gtk.Objects.GLArea#g:signal:createContext") signal.
-- 
-- /Since: 3.16/

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.GLArea
    ( 

-- * Exported types
    GLArea(..)                              ,
    IsGLArea                                ,
    toGLArea                                ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [attachBuffers]("GI.Gtk.Objects.GLArea#g:method:attachBuffers"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childNotify]("GI.Gtk.Objects.Widget#g:method:childNotify"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [makeCurrent]("GI.Gtk.Objects.GLArea#g:method:makeCurrent"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueRender]("GI.Gtk.Objects.GLArea#g:method:queueRender"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getAutoRender]("GI.Gtk.Objects.GLArea#g:method:getAutoRender"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getContext]("GI.Gtk.Objects.GLArea#g:method:getContext"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getError]("GI.Gtk.Objects.GLArea#g:method:getError"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasAlpha]("GI.Gtk.Objects.GLArea#g:method:getHasAlpha"), [getHasDepthBuffer]("GI.Gtk.Objects.GLArea#g:method:getHasDepthBuffer"), [getHasStencilBuffer]("GI.Gtk.Objects.GLArea#g:method:getHasStencilBuffer"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequiredVersion]("GI.Gtk.Objects.GLArea#g:method:getRequiredVersion"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getUseEs]("GI.Gtk.Objects.GLArea#g:method:getUseEs"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setAutoRender]("GI.Gtk.Objects.GLArea#g:method:setAutoRender"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setError]("GI.Gtk.Objects.GLArea#g:method:setError"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasAlpha]("GI.Gtk.Objects.GLArea#g:method:setHasAlpha"), [setHasDepthBuffer]("GI.Gtk.Objects.GLArea#g:method:setHasDepthBuffer"), [setHasStencilBuffer]("GI.Gtk.Objects.GLArea#g:method:setHasStencilBuffer"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setRequiredVersion]("GI.Gtk.Objects.GLArea#g:method:setRequiredVersion"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setUseEs]("GI.Gtk.Objects.GLArea#g:method:setUseEs"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveGLAreaMethod                     ,
#endif

-- ** attachBuffers #method:attachBuffers#

#if defined(ENABLE_OVERLOADING)
    GLAreaAttachBuffersMethodInfo           ,
#endif
    gLAreaAttachBuffers                     ,


-- ** getAutoRender #method:getAutoRender#

#if defined(ENABLE_OVERLOADING)
    GLAreaGetAutoRenderMethodInfo           ,
#endif
    gLAreaGetAutoRender                     ,


-- ** getContext #method:getContext#

#if defined(ENABLE_OVERLOADING)
    GLAreaGetContextMethodInfo              ,
#endif
    gLAreaGetContext                        ,


-- ** getError #method:getError#

#if defined(ENABLE_OVERLOADING)
    GLAreaGetErrorMethodInfo                ,
#endif
    gLAreaGetError                          ,


-- ** getHasAlpha #method:getHasAlpha#

#if defined(ENABLE_OVERLOADING)
    GLAreaGetHasAlphaMethodInfo             ,
#endif
    gLAreaGetHasAlpha                       ,


-- ** getHasDepthBuffer #method:getHasDepthBuffer#

#if defined(ENABLE_OVERLOADING)
    GLAreaGetHasDepthBufferMethodInfo       ,
#endif
    gLAreaGetHasDepthBuffer                 ,


-- ** getHasStencilBuffer #method:getHasStencilBuffer#

#if defined(ENABLE_OVERLOADING)
    GLAreaGetHasStencilBufferMethodInfo     ,
#endif
    gLAreaGetHasStencilBuffer               ,


-- ** getRequiredVersion #method:getRequiredVersion#

#if defined(ENABLE_OVERLOADING)
    GLAreaGetRequiredVersionMethodInfo      ,
#endif
    gLAreaGetRequiredVersion                ,


-- ** getUseEs #method:getUseEs#

#if defined(ENABLE_OVERLOADING)
    GLAreaGetUseEsMethodInfo                ,
#endif
    gLAreaGetUseEs                          ,


-- ** makeCurrent #method:makeCurrent#

#if defined(ENABLE_OVERLOADING)
    GLAreaMakeCurrentMethodInfo             ,
#endif
    gLAreaMakeCurrent                       ,


-- ** new #method:new#

    gLAreaNew                               ,


-- ** queueRender #method:queueRender#

#if defined(ENABLE_OVERLOADING)
    GLAreaQueueRenderMethodInfo             ,
#endif
    gLAreaQueueRender                       ,


-- ** setAutoRender #method:setAutoRender#

#if defined(ENABLE_OVERLOADING)
    GLAreaSetAutoRenderMethodInfo           ,
#endif
    gLAreaSetAutoRender                     ,


-- ** setError #method:setError#

#if defined(ENABLE_OVERLOADING)
    GLAreaSetErrorMethodInfo                ,
#endif
    gLAreaSetError                          ,


-- ** setHasAlpha #method:setHasAlpha#

#if defined(ENABLE_OVERLOADING)
    GLAreaSetHasAlphaMethodInfo             ,
#endif
    gLAreaSetHasAlpha                       ,


-- ** setHasDepthBuffer #method:setHasDepthBuffer#

#if defined(ENABLE_OVERLOADING)
    GLAreaSetHasDepthBufferMethodInfo       ,
#endif
    gLAreaSetHasDepthBuffer                 ,


-- ** setHasStencilBuffer #method:setHasStencilBuffer#

#if defined(ENABLE_OVERLOADING)
    GLAreaSetHasStencilBufferMethodInfo     ,
#endif
    gLAreaSetHasStencilBuffer               ,


-- ** setRequiredVersion #method:setRequiredVersion#

#if defined(ENABLE_OVERLOADING)
    GLAreaSetRequiredVersionMethodInfo      ,
#endif
    gLAreaSetRequiredVersion                ,


-- ** setUseEs #method:setUseEs#

#if defined(ENABLE_OVERLOADING)
    GLAreaSetUseEsMethodInfo                ,
#endif
    gLAreaSetUseEs                          ,




 -- * Properties


-- ** autoRender #attr:autoRender#
-- | If set to 'P.True' the [GLArea::render]("GI.Gtk.Objects.GLArea#g:signal:render") signal will be emitted every time
-- the widget draws. This is the default and is useful if drawing the widget
-- is faster.
-- 
-- If set to 'P.False' the data from previous rendering is kept around and will
-- be used for drawing the widget the next time, unless the window is resized.
-- In order to force a rendering 'GI.Gtk.Objects.GLArea.gLAreaQueueRender' must be called.
-- This mode is useful when the scene changes seldomly, but takes a long time
-- to redraw.
-- 
-- /Since: 3.16/

#if defined(ENABLE_OVERLOADING)
    GLAreaAutoRenderPropertyInfo            ,
#endif
    constructGLAreaAutoRender               ,
#if defined(ENABLE_OVERLOADING)
    gLAreaAutoRender                        ,
#endif
    getGLAreaAutoRender                     ,
    setGLAreaAutoRender                     ,


-- ** context #attr:context#
-- | The t'GI.Gdk.Objects.GLContext.GLContext' used by the t'GI.Gtk.Objects.GLArea.GLArea' widget.
-- 
-- The t'GI.Gtk.Objects.GLArea.GLArea' widget is responsible for creating the t'GI.Gdk.Objects.GLContext.GLContext'
-- instance. If you need to render with other kinds of buffers (stencil,
-- depth, etc), use render buffers.
-- 
-- /Since: 3.16/

#if defined(ENABLE_OVERLOADING)
    GLAreaContextPropertyInfo               ,
#endif
#if defined(ENABLE_OVERLOADING)
    gLAreaContext                           ,
#endif
    getGLAreaContext                        ,


-- ** hasAlpha #attr:hasAlpha#
-- | If set to 'P.True' the buffer allocated by the widget will have an alpha channel
-- component, and when rendering to the window the result will be composited over
-- whatever is below the widget.
-- 
-- If set to 'P.False' there will be no alpha channel, and the buffer will fully
-- replace anything below the widget.
-- 
-- /Since: 3.16/

#if defined(ENABLE_OVERLOADING)
    GLAreaHasAlphaPropertyInfo              ,
#endif
    constructGLAreaHasAlpha                 ,
#if defined(ENABLE_OVERLOADING)
    gLAreaHasAlpha                          ,
#endif
    getGLAreaHasAlpha                       ,
    setGLAreaHasAlpha                       ,


-- ** hasDepthBuffer #attr:hasDepthBuffer#
-- | If set to 'P.True' the widget will allocate and enable a depth buffer for the
-- target framebuffer.
-- 
-- /Since: 3.16/

#if defined(ENABLE_OVERLOADING)
    GLAreaHasDepthBufferPropertyInfo        ,
#endif
    constructGLAreaHasDepthBuffer           ,
#if defined(ENABLE_OVERLOADING)
    gLAreaHasDepthBuffer                    ,
#endif
    getGLAreaHasDepthBuffer                 ,
    setGLAreaHasDepthBuffer                 ,


-- ** hasStencilBuffer #attr:hasStencilBuffer#
-- | If set to 'P.True' the widget will allocate and enable a stencil buffer for the
-- target framebuffer.
-- 
-- /Since: 3.16/

#if defined(ENABLE_OVERLOADING)
    GLAreaHasStencilBufferPropertyInfo      ,
#endif
    constructGLAreaHasStencilBuffer         ,
#if defined(ENABLE_OVERLOADING)
    gLAreaHasStencilBuffer                  ,
#endif
    getGLAreaHasStencilBuffer               ,
    setGLAreaHasStencilBuffer               ,


-- ** useEs #attr:useEs#
-- | If set to 'P.True' the widget will try to create a t'GI.Gdk.Objects.GLContext.GLContext' using
-- OpenGL ES instead of OpenGL.
-- 
-- See also: 'GI.Gdk.Objects.GLContext.gLContextSetUseEs'
-- 
-- /Since: 3.22/

#if defined(ENABLE_OVERLOADING)
    GLAreaUseEsPropertyInfo                 ,
#endif
    constructGLAreaUseEs                    ,
#if defined(ENABLE_OVERLOADING)
    gLAreaUseEs                             ,
#endif
    getGLAreaUseEs                          ,
    setGLAreaUseEs                          ,




 -- * Signals


-- ** createContext #signal:createContext#

    GLAreaCreateContextCallback             ,
#if defined(ENABLE_OVERLOADING)
    GLAreaCreateContextSignalInfo           ,
#endif
    afterGLAreaCreateContext                ,
    onGLAreaCreateContext                   ,


-- ** render #signal:render#

    GLAreaRenderCallback                    ,
#if defined(ENABLE_OVERLOADING)
    GLAreaRenderSignalInfo                  ,
#endif
    afterGLAreaRender                       ,
    onGLAreaRender                          ,


-- ** resize #signal:resize#

    GLAreaResizeCallback                    ,
#if defined(ENABLE_OVERLOADING)
    GLAreaResizeSignalInfo                  ,
#endif
    afterGLAreaResize                       ,
    onGLAreaResize                          ,




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
import qualified GI.Gdk.Objects.GLContext as Gdk.GLContext
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype GLArea = GLArea (SP.ManagedPtr GLArea)
    deriving (Eq)

instance SP.ManagedPtrNewtype GLArea where
    toManagedPtr (GLArea p) = p

foreign import ccall "gtk_gl_area_get_type"
    c_gtk_gl_area_get_type :: IO B.Types.GType

instance B.Types.TypedObject GLArea where
    glibType = c_gtk_gl_area_get_type

instance B.Types.GObject GLArea

-- | Type class for types which can be safely cast to `GLArea`, for instance with `toGLArea`.
class (SP.GObject o, O.IsDescendantOf GLArea o) => IsGLArea o
instance (SP.GObject o, O.IsDescendantOf GLArea o) => IsGLArea o

instance O.HasParentTypes GLArea
type instance O.ParentTypes GLArea = '[Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable]

-- | Cast to `GLArea`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toGLArea :: (MIO.MonadIO m, IsGLArea o) => o -> m GLArea
toGLArea = MIO.liftIO . B.ManagedPtr.unsafeCastTo GLArea

-- | Convert 'GLArea' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe GLArea) where
    gvalueGType_ = c_gtk_gl_area_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr GLArea)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr GLArea)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject GLArea ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveGLAreaMethod (t :: Symbol) (o :: *) :: * where
    ResolveGLAreaMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveGLAreaMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveGLAreaMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveGLAreaMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveGLAreaMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveGLAreaMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveGLAreaMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveGLAreaMethod "attachBuffers" o = GLAreaAttachBuffersMethodInfo
    ResolveGLAreaMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveGLAreaMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveGLAreaMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveGLAreaMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveGLAreaMethod "childNotify" o = Gtk.Widget.WidgetChildNotifyMethodInfo
    ResolveGLAreaMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveGLAreaMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveGLAreaMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveGLAreaMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveGLAreaMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveGLAreaMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveGLAreaMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveGLAreaMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveGLAreaMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveGLAreaMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveGLAreaMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveGLAreaMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveGLAreaMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveGLAreaMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveGLAreaMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveGLAreaMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveGLAreaMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveGLAreaMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveGLAreaMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveGLAreaMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveGLAreaMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveGLAreaMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveGLAreaMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveGLAreaMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveGLAreaMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveGLAreaMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveGLAreaMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveGLAreaMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveGLAreaMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveGLAreaMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveGLAreaMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveGLAreaMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveGLAreaMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveGLAreaMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveGLAreaMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveGLAreaMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveGLAreaMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveGLAreaMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveGLAreaMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveGLAreaMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveGLAreaMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveGLAreaMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveGLAreaMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveGLAreaMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveGLAreaMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveGLAreaMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveGLAreaMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveGLAreaMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveGLAreaMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveGLAreaMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveGLAreaMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveGLAreaMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveGLAreaMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveGLAreaMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveGLAreaMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveGLAreaMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveGLAreaMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveGLAreaMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveGLAreaMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveGLAreaMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveGLAreaMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveGLAreaMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveGLAreaMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveGLAreaMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveGLAreaMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveGLAreaMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveGLAreaMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveGLAreaMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveGLAreaMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveGLAreaMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveGLAreaMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveGLAreaMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveGLAreaMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveGLAreaMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveGLAreaMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveGLAreaMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveGLAreaMethod "makeCurrent" o = GLAreaMakeCurrentMethodInfo
    ResolveGLAreaMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveGLAreaMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveGLAreaMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveGLAreaMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveGLAreaMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveGLAreaMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveGLAreaMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveGLAreaMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveGLAreaMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveGLAreaMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveGLAreaMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveGLAreaMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveGLAreaMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveGLAreaMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveGLAreaMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveGLAreaMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveGLAreaMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveGLAreaMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveGLAreaMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveGLAreaMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveGLAreaMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveGLAreaMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveGLAreaMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveGLAreaMethod "queueRender" o = GLAreaQueueRenderMethodInfo
    ResolveGLAreaMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveGLAreaMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveGLAreaMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveGLAreaMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveGLAreaMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveGLAreaMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveGLAreaMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveGLAreaMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveGLAreaMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveGLAreaMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveGLAreaMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveGLAreaMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveGLAreaMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveGLAreaMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveGLAreaMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveGLAreaMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveGLAreaMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveGLAreaMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveGLAreaMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveGLAreaMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveGLAreaMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveGLAreaMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveGLAreaMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveGLAreaMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveGLAreaMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveGLAreaMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveGLAreaMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveGLAreaMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveGLAreaMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveGLAreaMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveGLAreaMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveGLAreaMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveGLAreaMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveGLAreaMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveGLAreaMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveGLAreaMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveGLAreaMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveGLAreaMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveGLAreaMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveGLAreaMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveGLAreaMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveGLAreaMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveGLAreaMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveGLAreaMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveGLAreaMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveGLAreaMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveGLAreaMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveGLAreaMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveGLAreaMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveGLAreaMethod "getAutoRender" o = GLAreaGetAutoRenderMethodInfo
    ResolveGLAreaMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveGLAreaMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveGLAreaMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveGLAreaMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveGLAreaMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveGLAreaMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveGLAreaMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveGLAreaMethod "getContext" o = GLAreaGetContextMethodInfo
    ResolveGLAreaMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveGLAreaMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveGLAreaMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveGLAreaMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveGLAreaMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveGLAreaMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveGLAreaMethod "getError" o = GLAreaGetErrorMethodInfo
    ResolveGLAreaMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveGLAreaMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveGLAreaMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveGLAreaMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveGLAreaMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveGLAreaMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveGLAreaMethod "getHasAlpha" o = GLAreaGetHasAlphaMethodInfo
    ResolveGLAreaMethod "getHasDepthBuffer" o = GLAreaGetHasDepthBufferMethodInfo
    ResolveGLAreaMethod "getHasStencilBuffer" o = GLAreaGetHasStencilBufferMethodInfo
    ResolveGLAreaMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveGLAreaMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveGLAreaMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveGLAreaMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveGLAreaMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveGLAreaMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveGLAreaMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveGLAreaMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveGLAreaMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveGLAreaMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveGLAreaMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveGLAreaMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveGLAreaMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveGLAreaMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveGLAreaMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveGLAreaMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveGLAreaMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveGLAreaMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveGLAreaMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveGLAreaMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveGLAreaMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveGLAreaMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveGLAreaMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveGLAreaMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveGLAreaMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveGLAreaMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveGLAreaMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveGLAreaMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveGLAreaMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveGLAreaMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveGLAreaMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveGLAreaMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveGLAreaMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveGLAreaMethod "getRequiredVersion" o = GLAreaGetRequiredVersionMethodInfo
    ResolveGLAreaMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveGLAreaMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveGLAreaMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveGLAreaMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveGLAreaMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveGLAreaMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveGLAreaMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveGLAreaMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveGLAreaMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveGLAreaMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveGLAreaMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveGLAreaMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveGLAreaMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveGLAreaMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveGLAreaMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveGLAreaMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveGLAreaMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveGLAreaMethod "getUseEs" o = GLAreaGetUseEsMethodInfo
    ResolveGLAreaMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveGLAreaMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveGLAreaMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveGLAreaMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveGLAreaMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveGLAreaMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveGLAreaMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveGLAreaMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveGLAreaMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveGLAreaMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveGLAreaMethod "setAutoRender" o = GLAreaSetAutoRenderMethodInfo
    ResolveGLAreaMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveGLAreaMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveGLAreaMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveGLAreaMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveGLAreaMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveGLAreaMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveGLAreaMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveGLAreaMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveGLAreaMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveGLAreaMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveGLAreaMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveGLAreaMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveGLAreaMethod "setError" o = GLAreaSetErrorMethodInfo
    ResolveGLAreaMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveGLAreaMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveGLAreaMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveGLAreaMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveGLAreaMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveGLAreaMethod "setHasAlpha" o = GLAreaSetHasAlphaMethodInfo
    ResolveGLAreaMethod "setHasDepthBuffer" o = GLAreaSetHasDepthBufferMethodInfo
    ResolveGLAreaMethod "setHasStencilBuffer" o = GLAreaSetHasStencilBufferMethodInfo
    ResolveGLAreaMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveGLAreaMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveGLAreaMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveGLAreaMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveGLAreaMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveGLAreaMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveGLAreaMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveGLAreaMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveGLAreaMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveGLAreaMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveGLAreaMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveGLAreaMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveGLAreaMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveGLAreaMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveGLAreaMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveGLAreaMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveGLAreaMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveGLAreaMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveGLAreaMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveGLAreaMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveGLAreaMethod "setRequiredVersion" o = GLAreaSetRequiredVersionMethodInfo
    ResolveGLAreaMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveGLAreaMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveGLAreaMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveGLAreaMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveGLAreaMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveGLAreaMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveGLAreaMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveGLAreaMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveGLAreaMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveGLAreaMethod "setUseEs" o = GLAreaSetUseEsMethodInfo
    ResolveGLAreaMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveGLAreaMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveGLAreaMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveGLAreaMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveGLAreaMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveGLAreaMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveGLAreaMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveGLAreaMethod t GLArea, O.OverloadedMethod info GLArea p) => OL.IsLabel t (GLArea -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveGLAreaMethod t GLArea, O.OverloadedMethod info GLArea p, R.HasField t GLArea p) => R.HasField t GLArea p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveGLAreaMethod t GLArea, O.OverloadedMethodInfo info GLArea) => OL.IsLabel t (O.MethodProxy info GLArea) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal GLArea::create-context
-- | The [createContext](#g:signal:createContext) signal is emitted when the widget is being
-- realized, and allows you to override how the GL context is
-- created. This is useful when you want to reuse an existing GL
-- context, or if you want to try creating different kinds of GL
-- options.
-- 
-- If context creation fails then the signal handler can use
-- 'GI.Gtk.Objects.GLArea.gLAreaSetError' to register a more detailed error
-- of how the construction failed.
-- 
-- /Since: 3.16/
type GLAreaCreateContextCallback =
    IO Gdk.GLContext.GLContext
    -- ^ __Returns:__ a newly created t'GI.Gdk.Objects.GLContext.GLContext';
    --     the t'GI.Gtk.Objects.GLArea.GLArea' widget will take ownership of the returned value.

type C_GLAreaCreateContextCallback =
    Ptr GLArea ->                           -- object
    Ptr () ->                               -- user_data
    IO (Ptr Gdk.GLContext.GLContext)

-- | Generate a function pointer callable from C code, from a `C_GLAreaCreateContextCallback`.
foreign import ccall "wrapper"
    mk_GLAreaCreateContextCallback :: C_GLAreaCreateContextCallback -> IO (FunPtr C_GLAreaCreateContextCallback)

wrap_GLAreaCreateContextCallback :: 
    GObject a => (a -> GLAreaCreateContextCallback) ->
    C_GLAreaCreateContextCallback
wrap_GLAreaCreateContextCallback gi'cb gi'selfPtr _ = do
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 
    result' <- B.ManagedPtr.disownObject result
    return result'


-- | Connect a signal handler for the [createContext](#signal:createContext) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' gLArea #createContext callback
-- @
-- 
-- 
onGLAreaCreateContext :: (IsGLArea a, MonadIO m) => a -> ((?self :: a) => GLAreaCreateContextCallback) -> m SignalHandlerId
onGLAreaCreateContext obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_GLAreaCreateContextCallback wrapped
    wrapped'' <- mk_GLAreaCreateContextCallback wrapped'
    connectSignalFunPtr obj "create-context" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [createContext](#signal:createContext) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' gLArea #createContext callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterGLAreaCreateContext :: (IsGLArea a, MonadIO m) => a -> ((?self :: a) => GLAreaCreateContextCallback) -> m SignalHandlerId
afterGLAreaCreateContext obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_GLAreaCreateContextCallback wrapped
    wrapped'' <- mk_GLAreaCreateContextCallback wrapped'
    connectSignalFunPtr obj "create-context" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data GLAreaCreateContextSignalInfo
instance SignalInfo GLAreaCreateContextSignalInfo where
    type HaskellCallbackType GLAreaCreateContextSignalInfo = GLAreaCreateContextCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_GLAreaCreateContextCallback cb
        cb'' <- mk_GLAreaCreateContextCallback cb'
        connectSignalFunPtr obj "create-context" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GLArea::create-context"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GLArea.html#g:signal:createContext"})

#endif

-- signal GLArea::render
-- | The [render](#g:signal:render) signal is emitted every time the contents
-- of the t'GI.Gtk.Objects.GLArea.GLArea' should be redrawn.
-- 
-- The /@context@/ is bound to the /@area@/ prior to emitting this function,
-- and the buffers are painted to the window once the emission terminates.
-- 
-- /Since: 3.16/
type GLAreaRenderCallback =
    Gdk.GLContext.GLContext
    -- ^ /@context@/: the t'GI.Gdk.Objects.GLContext.GLContext' used by /@area@/
    -> IO Bool
    -- ^ __Returns:__ 'P.True' to stop other handlers from being invoked for the event.
    --   'P.False' to propagate the event further.

type C_GLAreaRenderCallback =
    Ptr GLArea ->                           -- object
    Ptr Gdk.GLContext.GLContext ->
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_GLAreaRenderCallback`.
foreign import ccall "wrapper"
    mk_GLAreaRenderCallback :: C_GLAreaRenderCallback -> IO (FunPtr C_GLAreaRenderCallback)

wrap_GLAreaRenderCallback :: 
    GObject a => (a -> GLAreaRenderCallback) ->
    C_GLAreaRenderCallback
wrap_GLAreaRenderCallback gi'cb gi'selfPtr context _ = do
    context' <- (newObject Gdk.GLContext.GLContext) context
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  context'
    let result' = (fromIntegral . fromEnum) result
    return result'


-- | Connect a signal handler for the [render](#signal:render) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' gLArea #render callback
-- @
-- 
-- 
onGLAreaRender :: (IsGLArea a, MonadIO m) => a -> ((?self :: a) => GLAreaRenderCallback) -> m SignalHandlerId
onGLAreaRender obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_GLAreaRenderCallback wrapped
    wrapped'' <- mk_GLAreaRenderCallback wrapped'
    connectSignalFunPtr obj "render" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [render](#signal:render) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' gLArea #render callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterGLAreaRender :: (IsGLArea a, MonadIO m) => a -> ((?self :: a) => GLAreaRenderCallback) -> m SignalHandlerId
afterGLAreaRender obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_GLAreaRenderCallback wrapped
    wrapped'' <- mk_GLAreaRenderCallback wrapped'
    connectSignalFunPtr obj "render" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data GLAreaRenderSignalInfo
instance SignalInfo GLAreaRenderSignalInfo where
    type HaskellCallbackType GLAreaRenderSignalInfo = GLAreaRenderCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_GLAreaRenderCallback cb
        cb'' <- mk_GLAreaRenderCallback cb'
        connectSignalFunPtr obj "render" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GLArea::render"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GLArea.html#g:signal:render"})

#endif

-- signal GLArea::resize
-- | The [resize](#g:signal:resize) signal is emitted once when the widget is realized, and
-- then each time the widget is changed while realized. This is useful
-- in order to keep GL state up to date with the widget size, like for
-- instance camera properties which may depend on the width\/height ratio.
-- 
-- The GL context for the area is guaranteed to be current when this signal
-- is emitted.
-- 
-- The default handler sets up the GL viewport.
-- 
-- /Since: 3.16/
type GLAreaResizeCallback =
    Int32
    -- ^ /@width@/: the width of the viewport
    -> Int32
    -- ^ /@height@/: the height of the viewport
    -> IO ()

type C_GLAreaResizeCallback =
    Ptr GLArea ->                           -- object
    Int32 ->
    Int32 ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_GLAreaResizeCallback`.
foreign import ccall "wrapper"
    mk_GLAreaResizeCallback :: C_GLAreaResizeCallback -> IO (FunPtr C_GLAreaResizeCallback)

wrap_GLAreaResizeCallback :: 
    GObject a => (a -> GLAreaResizeCallback) ->
    C_GLAreaResizeCallback
wrap_GLAreaResizeCallback gi'cb gi'selfPtr width height _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  width height


-- | Connect a signal handler for the [resize](#signal:resize) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' gLArea #resize callback
-- @
-- 
-- 
onGLAreaResize :: (IsGLArea a, MonadIO m) => a -> ((?self :: a) => GLAreaResizeCallback) -> m SignalHandlerId
onGLAreaResize obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_GLAreaResizeCallback wrapped
    wrapped'' <- mk_GLAreaResizeCallback wrapped'
    connectSignalFunPtr obj "resize" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [resize](#signal:resize) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' gLArea #resize callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterGLAreaResize :: (IsGLArea a, MonadIO m) => a -> ((?self :: a) => GLAreaResizeCallback) -> m SignalHandlerId
afterGLAreaResize obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_GLAreaResizeCallback wrapped
    wrapped'' <- mk_GLAreaResizeCallback wrapped'
    connectSignalFunPtr obj "resize" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data GLAreaResizeSignalInfo
instance SignalInfo GLAreaResizeSignalInfo where
    type HaskellCallbackType GLAreaResizeSignalInfo = GLAreaResizeCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_GLAreaResizeCallback cb
        cb'' <- mk_GLAreaResizeCallback cb'
        connectSignalFunPtr obj "resize" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GLArea::resize"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GLArea.html#g:signal:resize"})

#endif

-- VVV Prop "auto-render"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@auto-render@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' gLArea #autoRender
-- @
getGLAreaAutoRender :: (MonadIO m, IsGLArea o) => o -> m Bool
getGLAreaAutoRender obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "auto-render"

-- | Set the value of the “@auto-render@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' gLArea [ #autoRender 'Data.GI.Base.Attributes.:=' value ]
-- @
setGLAreaAutoRender :: (MonadIO m, IsGLArea o) => o -> Bool -> m ()
setGLAreaAutoRender obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "auto-render" val

-- | Construct a `GValueConstruct` with valid value for the “@auto-render@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructGLAreaAutoRender :: (IsGLArea o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructGLAreaAutoRender val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "auto-render" val

#if defined(ENABLE_OVERLOADING)
data GLAreaAutoRenderPropertyInfo
instance AttrInfo GLAreaAutoRenderPropertyInfo where
    type AttrAllowedOps GLAreaAutoRenderPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint GLAreaAutoRenderPropertyInfo = IsGLArea
    type AttrSetTypeConstraint GLAreaAutoRenderPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint GLAreaAutoRenderPropertyInfo = (~) Bool
    type AttrTransferType GLAreaAutoRenderPropertyInfo = Bool
    type AttrGetType GLAreaAutoRenderPropertyInfo = Bool
    type AttrLabel GLAreaAutoRenderPropertyInfo = "auto-render"
    type AttrOrigin GLAreaAutoRenderPropertyInfo = GLArea
    attrGet = getGLAreaAutoRender
    attrSet = setGLAreaAutoRender
    attrTransfer _ v = do
        return v
    attrConstruct = constructGLAreaAutoRender
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GLArea.autoRender"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GLArea.html#g:attr:autoRender"
        })
#endif

-- VVV Prop "context"
   -- Type: TInterface (Name {namespace = "Gdk", name = "GLContext"})
   -- Flags: [PropertyReadable]
   -- Nullable: (Just False,Nothing)

-- | Get the value of the “@context@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' gLArea #context
-- @
getGLAreaContext :: (MonadIO m, IsGLArea o) => o -> m Gdk.GLContext.GLContext
getGLAreaContext obj = MIO.liftIO $ checkUnexpectedNothing "getGLAreaContext" $ B.Properties.getObjectPropertyObject obj "context" Gdk.GLContext.GLContext

#if defined(ENABLE_OVERLOADING)
data GLAreaContextPropertyInfo
instance AttrInfo GLAreaContextPropertyInfo where
    type AttrAllowedOps GLAreaContextPropertyInfo = '[ 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint GLAreaContextPropertyInfo = IsGLArea
    type AttrSetTypeConstraint GLAreaContextPropertyInfo = (~) ()
    type AttrTransferTypeConstraint GLAreaContextPropertyInfo = (~) ()
    type AttrTransferType GLAreaContextPropertyInfo = ()
    type AttrGetType GLAreaContextPropertyInfo = Gdk.GLContext.GLContext
    type AttrLabel GLAreaContextPropertyInfo = "context"
    type AttrOrigin GLAreaContextPropertyInfo = GLArea
    attrGet = getGLAreaContext
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GLArea.context"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GLArea.html#g:attr:context"
        })
#endif

-- VVV Prop "has-alpha"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@has-alpha@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' gLArea #hasAlpha
-- @
getGLAreaHasAlpha :: (MonadIO m, IsGLArea o) => o -> m Bool
getGLAreaHasAlpha obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "has-alpha"

-- | Set the value of the “@has-alpha@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' gLArea [ #hasAlpha 'Data.GI.Base.Attributes.:=' value ]
-- @
setGLAreaHasAlpha :: (MonadIO m, IsGLArea o) => o -> Bool -> m ()
setGLAreaHasAlpha obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "has-alpha" val

-- | Construct a `GValueConstruct` with valid value for the “@has-alpha@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructGLAreaHasAlpha :: (IsGLArea o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructGLAreaHasAlpha val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "has-alpha" val

#if defined(ENABLE_OVERLOADING)
data GLAreaHasAlphaPropertyInfo
instance AttrInfo GLAreaHasAlphaPropertyInfo where
    type AttrAllowedOps GLAreaHasAlphaPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint GLAreaHasAlphaPropertyInfo = IsGLArea
    type AttrSetTypeConstraint GLAreaHasAlphaPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint GLAreaHasAlphaPropertyInfo = (~) Bool
    type AttrTransferType GLAreaHasAlphaPropertyInfo = Bool
    type AttrGetType GLAreaHasAlphaPropertyInfo = Bool
    type AttrLabel GLAreaHasAlphaPropertyInfo = "has-alpha"
    type AttrOrigin GLAreaHasAlphaPropertyInfo = GLArea
    attrGet = getGLAreaHasAlpha
    attrSet = setGLAreaHasAlpha
    attrTransfer _ v = do
        return v
    attrConstruct = constructGLAreaHasAlpha
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GLArea.hasAlpha"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GLArea.html#g:attr:hasAlpha"
        })
#endif

-- VVV Prop "has-depth-buffer"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@has-depth-buffer@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' gLArea #hasDepthBuffer
-- @
getGLAreaHasDepthBuffer :: (MonadIO m, IsGLArea o) => o -> m Bool
getGLAreaHasDepthBuffer obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "has-depth-buffer"

-- | Set the value of the “@has-depth-buffer@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' gLArea [ #hasDepthBuffer 'Data.GI.Base.Attributes.:=' value ]
-- @
setGLAreaHasDepthBuffer :: (MonadIO m, IsGLArea o) => o -> Bool -> m ()
setGLAreaHasDepthBuffer obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "has-depth-buffer" val

-- | Construct a `GValueConstruct` with valid value for the “@has-depth-buffer@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructGLAreaHasDepthBuffer :: (IsGLArea o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructGLAreaHasDepthBuffer val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "has-depth-buffer" val

#if defined(ENABLE_OVERLOADING)
data GLAreaHasDepthBufferPropertyInfo
instance AttrInfo GLAreaHasDepthBufferPropertyInfo where
    type AttrAllowedOps GLAreaHasDepthBufferPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint GLAreaHasDepthBufferPropertyInfo = IsGLArea
    type AttrSetTypeConstraint GLAreaHasDepthBufferPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint GLAreaHasDepthBufferPropertyInfo = (~) Bool
    type AttrTransferType GLAreaHasDepthBufferPropertyInfo = Bool
    type AttrGetType GLAreaHasDepthBufferPropertyInfo = Bool
    type AttrLabel GLAreaHasDepthBufferPropertyInfo = "has-depth-buffer"
    type AttrOrigin GLAreaHasDepthBufferPropertyInfo = GLArea
    attrGet = getGLAreaHasDepthBuffer
    attrSet = setGLAreaHasDepthBuffer
    attrTransfer _ v = do
        return v
    attrConstruct = constructGLAreaHasDepthBuffer
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GLArea.hasDepthBuffer"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GLArea.html#g:attr:hasDepthBuffer"
        })
#endif

-- VVV Prop "has-stencil-buffer"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@has-stencil-buffer@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' gLArea #hasStencilBuffer
-- @
getGLAreaHasStencilBuffer :: (MonadIO m, IsGLArea o) => o -> m Bool
getGLAreaHasStencilBuffer obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "has-stencil-buffer"

-- | Set the value of the “@has-stencil-buffer@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' gLArea [ #hasStencilBuffer 'Data.GI.Base.Attributes.:=' value ]
-- @
setGLAreaHasStencilBuffer :: (MonadIO m, IsGLArea o) => o -> Bool -> m ()
setGLAreaHasStencilBuffer obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "has-stencil-buffer" val

-- | Construct a `GValueConstruct` with valid value for the “@has-stencil-buffer@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructGLAreaHasStencilBuffer :: (IsGLArea o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructGLAreaHasStencilBuffer val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "has-stencil-buffer" val

#if defined(ENABLE_OVERLOADING)
data GLAreaHasStencilBufferPropertyInfo
instance AttrInfo GLAreaHasStencilBufferPropertyInfo where
    type AttrAllowedOps GLAreaHasStencilBufferPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint GLAreaHasStencilBufferPropertyInfo = IsGLArea
    type AttrSetTypeConstraint GLAreaHasStencilBufferPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint GLAreaHasStencilBufferPropertyInfo = (~) Bool
    type AttrTransferType GLAreaHasStencilBufferPropertyInfo = Bool
    type AttrGetType GLAreaHasStencilBufferPropertyInfo = Bool
    type AttrLabel GLAreaHasStencilBufferPropertyInfo = "has-stencil-buffer"
    type AttrOrigin GLAreaHasStencilBufferPropertyInfo = GLArea
    attrGet = getGLAreaHasStencilBuffer
    attrSet = setGLAreaHasStencilBuffer
    attrTransfer _ v = do
        return v
    attrConstruct = constructGLAreaHasStencilBuffer
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GLArea.hasStencilBuffer"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GLArea.html#g:attr:hasStencilBuffer"
        })
#endif

-- VVV Prop "use-es"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@use-es@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' gLArea #useEs
-- @
getGLAreaUseEs :: (MonadIO m, IsGLArea o) => o -> m Bool
getGLAreaUseEs obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "use-es"

-- | Set the value of the “@use-es@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' gLArea [ #useEs 'Data.GI.Base.Attributes.:=' value ]
-- @
setGLAreaUseEs :: (MonadIO m, IsGLArea o) => o -> Bool -> m ()
setGLAreaUseEs obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "use-es" val

-- | Construct a `GValueConstruct` with valid value for the “@use-es@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructGLAreaUseEs :: (IsGLArea o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructGLAreaUseEs val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "use-es" val

#if defined(ENABLE_OVERLOADING)
data GLAreaUseEsPropertyInfo
instance AttrInfo GLAreaUseEsPropertyInfo where
    type AttrAllowedOps GLAreaUseEsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint GLAreaUseEsPropertyInfo = IsGLArea
    type AttrSetTypeConstraint GLAreaUseEsPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint GLAreaUseEsPropertyInfo = (~) Bool
    type AttrTransferType GLAreaUseEsPropertyInfo = Bool
    type AttrGetType GLAreaUseEsPropertyInfo = Bool
    type AttrLabel GLAreaUseEsPropertyInfo = "use-es"
    type AttrOrigin GLAreaUseEsPropertyInfo = GLArea
    attrGet = getGLAreaUseEs
    attrSet = setGLAreaUseEs
    attrTransfer _ v = do
        return v
    attrConstruct = constructGLAreaUseEs
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GLArea.useEs"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GLArea.html#g:attr:useEs"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList GLArea
type instance O.AttributeList GLArea = GLAreaAttributeList
type GLAreaAttributeList = ('[ '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("autoRender", GLAreaAutoRenderPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("context", GLAreaContextPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasAlpha", GLAreaHasAlphaPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasDepthBuffer", GLAreaHasDepthBufferPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasStencilBuffer", GLAreaHasStencilBufferPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("useEs", GLAreaUseEsPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
gLAreaAutoRender :: AttrLabelProxy "autoRender"
gLAreaAutoRender = AttrLabelProxy

gLAreaContext :: AttrLabelProxy "context"
gLAreaContext = AttrLabelProxy

gLAreaHasAlpha :: AttrLabelProxy "hasAlpha"
gLAreaHasAlpha = AttrLabelProxy

gLAreaHasDepthBuffer :: AttrLabelProxy "hasDepthBuffer"
gLAreaHasDepthBuffer = AttrLabelProxy

gLAreaHasStencilBuffer :: AttrLabelProxy "hasStencilBuffer"
gLAreaHasStencilBuffer = AttrLabelProxy

gLAreaUseEs :: AttrLabelProxy "useEs"
gLAreaUseEs = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList GLArea = GLAreaSignalList
type GLAreaSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("createContext", GLAreaCreateContextSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("render", GLAreaRenderSignalInfo), '("resize", GLAreaResizeSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method GLArea::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "GLArea" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_gl_area_new" gtk_gl_area_new :: 
    IO (Ptr GLArea)

-- | Creates a new t'GI.Gtk.Objects.GLArea.GLArea' widget.
-- 
-- /Since: 3.16/
gLAreaNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m GLArea
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.GLArea.GLArea'
gLAreaNew  = liftIO $ do
    result <- gtk_gl_area_new
    checkUnexpectedReturnNULL "gLAreaNew" result
    result' <- (newObject GLArea) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method GLArea::attach_buffers
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType = TInterface Name { namespace = "Gtk" , name = "GLArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGLArea" , sinceVersion = Nothing }
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

foreign import ccall "gtk_gl_area_attach_buffers" gtk_gl_area_attach_buffers :: 
    Ptr GLArea ->                           -- area : TInterface (Name {namespace = "Gtk", name = "GLArea"})
    IO ()

-- | Ensures that the /@area@/ framebuffer object is made the current draw
-- and read target, and that all the required buffers for the /@area@/
-- are created and bound to the frambuffer.
-- 
-- This function is automatically called before emitting the
-- [GLArea::render]("GI.Gtk.Objects.GLArea#g:signal:render") signal, and doesn\'t normally need to be called
-- by application code.
-- 
-- /Since: 3.16/
gLAreaAttachBuffers ::
    (B.CallStack.HasCallStack, MonadIO m, IsGLArea a) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.GLArea.GLArea'
    -> m ()
gLAreaAttachBuffers area = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    gtk_gl_area_attach_buffers area'
    touchManagedPtr area
    return ()

#if defined(ENABLE_OVERLOADING)
data GLAreaAttachBuffersMethodInfo
instance (signature ~ (m ()), MonadIO m, IsGLArea a) => O.OverloadedMethod GLAreaAttachBuffersMethodInfo a signature where
    overloadedMethod = gLAreaAttachBuffers

instance O.OverloadedMethodInfo GLAreaAttachBuffersMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GLArea.gLAreaAttachBuffers",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GLArea.html#v:gLAreaAttachBuffers"
        })


#endif

-- method GLArea::get_auto_render
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType = TInterface Name { namespace = "Gtk" , name = "GLArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGLArea" , sinceVersion = Nothing }
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

foreign import ccall "gtk_gl_area_get_auto_render" gtk_gl_area_get_auto_render :: 
    Ptr GLArea ->                           -- area : TInterface (Name {namespace = "Gtk", name = "GLArea"})
    IO CInt

-- | Returns whether the area is in auto render mode or not.
-- 
-- /Since: 3.16/
gLAreaGetAutoRender ::
    (B.CallStack.HasCallStack, MonadIO m, IsGLArea a) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.GLArea.GLArea'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the /@area@/ is auto rendering, 'P.False' otherwise
gLAreaGetAutoRender area = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    result <- gtk_gl_area_get_auto_render area'
    let result' = (/= 0) result
    touchManagedPtr area
    return result'

#if defined(ENABLE_OVERLOADING)
data GLAreaGetAutoRenderMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsGLArea a) => O.OverloadedMethod GLAreaGetAutoRenderMethodInfo a signature where
    overloadedMethod = gLAreaGetAutoRender

instance O.OverloadedMethodInfo GLAreaGetAutoRenderMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GLArea.gLAreaGetAutoRender",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GLArea.html#v:gLAreaGetAutoRender"
        })


#endif

-- method GLArea::get_context
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType = TInterface Name { namespace = "Gtk" , name = "GLArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGLArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gdk" , name = "GLContext" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_gl_area_get_context" gtk_gl_area_get_context :: 
    Ptr GLArea ->                           -- area : TInterface (Name {namespace = "Gtk", name = "GLArea"})
    IO (Ptr Gdk.GLContext.GLContext)

-- | Retrieves the t'GI.Gdk.Objects.GLContext.GLContext' used by /@area@/.
-- 
-- /Since: 3.16/
gLAreaGetContext ::
    (B.CallStack.HasCallStack, MonadIO m, IsGLArea a) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.GLArea.GLArea'
    -> m Gdk.GLContext.GLContext
    -- ^ __Returns:__ the t'GI.Gdk.Objects.GLContext.GLContext'
gLAreaGetContext area = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    result <- gtk_gl_area_get_context area'
    checkUnexpectedReturnNULL "gLAreaGetContext" result
    result' <- (newObject Gdk.GLContext.GLContext) result
    touchManagedPtr area
    return result'

#if defined(ENABLE_OVERLOADING)
data GLAreaGetContextMethodInfo
instance (signature ~ (m Gdk.GLContext.GLContext), MonadIO m, IsGLArea a) => O.OverloadedMethod GLAreaGetContextMethodInfo a signature where
    overloadedMethod = gLAreaGetContext

instance O.OverloadedMethodInfo GLAreaGetContextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GLArea.gLAreaGetContext",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GLArea.html#v:gLAreaGetContext"
        })


#endif

-- method GLArea::get_error
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType = TInterface Name { namespace = "Gtk" , name = "GLArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGLArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just TError
-- throws : False
-- Skip return : False

foreign import ccall "gtk_gl_area_get_error" gtk_gl_area_get_error :: 
    Ptr GLArea ->                           -- area : TInterface (Name {namespace = "Gtk", name = "GLArea"})
    IO (Ptr GError)

-- | Gets the current error set on the /@area@/.
-- 
-- /Since: 3.16/
gLAreaGetError ::
    (B.CallStack.HasCallStack, MonadIO m, IsGLArea a) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.GLArea.GLArea'
    -> m (Maybe GError)
    -- ^ __Returns:__ the t'GError' or 'P.Nothing'
gLAreaGetError area = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    result <- gtk_gl_area_get_error area'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newBoxed GError) result'
        return result''
    touchManagedPtr area
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data GLAreaGetErrorMethodInfo
instance (signature ~ (m (Maybe GError)), MonadIO m, IsGLArea a) => O.OverloadedMethod GLAreaGetErrorMethodInfo a signature where
    overloadedMethod = gLAreaGetError

instance O.OverloadedMethodInfo GLAreaGetErrorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GLArea.gLAreaGetError",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GLArea.html#v:gLAreaGetError"
        })


#endif

-- method GLArea::get_has_alpha
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType = TInterface Name { namespace = "Gtk" , name = "GLArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGLArea" , sinceVersion = Nothing }
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

foreign import ccall "gtk_gl_area_get_has_alpha" gtk_gl_area_get_has_alpha :: 
    Ptr GLArea ->                           -- area : TInterface (Name {namespace = "Gtk", name = "GLArea"})
    IO CInt

-- | Returns whether the area has an alpha component.
-- 
-- /Since: 3.16/
gLAreaGetHasAlpha ::
    (B.CallStack.HasCallStack, MonadIO m, IsGLArea a) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.GLArea.GLArea'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the /@area@/ has an alpha component, 'P.False' otherwise
gLAreaGetHasAlpha area = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    result <- gtk_gl_area_get_has_alpha area'
    let result' = (/= 0) result
    touchManagedPtr area
    return result'

#if defined(ENABLE_OVERLOADING)
data GLAreaGetHasAlphaMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsGLArea a) => O.OverloadedMethod GLAreaGetHasAlphaMethodInfo a signature where
    overloadedMethod = gLAreaGetHasAlpha

instance O.OverloadedMethodInfo GLAreaGetHasAlphaMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GLArea.gLAreaGetHasAlpha",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GLArea.html#v:gLAreaGetHasAlpha"
        })


#endif

-- method GLArea::get_has_depth_buffer
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType = TInterface Name { namespace = "Gtk" , name = "GLArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGLArea" , sinceVersion = Nothing }
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

foreign import ccall "gtk_gl_area_get_has_depth_buffer" gtk_gl_area_get_has_depth_buffer :: 
    Ptr GLArea ->                           -- area : TInterface (Name {namespace = "Gtk", name = "GLArea"})
    IO CInt

-- | Returns whether the area has a depth buffer.
-- 
-- /Since: 3.16/
gLAreaGetHasDepthBuffer ::
    (B.CallStack.HasCallStack, MonadIO m, IsGLArea a) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.GLArea.GLArea'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the /@area@/ has a depth buffer, 'P.False' otherwise
gLAreaGetHasDepthBuffer area = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    result <- gtk_gl_area_get_has_depth_buffer area'
    let result' = (/= 0) result
    touchManagedPtr area
    return result'

#if defined(ENABLE_OVERLOADING)
data GLAreaGetHasDepthBufferMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsGLArea a) => O.OverloadedMethod GLAreaGetHasDepthBufferMethodInfo a signature where
    overloadedMethod = gLAreaGetHasDepthBuffer

instance O.OverloadedMethodInfo GLAreaGetHasDepthBufferMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GLArea.gLAreaGetHasDepthBuffer",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GLArea.html#v:gLAreaGetHasDepthBuffer"
        })


#endif

-- method GLArea::get_has_stencil_buffer
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType = TInterface Name { namespace = "Gtk" , name = "GLArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGLArea" , sinceVersion = Nothing }
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

foreign import ccall "gtk_gl_area_get_has_stencil_buffer" gtk_gl_area_get_has_stencil_buffer :: 
    Ptr GLArea ->                           -- area : TInterface (Name {namespace = "Gtk", name = "GLArea"})
    IO CInt

-- | Returns whether the area has a stencil buffer.
-- 
-- /Since: 3.16/
gLAreaGetHasStencilBuffer ::
    (B.CallStack.HasCallStack, MonadIO m, IsGLArea a) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.GLArea.GLArea'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the /@area@/ has a stencil buffer, 'P.False' otherwise
gLAreaGetHasStencilBuffer area = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    result <- gtk_gl_area_get_has_stencil_buffer area'
    let result' = (/= 0) result
    touchManagedPtr area
    return result'

#if defined(ENABLE_OVERLOADING)
data GLAreaGetHasStencilBufferMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsGLArea a) => O.OverloadedMethod GLAreaGetHasStencilBufferMethodInfo a signature where
    overloadedMethod = gLAreaGetHasStencilBuffer

instance O.OverloadedMethodInfo GLAreaGetHasStencilBufferMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GLArea.gLAreaGetHasStencilBuffer",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GLArea.html#v:gLAreaGetHasStencilBuffer"
        })


#endif

-- method GLArea::get_required_version
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType = TInterface Name { namespace = "Gtk" , name = "GLArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGLArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "major"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "return location for the required major version"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "minor"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "return location for the required minor version"
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

foreign import ccall "gtk_gl_area_get_required_version" gtk_gl_area_get_required_version :: 
    Ptr GLArea ->                           -- area : TInterface (Name {namespace = "Gtk", name = "GLArea"})
    Ptr Int32 ->                            -- major : TBasicType TInt
    Ptr Int32 ->                            -- minor : TBasicType TInt
    IO ()

-- | Retrieves the required version of OpenGL set
-- using 'GI.Gtk.Objects.GLArea.gLAreaSetRequiredVersion'.
-- 
-- /Since: 3.16/
gLAreaGetRequiredVersion ::
    (B.CallStack.HasCallStack, MonadIO m, IsGLArea a) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.GLArea.GLArea'
    -> m ((Int32, Int32))
gLAreaGetRequiredVersion area = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    major <- allocMem :: IO (Ptr Int32)
    minor <- allocMem :: IO (Ptr Int32)
    gtk_gl_area_get_required_version area' major minor
    major' <- peek major
    minor' <- peek minor
    touchManagedPtr area
    freeMem major
    freeMem minor
    return (major', minor')

#if defined(ENABLE_OVERLOADING)
data GLAreaGetRequiredVersionMethodInfo
instance (signature ~ (m ((Int32, Int32))), MonadIO m, IsGLArea a) => O.OverloadedMethod GLAreaGetRequiredVersionMethodInfo a signature where
    overloadedMethod = gLAreaGetRequiredVersion

instance O.OverloadedMethodInfo GLAreaGetRequiredVersionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GLArea.gLAreaGetRequiredVersion",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GLArea.html#v:gLAreaGetRequiredVersion"
        })


#endif

-- method GLArea::get_use_es
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType = TInterface Name { namespace = "Gtk" , name = "GLArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGLArea" , sinceVersion = Nothing }
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

foreign import ccall "gtk_gl_area_get_use_es" gtk_gl_area_get_use_es :: 
    Ptr GLArea ->                           -- area : TInterface (Name {namespace = "Gtk", name = "GLArea"})
    IO CInt

-- | Retrieves the value set by 'GI.Gtk.Objects.GLArea.gLAreaSetUseEs'.
-- 
-- /Since: 3.22/
gLAreaGetUseEs ::
    (B.CallStack.HasCallStack, MonadIO m, IsGLArea a) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.GLArea.GLArea'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the t'GI.Gtk.Objects.GLArea.GLArea' should create an OpenGL ES context
    --   and 'P.False' otherwise
gLAreaGetUseEs area = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    result <- gtk_gl_area_get_use_es area'
    let result' = (/= 0) result
    touchManagedPtr area
    return result'

#if defined(ENABLE_OVERLOADING)
data GLAreaGetUseEsMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsGLArea a) => O.OverloadedMethod GLAreaGetUseEsMethodInfo a signature where
    overloadedMethod = gLAreaGetUseEs

instance O.OverloadedMethodInfo GLAreaGetUseEsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GLArea.gLAreaGetUseEs",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GLArea.html#v:gLAreaGetUseEs"
        })


#endif

-- method GLArea::make_current
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType = TInterface Name { namespace = "Gtk" , name = "GLArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGLArea" , sinceVersion = Nothing }
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

foreign import ccall "gtk_gl_area_make_current" gtk_gl_area_make_current :: 
    Ptr GLArea ->                           -- area : TInterface (Name {namespace = "Gtk", name = "GLArea"})
    IO ()

-- | Ensures that the t'GI.Gdk.Objects.GLContext.GLContext' used by /@area@/ is associated with
-- the t'GI.Gtk.Objects.GLArea.GLArea'.
-- 
-- This function is automatically called before emitting the
-- [GLArea::render]("GI.Gtk.Objects.GLArea#g:signal:render") signal, and doesn\'t normally need to be called
-- by application code.
-- 
-- /Since: 3.16/
gLAreaMakeCurrent ::
    (B.CallStack.HasCallStack, MonadIO m, IsGLArea a) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.GLArea.GLArea'
    -> m ()
gLAreaMakeCurrent area = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    gtk_gl_area_make_current area'
    touchManagedPtr area
    return ()

#if defined(ENABLE_OVERLOADING)
data GLAreaMakeCurrentMethodInfo
instance (signature ~ (m ()), MonadIO m, IsGLArea a) => O.OverloadedMethod GLAreaMakeCurrentMethodInfo a signature where
    overloadedMethod = gLAreaMakeCurrent

instance O.OverloadedMethodInfo GLAreaMakeCurrentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GLArea.gLAreaMakeCurrent",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GLArea.html#v:gLAreaMakeCurrent"
        })


#endif

-- method GLArea::queue_render
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType = TInterface Name { namespace = "Gtk" , name = "GLArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGLArea" , sinceVersion = Nothing }
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

foreign import ccall "gtk_gl_area_queue_render" gtk_gl_area_queue_render :: 
    Ptr GLArea ->                           -- area : TInterface (Name {namespace = "Gtk", name = "GLArea"})
    IO ()

-- | Marks the currently rendered data (if any) as invalid, and queues
-- a redraw of the widget, ensuring that the [GLArea::render]("GI.Gtk.Objects.GLArea#g:signal:render") signal
-- is emitted during the draw.
-- 
-- This is only needed when the 'GI.Gtk.Objects.GLArea.gLAreaSetAutoRender' has
-- been called with a 'P.False' value. The default behaviour is to
-- emit [GLArea::render]("GI.Gtk.Objects.GLArea#g:signal:render") on each draw.
-- 
-- /Since: 3.16/
gLAreaQueueRender ::
    (B.CallStack.HasCallStack, MonadIO m, IsGLArea a) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.GLArea.GLArea'
    -> m ()
gLAreaQueueRender area = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    gtk_gl_area_queue_render area'
    touchManagedPtr area
    return ()

#if defined(ENABLE_OVERLOADING)
data GLAreaQueueRenderMethodInfo
instance (signature ~ (m ()), MonadIO m, IsGLArea a) => O.OverloadedMethod GLAreaQueueRenderMethodInfo a signature where
    overloadedMethod = gLAreaQueueRender

instance O.OverloadedMethodInfo GLAreaQueueRenderMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GLArea.gLAreaQueueRender",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GLArea.html#v:gLAreaQueueRender"
        })


#endif

-- method GLArea::set_auto_render
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType = TInterface Name { namespace = "Gtk" , name = "GLArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGLArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "auto_render"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a boolean" , sinceVersion = Nothing }
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

foreign import ccall "gtk_gl_area_set_auto_render" gtk_gl_area_set_auto_render :: 
    Ptr GLArea ->                           -- area : TInterface (Name {namespace = "Gtk", name = "GLArea"})
    CInt ->                                 -- auto_render : TBasicType TBoolean
    IO ()

-- | If /@autoRender@/ is 'P.True' the [GLArea::render]("GI.Gtk.Objects.GLArea#g:signal:render") signal will be
-- emitted every time the widget draws. This is the default and is
-- useful if drawing the widget is faster.
-- 
-- If /@autoRender@/ is 'P.False' the data from previous rendering is kept
-- around and will be used for drawing the widget the next time,
-- unless the window is resized. In order to force a rendering
-- 'GI.Gtk.Objects.GLArea.gLAreaQueueRender' must be called. This mode is useful when
-- the scene changes seldomly, but takes a long time to redraw.
-- 
-- /Since: 3.16/
gLAreaSetAutoRender ::
    (B.CallStack.HasCallStack, MonadIO m, IsGLArea a) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.GLArea.GLArea'
    -> Bool
    -- ^ /@autoRender@/: a boolean
    -> m ()
gLAreaSetAutoRender area autoRender = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    let autoRender' = (fromIntegral . fromEnum) autoRender
    gtk_gl_area_set_auto_render area' autoRender'
    touchManagedPtr area
    return ()

#if defined(ENABLE_OVERLOADING)
data GLAreaSetAutoRenderMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsGLArea a) => O.OverloadedMethod GLAreaSetAutoRenderMethodInfo a signature where
    overloadedMethod = gLAreaSetAutoRender

instance O.OverloadedMethodInfo GLAreaSetAutoRenderMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GLArea.gLAreaSetAutoRender",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GLArea.html#v:gLAreaSetAutoRender"
        })


#endif

-- method GLArea::set_error
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType = TInterface Name { namespace = "Gtk" , name = "GLArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGLArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "error"
--           , argType = TError
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a new #GError, or %NULL to unset the error"
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

foreign import ccall "gtk_gl_area_set_error" gtk_gl_area_set_error :: 
    Ptr GLArea ->                           -- area : TInterface (Name {namespace = "Gtk", name = "GLArea"})
    Ptr GError ->                           -- error : TError
    IO ()

-- | Sets an error on the area which will be shown instead of the
-- GL rendering. This is useful in the [GLArea::createContext]("GI.Gtk.Objects.GLArea#g:signal:createContext")
-- signal if GL context creation fails.
-- 
-- /Since: 3.16/
gLAreaSetError ::
    (B.CallStack.HasCallStack, MonadIO m, IsGLArea a) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.GLArea.GLArea'
    -> Maybe (GError)
    -- ^ /@error@/: a new t'GError', or 'P.Nothing' to unset the error
    -> m ()
gLAreaSetError area error_ = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    maybeError_ <- case error_ of
        Nothing -> return nullPtr
        Just jError_ -> do
            jError_' <- unsafeManagedPtrGetPtr jError_
            return jError_'
    gtk_gl_area_set_error area' maybeError_
    touchManagedPtr area
    whenJust error_ touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data GLAreaSetErrorMethodInfo
instance (signature ~ (Maybe (GError) -> m ()), MonadIO m, IsGLArea a) => O.OverloadedMethod GLAreaSetErrorMethodInfo a signature where
    overloadedMethod = gLAreaSetError

instance O.OverloadedMethodInfo GLAreaSetErrorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GLArea.gLAreaSetError",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GLArea.html#v:gLAreaSetError"
        })


#endif

-- method GLArea::set_has_alpha
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType = TInterface Name { namespace = "Gtk" , name = "GLArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGLArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "has_alpha"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE to add an alpha component"
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

foreign import ccall "gtk_gl_area_set_has_alpha" gtk_gl_area_set_has_alpha :: 
    Ptr GLArea ->                           -- area : TInterface (Name {namespace = "Gtk", name = "GLArea"})
    CInt ->                                 -- has_alpha : TBasicType TBoolean
    IO ()

-- | If /@hasAlpha@/ is 'P.True' the buffer allocated by the widget will have
-- an alpha channel component, and when rendering to the window the
-- result will be composited over whatever is below the widget.
-- 
-- If /@hasAlpha@/ is 'P.False' there will be no alpha channel, and the
-- buffer will fully replace anything below the widget.
-- 
-- /Since: 3.16/
gLAreaSetHasAlpha ::
    (B.CallStack.HasCallStack, MonadIO m, IsGLArea a) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.GLArea.GLArea'
    -> Bool
    -- ^ /@hasAlpha@/: 'P.True' to add an alpha component
    -> m ()
gLAreaSetHasAlpha area hasAlpha = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    let hasAlpha' = (fromIntegral . fromEnum) hasAlpha
    gtk_gl_area_set_has_alpha area' hasAlpha'
    touchManagedPtr area
    return ()

#if defined(ENABLE_OVERLOADING)
data GLAreaSetHasAlphaMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsGLArea a) => O.OverloadedMethod GLAreaSetHasAlphaMethodInfo a signature where
    overloadedMethod = gLAreaSetHasAlpha

instance O.OverloadedMethodInfo GLAreaSetHasAlphaMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GLArea.gLAreaSetHasAlpha",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GLArea.html#v:gLAreaSetHasAlpha"
        })


#endif

-- method GLArea::set_has_depth_buffer
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType = TInterface Name { namespace = "Gtk" , name = "GLArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGLArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "has_depth_buffer"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE to add a depth buffer"
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

foreign import ccall "gtk_gl_area_set_has_depth_buffer" gtk_gl_area_set_has_depth_buffer :: 
    Ptr GLArea ->                           -- area : TInterface (Name {namespace = "Gtk", name = "GLArea"})
    CInt ->                                 -- has_depth_buffer : TBasicType TBoolean
    IO ()

-- | If /@hasDepthBuffer@/ is 'P.True' the widget will allocate and
-- enable a depth buffer for the target framebuffer. Otherwise
-- there will be none.
-- 
-- /Since: 3.16/
gLAreaSetHasDepthBuffer ::
    (B.CallStack.HasCallStack, MonadIO m, IsGLArea a) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.GLArea.GLArea'
    -> Bool
    -- ^ /@hasDepthBuffer@/: 'P.True' to add a depth buffer
    -> m ()
gLAreaSetHasDepthBuffer area hasDepthBuffer = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    let hasDepthBuffer' = (fromIntegral . fromEnum) hasDepthBuffer
    gtk_gl_area_set_has_depth_buffer area' hasDepthBuffer'
    touchManagedPtr area
    return ()

#if defined(ENABLE_OVERLOADING)
data GLAreaSetHasDepthBufferMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsGLArea a) => O.OverloadedMethod GLAreaSetHasDepthBufferMethodInfo a signature where
    overloadedMethod = gLAreaSetHasDepthBuffer

instance O.OverloadedMethodInfo GLAreaSetHasDepthBufferMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GLArea.gLAreaSetHasDepthBuffer",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GLArea.html#v:gLAreaSetHasDepthBuffer"
        })


#endif

-- method GLArea::set_has_stencil_buffer
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType = TInterface Name { namespace = "Gtk" , name = "GLArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGLArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "has_stencil_buffer"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE to add a stencil buffer"
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

foreign import ccall "gtk_gl_area_set_has_stencil_buffer" gtk_gl_area_set_has_stencil_buffer :: 
    Ptr GLArea ->                           -- area : TInterface (Name {namespace = "Gtk", name = "GLArea"})
    CInt ->                                 -- has_stencil_buffer : TBasicType TBoolean
    IO ()

-- | If /@hasStencilBuffer@/ is 'P.True' the widget will allocate and
-- enable a stencil buffer for the target framebuffer. Otherwise
-- there will be none.
-- 
-- /Since: 3.16/
gLAreaSetHasStencilBuffer ::
    (B.CallStack.HasCallStack, MonadIO m, IsGLArea a) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.GLArea.GLArea'
    -> Bool
    -- ^ /@hasStencilBuffer@/: 'P.True' to add a stencil buffer
    -> m ()
gLAreaSetHasStencilBuffer area hasStencilBuffer = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    let hasStencilBuffer' = (fromIntegral . fromEnum) hasStencilBuffer
    gtk_gl_area_set_has_stencil_buffer area' hasStencilBuffer'
    touchManagedPtr area
    return ()

#if defined(ENABLE_OVERLOADING)
data GLAreaSetHasStencilBufferMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsGLArea a) => O.OverloadedMethod GLAreaSetHasStencilBufferMethodInfo a signature where
    overloadedMethod = gLAreaSetHasStencilBuffer

instance O.OverloadedMethodInfo GLAreaSetHasStencilBufferMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GLArea.gLAreaSetHasStencilBuffer",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GLArea.html#v:gLAreaSetHasStencilBuffer"
        })


#endif

-- method GLArea::set_required_version
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType = TInterface Name { namespace = "Gtk" , name = "GLArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGLArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "major"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the major version" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "minor"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the minor version" , sinceVersion = Nothing }
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

foreign import ccall "gtk_gl_area_set_required_version" gtk_gl_area_set_required_version :: 
    Ptr GLArea ->                           -- area : TInterface (Name {namespace = "Gtk", name = "GLArea"})
    Int32 ->                                -- major : TBasicType TInt
    Int32 ->                                -- minor : TBasicType TInt
    IO ()

-- | Sets the required version of OpenGL to be used when creating the context
-- for the widget.
-- 
-- This function must be called before the area has been realized.
-- 
-- /Since: 3.16/
gLAreaSetRequiredVersion ::
    (B.CallStack.HasCallStack, MonadIO m, IsGLArea a) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.GLArea.GLArea'
    -> Int32
    -- ^ /@major@/: the major version
    -> Int32
    -- ^ /@minor@/: the minor version
    -> m ()
gLAreaSetRequiredVersion area major minor = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    gtk_gl_area_set_required_version area' major minor
    touchManagedPtr area
    return ()

#if defined(ENABLE_OVERLOADING)
data GLAreaSetRequiredVersionMethodInfo
instance (signature ~ (Int32 -> Int32 -> m ()), MonadIO m, IsGLArea a) => O.OverloadedMethod GLAreaSetRequiredVersionMethodInfo a signature where
    overloadedMethod = gLAreaSetRequiredVersion

instance O.OverloadedMethodInfo GLAreaSetRequiredVersionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GLArea.gLAreaSetRequiredVersion",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GLArea.html#v:gLAreaSetRequiredVersion"
        })


#endif

-- method GLArea::set_use_es
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "area"
--           , argType = TInterface Name { namespace = "Gtk" , name = "GLArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkGLArea" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "use_es"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "whether to use OpenGL or OpenGL ES"
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

foreign import ccall "gtk_gl_area_set_use_es" gtk_gl_area_set_use_es :: 
    Ptr GLArea ->                           -- area : TInterface (Name {namespace = "Gtk", name = "GLArea"})
    CInt ->                                 -- use_es : TBasicType TBoolean
    IO ()

-- | Sets whether the /@area@/ should create an OpenGL or an OpenGL ES context.
-- 
-- You should check the capabilities of the t'GI.Gdk.Objects.GLContext.GLContext' before drawing
-- with either API.
-- 
-- /Since: 3.22/
gLAreaSetUseEs ::
    (B.CallStack.HasCallStack, MonadIO m, IsGLArea a) =>
    a
    -- ^ /@area@/: a t'GI.Gtk.Objects.GLArea.GLArea'
    -> Bool
    -- ^ /@useEs@/: whether to use OpenGL or OpenGL ES
    -> m ()
gLAreaSetUseEs area useEs = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    let useEs' = (fromIntegral . fromEnum) useEs
    gtk_gl_area_set_use_es area' useEs'
    touchManagedPtr area
    return ()

#if defined(ENABLE_OVERLOADING)
data GLAreaSetUseEsMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsGLArea a) => O.OverloadedMethod GLAreaSetUseEsMethodInfo a signature where
    overloadedMethod = gLAreaSetUseEs

instance O.OverloadedMethodInfo GLAreaSetUseEsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.GLArea.gLAreaSetUseEs",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-GLArea.html#v:gLAreaSetUseEs"
        })


#endif


