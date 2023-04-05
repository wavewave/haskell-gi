{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A t'GI.Gtk.Objects.ToolPalette.ToolPalette' allows you to add @/GtkToolItems/@ to a palette-like
-- container with different categories and drag and drop support.
-- 
-- A t'GI.Gtk.Objects.ToolPalette.ToolPalette' is created with a call to 'GI.Gtk.Objects.ToolPalette.toolPaletteNew'.
-- 
-- @/GtkToolItems/@ cannot be added directly to a t'GI.Gtk.Objects.ToolPalette.ToolPalette' -
-- instead they are added to a t'GI.Gtk.Objects.ToolItemGroup.ToolItemGroup' which can than be added
-- to a t'GI.Gtk.Objects.ToolPalette.ToolPalette'. To add a t'GI.Gtk.Objects.ToolItemGroup.ToolItemGroup' to a t'GI.Gtk.Objects.ToolPalette.ToolPalette',
-- use 'GI.Gtk.Objects.Container.containerAdd'.
-- 
-- 
-- === /C code/
-- >
-- >GtkWidget *palette, *group;
-- >GtkToolItem *item;
-- >
-- >palette = gtk_tool_palette_new ();
-- >group = gtk_tool_item_group_new (_("Test Category"));
-- >gtk_container_add (GTK_CONTAINER (palette), group);
-- >
-- >item = gtk_tool_button_new (NULL, _("_Open"));
-- >gtk_tool_button_set_icon_name (GTK_TOOL_BUTTON (item), "document-open");
-- >gtk_tool_item_group_insert (GTK_TOOL_ITEM_GROUP (group), item, -1);
-- 
-- 
-- The easiest way to use drag and drop with t'GI.Gtk.Objects.ToolPalette.ToolPalette' is to call
-- 'GI.Gtk.Objects.ToolPalette.toolPaletteAddDragDest' with the desired drag source /@palette@/
-- and the desired drag target /@widget@/. Then 'GI.Gtk.Objects.ToolPalette.toolPaletteGetDragItem'
-- can be used to get the dragged item in the [Widget::dragDataReceived]("GI.Gtk.Objects.Widget#g:signal:dragDataReceived")
-- signal handler of the drag target.
-- 
-- 
-- === /C code/
-- >
-- >static void
-- >passive_canvas_drag_data_received (GtkWidget        *widget,
-- >                                   GdkDragContext   *context,
-- >                                   gint              x,
-- >                                   gint              y,
-- >                                   GtkSelectionData *selection,
-- >                                   guint             info,
-- >                                   guint             time,
-- >                                   gpointer          data)
-- >{
-- >  GtkWidget *palette;
-- >  GtkWidget *item;
-- >
-- >  // Get the dragged item
-- >  palette = gtk_widget_get_ancestor (gtk_drag_get_source_widget (context),
-- >                                     GTK_TYPE_TOOL_PALETTE);
-- >  if (palette != NULL)
-- >    item = gtk_tool_palette_get_drag_item (GTK_TOOL_PALETTE (palette),
-- >                                           selection);
-- >
-- >  // Do something with item
-- >}
-- >
-- >GtkWidget *target, palette;
-- >
-- >palette = gtk_tool_palette_new ();
-- >target = gtk_drawing_area_new ();
-- >
-- >g_signal_connect (G_OBJECT (target), "drag-data-received",
-- >                  G_CALLBACK (passive_canvas_drag_data_received), NULL);
-- >gtk_tool_palette_add_drag_dest (GTK_TOOL_PALETTE (palette), target,
-- >                                GTK_DEST_DEFAULT_ALL,
-- >                                GTK_TOOL_PALETTE_DRAG_ITEMS,
-- >                                GDK_ACTION_COPY);
-- 
-- 
-- = CSS nodes
-- 
-- GtkToolPalette has a single CSS node named toolpalette.
-- 
-- /Since: 2.20/

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.ToolPalette
    ( 

-- * Exported types
    ToolPalette(..)                         ,
    IsToolPalette                           ,
    toToolPalette                           ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addDragDest]("GI.Gtk.Objects.ToolPalette#g:method:addDragDest"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetIconSize]("GI.Gtk.Objects.ToolPalette#g:method:unsetIconSize"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [unsetStyle]("GI.Gtk.Objects.ToolPalette#g:method:unsetStyle"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getBorder]("GI.Gtk.Interfaces.Scrollable#g:method:getBorder"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getDragItem]("GI.Gtk.Objects.ToolPalette#g:method:getDragItem"), [getDropGroup]("GI.Gtk.Objects.ToolPalette#g:method:getDropGroup"), [getDropItem]("GI.Gtk.Objects.ToolPalette#g:method:getDropItem"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getExclusive]("GI.Gtk.Objects.ToolPalette#g:method:getExclusive"), [getExpand]("GI.Gtk.Objects.ToolPalette#g:method:getExpand"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getGroupPosition]("GI.Gtk.Objects.ToolPalette#g:method:getGroupPosition"), [getHadjustment]("GI.Gtk.Objects.ToolPalette#g:method:getHadjustment"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getHscrollPolicy]("GI.Gtk.Interfaces.Scrollable#g:method:getHscrollPolicy"), [getIconSize]("GI.Gtk.Objects.ToolPalette#g:method:getIconSize"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getOrientation]("GI.Gtk.Interfaces.Orientable#g:method:getOrientation"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.ToolPalette#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getVadjustment]("GI.Gtk.Objects.ToolPalette#g:method:getVadjustment"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getVscrollPolicy]("GI.Gtk.Interfaces.Scrollable#g:method:getVscrollPolicy"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setDragSource]("GI.Gtk.Objects.ToolPalette#g:method:setDragSource"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setExclusive]("GI.Gtk.Objects.ToolPalette#g:method:setExclusive"), [setExpand]("GI.Gtk.Objects.ToolPalette#g:method:setExpand"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setGroupPosition]("GI.Gtk.Objects.ToolPalette#g:method:setGroupPosition"), [setHadjustment]("GI.Gtk.Interfaces.Scrollable#g:method:setHadjustment"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setHscrollPolicy]("GI.Gtk.Interfaces.Scrollable#g:method:setHscrollPolicy"), [setIconSize]("GI.Gtk.Objects.ToolPalette#g:method:setIconSize"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setOrientation]("GI.Gtk.Interfaces.Orientable#g:method:setOrientation"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.ToolPalette#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setVadjustment]("GI.Gtk.Interfaces.Scrollable#g:method:setVadjustment"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setVscrollPolicy]("GI.Gtk.Interfaces.Scrollable#g:method:setVscrollPolicy"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveToolPaletteMethod                ,
#endif

-- ** addDragDest #method:addDragDest#

#if defined(ENABLE_OVERLOADING)
    ToolPaletteAddDragDestMethodInfo        ,
#endif
    toolPaletteAddDragDest                  ,


-- ** getDragItem #method:getDragItem#

#if defined(ENABLE_OVERLOADING)
    ToolPaletteGetDragItemMethodInfo        ,
#endif
    toolPaletteGetDragItem                  ,


-- ** getDragTargetGroup #method:getDragTargetGroup#

    toolPaletteGetDragTargetGroup           ,


-- ** getDragTargetItem #method:getDragTargetItem#

    toolPaletteGetDragTargetItem            ,


-- ** getDropGroup #method:getDropGroup#

#if defined(ENABLE_OVERLOADING)
    ToolPaletteGetDropGroupMethodInfo       ,
#endif
    toolPaletteGetDropGroup                 ,


-- ** getDropItem #method:getDropItem#

#if defined(ENABLE_OVERLOADING)
    ToolPaletteGetDropItemMethodInfo        ,
#endif
    toolPaletteGetDropItem                  ,


-- ** getExclusive #method:getExclusive#

#if defined(ENABLE_OVERLOADING)
    ToolPaletteGetExclusiveMethodInfo       ,
#endif
    toolPaletteGetExclusive                 ,


-- ** getExpand #method:getExpand#

#if defined(ENABLE_OVERLOADING)
    ToolPaletteGetExpandMethodInfo          ,
#endif
    toolPaletteGetExpand                    ,


-- ** getGroupPosition #method:getGroupPosition#

#if defined(ENABLE_OVERLOADING)
    ToolPaletteGetGroupPositionMethodInfo   ,
#endif
    toolPaletteGetGroupPosition             ,


-- ** getHadjustment #method:getHadjustment#

#if defined(ENABLE_OVERLOADING)
    ToolPaletteGetHadjustmentMethodInfo     ,
#endif
    toolPaletteGetHadjustment               ,


-- ** getIconSize #method:getIconSize#

#if defined(ENABLE_OVERLOADING)
    ToolPaletteGetIconSizeMethodInfo        ,
#endif
    toolPaletteGetIconSize                  ,


-- ** getStyle #method:getStyle#

#if defined(ENABLE_OVERLOADING)
    ToolPaletteGetStyleMethodInfo           ,
#endif
    toolPaletteGetStyle                     ,


-- ** getVadjustment #method:getVadjustment#

#if defined(ENABLE_OVERLOADING)
    ToolPaletteGetVadjustmentMethodInfo     ,
#endif
    toolPaletteGetVadjustment               ,


-- ** new #method:new#

    toolPaletteNew                          ,


-- ** setDragSource #method:setDragSource#

#if defined(ENABLE_OVERLOADING)
    ToolPaletteSetDragSourceMethodInfo      ,
#endif
    toolPaletteSetDragSource                ,


-- ** setExclusive #method:setExclusive#

#if defined(ENABLE_OVERLOADING)
    ToolPaletteSetExclusiveMethodInfo       ,
#endif
    toolPaletteSetExclusive                 ,


-- ** setExpand #method:setExpand#

#if defined(ENABLE_OVERLOADING)
    ToolPaletteSetExpandMethodInfo          ,
#endif
    toolPaletteSetExpand                    ,


-- ** setGroupPosition #method:setGroupPosition#

#if defined(ENABLE_OVERLOADING)
    ToolPaletteSetGroupPositionMethodInfo   ,
#endif
    toolPaletteSetGroupPosition             ,


-- ** setIconSize #method:setIconSize#

#if defined(ENABLE_OVERLOADING)
    ToolPaletteSetIconSizeMethodInfo        ,
#endif
    toolPaletteSetIconSize                  ,


-- ** setStyle #method:setStyle#

#if defined(ENABLE_OVERLOADING)
    ToolPaletteSetStyleMethodInfo           ,
#endif
    toolPaletteSetStyle                     ,


-- ** unsetIconSize #method:unsetIconSize#

#if defined(ENABLE_OVERLOADING)
    ToolPaletteUnsetIconSizeMethodInfo      ,
#endif
    toolPaletteUnsetIconSize                ,


-- ** unsetStyle #method:unsetStyle#

#if defined(ENABLE_OVERLOADING)
    ToolPaletteUnsetStyleMethodInfo         ,
#endif
    toolPaletteUnsetStyle                   ,




 -- * Properties


-- ** iconSize #attr:iconSize#
-- | The size of the icons in a tool palette. When this property is set,
-- it overrides the default setting.
-- 
-- This should only be used for special-purpose tool palettes, normal
-- application tool palettes should respect the user preferences for the
-- size of icons.
-- 
-- /Since: 2.20/

#if defined(ENABLE_OVERLOADING)
    ToolPaletteIconSizePropertyInfo         ,
#endif
    constructToolPaletteIconSize            ,
    getToolPaletteIconSize                  ,
    setToolPaletteIconSize                  ,
#if defined(ENABLE_OVERLOADING)
    toolPaletteIconSize                     ,
#endif


-- ** iconSizeSet #attr:iconSizeSet#
-- | Is 'P.True' if the [ToolPalette:iconSize]("GI.Gtk.Objects.ToolPalette#g:attr:iconSize") property has been set.
-- 
-- /Since: 2.20/

#if defined(ENABLE_OVERLOADING)
    ToolPaletteIconSizeSetPropertyInfo      ,
#endif
    constructToolPaletteIconSizeSet         ,
    getToolPaletteIconSizeSet               ,
    setToolPaletteIconSizeSet               ,
#if defined(ENABLE_OVERLOADING)
    toolPaletteIconSizeSet                  ,
#endif


-- ** toolbarStyle #attr:toolbarStyle#
-- | The style of items in the tool palette.
-- 
-- /Since: 2.20/

#if defined(ENABLE_OVERLOADING)
    ToolPaletteToolbarStylePropertyInfo     ,
#endif
    constructToolPaletteToolbarStyle        ,
    getToolPaletteToolbarStyle              ,
    setToolPaletteToolbarStyle              ,
#if defined(ENABLE_OVERLOADING)
    toolPaletteToolbarStyle                 ,
#endif




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
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Flags as Gtk.Flags
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Orientable as Gtk.Orientable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Scrollable as Gtk.Scrollable
import {-# SOURCE #-} qualified GI.Gtk.Objects.Adjustment as Gtk.Adjustment
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.ToolItem as Gtk.ToolItem
import {-# SOURCE #-} qualified GI.Gtk.Objects.ToolItemGroup as Gtk.ToolItemGroup
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget
import {-# SOURCE #-} qualified GI.Gtk.Structs.SelectionData as Gtk.SelectionData
import {-# SOURCE #-} qualified GI.Gtk.Structs.TargetEntry as Gtk.TargetEntry

-- | Memory-managed wrapper type.
newtype ToolPalette = ToolPalette (SP.ManagedPtr ToolPalette)
    deriving (Eq)

instance SP.ManagedPtrNewtype ToolPalette where
    toManagedPtr (ToolPalette p) = p

foreign import ccall "gtk_tool_palette_get_type"
    c_gtk_tool_palette_get_type :: IO B.Types.GType

instance B.Types.TypedObject ToolPalette where
    glibType = c_gtk_tool_palette_get_type

instance B.Types.GObject ToolPalette

-- | Type class for types which can be safely cast to `ToolPalette`, for instance with `toToolPalette`.
class (SP.GObject o, O.IsDescendantOf ToolPalette o) => IsToolPalette o
instance (SP.GObject o, O.IsDescendantOf ToolPalette o) => IsToolPalette o

instance O.HasParentTypes ToolPalette
type instance O.ParentTypes ToolPalette = '[Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable, Gtk.Orientable.Orientable, Gtk.Scrollable.Scrollable]

-- | Cast to `ToolPalette`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toToolPalette :: (MIO.MonadIO m, IsToolPalette o) => o -> m ToolPalette
toToolPalette = MIO.liftIO . B.ManagedPtr.unsafeCastTo ToolPalette

-- | Convert 'ToolPalette' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe ToolPalette) where
    gvalueGType_ = c_gtk_tool_palette_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr ToolPalette)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr ToolPalette)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject ToolPalette ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveToolPaletteMethod (t :: Symbol) (o :: *) :: * where
    ResolveToolPaletteMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveToolPaletteMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveToolPaletteMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveToolPaletteMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveToolPaletteMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveToolPaletteMethod "addDragDest" o = ToolPaletteAddDragDestMethodInfo
    ResolveToolPaletteMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveToolPaletteMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveToolPaletteMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveToolPaletteMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveToolPaletteMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveToolPaletteMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveToolPaletteMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveToolPaletteMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveToolPaletteMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveToolPaletteMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveToolPaletteMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveToolPaletteMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveToolPaletteMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveToolPaletteMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveToolPaletteMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveToolPaletteMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveToolPaletteMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveToolPaletteMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveToolPaletteMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveToolPaletteMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveToolPaletteMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveToolPaletteMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveToolPaletteMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveToolPaletteMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveToolPaletteMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveToolPaletteMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveToolPaletteMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveToolPaletteMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveToolPaletteMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveToolPaletteMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveToolPaletteMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveToolPaletteMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveToolPaletteMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveToolPaletteMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveToolPaletteMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveToolPaletteMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveToolPaletteMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveToolPaletteMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveToolPaletteMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveToolPaletteMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveToolPaletteMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveToolPaletteMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveToolPaletteMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveToolPaletteMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveToolPaletteMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveToolPaletteMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveToolPaletteMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveToolPaletteMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveToolPaletteMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveToolPaletteMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveToolPaletteMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveToolPaletteMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveToolPaletteMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveToolPaletteMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveToolPaletteMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveToolPaletteMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveToolPaletteMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveToolPaletteMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveToolPaletteMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveToolPaletteMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveToolPaletteMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveToolPaletteMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveToolPaletteMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveToolPaletteMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveToolPaletteMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveToolPaletteMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveToolPaletteMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveToolPaletteMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveToolPaletteMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveToolPaletteMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveToolPaletteMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveToolPaletteMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveToolPaletteMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveToolPaletteMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveToolPaletteMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveToolPaletteMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveToolPaletteMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveToolPaletteMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveToolPaletteMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveToolPaletteMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveToolPaletteMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveToolPaletteMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveToolPaletteMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveToolPaletteMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveToolPaletteMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveToolPaletteMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveToolPaletteMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveToolPaletteMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveToolPaletteMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveToolPaletteMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveToolPaletteMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveToolPaletteMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveToolPaletteMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveToolPaletteMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveToolPaletteMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveToolPaletteMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveToolPaletteMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveToolPaletteMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveToolPaletteMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveToolPaletteMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveToolPaletteMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveToolPaletteMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveToolPaletteMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveToolPaletteMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveToolPaletteMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveToolPaletteMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveToolPaletteMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveToolPaletteMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveToolPaletteMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveToolPaletteMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveToolPaletteMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveToolPaletteMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveToolPaletteMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveToolPaletteMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveToolPaletteMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveToolPaletteMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveToolPaletteMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveToolPaletteMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveToolPaletteMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveToolPaletteMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveToolPaletteMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveToolPaletteMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveToolPaletteMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolveToolPaletteMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveToolPaletteMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveToolPaletteMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveToolPaletteMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveToolPaletteMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveToolPaletteMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveToolPaletteMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveToolPaletteMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveToolPaletteMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveToolPaletteMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveToolPaletteMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveToolPaletteMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveToolPaletteMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveToolPaletteMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveToolPaletteMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveToolPaletteMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveToolPaletteMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveToolPaletteMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveToolPaletteMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveToolPaletteMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveToolPaletteMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveToolPaletteMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveToolPaletteMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveToolPaletteMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveToolPaletteMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveToolPaletteMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveToolPaletteMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveToolPaletteMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveToolPaletteMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveToolPaletteMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveToolPaletteMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveToolPaletteMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveToolPaletteMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveToolPaletteMethod "unsetIconSize" o = ToolPaletteUnsetIconSizeMethodInfo
    ResolveToolPaletteMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveToolPaletteMethod "unsetStyle" o = ToolPaletteUnsetStyleMethodInfo
    ResolveToolPaletteMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveToolPaletteMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveToolPaletteMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveToolPaletteMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveToolPaletteMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveToolPaletteMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveToolPaletteMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveToolPaletteMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveToolPaletteMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveToolPaletteMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveToolPaletteMethod "getBorder" o = Gtk.Scrollable.ScrollableGetBorderMethodInfo
    ResolveToolPaletteMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveToolPaletteMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveToolPaletteMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveToolPaletteMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveToolPaletteMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveToolPaletteMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveToolPaletteMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveToolPaletteMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveToolPaletteMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveToolPaletteMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveToolPaletteMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveToolPaletteMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveToolPaletteMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveToolPaletteMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveToolPaletteMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveToolPaletteMethod "getDragItem" o = ToolPaletteGetDragItemMethodInfo
    ResolveToolPaletteMethod "getDropGroup" o = ToolPaletteGetDropGroupMethodInfo
    ResolveToolPaletteMethod "getDropItem" o = ToolPaletteGetDropItemMethodInfo
    ResolveToolPaletteMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveToolPaletteMethod "getExclusive" o = ToolPaletteGetExclusiveMethodInfo
    ResolveToolPaletteMethod "getExpand" o = ToolPaletteGetExpandMethodInfo
    ResolveToolPaletteMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveToolPaletteMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveToolPaletteMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveToolPaletteMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveToolPaletteMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveToolPaletteMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveToolPaletteMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveToolPaletteMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveToolPaletteMethod "getGroupPosition" o = ToolPaletteGetGroupPositionMethodInfo
    ResolveToolPaletteMethod "getHadjustment" o = ToolPaletteGetHadjustmentMethodInfo
    ResolveToolPaletteMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveToolPaletteMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveToolPaletteMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveToolPaletteMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveToolPaletteMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveToolPaletteMethod "getHscrollPolicy" o = Gtk.Scrollable.ScrollableGetHscrollPolicyMethodInfo
    ResolveToolPaletteMethod "getIconSize" o = ToolPaletteGetIconSizeMethodInfo
    ResolveToolPaletteMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveToolPaletteMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveToolPaletteMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveToolPaletteMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveToolPaletteMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveToolPaletteMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveToolPaletteMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveToolPaletteMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveToolPaletteMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveToolPaletteMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveToolPaletteMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveToolPaletteMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveToolPaletteMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveToolPaletteMethod "getOrientation" o = Gtk.Orientable.OrientableGetOrientationMethodInfo
    ResolveToolPaletteMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveToolPaletteMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveToolPaletteMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveToolPaletteMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveToolPaletteMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveToolPaletteMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveToolPaletteMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveToolPaletteMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveToolPaletteMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveToolPaletteMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveToolPaletteMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveToolPaletteMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveToolPaletteMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveToolPaletteMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveToolPaletteMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveToolPaletteMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveToolPaletteMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveToolPaletteMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveToolPaletteMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveToolPaletteMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveToolPaletteMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveToolPaletteMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveToolPaletteMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveToolPaletteMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveToolPaletteMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveToolPaletteMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveToolPaletteMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveToolPaletteMethod "getStyle" o = ToolPaletteGetStyleMethodInfo
    ResolveToolPaletteMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveToolPaletteMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveToolPaletteMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveToolPaletteMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveToolPaletteMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveToolPaletteMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveToolPaletteMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveToolPaletteMethod "getVadjustment" o = ToolPaletteGetVadjustmentMethodInfo
    ResolveToolPaletteMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveToolPaletteMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveToolPaletteMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveToolPaletteMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveToolPaletteMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveToolPaletteMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveToolPaletteMethod "getVscrollPolicy" o = Gtk.Scrollable.ScrollableGetVscrollPolicyMethodInfo
    ResolveToolPaletteMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveToolPaletteMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveToolPaletteMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveToolPaletteMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveToolPaletteMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveToolPaletteMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveToolPaletteMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveToolPaletteMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveToolPaletteMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveToolPaletteMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveToolPaletteMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveToolPaletteMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveToolPaletteMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveToolPaletteMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveToolPaletteMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveToolPaletteMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveToolPaletteMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveToolPaletteMethod "setDragSource" o = ToolPaletteSetDragSourceMethodInfo
    ResolveToolPaletteMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveToolPaletteMethod "setExclusive" o = ToolPaletteSetExclusiveMethodInfo
    ResolveToolPaletteMethod "setExpand" o = ToolPaletteSetExpandMethodInfo
    ResolveToolPaletteMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveToolPaletteMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveToolPaletteMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveToolPaletteMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveToolPaletteMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveToolPaletteMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveToolPaletteMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveToolPaletteMethod "setGroupPosition" o = ToolPaletteSetGroupPositionMethodInfo
    ResolveToolPaletteMethod "setHadjustment" o = Gtk.Scrollable.ScrollableSetHadjustmentMethodInfo
    ResolveToolPaletteMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveToolPaletteMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveToolPaletteMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveToolPaletteMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveToolPaletteMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveToolPaletteMethod "setHscrollPolicy" o = Gtk.Scrollable.ScrollableSetHscrollPolicyMethodInfo
    ResolveToolPaletteMethod "setIconSize" o = ToolPaletteSetIconSizeMethodInfo
    ResolveToolPaletteMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveToolPaletteMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveToolPaletteMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveToolPaletteMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveToolPaletteMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveToolPaletteMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveToolPaletteMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveToolPaletteMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveToolPaletteMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveToolPaletteMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveToolPaletteMethod "setOrientation" o = Gtk.Orientable.OrientableSetOrientationMethodInfo
    ResolveToolPaletteMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveToolPaletteMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveToolPaletteMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveToolPaletteMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveToolPaletteMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveToolPaletteMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveToolPaletteMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveToolPaletteMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveToolPaletteMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveToolPaletteMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveToolPaletteMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveToolPaletteMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveToolPaletteMethod "setStyle" o = ToolPaletteSetStyleMethodInfo
    ResolveToolPaletteMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveToolPaletteMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveToolPaletteMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveToolPaletteMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveToolPaletteMethod "setVadjustment" o = Gtk.Scrollable.ScrollableSetVadjustmentMethodInfo
    ResolveToolPaletteMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveToolPaletteMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveToolPaletteMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveToolPaletteMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveToolPaletteMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveToolPaletteMethod "setVscrollPolicy" o = Gtk.Scrollable.ScrollableSetVscrollPolicyMethodInfo
    ResolveToolPaletteMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveToolPaletteMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveToolPaletteMethod t ToolPalette, O.OverloadedMethod info ToolPalette p) => OL.IsLabel t (ToolPalette -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveToolPaletteMethod t ToolPalette, O.OverloadedMethod info ToolPalette p, R.HasField t ToolPalette p) => R.HasField t ToolPalette p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveToolPaletteMethod t ToolPalette, O.OverloadedMethodInfo info ToolPalette) => OL.IsLabel t (O.MethodProxy info ToolPalette) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- VVV Prop "icon-size"
   -- Type: TInterface (Name {namespace = "Gtk", name = "IconSize"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@icon-size@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' toolPalette #iconSize
-- @
getToolPaletteIconSize :: (MonadIO m, IsToolPalette o) => o -> m Gtk.Enums.IconSize
getToolPaletteIconSize obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "icon-size"

-- | Set the value of the “@icon-size@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' toolPalette [ #iconSize 'Data.GI.Base.Attributes.:=' value ]
-- @
setToolPaletteIconSize :: (MonadIO m, IsToolPalette o) => o -> Gtk.Enums.IconSize -> m ()
setToolPaletteIconSize obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "icon-size" val

-- | Construct a `GValueConstruct` with valid value for the “@icon-size@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructToolPaletteIconSize :: (IsToolPalette o, MIO.MonadIO m) => Gtk.Enums.IconSize -> m (GValueConstruct o)
constructToolPaletteIconSize val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "icon-size" val

#if defined(ENABLE_OVERLOADING)
data ToolPaletteIconSizePropertyInfo
instance AttrInfo ToolPaletteIconSizePropertyInfo where
    type AttrAllowedOps ToolPaletteIconSizePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ToolPaletteIconSizePropertyInfo = IsToolPalette
    type AttrSetTypeConstraint ToolPaletteIconSizePropertyInfo = (~) Gtk.Enums.IconSize
    type AttrTransferTypeConstraint ToolPaletteIconSizePropertyInfo = (~) Gtk.Enums.IconSize
    type AttrTransferType ToolPaletteIconSizePropertyInfo = Gtk.Enums.IconSize
    type AttrGetType ToolPaletteIconSizePropertyInfo = Gtk.Enums.IconSize
    type AttrLabel ToolPaletteIconSizePropertyInfo = "icon-size"
    type AttrOrigin ToolPaletteIconSizePropertyInfo = ToolPalette
    attrGet = getToolPaletteIconSize
    attrSet = setToolPaletteIconSize
    attrTransfer _ v = do
        return v
    attrConstruct = constructToolPaletteIconSize
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToolPalette.iconSize"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToolPalette.html#g:attr:iconSize"
        })
#endif

-- VVV Prop "icon-size-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@icon-size-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' toolPalette #iconSizeSet
-- @
getToolPaletteIconSizeSet :: (MonadIO m, IsToolPalette o) => o -> m Bool
getToolPaletteIconSizeSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "icon-size-set"

-- | Set the value of the “@icon-size-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' toolPalette [ #iconSizeSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setToolPaletteIconSizeSet :: (MonadIO m, IsToolPalette o) => o -> Bool -> m ()
setToolPaletteIconSizeSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "icon-size-set" val

-- | Construct a `GValueConstruct` with valid value for the “@icon-size-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructToolPaletteIconSizeSet :: (IsToolPalette o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructToolPaletteIconSizeSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "icon-size-set" val

#if defined(ENABLE_OVERLOADING)
data ToolPaletteIconSizeSetPropertyInfo
instance AttrInfo ToolPaletteIconSizeSetPropertyInfo where
    type AttrAllowedOps ToolPaletteIconSizeSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ToolPaletteIconSizeSetPropertyInfo = IsToolPalette
    type AttrSetTypeConstraint ToolPaletteIconSizeSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint ToolPaletteIconSizeSetPropertyInfo = (~) Bool
    type AttrTransferType ToolPaletteIconSizeSetPropertyInfo = Bool
    type AttrGetType ToolPaletteIconSizeSetPropertyInfo = Bool
    type AttrLabel ToolPaletteIconSizeSetPropertyInfo = "icon-size-set"
    type AttrOrigin ToolPaletteIconSizeSetPropertyInfo = ToolPalette
    attrGet = getToolPaletteIconSizeSet
    attrSet = setToolPaletteIconSizeSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructToolPaletteIconSizeSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToolPalette.iconSizeSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToolPalette.html#g:attr:iconSizeSet"
        })
#endif

-- VVV Prop "toolbar-style"
   -- Type: TInterface (Name {namespace = "Gtk", name = "ToolbarStyle"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@toolbar-style@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' toolPalette #toolbarStyle
-- @
getToolPaletteToolbarStyle :: (MonadIO m, IsToolPalette o) => o -> m Gtk.Enums.ToolbarStyle
getToolPaletteToolbarStyle obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "toolbar-style"

-- | Set the value of the “@toolbar-style@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' toolPalette [ #toolbarStyle 'Data.GI.Base.Attributes.:=' value ]
-- @
setToolPaletteToolbarStyle :: (MonadIO m, IsToolPalette o) => o -> Gtk.Enums.ToolbarStyle -> m ()
setToolPaletteToolbarStyle obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "toolbar-style" val

-- | Construct a `GValueConstruct` with valid value for the “@toolbar-style@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructToolPaletteToolbarStyle :: (IsToolPalette o, MIO.MonadIO m) => Gtk.Enums.ToolbarStyle -> m (GValueConstruct o)
constructToolPaletteToolbarStyle val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "toolbar-style" val

#if defined(ENABLE_OVERLOADING)
data ToolPaletteToolbarStylePropertyInfo
instance AttrInfo ToolPaletteToolbarStylePropertyInfo where
    type AttrAllowedOps ToolPaletteToolbarStylePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ToolPaletteToolbarStylePropertyInfo = IsToolPalette
    type AttrSetTypeConstraint ToolPaletteToolbarStylePropertyInfo = (~) Gtk.Enums.ToolbarStyle
    type AttrTransferTypeConstraint ToolPaletteToolbarStylePropertyInfo = (~) Gtk.Enums.ToolbarStyle
    type AttrTransferType ToolPaletteToolbarStylePropertyInfo = Gtk.Enums.ToolbarStyle
    type AttrGetType ToolPaletteToolbarStylePropertyInfo = Gtk.Enums.ToolbarStyle
    type AttrLabel ToolPaletteToolbarStylePropertyInfo = "toolbar-style"
    type AttrOrigin ToolPaletteToolbarStylePropertyInfo = ToolPalette
    attrGet = getToolPaletteToolbarStyle
    attrSet = setToolPaletteToolbarStyle
    attrTransfer _ v = do
        return v
    attrConstruct = constructToolPaletteToolbarStyle
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToolPalette.toolbarStyle"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToolPalette.html#g:attr:toolbarStyle"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList ToolPalette
type instance O.AttributeList ToolPalette = ToolPaletteAttributeList
type ToolPaletteAttributeList = ('[ '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("hadjustment", Gtk.Scrollable.ScrollableHadjustmentPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("hscrollPolicy", Gtk.Scrollable.ScrollableHscrollPolicyPropertyInfo), '("iconSize", ToolPaletteIconSizePropertyInfo), '("iconSizeSet", ToolPaletteIconSizeSetPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("orientation", Gtk.Orientable.OrientableOrientationPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("toolbarStyle", ToolPaletteToolbarStylePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("vadjustment", Gtk.Scrollable.ScrollableVadjustmentPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("vscrollPolicy", Gtk.Scrollable.ScrollableVscrollPolicyPropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
toolPaletteIconSize :: AttrLabelProxy "iconSize"
toolPaletteIconSize = AttrLabelProxy

toolPaletteIconSizeSet :: AttrLabelProxy "iconSizeSet"
toolPaletteIconSizeSet = AttrLabelProxy

toolPaletteToolbarStyle :: AttrLabelProxy "toolbarStyle"
toolPaletteToolbarStyle = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList ToolPalette = ToolPaletteSignalList
type ToolPaletteSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method ToolPalette::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "ToolPalette" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tool_palette_new" gtk_tool_palette_new :: 
    IO (Ptr ToolPalette)

-- | Creates a new tool palette.
-- 
-- /Since: 2.20/
toolPaletteNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m ToolPalette
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.ToolPalette.ToolPalette'
toolPaletteNew  = liftIO $ do
    result <- gtk_tool_palette_new
    checkUnexpectedReturnNULL "toolPaletteNew" result
    result' <- (newObject ToolPalette) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method ToolPalette::add_drag_dest
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "palette"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolPalette" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToolPalette" , sinceVersion = Nothing }
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
--                 { rawDocText =
--                     Just "a #GtkWidget which should be a drag destination for @palette"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "flags"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "DestDefaults" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the flags that specify what actions GTK+ should take for drops\n    on that widget"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "targets"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "ToolPaletteDragTargets" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the #GtkToolPaletteDragTargets which the widget\n    should support"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "actions"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "DragAction" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the #GdkDragActions which the widget should suppport"
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

foreign import ccall "gtk_tool_palette_add_drag_dest" gtk_tool_palette_add_drag_dest :: 
    Ptr ToolPalette ->                      -- palette : TInterface (Name {namespace = "Gtk", name = "ToolPalette"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CUInt ->                                -- flags : TInterface (Name {namespace = "Gtk", name = "DestDefaults"})
    CUInt ->                                -- targets : TInterface (Name {namespace = "Gtk", name = "ToolPaletteDragTargets"})
    CUInt ->                                -- actions : TInterface (Name {namespace = "Gdk", name = "DragAction"})
    IO ()

-- | Sets /@palette@/ as drag source (see 'GI.Gtk.Objects.ToolPalette.toolPaletteSetDragSource')
-- and sets /@widget@/ as a drag destination for drags from /@palette@/.
-- See 'GI.Gtk.Objects.Widget.widgetDragDestSet'.
-- 
-- /Since: 2.20/
toolPaletteAddDragDest ::
    (B.CallStack.HasCallStack, MonadIO m, IsToolPalette a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@palette@/: a t'GI.Gtk.Objects.ToolPalette.ToolPalette'
    -> b
    -- ^ /@widget@/: a t'GI.Gtk.Objects.Widget.Widget' which should be a drag destination for /@palette@/
    -> [Gtk.Flags.DestDefaults]
    -- ^ /@flags@/: the flags that specify what actions GTK+ should take for drops
    --     on that widget
    -> [Gtk.Flags.ToolPaletteDragTargets]
    -- ^ /@targets@/: the t'GI.Gtk.Flags.ToolPaletteDragTargets' which the widget
    --     should support
    -> [Gdk.Flags.DragAction]
    -- ^ /@actions@/: the @/GdkDragActions/@ which the widget should suppport
    -> m ()
toolPaletteAddDragDest palette widget flags targets actions = liftIO $ do
    palette' <- unsafeManagedPtrCastPtr palette
    widget' <- unsafeManagedPtrCastPtr widget
    let flags' = gflagsToWord flags
    let targets' = gflagsToWord targets
    let actions' = gflagsToWord actions
    gtk_tool_palette_add_drag_dest palette' widget' flags' targets' actions'
    touchManagedPtr palette
    touchManagedPtr widget
    return ()

#if defined(ENABLE_OVERLOADING)
data ToolPaletteAddDragDestMethodInfo
instance (signature ~ (b -> [Gtk.Flags.DestDefaults] -> [Gtk.Flags.ToolPaletteDragTargets] -> [Gdk.Flags.DragAction] -> m ()), MonadIO m, IsToolPalette a, Gtk.Widget.IsWidget b) => O.OverloadedMethod ToolPaletteAddDragDestMethodInfo a signature where
    overloadedMethod = toolPaletteAddDragDest

instance O.OverloadedMethodInfo ToolPaletteAddDragDestMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToolPalette.toolPaletteAddDragDest",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToolPalette.html#v:toolPaletteAddDragDest"
        })


#endif

-- method ToolPalette::get_drag_item
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "palette"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolPalette" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToolPalette" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "selection"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SelectionData" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSelectionData"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Widget" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tool_palette_get_drag_item" gtk_tool_palette_get_drag_item :: 
    Ptr ToolPalette ->                      -- palette : TInterface (Name {namespace = "Gtk", name = "ToolPalette"})
    Ptr Gtk.SelectionData.SelectionData ->  -- selection : TInterface (Name {namespace = "Gtk", name = "SelectionData"})
    IO (Ptr Gtk.Widget.Widget)

-- | Get the dragged item from the selection.
-- This could be a t'GI.Gtk.Objects.ToolItem.ToolItem' or a t'GI.Gtk.Objects.ToolItemGroup.ToolItemGroup'.
-- 
-- /Since: 2.20/
toolPaletteGetDragItem ::
    (B.CallStack.HasCallStack, MonadIO m, IsToolPalette a) =>
    a
    -- ^ /@palette@/: a t'GI.Gtk.Objects.ToolPalette.ToolPalette'
    -> Gtk.SelectionData.SelectionData
    -- ^ /@selection@/: a t'GI.Gtk.Structs.SelectionData.SelectionData'
    -> m Gtk.Widget.Widget
    -- ^ __Returns:__ the dragged item in selection
toolPaletteGetDragItem palette selection = liftIO $ do
    palette' <- unsafeManagedPtrCastPtr palette
    selection' <- unsafeManagedPtrGetPtr selection
    result <- gtk_tool_palette_get_drag_item palette' selection'
    checkUnexpectedReturnNULL "toolPaletteGetDragItem" result
    result' <- (newObject Gtk.Widget.Widget) result
    touchManagedPtr palette
    touchManagedPtr selection
    return result'

#if defined(ENABLE_OVERLOADING)
data ToolPaletteGetDragItemMethodInfo
instance (signature ~ (Gtk.SelectionData.SelectionData -> m Gtk.Widget.Widget), MonadIO m, IsToolPalette a) => O.OverloadedMethod ToolPaletteGetDragItemMethodInfo a signature where
    overloadedMethod = toolPaletteGetDragItem

instance O.OverloadedMethodInfo ToolPaletteGetDragItemMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToolPalette.toolPaletteGetDragItem",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToolPalette.html#v:toolPaletteGetDragItem"
        })


#endif

-- method ToolPalette::get_drop_group
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "palette"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolPalette" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToolPalette" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the x position" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the y position" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "ToolItemGroup" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tool_palette_get_drop_group" gtk_tool_palette_get_drop_group :: 
    Ptr ToolPalette ->                      -- palette : TInterface (Name {namespace = "Gtk", name = "ToolPalette"})
    Int32 ->                                -- x : TBasicType TInt
    Int32 ->                                -- y : TBasicType TInt
    IO (Ptr Gtk.ToolItemGroup.ToolItemGroup)

-- | Gets the group at position (x, y).
-- 
-- /Since: 2.20/
toolPaletteGetDropGroup ::
    (B.CallStack.HasCallStack, MonadIO m, IsToolPalette a) =>
    a
    -- ^ /@palette@/: a t'GI.Gtk.Objects.ToolPalette.ToolPalette'
    -> Int32
    -- ^ /@x@/: the x position
    -> Int32
    -- ^ /@y@/: the y position
    -> m (Maybe Gtk.ToolItemGroup.ToolItemGroup)
    -- ^ __Returns:__ the t'GI.Gtk.Objects.ToolItemGroup.ToolItemGroup' at position
    -- or 'P.Nothing' if there is no such group
toolPaletteGetDropGroup palette x y = liftIO $ do
    palette' <- unsafeManagedPtrCastPtr palette
    result <- gtk_tool_palette_get_drop_group palette' x y
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.ToolItemGroup.ToolItemGroup) result'
        return result''
    touchManagedPtr palette
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data ToolPaletteGetDropGroupMethodInfo
instance (signature ~ (Int32 -> Int32 -> m (Maybe Gtk.ToolItemGroup.ToolItemGroup)), MonadIO m, IsToolPalette a) => O.OverloadedMethod ToolPaletteGetDropGroupMethodInfo a signature where
    overloadedMethod = toolPaletteGetDropGroup

instance O.OverloadedMethodInfo ToolPaletteGetDropGroupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToolPalette.toolPaletteGetDropGroup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToolPalette.html#v:toolPaletteGetDropGroup"
        })


#endif

-- method ToolPalette::get_drop_item
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "palette"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolPalette" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToolPalette" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the x position" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the y position" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "ToolItem" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tool_palette_get_drop_item" gtk_tool_palette_get_drop_item :: 
    Ptr ToolPalette ->                      -- palette : TInterface (Name {namespace = "Gtk", name = "ToolPalette"})
    Int32 ->                                -- x : TBasicType TInt
    Int32 ->                                -- y : TBasicType TInt
    IO (Ptr Gtk.ToolItem.ToolItem)

-- | Gets the item at position (x, y).
-- See 'GI.Gtk.Objects.ToolPalette.toolPaletteGetDropGroup'.
-- 
-- /Since: 2.20/
toolPaletteGetDropItem ::
    (B.CallStack.HasCallStack, MonadIO m, IsToolPalette a) =>
    a
    -- ^ /@palette@/: a t'GI.Gtk.Objects.ToolPalette.ToolPalette'
    -> Int32
    -- ^ /@x@/: the x position
    -> Int32
    -- ^ /@y@/: the y position
    -> m (Maybe Gtk.ToolItem.ToolItem)
    -- ^ __Returns:__ the t'GI.Gtk.Objects.ToolItem.ToolItem' at position or 'P.Nothing' if there is no such item
toolPaletteGetDropItem palette x y = liftIO $ do
    palette' <- unsafeManagedPtrCastPtr palette
    result <- gtk_tool_palette_get_drop_item palette' x y
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.ToolItem.ToolItem) result'
        return result''
    touchManagedPtr palette
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data ToolPaletteGetDropItemMethodInfo
instance (signature ~ (Int32 -> Int32 -> m (Maybe Gtk.ToolItem.ToolItem)), MonadIO m, IsToolPalette a) => O.OverloadedMethod ToolPaletteGetDropItemMethodInfo a signature where
    overloadedMethod = toolPaletteGetDropItem

instance O.OverloadedMethodInfo ToolPaletteGetDropItemMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToolPalette.toolPaletteGetDropItem",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToolPalette.html#v:toolPaletteGetDropItem"
        })


#endif

-- method ToolPalette::get_exclusive
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "palette"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolPalette" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToolPalette" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolItemGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GtkToolItemGroup which is a child of palette"
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

foreign import ccall "gtk_tool_palette_get_exclusive" gtk_tool_palette_get_exclusive :: 
    Ptr ToolPalette ->                      -- palette : TInterface (Name {namespace = "Gtk", name = "ToolPalette"})
    Ptr Gtk.ToolItemGroup.ToolItemGroup ->  -- group : TInterface (Name {namespace = "Gtk", name = "ToolItemGroup"})
    IO CInt

-- | Gets whether /@group@/ is exclusive or not.
-- See 'GI.Gtk.Objects.ToolPalette.toolPaletteSetExclusive'.
-- 
-- /Since: 2.20/
toolPaletteGetExclusive ::
    (B.CallStack.HasCallStack, MonadIO m, IsToolPalette a, Gtk.ToolItemGroup.IsToolItemGroup b) =>
    a
    -- ^ /@palette@/: a t'GI.Gtk.Objects.ToolPalette.ToolPalette'
    -> b
    -- ^ /@group@/: a t'GI.Gtk.Objects.ToolItemGroup.ToolItemGroup' which is a child of palette
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@group@/ is exclusive
toolPaletteGetExclusive palette group = liftIO $ do
    palette' <- unsafeManagedPtrCastPtr palette
    group' <- unsafeManagedPtrCastPtr group
    result <- gtk_tool_palette_get_exclusive palette' group'
    let result' = (/= 0) result
    touchManagedPtr palette
    touchManagedPtr group
    return result'

#if defined(ENABLE_OVERLOADING)
data ToolPaletteGetExclusiveMethodInfo
instance (signature ~ (b -> m Bool), MonadIO m, IsToolPalette a, Gtk.ToolItemGroup.IsToolItemGroup b) => O.OverloadedMethod ToolPaletteGetExclusiveMethodInfo a signature where
    overloadedMethod = toolPaletteGetExclusive

instance O.OverloadedMethodInfo ToolPaletteGetExclusiveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToolPalette.toolPaletteGetExclusive",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToolPalette.html#v:toolPaletteGetExclusive"
        })


#endif

-- method ToolPalette::get_expand
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "palette"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolPalette" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToolPalette" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolItemGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GtkToolItemGroup which is a child of palette"
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

foreign import ccall "gtk_tool_palette_get_expand" gtk_tool_palette_get_expand :: 
    Ptr ToolPalette ->                      -- palette : TInterface (Name {namespace = "Gtk", name = "ToolPalette"})
    Ptr Gtk.ToolItemGroup.ToolItemGroup ->  -- group : TInterface (Name {namespace = "Gtk", name = "ToolItemGroup"})
    IO CInt

-- | Gets whether group should be given extra space.
-- See 'GI.Gtk.Objects.ToolPalette.toolPaletteSetExpand'.
-- 
-- /Since: 2.20/
toolPaletteGetExpand ::
    (B.CallStack.HasCallStack, MonadIO m, IsToolPalette a, Gtk.ToolItemGroup.IsToolItemGroup b) =>
    a
    -- ^ /@palette@/: a t'GI.Gtk.Objects.ToolPalette.ToolPalette'
    -> b
    -- ^ /@group@/: a t'GI.Gtk.Objects.ToolItemGroup.ToolItemGroup' which is a child of palette
    -> m Bool
    -- ^ __Returns:__ 'P.True' if group should be given extra space, 'P.False' otherwise
toolPaletteGetExpand palette group = liftIO $ do
    palette' <- unsafeManagedPtrCastPtr palette
    group' <- unsafeManagedPtrCastPtr group
    result <- gtk_tool_palette_get_expand palette' group'
    let result' = (/= 0) result
    touchManagedPtr palette
    touchManagedPtr group
    return result'

#if defined(ENABLE_OVERLOADING)
data ToolPaletteGetExpandMethodInfo
instance (signature ~ (b -> m Bool), MonadIO m, IsToolPalette a, Gtk.ToolItemGroup.IsToolItemGroup b) => O.OverloadedMethod ToolPaletteGetExpandMethodInfo a signature where
    overloadedMethod = toolPaletteGetExpand

instance O.OverloadedMethodInfo ToolPaletteGetExpandMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToolPalette.toolPaletteGetExpand",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToolPalette.html#v:toolPaletteGetExpand"
        })


#endif

-- method ToolPalette::get_group_position
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "palette"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolPalette" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToolPalette" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolItemGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToolItemGroup"
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
-- returnType: Just (TBasicType TInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tool_palette_get_group_position" gtk_tool_palette_get_group_position :: 
    Ptr ToolPalette ->                      -- palette : TInterface (Name {namespace = "Gtk", name = "ToolPalette"})
    Ptr Gtk.ToolItemGroup.ToolItemGroup ->  -- group : TInterface (Name {namespace = "Gtk", name = "ToolItemGroup"})
    IO Int32

-- | Gets the position of /@group@/ in /@palette@/ as index.
-- See 'GI.Gtk.Objects.ToolPalette.toolPaletteSetGroupPosition'.
-- 
-- /Since: 2.20/
toolPaletteGetGroupPosition ::
    (B.CallStack.HasCallStack, MonadIO m, IsToolPalette a, Gtk.ToolItemGroup.IsToolItemGroup b) =>
    a
    -- ^ /@palette@/: a t'GI.Gtk.Objects.ToolPalette.ToolPalette'
    -> b
    -- ^ /@group@/: a t'GI.Gtk.Objects.ToolItemGroup.ToolItemGroup'
    -> m Int32
    -- ^ __Returns:__ the index of group or -1 if /@group@/ is not a child of /@palette@/
toolPaletteGetGroupPosition palette group = liftIO $ do
    palette' <- unsafeManagedPtrCastPtr palette
    group' <- unsafeManagedPtrCastPtr group
    result <- gtk_tool_palette_get_group_position palette' group'
    touchManagedPtr palette
    touchManagedPtr group
    return result

#if defined(ENABLE_OVERLOADING)
data ToolPaletteGetGroupPositionMethodInfo
instance (signature ~ (b -> m Int32), MonadIO m, IsToolPalette a, Gtk.ToolItemGroup.IsToolItemGroup b) => O.OverloadedMethod ToolPaletteGetGroupPositionMethodInfo a signature where
    overloadedMethod = toolPaletteGetGroupPosition

instance O.OverloadedMethodInfo ToolPaletteGetGroupPositionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToolPalette.toolPaletteGetGroupPosition",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToolPalette.html#v:toolPaletteGetGroupPosition"
        })


#endif

-- method ToolPalette::get_hadjustment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "palette"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolPalette" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToolPalette" , sinceVersion = Nothing }
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

foreign import ccall "gtk_tool_palette_get_hadjustment" gtk_tool_palette_get_hadjustment :: 
    Ptr ToolPalette ->                      -- palette : TInterface (Name {namespace = "Gtk", name = "ToolPalette"})
    IO (Ptr Gtk.Adjustment.Adjustment)

{-# DEPRECATED toolPaletteGetHadjustment ["(Since version 3.0)","Use 'GI.Gtk.Interfaces.Scrollable.scrollableGetHadjustment'"] #-}
-- | Gets the horizontal adjustment of the tool palette.
-- 
-- /Since: 2.20/
toolPaletteGetHadjustment ::
    (B.CallStack.HasCallStack, MonadIO m, IsToolPalette a) =>
    a
    -- ^ /@palette@/: a t'GI.Gtk.Objects.ToolPalette.ToolPalette'
    -> m Gtk.Adjustment.Adjustment
    -- ^ __Returns:__ the horizontal adjustment of /@palette@/
toolPaletteGetHadjustment palette = liftIO $ do
    palette' <- unsafeManagedPtrCastPtr palette
    result <- gtk_tool_palette_get_hadjustment palette'
    checkUnexpectedReturnNULL "toolPaletteGetHadjustment" result
    result' <- (newObject Gtk.Adjustment.Adjustment) result
    touchManagedPtr palette
    return result'

#if defined(ENABLE_OVERLOADING)
data ToolPaletteGetHadjustmentMethodInfo
instance (signature ~ (m Gtk.Adjustment.Adjustment), MonadIO m, IsToolPalette a) => O.OverloadedMethod ToolPaletteGetHadjustmentMethodInfo a signature where
    overloadedMethod = toolPaletteGetHadjustment

instance O.OverloadedMethodInfo ToolPaletteGetHadjustmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToolPalette.toolPaletteGetHadjustment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToolPalette.html#v:toolPaletteGetHadjustment"
        })


#endif

-- method ToolPalette::get_icon_size
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "palette"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolPalette" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToolPalette" , sinceVersion = Nothing }
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

foreign import ccall "gtk_tool_palette_get_icon_size" gtk_tool_palette_get_icon_size :: 
    Ptr ToolPalette ->                      -- palette : TInterface (Name {namespace = "Gtk", name = "ToolPalette"})
    IO Int32

-- | Gets the size of icons in the tool palette.
-- See 'GI.Gtk.Objects.ToolPalette.toolPaletteSetIconSize'.
-- 
-- /Since: 2.20/
toolPaletteGetIconSize ::
    (B.CallStack.HasCallStack, MonadIO m, IsToolPalette a) =>
    a
    -- ^ /@palette@/: a t'GI.Gtk.Objects.ToolPalette.ToolPalette'
    -> m Int32
    -- ^ __Returns:__ the t'GI.Gtk.Enums.IconSize' of icons in the tool palette
toolPaletteGetIconSize palette = liftIO $ do
    palette' <- unsafeManagedPtrCastPtr palette
    result <- gtk_tool_palette_get_icon_size palette'
    touchManagedPtr palette
    return result

#if defined(ENABLE_OVERLOADING)
data ToolPaletteGetIconSizeMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsToolPalette a) => O.OverloadedMethod ToolPaletteGetIconSizeMethodInfo a signature where
    overloadedMethod = toolPaletteGetIconSize

instance O.OverloadedMethodInfo ToolPaletteGetIconSizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToolPalette.toolPaletteGetIconSize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToolPalette.html#v:toolPaletteGetIconSize"
        })


#endif

-- method ToolPalette::get_style
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "palette"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolPalette" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToolPalette" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "ToolbarStyle" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tool_palette_get_style" gtk_tool_palette_get_style :: 
    Ptr ToolPalette ->                      -- palette : TInterface (Name {namespace = "Gtk", name = "ToolPalette"})
    IO CUInt

-- | Gets the style (icons, text or both) of items in the tool palette.
-- 
-- /Since: 2.20/
toolPaletteGetStyle ::
    (B.CallStack.HasCallStack, MonadIO m, IsToolPalette a) =>
    a
    -- ^ /@palette@/: a t'GI.Gtk.Objects.ToolPalette.ToolPalette'
    -> m Gtk.Enums.ToolbarStyle
    -- ^ __Returns:__ the t'GI.Gtk.Enums.ToolbarStyle' of items in the tool palette.
toolPaletteGetStyle palette = liftIO $ do
    palette' <- unsafeManagedPtrCastPtr palette
    result <- gtk_tool_palette_get_style palette'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr palette
    return result'

#if defined(ENABLE_OVERLOADING)
data ToolPaletteGetStyleMethodInfo
instance (signature ~ (m Gtk.Enums.ToolbarStyle), MonadIO m, IsToolPalette a) => O.OverloadedMethod ToolPaletteGetStyleMethodInfo a signature where
    overloadedMethod = toolPaletteGetStyle

instance O.OverloadedMethodInfo ToolPaletteGetStyleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToolPalette.toolPaletteGetStyle",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToolPalette.html#v:toolPaletteGetStyle"
        })


#endif

-- method ToolPalette::get_vadjustment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "palette"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolPalette" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToolPalette" , sinceVersion = Nothing }
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

foreign import ccall "gtk_tool_palette_get_vadjustment" gtk_tool_palette_get_vadjustment :: 
    Ptr ToolPalette ->                      -- palette : TInterface (Name {namespace = "Gtk", name = "ToolPalette"})
    IO (Ptr Gtk.Adjustment.Adjustment)

{-# DEPRECATED toolPaletteGetVadjustment ["(Since version 3.0)","Use 'GI.Gtk.Interfaces.Scrollable.scrollableGetVadjustment'"] #-}
-- | Gets the vertical adjustment of the tool palette.
-- 
-- /Since: 2.20/
toolPaletteGetVadjustment ::
    (B.CallStack.HasCallStack, MonadIO m, IsToolPalette a) =>
    a
    -- ^ /@palette@/: a t'GI.Gtk.Objects.ToolPalette.ToolPalette'
    -> m Gtk.Adjustment.Adjustment
    -- ^ __Returns:__ the vertical adjustment of /@palette@/
toolPaletteGetVadjustment palette = liftIO $ do
    palette' <- unsafeManagedPtrCastPtr palette
    result <- gtk_tool_palette_get_vadjustment palette'
    checkUnexpectedReturnNULL "toolPaletteGetVadjustment" result
    result' <- (newObject Gtk.Adjustment.Adjustment) result
    touchManagedPtr palette
    return result'

#if defined(ENABLE_OVERLOADING)
data ToolPaletteGetVadjustmentMethodInfo
instance (signature ~ (m Gtk.Adjustment.Adjustment), MonadIO m, IsToolPalette a) => O.OverloadedMethod ToolPaletteGetVadjustmentMethodInfo a signature where
    overloadedMethod = toolPaletteGetVadjustment

instance O.OverloadedMethodInfo ToolPaletteGetVadjustmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToolPalette.toolPaletteGetVadjustment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToolPalette.html#v:toolPaletteGetVadjustment"
        })


#endif

-- method ToolPalette::set_drag_source
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "palette"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolPalette" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToolPalette" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "targets"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "ToolPaletteDragTargets" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the #GtkToolPaletteDragTargets\n    which the widget should support"
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

foreign import ccall "gtk_tool_palette_set_drag_source" gtk_tool_palette_set_drag_source :: 
    Ptr ToolPalette ->                      -- palette : TInterface (Name {namespace = "Gtk", name = "ToolPalette"})
    CUInt ->                                -- targets : TInterface (Name {namespace = "Gtk", name = "ToolPaletteDragTargets"})
    IO ()

-- | Sets the tool palette as a drag source.
-- Enables all groups and items in the tool palette as drag sources
-- on button 1 and button 3 press with copy and move actions.
-- See 'GI.Gtk.Objects.Widget.widgetDragSourceSet'.
-- 
-- /Since: 2.20/
toolPaletteSetDragSource ::
    (B.CallStack.HasCallStack, MonadIO m, IsToolPalette a) =>
    a
    -- ^ /@palette@/: a t'GI.Gtk.Objects.ToolPalette.ToolPalette'
    -> [Gtk.Flags.ToolPaletteDragTargets]
    -- ^ /@targets@/: the t'GI.Gtk.Flags.ToolPaletteDragTargets'
    --     which the widget should support
    -> m ()
toolPaletteSetDragSource palette targets = liftIO $ do
    palette' <- unsafeManagedPtrCastPtr palette
    let targets' = gflagsToWord targets
    gtk_tool_palette_set_drag_source palette' targets'
    touchManagedPtr palette
    return ()

#if defined(ENABLE_OVERLOADING)
data ToolPaletteSetDragSourceMethodInfo
instance (signature ~ ([Gtk.Flags.ToolPaletteDragTargets] -> m ()), MonadIO m, IsToolPalette a) => O.OverloadedMethod ToolPaletteSetDragSourceMethodInfo a signature where
    overloadedMethod = toolPaletteSetDragSource

instance O.OverloadedMethodInfo ToolPaletteSetDragSourceMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToolPalette.toolPaletteSetDragSource",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToolPalette.html#v:toolPaletteSetDragSource"
        })


#endif

-- method ToolPalette::set_exclusive
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "palette"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolPalette" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToolPalette" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolItemGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GtkToolItemGroup which is a child of palette"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "exclusive"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "whether the group should be exclusive or not"
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

foreign import ccall "gtk_tool_palette_set_exclusive" gtk_tool_palette_set_exclusive :: 
    Ptr ToolPalette ->                      -- palette : TInterface (Name {namespace = "Gtk", name = "ToolPalette"})
    Ptr Gtk.ToolItemGroup.ToolItemGroup ->  -- group : TInterface (Name {namespace = "Gtk", name = "ToolItemGroup"})
    CInt ->                                 -- exclusive : TBasicType TBoolean
    IO ()

-- | Sets whether the group should be exclusive or not.
-- If an exclusive group is expanded all other groups are collapsed.
-- 
-- /Since: 2.20/
toolPaletteSetExclusive ::
    (B.CallStack.HasCallStack, MonadIO m, IsToolPalette a, Gtk.ToolItemGroup.IsToolItemGroup b) =>
    a
    -- ^ /@palette@/: a t'GI.Gtk.Objects.ToolPalette.ToolPalette'
    -> b
    -- ^ /@group@/: a t'GI.Gtk.Objects.ToolItemGroup.ToolItemGroup' which is a child of palette
    -> Bool
    -- ^ /@exclusive@/: whether the group should be exclusive or not
    -> m ()
toolPaletteSetExclusive palette group exclusive = liftIO $ do
    palette' <- unsafeManagedPtrCastPtr palette
    group' <- unsafeManagedPtrCastPtr group
    let exclusive' = (fromIntegral . fromEnum) exclusive
    gtk_tool_palette_set_exclusive palette' group' exclusive'
    touchManagedPtr palette
    touchManagedPtr group
    return ()

#if defined(ENABLE_OVERLOADING)
data ToolPaletteSetExclusiveMethodInfo
instance (signature ~ (b -> Bool -> m ()), MonadIO m, IsToolPalette a, Gtk.ToolItemGroup.IsToolItemGroup b) => O.OverloadedMethod ToolPaletteSetExclusiveMethodInfo a signature where
    overloadedMethod = toolPaletteSetExclusive

instance O.OverloadedMethodInfo ToolPaletteSetExclusiveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToolPalette.toolPaletteSetExclusive",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToolPalette.html#v:toolPaletteSetExclusive"
        })


#endif

-- method ToolPalette::set_expand
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "palette"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolPalette" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToolPalette" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolItemGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GtkToolItemGroup which is a child of palette"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "expand"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "whether the group should be given extra space"
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

foreign import ccall "gtk_tool_palette_set_expand" gtk_tool_palette_set_expand :: 
    Ptr ToolPalette ->                      -- palette : TInterface (Name {namespace = "Gtk", name = "ToolPalette"})
    Ptr Gtk.ToolItemGroup.ToolItemGroup ->  -- group : TInterface (Name {namespace = "Gtk", name = "ToolItemGroup"})
    CInt ->                                 -- expand : TBasicType TBoolean
    IO ()

-- | Sets whether the group should be given extra space.
-- 
-- /Since: 2.20/
toolPaletteSetExpand ::
    (B.CallStack.HasCallStack, MonadIO m, IsToolPalette a, Gtk.ToolItemGroup.IsToolItemGroup b) =>
    a
    -- ^ /@palette@/: a t'GI.Gtk.Objects.ToolPalette.ToolPalette'
    -> b
    -- ^ /@group@/: a t'GI.Gtk.Objects.ToolItemGroup.ToolItemGroup' which is a child of palette
    -> Bool
    -- ^ /@expand@/: whether the group should be given extra space
    -> m ()
toolPaletteSetExpand palette group expand = liftIO $ do
    palette' <- unsafeManagedPtrCastPtr palette
    group' <- unsafeManagedPtrCastPtr group
    let expand' = (fromIntegral . fromEnum) expand
    gtk_tool_palette_set_expand palette' group' expand'
    touchManagedPtr palette
    touchManagedPtr group
    return ()

#if defined(ENABLE_OVERLOADING)
data ToolPaletteSetExpandMethodInfo
instance (signature ~ (b -> Bool -> m ()), MonadIO m, IsToolPalette a, Gtk.ToolItemGroup.IsToolItemGroup b) => O.OverloadedMethod ToolPaletteSetExpandMethodInfo a signature where
    overloadedMethod = toolPaletteSetExpand

instance O.OverloadedMethodInfo ToolPaletteSetExpandMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToolPalette.toolPaletteSetExpand",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToolPalette.html#v:toolPaletteSetExpand"
        })


#endif

-- method ToolPalette::set_group_position
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "palette"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolPalette" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToolPalette" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolItemGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GtkToolItemGroup which is a child of palette"
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
--                 { rawDocText = Just "a new index for group"
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

foreign import ccall "gtk_tool_palette_set_group_position" gtk_tool_palette_set_group_position :: 
    Ptr ToolPalette ->                      -- palette : TInterface (Name {namespace = "Gtk", name = "ToolPalette"})
    Ptr Gtk.ToolItemGroup.ToolItemGroup ->  -- group : TInterface (Name {namespace = "Gtk", name = "ToolItemGroup"})
    Int32 ->                                -- position : TBasicType TInt
    IO ()

-- | Sets the position of the group as an index of the tool palette.
-- If position is 0 the group will become the first child, if position is
-- -1 it will become the last child.
-- 
-- /Since: 2.20/
toolPaletteSetGroupPosition ::
    (B.CallStack.HasCallStack, MonadIO m, IsToolPalette a, Gtk.ToolItemGroup.IsToolItemGroup b) =>
    a
    -- ^ /@palette@/: a t'GI.Gtk.Objects.ToolPalette.ToolPalette'
    -> b
    -- ^ /@group@/: a t'GI.Gtk.Objects.ToolItemGroup.ToolItemGroup' which is a child of palette
    -> Int32
    -- ^ /@position@/: a new index for group
    -> m ()
toolPaletteSetGroupPosition palette group position = liftIO $ do
    palette' <- unsafeManagedPtrCastPtr palette
    group' <- unsafeManagedPtrCastPtr group
    gtk_tool_palette_set_group_position palette' group' position
    touchManagedPtr palette
    touchManagedPtr group
    return ()

#if defined(ENABLE_OVERLOADING)
data ToolPaletteSetGroupPositionMethodInfo
instance (signature ~ (b -> Int32 -> m ()), MonadIO m, IsToolPalette a, Gtk.ToolItemGroup.IsToolItemGroup b) => O.OverloadedMethod ToolPaletteSetGroupPositionMethodInfo a signature where
    overloadedMethod = toolPaletteSetGroupPosition

instance O.OverloadedMethodInfo ToolPaletteSetGroupPositionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToolPalette.toolPaletteSetGroupPosition",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToolPalette.html#v:toolPaletteSetGroupPosition"
        })


#endif

-- method ToolPalette::set_icon_size
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "palette"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolPalette" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToolPalette" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icon_size"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the #GtkIconSize that icons in the tool\n    palette shall have"
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

foreign import ccall "gtk_tool_palette_set_icon_size" gtk_tool_palette_set_icon_size :: 
    Ptr ToolPalette ->                      -- palette : TInterface (Name {namespace = "Gtk", name = "ToolPalette"})
    Int32 ->                                -- icon_size : TBasicType TInt
    IO ()

-- | Sets the size of icons in the tool palette.
-- 
-- /Since: 2.20/
toolPaletteSetIconSize ::
    (B.CallStack.HasCallStack, MonadIO m, IsToolPalette a) =>
    a
    -- ^ /@palette@/: a t'GI.Gtk.Objects.ToolPalette.ToolPalette'
    -> Int32
    -- ^ /@iconSize@/: the t'GI.Gtk.Enums.IconSize' that icons in the tool
    --     palette shall have
    -> m ()
toolPaletteSetIconSize palette iconSize = liftIO $ do
    palette' <- unsafeManagedPtrCastPtr palette
    gtk_tool_palette_set_icon_size palette' iconSize
    touchManagedPtr palette
    return ()

#if defined(ENABLE_OVERLOADING)
data ToolPaletteSetIconSizeMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsToolPalette a) => O.OverloadedMethod ToolPaletteSetIconSizeMethodInfo a signature where
    overloadedMethod = toolPaletteSetIconSize

instance O.OverloadedMethodInfo ToolPaletteSetIconSizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToolPalette.toolPaletteSetIconSize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToolPalette.html#v:toolPaletteSetIconSize"
        })


#endif

-- method ToolPalette::set_style
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "palette"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolPalette" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToolPalette" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "style"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolbarStyle" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the #GtkToolbarStyle that items in the tool palette shall have"
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

foreign import ccall "gtk_tool_palette_set_style" gtk_tool_palette_set_style :: 
    Ptr ToolPalette ->                      -- palette : TInterface (Name {namespace = "Gtk", name = "ToolPalette"})
    CUInt ->                                -- style : TInterface (Name {namespace = "Gtk", name = "ToolbarStyle"})
    IO ()

-- | Sets the style (text, icons or both) of items in the tool palette.
-- 
-- /Since: 2.20/
toolPaletteSetStyle ::
    (B.CallStack.HasCallStack, MonadIO m, IsToolPalette a) =>
    a
    -- ^ /@palette@/: a t'GI.Gtk.Objects.ToolPalette.ToolPalette'
    -> Gtk.Enums.ToolbarStyle
    -- ^ /@style@/: the t'GI.Gtk.Enums.ToolbarStyle' that items in the tool palette shall have
    -> m ()
toolPaletteSetStyle palette style = liftIO $ do
    palette' <- unsafeManagedPtrCastPtr palette
    let style' = (fromIntegral . fromEnum) style
    gtk_tool_palette_set_style palette' style'
    touchManagedPtr palette
    return ()

#if defined(ENABLE_OVERLOADING)
data ToolPaletteSetStyleMethodInfo
instance (signature ~ (Gtk.Enums.ToolbarStyle -> m ()), MonadIO m, IsToolPalette a) => O.OverloadedMethod ToolPaletteSetStyleMethodInfo a signature where
    overloadedMethod = toolPaletteSetStyle

instance O.OverloadedMethodInfo ToolPaletteSetStyleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToolPalette.toolPaletteSetStyle",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToolPalette.html#v:toolPaletteSetStyle"
        })


#endif

-- method ToolPalette::unset_icon_size
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "palette"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolPalette" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToolPalette" , sinceVersion = Nothing }
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

foreign import ccall "gtk_tool_palette_unset_icon_size" gtk_tool_palette_unset_icon_size :: 
    Ptr ToolPalette ->                      -- palette : TInterface (Name {namespace = "Gtk", name = "ToolPalette"})
    IO ()

-- | Unsets the tool palette icon size set with 'GI.Gtk.Objects.ToolPalette.toolPaletteSetIconSize',
-- so that user preferences will be used to determine the icon size.
-- 
-- /Since: 2.20/
toolPaletteUnsetIconSize ::
    (B.CallStack.HasCallStack, MonadIO m, IsToolPalette a) =>
    a
    -- ^ /@palette@/: a t'GI.Gtk.Objects.ToolPalette.ToolPalette'
    -> m ()
toolPaletteUnsetIconSize palette = liftIO $ do
    palette' <- unsafeManagedPtrCastPtr palette
    gtk_tool_palette_unset_icon_size palette'
    touchManagedPtr palette
    return ()

#if defined(ENABLE_OVERLOADING)
data ToolPaletteUnsetIconSizeMethodInfo
instance (signature ~ (m ()), MonadIO m, IsToolPalette a) => O.OverloadedMethod ToolPaletteUnsetIconSizeMethodInfo a signature where
    overloadedMethod = toolPaletteUnsetIconSize

instance O.OverloadedMethodInfo ToolPaletteUnsetIconSizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToolPalette.toolPaletteUnsetIconSize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToolPalette.html#v:toolPaletteUnsetIconSize"
        })


#endif

-- method ToolPalette::unset_style
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "palette"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ToolPalette" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkToolPalette" , sinceVersion = Nothing }
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

foreign import ccall "gtk_tool_palette_unset_style" gtk_tool_palette_unset_style :: 
    Ptr ToolPalette ->                      -- palette : TInterface (Name {namespace = "Gtk", name = "ToolPalette"})
    IO ()

-- | Unsets a toolbar style set with 'GI.Gtk.Objects.ToolPalette.toolPaletteSetStyle',
-- so that user preferences will be used to determine the toolbar style.
-- 
-- /Since: 2.20/
toolPaletteUnsetStyle ::
    (B.CallStack.HasCallStack, MonadIO m, IsToolPalette a) =>
    a
    -- ^ /@palette@/: a t'GI.Gtk.Objects.ToolPalette.ToolPalette'
    -> m ()
toolPaletteUnsetStyle palette = liftIO $ do
    palette' <- unsafeManagedPtrCastPtr palette
    gtk_tool_palette_unset_style palette'
    touchManagedPtr palette
    return ()

#if defined(ENABLE_OVERLOADING)
data ToolPaletteUnsetStyleMethodInfo
instance (signature ~ (m ()), MonadIO m, IsToolPalette a) => O.OverloadedMethod ToolPaletteUnsetStyleMethodInfo a signature where
    overloadedMethod = toolPaletteUnsetStyle

instance O.OverloadedMethodInfo ToolPaletteUnsetStyleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ToolPalette.toolPaletteUnsetStyle",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ToolPalette.html#v:toolPaletteUnsetStyle"
        })


#endif

-- method ToolPalette::get_drag_target_group
-- method type : MemberFunction
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "TargetEntry" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tool_palette_get_drag_target_group" gtk_tool_palette_get_drag_target_group :: 
    IO (Ptr Gtk.TargetEntry.TargetEntry)

-- | Get the target entry for a dragged t'GI.Gtk.Objects.ToolItemGroup.ToolItemGroup'.
-- 
-- /Since: 2.20/
toolPaletteGetDragTargetGroup ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m Gtk.TargetEntry.TargetEntry
    -- ^ __Returns:__ the t'GI.Gtk.Structs.TargetEntry.TargetEntry' for a dragged group
toolPaletteGetDragTargetGroup  = liftIO $ do
    result <- gtk_tool_palette_get_drag_target_group
    checkUnexpectedReturnNULL "toolPaletteGetDragTargetGroup" result
    result' <- (newBoxed Gtk.TargetEntry.TargetEntry) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method ToolPalette::get_drag_target_item
-- method type : MemberFunction
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "TargetEntry" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tool_palette_get_drag_target_item" gtk_tool_palette_get_drag_target_item :: 
    IO (Ptr Gtk.TargetEntry.TargetEntry)

-- | Gets the target entry for a dragged t'GI.Gtk.Objects.ToolItem.ToolItem'.
-- 
-- /Since: 2.20/
toolPaletteGetDragTargetItem ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m Gtk.TargetEntry.TargetEntry
    -- ^ __Returns:__ the t'GI.Gtk.Structs.TargetEntry.TargetEntry' for a dragged item.
toolPaletteGetDragTargetItem  = liftIO $ do
    result <- gtk_tool_palette_get_drag_target_item
    checkUnexpectedReturnNULL "toolPaletteGetDragTargetItem" result
    result' <- (newBoxed Gtk.TargetEntry.TargetEntry) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif


