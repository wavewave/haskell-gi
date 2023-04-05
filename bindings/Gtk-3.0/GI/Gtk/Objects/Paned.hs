{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Objects.Paned.Paned' has two panes, arranged either
-- horizontally or vertically. The division between
-- the two panes is adjustable by the user by dragging
-- a handle.
-- 
-- Child widgets are
-- added to the panes of the widget with 'GI.Gtk.Objects.Paned.panedPack1' and
-- 'GI.Gtk.Objects.Paned.panedPack2'. The division between the two children is set by default
-- from the size requests of the children, but it can be adjusted by the
-- user.
-- 
-- A paned widget draws a separator between the two child widgets and a
-- small handle that the user can drag to adjust the division. It does not
-- draw any relief around the children or around the separator. (The space
-- in which the separator is called the gutter.) Often, it is useful to put
-- each child inside a t'GI.Gtk.Objects.Frame.Frame' with the shadow type set to 'GI.Gtk.Enums.ShadowTypeIn'
-- so that the gutter appears as a ridge. No separator is drawn if one of
-- the children is missing.
-- 
-- Each child has two options that can be set, /@resize@/ and /@shrink@/. If
-- /@resize@/ is true, then when the t'GI.Gtk.Objects.Paned.Paned' is resized, that child will
-- expand or shrink along with the paned widget. If /@shrink@/ is true, then
-- that child can be made smaller than its requisition by the user.
-- Setting /@shrink@/ to 'P.False' allows the application to set a minimum size.
-- If /@resize@/ is false for both children, then this is treated as if
-- /@resize@/ is true for both children.
-- 
-- The application can set the position of the slider as if it were set
-- by the user, by calling 'GI.Gtk.Objects.Paned.panedSetPosition'.
-- 
-- = CSS nodes
-- 
-- 
-- === /plain code/
-- >
-- >paned
-- >├── <child>
-- >├── separator[.wide]
-- >╰── <child>
-- 
-- 
-- GtkPaned has a main CSS node with name paned, and a subnode for
-- the separator with name separator. The subnode gets a .wide style
-- class when the paned is supposed to be wide.
-- 
-- In horizontal orientation, the nodes of the children are always arranged
-- from left to right. So :first-child will always select the leftmost child,
-- regardless of text direction.
-- 
-- == Creating a paned widget with minimum sizes.
-- 
-- 
-- === /C code/
-- >
-- >GtkWidget *hpaned = gtk_paned_new (GTK_ORIENTATION_HORIZONTAL);
-- >GtkWidget *frame1 = gtk_frame_new (NULL);
-- >GtkWidget *frame2 = gtk_frame_new (NULL);
-- >gtk_frame_set_shadow_type (GTK_FRAME (frame1), GTK_SHADOW_IN);
-- >gtk_frame_set_shadow_type (GTK_FRAME (frame2), GTK_SHADOW_IN);
-- >
-- >gtk_widget_set_size_request (hpaned, 200, -1);
-- >
-- >gtk_paned_pack1 (GTK_PANED (hpaned), frame1, TRUE, FALSE);
-- >gtk_widget_set_size_request (frame1, 50, -1);
-- >
-- >gtk_paned_pack2 (GTK_PANED (hpaned), frame2, FALSE, FALSE);
-- >gtk_widget_set_size_request (frame2, 50, -1);
-- 

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.Paned
    ( 

-- * Exported types
    Paned(..)                               ,
    IsPaned                                 ,
    toPaned                                 ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [add]("GI.Gtk.Objects.Container#g:method:add"), [add1]("GI.Gtk.Objects.Paned#g:method:add1"), [add2]("GI.Gtk.Objects.Paned#g:method:add2"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [pack1]("GI.Gtk.Objects.Paned#g:method:pack1"), [pack2]("GI.Gtk.Objects.Paned#g:method:pack2"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChild1]("GI.Gtk.Objects.Paned#g:method:getChild1"), [getChild2]("GI.Gtk.Objects.Paned#g:method:getChild2"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHandleWindow]("GI.Gtk.Objects.Paned#g:method:getHandleWindow"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getOrientation]("GI.Gtk.Interfaces.Orientable#g:method:getOrientation"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPosition]("GI.Gtk.Objects.Paned#g:method:getPosition"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWideHandle]("GI.Gtk.Objects.Paned#g:method:getWideHandle"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setOrientation]("GI.Gtk.Interfaces.Orientable#g:method:setOrientation"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setPosition]("GI.Gtk.Objects.Paned#g:method:setPosition"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWideHandle]("GI.Gtk.Objects.Paned#g:method:setWideHandle"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolvePanedMethod                      ,
#endif

-- ** add1 #method:add1#

#if defined(ENABLE_OVERLOADING)
    PanedAdd1MethodInfo                     ,
#endif
    panedAdd1                               ,


-- ** add2 #method:add2#

#if defined(ENABLE_OVERLOADING)
    PanedAdd2MethodInfo                     ,
#endif
    panedAdd2                               ,


-- ** getChild1 #method:getChild1#

#if defined(ENABLE_OVERLOADING)
    PanedGetChild1MethodInfo                ,
#endif
    panedGetChild1                          ,


-- ** getChild2 #method:getChild2#

#if defined(ENABLE_OVERLOADING)
    PanedGetChild2MethodInfo                ,
#endif
    panedGetChild2                          ,


-- ** getHandleWindow #method:getHandleWindow#

#if defined(ENABLE_OVERLOADING)
    PanedGetHandleWindowMethodInfo          ,
#endif
    panedGetHandleWindow                    ,


-- ** getPosition #method:getPosition#

#if defined(ENABLE_OVERLOADING)
    PanedGetPositionMethodInfo              ,
#endif
    panedGetPosition                        ,


-- ** getWideHandle #method:getWideHandle#

#if defined(ENABLE_OVERLOADING)
    PanedGetWideHandleMethodInfo            ,
#endif
    panedGetWideHandle                      ,


-- ** new #method:new#

    panedNew                                ,


-- ** pack1 #method:pack1#

#if defined(ENABLE_OVERLOADING)
    PanedPack1MethodInfo                    ,
#endif
    panedPack1                              ,


-- ** pack2 #method:pack2#

#if defined(ENABLE_OVERLOADING)
    PanedPack2MethodInfo                    ,
#endif
    panedPack2                              ,


-- ** setPosition #method:setPosition#

#if defined(ENABLE_OVERLOADING)
    PanedSetPositionMethodInfo              ,
#endif
    panedSetPosition                        ,


-- ** setWideHandle #method:setWideHandle#

#if defined(ENABLE_OVERLOADING)
    PanedSetWideHandleMethodInfo            ,
#endif
    panedSetWideHandle                      ,




 -- * Properties


-- ** maxPosition #attr:maxPosition#
-- | The largest possible value for the position property.
-- This property is derived from the size and shrinkability
-- of the widget\'s children.
-- 
-- /Since: 2.4/

#if defined(ENABLE_OVERLOADING)
    PanedMaxPositionPropertyInfo            ,
#endif
    getPanedMaxPosition                     ,
#if defined(ENABLE_OVERLOADING)
    panedMaxPosition                        ,
#endif


-- ** minPosition #attr:minPosition#
-- | The smallest possible value for the position property.
-- This property is derived from the size and shrinkability
-- of the widget\'s children.
-- 
-- /Since: 2.4/

#if defined(ENABLE_OVERLOADING)
    PanedMinPositionPropertyInfo            ,
#endif
    getPanedMinPosition                     ,
#if defined(ENABLE_OVERLOADING)
    panedMinPosition                        ,
#endif


-- ** position #attr:position#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    PanedPositionPropertyInfo               ,
#endif
    constructPanedPosition                  ,
    getPanedPosition                        ,
#if defined(ENABLE_OVERLOADING)
    panedPosition                           ,
#endif
    setPanedPosition                        ,


-- ** positionSet #attr:positionSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    PanedPositionSetPropertyInfo            ,
#endif
    constructPanedPositionSet               ,
    getPanedPositionSet                     ,
#if defined(ENABLE_OVERLOADING)
    panedPositionSet                        ,
#endif
    setPanedPositionSet                     ,


-- ** wideHandle #attr:wideHandle#
-- | Setting this property to 'P.True' indicates that the paned needs
-- to provide stronger visual separation (e.g. because it separates
-- between two notebooks, whose tab rows would otherwise merge visually).
-- 
-- /Since: 3.16/

#if defined(ENABLE_OVERLOADING)
    PanedWideHandlePropertyInfo             ,
#endif
    constructPanedWideHandle                ,
    getPanedWideHandle                      ,
#if defined(ENABLE_OVERLOADING)
    panedWideHandle                         ,
#endif
    setPanedWideHandle                      ,




 -- * Signals


-- ** acceptPosition #signal:acceptPosition#

    PanedAcceptPositionCallback             ,
#if defined(ENABLE_OVERLOADING)
    PanedAcceptPositionSignalInfo           ,
#endif
    afterPanedAcceptPosition                ,
    onPanedAcceptPosition                   ,


-- ** cancelPosition #signal:cancelPosition#

    PanedCancelPositionCallback             ,
#if defined(ENABLE_OVERLOADING)
    PanedCancelPositionSignalInfo           ,
#endif
    afterPanedCancelPosition                ,
    onPanedCancelPosition                   ,


-- ** cycleChildFocus #signal:cycleChildFocus#

    PanedCycleChildFocusCallback            ,
#if defined(ENABLE_OVERLOADING)
    PanedCycleChildFocusSignalInfo          ,
#endif
    afterPanedCycleChildFocus               ,
    onPanedCycleChildFocus                  ,


-- ** cycleHandleFocus #signal:cycleHandleFocus#

    PanedCycleHandleFocusCallback           ,
#if defined(ENABLE_OVERLOADING)
    PanedCycleHandleFocusSignalInfo         ,
#endif
    afterPanedCycleHandleFocus              ,
    onPanedCycleHandleFocus                 ,


-- ** moveHandle #signal:moveHandle#

    PanedMoveHandleCallback                 ,
#if defined(ENABLE_OVERLOADING)
    PanedMoveHandleSignalInfo               ,
#endif
    afterPanedMoveHandle                    ,
    onPanedMoveHandle                       ,


-- ** toggleHandleFocus #signal:toggleHandleFocus#

    PanedToggleHandleFocusCallback          ,
#if defined(ENABLE_OVERLOADING)
    PanedToggleHandleFocusSignalInfo        ,
#endif
    afterPanedToggleHandleFocus             ,
    onPanedToggleHandleFocus                ,




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
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Orientable as Gtk.Orientable
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype Paned = Paned (SP.ManagedPtr Paned)
    deriving (Eq)

instance SP.ManagedPtrNewtype Paned where
    toManagedPtr (Paned p) = p

foreign import ccall "gtk_paned_get_type"
    c_gtk_paned_get_type :: IO B.Types.GType

instance B.Types.TypedObject Paned where
    glibType = c_gtk_paned_get_type

instance B.Types.GObject Paned

-- | Type class for types which can be safely cast to `Paned`, for instance with `toPaned`.
class (SP.GObject o, O.IsDescendantOf Paned o) => IsPaned o
instance (SP.GObject o, O.IsDescendantOf Paned o) => IsPaned o

instance O.HasParentTypes Paned
type instance O.ParentTypes Paned = '[Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable, Gtk.Orientable.Orientable]

-- | Cast to `Paned`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toPaned :: (MIO.MonadIO m, IsPaned o) => o -> m Paned
toPaned = MIO.liftIO . B.ManagedPtr.unsafeCastTo Paned

-- | Convert 'Paned' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe Paned) where
    gvalueGType_ = c_gtk_paned_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr Paned)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr Paned)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject Paned ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolvePanedMethod (t :: Symbol) (o :: *) :: * where
    ResolvePanedMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolvePanedMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolvePanedMethod "add1" o = PanedAdd1MethodInfo
    ResolvePanedMethod "add2" o = PanedAdd2MethodInfo
    ResolvePanedMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolvePanedMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolvePanedMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolvePanedMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolvePanedMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolvePanedMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolvePanedMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolvePanedMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolvePanedMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolvePanedMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolvePanedMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolvePanedMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolvePanedMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolvePanedMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolvePanedMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolvePanedMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolvePanedMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolvePanedMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolvePanedMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolvePanedMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolvePanedMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolvePanedMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolvePanedMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolvePanedMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolvePanedMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolvePanedMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolvePanedMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolvePanedMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolvePanedMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolvePanedMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolvePanedMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolvePanedMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolvePanedMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolvePanedMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolvePanedMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolvePanedMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolvePanedMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolvePanedMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolvePanedMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolvePanedMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolvePanedMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolvePanedMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolvePanedMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolvePanedMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolvePanedMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolvePanedMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolvePanedMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolvePanedMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolvePanedMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolvePanedMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolvePanedMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolvePanedMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolvePanedMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolvePanedMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolvePanedMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolvePanedMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolvePanedMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolvePanedMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolvePanedMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolvePanedMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolvePanedMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolvePanedMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolvePanedMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolvePanedMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolvePanedMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolvePanedMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolvePanedMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolvePanedMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolvePanedMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolvePanedMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolvePanedMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolvePanedMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolvePanedMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolvePanedMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolvePanedMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolvePanedMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolvePanedMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolvePanedMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolvePanedMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolvePanedMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolvePanedMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolvePanedMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolvePanedMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolvePanedMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolvePanedMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolvePanedMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolvePanedMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolvePanedMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolvePanedMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolvePanedMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolvePanedMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolvePanedMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolvePanedMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolvePanedMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolvePanedMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolvePanedMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolvePanedMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolvePanedMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolvePanedMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolvePanedMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolvePanedMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolvePanedMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolvePanedMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolvePanedMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolvePanedMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolvePanedMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolvePanedMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolvePanedMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolvePanedMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolvePanedMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolvePanedMethod "pack1" o = PanedPack1MethodInfo
    ResolvePanedMethod "pack2" o = PanedPack2MethodInfo
    ResolvePanedMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolvePanedMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolvePanedMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolvePanedMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolvePanedMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolvePanedMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolvePanedMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolvePanedMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolvePanedMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolvePanedMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolvePanedMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolvePanedMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolvePanedMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolvePanedMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolvePanedMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolvePanedMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolvePanedMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolvePanedMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolvePanedMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolvePanedMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolvePanedMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolvePanedMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolvePanedMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolvePanedMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolvePanedMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolvePanedMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolvePanedMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolvePanedMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolvePanedMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolvePanedMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolvePanedMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolvePanedMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolvePanedMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolvePanedMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolvePanedMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolvePanedMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolvePanedMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolvePanedMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolvePanedMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolvePanedMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolvePanedMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolvePanedMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolvePanedMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolvePanedMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolvePanedMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolvePanedMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolvePanedMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolvePanedMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolvePanedMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolvePanedMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolvePanedMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolvePanedMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolvePanedMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolvePanedMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolvePanedMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolvePanedMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolvePanedMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolvePanedMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolvePanedMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolvePanedMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolvePanedMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolvePanedMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolvePanedMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolvePanedMethod "getChild1" o = PanedGetChild1MethodInfo
    ResolvePanedMethod "getChild2" o = PanedGetChild2MethodInfo
    ResolvePanedMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolvePanedMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolvePanedMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolvePanedMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolvePanedMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolvePanedMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolvePanedMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolvePanedMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolvePanedMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolvePanedMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolvePanedMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolvePanedMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolvePanedMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolvePanedMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolvePanedMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolvePanedMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolvePanedMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolvePanedMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolvePanedMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolvePanedMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolvePanedMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolvePanedMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolvePanedMethod "getHandleWindow" o = PanedGetHandleWindowMethodInfo
    ResolvePanedMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolvePanedMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolvePanedMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolvePanedMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolvePanedMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolvePanedMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolvePanedMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolvePanedMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolvePanedMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolvePanedMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolvePanedMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolvePanedMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolvePanedMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolvePanedMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolvePanedMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolvePanedMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolvePanedMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolvePanedMethod "getOrientation" o = Gtk.Orientable.OrientableGetOrientationMethodInfo
    ResolvePanedMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolvePanedMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolvePanedMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolvePanedMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolvePanedMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolvePanedMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolvePanedMethod "getPosition" o = PanedGetPositionMethodInfo
    ResolvePanedMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolvePanedMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolvePanedMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolvePanedMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolvePanedMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolvePanedMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolvePanedMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolvePanedMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolvePanedMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolvePanedMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolvePanedMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolvePanedMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolvePanedMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolvePanedMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolvePanedMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolvePanedMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolvePanedMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolvePanedMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolvePanedMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolvePanedMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolvePanedMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolvePanedMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolvePanedMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolvePanedMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolvePanedMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolvePanedMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolvePanedMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolvePanedMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolvePanedMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolvePanedMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolvePanedMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolvePanedMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolvePanedMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolvePanedMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolvePanedMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolvePanedMethod "getWideHandle" o = PanedGetWideHandleMethodInfo
    ResolvePanedMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolvePanedMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolvePanedMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolvePanedMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolvePanedMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolvePanedMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolvePanedMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolvePanedMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolvePanedMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolvePanedMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolvePanedMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolvePanedMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolvePanedMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolvePanedMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolvePanedMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolvePanedMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolvePanedMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolvePanedMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolvePanedMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolvePanedMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolvePanedMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolvePanedMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolvePanedMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolvePanedMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolvePanedMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolvePanedMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolvePanedMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolvePanedMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolvePanedMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolvePanedMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolvePanedMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolvePanedMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolvePanedMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolvePanedMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolvePanedMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolvePanedMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolvePanedMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolvePanedMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolvePanedMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolvePanedMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolvePanedMethod "setOrientation" o = Gtk.Orientable.OrientableSetOrientationMethodInfo
    ResolvePanedMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolvePanedMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolvePanedMethod "setPosition" o = PanedSetPositionMethodInfo
    ResolvePanedMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolvePanedMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolvePanedMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolvePanedMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolvePanedMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolvePanedMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolvePanedMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolvePanedMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolvePanedMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolvePanedMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolvePanedMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolvePanedMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolvePanedMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolvePanedMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolvePanedMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolvePanedMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolvePanedMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolvePanedMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolvePanedMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolvePanedMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolvePanedMethod "setWideHandle" o = PanedSetWideHandleMethodInfo
    ResolvePanedMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolvePanedMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolvePanedMethod t Paned, O.OverloadedMethod info Paned p) => OL.IsLabel t (Paned -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolvePanedMethod t Paned, O.OverloadedMethod info Paned p, R.HasField t Paned p) => R.HasField t Paned p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolvePanedMethod t Paned, O.OverloadedMethodInfo info Paned) => OL.IsLabel t (O.MethodProxy info Paned) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal Paned::accept-position
-- | The [acceptPosition](#g:signal:acceptPosition) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted to accept the current position of the handle when
-- moving it using key bindings.
-- 
-- The default binding for this signal is Return or Space.
-- 
-- /Since: 2.0/
type PanedAcceptPositionCallback =
    IO Bool

type C_PanedAcceptPositionCallback =
    Ptr Paned ->                            -- object
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_PanedAcceptPositionCallback`.
foreign import ccall "wrapper"
    mk_PanedAcceptPositionCallback :: C_PanedAcceptPositionCallback -> IO (FunPtr C_PanedAcceptPositionCallback)

wrap_PanedAcceptPositionCallback :: 
    GObject a => (a -> PanedAcceptPositionCallback) ->
    C_PanedAcceptPositionCallback
wrap_PanedAcceptPositionCallback gi'cb gi'selfPtr _ = do
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 
    let result' = (fromIntegral . fromEnum) result
    return result'


-- | Connect a signal handler for the [acceptPosition](#signal:acceptPosition) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' paned #acceptPosition callback
-- @
-- 
-- 
onPanedAcceptPosition :: (IsPaned a, MonadIO m) => a -> ((?self :: a) => PanedAcceptPositionCallback) -> m SignalHandlerId
onPanedAcceptPosition obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PanedAcceptPositionCallback wrapped
    wrapped'' <- mk_PanedAcceptPositionCallback wrapped'
    connectSignalFunPtr obj "accept-position" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [acceptPosition](#signal:acceptPosition) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' paned #acceptPosition callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterPanedAcceptPosition :: (IsPaned a, MonadIO m) => a -> ((?self :: a) => PanedAcceptPositionCallback) -> m SignalHandlerId
afterPanedAcceptPosition obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PanedAcceptPositionCallback wrapped
    wrapped'' <- mk_PanedAcceptPositionCallback wrapped'
    connectSignalFunPtr obj "accept-position" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data PanedAcceptPositionSignalInfo
instance SignalInfo PanedAcceptPositionSignalInfo where
    type HaskellCallbackType PanedAcceptPositionSignalInfo = PanedAcceptPositionCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_PanedAcceptPositionCallback cb
        cb'' <- mk_PanedAcceptPositionCallback cb'
        connectSignalFunPtr obj "accept-position" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Paned::accept-position"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Paned.html#g:signal:acceptPosition"})

#endif

-- signal Paned::cancel-position
-- | The [cancelPosition](#g:signal:cancelPosition) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted to cancel moving the position of the handle using key
-- bindings. The position of the handle will be reset to the value prior to
-- moving it.
-- 
-- The default binding for this signal is Escape.
-- 
-- /Since: 2.0/
type PanedCancelPositionCallback =
    IO Bool

type C_PanedCancelPositionCallback =
    Ptr Paned ->                            -- object
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_PanedCancelPositionCallback`.
foreign import ccall "wrapper"
    mk_PanedCancelPositionCallback :: C_PanedCancelPositionCallback -> IO (FunPtr C_PanedCancelPositionCallback)

wrap_PanedCancelPositionCallback :: 
    GObject a => (a -> PanedCancelPositionCallback) ->
    C_PanedCancelPositionCallback
wrap_PanedCancelPositionCallback gi'cb gi'selfPtr _ = do
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 
    let result' = (fromIntegral . fromEnum) result
    return result'


-- | Connect a signal handler for the [cancelPosition](#signal:cancelPosition) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' paned #cancelPosition callback
-- @
-- 
-- 
onPanedCancelPosition :: (IsPaned a, MonadIO m) => a -> ((?self :: a) => PanedCancelPositionCallback) -> m SignalHandlerId
onPanedCancelPosition obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PanedCancelPositionCallback wrapped
    wrapped'' <- mk_PanedCancelPositionCallback wrapped'
    connectSignalFunPtr obj "cancel-position" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [cancelPosition](#signal:cancelPosition) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' paned #cancelPosition callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterPanedCancelPosition :: (IsPaned a, MonadIO m) => a -> ((?self :: a) => PanedCancelPositionCallback) -> m SignalHandlerId
afterPanedCancelPosition obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PanedCancelPositionCallback wrapped
    wrapped'' <- mk_PanedCancelPositionCallback wrapped'
    connectSignalFunPtr obj "cancel-position" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data PanedCancelPositionSignalInfo
instance SignalInfo PanedCancelPositionSignalInfo where
    type HaskellCallbackType PanedCancelPositionSignalInfo = PanedCancelPositionCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_PanedCancelPositionCallback cb
        cb'' <- mk_PanedCancelPositionCallback cb'
        connectSignalFunPtr obj "cancel-position" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Paned::cancel-position"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Paned.html#g:signal:cancelPosition"})

#endif

-- signal Paned::cycle-child-focus
-- | The [cycleChildFocus](#g:signal:cycleChildFocus) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted to cycle the focus between the children of the paned.
-- 
-- The default binding is f6.
-- 
-- /Since: 2.0/
type PanedCycleChildFocusCallback =
    Bool
    -- ^ /@reversed@/: whether cycling backward or forward
    -> IO Bool

type C_PanedCycleChildFocusCallback =
    Ptr Paned ->                            -- object
    CInt ->
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_PanedCycleChildFocusCallback`.
foreign import ccall "wrapper"
    mk_PanedCycleChildFocusCallback :: C_PanedCycleChildFocusCallback -> IO (FunPtr C_PanedCycleChildFocusCallback)

wrap_PanedCycleChildFocusCallback :: 
    GObject a => (a -> PanedCycleChildFocusCallback) ->
    C_PanedCycleChildFocusCallback
wrap_PanedCycleChildFocusCallback gi'cb gi'selfPtr reversed _ = do
    let reversed' = (/= 0) reversed
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  reversed'
    let result' = (fromIntegral . fromEnum) result
    return result'


-- | Connect a signal handler for the [cycleChildFocus](#signal:cycleChildFocus) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' paned #cycleChildFocus callback
-- @
-- 
-- 
onPanedCycleChildFocus :: (IsPaned a, MonadIO m) => a -> ((?self :: a) => PanedCycleChildFocusCallback) -> m SignalHandlerId
onPanedCycleChildFocus obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PanedCycleChildFocusCallback wrapped
    wrapped'' <- mk_PanedCycleChildFocusCallback wrapped'
    connectSignalFunPtr obj "cycle-child-focus" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [cycleChildFocus](#signal:cycleChildFocus) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' paned #cycleChildFocus callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterPanedCycleChildFocus :: (IsPaned a, MonadIO m) => a -> ((?self :: a) => PanedCycleChildFocusCallback) -> m SignalHandlerId
afterPanedCycleChildFocus obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PanedCycleChildFocusCallback wrapped
    wrapped'' <- mk_PanedCycleChildFocusCallback wrapped'
    connectSignalFunPtr obj "cycle-child-focus" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data PanedCycleChildFocusSignalInfo
instance SignalInfo PanedCycleChildFocusSignalInfo where
    type HaskellCallbackType PanedCycleChildFocusSignalInfo = PanedCycleChildFocusCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_PanedCycleChildFocusCallback cb
        cb'' <- mk_PanedCycleChildFocusCallback cb'
        connectSignalFunPtr obj "cycle-child-focus" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Paned::cycle-child-focus"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Paned.html#g:signal:cycleChildFocus"})

#endif

-- signal Paned::cycle-handle-focus
-- | The [cycleHandleFocus](#g:signal:cycleHandleFocus) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted to cycle whether the paned should grab focus to allow
-- the user to change position of the handle by using key bindings.
-- 
-- The default binding for this signal is f8.
-- 
-- /Since: 2.0/
type PanedCycleHandleFocusCallback =
    Bool
    -- ^ /@reversed@/: whether cycling backward or forward
    -> IO Bool

type C_PanedCycleHandleFocusCallback =
    Ptr Paned ->                            -- object
    CInt ->
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_PanedCycleHandleFocusCallback`.
foreign import ccall "wrapper"
    mk_PanedCycleHandleFocusCallback :: C_PanedCycleHandleFocusCallback -> IO (FunPtr C_PanedCycleHandleFocusCallback)

wrap_PanedCycleHandleFocusCallback :: 
    GObject a => (a -> PanedCycleHandleFocusCallback) ->
    C_PanedCycleHandleFocusCallback
wrap_PanedCycleHandleFocusCallback gi'cb gi'selfPtr reversed _ = do
    let reversed' = (/= 0) reversed
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  reversed'
    let result' = (fromIntegral . fromEnum) result
    return result'


-- | Connect a signal handler for the [cycleHandleFocus](#signal:cycleHandleFocus) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' paned #cycleHandleFocus callback
-- @
-- 
-- 
onPanedCycleHandleFocus :: (IsPaned a, MonadIO m) => a -> ((?self :: a) => PanedCycleHandleFocusCallback) -> m SignalHandlerId
onPanedCycleHandleFocus obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PanedCycleHandleFocusCallback wrapped
    wrapped'' <- mk_PanedCycleHandleFocusCallback wrapped'
    connectSignalFunPtr obj "cycle-handle-focus" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [cycleHandleFocus](#signal:cycleHandleFocus) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' paned #cycleHandleFocus callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterPanedCycleHandleFocus :: (IsPaned a, MonadIO m) => a -> ((?self :: a) => PanedCycleHandleFocusCallback) -> m SignalHandlerId
afterPanedCycleHandleFocus obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PanedCycleHandleFocusCallback wrapped
    wrapped'' <- mk_PanedCycleHandleFocusCallback wrapped'
    connectSignalFunPtr obj "cycle-handle-focus" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data PanedCycleHandleFocusSignalInfo
instance SignalInfo PanedCycleHandleFocusSignalInfo where
    type HaskellCallbackType PanedCycleHandleFocusSignalInfo = PanedCycleHandleFocusCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_PanedCycleHandleFocusCallback cb
        cb'' <- mk_PanedCycleHandleFocusCallback cb'
        connectSignalFunPtr obj "cycle-handle-focus" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Paned::cycle-handle-focus"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Paned.html#g:signal:cycleHandleFocus"})

#endif

-- signal Paned::move-handle
-- | The [moveHandle](#g:signal:moveHandle) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted to move the handle when the user is using key bindings
-- to move it.
-- 
-- /Since: 2.0/
type PanedMoveHandleCallback =
    Gtk.Enums.ScrollType
    -- ^ /@scrollType@/: a t'GI.Gtk.Enums.ScrollType'
    -> IO Bool

type C_PanedMoveHandleCallback =
    Ptr Paned ->                            -- object
    CUInt ->
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_PanedMoveHandleCallback`.
foreign import ccall "wrapper"
    mk_PanedMoveHandleCallback :: C_PanedMoveHandleCallback -> IO (FunPtr C_PanedMoveHandleCallback)

wrap_PanedMoveHandleCallback :: 
    GObject a => (a -> PanedMoveHandleCallback) ->
    C_PanedMoveHandleCallback
wrap_PanedMoveHandleCallback gi'cb gi'selfPtr scrollType _ = do
    let scrollType' = (toEnum . fromIntegral) scrollType
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  scrollType'
    let result' = (fromIntegral . fromEnum) result
    return result'


-- | Connect a signal handler for the [moveHandle](#signal:moveHandle) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' paned #moveHandle callback
-- @
-- 
-- 
onPanedMoveHandle :: (IsPaned a, MonadIO m) => a -> ((?self :: a) => PanedMoveHandleCallback) -> m SignalHandlerId
onPanedMoveHandle obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PanedMoveHandleCallback wrapped
    wrapped'' <- mk_PanedMoveHandleCallback wrapped'
    connectSignalFunPtr obj "move-handle" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [moveHandle](#signal:moveHandle) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' paned #moveHandle callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterPanedMoveHandle :: (IsPaned a, MonadIO m) => a -> ((?self :: a) => PanedMoveHandleCallback) -> m SignalHandlerId
afterPanedMoveHandle obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PanedMoveHandleCallback wrapped
    wrapped'' <- mk_PanedMoveHandleCallback wrapped'
    connectSignalFunPtr obj "move-handle" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data PanedMoveHandleSignalInfo
instance SignalInfo PanedMoveHandleSignalInfo where
    type HaskellCallbackType PanedMoveHandleSignalInfo = PanedMoveHandleCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_PanedMoveHandleCallback cb
        cb'' <- mk_PanedMoveHandleCallback cb'
        connectSignalFunPtr obj "move-handle" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Paned::move-handle"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Paned.html#g:signal:moveHandle"})

#endif

-- signal Paned::toggle-handle-focus
-- | The [toggleHandleFocus](#g:signal:toggleHandleFocus) is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted to accept the current position of the handle and then
-- move focus to the next widget in the focus chain.
-- 
-- The default binding is Tab.
-- 
-- /Since: 2.0/
type PanedToggleHandleFocusCallback =
    IO Bool

type C_PanedToggleHandleFocusCallback =
    Ptr Paned ->                            -- object
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_PanedToggleHandleFocusCallback`.
foreign import ccall "wrapper"
    mk_PanedToggleHandleFocusCallback :: C_PanedToggleHandleFocusCallback -> IO (FunPtr C_PanedToggleHandleFocusCallback)

wrap_PanedToggleHandleFocusCallback :: 
    GObject a => (a -> PanedToggleHandleFocusCallback) ->
    C_PanedToggleHandleFocusCallback
wrap_PanedToggleHandleFocusCallback gi'cb gi'selfPtr _ = do
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 
    let result' = (fromIntegral . fromEnum) result
    return result'


-- | Connect a signal handler for the [toggleHandleFocus](#signal:toggleHandleFocus) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' paned #toggleHandleFocus callback
-- @
-- 
-- 
onPanedToggleHandleFocus :: (IsPaned a, MonadIO m) => a -> ((?self :: a) => PanedToggleHandleFocusCallback) -> m SignalHandlerId
onPanedToggleHandleFocus obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PanedToggleHandleFocusCallback wrapped
    wrapped'' <- mk_PanedToggleHandleFocusCallback wrapped'
    connectSignalFunPtr obj "toggle-handle-focus" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [toggleHandleFocus](#signal:toggleHandleFocus) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' paned #toggleHandleFocus callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterPanedToggleHandleFocus :: (IsPaned a, MonadIO m) => a -> ((?self :: a) => PanedToggleHandleFocusCallback) -> m SignalHandlerId
afterPanedToggleHandleFocus obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_PanedToggleHandleFocusCallback wrapped
    wrapped'' <- mk_PanedToggleHandleFocusCallback wrapped'
    connectSignalFunPtr obj "toggle-handle-focus" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data PanedToggleHandleFocusSignalInfo
instance SignalInfo PanedToggleHandleFocusSignalInfo where
    type HaskellCallbackType PanedToggleHandleFocusSignalInfo = PanedToggleHandleFocusCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_PanedToggleHandleFocusCallback cb
        cb'' <- mk_PanedToggleHandleFocusCallback cb'
        connectSignalFunPtr obj "toggle-handle-focus" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Paned::toggle-handle-focus"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Paned.html#g:signal:toggleHandleFocus"})

#endif

-- VVV Prop "max-position"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@max-position@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' paned #maxPosition
-- @
getPanedMaxPosition :: (MonadIO m, IsPaned o) => o -> m Int32
getPanedMaxPosition obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "max-position"

#if defined(ENABLE_OVERLOADING)
data PanedMaxPositionPropertyInfo
instance AttrInfo PanedMaxPositionPropertyInfo where
    type AttrAllowedOps PanedMaxPositionPropertyInfo = '[ 'AttrGet]
    type AttrBaseTypeConstraint PanedMaxPositionPropertyInfo = IsPaned
    type AttrSetTypeConstraint PanedMaxPositionPropertyInfo = (~) ()
    type AttrTransferTypeConstraint PanedMaxPositionPropertyInfo = (~) ()
    type AttrTransferType PanedMaxPositionPropertyInfo = ()
    type AttrGetType PanedMaxPositionPropertyInfo = Int32
    type AttrLabel PanedMaxPositionPropertyInfo = "max-position"
    type AttrOrigin PanedMaxPositionPropertyInfo = Paned
    attrGet = getPanedMaxPosition
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Paned.maxPosition"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Paned.html#g:attr:maxPosition"
        })
#endif

-- VVV Prop "min-position"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@min-position@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' paned #minPosition
-- @
getPanedMinPosition :: (MonadIO m, IsPaned o) => o -> m Int32
getPanedMinPosition obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "min-position"

#if defined(ENABLE_OVERLOADING)
data PanedMinPositionPropertyInfo
instance AttrInfo PanedMinPositionPropertyInfo where
    type AttrAllowedOps PanedMinPositionPropertyInfo = '[ 'AttrGet]
    type AttrBaseTypeConstraint PanedMinPositionPropertyInfo = IsPaned
    type AttrSetTypeConstraint PanedMinPositionPropertyInfo = (~) ()
    type AttrTransferTypeConstraint PanedMinPositionPropertyInfo = (~) ()
    type AttrTransferType PanedMinPositionPropertyInfo = ()
    type AttrGetType PanedMinPositionPropertyInfo = Int32
    type AttrLabel PanedMinPositionPropertyInfo = "min-position"
    type AttrOrigin PanedMinPositionPropertyInfo = Paned
    attrGet = getPanedMinPosition
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Paned.minPosition"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Paned.html#g:attr:minPosition"
        })
#endif

-- VVV Prop "position"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@position@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' paned #position
-- @
getPanedPosition :: (MonadIO m, IsPaned o) => o -> m Int32
getPanedPosition obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "position"

-- | Set the value of the “@position@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' paned [ #position 'Data.GI.Base.Attributes.:=' value ]
-- @
setPanedPosition :: (MonadIO m, IsPaned o) => o -> Int32 -> m ()
setPanedPosition obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "position" val

-- | Construct a `GValueConstruct` with valid value for the “@position@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructPanedPosition :: (IsPaned o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructPanedPosition val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "position" val

#if defined(ENABLE_OVERLOADING)
data PanedPositionPropertyInfo
instance AttrInfo PanedPositionPropertyInfo where
    type AttrAllowedOps PanedPositionPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint PanedPositionPropertyInfo = IsPaned
    type AttrSetTypeConstraint PanedPositionPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint PanedPositionPropertyInfo = (~) Int32
    type AttrTransferType PanedPositionPropertyInfo = Int32
    type AttrGetType PanedPositionPropertyInfo = Int32
    type AttrLabel PanedPositionPropertyInfo = "position"
    type AttrOrigin PanedPositionPropertyInfo = Paned
    attrGet = getPanedPosition
    attrSet = setPanedPosition
    attrTransfer _ v = do
        return v
    attrConstruct = constructPanedPosition
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Paned.position"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Paned.html#g:attr:position"
        })
#endif

-- VVV Prop "position-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@position-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' paned #positionSet
-- @
getPanedPositionSet :: (MonadIO m, IsPaned o) => o -> m Bool
getPanedPositionSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "position-set"

-- | Set the value of the “@position-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' paned [ #positionSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setPanedPositionSet :: (MonadIO m, IsPaned o) => o -> Bool -> m ()
setPanedPositionSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "position-set" val

-- | Construct a `GValueConstruct` with valid value for the “@position-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructPanedPositionSet :: (IsPaned o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructPanedPositionSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "position-set" val

#if defined(ENABLE_OVERLOADING)
data PanedPositionSetPropertyInfo
instance AttrInfo PanedPositionSetPropertyInfo where
    type AttrAllowedOps PanedPositionSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint PanedPositionSetPropertyInfo = IsPaned
    type AttrSetTypeConstraint PanedPositionSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint PanedPositionSetPropertyInfo = (~) Bool
    type AttrTransferType PanedPositionSetPropertyInfo = Bool
    type AttrGetType PanedPositionSetPropertyInfo = Bool
    type AttrLabel PanedPositionSetPropertyInfo = "position-set"
    type AttrOrigin PanedPositionSetPropertyInfo = Paned
    attrGet = getPanedPositionSet
    attrSet = setPanedPositionSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructPanedPositionSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Paned.positionSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Paned.html#g:attr:positionSet"
        })
#endif

-- VVV Prop "wide-handle"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@wide-handle@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' paned #wideHandle
-- @
getPanedWideHandle :: (MonadIO m, IsPaned o) => o -> m Bool
getPanedWideHandle obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "wide-handle"

-- | Set the value of the “@wide-handle@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' paned [ #wideHandle 'Data.GI.Base.Attributes.:=' value ]
-- @
setPanedWideHandle :: (MonadIO m, IsPaned o) => o -> Bool -> m ()
setPanedWideHandle obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "wide-handle" val

-- | Construct a `GValueConstruct` with valid value for the “@wide-handle@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructPanedWideHandle :: (IsPaned o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructPanedWideHandle val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "wide-handle" val

#if defined(ENABLE_OVERLOADING)
data PanedWideHandlePropertyInfo
instance AttrInfo PanedWideHandlePropertyInfo where
    type AttrAllowedOps PanedWideHandlePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint PanedWideHandlePropertyInfo = IsPaned
    type AttrSetTypeConstraint PanedWideHandlePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint PanedWideHandlePropertyInfo = (~) Bool
    type AttrTransferType PanedWideHandlePropertyInfo = Bool
    type AttrGetType PanedWideHandlePropertyInfo = Bool
    type AttrLabel PanedWideHandlePropertyInfo = "wide-handle"
    type AttrOrigin PanedWideHandlePropertyInfo = Paned
    attrGet = getPanedWideHandle
    attrSet = setPanedWideHandle
    attrTransfer _ v = do
        return v
    attrConstruct = constructPanedWideHandle
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Paned.wideHandle"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Paned.html#g:attr:wideHandle"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList Paned
type instance O.AttributeList Paned = PanedAttributeList
type PanedAttributeList = ('[ '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("maxPosition", PanedMaxPositionPropertyInfo), '("minPosition", PanedMinPositionPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("orientation", Gtk.Orientable.OrientableOrientationPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("position", PanedPositionPropertyInfo), '("positionSet", PanedPositionSetPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("wideHandle", PanedWideHandlePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
panedMaxPosition :: AttrLabelProxy "maxPosition"
panedMaxPosition = AttrLabelProxy

panedMinPosition :: AttrLabelProxy "minPosition"
panedMinPosition = AttrLabelProxy

panedPosition :: AttrLabelProxy "position"
panedPosition = AttrLabelProxy

panedPositionSet :: AttrLabelProxy "positionSet"
panedPositionSet = AttrLabelProxy

panedWideHandle :: AttrLabelProxy "wideHandle"
panedWideHandle = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList Paned = PanedSignalList
type PanedSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("acceptPosition", PanedAcceptPositionSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("cancelPosition", PanedCancelPositionSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("cycleChildFocus", PanedCycleChildFocusSignalInfo), '("cycleHandleFocus", PanedCycleHandleFocusSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("moveHandle", PanedMoveHandleSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("toggleHandleFocus", PanedToggleHandleFocusSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method Paned::new
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "orientation"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Orientation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the paned\8217s orientation."
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Paned" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_paned_new" gtk_paned_new :: 
    CUInt ->                                -- orientation : TInterface (Name {namespace = "Gtk", name = "Orientation"})
    IO (Ptr Paned)

-- | Creates a new t'GI.Gtk.Objects.Paned.Paned' widget.
-- 
-- /Since: 3.0/
panedNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Gtk.Enums.Orientation
    -- ^ /@orientation@/: the paned’s orientation.
    -> m Paned
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.Paned.Paned'.
panedNew orientation = liftIO $ do
    let orientation' = (fromIntegral . fromEnum) orientation
    result <- gtk_paned_new orientation'
    checkUnexpectedReturnNULL "panedNew" result
    result' <- (newObject Paned) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Paned::add1
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "paned"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Paned" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a paned widget" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the child to add" , sinceVersion = Nothing }
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

foreign import ccall "gtk_paned_add1" gtk_paned_add1 :: 
    Ptr Paned ->                            -- paned : TInterface (Name {namespace = "Gtk", name = "Paned"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | Adds a child to the top or left pane with default parameters. This is
-- equivalent to
-- @gtk_paned_pack1 (paned, child, FALSE, TRUE)@.
panedAdd1 ::
    (B.CallStack.HasCallStack, MonadIO m, IsPaned a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@paned@/: a paned widget
    -> b
    -- ^ /@child@/: the child to add
    -> m ()
panedAdd1 paned child = liftIO $ do
    paned' <- unsafeManagedPtrCastPtr paned
    child' <- unsafeManagedPtrCastPtr child
    gtk_paned_add1 paned' child'
    touchManagedPtr paned
    touchManagedPtr child
    return ()

#if defined(ENABLE_OVERLOADING)
data PanedAdd1MethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsPaned a, Gtk.Widget.IsWidget b) => O.OverloadedMethod PanedAdd1MethodInfo a signature where
    overloadedMethod = panedAdd1

instance O.OverloadedMethodInfo PanedAdd1MethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Paned.panedAdd1",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Paned.html#v:panedAdd1"
        })


#endif

-- method Paned::add2
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "paned"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Paned" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a paned widget" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the child to add" , sinceVersion = Nothing }
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

foreign import ccall "gtk_paned_add2" gtk_paned_add2 :: 
    Ptr Paned ->                            -- paned : TInterface (Name {namespace = "Gtk", name = "Paned"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | Adds a child to the bottom or right pane with default parameters. This
-- is equivalent to
-- @gtk_paned_pack2 (paned, child, TRUE, TRUE)@.
panedAdd2 ::
    (B.CallStack.HasCallStack, MonadIO m, IsPaned a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@paned@/: a paned widget
    -> b
    -- ^ /@child@/: the child to add
    -> m ()
panedAdd2 paned child = liftIO $ do
    paned' <- unsafeManagedPtrCastPtr paned
    child' <- unsafeManagedPtrCastPtr child
    gtk_paned_add2 paned' child'
    touchManagedPtr paned
    touchManagedPtr child
    return ()

#if defined(ENABLE_OVERLOADING)
data PanedAdd2MethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsPaned a, Gtk.Widget.IsWidget b) => O.OverloadedMethod PanedAdd2MethodInfo a signature where
    overloadedMethod = panedAdd2

instance O.OverloadedMethodInfo PanedAdd2MethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Paned.panedAdd2",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Paned.html#v:panedAdd2"
        })


#endif

-- method Paned::get_child1
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "paned"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Paned" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPaned widget" , sinceVersion = Nothing }
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

foreign import ccall "gtk_paned_get_child1" gtk_paned_get_child1 :: 
    Ptr Paned ->                            -- paned : TInterface (Name {namespace = "Gtk", name = "Paned"})
    IO (Ptr Gtk.Widget.Widget)

-- | Obtains the first child of the paned widget.
-- 
-- /Since: 2.4/
panedGetChild1 ::
    (B.CallStack.HasCallStack, MonadIO m, IsPaned a) =>
    a
    -- ^ /@paned@/: a t'GI.Gtk.Objects.Paned.Paned' widget
    -> m (Maybe Gtk.Widget.Widget)
    -- ^ __Returns:__ first child, or 'P.Nothing' if it is not set.
panedGetChild1 paned = liftIO $ do
    paned' <- unsafeManagedPtrCastPtr paned
    result <- gtk_paned_get_child1 paned'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Widget.Widget) result'
        return result''
    touchManagedPtr paned
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data PanedGetChild1MethodInfo
instance (signature ~ (m (Maybe Gtk.Widget.Widget)), MonadIO m, IsPaned a) => O.OverloadedMethod PanedGetChild1MethodInfo a signature where
    overloadedMethod = panedGetChild1

instance O.OverloadedMethodInfo PanedGetChild1MethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Paned.panedGetChild1",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Paned.html#v:panedGetChild1"
        })


#endif

-- method Paned::get_child2
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "paned"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Paned" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPaned widget" , sinceVersion = Nothing }
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

foreign import ccall "gtk_paned_get_child2" gtk_paned_get_child2 :: 
    Ptr Paned ->                            -- paned : TInterface (Name {namespace = "Gtk", name = "Paned"})
    IO (Ptr Gtk.Widget.Widget)

-- | Obtains the second child of the paned widget.
-- 
-- /Since: 2.4/
panedGetChild2 ::
    (B.CallStack.HasCallStack, MonadIO m, IsPaned a) =>
    a
    -- ^ /@paned@/: a t'GI.Gtk.Objects.Paned.Paned' widget
    -> m (Maybe Gtk.Widget.Widget)
    -- ^ __Returns:__ second child, or 'P.Nothing' if it is not set.
panedGetChild2 paned = liftIO $ do
    paned' <- unsafeManagedPtrCastPtr paned
    result <- gtk_paned_get_child2 paned'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Widget.Widget) result'
        return result''
    touchManagedPtr paned
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data PanedGetChild2MethodInfo
instance (signature ~ (m (Maybe Gtk.Widget.Widget)), MonadIO m, IsPaned a) => O.OverloadedMethod PanedGetChild2MethodInfo a signature where
    overloadedMethod = panedGetChild2

instance O.OverloadedMethodInfo PanedGetChild2MethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Paned.panedGetChild2",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Paned.html#v:panedGetChild2"
        })


#endif

-- method Paned::get_handle_window
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "paned"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Paned" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPaned" , sinceVersion = Nothing }
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

foreign import ccall "gtk_paned_get_handle_window" gtk_paned_get_handle_window :: 
    Ptr Paned ->                            -- paned : TInterface (Name {namespace = "Gtk", name = "Paned"})
    IO (Ptr Gdk.Window.Window)

-- | Returns the t'GI.Gdk.Objects.Window.Window' of the handle. This function is
-- useful when handling button or motion events because it
-- enables the callback to distinguish between the window
-- of the paned, a child and the handle.
-- 
-- /Since: 2.20/
panedGetHandleWindow ::
    (B.CallStack.HasCallStack, MonadIO m, IsPaned a) =>
    a
    -- ^ /@paned@/: a t'GI.Gtk.Objects.Paned.Paned'
    -> m Gdk.Window.Window
    -- ^ __Returns:__ the paned’s handle window.
panedGetHandleWindow paned = liftIO $ do
    paned' <- unsafeManagedPtrCastPtr paned
    result <- gtk_paned_get_handle_window paned'
    checkUnexpectedReturnNULL "panedGetHandleWindow" result
    result' <- (newObject Gdk.Window.Window) result
    touchManagedPtr paned
    return result'

#if defined(ENABLE_OVERLOADING)
data PanedGetHandleWindowMethodInfo
instance (signature ~ (m Gdk.Window.Window), MonadIO m, IsPaned a) => O.OverloadedMethod PanedGetHandleWindowMethodInfo a signature where
    overloadedMethod = panedGetHandleWindow

instance O.OverloadedMethodInfo PanedGetHandleWindowMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Paned.panedGetHandleWindow",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Paned.html#v:panedGetHandleWindow"
        })


#endif

-- method Paned::get_position
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "paned"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Paned" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPaned widget" , sinceVersion = Nothing }
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

foreign import ccall "gtk_paned_get_position" gtk_paned_get_position :: 
    Ptr Paned ->                            -- paned : TInterface (Name {namespace = "Gtk", name = "Paned"})
    IO Int32

-- | Obtains the position of the divider between the two panes.
panedGetPosition ::
    (B.CallStack.HasCallStack, MonadIO m, IsPaned a) =>
    a
    -- ^ /@paned@/: a t'GI.Gtk.Objects.Paned.Paned' widget
    -> m Int32
    -- ^ __Returns:__ position of the divider
panedGetPosition paned = liftIO $ do
    paned' <- unsafeManagedPtrCastPtr paned
    result <- gtk_paned_get_position paned'
    touchManagedPtr paned
    return result

#if defined(ENABLE_OVERLOADING)
data PanedGetPositionMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsPaned a) => O.OverloadedMethod PanedGetPositionMethodInfo a signature where
    overloadedMethod = panedGetPosition

instance O.OverloadedMethodInfo PanedGetPositionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Paned.panedGetPosition",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Paned.html#v:panedGetPosition"
        })


#endif

-- method Paned::get_wide_handle
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "paned"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Paned" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPaned" , sinceVersion = Nothing }
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

foreign import ccall "gtk_paned_get_wide_handle" gtk_paned_get_wide_handle :: 
    Ptr Paned ->                            -- paned : TInterface (Name {namespace = "Gtk", name = "Paned"})
    IO CInt

-- | Gets the [Paned:wideHandle]("GI.Gtk.Objects.Paned#g:attr:wideHandle") property.
-- 
-- /Since: 3.16/
panedGetWideHandle ::
    (B.CallStack.HasCallStack, MonadIO m, IsPaned a) =>
    a
    -- ^ /@paned@/: a t'GI.Gtk.Objects.Paned.Paned'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the paned should have a wide handle
panedGetWideHandle paned = liftIO $ do
    paned' <- unsafeManagedPtrCastPtr paned
    result <- gtk_paned_get_wide_handle paned'
    let result' = (/= 0) result
    touchManagedPtr paned
    return result'

#if defined(ENABLE_OVERLOADING)
data PanedGetWideHandleMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsPaned a) => O.OverloadedMethod PanedGetWideHandleMethodInfo a signature where
    overloadedMethod = panedGetWideHandle

instance O.OverloadedMethodInfo PanedGetWideHandleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Paned.panedGetWideHandle",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Paned.html#v:panedGetWideHandle"
        })


#endif

-- method Paned::pack1
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "paned"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Paned" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a paned widget" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the child to add" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "resize"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "should this child expand when the paned widget is resized."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "shrink"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "can this child be made smaller than its requisition."
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

foreign import ccall "gtk_paned_pack1" gtk_paned_pack1 :: 
    Ptr Paned ->                            -- paned : TInterface (Name {namespace = "Gtk", name = "Paned"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CInt ->                                 -- resize : TBasicType TBoolean
    CInt ->                                 -- shrink : TBasicType TBoolean
    IO ()

-- | Adds a child to the top or left pane.
panedPack1 ::
    (B.CallStack.HasCallStack, MonadIO m, IsPaned a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@paned@/: a paned widget
    -> b
    -- ^ /@child@/: the child to add
    -> Bool
    -- ^ /@resize@/: should this child expand when the paned widget is resized.
    -> Bool
    -- ^ /@shrink@/: can this child be made smaller than its requisition.
    -> m ()
panedPack1 paned child resize shrink = liftIO $ do
    paned' <- unsafeManagedPtrCastPtr paned
    child' <- unsafeManagedPtrCastPtr child
    let resize' = (fromIntegral . fromEnum) resize
    let shrink' = (fromIntegral . fromEnum) shrink
    gtk_paned_pack1 paned' child' resize' shrink'
    touchManagedPtr paned
    touchManagedPtr child
    return ()

#if defined(ENABLE_OVERLOADING)
data PanedPack1MethodInfo
instance (signature ~ (b -> Bool -> Bool -> m ()), MonadIO m, IsPaned a, Gtk.Widget.IsWidget b) => O.OverloadedMethod PanedPack1MethodInfo a signature where
    overloadedMethod = panedPack1

instance O.OverloadedMethodInfo PanedPack1MethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Paned.panedPack1",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Paned.html#v:panedPack1"
        })


#endif

-- method Paned::pack2
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "paned"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Paned" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a paned widget" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the child to add" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "resize"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "should this child expand when the paned widget is resized."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "shrink"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "can this child be made smaller than its requisition."
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

foreign import ccall "gtk_paned_pack2" gtk_paned_pack2 :: 
    Ptr Paned ->                            -- paned : TInterface (Name {namespace = "Gtk", name = "Paned"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CInt ->                                 -- resize : TBasicType TBoolean
    CInt ->                                 -- shrink : TBasicType TBoolean
    IO ()

-- | Adds a child to the bottom or right pane.
panedPack2 ::
    (B.CallStack.HasCallStack, MonadIO m, IsPaned a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@paned@/: a paned widget
    -> b
    -- ^ /@child@/: the child to add
    -> Bool
    -- ^ /@resize@/: should this child expand when the paned widget is resized.
    -> Bool
    -- ^ /@shrink@/: can this child be made smaller than its requisition.
    -> m ()
panedPack2 paned child resize shrink = liftIO $ do
    paned' <- unsafeManagedPtrCastPtr paned
    child' <- unsafeManagedPtrCastPtr child
    let resize' = (fromIntegral . fromEnum) resize
    let shrink' = (fromIntegral . fromEnum) shrink
    gtk_paned_pack2 paned' child' resize' shrink'
    touchManagedPtr paned
    touchManagedPtr child
    return ()

#if defined(ENABLE_OVERLOADING)
data PanedPack2MethodInfo
instance (signature ~ (b -> Bool -> Bool -> m ()), MonadIO m, IsPaned a, Gtk.Widget.IsWidget b) => O.OverloadedMethod PanedPack2MethodInfo a signature where
    overloadedMethod = panedPack2

instance O.OverloadedMethodInfo PanedPack2MethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Paned.panedPack2",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Paned.html#v:panedPack2"
        })


#endif

-- method Paned::set_position
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "paned"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Paned" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPaned widget" , sinceVersion = Nothing }
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
--                       "pixel position of divider, a negative value means that the position\n           is unset."
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

foreign import ccall "gtk_paned_set_position" gtk_paned_set_position :: 
    Ptr Paned ->                            -- paned : TInterface (Name {namespace = "Gtk", name = "Paned"})
    Int32 ->                                -- position : TBasicType TInt
    IO ()

-- | Sets the position of the divider between the two panes.
panedSetPosition ::
    (B.CallStack.HasCallStack, MonadIO m, IsPaned a) =>
    a
    -- ^ /@paned@/: a t'GI.Gtk.Objects.Paned.Paned' widget
    -> Int32
    -- ^ /@position@/: pixel position of divider, a negative value means that the position
    --            is unset.
    -> m ()
panedSetPosition paned position = liftIO $ do
    paned' <- unsafeManagedPtrCastPtr paned
    gtk_paned_set_position paned' position
    touchManagedPtr paned
    return ()

#if defined(ENABLE_OVERLOADING)
data PanedSetPositionMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsPaned a) => O.OverloadedMethod PanedSetPositionMethodInfo a signature where
    overloadedMethod = panedSetPosition

instance O.OverloadedMethodInfo PanedSetPositionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Paned.panedSetPosition",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Paned.html#v:panedSetPosition"
        })


#endif

-- method Paned::set_wide_handle
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "paned"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Paned" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPaned" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "wide"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the new value for the #GtkPaned:wide-handle property"
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

foreign import ccall "gtk_paned_set_wide_handle" gtk_paned_set_wide_handle :: 
    Ptr Paned ->                            -- paned : TInterface (Name {namespace = "Gtk", name = "Paned"})
    CInt ->                                 -- wide : TBasicType TBoolean
    IO ()

-- | Sets the [Paned:wideHandle]("GI.Gtk.Objects.Paned#g:attr:wideHandle") property.
-- 
-- /Since: 3.16/
panedSetWideHandle ::
    (B.CallStack.HasCallStack, MonadIO m, IsPaned a) =>
    a
    -- ^ /@paned@/: a t'GI.Gtk.Objects.Paned.Paned'
    -> Bool
    -- ^ /@wide@/: the new value for the [Paned:wideHandle]("GI.Gtk.Objects.Paned#g:attr:wideHandle") property
    -> m ()
panedSetWideHandle paned wide = liftIO $ do
    paned' <- unsafeManagedPtrCastPtr paned
    let wide' = (fromIntegral . fromEnum) wide
    gtk_paned_set_wide_handle paned' wide'
    touchManagedPtr paned
    return ()

#if defined(ENABLE_OVERLOADING)
data PanedSetWideHandleMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsPaned a) => O.OverloadedMethod PanedSetWideHandleMethodInfo a signature where
    overloadedMethod = panedSetWideHandle

instance O.OverloadedMethodInfo PanedSetWideHandleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Paned.panedSetWideHandle",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Paned.html#v:panedSetWideHandle"
        })


#endif


