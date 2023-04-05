{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- The t'GI.Gtk.Objects.MenuItem.MenuItem' widget and the derived widgets are the only valid
-- children for menus. Their function is to correctly handle highlighting,
-- alignment, events and submenus.
-- 
-- As a GtkMenuItem derives from t'GI.Gtk.Objects.Bin.Bin' it can hold any valid child widget,
-- although only a few are really useful.
-- 
-- By default, a GtkMenuItem sets a t'GI.Gtk.Objects.AccelLabel.AccelLabel' as its child.
-- GtkMenuItem has direct functions to set the label and its mnemonic.
-- For more advanced label settings, you can fetch the child widget from the GtkBin.
-- 
-- An example for setting markup and accelerator on a MenuItem:
-- 
-- 
-- === /C code/
-- >
-- >GtkWidget *menu_item = gtk_menu_item_new_with_label ("Example Menu Item");
-- >
-- >GtkWidget *child = gtk_bin_get_child (GTK_BIN (menu_item));
-- >gtk_label_set_markup (GTK_LABEL (child), "<i>new label</i> with <b>markup</b>");
-- >gtk_accel_label_set_accel (GTK_ACCEL_LABEL (child), GDK_KEY_1, 0);
-- 
-- 
-- = GtkMenuItem as GtkBuildable
-- 
-- The GtkMenuItem implementation of the t'GI.Gtk.Interfaces.Buildable.Buildable' interface supports
-- adding a submenu by specifying “submenu” as the “type” attribute of
-- a @\<child>@ element.
-- 
-- An example of UI definition fragment with submenus:
-- 
-- 
-- === /xml code/
-- >
-- ><object class="GtkMenuItem">
-- >  <child type="submenu">
-- >    <object class="GtkMenu"/>
-- >  </child>
-- ></object>
-- 
-- 
-- = CSS nodes
-- 
-- 
-- === /plain code/
-- >
-- >menuitem
-- >├── <child>
-- >╰── [arrow.right]
-- 
-- 
-- GtkMenuItem has a single CSS node with name menuitem. If the menuitem
-- has a submenu, it gets another CSS node with name arrow, which has
-- the .left or .right style class.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.MenuItem
    ( 

-- * Exported types
    MenuItem(..)                            ,
    IsMenuItem                              ,
    toMenuItem                              ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.MenuItem#g:method:activate"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [deselect]("GI.Gtk.Objects.MenuItem#g:method:deselect"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [doSetRelatedAction]("GI.Gtk.Interfaces.Activatable#g:method:doSetRelatedAction"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [select]("GI.Gtk.Objects.MenuItem#g:method:select"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [syncActionProperties]("GI.Gtk.Interfaces.Activatable#g:method:syncActionProperties"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [toggleSizeAllocate]("GI.Gtk.Objects.MenuItem#g:method:toggleSizeAllocate"), [toggleSizeRequest]("GI.Gtk.Objects.MenuItem#g:method:toggleSizeRequest"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccelPath]("GI.Gtk.Objects.MenuItem#g:method:getAccelPath"), [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getActionName]("GI.Gtk.Interfaces.Actionable#g:method:getActionName"), [getActionTargetValue]("GI.Gtk.Interfaces.Actionable#g:method:getActionTargetValue"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChild]("GI.Gtk.Objects.Bin#g:method:getChild"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getLabel]("GI.Gtk.Objects.MenuItem#g:method:getLabel"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRelatedAction]("GI.Gtk.Interfaces.Activatable#g:method:getRelatedAction"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getReserveIndicator]("GI.Gtk.Objects.MenuItem#g:method:getReserveIndicator"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRightJustified]("GI.Gtk.Objects.MenuItem#g:method:getRightJustified"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSubmenu]("GI.Gtk.Objects.MenuItem#g:method:getSubmenu"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getUseActionAppearance]("GI.Gtk.Interfaces.Activatable#g:method:getUseActionAppearance"), [getUseUnderline]("GI.Gtk.Objects.MenuItem#g:method:getUseUnderline"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.MenuItem#g:method:setAccelPath"), [setActionName]("GI.Gtk.Interfaces.Actionable#g:method:setActionName"), [setActionTargetValue]("GI.Gtk.Interfaces.Actionable#g:method:setActionTargetValue"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDetailedActionName]("GI.Gtk.Interfaces.Actionable#g:method:setDetailedActionName"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setLabel]("GI.Gtk.Objects.MenuItem#g:method:setLabel"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setRelatedAction]("GI.Gtk.Interfaces.Activatable#g:method:setRelatedAction"), [setReserveIndicator]("GI.Gtk.Objects.MenuItem#g:method:setReserveIndicator"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setRightJustified]("GI.Gtk.Objects.MenuItem#g:method:setRightJustified"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSubmenu]("GI.Gtk.Objects.MenuItem#g:method:setSubmenu"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setUseActionAppearance]("GI.Gtk.Interfaces.Activatable#g:method:setUseActionAppearance"), [setUseUnderline]("GI.Gtk.Objects.MenuItem#g:method:setUseUnderline"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveMenuItemMethod                   ,
#endif

-- ** activate #method:activate#

#if defined(ENABLE_OVERLOADING)
    MenuItemActivateMethodInfo              ,
#endif
    menuItemActivate                        ,


-- ** deselect #method:deselect#

#if defined(ENABLE_OVERLOADING)
    MenuItemDeselectMethodInfo              ,
#endif
    menuItemDeselect                        ,


-- ** getAccelPath #method:getAccelPath#

#if defined(ENABLE_OVERLOADING)
    MenuItemGetAccelPathMethodInfo          ,
#endif
    menuItemGetAccelPath                    ,


-- ** getLabel #method:getLabel#

#if defined(ENABLE_OVERLOADING)
    MenuItemGetLabelMethodInfo              ,
#endif
    menuItemGetLabel                        ,


-- ** getReserveIndicator #method:getReserveIndicator#

#if defined(ENABLE_OVERLOADING)
    MenuItemGetReserveIndicatorMethodInfo   ,
#endif
    menuItemGetReserveIndicator             ,


-- ** getRightJustified #method:getRightJustified#

#if defined(ENABLE_OVERLOADING)
    MenuItemGetRightJustifiedMethodInfo     ,
#endif
    menuItemGetRightJustified               ,


-- ** getSubmenu #method:getSubmenu#

#if defined(ENABLE_OVERLOADING)
    MenuItemGetSubmenuMethodInfo            ,
#endif
    menuItemGetSubmenu                      ,


-- ** getUseUnderline #method:getUseUnderline#

#if defined(ENABLE_OVERLOADING)
    MenuItemGetUseUnderlineMethodInfo       ,
#endif
    menuItemGetUseUnderline                 ,


-- ** new #method:new#

    menuItemNew                             ,


-- ** newWithLabel #method:newWithLabel#

    menuItemNewWithLabel                    ,


-- ** newWithMnemonic #method:newWithMnemonic#

    menuItemNewWithMnemonic                 ,


-- ** select #method:select#

#if defined(ENABLE_OVERLOADING)
    MenuItemSelectMethodInfo                ,
#endif
    menuItemSelect                          ,


-- ** setAccelPath #method:setAccelPath#

#if defined(ENABLE_OVERLOADING)
    MenuItemSetAccelPathMethodInfo          ,
#endif
    menuItemSetAccelPath                    ,


-- ** setLabel #method:setLabel#

#if defined(ENABLE_OVERLOADING)
    MenuItemSetLabelMethodInfo              ,
#endif
    menuItemSetLabel                        ,


-- ** setReserveIndicator #method:setReserveIndicator#

#if defined(ENABLE_OVERLOADING)
    MenuItemSetReserveIndicatorMethodInfo   ,
#endif
    menuItemSetReserveIndicator             ,


-- ** setRightJustified #method:setRightJustified#

#if defined(ENABLE_OVERLOADING)
    MenuItemSetRightJustifiedMethodInfo     ,
#endif
    menuItemSetRightJustified               ,


-- ** setSubmenu #method:setSubmenu#

#if defined(ENABLE_OVERLOADING)
    MenuItemSetSubmenuMethodInfo            ,
#endif
    menuItemSetSubmenu                      ,


-- ** setUseUnderline #method:setUseUnderline#

#if defined(ENABLE_OVERLOADING)
    MenuItemSetUseUnderlineMethodInfo       ,
#endif
    menuItemSetUseUnderline                 ,


-- ** toggleSizeAllocate #method:toggleSizeAllocate#

#if defined(ENABLE_OVERLOADING)
    MenuItemToggleSizeAllocateMethodInfo    ,
#endif
    menuItemToggleSizeAllocate              ,


-- ** toggleSizeRequest #method:toggleSizeRequest#

#if defined(ENABLE_OVERLOADING)
    MenuItemToggleSizeRequestMethodInfo     ,
#endif
    menuItemToggleSizeRequest               ,




 -- * Properties


-- ** accelPath #attr:accelPath#
-- | Sets the accelerator path of the menu item, through which runtime
-- changes of the menu item\'s accelerator caused by the user can be
-- identified and saved to persistant storage.
-- 
-- /Since: 2.14/

#if defined(ENABLE_OVERLOADING)
    MenuItemAccelPathPropertyInfo           ,
#endif
    clearMenuItemAccelPath                  ,
    constructMenuItemAccelPath              ,
    getMenuItemAccelPath                    ,
#if defined(ENABLE_OVERLOADING)
    menuItemAccelPath                       ,
#endif
    setMenuItemAccelPath                    ,


-- ** label #attr:label#
-- | The text for the child label.
-- 
-- /Since: 2.16/

#if defined(ENABLE_OVERLOADING)
    MenuItemLabelPropertyInfo               ,
#endif
    constructMenuItemLabel                  ,
    getMenuItemLabel                        ,
#if defined(ENABLE_OVERLOADING)
    menuItemLabel                           ,
#endif
    setMenuItemLabel                        ,


-- ** rightJustified #attr:rightJustified#
-- | Sets whether the menu item appears justified
-- at the right side of a menu bar.
-- 
-- /Since: 2.14/

#if defined(ENABLE_OVERLOADING)
    MenuItemRightJustifiedPropertyInfo      ,
#endif
    constructMenuItemRightJustified         ,
    getMenuItemRightJustified               ,
#if defined(ENABLE_OVERLOADING)
    menuItemRightJustified                  ,
#endif
    setMenuItemRightJustified               ,


-- ** submenu #attr:submenu#
-- | The submenu attached to the menu item, or 'P.Nothing' if it has none.
-- 
-- /Since: 2.12/

#if defined(ENABLE_OVERLOADING)
    MenuItemSubmenuPropertyInfo             ,
#endif
    clearMenuItemSubmenu                    ,
    constructMenuItemSubmenu                ,
    getMenuItemSubmenu                      ,
#if defined(ENABLE_OVERLOADING)
    menuItemSubmenu                         ,
#endif
    setMenuItemSubmenu                      ,


-- ** useUnderline #attr:useUnderline#
-- | 'P.True' if underlines in the text indicate mnemonics.
-- 
-- /Since: 2.16/

#if defined(ENABLE_OVERLOADING)
    MenuItemUseUnderlinePropertyInfo        ,
#endif
    constructMenuItemUseUnderline           ,
    getMenuItemUseUnderline                 ,
#if defined(ENABLE_OVERLOADING)
    menuItemUseUnderline                    ,
#endif
    setMenuItemUseUnderline                 ,




 -- * Signals


-- ** activate #signal:activate#

    MenuItemActivateCallback                ,
#if defined(ENABLE_OVERLOADING)
    MenuItemActivateSignalInfo              ,
#endif
    afterMenuItemActivate                   ,
    onMenuItemActivate                      ,


-- ** activateItem #signal:activateItem#

    MenuItemActivateItemCallback            ,
#if defined(ENABLE_OVERLOADING)
    MenuItemActivateItemSignalInfo          ,
#endif
    afterMenuItemActivateItem               ,
    onMenuItemActivateItem                  ,


-- ** deselect #signal:deselect#

    MenuItemDeselectCallback                ,
#if defined(ENABLE_OVERLOADING)
    MenuItemDeselectSignalInfo              ,
#endif
    afterMenuItemDeselect                   ,
    onMenuItemDeselect                      ,


-- ** select #signal:select#

    MenuItemSelectCallback                  ,
#if defined(ENABLE_OVERLOADING)
    MenuItemSelectSignalInfo                ,
#endif
    afterMenuItemSelect                     ,
    onMenuItemSelect                        ,


-- ** toggleSizeAllocate #signal:toggleSizeAllocate#

    MenuItemToggleSizeAllocateCallback      ,
#if defined(ENABLE_OVERLOADING)
    MenuItemToggleSizeAllocateSignalInfo    ,
#endif
    afterMenuItemToggleSizeAllocate         ,
    onMenuItemToggleSizeAllocate            ,


-- ** toggleSizeRequest #signal:toggleSizeRequest#

    MenuItemToggleSizeRequestCallback       ,
#if defined(ENABLE_OVERLOADING)
    MenuItemToggleSizeRequestSignalInfo     ,
#endif
    afterMenuItemToggleSizeRequest          ,
    onMenuItemToggleSizeRequest             ,




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
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.Menu as Gtk.Menu
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype MenuItem = MenuItem (SP.ManagedPtr MenuItem)
    deriving (Eq)

instance SP.ManagedPtrNewtype MenuItem where
    toManagedPtr (MenuItem p) = p

foreign import ccall "gtk_menu_item_get_type"
    c_gtk_menu_item_get_type :: IO B.Types.GType

instance B.Types.TypedObject MenuItem where
    glibType = c_gtk_menu_item_get_type

instance B.Types.GObject MenuItem

-- | Type class for types which can be safely cast to `MenuItem`, for instance with `toMenuItem`.
class (SP.GObject o, O.IsDescendantOf MenuItem o) => IsMenuItem o
instance (SP.GObject o, O.IsDescendantOf MenuItem o) => IsMenuItem o

instance O.HasParentTypes MenuItem
type instance O.ParentTypes MenuItem = '[Gtk.Bin.Bin, Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Actionable.Actionable, Gtk.Activatable.Activatable, Gtk.Buildable.Buildable]

-- | Cast to `MenuItem`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toMenuItem :: (MIO.MonadIO m, IsMenuItem o) => o -> m MenuItem
toMenuItem = MIO.liftIO . B.ManagedPtr.unsafeCastTo MenuItem

-- | Convert 'MenuItem' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe MenuItem) where
    gvalueGType_ = c_gtk_menu_item_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr MenuItem)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr MenuItem)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject MenuItem ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveMenuItemMethod (t :: Symbol) (o :: *) :: * where
    ResolveMenuItemMethod "activate" o = MenuItemActivateMethodInfo
    ResolveMenuItemMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveMenuItemMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveMenuItemMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveMenuItemMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveMenuItemMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveMenuItemMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveMenuItemMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveMenuItemMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveMenuItemMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveMenuItemMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveMenuItemMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveMenuItemMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveMenuItemMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveMenuItemMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveMenuItemMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveMenuItemMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveMenuItemMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveMenuItemMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveMenuItemMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveMenuItemMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveMenuItemMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveMenuItemMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveMenuItemMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveMenuItemMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveMenuItemMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveMenuItemMethod "deselect" o = MenuItemDeselectMethodInfo
    ResolveMenuItemMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveMenuItemMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveMenuItemMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveMenuItemMethod "doSetRelatedAction" o = Gtk.Activatable.ActivatableDoSetRelatedActionMethodInfo
    ResolveMenuItemMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveMenuItemMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveMenuItemMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveMenuItemMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveMenuItemMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveMenuItemMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveMenuItemMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveMenuItemMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveMenuItemMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveMenuItemMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveMenuItemMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveMenuItemMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveMenuItemMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveMenuItemMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveMenuItemMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveMenuItemMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveMenuItemMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveMenuItemMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveMenuItemMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveMenuItemMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveMenuItemMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveMenuItemMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveMenuItemMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveMenuItemMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveMenuItemMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveMenuItemMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveMenuItemMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveMenuItemMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveMenuItemMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveMenuItemMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveMenuItemMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveMenuItemMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveMenuItemMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveMenuItemMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveMenuItemMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveMenuItemMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveMenuItemMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveMenuItemMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveMenuItemMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveMenuItemMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveMenuItemMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveMenuItemMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveMenuItemMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveMenuItemMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveMenuItemMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveMenuItemMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveMenuItemMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveMenuItemMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveMenuItemMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveMenuItemMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveMenuItemMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveMenuItemMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveMenuItemMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveMenuItemMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveMenuItemMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveMenuItemMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveMenuItemMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveMenuItemMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveMenuItemMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveMenuItemMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveMenuItemMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveMenuItemMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveMenuItemMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveMenuItemMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveMenuItemMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveMenuItemMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveMenuItemMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveMenuItemMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveMenuItemMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveMenuItemMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveMenuItemMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveMenuItemMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveMenuItemMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveMenuItemMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveMenuItemMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveMenuItemMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveMenuItemMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveMenuItemMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveMenuItemMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveMenuItemMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveMenuItemMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveMenuItemMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveMenuItemMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveMenuItemMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveMenuItemMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveMenuItemMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveMenuItemMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveMenuItemMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveMenuItemMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveMenuItemMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveMenuItemMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveMenuItemMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveMenuItemMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveMenuItemMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveMenuItemMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveMenuItemMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveMenuItemMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveMenuItemMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveMenuItemMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolveMenuItemMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveMenuItemMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveMenuItemMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveMenuItemMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveMenuItemMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveMenuItemMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveMenuItemMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveMenuItemMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveMenuItemMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveMenuItemMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveMenuItemMethod "select" o = MenuItemSelectMethodInfo
    ResolveMenuItemMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveMenuItemMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveMenuItemMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveMenuItemMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveMenuItemMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveMenuItemMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveMenuItemMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveMenuItemMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveMenuItemMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveMenuItemMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveMenuItemMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveMenuItemMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveMenuItemMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveMenuItemMethod "syncActionProperties" o = Gtk.Activatable.ActivatableSyncActionPropertiesMethodInfo
    ResolveMenuItemMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveMenuItemMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveMenuItemMethod "toggleSizeAllocate" o = MenuItemToggleSizeAllocateMethodInfo
    ResolveMenuItemMethod "toggleSizeRequest" o = MenuItemToggleSizeRequestMethodInfo
    ResolveMenuItemMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveMenuItemMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveMenuItemMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveMenuItemMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveMenuItemMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveMenuItemMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveMenuItemMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveMenuItemMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveMenuItemMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveMenuItemMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveMenuItemMethod "getAccelPath" o = MenuItemGetAccelPathMethodInfo
    ResolveMenuItemMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveMenuItemMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveMenuItemMethod "getActionName" o = Gtk.Actionable.ActionableGetActionNameMethodInfo
    ResolveMenuItemMethod "getActionTargetValue" o = Gtk.Actionable.ActionableGetActionTargetValueMethodInfo
    ResolveMenuItemMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveMenuItemMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveMenuItemMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveMenuItemMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveMenuItemMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveMenuItemMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveMenuItemMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveMenuItemMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveMenuItemMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveMenuItemMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveMenuItemMethod "getChild" o = Gtk.Bin.BinGetChildMethodInfo
    ResolveMenuItemMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveMenuItemMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveMenuItemMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveMenuItemMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveMenuItemMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveMenuItemMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveMenuItemMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveMenuItemMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveMenuItemMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveMenuItemMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveMenuItemMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveMenuItemMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveMenuItemMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveMenuItemMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveMenuItemMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveMenuItemMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveMenuItemMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveMenuItemMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveMenuItemMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveMenuItemMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveMenuItemMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveMenuItemMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveMenuItemMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveMenuItemMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveMenuItemMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveMenuItemMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveMenuItemMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveMenuItemMethod "getLabel" o = MenuItemGetLabelMethodInfo
    ResolveMenuItemMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveMenuItemMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveMenuItemMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveMenuItemMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveMenuItemMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveMenuItemMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveMenuItemMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveMenuItemMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveMenuItemMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveMenuItemMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveMenuItemMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveMenuItemMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveMenuItemMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveMenuItemMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveMenuItemMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveMenuItemMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveMenuItemMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveMenuItemMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveMenuItemMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveMenuItemMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveMenuItemMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveMenuItemMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveMenuItemMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveMenuItemMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveMenuItemMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveMenuItemMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveMenuItemMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveMenuItemMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveMenuItemMethod "getRelatedAction" o = Gtk.Activatable.ActivatableGetRelatedActionMethodInfo
    ResolveMenuItemMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveMenuItemMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveMenuItemMethod "getReserveIndicator" o = MenuItemGetReserveIndicatorMethodInfo
    ResolveMenuItemMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveMenuItemMethod "getRightJustified" o = MenuItemGetRightJustifiedMethodInfo
    ResolveMenuItemMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveMenuItemMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveMenuItemMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveMenuItemMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveMenuItemMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveMenuItemMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveMenuItemMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveMenuItemMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveMenuItemMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveMenuItemMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveMenuItemMethod "getSubmenu" o = MenuItemGetSubmenuMethodInfo
    ResolveMenuItemMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveMenuItemMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveMenuItemMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveMenuItemMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveMenuItemMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveMenuItemMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveMenuItemMethod "getUseActionAppearance" o = Gtk.Activatable.ActivatableGetUseActionAppearanceMethodInfo
    ResolveMenuItemMethod "getUseUnderline" o = MenuItemGetUseUnderlineMethodInfo
    ResolveMenuItemMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveMenuItemMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveMenuItemMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveMenuItemMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveMenuItemMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveMenuItemMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveMenuItemMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveMenuItemMethod "setAccelPath" o = MenuItemSetAccelPathMethodInfo
    ResolveMenuItemMethod "setActionName" o = Gtk.Actionable.ActionableSetActionNameMethodInfo
    ResolveMenuItemMethod "setActionTargetValue" o = Gtk.Actionable.ActionableSetActionTargetValueMethodInfo
    ResolveMenuItemMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveMenuItemMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveMenuItemMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveMenuItemMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveMenuItemMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveMenuItemMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveMenuItemMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveMenuItemMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveMenuItemMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveMenuItemMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveMenuItemMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveMenuItemMethod "setDetailedActionName" o = Gtk.Actionable.ActionableSetDetailedActionNameMethodInfo
    ResolveMenuItemMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveMenuItemMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveMenuItemMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveMenuItemMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveMenuItemMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveMenuItemMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveMenuItemMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveMenuItemMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveMenuItemMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveMenuItemMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveMenuItemMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveMenuItemMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveMenuItemMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveMenuItemMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveMenuItemMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveMenuItemMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveMenuItemMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveMenuItemMethod "setLabel" o = MenuItemSetLabelMethodInfo
    ResolveMenuItemMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveMenuItemMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveMenuItemMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveMenuItemMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveMenuItemMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveMenuItemMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveMenuItemMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveMenuItemMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveMenuItemMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveMenuItemMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveMenuItemMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveMenuItemMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveMenuItemMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveMenuItemMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveMenuItemMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveMenuItemMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveMenuItemMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveMenuItemMethod "setRelatedAction" o = Gtk.Activatable.ActivatableSetRelatedActionMethodInfo
    ResolveMenuItemMethod "setReserveIndicator" o = MenuItemSetReserveIndicatorMethodInfo
    ResolveMenuItemMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveMenuItemMethod "setRightJustified" o = MenuItemSetRightJustifiedMethodInfo
    ResolveMenuItemMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveMenuItemMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveMenuItemMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveMenuItemMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveMenuItemMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveMenuItemMethod "setSubmenu" o = MenuItemSetSubmenuMethodInfo
    ResolveMenuItemMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveMenuItemMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveMenuItemMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveMenuItemMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveMenuItemMethod "setUseActionAppearance" o = Gtk.Activatable.ActivatableSetUseActionAppearanceMethodInfo
    ResolveMenuItemMethod "setUseUnderline" o = MenuItemSetUseUnderlineMethodInfo
    ResolveMenuItemMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveMenuItemMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveMenuItemMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveMenuItemMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveMenuItemMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveMenuItemMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveMenuItemMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveMenuItemMethod t MenuItem, O.OverloadedMethod info MenuItem p) => OL.IsLabel t (MenuItem -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveMenuItemMethod t MenuItem, O.OverloadedMethod info MenuItem p, R.HasField t MenuItem p) => R.HasField t MenuItem p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveMenuItemMethod t MenuItem, O.OverloadedMethodInfo info MenuItem) => OL.IsLabel t (O.MethodProxy info MenuItem) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal MenuItem::activate
-- | Emitted when the item is activated.
type MenuItemActivateCallback =
    IO ()

type C_MenuItemActivateCallback =
    Ptr MenuItem ->                         -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_MenuItemActivateCallback`.
foreign import ccall "wrapper"
    mk_MenuItemActivateCallback :: C_MenuItemActivateCallback -> IO (FunPtr C_MenuItemActivateCallback)

wrap_MenuItemActivateCallback :: 
    GObject a => (a -> MenuItemActivateCallback) ->
    C_MenuItemActivateCallback
wrap_MenuItemActivateCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [activate](#signal:activate) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' menuItem #activate callback
-- @
-- 
-- 
onMenuItemActivate :: (IsMenuItem a, MonadIO m) => a -> ((?self :: a) => MenuItemActivateCallback) -> m SignalHandlerId
onMenuItemActivate obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_MenuItemActivateCallback wrapped
    wrapped'' <- mk_MenuItemActivateCallback wrapped'
    connectSignalFunPtr obj "activate" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [activate](#signal:activate) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' menuItem #activate callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterMenuItemActivate :: (IsMenuItem a, MonadIO m) => a -> ((?self :: a) => MenuItemActivateCallback) -> m SignalHandlerId
afterMenuItemActivate obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_MenuItemActivateCallback wrapped
    wrapped'' <- mk_MenuItemActivateCallback wrapped'
    connectSignalFunPtr obj "activate" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data MenuItemActivateSignalInfo
instance SignalInfo MenuItemActivateSignalInfo where
    type HaskellCallbackType MenuItemActivateSignalInfo = MenuItemActivateCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_MenuItemActivateCallback cb
        cb'' <- mk_MenuItemActivateCallback cb'
        connectSignalFunPtr obj "activate" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuItem::activate"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuItem.html#g:signal:activate"})

#endif

-- signal MenuItem::activate-item
-- | Emitted when the item is activated, but also if the menu item has a
-- submenu. For normal applications, the relevant signal is
-- [MenuItem::activate]("GI.Gtk.Objects.MenuItem#g:signal:activate").
type MenuItemActivateItemCallback =
    IO ()

type C_MenuItemActivateItemCallback =
    Ptr MenuItem ->                         -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_MenuItemActivateItemCallback`.
foreign import ccall "wrapper"
    mk_MenuItemActivateItemCallback :: C_MenuItemActivateItemCallback -> IO (FunPtr C_MenuItemActivateItemCallback)

wrap_MenuItemActivateItemCallback :: 
    GObject a => (a -> MenuItemActivateItemCallback) ->
    C_MenuItemActivateItemCallback
wrap_MenuItemActivateItemCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [activateItem](#signal:activateItem) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' menuItem #activateItem callback
-- @
-- 
-- 
onMenuItemActivateItem :: (IsMenuItem a, MonadIO m) => a -> ((?self :: a) => MenuItemActivateItemCallback) -> m SignalHandlerId
onMenuItemActivateItem obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_MenuItemActivateItemCallback wrapped
    wrapped'' <- mk_MenuItemActivateItemCallback wrapped'
    connectSignalFunPtr obj "activate-item" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [activateItem](#signal:activateItem) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' menuItem #activateItem callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterMenuItemActivateItem :: (IsMenuItem a, MonadIO m) => a -> ((?self :: a) => MenuItemActivateItemCallback) -> m SignalHandlerId
afterMenuItemActivateItem obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_MenuItemActivateItemCallback wrapped
    wrapped'' <- mk_MenuItemActivateItemCallback wrapped'
    connectSignalFunPtr obj "activate-item" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data MenuItemActivateItemSignalInfo
instance SignalInfo MenuItemActivateItemSignalInfo where
    type HaskellCallbackType MenuItemActivateItemSignalInfo = MenuItemActivateItemCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_MenuItemActivateItemCallback cb
        cb'' <- mk_MenuItemActivateItemCallback cb'
        connectSignalFunPtr obj "activate-item" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuItem::activate-item"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuItem.html#g:signal:activateItem"})

#endif

-- signal MenuItem::deselect
-- | /No description available in the introspection data./
type MenuItemDeselectCallback =
    IO ()

type C_MenuItemDeselectCallback =
    Ptr MenuItem ->                         -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_MenuItemDeselectCallback`.
foreign import ccall "wrapper"
    mk_MenuItemDeselectCallback :: C_MenuItemDeselectCallback -> IO (FunPtr C_MenuItemDeselectCallback)

wrap_MenuItemDeselectCallback :: 
    GObject a => (a -> MenuItemDeselectCallback) ->
    C_MenuItemDeselectCallback
wrap_MenuItemDeselectCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [deselect](#signal:deselect) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' menuItem #deselect callback
-- @
-- 
-- 
onMenuItemDeselect :: (IsMenuItem a, MonadIO m) => a -> ((?self :: a) => MenuItemDeselectCallback) -> m SignalHandlerId
onMenuItemDeselect obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_MenuItemDeselectCallback wrapped
    wrapped'' <- mk_MenuItemDeselectCallback wrapped'
    connectSignalFunPtr obj "deselect" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [deselect](#signal:deselect) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' menuItem #deselect callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterMenuItemDeselect :: (IsMenuItem a, MonadIO m) => a -> ((?self :: a) => MenuItemDeselectCallback) -> m SignalHandlerId
afterMenuItemDeselect obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_MenuItemDeselectCallback wrapped
    wrapped'' <- mk_MenuItemDeselectCallback wrapped'
    connectSignalFunPtr obj "deselect" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data MenuItemDeselectSignalInfo
instance SignalInfo MenuItemDeselectSignalInfo where
    type HaskellCallbackType MenuItemDeselectSignalInfo = MenuItemDeselectCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_MenuItemDeselectCallback cb
        cb'' <- mk_MenuItemDeselectCallback cb'
        connectSignalFunPtr obj "deselect" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuItem::deselect"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuItem.html#g:signal:deselect"})

#endif

-- signal MenuItem::select
-- | /No description available in the introspection data./
type MenuItemSelectCallback =
    IO ()

type C_MenuItemSelectCallback =
    Ptr MenuItem ->                         -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_MenuItemSelectCallback`.
foreign import ccall "wrapper"
    mk_MenuItemSelectCallback :: C_MenuItemSelectCallback -> IO (FunPtr C_MenuItemSelectCallback)

wrap_MenuItemSelectCallback :: 
    GObject a => (a -> MenuItemSelectCallback) ->
    C_MenuItemSelectCallback
wrap_MenuItemSelectCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [select](#signal:select) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' menuItem #select callback
-- @
-- 
-- 
onMenuItemSelect :: (IsMenuItem a, MonadIO m) => a -> ((?self :: a) => MenuItemSelectCallback) -> m SignalHandlerId
onMenuItemSelect obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_MenuItemSelectCallback wrapped
    wrapped'' <- mk_MenuItemSelectCallback wrapped'
    connectSignalFunPtr obj "select" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [select](#signal:select) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' menuItem #select callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterMenuItemSelect :: (IsMenuItem a, MonadIO m) => a -> ((?self :: a) => MenuItemSelectCallback) -> m SignalHandlerId
afterMenuItemSelect obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_MenuItemSelectCallback wrapped
    wrapped'' <- mk_MenuItemSelectCallback wrapped'
    connectSignalFunPtr obj "select" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data MenuItemSelectSignalInfo
instance SignalInfo MenuItemSelectSignalInfo where
    type HaskellCallbackType MenuItemSelectSignalInfo = MenuItemSelectCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_MenuItemSelectCallback cb
        cb'' <- mk_MenuItemSelectCallback cb'
        connectSignalFunPtr obj "select" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuItem::select"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuItem.html#g:signal:select"})

#endif

-- signal MenuItem::toggle-size-allocate
-- | /No description available in the introspection data./
type MenuItemToggleSizeAllocateCallback =
    Int32
    -> IO ()

type C_MenuItemToggleSizeAllocateCallback =
    Ptr MenuItem ->                         -- object
    Int32 ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_MenuItemToggleSizeAllocateCallback`.
foreign import ccall "wrapper"
    mk_MenuItemToggleSizeAllocateCallback :: C_MenuItemToggleSizeAllocateCallback -> IO (FunPtr C_MenuItemToggleSizeAllocateCallback)

wrap_MenuItemToggleSizeAllocateCallback :: 
    GObject a => (a -> MenuItemToggleSizeAllocateCallback) ->
    C_MenuItemToggleSizeAllocateCallback
wrap_MenuItemToggleSizeAllocateCallback gi'cb gi'selfPtr object _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  object


-- | Connect a signal handler for the [toggleSizeAllocate](#signal:toggleSizeAllocate) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' menuItem #toggleSizeAllocate callback
-- @
-- 
-- 
onMenuItemToggleSizeAllocate :: (IsMenuItem a, MonadIO m) => a -> ((?self :: a) => MenuItemToggleSizeAllocateCallback) -> m SignalHandlerId
onMenuItemToggleSizeAllocate obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_MenuItemToggleSizeAllocateCallback wrapped
    wrapped'' <- mk_MenuItemToggleSizeAllocateCallback wrapped'
    connectSignalFunPtr obj "toggle-size-allocate" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [toggleSizeAllocate](#signal:toggleSizeAllocate) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' menuItem #toggleSizeAllocate callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterMenuItemToggleSizeAllocate :: (IsMenuItem a, MonadIO m) => a -> ((?self :: a) => MenuItemToggleSizeAllocateCallback) -> m SignalHandlerId
afterMenuItemToggleSizeAllocate obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_MenuItemToggleSizeAllocateCallback wrapped
    wrapped'' <- mk_MenuItemToggleSizeAllocateCallback wrapped'
    connectSignalFunPtr obj "toggle-size-allocate" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data MenuItemToggleSizeAllocateSignalInfo
instance SignalInfo MenuItemToggleSizeAllocateSignalInfo where
    type HaskellCallbackType MenuItemToggleSizeAllocateSignalInfo = MenuItemToggleSizeAllocateCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_MenuItemToggleSizeAllocateCallback cb
        cb'' <- mk_MenuItemToggleSizeAllocateCallback cb'
        connectSignalFunPtr obj "toggle-size-allocate" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuItem::toggle-size-allocate"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuItem.html#g:signal:toggleSizeAllocate"})

#endif

-- signal MenuItem::toggle-size-request
-- | /No description available in the introspection data./
type MenuItemToggleSizeRequestCallback =
    Ptr ()
    -> IO ()

type C_MenuItemToggleSizeRequestCallback =
    Ptr MenuItem ->                         -- object
    Ptr () ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_MenuItemToggleSizeRequestCallback`.
foreign import ccall "wrapper"
    mk_MenuItemToggleSizeRequestCallback :: C_MenuItemToggleSizeRequestCallback -> IO (FunPtr C_MenuItemToggleSizeRequestCallback)

wrap_MenuItemToggleSizeRequestCallback :: 
    GObject a => (a -> MenuItemToggleSizeRequestCallback) ->
    C_MenuItemToggleSizeRequestCallback
wrap_MenuItemToggleSizeRequestCallback gi'cb gi'selfPtr object _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  object


-- | Connect a signal handler for the [toggleSizeRequest](#signal:toggleSizeRequest) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' menuItem #toggleSizeRequest callback
-- @
-- 
-- 
onMenuItemToggleSizeRequest :: (IsMenuItem a, MonadIO m) => a -> ((?self :: a) => MenuItemToggleSizeRequestCallback) -> m SignalHandlerId
onMenuItemToggleSizeRequest obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_MenuItemToggleSizeRequestCallback wrapped
    wrapped'' <- mk_MenuItemToggleSizeRequestCallback wrapped'
    connectSignalFunPtr obj "toggle-size-request" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [toggleSizeRequest](#signal:toggleSizeRequest) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' menuItem #toggleSizeRequest callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterMenuItemToggleSizeRequest :: (IsMenuItem a, MonadIO m) => a -> ((?self :: a) => MenuItemToggleSizeRequestCallback) -> m SignalHandlerId
afterMenuItemToggleSizeRequest obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_MenuItemToggleSizeRequestCallback wrapped
    wrapped'' <- mk_MenuItemToggleSizeRequestCallback wrapped'
    connectSignalFunPtr obj "toggle-size-request" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data MenuItemToggleSizeRequestSignalInfo
instance SignalInfo MenuItemToggleSizeRequestSignalInfo where
    type HaskellCallbackType MenuItemToggleSizeRequestSignalInfo = MenuItemToggleSizeRequestCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_MenuItemToggleSizeRequestCallback cb
        cb'' <- mk_MenuItemToggleSizeRequestCallback cb'
        connectSignalFunPtr obj "toggle-size-request" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuItem::toggle-size-request"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuItem.html#g:signal:toggleSizeRequest"})

#endif

-- VVV Prop "accel-path"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just True)

-- | Get the value of the “@accel-path@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' menuItem #accelPath
-- @
getMenuItemAccelPath :: (MonadIO m, IsMenuItem o) => o -> m (Maybe T.Text)
getMenuItemAccelPath obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "accel-path"

-- | Set the value of the “@accel-path@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' menuItem [ #accelPath 'Data.GI.Base.Attributes.:=' value ]
-- @
setMenuItemAccelPath :: (MonadIO m, IsMenuItem o) => o -> T.Text -> m ()
setMenuItemAccelPath obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "accel-path" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@accel-path@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructMenuItemAccelPath :: (IsMenuItem o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructMenuItemAccelPath val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "accel-path" (P.Just val)

-- | Set the value of the “@accel-path@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #accelPath
-- @
clearMenuItemAccelPath :: (MonadIO m, IsMenuItem o) => o -> m ()
clearMenuItemAccelPath obj = liftIO $ B.Properties.setObjectPropertyString obj "accel-path" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data MenuItemAccelPathPropertyInfo
instance AttrInfo MenuItemAccelPathPropertyInfo where
    type AttrAllowedOps MenuItemAccelPathPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint MenuItemAccelPathPropertyInfo = IsMenuItem
    type AttrSetTypeConstraint MenuItemAccelPathPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint MenuItemAccelPathPropertyInfo = (~) T.Text
    type AttrTransferType MenuItemAccelPathPropertyInfo = T.Text
    type AttrGetType MenuItemAccelPathPropertyInfo = (Maybe T.Text)
    type AttrLabel MenuItemAccelPathPropertyInfo = "accel-path"
    type AttrOrigin MenuItemAccelPathPropertyInfo = MenuItem
    attrGet = getMenuItemAccelPath
    attrSet = setMenuItemAccelPath
    attrTransfer _ v = do
        return v
    attrConstruct = constructMenuItemAccelPath
    attrClear = clearMenuItemAccelPath
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuItem.accelPath"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuItem.html#g:attr:accelPath"
        })
#endif

-- VVV Prop "label"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@label@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' menuItem #label
-- @
getMenuItemLabel :: (MonadIO m, IsMenuItem o) => o -> m T.Text
getMenuItemLabel obj = MIO.liftIO $ checkUnexpectedNothing "getMenuItemLabel" $ B.Properties.getObjectPropertyString obj "label"

-- | Set the value of the “@label@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' menuItem [ #label 'Data.GI.Base.Attributes.:=' value ]
-- @
setMenuItemLabel :: (MonadIO m, IsMenuItem o) => o -> T.Text -> m ()
setMenuItemLabel obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "label" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@label@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructMenuItemLabel :: (IsMenuItem o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructMenuItemLabel val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "label" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data MenuItemLabelPropertyInfo
instance AttrInfo MenuItemLabelPropertyInfo where
    type AttrAllowedOps MenuItemLabelPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint MenuItemLabelPropertyInfo = IsMenuItem
    type AttrSetTypeConstraint MenuItemLabelPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint MenuItemLabelPropertyInfo = (~) T.Text
    type AttrTransferType MenuItemLabelPropertyInfo = T.Text
    type AttrGetType MenuItemLabelPropertyInfo = T.Text
    type AttrLabel MenuItemLabelPropertyInfo = "label"
    type AttrOrigin MenuItemLabelPropertyInfo = MenuItem
    attrGet = getMenuItemLabel
    attrSet = setMenuItemLabel
    attrTransfer _ v = do
        return v
    attrConstruct = constructMenuItemLabel
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuItem.label"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuItem.html#g:attr:label"
        })
#endif

-- VVV Prop "right-justified"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@right-justified@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' menuItem #rightJustified
-- @
getMenuItemRightJustified :: (MonadIO m, IsMenuItem o) => o -> m Bool
getMenuItemRightJustified obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "right-justified"

-- | Set the value of the “@right-justified@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' menuItem [ #rightJustified 'Data.GI.Base.Attributes.:=' value ]
-- @
setMenuItemRightJustified :: (MonadIO m, IsMenuItem o) => o -> Bool -> m ()
setMenuItemRightJustified obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "right-justified" val

-- | Construct a `GValueConstruct` with valid value for the “@right-justified@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructMenuItemRightJustified :: (IsMenuItem o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructMenuItemRightJustified val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "right-justified" val

#if defined(ENABLE_OVERLOADING)
data MenuItemRightJustifiedPropertyInfo
instance AttrInfo MenuItemRightJustifiedPropertyInfo where
    type AttrAllowedOps MenuItemRightJustifiedPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint MenuItemRightJustifiedPropertyInfo = IsMenuItem
    type AttrSetTypeConstraint MenuItemRightJustifiedPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint MenuItemRightJustifiedPropertyInfo = (~) Bool
    type AttrTransferType MenuItemRightJustifiedPropertyInfo = Bool
    type AttrGetType MenuItemRightJustifiedPropertyInfo = Bool
    type AttrLabel MenuItemRightJustifiedPropertyInfo = "right-justified"
    type AttrOrigin MenuItemRightJustifiedPropertyInfo = MenuItem
    attrGet = getMenuItemRightJustified
    attrSet = setMenuItemRightJustified
    attrTransfer _ v = do
        return v
    attrConstruct = constructMenuItemRightJustified
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuItem.rightJustified"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuItem.html#g:attr:rightJustified"
        })
#endif

-- VVV Prop "submenu"
   -- Type: TInterface (Name {namespace = "Gtk", name = "Menu"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Just True)

-- | Get the value of the “@submenu@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' menuItem #submenu
-- @
getMenuItemSubmenu :: (MonadIO m, IsMenuItem o) => o -> m (Maybe Gtk.Menu.Menu)
getMenuItemSubmenu obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "submenu" Gtk.Menu.Menu

-- | Set the value of the “@submenu@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' menuItem [ #submenu 'Data.GI.Base.Attributes.:=' value ]
-- @
setMenuItemSubmenu :: (MonadIO m, IsMenuItem o, Gtk.Menu.IsMenu a) => o -> a -> m ()
setMenuItemSubmenu obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "submenu" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@submenu@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructMenuItemSubmenu :: (IsMenuItem o, MIO.MonadIO m, Gtk.Menu.IsMenu a) => a -> m (GValueConstruct o)
constructMenuItemSubmenu val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "submenu" (P.Just val)

-- | Set the value of the “@submenu@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #submenu
-- @
clearMenuItemSubmenu :: (MonadIO m, IsMenuItem o) => o -> m ()
clearMenuItemSubmenu obj = liftIO $ B.Properties.setObjectPropertyObject obj "submenu" (Nothing :: Maybe Gtk.Menu.Menu)

#if defined(ENABLE_OVERLOADING)
data MenuItemSubmenuPropertyInfo
instance AttrInfo MenuItemSubmenuPropertyInfo where
    type AttrAllowedOps MenuItemSubmenuPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint MenuItemSubmenuPropertyInfo = IsMenuItem
    type AttrSetTypeConstraint MenuItemSubmenuPropertyInfo = Gtk.Menu.IsMenu
    type AttrTransferTypeConstraint MenuItemSubmenuPropertyInfo = Gtk.Menu.IsMenu
    type AttrTransferType MenuItemSubmenuPropertyInfo = Gtk.Menu.Menu
    type AttrGetType MenuItemSubmenuPropertyInfo = (Maybe Gtk.Menu.Menu)
    type AttrLabel MenuItemSubmenuPropertyInfo = "submenu"
    type AttrOrigin MenuItemSubmenuPropertyInfo = MenuItem
    attrGet = getMenuItemSubmenu
    attrSet = setMenuItemSubmenu
    attrTransfer _ v = do
        unsafeCastTo Gtk.Menu.Menu v
    attrConstruct = constructMenuItemSubmenu
    attrClear = clearMenuItemSubmenu
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuItem.submenu"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuItem.html#g:attr:submenu"
        })
#endif

-- VVV Prop "use-underline"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@use-underline@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' menuItem #useUnderline
-- @
getMenuItemUseUnderline :: (MonadIO m, IsMenuItem o) => o -> m Bool
getMenuItemUseUnderline obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "use-underline"

-- | Set the value of the “@use-underline@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' menuItem [ #useUnderline 'Data.GI.Base.Attributes.:=' value ]
-- @
setMenuItemUseUnderline :: (MonadIO m, IsMenuItem o) => o -> Bool -> m ()
setMenuItemUseUnderline obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "use-underline" val

-- | Construct a `GValueConstruct` with valid value for the “@use-underline@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructMenuItemUseUnderline :: (IsMenuItem o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructMenuItemUseUnderline val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "use-underline" val

#if defined(ENABLE_OVERLOADING)
data MenuItemUseUnderlinePropertyInfo
instance AttrInfo MenuItemUseUnderlinePropertyInfo where
    type AttrAllowedOps MenuItemUseUnderlinePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint MenuItemUseUnderlinePropertyInfo = IsMenuItem
    type AttrSetTypeConstraint MenuItemUseUnderlinePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint MenuItemUseUnderlinePropertyInfo = (~) Bool
    type AttrTransferType MenuItemUseUnderlinePropertyInfo = Bool
    type AttrGetType MenuItemUseUnderlinePropertyInfo = Bool
    type AttrLabel MenuItemUseUnderlinePropertyInfo = "use-underline"
    type AttrOrigin MenuItemUseUnderlinePropertyInfo = MenuItem
    attrGet = getMenuItemUseUnderline
    attrSet = setMenuItemUseUnderline
    attrTransfer _ v = do
        return v
    attrConstruct = constructMenuItemUseUnderline
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuItem.useUnderline"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuItem.html#g:attr:useUnderline"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList MenuItem
type instance O.AttributeList MenuItem = MenuItemAttributeList
type MenuItemAttributeList = ('[ '("accelPath", MenuItemAccelPathPropertyInfo), '("actionName", Gtk.Actionable.ActionableActionNamePropertyInfo), '("actionTarget", Gtk.Actionable.ActionableActionTargetPropertyInfo), '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("label", MenuItemLabelPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("relatedAction", Gtk.Activatable.ActivatableRelatedActionPropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("rightJustified", MenuItemRightJustifiedPropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("submenu", MenuItemSubmenuPropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("useActionAppearance", Gtk.Activatable.ActivatableUseActionAppearancePropertyInfo), '("useUnderline", MenuItemUseUnderlinePropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
menuItemAccelPath :: AttrLabelProxy "accelPath"
menuItemAccelPath = AttrLabelProxy

menuItemLabel :: AttrLabelProxy "label"
menuItemLabel = AttrLabelProxy

menuItemRightJustified :: AttrLabelProxy "rightJustified"
menuItemRightJustified = AttrLabelProxy

menuItemSubmenu :: AttrLabelProxy "submenu"
menuItemSubmenu = AttrLabelProxy

menuItemUseUnderline :: AttrLabelProxy "useUnderline"
menuItemUseUnderline = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList MenuItem = MenuItemSignalList
type MenuItemSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("activate", MenuItemActivateSignalInfo), '("activateItem", MenuItemActivateItemSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("deselect", MenuItemDeselectSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("select", MenuItemSelectSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("toggleSizeAllocate", MenuItemToggleSizeAllocateSignalInfo), '("toggleSizeRequest", MenuItemToggleSizeRequestSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method MenuItem::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "MenuItem" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_menu_item_new" gtk_menu_item_new :: 
    IO (Ptr MenuItem)

-- | Creates a new t'GI.Gtk.Objects.MenuItem.MenuItem'.
menuItemNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m MenuItem
    -- ^ __Returns:__ the newly created t'GI.Gtk.Objects.MenuItem.MenuItem'
menuItemNew  = liftIO $ do
    result <- gtk_menu_item_new
    checkUnexpectedReturnNULL "menuItemNew" result
    result' <- (newObject MenuItem) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method MenuItem::new_with_label
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the text for the label"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "MenuItem" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_menu_item_new_with_label" gtk_menu_item_new_with_label :: 
    CString ->                              -- label : TBasicType TUTF8
    IO (Ptr MenuItem)

-- | Creates a new t'GI.Gtk.Objects.MenuItem.MenuItem' whose child is a t'GI.Gtk.Objects.Label.Label'.
menuItemNewWithLabel ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@label@/: the text for the label
    -> m MenuItem
    -- ^ __Returns:__ the newly created t'GI.Gtk.Objects.MenuItem.MenuItem'
menuItemNewWithLabel label = liftIO $ do
    label' <- textToCString label
    result <- gtk_menu_item_new_with_label label'
    checkUnexpectedReturnNULL "menuItemNewWithLabel" result
    result' <- (newObject MenuItem) result
    freeMem label'
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method MenuItem::new_with_mnemonic
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
--                       "The text of the button, with an underscore in front of the\n    mnemonic character"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "MenuItem" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_menu_item_new_with_mnemonic" gtk_menu_item_new_with_mnemonic :: 
    CString ->                              -- label : TBasicType TUTF8
    IO (Ptr MenuItem)

-- | Creates a new t'GI.Gtk.Objects.MenuItem.MenuItem' containing a label.
-- 
-- The label will be created using 'GI.Gtk.Objects.Label.labelNewWithMnemonic',
-- so underscores in /@label@/ indicate the mnemonic for the menu item.
menuItemNewWithMnemonic ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@label@/: The text of the button, with an underscore in front of the
    --     mnemonic character
    -> m MenuItem
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.MenuItem.MenuItem'
menuItemNewWithMnemonic label = liftIO $ do
    label' <- textToCString label
    result <- gtk_menu_item_new_with_mnemonic label'
    checkUnexpectedReturnNULL "menuItemNewWithMnemonic" result
    result' <- (newObject MenuItem) result
    freeMem label'
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method MenuItem::activate
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu_item"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MenuItem" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the menu item" , sinceVersion = Nothing }
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

foreign import ccall "gtk_menu_item_activate" gtk_menu_item_activate :: 
    Ptr MenuItem ->                         -- menu_item : TInterface (Name {namespace = "Gtk", name = "MenuItem"})
    IO ()

-- | Emits the [MenuItem::activate]("GI.Gtk.Objects.MenuItem#g:signal:activate") signal on the given item
menuItemActivate ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenuItem a) =>
    a
    -- ^ /@menuItem@/: the menu item
    -> m ()
menuItemActivate menuItem = liftIO $ do
    menuItem' <- unsafeManagedPtrCastPtr menuItem
    gtk_menu_item_activate menuItem'
    touchManagedPtr menuItem
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuItemActivateMethodInfo
instance (signature ~ (m ()), MonadIO m, IsMenuItem a) => O.OverloadedMethod MenuItemActivateMethodInfo a signature where
    overloadedMethod = menuItemActivate

instance O.OverloadedMethodInfo MenuItemActivateMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuItem.menuItemActivate",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuItem.html#v:menuItemActivate"
        })


#endif

-- method MenuItem::deselect
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu_item"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MenuItem" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the menu item" , sinceVersion = Nothing }
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

foreign import ccall "gtk_menu_item_deselect" gtk_menu_item_deselect :: 
    Ptr MenuItem ->                         -- menu_item : TInterface (Name {namespace = "Gtk", name = "MenuItem"})
    IO ()

-- | Emits the [MenuItem::deselect]("GI.Gtk.Objects.MenuItem#g:signal:deselect") signal on the given item.
menuItemDeselect ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenuItem a) =>
    a
    -- ^ /@menuItem@/: the menu item
    -> m ()
menuItemDeselect menuItem = liftIO $ do
    menuItem' <- unsafeManagedPtrCastPtr menuItem
    gtk_menu_item_deselect menuItem'
    touchManagedPtr menuItem
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuItemDeselectMethodInfo
instance (signature ~ (m ()), MonadIO m, IsMenuItem a) => O.OverloadedMethod MenuItemDeselectMethodInfo a signature where
    overloadedMethod = menuItemDeselect

instance O.OverloadedMethodInfo MenuItemDeselectMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuItem.menuItemDeselect",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuItem.html#v:menuItemDeselect"
        })


#endif

-- method MenuItem::get_accel_path
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu_item"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MenuItem" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a valid #GtkMenuItem"
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
-- returnType: Just (TBasicType TUTF8)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_menu_item_get_accel_path" gtk_menu_item_get_accel_path :: 
    Ptr MenuItem ->                         -- menu_item : TInterface (Name {namespace = "Gtk", name = "MenuItem"})
    IO CString

-- | Retrieve the accelerator path that was previously set on /@menuItem@/.
-- 
-- See 'GI.Gtk.Objects.MenuItem.menuItemSetAccelPath' for details.
-- 
-- /Since: 2.14/
menuItemGetAccelPath ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenuItem a) =>
    a
    -- ^ /@menuItem@/: a valid t'GI.Gtk.Objects.MenuItem.MenuItem'
    -> m (Maybe T.Text)
    -- ^ __Returns:__ the accelerator path corresponding to
    --     this menu item’s functionality, or 'P.Nothing' if not set
menuItemGetAccelPath menuItem = liftIO $ do
    menuItem' <- unsafeManagedPtrCastPtr menuItem
    result <- gtk_menu_item_get_accel_path menuItem'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToText result'
        return result''
    touchManagedPtr menuItem
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data MenuItemGetAccelPathMethodInfo
instance (signature ~ (m (Maybe T.Text)), MonadIO m, IsMenuItem a) => O.OverloadedMethod MenuItemGetAccelPathMethodInfo a signature where
    overloadedMethod = menuItemGetAccelPath

instance O.OverloadedMethodInfo MenuItemGetAccelPathMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuItem.menuItemGetAccelPath",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuItem.html#v:menuItemGetAccelPath"
        })


#endif

-- method MenuItem::get_label
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu_item"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MenuItem" }
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
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TUTF8)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_menu_item_get_label" gtk_menu_item_get_label :: 
    Ptr MenuItem ->                         -- menu_item : TInterface (Name {namespace = "Gtk", name = "MenuItem"})
    IO CString

-- | Sets /@text@/ on the /@menuItem@/ label
-- 
-- /Since: 2.16/
menuItemGetLabel ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenuItem a) =>
    a
    -- ^ /@menuItem@/: a t'GI.Gtk.Objects.MenuItem.MenuItem'
    -> m T.Text
    -- ^ __Returns:__ The text in the /@menuItem@/ label. This is the internal
    --   string used by the label, and must not be modified.
menuItemGetLabel menuItem = liftIO $ do
    menuItem' <- unsafeManagedPtrCastPtr menuItem
    result <- gtk_menu_item_get_label menuItem'
    checkUnexpectedReturnNULL "menuItemGetLabel" result
    result' <- cstringToText result
    touchManagedPtr menuItem
    return result'

#if defined(ENABLE_OVERLOADING)
data MenuItemGetLabelMethodInfo
instance (signature ~ (m T.Text), MonadIO m, IsMenuItem a) => O.OverloadedMethod MenuItemGetLabelMethodInfo a signature where
    overloadedMethod = menuItemGetLabel

instance O.OverloadedMethodInfo MenuItemGetLabelMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuItem.menuItemGetLabel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuItem.html#v:menuItemGetLabel"
        })


#endif

-- method MenuItem::get_reserve_indicator
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu_item"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MenuItem" }
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
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_menu_item_get_reserve_indicator" gtk_menu_item_get_reserve_indicator :: 
    Ptr MenuItem ->                         -- menu_item : TInterface (Name {namespace = "Gtk", name = "MenuItem"})
    IO CInt

-- | Returns whether the /@menuItem@/ reserves space for
-- the submenu indicator, regardless if it has a submenu
-- or not.
-- 
-- /Since: 3.0/
menuItemGetReserveIndicator ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenuItem a) =>
    a
    -- ^ /@menuItem@/: a t'GI.Gtk.Objects.MenuItem.MenuItem'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@menuItem@/ always reserves space for the
    --     submenu indicator
menuItemGetReserveIndicator menuItem = liftIO $ do
    menuItem' <- unsafeManagedPtrCastPtr menuItem
    result <- gtk_menu_item_get_reserve_indicator menuItem'
    let result' = (/= 0) result
    touchManagedPtr menuItem
    return result'

#if defined(ENABLE_OVERLOADING)
data MenuItemGetReserveIndicatorMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsMenuItem a) => O.OverloadedMethod MenuItemGetReserveIndicatorMethodInfo a signature where
    overloadedMethod = menuItemGetReserveIndicator

instance O.OverloadedMethodInfo MenuItemGetReserveIndicatorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuItem.menuItemGetReserveIndicator",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuItem.html#v:menuItemGetReserveIndicator"
        })


#endif

-- method MenuItem::get_right_justified
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu_item"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MenuItem" }
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
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_menu_item_get_right_justified" gtk_menu_item_get_right_justified :: 
    Ptr MenuItem ->                         -- menu_item : TInterface (Name {namespace = "Gtk", name = "MenuItem"})
    IO CInt

{-# DEPRECATED menuItemGetRightJustified ["(Since version 3.2)","See 'GI.Gtk.Objects.MenuItem.menuItemSetRightJustified'"] #-}
-- | Gets whether the menu item appears justified at the right
-- side of the menu bar.
menuItemGetRightJustified ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenuItem a) =>
    a
    -- ^ /@menuItem@/: a t'GI.Gtk.Objects.MenuItem.MenuItem'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the menu item will appear at the
    --   far right if added to a menu bar.
menuItemGetRightJustified menuItem = liftIO $ do
    menuItem' <- unsafeManagedPtrCastPtr menuItem
    result <- gtk_menu_item_get_right_justified menuItem'
    let result' = (/= 0) result
    touchManagedPtr menuItem
    return result'

#if defined(ENABLE_OVERLOADING)
data MenuItemGetRightJustifiedMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsMenuItem a) => O.OverloadedMethod MenuItemGetRightJustifiedMethodInfo a signature where
    overloadedMethod = menuItemGetRightJustified

instance O.OverloadedMethodInfo MenuItemGetRightJustifiedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuItem.menuItemGetRightJustified",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuItem.html#v:menuItemGetRightJustified"
        })


#endif

-- method MenuItem::get_submenu
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu_item"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MenuItem" }
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
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Widget" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_menu_item_get_submenu" gtk_menu_item_get_submenu :: 
    Ptr MenuItem ->                         -- menu_item : TInterface (Name {namespace = "Gtk", name = "MenuItem"})
    IO (Ptr Gtk.Widget.Widget)

-- | Gets the submenu underneath this menu item, if any.
-- See 'GI.Gtk.Objects.MenuItem.menuItemSetSubmenu'.
menuItemGetSubmenu ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenuItem a) =>
    a
    -- ^ /@menuItem@/: a t'GI.Gtk.Objects.MenuItem.MenuItem'
    -> m (Maybe Gtk.Widget.Widget)
    -- ^ __Returns:__ submenu for this menu item, or 'P.Nothing' if none
menuItemGetSubmenu menuItem = liftIO $ do
    menuItem' <- unsafeManagedPtrCastPtr menuItem
    result <- gtk_menu_item_get_submenu menuItem'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Widget.Widget) result'
        return result''
    touchManagedPtr menuItem
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data MenuItemGetSubmenuMethodInfo
instance (signature ~ (m (Maybe Gtk.Widget.Widget)), MonadIO m, IsMenuItem a) => O.OverloadedMethod MenuItemGetSubmenuMethodInfo a signature where
    overloadedMethod = menuItemGetSubmenu

instance O.OverloadedMethodInfo MenuItemGetSubmenuMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuItem.menuItemGetSubmenu",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuItem.html#v:menuItemGetSubmenu"
        })


#endif

-- method MenuItem::get_use_underline
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu_item"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MenuItem" }
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
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_menu_item_get_use_underline" gtk_menu_item_get_use_underline :: 
    Ptr MenuItem ->                         -- menu_item : TInterface (Name {namespace = "Gtk", name = "MenuItem"})
    IO CInt

-- | Checks if an underline in the text indicates the next character
-- should be used for the mnemonic accelerator key.
-- 
-- /Since: 2.16/
menuItemGetUseUnderline ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenuItem a) =>
    a
    -- ^ /@menuItem@/: a t'GI.Gtk.Objects.MenuItem.MenuItem'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if an embedded underline in the label
    --     indicates the mnemonic accelerator key.
menuItemGetUseUnderline menuItem = liftIO $ do
    menuItem' <- unsafeManagedPtrCastPtr menuItem
    result <- gtk_menu_item_get_use_underline menuItem'
    let result' = (/= 0) result
    touchManagedPtr menuItem
    return result'

#if defined(ENABLE_OVERLOADING)
data MenuItemGetUseUnderlineMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsMenuItem a) => O.OverloadedMethod MenuItemGetUseUnderlineMethodInfo a signature where
    overloadedMethod = menuItemGetUseUnderline

instance O.OverloadedMethodInfo MenuItemGetUseUnderlineMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuItem.menuItemGetUseUnderline",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuItem.html#v:menuItemGetUseUnderline"
        })


#endif

-- method MenuItem::select
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu_item"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MenuItem" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the menu item" , sinceVersion = Nothing }
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

foreign import ccall "gtk_menu_item_select" gtk_menu_item_select :: 
    Ptr MenuItem ->                         -- menu_item : TInterface (Name {namespace = "Gtk", name = "MenuItem"})
    IO ()

-- | Emits the [MenuItem::select]("GI.Gtk.Objects.MenuItem#g:signal:select") signal on the given item.
menuItemSelect ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenuItem a) =>
    a
    -- ^ /@menuItem@/: the menu item
    -> m ()
menuItemSelect menuItem = liftIO $ do
    menuItem' <- unsafeManagedPtrCastPtr menuItem
    gtk_menu_item_select menuItem'
    touchManagedPtr menuItem
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuItemSelectMethodInfo
instance (signature ~ (m ()), MonadIO m, IsMenuItem a) => O.OverloadedMethod MenuItemSelectMethodInfo a signature where
    overloadedMethod = menuItemSelect

instance O.OverloadedMethodInfo MenuItemSelectMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuItem.menuItemSelect",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuItem.html#v:menuItemSelect"
        })


#endif

-- method MenuItem::set_accel_path
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu_item"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MenuItem" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a valid #GtkMenuItem"
--                 , sinceVersion = Nothing
--                 }
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
--                     Just
--                       "accelerator path, corresponding to this menu\n    item\8217s functionality, or %NULL to unset the current path."
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

foreign import ccall "gtk_menu_item_set_accel_path" gtk_menu_item_set_accel_path :: 
    Ptr MenuItem ->                         -- menu_item : TInterface (Name {namespace = "Gtk", name = "MenuItem"})
    CString ->                              -- accel_path : TBasicType TUTF8
    IO ()

-- | Set the accelerator path on /@menuItem@/, through which runtime
-- changes of the menu item’s accelerator caused by the user can be
-- identified and saved to persistent storage (see 'GI.Gtk.Objects.AccelMap.accelMapSave'
-- on this). To set up a default accelerator for this menu item, call
-- 'GI.Gtk.Objects.AccelMap.accelMapAddEntry' with the same /@accelPath@/. See also
-- 'GI.Gtk.Objects.AccelMap.accelMapAddEntry' on the specifics of accelerator paths,
-- and 'GI.Gtk.Objects.Menu.menuSetAccelPath' for a more convenient variant of
-- this function.
-- 
-- This function is basically a convenience wrapper that handles
-- calling 'GI.Gtk.Objects.Widget.widgetSetAccelPath' with the appropriate accelerator
-- group for the menu item.
-- 
-- Note that you do need to set an accelerator on the parent menu with
-- 'GI.Gtk.Objects.Menu.menuSetAccelGroup' for this to work.
-- 
-- Note that /@accelPath@/ string will be stored in a @/GQuark/@.
-- Therefore, if you pass a static string, you can save some memory
-- by interning it first with 'GI.GLib.Functions.internStaticString'.
menuItemSetAccelPath ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenuItem a) =>
    a
    -- ^ /@menuItem@/: a valid t'GI.Gtk.Objects.MenuItem.MenuItem'
    -> Maybe (T.Text)
    -- ^ /@accelPath@/: accelerator path, corresponding to this menu
    --     item’s functionality, or 'P.Nothing' to unset the current path.
    -> m ()
menuItemSetAccelPath menuItem accelPath = liftIO $ do
    menuItem' <- unsafeManagedPtrCastPtr menuItem
    maybeAccelPath <- case accelPath of
        Nothing -> return nullPtr
        Just jAccelPath -> do
            jAccelPath' <- textToCString jAccelPath
            return jAccelPath'
    gtk_menu_item_set_accel_path menuItem' maybeAccelPath
    touchManagedPtr menuItem
    freeMem maybeAccelPath
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuItemSetAccelPathMethodInfo
instance (signature ~ (Maybe (T.Text) -> m ()), MonadIO m, IsMenuItem a) => O.OverloadedMethod MenuItemSetAccelPathMethodInfo a signature where
    overloadedMethod = menuItemSetAccelPath

instance O.OverloadedMethodInfo MenuItemSetAccelPathMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuItem.menuItemSetAccelPath",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuItem.html#v:menuItemSetAccelPath"
        })


#endif

-- method MenuItem::set_label
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu_item"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MenuItem" }
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
--           { argCName = "label"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the text you want to set"
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

foreign import ccall "gtk_menu_item_set_label" gtk_menu_item_set_label :: 
    Ptr MenuItem ->                         -- menu_item : TInterface (Name {namespace = "Gtk", name = "MenuItem"})
    CString ->                              -- label : TBasicType TUTF8
    IO ()

-- | Sets /@text@/ on the /@menuItem@/ label
-- 
-- /Since: 2.16/
menuItemSetLabel ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenuItem a) =>
    a
    -- ^ /@menuItem@/: a t'GI.Gtk.Objects.MenuItem.MenuItem'
    -> T.Text
    -- ^ /@label@/: the text you want to set
    -> m ()
menuItemSetLabel menuItem label = liftIO $ do
    menuItem' <- unsafeManagedPtrCastPtr menuItem
    label' <- textToCString label
    gtk_menu_item_set_label menuItem' label'
    touchManagedPtr menuItem
    freeMem label'
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuItemSetLabelMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsMenuItem a) => O.OverloadedMethod MenuItemSetLabelMethodInfo a signature where
    overloadedMethod = menuItemSetLabel

instance O.OverloadedMethodInfo MenuItemSetLabelMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuItem.menuItemSetLabel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuItem.html#v:menuItemSetLabel"
        })


#endif

-- method MenuItem::set_reserve_indicator
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu_item"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MenuItem" }
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
--           { argCName = "reserve"
--           , argType = TBasicType TBoolean
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

foreign import ccall "gtk_menu_item_set_reserve_indicator" gtk_menu_item_set_reserve_indicator :: 
    Ptr MenuItem ->                         -- menu_item : TInterface (Name {namespace = "Gtk", name = "MenuItem"})
    CInt ->                                 -- reserve : TBasicType TBoolean
    IO ()

-- | Sets whether the /@menuItem@/ should reserve space for
-- the submenu indicator, regardless if it actually has
-- a submenu or not.
-- 
-- There should be little need for applications to call
-- this functions.
-- 
-- /Since: 3.0/
menuItemSetReserveIndicator ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenuItem a) =>
    a
    -- ^ /@menuItem@/: a t'GI.Gtk.Objects.MenuItem.MenuItem'
    -> Bool
    -- ^ /@reserve@/: the new value
    -> m ()
menuItemSetReserveIndicator menuItem reserve = liftIO $ do
    menuItem' <- unsafeManagedPtrCastPtr menuItem
    let reserve' = (fromIntegral . fromEnum) reserve
    gtk_menu_item_set_reserve_indicator menuItem' reserve'
    touchManagedPtr menuItem
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuItemSetReserveIndicatorMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsMenuItem a) => O.OverloadedMethod MenuItemSetReserveIndicatorMethodInfo a signature where
    overloadedMethod = menuItemSetReserveIndicator

instance O.OverloadedMethodInfo MenuItemSetReserveIndicatorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuItem.menuItemSetReserveIndicator",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuItem.html#v:menuItemSetReserveIndicator"
        })


#endif

-- method MenuItem::set_right_justified
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu_item"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MenuItem" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMenuItem." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "right_justified"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "if %TRUE the menu item will appear at the\n  far right if added to a menu bar"
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

foreign import ccall "gtk_menu_item_set_right_justified" gtk_menu_item_set_right_justified :: 
    Ptr MenuItem ->                         -- menu_item : TInterface (Name {namespace = "Gtk", name = "MenuItem"})
    CInt ->                                 -- right_justified : TBasicType TBoolean
    IO ()

{-# DEPRECATED menuItemSetRightJustified ["(Since version 3.2)","If you insist on using it, use","  'GI.Gtk.Objects.Widget.widgetSetHexpand' and 'GI.Gtk.Objects.Widget.widgetSetHalign'."] #-}
-- | Sets whether the menu item appears justified at the right
-- side of a menu bar. This was traditionally done for “Help”
-- menu items, but is now considered a bad idea. (If the widget
-- layout is reversed for a right-to-left language like Hebrew
-- or Arabic, right-justified-menu-items appear at the left.)
menuItemSetRightJustified ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenuItem a) =>
    a
    -- ^ /@menuItem@/: a t'GI.Gtk.Objects.MenuItem.MenuItem'.
    -> Bool
    -- ^ /@rightJustified@/: if 'P.True' the menu item will appear at the
    --   far right if added to a menu bar
    -> m ()
menuItemSetRightJustified menuItem rightJustified = liftIO $ do
    menuItem' <- unsafeManagedPtrCastPtr menuItem
    let rightJustified' = (fromIntegral . fromEnum) rightJustified
    gtk_menu_item_set_right_justified menuItem' rightJustified'
    touchManagedPtr menuItem
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuItemSetRightJustifiedMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsMenuItem a) => O.OverloadedMethod MenuItemSetRightJustifiedMethodInfo a signature where
    overloadedMethod = menuItemSetRightJustified

instance O.OverloadedMethodInfo MenuItemSetRightJustifiedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuItem.menuItemSetRightJustified",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuItem.html#v:menuItemSetRightJustified"
        })


#endif

-- method MenuItem::set_submenu
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu_item"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MenuItem" }
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
--           { argCName = "submenu"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Menu" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the submenu, or %NULL"
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

foreign import ccall "gtk_menu_item_set_submenu" gtk_menu_item_set_submenu :: 
    Ptr MenuItem ->                         -- menu_item : TInterface (Name {namespace = "Gtk", name = "MenuItem"})
    Ptr Gtk.Menu.Menu ->                    -- submenu : TInterface (Name {namespace = "Gtk", name = "Menu"})
    IO ()

-- | Sets or replaces the menu item’s submenu, or removes it when a 'P.Nothing'
-- submenu is passed.
menuItemSetSubmenu ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenuItem a, Gtk.Menu.IsMenu b) =>
    a
    -- ^ /@menuItem@/: a t'GI.Gtk.Objects.MenuItem.MenuItem'
    -> Maybe (b)
    -- ^ /@submenu@/: the submenu, or 'P.Nothing'
    -> m ()
menuItemSetSubmenu menuItem submenu = liftIO $ do
    menuItem' <- unsafeManagedPtrCastPtr menuItem
    maybeSubmenu <- case submenu of
        Nothing -> return nullPtr
        Just jSubmenu -> do
            jSubmenu' <- unsafeManagedPtrCastPtr jSubmenu
            return jSubmenu'
    gtk_menu_item_set_submenu menuItem' maybeSubmenu
    touchManagedPtr menuItem
    whenJust submenu touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuItemSetSubmenuMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsMenuItem a, Gtk.Menu.IsMenu b) => O.OverloadedMethod MenuItemSetSubmenuMethodInfo a signature where
    overloadedMethod = menuItemSetSubmenu

instance O.OverloadedMethodInfo MenuItemSetSubmenuMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuItem.menuItemSetSubmenu",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuItem.html#v:menuItemSetSubmenu"
        })


#endif

-- method MenuItem::set_use_underline
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu_item"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MenuItem" }
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
--           { argCName = "setting"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "%TRUE if underlines in the text indicate mnemonics"
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

foreign import ccall "gtk_menu_item_set_use_underline" gtk_menu_item_set_use_underline :: 
    Ptr MenuItem ->                         -- menu_item : TInterface (Name {namespace = "Gtk", name = "MenuItem"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | If true, an underline in the text indicates the next character
-- should be used for the mnemonic accelerator key.
-- 
-- /Since: 2.16/
menuItemSetUseUnderline ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenuItem a) =>
    a
    -- ^ /@menuItem@/: a t'GI.Gtk.Objects.MenuItem.MenuItem'
    -> Bool
    -- ^ /@setting@/: 'P.True' if underlines in the text indicate mnemonics
    -> m ()
menuItemSetUseUnderline menuItem setting = liftIO $ do
    menuItem' <- unsafeManagedPtrCastPtr menuItem
    let setting' = (fromIntegral . fromEnum) setting
    gtk_menu_item_set_use_underline menuItem' setting'
    touchManagedPtr menuItem
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuItemSetUseUnderlineMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsMenuItem a) => O.OverloadedMethod MenuItemSetUseUnderlineMethodInfo a signature where
    overloadedMethod = menuItemSetUseUnderline

instance O.OverloadedMethodInfo MenuItemSetUseUnderlineMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuItem.menuItemSetUseUnderline",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuItem.html#v:menuItemSetUseUnderline"
        })


#endif

-- method MenuItem::toggle_size_allocate
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu_item"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MenuItem" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the menu item." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "allocation"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the allocation to use as signal data."
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

foreign import ccall "gtk_menu_item_toggle_size_allocate" gtk_menu_item_toggle_size_allocate :: 
    Ptr MenuItem ->                         -- menu_item : TInterface (Name {namespace = "Gtk", name = "MenuItem"})
    Int32 ->                                -- allocation : TBasicType TInt
    IO ()

-- | Emits the [MenuItem::toggleSizeAllocate]("GI.Gtk.Objects.MenuItem#g:signal:toggleSizeAllocate") signal on the given item.
menuItemToggleSizeAllocate ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenuItem a) =>
    a
    -- ^ /@menuItem@/: the menu item.
    -> Int32
    -- ^ /@allocation@/: the allocation to use as signal data.
    -> m ()
menuItemToggleSizeAllocate menuItem allocation = liftIO $ do
    menuItem' <- unsafeManagedPtrCastPtr menuItem
    gtk_menu_item_toggle_size_allocate menuItem' allocation
    touchManagedPtr menuItem
    return ()

#if defined(ENABLE_OVERLOADING)
data MenuItemToggleSizeAllocateMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsMenuItem a) => O.OverloadedMethod MenuItemToggleSizeAllocateMethodInfo a signature where
    overloadedMethod = menuItemToggleSizeAllocate

instance O.OverloadedMethodInfo MenuItemToggleSizeAllocateMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuItem.menuItemToggleSizeAllocate",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuItem.html#v:menuItemToggleSizeAllocate"
        })


#endif

-- method MenuItem::toggle_size_request
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu_item"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MenuItem" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the menu item" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "requisition"
--           , argType = TBasicType TInt
--           , direction = DirectionInout
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the requisition to use as signal data."
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

foreign import ccall "gtk_menu_item_toggle_size_request" gtk_menu_item_toggle_size_request :: 
    Ptr MenuItem ->                         -- menu_item : TInterface (Name {namespace = "Gtk", name = "MenuItem"})
    Ptr Int32 ->                            -- requisition : TBasicType TInt
    IO ()

-- | Emits the [MenuItem::toggleSizeRequest]("GI.Gtk.Objects.MenuItem#g:signal:toggleSizeRequest") signal on the given item.
menuItemToggleSizeRequest ::
    (B.CallStack.HasCallStack, MonadIO m, IsMenuItem a) =>
    a
    -- ^ /@menuItem@/: the menu item
    -> Int32
    -- ^ /@requisition@/: the requisition to use as signal data.
    -> m (Int32)
menuItemToggleSizeRequest menuItem requisition = liftIO $ do
    menuItem' <- unsafeManagedPtrCastPtr menuItem
    requisition' <- allocMem :: IO (Ptr Int32)
    poke requisition' requisition
    gtk_menu_item_toggle_size_request menuItem' requisition'
    requisition'' <- peek requisition'
    touchManagedPtr menuItem
    freeMem requisition'
    return requisition''

#if defined(ENABLE_OVERLOADING)
data MenuItemToggleSizeRequestMethodInfo
instance (signature ~ (Int32 -> m (Int32)), MonadIO m, IsMenuItem a) => O.OverloadedMethod MenuItemToggleSizeRequestMethodInfo a signature where
    overloadedMethod = menuItemToggleSizeRequest

instance O.OverloadedMethodInfo MenuItemToggleSizeRequestMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MenuItem.menuItemToggleSizeRequest",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MenuItem.html#v:menuItemToggleSizeRequest"
        })


#endif


