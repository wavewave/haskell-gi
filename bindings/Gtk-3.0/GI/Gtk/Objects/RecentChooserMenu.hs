{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Objects.RecentChooserMenu.RecentChooserMenu' is a widget suitable for displaying recently used files
-- inside a menu.  It can be used to set a sub-menu of a t'GI.Gtk.Objects.MenuItem.MenuItem' using
-- 'GI.Gtk.Objects.MenuItem.menuItemSetSubmenu', or as the menu of a t'GI.Gtk.Objects.MenuToolButton.MenuToolButton'.
-- 
-- Note that t'GI.Gtk.Objects.RecentChooserMenu.RecentChooserMenu' does not have any methods of its own. Instead,
-- you should use the functions that work on a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'.
-- 
-- Note also that t'GI.Gtk.Objects.RecentChooserMenu.RecentChooserMenu' does not support multiple filters, as it
-- has no way to let the user choose between them as the t'GI.Gtk.Objects.RecentChooserWidget.RecentChooserWidget'
-- and t'GI.Gtk.Objects.RecentChooserDialog.RecentChooserDialog' widgets do. Thus using 'GI.Gtk.Interfaces.RecentChooser.recentChooserAddFilter'
-- on a t'GI.Gtk.Objects.RecentChooserMenu.RecentChooserMenu' widget will yield the same effects as using
-- 'GI.Gtk.Interfaces.RecentChooser.recentChooserSetFilter', replacing any currently set filter
-- with the supplied filter; 'GI.Gtk.Interfaces.RecentChooser.recentChooserRemoveFilter' will remove
-- any currently set t'GI.Gtk.Objects.RecentFilter.RecentFilter' object and will unset the current filter;
-- 'GI.Gtk.Interfaces.RecentChooser.recentChooserListFilters' will return a list containing a single
-- t'GI.Gtk.Objects.RecentFilter.RecentFilter' object.
-- 
-- Recently used files are supported since GTK+ 2.10.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.RecentChooserMenu
    ( 

-- * Exported types
    RecentChooserMenu(..)                   ,
    IsRecentChooserMenu                     ,
    toRecentChooserMenu                     ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [activateItem]("GI.Gtk.Objects.MenuShell#g:method:activateItem"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addFilter]("GI.Gtk.Interfaces.RecentChooser#g:method:addFilter"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [append]("GI.Gtk.Objects.MenuShell#g:method:append"), [attach]("GI.Gtk.Objects.Menu#g:method:attach"), [attachToWidget]("GI.Gtk.Objects.Menu#g:method:attachToWidget"), [bindModel]("GI.Gtk.Objects.MenuShell#g:method:bindModel"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [cancel]("GI.Gtk.Objects.MenuShell#g:method:cancel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [deactivate]("GI.Gtk.Objects.MenuShell#g:method:deactivate"), [deselect]("GI.Gtk.Objects.MenuShell#g:method:deselect"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [detach]("GI.Gtk.Objects.Menu#g:method:detach"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [doSetRelatedAction]("GI.Gtk.Interfaces.Activatable#g:method:doSetRelatedAction"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insert]("GI.Gtk.Objects.MenuShell#g:method:insert"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listFilters]("GI.Gtk.Interfaces.RecentChooser#g:method:listFilters"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [placeOnMonitor]("GI.Gtk.Objects.Menu#g:method:placeOnMonitor"), [popdown]("GI.Gtk.Objects.Menu#g:method:popdown"), [popup]("GI.Gtk.Objects.Menu#g:method:popup"), [popupAtPointer]("GI.Gtk.Objects.Menu#g:method:popupAtPointer"), [popupAtRect]("GI.Gtk.Objects.Menu#g:method:popupAtRect"), [popupAtWidget]("GI.Gtk.Objects.Menu#g:method:popupAtWidget"), [popupForDevice]("GI.Gtk.Objects.Menu#g:method:popupForDevice"), [prepend]("GI.Gtk.Objects.MenuShell#g:method:prepend"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeFilter]("GI.Gtk.Interfaces.RecentChooser#g:method:removeFilter"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reorderChild]("GI.Gtk.Objects.Menu#g:method:reorderChild"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [reposition]("GI.Gtk.Objects.Menu#g:method:reposition"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [selectAll]("GI.Gtk.Interfaces.RecentChooser#g:method:selectAll"), [selectFirst]("GI.Gtk.Objects.MenuShell#g:method:selectFirst"), [selectItem]("GI.Gtk.Objects.MenuShell#g:method:selectItem"), [selectUri]("GI.Gtk.Interfaces.RecentChooser#g:method:selectUri"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [syncActionProperties]("GI.Gtk.Interfaces.Activatable#g:method:syncActionProperties"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unselectAll]("GI.Gtk.Interfaces.RecentChooser#g:method:unselectAll"), [unselectUri]("GI.Gtk.Interfaces.RecentChooser#g:method:unselectUri"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccelGroup]("GI.Gtk.Objects.Menu#g:method:getAccelGroup"), [getAccelPath]("GI.Gtk.Objects.Menu#g:method:getAccelPath"), [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getActive]("GI.Gtk.Objects.Menu#g:method:getActive"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getAttachWidget]("GI.Gtk.Objects.Menu#g:method:getAttachWidget"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getCurrentItem]("GI.Gtk.Interfaces.RecentChooser#g:method:getCurrentItem"), [getCurrentUri]("GI.Gtk.Interfaces.RecentChooser#g:method:getCurrentUri"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFilter]("GI.Gtk.Interfaces.RecentChooser#g:method:getFilter"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getItems]("GI.Gtk.Interfaces.RecentChooser#g:method:getItems"), [getLimit]("GI.Gtk.Interfaces.RecentChooser#g:method:getLimit"), [getLocalOnly]("GI.Gtk.Interfaces.RecentChooser#g:method:getLocalOnly"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getMonitor]("GI.Gtk.Objects.Menu#g:method:getMonitor"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentShell]("GI.Gtk.Objects.MenuShell#g:method:getParentShell"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRelatedAction]("GI.Gtk.Interfaces.Activatable#g:method:getRelatedAction"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getReserveToggleSize]("GI.Gtk.Objects.Menu#g:method:getReserveToggleSize"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSelectMultiple]("GI.Gtk.Interfaces.RecentChooser#g:method:getSelectMultiple"), [getSelectedItem]("GI.Gtk.Objects.MenuShell#g:method:getSelectedItem"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getShowIcons]("GI.Gtk.Interfaces.RecentChooser#g:method:getShowIcons"), [getShowNotFound]("GI.Gtk.Interfaces.RecentChooser#g:method:getShowNotFound"), [getShowNumbers]("GI.Gtk.Objects.RecentChooserMenu#g:method:getShowNumbers"), [getShowPrivate]("GI.Gtk.Interfaces.RecentChooser#g:method:getShowPrivate"), [getShowTips]("GI.Gtk.Interfaces.RecentChooser#g:method:getShowTips"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getSortType]("GI.Gtk.Interfaces.RecentChooser#g:method:getSortType"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTakeFocus]("GI.Gtk.Objects.MenuShell#g:method:getTakeFocus"), [getTearoffState]("GI.Gtk.Objects.Menu#g:method:getTearoffState"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTitle]("GI.Gtk.Objects.Menu#g:method:getTitle"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getUris]("GI.Gtk.Interfaces.RecentChooser#g:method:getUris"), [getUseActionAppearance]("GI.Gtk.Interfaces.Activatable#g:method:getUseActionAppearance"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelGroup]("GI.Gtk.Objects.Menu#g:method:setAccelGroup"), [setAccelPath]("GI.Gtk.Objects.Menu#g:method:setAccelPath"), [setActive]("GI.Gtk.Objects.Menu#g:method:setActive"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setCurrentUri]("GI.Gtk.Interfaces.RecentChooser#g:method:setCurrentUri"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFilter]("GI.Gtk.Interfaces.RecentChooser#g:method:setFilter"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setLimit]("GI.Gtk.Interfaces.RecentChooser#g:method:setLimit"), [setLocalOnly]("GI.Gtk.Interfaces.RecentChooser#g:method:setLocalOnly"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setMonitor]("GI.Gtk.Objects.Menu#g:method:setMonitor"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setRelatedAction]("GI.Gtk.Interfaces.Activatable#g:method:setRelatedAction"), [setReserveToggleSize]("GI.Gtk.Objects.Menu#g:method:setReserveToggleSize"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setScreen]("GI.Gtk.Objects.Menu#g:method:setScreen"), [setSelectMultiple]("GI.Gtk.Interfaces.RecentChooser#g:method:setSelectMultiple"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setShowIcons]("GI.Gtk.Interfaces.RecentChooser#g:method:setShowIcons"), [setShowNotFound]("GI.Gtk.Interfaces.RecentChooser#g:method:setShowNotFound"), [setShowNumbers]("GI.Gtk.Objects.RecentChooserMenu#g:method:setShowNumbers"), [setShowPrivate]("GI.Gtk.Interfaces.RecentChooser#g:method:setShowPrivate"), [setShowTips]("GI.Gtk.Interfaces.RecentChooser#g:method:setShowTips"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setSortFunc]("GI.Gtk.Interfaces.RecentChooser#g:method:setSortFunc"), [setSortType]("GI.Gtk.Interfaces.RecentChooser#g:method:setSortType"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTakeFocus]("GI.Gtk.Objects.MenuShell#g:method:setTakeFocus"), [setTearoffState]("GI.Gtk.Objects.Menu#g:method:setTearoffState"), [setTitle]("GI.Gtk.Objects.Menu#g:method:setTitle"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setUseActionAppearance]("GI.Gtk.Interfaces.Activatable#g:method:setUseActionAppearance"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveRecentChooserMenuMethod          ,
#endif

-- ** getShowNumbers #method:getShowNumbers#

#if defined(ENABLE_OVERLOADING)
    RecentChooserMenuGetShowNumbersMethodInfo,
#endif
    recentChooserMenuGetShowNumbers         ,


-- ** new #method:new#

    recentChooserMenuNew                    ,


-- ** newForManager #method:newForManager#

    recentChooserMenuNewForManager          ,


-- ** setShowNumbers #method:setShowNumbers#

#if defined(ENABLE_OVERLOADING)
    RecentChooserMenuSetShowNumbersMethodInfo,
#endif
    recentChooserMenuSetShowNumbers         ,




 -- * Properties


-- ** showNumbers #attr:showNumbers#
-- | Whether the first ten items in the menu should be prepended by
-- a number acting as a unique mnemonic.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    RecentChooserMenuShowNumbersPropertyInfo,
#endif
    constructRecentChooserMenuShowNumbers   ,
    getRecentChooserMenuShowNumbers         ,
#if defined(ENABLE_OVERLOADING)
    recentChooserMenuShowNumbers            ,
#endif
    setRecentChooserMenuShowNumbers         ,




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
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Activatable as Gtk.Activatable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.RecentChooser as Gtk.RecentChooser
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.Menu as Gtk.Menu
import {-# SOURCE #-} qualified GI.Gtk.Objects.MenuShell as Gtk.MenuShell
import {-# SOURCE #-} qualified GI.Gtk.Objects.RecentManager as Gtk.RecentManager
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype RecentChooserMenu = RecentChooserMenu (SP.ManagedPtr RecentChooserMenu)
    deriving (Eq)

instance SP.ManagedPtrNewtype RecentChooserMenu where
    toManagedPtr (RecentChooserMenu p) = p

foreign import ccall "gtk_recent_chooser_menu_get_type"
    c_gtk_recent_chooser_menu_get_type :: IO B.Types.GType

instance B.Types.TypedObject RecentChooserMenu where
    glibType = c_gtk_recent_chooser_menu_get_type

instance B.Types.GObject RecentChooserMenu

-- | Type class for types which can be safely cast to `RecentChooserMenu`, for instance with `toRecentChooserMenu`.
class (SP.GObject o, O.IsDescendantOf RecentChooserMenu o) => IsRecentChooserMenu o
instance (SP.GObject o, O.IsDescendantOf RecentChooserMenu o) => IsRecentChooserMenu o

instance O.HasParentTypes RecentChooserMenu
type instance O.ParentTypes RecentChooserMenu = '[Gtk.Menu.Menu, Gtk.MenuShell.MenuShell, Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Activatable.Activatable, Gtk.Buildable.Buildable, Gtk.RecentChooser.RecentChooser]

-- | Cast to `RecentChooserMenu`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toRecentChooserMenu :: (MIO.MonadIO m, IsRecentChooserMenu o) => o -> m RecentChooserMenu
toRecentChooserMenu = MIO.liftIO . B.ManagedPtr.unsafeCastTo RecentChooserMenu

-- | Convert 'RecentChooserMenu' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe RecentChooserMenu) where
    gvalueGType_ = c_gtk_recent_chooser_menu_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr RecentChooserMenu)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr RecentChooserMenu)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject RecentChooserMenu ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveRecentChooserMenuMethod (t :: Symbol) (o :: *) :: * where
    ResolveRecentChooserMenuMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveRecentChooserMenuMethod "activateItem" o = Gtk.MenuShell.MenuShellActivateItemMethodInfo
    ResolveRecentChooserMenuMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveRecentChooserMenuMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveRecentChooserMenuMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveRecentChooserMenuMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveRecentChooserMenuMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveRecentChooserMenuMethod "addFilter" o = Gtk.RecentChooser.RecentChooserAddFilterMethodInfo
    ResolveRecentChooserMenuMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveRecentChooserMenuMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveRecentChooserMenuMethod "append" o = Gtk.MenuShell.MenuShellAppendMethodInfo
    ResolveRecentChooserMenuMethod "attach" o = Gtk.Menu.MenuAttachMethodInfo
    ResolveRecentChooserMenuMethod "attachToWidget" o = Gtk.Menu.MenuAttachToWidgetMethodInfo
    ResolveRecentChooserMenuMethod "bindModel" o = Gtk.MenuShell.MenuShellBindModelMethodInfo
    ResolveRecentChooserMenuMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveRecentChooserMenuMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveRecentChooserMenuMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveRecentChooserMenuMethod "cancel" o = Gtk.MenuShell.MenuShellCancelMethodInfo
    ResolveRecentChooserMenuMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveRecentChooserMenuMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveRecentChooserMenuMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveRecentChooserMenuMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveRecentChooserMenuMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveRecentChooserMenuMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveRecentChooserMenuMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveRecentChooserMenuMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveRecentChooserMenuMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveRecentChooserMenuMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveRecentChooserMenuMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveRecentChooserMenuMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveRecentChooserMenuMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveRecentChooserMenuMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveRecentChooserMenuMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveRecentChooserMenuMethod "deactivate" o = Gtk.MenuShell.MenuShellDeactivateMethodInfo
    ResolveRecentChooserMenuMethod "deselect" o = Gtk.MenuShell.MenuShellDeselectMethodInfo
    ResolveRecentChooserMenuMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveRecentChooserMenuMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveRecentChooserMenuMethod "detach" o = Gtk.Menu.MenuDetachMethodInfo
    ResolveRecentChooserMenuMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveRecentChooserMenuMethod "doSetRelatedAction" o = Gtk.Activatable.ActivatableDoSetRelatedActionMethodInfo
    ResolveRecentChooserMenuMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveRecentChooserMenuMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveRecentChooserMenuMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveRecentChooserMenuMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveRecentChooserMenuMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveRecentChooserMenuMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveRecentChooserMenuMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveRecentChooserMenuMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveRecentChooserMenuMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveRecentChooserMenuMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveRecentChooserMenuMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveRecentChooserMenuMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveRecentChooserMenuMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveRecentChooserMenuMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveRecentChooserMenuMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveRecentChooserMenuMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveRecentChooserMenuMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveRecentChooserMenuMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveRecentChooserMenuMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveRecentChooserMenuMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveRecentChooserMenuMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveRecentChooserMenuMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveRecentChooserMenuMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveRecentChooserMenuMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveRecentChooserMenuMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveRecentChooserMenuMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveRecentChooserMenuMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveRecentChooserMenuMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveRecentChooserMenuMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveRecentChooserMenuMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveRecentChooserMenuMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveRecentChooserMenuMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveRecentChooserMenuMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveRecentChooserMenuMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveRecentChooserMenuMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveRecentChooserMenuMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveRecentChooserMenuMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveRecentChooserMenuMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveRecentChooserMenuMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveRecentChooserMenuMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveRecentChooserMenuMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveRecentChooserMenuMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveRecentChooserMenuMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveRecentChooserMenuMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveRecentChooserMenuMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveRecentChooserMenuMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveRecentChooserMenuMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveRecentChooserMenuMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveRecentChooserMenuMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveRecentChooserMenuMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveRecentChooserMenuMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveRecentChooserMenuMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveRecentChooserMenuMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveRecentChooserMenuMethod "insert" o = Gtk.MenuShell.MenuShellInsertMethodInfo
    ResolveRecentChooserMenuMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveRecentChooserMenuMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveRecentChooserMenuMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveRecentChooserMenuMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveRecentChooserMenuMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveRecentChooserMenuMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveRecentChooserMenuMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveRecentChooserMenuMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveRecentChooserMenuMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveRecentChooserMenuMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveRecentChooserMenuMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveRecentChooserMenuMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveRecentChooserMenuMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveRecentChooserMenuMethod "listFilters" o = Gtk.RecentChooser.RecentChooserListFiltersMethodInfo
    ResolveRecentChooserMenuMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveRecentChooserMenuMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveRecentChooserMenuMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveRecentChooserMenuMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveRecentChooserMenuMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveRecentChooserMenuMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveRecentChooserMenuMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveRecentChooserMenuMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveRecentChooserMenuMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveRecentChooserMenuMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveRecentChooserMenuMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveRecentChooserMenuMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveRecentChooserMenuMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveRecentChooserMenuMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveRecentChooserMenuMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveRecentChooserMenuMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveRecentChooserMenuMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveRecentChooserMenuMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveRecentChooserMenuMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveRecentChooserMenuMethod "placeOnMonitor" o = Gtk.Menu.MenuPlaceOnMonitorMethodInfo
    ResolveRecentChooserMenuMethod "popdown" o = Gtk.Menu.MenuPopdownMethodInfo
    ResolveRecentChooserMenuMethod "popup" o = Gtk.Menu.MenuPopupMethodInfo
    ResolveRecentChooserMenuMethod "popupAtPointer" o = Gtk.Menu.MenuPopupAtPointerMethodInfo
    ResolveRecentChooserMenuMethod "popupAtRect" o = Gtk.Menu.MenuPopupAtRectMethodInfo
    ResolveRecentChooserMenuMethod "popupAtWidget" o = Gtk.Menu.MenuPopupAtWidgetMethodInfo
    ResolveRecentChooserMenuMethod "popupForDevice" o = Gtk.Menu.MenuPopupForDeviceMethodInfo
    ResolveRecentChooserMenuMethod "prepend" o = Gtk.MenuShell.MenuShellPrependMethodInfo
    ResolveRecentChooserMenuMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveRecentChooserMenuMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveRecentChooserMenuMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveRecentChooserMenuMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveRecentChooserMenuMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveRecentChooserMenuMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveRecentChooserMenuMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveRecentChooserMenuMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveRecentChooserMenuMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveRecentChooserMenuMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveRecentChooserMenuMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveRecentChooserMenuMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveRecentChooserMenuMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveRecentChooserMenuMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolveRecentChooserMenuMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveRecentChooserMenuMethod "removeFilter" o = Gtk.RecentChooser.RecentChooserRemoveFilterMethodInfo
    ResolveRecentChooserMenuMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveRecentChooserMenuMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveRecentChooserMenuMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveRecentChooserMenuMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveRecentChooserMenuMethod "reorderChild" o = Gtk.Menu.MenuReorderChildMethodInfo
    ResolveRecentChooserMenuMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveRecentChooserMenuMethod "reposition" o = Gtk.Menu.MenuRepositionMethodInfo
    ResolveRecentChooserMenuMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveRecentChooserMenuMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveRecentChooserMenuMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveRecentChooserMenuMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveRecentChooserMenuMethod "selectAll" o = Gtk.RecentChooser.RecentChooserSelectAllMethodInfo
    ResolveRecentChooserMenuMethod "selectFirst" o = Gtk.MenuShell.MenuShellSelectFirstMethodInfo
    ResolveRecentChooserMenuMethod "selectItem" o = Gtk.MenuShell.MenuShellSelectItemMethodInfo
    ResolveRecentChooserMenuMethod "selectUri" o = Gtk.RecentChooser.RecentChooserSelectUriMethodInfo
    ResolveRecentChooserMenuMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveRecentChooserMenuMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveRecentChooserMenuMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveRecentChooserMenuMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveRecentChooserMenuMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveRecentChooserMenuMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveRecentChooserMenuMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveRecentChooserMenuMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveRecentChooserMenuMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveRecentChooserMenuMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveRecentChooserMenuMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveRecentChooserMenuMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveRecentChooserMenuMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveRecentChooserMenuMethod "syncActionProperties" o = Gtk.Activatable.ActivatableSyncActionPropertiesMethodInfo
    ResolveRecentChooserMenuMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveRecentChooserMenuMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveRecentChooserMenuMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveRecentChooserMenuMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveRecentChooserMenuMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveRecentChooserMenuMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveRecentChooserMenuMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveRecentChooserMenuMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveRecentChooserMenuMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveRecentChooserMenuMethod "unselectAll" o = Gtk.RecentChooser.RecentChooserUnselectAllMethodInfo
    ResolveRecentChooserMenuMethod "unselectUri" o = Gtk.RecentChooser.RecentChooserUnselectUriMethodInfo
    ResolveRecentChooserMenuMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveRecentChooserMenuMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveRecentChooserMenuMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveRecentChooserMenuMethod "getAccelGroup" o = Gtk.Menu.MenuGetAccelGroupMethodInfo
    ResolveRecentChooserMenuMethod "getAccelPath" o = Gtk.Menu.MenuGetAccelPathMethodInfo
    ResolveRecentChooserMenuMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveRecentChooserMenuMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveRecentChooserMenuMethod "getActive" o = Gtk.Menu.MenuGetActiveMethodInfo
    ResolveRecentChooserMenuMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveRecentChooserMenuMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveRecentChooserMenuMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveRecentChooserMenuMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveRecentChooserMenuMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveRecentChooserMenuMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveRecentChooserMenuMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveRecentChooserMenuMethod "getAttachWidget" o = Gtk.Menu.MenuGetAttachWidgetMethodInfo
    ResolveRecentChooserMenuMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveRecentChooserMenuMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveRecentChooserMenuMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveRecentChooserMenuMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveRecentChooserMenuMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveRecentChooserMenuMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveRecentChooserMenuMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveRecentChooserMenuMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveRecentChooserMenuMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveRecentChooserMenuMethod "getCurrentItem" o = Gtk.RecentChooser.RecentChooserGetCurrentItemMethodInfo
    ResolveRecentChooserMenuMethod "getCurrentUri" o = Gtk.RecentChooser.RecentChooserGetCurrentUriMethodInfo
    ResolveRecentChooserMenuMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveRecentChooserMenuMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveRecentChooserMenuMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveRecentChooserMenuMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveRecentChooserMenuMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveRecentChooserMenuMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveRecentChooserMenuMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveRecentChooserMenuMethod "getFilter" o = Gtk.RecentChooser.RecentChooserGetFilterMethodInfo
    ResolveRecentChooserMenuMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveRecentChooserMenuMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveRecentChooserMenuMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveRecentChooserMenuMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveRecentChooserMenuMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveRecentChooserMenuMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveRecentChooserMenuMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveRecentChooserMenuMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveRecentChooserMenuMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveRecentChooserMenuMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveRecentChooserMenuMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveRecentChooserMenuMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveRecentChooserMenuMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveRecentChooserMenuMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveRecentChooserMenuMethod "getItems" o = Gtk.RecentChooser.RecentChooserGetItemsMethodInfo
    ResolveRecentChooserMenuMethod "getLimit" o = Gtk.RecentChooser.RecentChooserGetLimitMethodInfo
    ResolveRecentChooserMenuMethod "getLocalOnly" o = Gtk.RecentChooser.RecentChooserGetLocalOnlyMethodInfo
    ResolveRecentChooserMenuMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveRecentChooserMenuMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveRecentChooserMenuMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveRecentChooserMenuMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveRecentChooserMenuMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveRecentChooserMenuMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveRecentChooserMenuMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveRecentChooserMenuMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveRecentChooserMenuMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveRecentChooserMenuMethod "getMonitor" o = Gtk.Menu.MenuGetMonitorMethodInfo
    ResolveRecentChooserMenuMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveRecentChooserMenuMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveRecentChooserMenuMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveRecentChooserMenuMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveRecentChooserMenuMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveRecentChooserMenuMethod "getParentShell" o = Gtk.MenuShell.MenuShellGetParentShellMethodInfo
    ResolveRecentChooserMenuMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveRecentChooserMenuMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveRecentChooserMenuMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveRecentChooserMenuMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveRecentChooserMenuMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveRecentChooserMenuMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveRecentChooserMenuMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveRecentChooserMenuMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveRecentChooserMenuMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveRecentChooserMenuMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveRecentChooserMenuMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveRecentChooserMenuMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveRecentChooserMenuMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveRecentChooserMenuMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveRecentChooserMenuMethod "getRelatedAction" o = Gtk.Activatable.ActivatableGetRelatedActionMethodInfo
    ResolveRecentChooserMenuMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveRecentChooserMenuMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveRecentChooserMenuMethod "getReserveToggleSize" o = Gtk.Menu.MenuGetReserveToggleSizeMethodInfo
    ResolveRecentChooserMenuMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveRecentChooserMenuMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveRecentChooserMenuMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveRecentChooserMenuMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveRecentChooserMenuMethod "getSelectMultiple" o = Gtk.RecentChooser.RecentChooserGetSelectMultipleMethodInfo
    ResolveRecentChooserMenuMethod "getSelectedItem" o = Gtk.MenuShell.MenuShellGetSelectedItemMethodInfo
    ResolveRecentChooserMenuMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveRecentChooserMenuMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveRecentChooserMenuMethod "getShowIcons" o = Gtk.RecentChooser.RecentChooserGetShowIconsMethodInfo
    ResolveRecentChooserMenuMethod "getShowNotFound" o = Gtk.RecentChooser.RecentChooserGetShowNotFoundMethodInfo
    ResolveRecentChooserMenuMethod "getShowNumbers" o = RecentChooserMenuGetShowNumbersMethodInfo
    ResolveRecentChooserMenuMethod "getShowPrivate" o = Gtk.RecentChooser.RecentChooserGetShowPrivateMethodInfo
    ResolveRecentChooserMenuMethod "getShowTips" o = Gtk.RecentChooser.RecentChooserGetShowTipsMethodInfo
    ResolveRecentChooserMenuMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveRecentChooserMenuMethod "getSortType" o = Gtk.RecentChooser.RecentChooserGetSortTypeMethodInfo
    ResolveRecentChooserMenuMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveRecentChooserMenuMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveRecentChooserMenuMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveRecentChooserMenuMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveRecentChooserMenuMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveRecentChooserMenuMethod "getTakeFocus" o = Gtk.MenuShell.MenuShellGetTakeFocusMethodInfo
    ResolveRecentChooserMenuMethod "getTearoffState" o = Gtk.Menu.MenuGetTearoffStateMethodInfo
    ResolveRecentChooserMenuMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveRecentChooserMenuMethod "getTitle" o = Gtk.Menu.MenuGetTitleMethodInfo
    ResolveRecentChooserMenuMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveRecentChooserMenuMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveRecentChooserMenuMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveRecentChooserMenuMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveRecentChooserMenuMethod "getUris" o = Gtk.RecentChooser.RecentChooserGetUrisMethodInfo
    ResolveRecentChooserMenuMethod "getUseActionAppearance" o = Gtk.Activatable.ActivatableGetUseActionAppearanceMethodInfo
    ResolveRecentChooserMenuMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveRecentChooserMenuMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveRecentChooserMenuMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveRecentChooserMenuMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveRecentChooserMenuMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveRecentChooserMenuMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveRecentChooserMenuMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveRecentChooserMenuMethod "setAccelGroup" o = Gtk.Menu.MenuSetAccelGroupMethodInfo
    ResolveRecentChooserMenuMethod "setAccelPath" o = Gtk.Menu.MenuSetAccelPathMethodInfo
    ResolveRecentChooserMenuMethod "setActive" o = Gtk.Menu.MenuSetActiveMethodInfo
    ResolveRecentChooserMenuMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveRecentChooserMenuMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveRecentChooserMenuMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveRecentChooserMenuMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveRecentChooserMenuMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveRecentChooserMenuMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveRecentChooserMenuMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveRecentChooserMenuMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveRecentChooserMenuMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveRecentChooserMenuMethod "setCurrentUri" o = Gtk.RecentChooser.RecentChooserSetCurrentUriMethodInfo
    ResolveRecentChooserMenuMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveRecentChooserMenuMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveRecentChooserMenuMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveRecentChooserMenuMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveRecentChooserMenuMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveRecentChooserMenuMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveRecentChooserMenuMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveRecentChooserMenuMethod "setFilter" o = Gtk.RecentChooser.RecentChooserSetFilterMethodInfo
    ResolveRecentChooserMenuMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveRecentChooserMenuMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveRecentChooserMenuMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveRecentChooserMenuMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveRecentChooserMenuMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveRecentChooserMenuMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveRecentChooserMenuMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveRecentChooserMenuMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveRecentChooserMenuMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveRecentChooserMenuMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveRecentChooserMenuMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveRecentChooserMenuMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveRecentChooserMenuMethod "setLimit" o = Gtk.RecentChooser.RecentChooserSetLimitMethodInfo
    ResolveRecentChooserMenuMethod "setLocalOnly" o = Gtk.RecentChooser.RecentChooserSetLocalOnlyMethodInfo
    ResolveRecentChooserMenuMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveRecentChooserMenuMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveRecentChooserMenuMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveRecentChooserMenuMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveRecentChooserMenuMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveRecentChooserMenuMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveRecentChooserMenuMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveRecentChooserMenuMethod "setMonitor" o = Gtk.Menu.MenuSetMonitorMethodInfo
    ResolveRecentChooserMenuMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveRecentChooserMenuMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveRecentChooserMenuMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveRecentChooserMenuMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveRecentChooserMenuMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveRecentChooserMenuMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveRecentChooserMenuMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveRecentChooserMenuMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveRecentChooserMenuMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveRecentChooserMenuMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveRecentChooserMenuMethod "setRelatedAction" o = Gtk.Activatable.ActivatableSetRelatedActionMethodInfo
    ResolveRecentChooserMenuMethod "setReserveToggleSize" o = Gtk.Menu.MenuSetReserveToggleSizeMethodInfo
    ResolveRecentChooserMenuMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveRecentChooserMenuMethod "setScreen" o = Gtk.Menu.MenuSetScreenMethodInfo
    ResolveRecentChooserMenuMethod "setSelectMultiple" o = Gtk.RecentChooser.RecentChooserSetSelectMultipleMethodInfo
    ResolveRecentChooserMenuMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveRecentChooserMenuMethod "setShowIcons" o = Gtk.RecentChooser.RecentChooserSetShowIconsMethodInfo
    ResolveRecentChooserMenuMethod "setShowNotFound" o = Gtk.RecentChooser.RecentChooserSetShowNotFoundMethodInfo
    ResolveRecentChooserMenuMethod "setShowNumbers" o = RecentChooserMenuSetShowNumbersMethodInfo
    ResolveRecentChooserMenuMethod "setShowPrivate" o = Gtk.RecentChooser.RecentChooserSetShowPrivateMethodInfo
    ResolveRecentChooserMenuMethod "setShowTips" o = Gtk.RecentChooser.RecentChooserSetShowTipsMethodInfo
    ResolveRecentChooserMenuMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveRecentChooserMenuMethod "setSortFunc" o = Gtk.RecentChooser.RecentChooserSetSortFuncMethodInfo
    ResolveRecentChooserMenuMethod "setSortType" o = Gtk.RecentChooser.RecentChooserSetSortTypeMethodInfo
    ResolveRecentChooserMenuMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveRecentChooserMenuMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveRecentChooserMenuMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveRecentChooserMenuMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveRecentChooserMenuMethod "setTakeFocus" o = Gtk.MenuShell.MenuShellSetTakeFocusMethodInfo
    ResolveRecentChooserMenuMethod "setTearoffState" o = Gtk.Menu.MenuSetTearoffStateMethodInfo
    ResolveRecentChooserMenuMethod "setTitle" o = Gtk.Menu.MenuSetTitleMethodInfo
    ResolveRecentChooserMenuMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveRecentChooserMenuMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveRecentChooserMenuMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveRecentChooserMenuMethod "setUseActionAppearance" o = Gtk.Activatable.ActivatableSetUseActionAppearanceMethodInfo
    ResolveRecentChooserMenuMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveRecentChooserMenuMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveRecentChooserMenuMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveRecentChooserMenuMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveRecentChooserMenuMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveRecentChooserMenuMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveRecentChooserMenuMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveRecentChooserMenuMethod t RecentChooserMenu, O.OverloadedMethod info RecentChooserMenu p) => OL.IsLabel t (RecentChooserMenu -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveRecentChooserMenuMethod t RecentChooserMenu, O.OverloadedMethod info RecentChooserMenu p, R.HasField t RecentChooserMenu p) => R.HasField t RecentChooserMenu p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveRecentChooserMenuMethod t RecentChooserMenu, O.OverloadedMethodInfo info RecentChooserMenu) => OL.IsLabel t (O.MethodProxy info RecentChooserMenu) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- VVV Prop "show-numbers"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@show-numbers@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' recentChooserMenu #showNumbers
-- @
getRecentChooserMenuShowNumbers :: (MonadIO m, IsRecentChooserMenu o) => o -> m Bool
getRecentChooserMenuShowNumbers obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "show-numbers"

-- | Set the value of the “@show-numbers@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' recentChooserMenu [ #showNumbers 'Data.GI.Base.Attributes.:=' value ]
-- @
setRecentChooserMenuShowNumbers :: (MonadIO m, IsRecentChooserMenu o) => o -> Bool -> m ()
setRecentChooserMenuShowNumbers obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "show-numbers" val

-- | Construct a `GValueConstruct` with valid value for the “@show-numbers@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructRecentChooserMenuShowNumbers :: (IsRecentChooserMenu o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructRecentChooserMenuShowNumbers val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "show-numbers" val

#if defined(ENABLE_OVERLOADING)
data RecentChooserMenuShowNumbersPropertyInfo
instance AttrInfo RecentChooserMenuShowNumbersPropertyInfo where
    type AttrAllowedOps RecentChooserMenuShowNumbersPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint RecentChooserMenuShowNumbersPropertyInfo = IsRecentChooserMenu
    type AttrSetTypeConstraint RecentChooserMenuShowNumbersPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint RecentChooserMenuShowNumbersPropertyInfo = (~) Bool
    type AttrTransferType RecentChooserMenuShowNumbersPropertyInfo = Bool
    type AttrGetType RecentChooserMenuShowNumbersPropertyInfo = Bool
    type AttrLabel RecentChooserMenuShowNumbersPropertyInfo = "show-numbers"
    type AttrOrigin RecentChooserMenuShowNumbersPropertyInfo = RecentChooserMenu
    attrGet = getRecentChooserMenuShowNumbers
    attrSet = setRecentChooserMenuShowNumbers
    attrTransfer _ v = do
        return v
    attrConstruct = constructRecentChooserMenuShowNumbers
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.RecentChooserMenu.showNumbers"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-RecentChooserMenu.html#g:attr:showNumbers"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList RecentChooserMenu
type instance O.AttributeList RecentChooserMenu = RecentChooserMenuAttributeList
type RecentChooserMenuAttributeList = ('[ '("accelGroup", Gtk.Menu.MenuAccelGroupPropertyInfo), '("accelPath", Gtk.Menu.MenuAccelPathPropertyInfo), '("active", Gtk.Menu.MenuActivePropertyInfo), '("anchorHints", Gtk.Menu.MenuAnchorHintsPropertyInfo), '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("attachWidget", Gtk.Menu.MenuAttachWidgetPropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("filter", Gtk.RecentChooser.RecentChooserFilterPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("limit", Gtk.RecentChooser.RecentChooserLimitPropertyInfo), '("localOnly", Gtk.RecentChooser.RecentChooserLocalOnlyPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("menuTypeHint", Gtk.Menu.MenuMenuTypeHintPropertyInfo), '("monitor", Gtk.Menu.MenuMonitorPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("recentManager", Gtk.RecentChooser.RecentChooserRecentManagerPropertyInfo), '("rectAnchorDx", Gtk.Menu.MenuRectAnchorDxPropertyInfo), '("rectAnchorDy", Gtk.Menu.MenuRectAnchorDyPropertyInfo), '("relatedAction", Gtk.Activatable.ActivatableRelatedActionPropertyInfo), '("reserveToggleSize", Gtk.Menu.MenuReserveToggleSizePropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("selectMultiple", Gtk.RecentChooser.RecentChooserSelectMultiplePropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("showIcons", Gtk.RecentChooser.RecentChooserShowIconsPropertyInfo), '("showNotFound", Gtk.RecentChooser.RecentChooserShowNotFoundPropertyInfo), '("showNumbers", RecentChooserMenuShowNumbersPropertyInfo), '("showPrivate", Gtk.RecentChooser.RecentChooserShowPrivatePropertyInfo), '("showTips", Gtk.RecentChooser.RecentChooserShowTipsPropertyInfo), '("sortType", Gtk.RecentChooser.RecentChooserSortTypePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("takeFocus", Gtk.MenuShell.MenuShellTakeFocusPropertyInfo), '("tearoffState", Gtk.Menu.MenuTearoffStatePropertyInfo), '("tearoffTitle", Gtk.Menu.MenuTearoffTitlePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("useActionAppearance", Gtk.Activatable.ActivatableUseActionAppearancePropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
recentChooserMenuShowNumbers :: AttrLabelProxy "showNumbers"
recentChooserMenuShowNumbers = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList RecentChooserMenu = RecentChooserMenuSignalList
type RecentChooserMenuSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("activateCurrent", Gtk.MenuShell.MenuShellActivateCurrentSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("cancel", Gtk.MenuShell.MenuShellCancelSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("cycleFocus", Gtk.MenuShell.MenuShellCycleFocusSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deactivate", Gtk.MenuShell.MenuShellDeactivateSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("insert", Gtk.MenuShell.MenuShellInsertSignalInfo), '("itemActivated", Gtk.RecentChooser.RecentChooserItemActivatedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveCurrent", Gtk.MenuShell.MenuShellMoveCurrentSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("moveScroll", Gtk.Menu.MenuMoveScrollSignalInfo), '("moveSelected", Gtk.MenuShell.MenuShellMoveSelectedSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("poppedUp", Gtk.Menu.MenuPoppedUpSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionChanged", Gtk.RecentChooser.RecentChooserSelectionChangedSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionDone", Gtk.MenuShell.MenuShellSelectionDoneSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method RecentChooserMenu::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just
--               (TInterface
--                  Name { namespace = "Gtk" , name = "RecentChooserMenu" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_chooser_menu_new" gtk_recent_chooser_menu_new :: 
    IO (Ptr RecentChooserMenu)

-- | Creates a new t'GI.Gtk.Objects.RecentChooserMenu.RecentChooserMenu' widget.
-- 
-- This kind of widget shows the list of recently used resources as
-- a menu, each item as a menu item.  Each item inside the menu might
-- have an icon, representing its MIME type, and a number, for mnemonic
-- access.
-- 
-- This widget implements the t'GI.Gtk.Interfaces.RecentChooser.RecentChooser' interface.
-- 
-- This widget creates its own t'GI.Gtk.Objects.RecentManager.RecentManager' object.  See the
-- 'GI.Gtk.Objects.RecentChooserMenu.recentChooserMenuNewForManager' function to know how to create
-- a t'GI.Gtk.Objects.RecentChooserMenu.RecentChooserMenu' widget bound to another t'GI.Gtk.Objects.RecentManager.RecentManager' object.
-- 
-- /Since: 2.10/
recentChooserMenuNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m RecentChooserMenu
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.RecentChooserMenu.RecentChooserMenu'
recentChooserMenuNew  = liftIO $ do
    result <- gtk_recent_chooser_menu_new
    checkUnexpectedReturnNULL "recentChooserMenuNew" result
    result' <- (newObject RecentChooserMenu) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method RecentChooserMenu::new_for_manager
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "manager"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentManager" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentManager"
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
--               (TInterface
--                  Name { namespace = "Gtk" , name = "RecentChooserMenu" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_chooser_menu_new_for_manager" gtk_recent_chooser_menu_new_for_manager :: 
    Ptr Gtk.RecentManager.RecentManager ->  -- manager : TInterface (Name {namespace = "Gtk", name = "RecentManager"})
    IO (Ptr RecentChooserMenu)

-- | Creates a new t'GI.Gtk.Objects.RecentChooserMenu.RecentChooserMenu' widget using /@manager@/ as
-- the underlying recently used resources manager.
-- 
-- This is useful if you have implemented your own recent manager,
-- or if you have a customized instance of a t'GI.Gtk.Objects.RecentManager.RecentManager'
-- object or if you wish to share a common t'GI.Gtk.Objects.RecentManager.RecentManager' object
-- among multiple t'GI.Gtk.Interfaces.RecentChooser.RecentChooser' widgets.
-- 
-- /Since: 2.10/
recentChooserMenuNewForManager ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.RecentManager.IsRecentManager a) =>
    a
    -- ^ /@manager@/: a t'GI.Gtk.Objects.RecentManager.RecentManager'
    -> m RecentChooserMenu
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.RecentChooserMenu.RecentChooserMenu', bound to /@manager@/.
recentChooserMenuNewForManager manager = liftIO $ do
    manager' <- unsafeManagedPtrCastPtr manager
    result <- gtk_recent_chooser_menu_new_for_manager manager'
    checkUnexpectedReturnNULL "recentChooserMenuNewForManager" result
    result' <- (newObject RecentChooserMenu) result
    touchManagedPtr manager
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method RecentChooserMenu::get_show_numbers
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentChooserMenu" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentChooserMenu"
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

foreign import ccall "gtk_recent_chooser_menu_get_show_numbers" gtk_recent_chooser_menu_get_show_numbers :: 
    Ptr RecentChooserMenu ->                -- menu : TInterface (Name {namespace = "Gtk", name = "RecentChooserMenu"})
    IO CInt

-- | Returns the value set by 'GI.Gtk.Objects.RecentChooserMenu.recentChooserMenuSetShowNumbers'.
-- 
-- /Since: 2.10/
recentChooserMenuGetShowNumbers ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentChooserMenu a) =>
    a
    -- ^ /@menu@/: a t'GI.Gtk.Objects.RecentChooserMenu.RecentChooserMenu'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if numbers should be shown.
recentChooserMenuGetShowNumbers menu = liftIO $ do
    menu' <- unsafeManagedPtrCastPtr menu
    result <- gtk_recent_chooser_menu_get_show_numbers menu'
    let result' = (/= 0) result
    touchManagedPtr menu
    return result'

#if defined(ENABLE_OVERLOADING)
data RecentChooserMenuGetShowNumbersMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsRecentChooserMenu a) => O.OverloadedMethod RecentChooserMenuGetShowNumbersMethodInfo a signature where
    overloadedMethod = recentChooserMenuGetShowNumbers

instance O.OverloadedMethodInfo RecentChooserMenuGetShowNumbersMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.RecentChooserMenu.recentChooserMenuGetShowNumbers",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-RecentChooserMenu.html#v:recentChooserMenuGetShowNumbers"
        })


#endif

-- method RecentChooserMenu::set_show_numbers
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "menu"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentChooserMenu" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentChooserMenu"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "show_numbers"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "whether to show numbers"
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

foreign import ccall "gtk_recent_chooser_menu_set_show_numbers" gtk_recent_chooser_menu_set_show_numbers :: 
    Ptr RecentChooserMenu ->                -- menu : TInterface (Name {namespace = "Gtk", name = "RecentChooserMenu"})
    CInt ->                                 -- show_numbers : TBasicType TBoolean
    IO ()

-- | Sets whether a number should be added to the items of /@menu@/.  The
-- numbers are shown to provide a unique character for a mnemonic to
-- be used inside ten menu item’s label.  Only the first the items
-- get a number to avoid clashes.
-- 
-- /Since: 2.10/
recentChooserMenuSetShowNumbers ::
    (B.CallStack.HasCallStack, MonadIO m, IsRecentChooserMenu a) =>
    a
    -- ^ /@menu@/: a t'GI.Gtk.Objects.RecentChooserMenu.RecentChooserMenu'
    -> Bool
    -- ^ /@showNumbers@/: whether to show numbers
    -> m ()
recentChooserMenuSetShowNumbers menu showNumbers = liftIO $ do
    menu' <- unsafeManagedPtrCastPtr menu
    let showNumbers' = (fromIntegral . fromEnum) showNumbers
    gtk_recent_chooser_menu_set_show_numbers menu' showNumbers'
    touchManagedPtr menu
    return ()

#if defined(ENABLE_OVERLOADING)
data RecentChooserMenuSetShowNumbersMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsRecentChooserMenu a) => O.OverloadedMethod RecentChooserMenuSetShowNumbersMethodInfo a signature where
    overloadedMethod = recentChooserMenuSetShowNumbers

instance O.OverloadedMethodInfo RecentChooserMenuSetShowNumbersMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.RecentChooserMenu.recentChooserMenuSetShowNumbers",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-RecentChooserMenu.html#v:recentChooserMenuSetShowNumbers"
        })


#endif


