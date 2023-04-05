{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Objects.RecentChooserWidget.RecentChooserWidget' is a widget suitable for selecting recently used
-- files.  It is the main building block of a t'GI.Gtk.Objects.RecentChooserDialog.RecentChooserDialog'.  Most
-- applications will only need to use the latter; you can use
-- t'GI.Gtk.Objects.RecentChooserWidget.RecentChooserWidget' as part of a larger window if you have special needs.
-- 
-- Note that t'GI.Gtk.Objects.RecentChooserWidget.RecentChooserWidget' does not have any methods of its own.
-- Instead, you should use the functions that work on a t'GI.Gtk.Interfaces.RecentChooser.RecentChooser'.
-- 
-- Recently used files are supported since GTK+ 2.10.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.RecentChooserWidget
    ( 

-- * Exported types
    RecentChooserWidget(..)                 ,
    IsRecentChooserWidget                   ,
    toRecentChooserWidget                   ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addFilter]("GI.Gtk.Interfaces.RecentChooser#g:method:addFilter"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listFilters]("GI.Gtk.Interfaces.RecentChooser#g:method:listFilters"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [packEnd]("GI.Gtk.Objects.Box#g:method:packEnd"), [packStart]("GI.Gtk.Objects.Box#g:method:packStart"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [queryChildPacking]("GI.Gtk.Objects.Box#g:method:queryChildPacking"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeFilter]("GI.Gtk.Interfaces.RecentChooser#g:method:removeFilter"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reorderChild]("GI.Gtk.Objects.Box#g:method:reorderChild"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [selectAll]("GI.Gtk.Interfaces.RecentChooser#g:method:selectAll"), [selectUri]("GI.Gtk.Interfaces.RecentChooser#g:method:selectUri"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unselectAll]("GI.Gtk.Interfaces.RecentChooser#g:method:unselectAll"), [unselectUri]("GI.Gtk.Interfaces.RecentChooser#g:method:unselectUri"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getBaselinePosition]("GI.Gtk.Objects.Box#g:method:getBaselinePosition"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getCenterWidget]("GI.Gtk.Objects.Box#g:method:getCenterWidget"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getCurrentItem]("GI.Gtk.Interfaces.RecentChooser#g:method:getCurrentItem"), [getCurrentUri]("GI.Gtk.Interfaces.RecentChooser#g:method:getCurrentUri"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFilter]("GI.Gtk.Interfaces.RecentChooser#g:method:getFilter"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getHomogeneous]("GI.Gtk.Objects.Box#g:method:getHomogeneous"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getItems]("GI.Gtk.Interfaces.RecentChooser#g:method:getItems"), [getLimit]("GI.Gtk.Interfaces.RecentChooser#g:method:getLimit"), [getLocalOnly]("GI.Gtk.Interfaces.RecentChooser#g:method:getLocalOnly"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getOrientation]("GI.Gtk.Interfaces.Orientable#g:method:getOrientation"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSelectMultiple]("GI.Gtk.Interfaces.RecentChooser#g:method:getSelectMultiple"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getShowIcons]("GI.Gtk.Interfaces.RecentChooser#g:method:getShowIcons"), [getShowNotFound]("GI.Gtk.Interfaces.RecentChooser#g:method:getShowNotFound"), [getShowPrivate]("GI.Gtk.Interfaces.RecentChooser#g:method:getShowPrivate"), [getShowTips]("GI.Gtk.Interfaces.RecentChooser#g:method:getShowTips"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getSortType]("GI.Gtk.Interfaces.RecentChooser#g:method:getSortType"), [getSpacing]("GI.Gtk.Objects.Box#g:method:getSpacing"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getUris]("GI.Gtk.Interfaces.RecentChooser#g:method:getUris"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBaselinePosition]("GI.Gtk.Objects.Box#g:method:setBaselinePosition"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setCenterWidget]("GI.Gtk.Objects.Box#g:method:setCenterWidget"), [setChildPacking]("GI.Gtk.Objects.Box#g:method:setChildPacking"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setCurrentUri]("GI.Gtk.Interfaces.RecentChooser#g:method:setCurrentUri"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFilter]("GI.Gtk.Interfaces.RecentChooser#g:method:setFilter"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setHomogeneous]("GI.Gtk.Objects.Box#g:method:setHomogeneous"), [setLimit]("GI.Gtk.Interfaces.RecentChooser#g:method:setLimit"), [setLocalOnly]("GI.Gtk.Interfaces.RecentChooser#g:method:setLocalOnly"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setOrientation]("GI.Gtk.Interfaces.Orientable#g:method:setOrientation"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setSelectMultiple]("GI.Gtk.Interfaces.RecentChooser#g:method:setSelectMultiple"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setShowIcons]("GI.Gtk.Interfaces.RecentChooser#g:method:setShowIcons"), [setShowNotFound]("GI.Gtk.Interfaces.RecentChooser#g:method:setShowNotFound"), [setShowPrivate]("GI.Gtk.Interfaces.RecentChooser#g:method:setShowPrivate"), [setShowTips]("GI.Gtk.Interfaces.RecentChooser#g:method:setShowTips"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setSortFunc]("GI.Gtk.Interfaces.RecentChooser#g:method:setSortFunc"), [setSortType]("GI.Gtk.Interfaces.RecentChooser#g:method:setSortType"), [setSpacing]("GI.Gtk.Objects.Box#g:method:setSpacing"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveRecentChooserWidgetMethod        ,
#endif

-- ** new #method:new#

    recentChooserWidgetNew                  ,


-- ** newForManager #method:newForManager#

    recentChooserWidgetNewForManager        ,




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
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Orientable as Gtk.Orientable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.RecentChooser as Gtk.RecentChooser
import {-# SOURCE #-} qualified GI.Gtk.Objects.Box as Gtk.Box
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.RecentManager as Gtk.RecentManager
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype RecentChooserWidget = RecentChooserWidget (SP.ManagedPtr RecentChooserWidget)
    deriving (Eq)

instance SP.ManagedPtrNewtype RecentChooserWidget where
    toManagedPtr (RecentChooserWidget p) = p

foreign import ccall "gtk_recent_chooser_widget_get_type"
    c_gtk_recent_chooser_widget_get_type :: IO B.Types.GType

instance B.Types.TypedObject RecentChooserWidget where
    glibType = c_gtk_recent_chooser_widget_get_type

instance B.Types.GObject RecentChooserWidget

-- | Type class for types which can be safely cast to `RecentChooserWidget`, for instance with `toRecentChooserWidget`.
class (SP.GObject o, O.IsDescendantOf RecentChooserWidget o) => IsRecentChooserWidget o
instance (SP.GObject o, O.IsDescendantOf RecentChooserWidget o) => IsRecentChooserWidget o

instance O.HasParentTypes RecentChooserWidget
type instance O.ParentTypes RecentChooserWidget = '[Gtk.Box.Box, Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable, Gtk.Orientable.Orientable, Gtk.RecentChooser.RecentChooser]

-- | Cast to `RecentChooserWidget`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toRecentChooserWidget :: (MIO.MonadIO m, IsRecentChooserWidget o) => o -> m RecentChooserWidget
toRecentChooserWidget = MIO.liftIO . B.ManagedPtr.unsafeCastTo RecentChooserWidget

-- | Convert 'RecentChooserWidget' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe RecentChooserWidget) where
    gvalueGType_ = c_gtk_recent_chooser_widget_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr RecentChooserWidget)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr RecentChooserWidget)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject RecentChooserWidget ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveRecentChooserWidgetMethod (t :: Symbol) (o :: *) :: * where
    ResolveRecentChooserWidgetMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveRecentChooserWidgetMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveRecentChooserWidgetMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveRecentChooserWidgetMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveRecentChooserWidgetMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveRecentChooserWidgetMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveRecentChooserWidgetMethod "addFilter" o = Gtk.RecentChooser.RecentChooserAddFilterMethodInfo
    ResolveRecentChooserWidgetMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveRecentChooserWidgetMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveRecentChooserWidgetMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveRecentChooserWidgetMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveRecentChooserWidgetMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveRecentChooserWidgetMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveRecentChooserWidgetMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveRecentChooserWidgetMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveRecentChooserWidgetMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveRecentChooserWidgetMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveRecentChooserWidgetMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveRecentChooserWidgetMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveRecentChooserWidgetMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveRecentChooserWidgetMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveRecentChooserWidgetMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveRecentChooserWidgetMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveRecentChooserWidgetMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveRecentChooserWidgetMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveRecentChooserWidgetMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveRecentChooserWidgetMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveRecentChooserWidgetMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveRecentChooserWidgetMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveRecentChooserWidgetMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveRecentChooserWidgetMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveRecentChooserWidgetMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveRecentChooserWidgetMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveRecentChooserWidgetMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveRecentChooserWidgetMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveRecentChooserWidgetMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveRecentChooserWidgetMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveRecentChooserWidgetMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveRecentChooserWidgetMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveRecentChooserWidgetMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveRecentChooserWidgetMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveRecentChooserWidgetMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveRecentChooserWidgetMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveRecentChooserWidgetMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveRecentChooserWidgetMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveRecentChooserWidgetMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveRecentChooserWidgetMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveRecentChooserWidgetMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveRecentChooserWidgetMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveRecentChooserWidgetMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveRecentChooserWidgetMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveRecentChooserWidgetMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveRecentChooserWidgetMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveRecentChooserWidgetMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveRecentChooserWidgetMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveRecentChooserWidgetMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveRecentChooserWidgetMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveRecentChooserWidgetMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveRecentChooserWidgetMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveRecentChooserWidgetMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveRecentChooserWidgetMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveRecentChooserWidgetMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveRecentChooserWidgetMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveRecentChooserWidgetMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveRecentChooserWidgetMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveRecentChooserWidgetMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveRecentChooserWidgetMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveRecentChooserWidgetMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveRecentChooserWidgetMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveRecentChooserWidgetMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveRecentChooserWidgetMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveRecentChooserWidgetMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveRecentChooserWidgetMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveRecentChooserWidgetMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveRecentChooserWidgetMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveRecentChooserWidgetMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveRecentChooserWidgetMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveRecentChooserWidgetMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveRecentChooserWidgetMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveRecentChooserWidgetMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveRecentChooserWidgetMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveRecentChooserWidgetMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveRecentChooserWidgetMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveRecentChooserWidgetMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveRecentChooserWidgetMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveRecentChooserWidgetMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveRecentChooserWidgetMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveRecentChooserWidgetMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveRecentChooserWidgetMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveRecentChooserWidgetMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveRecentChooserWidgetMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveRecentChooserWidgetMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveRecentChooserWidgetMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveRecentChooserWidgetMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveRecentChooserWidgetMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveRecentChooserWidgetMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveRecentChooserWidgetMethod "listFilters" o = Gtk.RecentChooser.RecentChooserListFiltersMethodInfo
    ResolveRecentChooserWidgetMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveRecentChooserWidgetMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveRecentChooserWidgetMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveRecentChooserWidgetMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveRecentChooserWidgetMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveRecentChooserWidgetMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveRecentChooserWidgetMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveRecentChooserWidgetMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveRecentChooserWidgetMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveRecentChooserWidgetMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveRecentChooserWidgetMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveRecentChooserWidgetMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveRecentChooserWidgetMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveRecentChooserWidgetMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveRecentChooserWidgetMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveRecentChooserWidgetMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveRecentChooserWidgetMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveRecentChooserWidgetMethod "packEnd" o = Gtk.Box.BoxPackEndMethodInfo
    ResolveRecentChooserWidgetMethod "packStart" o = Gtk.Box.BoxPackStartMethodInfo
    ResolveRecentChooserWidgetMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveRecentChooserWidgetMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveRecentChooserWidgetMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveRecentChooserWidgetMethod "queryChildPacking" o = Gtk.Box.BoxQueryChildPackingMethodInfo
    ResolveRecentChooserWidgetMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveRecentChooserWidgetMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveRecentChooserWidgetMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveRecentChooserWidgetMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveRecentChooserWidgetMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveRecentChooserWidgetMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveRecentChooserWidgetMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveRecentChooserWidgetMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveRecentChooserWidgetMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveRecentChooserWidgetMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveRecentChooserWidgetMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveRecentChooserWidgetMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveRecentChooserWidgetMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolveRecentChooserWidgetMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveRecentChooserWidgetMethod "removeFilter" o = Gtk.RecentChooser.RecentChooserRemoveFilterMethodInfo
    ResolveRecentChooserWidgetMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveRecentChooserWidgetMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveRecentChooserWidgetMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveRecentChooserWidgetMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveRecentChooserWidgetMethod "reorderChild" o = Gtk.Box.BoxReorderChildMethodInfo
    ResolveRecentChooserWidgetMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveRecentChooserWidgetMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveRecentChooserWidgetMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveRecentChooserWidgetMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveRecentChooserWidgetMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveRecentChooserWidgetMethod "selectAll" o = Gtk.RecentChooser.RecentChooserSelectAllMethodInfo
    ResolveRecentChooserWidgetMethod "selectUri" o = Gtk.RecentChooser.RecentChooserSelectUriMethodInfo
    ResolveRecentChooserWidgetMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveRecentChooserWidgetMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveRecentChooserWidgetMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveRecentChooserWidgetMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveRecentChooserWidgetMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveRecentChooserWidgetMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveRecentChooserWidgetMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveRecentChooserWidgetMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveRecentChooserWidgetMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveRecentChooserWidgetMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveRecentChooserWidgetMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveRecentChooserWidgetMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveRecentChooserWidgetMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveRecentChooserWidgetMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveRecentChooserWidgetMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveRecentChooserWidgetMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveRecentChooserWidgetMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveRecentChooserWidgetMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveRecentChooserWidgetMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveRecentChooserWidgetMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveRecentChooserWidgetMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveRecentChooserWidgetMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveRecentChooserWidgetMethod "unselectAll" o = Gtk.RecentChooser.RecentChooserUnselectAllMethodInfo
    ResolveRecentChooserWidgetMethod "unselectUri" o = Gtk.RecentChooser.RecentChooserUnselectUriMethodInfo
    ResolveRecentChooserWidgetMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveRecentChooserWidgetMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveRecentChooserWidgetMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveRecentChooserWidgetMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveRecentChooserWidgetMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveRecentChooserWidgetMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveRecentChooserWidgetMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveRecentChooserWidgetMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveRecentChooserWidgetMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveRecentChooserWidgetMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveRecentChooserWidgetMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveRecentChooserWidgetMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveRecentChooserWidgetMethod "getBaselinePosition" o = Gtk.Box.BoxGetBaselinePositionMethodInfo
    ResolveRecentChooserWidgetMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveRecentChooserWidgetMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveRecentChooserWidgetMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveRecentChooserWidgetMethod "getCenterWidget" o = Gtk.Box.BoxGetCenterWidgetMethodInfo
    ResolveRecentChooserWidgetMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveRecentChooserWidgetMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveRecentChooserWidgetMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveRecentChooserWidgetMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveRecentChooserWidgetMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveRecentChooserWidgetMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveRecentChooserWidgetMethod "getCurrentItem" o = Gtk.RecentChooser.RecentChooserGetCurrentItemMethodInfo
    ResolveRecentChooserWidgetMethod "getCurrentUri" o = Gtk.RecentChooser.RecentChooserGetCurrentUriMethodInfo
    ResolveRecentChooserWidgetMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveRecentChooserWidgetMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveRecentChooserWidgetMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveRecentChooserWidgetMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveRecentChooserWidgetMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveRecentChooserWidgetMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveRecentChooserWidgetMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveRecentChooserWidgetMethod "getFilter" o = Gtk.RecentChooser.RecentChooserGetFilterMethodInfo
    ResolveRecentChooserWidgetMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveRecentChooserWidgetMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveRecentChooserWidgetMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveRecentChooserWidgetMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveRecentChooserWidgetMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveRecentChooserWidgetMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveRecentChooserWidgetMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveRecentChooserWidgetMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveRecentChooserWidgetMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveRecentChooserWidgetMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveRecentChooserWidgetMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveRecentChooserWidgetMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveRecentChooserWidgetMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveRecentChooserWidgetMethod "getHomogeneous" o = Gtk.Box.BoxGetHomogeneousMethodInfo
    ResolveRecentChooserWidgetMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveRecentChooserWidgetMethod "getItems" o = Gtk.RecentChooser.RecentChooserGetItemsMethodInfo
    ResolveRecentChooserWidgetMethod "getLimit" o = Gtk.RecentChooser.RecentChooserGetLimitMethodInfo
    ResolveRecentChooserWidgetMethod "getLocalOnly" o = Gtk.RecentChooser.RecentChooserGetLocalOnlyMethodInfo
    ResolveRecentChooserWidgetMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveRecentChooserWidgetMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveRecentChooserWidgetMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveRecentChooserWidgetMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveRecentChooserWidgetMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveRecentChooserWidgetMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveRecentChooserWidgetMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveRecentChooserWidgetMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveRecentChooserWidgetMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveRecentChooserWidgetMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveRecentChooserWidgetMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveRecentChooserWidgetMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveRecentChooserWidgetMethod "getOrientation" o = Gtk.Orientable.OrientableGetOrientationMethodInfo
    ResolveRecentChooserWidgetMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveRecentChooserWidgetMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveRecentChooserWidgetMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveRecentChooserWidgetMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveRecentChooserWidgetMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveRecentChooserWidgetMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveRecentChooserWidgetMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveRecentChooserWidgetMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveRecentChooserWidgetMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveRecentChooserWidgetMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveRecentChooserWidgetMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveRecentChooserWidgetMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveRecentChooserWidgetMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveRecentChooserWidgetMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveRecentChooserWidgetMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveRecentChooserWidgetMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveRecentChooserWidgetMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveRecentChooserWidgetMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveRecentChooserWidgetMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveRecentChooserWidgetMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveRecentChooserWidgetMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveRecentChooserWidgetMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveRecentChooserWidgetMethod "getSelectMultiple" o = Gtk.RecentChooser.RecentChooserGetSelectMultipleMethodInfo
    ResolveRecentChooserWidgetMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveRecentChooserWidgetMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveRecentChooserWidgetMethod "getShowIcons" o = Gtk.RecentChooser.RecentChooserGetShowIconsMethodInfo
    ResolveRecentChooserWidgetMethod "getShowNotFound" o = Gtk.RecentChooser.RecentChooserGetShowNotFoundMethodInfo
    ResolveRecentChooserWidgetMethod "getShowPrivate" o = Gtk.RecentChooser.RecentChooserGetShowPrivateMethodInfo
    ResolveRecentChooserWidgetMethod "getShowTips" o = Gtk.RecentChooser.RecentChooserGetShowTipsMethodInfo
    ResolveRecentChooserWidgetMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveRecentChooserWidgetMethod "getSortType" o = Gtk.RecentChooser.RecentChooserGetSortTypeMethodInfo
    ResolveRecentChooserWidgetMethod "getSpacing" o = Gtk.Box.BoxGetSpacingMethodInfo
    ResolveRecentChooserWidgetMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveRecentChooserWidgetMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveRecentChooserWidgetMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveRecentChooserWidgetMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveRecentChooserWidgetMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveRecentChooserWidgetMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveRecentChooserWidgetMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveRecentChooserWidgetMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveRecentChooserWidgetMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveRecentChooserWidgetMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveRecentChooserWidgetMethod "getUris" o = Gtk.RecentChooser.RecentChooserGetUrisMethodInfo
    ResolveRecentChooserWidgetMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveRecentChooserWidgetMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveRecentChooserWidgetMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveRecentChooserWidgetMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveRecentChooserWidgetMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveRecentChooserWidgetMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveRecentChooserWidgetMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveRecentChooserWidgetMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveRecentChooserWidgetMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveRecentChooserWidgetMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveRecentChooserWidgetMethod "setBaselinePosition" o = Gtk.Box.BoxSetBaselinePositionMethodInfo
    ResolveRecentChooserWidgetMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveRecentChooserWidgetMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveRecentChooserWidgetMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveRecentChooserWidgetMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveRecentChooserWidgetMethod "setCenterWidget" o = Gtk.Box.BoxSetCenterWidgetMethodInfo
    ResolveRecentChooserWidgetMethod "setChildPacking" o = Gtk.Box.BoxSetChildPackingMethodInfo
    ResolveRecentChooserWidgetMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveRecentChooserWidgetMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveRecentChooserWidgetMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveRecentChooserWidgetMethod "setCurrentUri" o = Gtk.RecentChooser.RecentChooserSetCurrentUriMethodInfo
    ResolveRecentChooserWidgetMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveRecentChooserWidgetMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveRecentChooserWidgetMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveRecentChooserWidgetMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveRecentChooserWidgetMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveRecentChooserWidgetMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveRecentChooserWidgetMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveRecentChooserWidgetMethod "setFilter" o = Gtk.RecentChooser.RecentChooserSetFilterMethodInfo
    ResolveRecentChooserWidgetMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveRecentChooserWidgetMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveRecentChooserWidgetMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveRecentChooserWidgetMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveRecentChooserWidgetMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveRecentChooserWidgetMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveRecentChooserWidgetMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveRecentChooserWidgetMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveRecentChooserWidgetMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveRecentChooserWidgetMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveRecentChooserWidgetMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveRecentChooserWidgetMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveRecentChooserWidgetMethod "setHomogeneous" o = Gtk.Box.BoxSetHomogeneousMethodInfo
    ResolveRecentChooserWidgetMethod "setLimit" o = Gtk.RecentChooser.RecentChooserSetLimitMethodInfo
    ResolveRecentChooserWidgetMethod "setLocalOnly" o = Gtk.RecentChooser.RecentChooserSetLocalOnlyMethodInfo
    ResolveRecentChooserWidgetMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveRecentChooserWidgetMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveRecentChooserWidgetMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveRecentChooserWidgetMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveRecentChooserWidgetMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveRecentChooserWidgetMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveRecentChooserWidgetMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveRecentChooserWidgetMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveRecentChooserWidgetMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveRecentChooserWidgetMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveRecentChooserWidgetMethod "setOrientation" o = Gtk.Orientable.OrientableSetOrientationMethodInfo
    ResolveRecentChooserWidgetMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveRecentChooserWidgetMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveRecentChooserWidgetMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveRecentChooserWidgetMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveRecentChooserWidgetMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveRecentChooserWidgetMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveRecentChooserWidgetMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveRecentChooserWidgetMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveRecentChooserWidgetMethod "setSelectMultiple" o = Gtk.RecentChooser.RecentChooserSetSelectMultipleMethodInfo
    ResolveRecentChooserWidgetMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveRecentChooserWidgetMethod "setShowIcons" o = Gtk.RecentChooser.RecentChooserSetShowIconsMethodInfo
    ResolveRecentChooserWidgetMethod "setShowNotFound" o = Gtk.RecentChooser.RecentChooserSetShowNotFoundMethodInfo
    ResolveRecentChooserWidgetMethod "setShowPrivate" o = Gtk.RecentChooser.RecentChooserSetShowPrivateMethodInfo
    ResolveRecentChooserWidgetMethod "setShowTips" o = Gtk.RecentChooser.RecentChooserSetShowTipsMethodInfo
    ResolveRecentChooserWidgetMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveRecentChooserWidgetMethod "setSortFunc" o = Gtk.RecentChooser.RecentChooserSetSortFuncMethodInfo
    ResolveRecentChooserWidgetMethod "setSortType" o = Gtk.RecentChooser.RecentChooserSetSortTypeMethodInfo
    ResolveRecentChooserWidgetMethod "setSpacing" o = Gtk.Box.BoxSetSpacingMethodInfo
    ResolveRecentChooserWidgetMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveRecentChooserWidgetMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveRecentChooserWidgetMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveRecentChooserWidgetMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveRecentChooserWidgetMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveRecentChooserWidgetMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveRecentChooserWidgetMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveRecentChooserWidgetMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveRecentChooserWidgetMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveRecentChooserWidgetMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveRecentChooserWidgetMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveRecentChooserWidgetMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveRecentChooserWidgetMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveRecentChooserWidgetMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveRecentChooserWidgetMethod t RecentChooserWidget, O.OverloadedMethod info RecentChooserWidget p) => OL.IsLabel t (RecentChooserWidget -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveRecentChooserWidgetMethod t RecentChooserWidget, O.OverloadedMethod info RecentChooserWidget p, R.HasField t RecentChooserWidget p) => R.HasField t RecentChooserWidget p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveRecentChooserWidgetMethod t RecentChooserWidget, O.OverloadedMethodInfo info RecentChooserWidget) => OL.IsLabel t (O.MethodProxy info RecentChooserWidget) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList RecentChooserWidget
type instance O.AttributeList RecentChooserWidget = RecentChooserWidgetAttributeList
type RecentChooserWidgetAttributeList = ('[ '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("baselinePosition", Gtk.Box.BoxBaselinePositionPropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("filter", Gtk.RecentChooser.RecentChooserFilterPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("homogeneous", Gtk.Box.BoxHomogeneousPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("limit", Gtk.RecentChooser.RecentChooserLimitPropertyInfo), '("localOnly", Gtk.RecentChooser.RecentChooserLocalOnlyPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("orientation", Gtk.Orientable.OrientableOrientationPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("recentManager", Gtk.RecentChooser.RecentChooserRecentManagerPropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("selectMultiple", Gtk.RecentChooser.RecentChooserSelectMultiplePropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("showIcons", Gtk.RecentChooser.RecentChooserShowIconsPropertyInfo), '("showNotFound", Gtk.RecentChooser.RecentChooserShowNotFoundPropertyInfo), '("showPrivate", Gtk.RecentChooser.RecentChooserShowPrivatePropertyInfo), '("showTips", Gtk.RecentChooser.RecentChooserShowTipsPropertyInfo), '("sortType", Gtk.RecentChooser.RecentChooserSortTypePropertyInfo), '("spacing", Gtk.Box.BoxSpacingPropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList RecentChooserWidget = RecentChooserWidgetSignalList
type RecentChooserWidgetSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("itemActivated", Gtk.RecentChooser.RecentChooserItemActivatedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionChanged", Gtk.RecentChooser.RecentChooserSelectionChangedSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method RecentChooserWidget::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just
--               (TInterface
--                  Name { namespace = "Gtk" , name = "RecentChooserWidget" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_chooser_widget_new" gtk_recent_chooser_widget_new :: 
    IO (Ptr RecentChooserWidget)

-- | Creates a new t'GI.Gtk.Objects.RecentChooserWidget.RecentChooserWidget' object.  This is an embeddable widget
-- used to access the recently used resources list.
-- 
-- /Since: 2.10/
recentChooserWidgetNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m RecentChooserWidget
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.RecentChooserWidget.RecentChooserWidget'
recentChooserWidgetNew  = liftIO $ do
    result <- gtk_recent_chooser_widget_new
    checkUnexpectedReturnNULL "recentChooserWidgetNew" result
    result' <- (newObject RecentChooserWidget) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method RecentChooserWidget::new_for_manager
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
--                  Name { namespace = "Gtk" , name = "RecentChooserWidget" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_chooser_widget_new_for_manager" gtk_recent_chooser_widget_new_for_manager :: 
    Ptr Gtk.RecentManager.RecentManager ->  -- manager : TInterface (Name {namespace = "Gtk", name = "RecentManager"})
    IO (Ptr RecentChooserWidget)

-- | Creates a new t'GI.Gtk.Objects.RecentChooserWidget.RecentChooserWidget' with a specified recent manager.
-- 
-- This is useful if you have implemented your own recent manager, or if you
-- have a customized instance of a t'GI.Gtk.Objects.RecentManager.RecentManager' object.
-- 
-- /Since: 2.10/
recentChooserWidgetNewForManager ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.RecentManager.IsRecentManager a) =>
    a
    -- ^ /@manager@/: a t'GI.Gtk.Objects.RecentManager.RecentManager'
    -> m RecentChooserWidget
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.RecentChooserWidget.RecentChooserWidget'
recentChooserWidgetNewForManager manager = liftIO $ do
    manager' <- unsafeManagedPtrCastPtr manager
    result <- gtk_recent_chooser_widget_new_for_manager manager'
    checkUnexpectedReturnNULL "recentChooserWidgetNewForManager" result
    result' <- (newObject RecentChooserWidget) result
    touchManagedPtr manager
    return result'

#if defined(ENABLE_OVERLOADING)
#endif


