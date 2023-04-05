{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Objects.AppChooserWidget.AppChooserWidget' is a widget for selecting applications.
-- It is the main building block for t'GI.Gtk.Objects.AppChooserDialog.AppChooserDialog'. Most
-- applications only need to use the latter; but you can use
-- this widget as part of a larger widget if you have special needs.
-- 
-- t'GI.Gtk.Objects.AppChooserWidget.AppChooserWidget' offers detailed control over what applications
-- are shown, using the
-- [AppChooserWidget:showDefault]("GI.Gtk.Objects.AppChooserWidget#g:attr:showDefault"),
-- [AppChooserWidget:showRecommended]("GI.Gtk.Objects.AppChooserWidget#g:attr:showRecommended"),
-- [AppChooserWidget:showFallback]("GI.Gtk.Objects.AppChooserWidget#g:attr:showFallback"),
-- [AppChooserWidget:showOther]("GI.Gtk.Objects.AppChooserWidget#g:attr:showOther") and
-- [AppChooserWidget:showAll]("GI.Gtk.Objects.AppChooserWidget#g:attr:showAll")
-- properties. See the t'GI.Gtk.Interfaces.AppChooser.AppChooser' documentation for more information
-- about these groups of applications.
-- 
-- To keep track of the selected application, use the
-- [AppChooserWidget::applicationSelected]("GI.Gtk.Objects.AppChooserWidget#g:signal:applicationSelected") and [AppChooserWidget::applicationActivated]("GI.Gtk.Objects.AppChooserWidget#g:signal:applicationActivated") signals.
-- 
-- = CSS nodes
-- 
-- GtkAppChooserWidget has a single CSS node with name appchooser.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.AppChooserWidget
    ( 

-- * Exported types
    AppChooserWidget(..)                    ,
    IsAppChooserWidget                      ,
    toAppChooserWidget                      ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [packEnd]("GI.Gtk.Objects.Box#g:method:packEnd"), [packStart]("GI.Gtk.Objects.Box#g:method:packStart"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [queryChildPacking]("GI.Gtk.Objects.Box#g:method:queryChildPacking"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [refresh]("GI.Gtk.Interfaces.AppChooser#g:method:refresh"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reorderChild]("GI.Gtk.Objects.Box#g:method:reorderChild"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppInfo]("GI.Gtk.Interfaces.AppChooser#g:method:getAppInfo"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getBaselinePosition]("GI.Gtk.Objects.Box#g:method:getBaselinePosition"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getCenterWidget]("GI.Gtk.Objects.Box#g:method:getCenterWidget"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getContentType]("GI.Gtk.Interfaces.AppChooser#g:method:getContentType"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDefaultText]("GI.Gtk.Objects.AppChooserWidget#g:method:getDefaultText"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getHomogeneous]("GI.Gtk.Objects.Box#g:method:getHomogeneous"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getOrientation]("GI.Gtk.Interfaces.Orientable#g:method:getOrientation"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getShowAll]("GI.Gtk.Objects.AppChooserWidget#g:method:getShowAll"), [getShowDefault]("GI.Gtk.Objects.AppChooserWidget#g:method:getShowDefault"), [getShowFallback]("GI.Gtk.Objects.AppChooserWidget#g:method:getShowFallback"), [getShowOther]("GI.Gtk.Objects.AppChooserWidget#g:method:getShowOther"), [getShowRecommended]("GI.Gtk.Objects.AppChooserWidget#g:method:getShowRecommended"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getSpacing]("GI.Gtk.Objects.Box#g:method:getSpacing"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBaselinePosition]("GI.Gtk.Objects.Box#g:method:setBaselinePosition"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setCenterWidget]("GI.Gtk.Objects.Box#g:method:setCenterWidget"), [setChildPacking]("GI.Gtk.Objects.Box#g:method:setChildPacking"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDefaultText]("GI.Gtk.Objects.AppChooserWidget#g:method:setDefaultText"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setHomogeneous]("GI.Gtk.Objects.Box#g:method:setHomogeneous"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setOrientation]("GI.Gtk.Interfaces.Orientable#g:method:setOrientation"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setShowAll]("GI.Gtk.Objects.AppChooserWidget#g:method:setShowAll"), [setShowDefault]("GI.Gtk.Objects.AppChooserWidget#g:method:setShowDefault"), [setShowFallback]("GI.Gtk.Objects.AppChooserWidget#g:method:setShowFallback"), [setShowOther]("GI.Gtk.Objects.AppChooserWidget#g:method:setShowOther"), [setShowRecommended]("GI.Gtk.Objects.AppChooserWidget#g:method:setShowRecommended"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setSpacing]("GI.Gtk.Objects.Box#g:method:setSpacing"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveAppChooserWidgetMethod           ,
#endif

-- ** getDefaultText #method:getDefaultText#

#if defined(ENABLE_OVERLOADING)
    AppChooserWidgetGetDefaultTextMethodInfo,
#endif
    appChooserWidgetGetDefaultText          ,


-- ** getShowAll #method:getShowAll#

#if defined(ENABLE_OVERLOADING)
    AppChooserWidgetGetShowAllMethodInfo    ,
#endif
    appChooserWidgetGetShowAll              ,


-- ** getShowDefault #method:getShowDefault#

#if defined(ENABLE_OVERLOADING)
    AppChooserWidgetGetShowDefaultMethodInfo,
#endif
    appChooserWidgetGetShowDefault          ,


-- ** getShowFallback #method:getShowFallback#

#if defined(ENABLE_OVERLOADING)
    AppChooserWidgetGetShowFallbackMethodInfo,
#endif
    appChooserWidgetGetShowFallback         ,


-- ** getShowOther #method:getShowOther#

#if defined(ENABLE_OVERLOADING)
    AppChooserWidgetGetShowOtherMethodInfo  ,
#endif
    appChooserWidgetGetShowOther            ,


-- ** getShowRecommended #method:getShowRecommended#

#if defined(ENABLE_OVERLOADING)
    AppChooserWidgetGetShowRecommendedMethodInfo,
#endif
    appChooserWidgetGetShowRecommended      ,


-- ** new #method:new#

    appChooserWidgetNew                     ,


-- ** setDefaultText #method:setDefaultText#

#if defined(ENABLE_OVERLOADING)
    AppChooserWidgetSetDefaultTextMethodInfo,
#endif
    appChooserWidgetSetDefaultText          ,


-- ** setShowAll #method:setShowAll#

#if defined(ENABLE_OVERLOADING)
    AppChooserWidgetSetShowAllMethodInfo    ,
#endif
    appChooserWidgetSetShowAll              ,


-- ** setShowDefault #method:setShowDefault#

#if defined(ENABLE_OVERLOADING)
    AppChooserWidgetSetShowDefaultMethodInfo,
#endif
    appChooserWidgetSetShowDefault          ,


-- ** setShowFallback #method:setShowFallback#

#if defined(ENABLE_OVERLOADING)
    AppChooserWidgetSetShowFallbackMethodInfo,
#endif
    appChooserWidgetSetShowFallback         ,


-- ** setShowOther #method:setShowOther#

#if defined(ENABLE_OVERLOADING)
    AppChooserWidgetSetShowOtherMethodInfo  ,
#endif
    appChooserWidgetSetShowOther            ,


-- ** setShowRecommended #method:setShowRecommended#

#if defined(ENABLE_OVERLOADING)
    AppChooserWidgetSetShowRecommendedMethodInfo,
#endif
    appChooserWidgetSetShowRecommended      ,




 -- * Properties


-- ** defaultText #attr:defaultText#
-- | The [AppChooserWidget:defaultText]("GI.Gtk.Objects.AppChooserWidget#g:attr:defaultText") property determines the text
-- that appears in the widget when there are no applications for the
-- given content type.
-- See also 'GI.Gtk.Objects.AppChooserWidget.appChooserWidgetSetDefaultText'.

#if defined(ENABLE_OVERLOADING)
    AppChooserWidgetDefaultTextPropertyInfo ,
#endif
#if defined(ENABLE_OVERLOADING)
    appChooserWidgetDefaultText             ,
#endif
    constructAppChooserWidgetDefaultText    ,
    getAppChooserWidgetDefaultText          ,
    setAppChooserWidgetDefaultText          ,


-- ** showAll #attr:showAll#
-- | If the [AppChooserWidget:showAll]("GI.Gtk.Objects.AppChooserWidget#g:attr:showAll") property is 'P.True', the app
-- chooser presents all applications in a single list, without
-- subsections for default, recommended or related applications.

#if defined(ENABLE_OVERLOADING)
    AppChooserWidgetShowAllPropertyInfo     ,
#endif
#if defined(ENABLE_OVERLOADING)
    appChooserWidgetShowAll                 ,
#endif
    constructAppChooserWidgetShowAll        ,
    getAppChooserWidgetShowAll              ,
    setAppChooserWidgetShowAll              ,


-- ** showDefault #attr:showDefault#
-- | The [showDefault](#g:signal:showDefault) property determines whether the app chooser
-- should show the default handler for the content type in a
-- separate section. If 'P.False', the default handler is listed
-- among the recommended applications.

#if defined(ENABLE_OVERLOADING)
    AppChooserWidgetShowDefaultPropertyInfo ,
#endif
#if defined(ENABLE_OVERLOADING)
    appChooserWidgetShowDefault             ,
#endif
    constructAppChooserWidgetShowDefault    ,
    getAppChooserWidgetShowDefault          ,
    setAppChooserWidgetShowDefault          ,


-- ** showFallback #attr:showFallback#
-- | The [AppChooserWidget:showFallback]("GI.Gtk.Objects.AppChooserWidget#g:attr:showFallback") property determines whether
-- the app chooser should show a section for fallback applications.
-- If 'P.False', the fallback applications are listed among the other
-- applications.

#if defined(ENABLE_OVERLOADING)
    AppChooserWidgetShowFallbackPropertyInfo,
#endif
#if defined(ENABLE_OVERLOADING)
    appChooserWidgetShowFallback            ,
#endif
    constructAppChooserWidgetShowFallback   ,
    getAppChooserWidgetShowFallback         ,
    setAppChooserWidgetShowFallback         ,


-- ** showOther #attr:showOther#
-- | The [AppChooserWidget:showOther]("GI.Gtk.Objects.AppChooserWidget#g:attr:showOther") property determines whether
-- the app chooser should show a section for other applications.

#if defined(ENABLE_OVERLOADING)
    AppChooserWidgetShowOtherPropertyInfo   ,
#endif
#if defined(ENABLE_OVERLOADING)
    appChooserWidgetShowOther               ,
#endif
    constructAppChooserWidgetShowOther      ,
    getAppChooserWidgetShowOther            ,
    setAppChooserWidgetShowOther            ,


-- ** showRecommended #attr:showRecommended#
-- | The [AppChooserWidget:showRecommended]("GI.Gtk.Objects.AppChooserWidget#g:attr:showRecommended") property determines
-- whether the app chooser should show a section for recommended
-- applications. If 'P.False', the recommended applications are listed
-- among the other applications.

#if defined(ENABLE_OVERLOADING)
    AppChooserWidgetShowRecommendedPropertyInfo,
#endif
#if defined(ENABLE_OVERLOADING)
    appChooserWidgetShowRecommended         ,
#endif
    constructAppChooserWidgetShowRecommended,
    getAppChooserWidgetShowRecommended      ,
    setAppChooserWidgetShowRecommended      ,




 -- * Signals


-- ** applicationActivated #signal:applicationActivated#

    AppChooserWidgetApplicationActivatedCallback,
#if defined(ENABLE_OVERLOADING)
    AppChooserWidgetApplicationActivatedSignalInfo,
#endif
    afterAppChooserWidgetApplicationActivated,
    onAppChooserWidgetApplicationActivated  ,


-- ** applicationSelected #signal:applicationSelected#

    AppChooserWidgetApplicationSelectedCallback,
#if defined(ENABLE_OVERLOADING)
    AppChooserWidgetApplicationSelectedSignalInfo,
#endif
    afterAppChooserWidgetApplicationSelected,
    onAppChooserWidgetApplicationSelected   ,


-- ** populatePopup #signal:populatePopup#

    AppChooserWidgetPopulatePopupCallback   ,
#if defined(ENABLE_OVERLOADING)
    AppChooserWidgetPopulatePopupSignalInfo ,
#endif
    afterAppChooserWidgetPopulatePopup      ,
    onAppChooserWidgetPopulatePopup         ,




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
import qualified GI.Gio.Interfaces.AppInfo as Gio.AppInfo
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.AppChooser as Gtk.AppChooser
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Orientable as Gtk.Orientable
import {-# SOURCE #-} qualified GI.Gtk.Objects.Box as Gtk.Box
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.Menu as Gtk.Menu
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype AppChooserWidget = AppChooserWidget (SP.ManagedPtr AppChooserWidget)
    deriving (Eq)

instance SP.ManagedPtrNewtype AppChooserWidget where
    toManagedPtr (AppChooserWidget p) = p

foreign import ccall "gtk_app_chooser_widget_get_type"
    c_gtk_app_chooser_widget_get_type :: IO B.Types.GType

instance B.Types.TypedObject AppChooserWidget where
    glibType = c_gtk_app_chooser_widget_get_type

instance B.Types.GObject AppChooserWidget

-- | Type class for types which can be safely cast to `AppChooserWidget`, for instance with `toAppChooserWidget`.
class (SP.GObject o, O.IsDescendantOf AppChooserWidget o) => IsAppChooserWidget o
instance (SP.GObject o, O.IsDescendantOf AppChooserWidget o) => IsAppChooserWidget o

instance O.HasParentTypes AppChooserWidget
type instance O.ParentTypes AppChooserWidget = '[Gtk.Box.Box, Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.AppChooser.AppChooser, Gtk.Buildable.Buildable, Gtk.Orientable.Orientable]

-- | Cast to `AppChooserWidget`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toAppChooserWidget :: (MIO.MonadIO m, IsAppChooserWidget o) => o -> m AppChooserWidget
toAppChooserWidget = MIO.liftIO . B.ManagedPtr.unsafeCastTo AppChooserWidget

-- | Convert 'AppChooserWidget' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe AppChooserWidget) where
    gvalueGType_ = c_gtk_app_chooser_widget_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr AppChooserWidget)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr AppChooserWidget)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject AppChooserWidget ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveAppChooserWidgetMethod (t :: Symbol) (o :: *) :: * where
    ResolveAppChooserWidgetMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveAppChooserWidgetMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveAppChooserWidgetMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveAppChooserWidgetMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveAppChooserWidgetMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveAppChooserWidgetMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveAppChooserWidgetMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveAppChooserWidgetMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveAppChooserWidgetMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveAppChooserWidgetMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveAppChooserWidgetMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveAppChooserWidgetMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveAppChooserWidgetMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveAppChooserWidgetMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveAppChooserWidgetMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveAppChooserWidgetMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveAppChooserWidgetMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveAppChooserWidgetMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveAppChooserWidgetMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveAppChooserWidgetMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveAppChooserWidgetMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveAppChooserWidgetMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveAppChooserWidgetMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveAppChooserWidgetMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveAppChooserWidgetMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveAppChooserWidgetMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveAppChooserWidgetMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveAppChooserWidgetMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveAppChooserWidgetMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveAppChooserWidgetMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveAppChooserWidgetMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveAppChooserWidgetMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveAppChooserWidgetMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveAppChooserWidgetMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveAppChooserWidgetMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveAppChooserWidgetMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveAppChooserWidgetMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveAppChooserWidgetMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveAppChooserWidgetMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveAppChooserWidgetMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveAppChooserWidgetMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveAppChooserWidgetMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveAppChooserWidgetMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveAppChooserWidgetMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveAppChooserWidgetMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveAppChooserWidgetMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveAppChooserWidgetMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveAppChooserWidgetMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveAppChooserWidgetMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveAppChooserWidgetMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveAppChooserWidgetMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveAppChooserWidgetMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveAppChooserWidgetMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveAppChooserWidgetMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveAppChooserWidgetMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveAppChooserWidgetMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveAppChooserWidgetMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveAppChooserWidgetMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveAppChooserWidgetMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveAppChooserWidgetMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveAppChooserWidgetMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveAppChooserWidgetMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveAppChooserWidgetMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveAppChooserWidgetMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveAppChooserWidgetMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveAppChooserWidgetMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveAppChooserWidgetMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveAppChooserWidgetMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveAppChooserWidgetMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveAppChooserWidgetMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveAppChooserWidgetMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveAppChooserWidgetMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveAppChooserWidgetMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveAppChooserWidgetMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveAppChooserWidgetMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveAppChooserWidgetMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveAppChooserWidgetMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveAppChooserWidgetMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveAppChooserWidgetMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveAppChooserWidgetMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveAppChooserWidgetMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveAppChooserWidgetMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveAppChooserWidgetMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveAppChooserWidgetMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveAppChooserWidgetMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveAppChooserWidgetMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveAppChooserWidgetMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveAppChooserWidgetMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveAppChooserWidgetMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveAppChooserWidgetMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveAppChooserWidgetMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveAppChooserWidgetMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveAppChooserWidgetMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveAppChooserWidgetMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveAppChooserWidgetMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveAppChooserWidgetMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveAppChooserWidgetMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveAppChooserWidgetMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveAppChooserWidgetMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveAppChooserWidgetMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveAppChooserWidgetMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveAppChooserWidgetMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveAppChooserWidgetMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveAppChooserWidgetMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveAppChooserWidgetMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveAppChooserWidgetMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveAppChooserWidgetMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveAppChooserWidgetMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveAppChooserWidgetMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveAppChooserWidgetMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveAppChooserWidgetMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveAppChooserWidgetMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveAppChooserWidgetMethod "packEnd" o = Gtk.Box.BoxPackEndMethodInfo
    ResolveAppChooserWidgetMethod "packStart" o = Gtk.Box.BoxPackStartMethodInfo
    ResolveAppChooserWidgetMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveAppChooserWidgetMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveAppChooserWidgetMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveAppChooserWidgetMethod "queryChildPacking" o = Gtk.Box.BoxQueryChildPackingMethodInfo
    ResolveAppChooserWidgetMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveAppChooserWidgetMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveAppChooserWidgetMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveAppChooserWidgetMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveAppChooserWidgetMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveAppChooserWidgetMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveAppChooserWidgetMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveAppChooserWidgetMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveAppChooserWidgetMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveAppChooserWidgetMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveAppChooserWidgetMethod "refresh" o = Gtk.AppChooser.AppChooserRefreshMethodInfo
    ResolveAppChooserWidgetMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveAppChooserWidgetMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveAppChooserWidgetMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolveAppChooserWidgetMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveAppChooserWidgetMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveAppChooserWidgetMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveAppChooserWidgetMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveAppChooserWidgetMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveAppChooserWidgetMethod "reorderChild" o = Gtk.Box.BoxReorderChildMethodInfo
    ResolveAppChooserWidgetMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveAppChooserWidgetMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveAppChooserWidgetMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveAppChooserWidgetMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveAppChooserWidgetMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveAppChooserWidgetMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveAppChooserWidgetMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveAppChooserWidgetMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveAppChooserWidgetMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveAppChooserWidgetMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveAppChooserWidgetMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveAppChooserWidgetMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveAppChooserWidgetMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveAppChooserWidgetMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveAppChooserWidgetMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveAppChooserWidgetMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveAppChooserWidgetMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveAppChooserWidgetMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveAppChooserWidgetMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveAppChooserWidgetMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveAppChooserWidgetMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveAppChooserWidgetMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveAppChooserWidgetMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveAppChooserWidgetMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveAppChooserWidgetMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveAppChooserWidgetMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveAppChooserWidgetMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveAppChooserWidgetMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveAppChooserWidgetMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveAppChooserWidgetMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveAppChooserWidgetMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveAppChooserWidgetMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveAppChooserWidgetMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveAppChooserWidgetMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveAppChooserWidgetMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveAppChooserWidgetMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveAppChooserWidgetMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveAppChooserWidgetMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveAppChooserWidgetMethod "getAppInfo" o = Gtk.AppChooser.AppChooserGetAppInfoMethodInfo
    ResolveAppChooserWidgetMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveAppChooserWidgetMethod "getBaselinePosition" o = Gtk.Box.BoxGetBaselinePositionMethodInfo
    ResolveAppChooserWidgetMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveAppChooserWidgetMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveAppChooserWidgetMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveAppChooserWidgetMethod "getCenterWidget" o = Gtk.Box.BoxGetCenterWidgetMethodInfo
    ResolveAppChooserWidgetMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveAppChooserWidgetMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveAppChooserWidgetMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveAppChooserWidgetMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveAppChooserWidgetMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveAppChooserWidgetMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveAppChooserWidgetMethod "getContentType" o = Gtk.AppChooser.AppChooserGetContentTypeMethodInfo
    ResolveAppChooserWidgetMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveAppChooserWidgetMethod "getDefaultText" o = AppChooserWidgetGetDefaultTextMethodInfo
    ResolveAppChooserWidgetMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveAppChooserWidgetMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveAppChooserWidgetMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveAppChooserWidgetMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveAppChooserWidgetMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveAppChooserWidgetMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveAppChooserWidgetMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveAppChooserWidgetMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveAppChooserWidgetMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveAppChooserWidgetMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveAppChooserWidgetMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveAppChooserWidgetMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveAppChooserWidgetMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveAppChooserWidgetMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveAppChooserWidgetMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveAppChooserWidgetMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveAppChooserWidgetMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveAppChooserWidgetMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveAppChooserWidgetMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveAppChooserWidgetMethod "getHomogeneous" o = Gtk.Box.BoxGetHomogeneousMethodInfo
    ResolveAppChooserWidgetMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveAppChooserWidgetMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveAppChooserWidgetMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveAppChooserWidgetMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveAppChooserWidgetMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveAppChooserWidgetMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveAppChooserWidgetMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveAppChooserWidgetMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveAppChooserWidgetMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveAppChooserWidgetMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveAppChooserWidgetMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveAppChooserWidgetMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveAppChooserWidgetMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveAppChooserWidgetMethod "getOrientation" o = Gtk.Orientable.OrientableGetOrientationMethodInfo
    ResolveAppChooserWidgetMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveAppChooserWidgetMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveAppChooserWidgetMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveAppChooserWidgetMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveAppChooserWidgetMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveAppChooserWidgetMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveAppChooserWidgetMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveAppChooserWidgetMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveAppChooserWidgetMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveAppChooserWidgetMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveAppChooserWidgetMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveAppChooserWidgetMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveAppChooserWidgetMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveAppChooserWidgetMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveAppChooserWidgetMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveAppChooserWidgetMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveAppChooserWidgetMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveAppChooserWidgetMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveAppChooserWidgetMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveAppChooserWidgetMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveAppChooserWidgetMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveAppChooserWidgetMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveAppChooserWidgetMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveAppChooserWidgetMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveAppChooserWidgetMethod "getShowAll" o = AppChooserWidgetGetShowAllMethodInfo
    ResolveAppChooserWidgetMethod "getShowDefault" o = AppChooserWidgetGetShowDefaultMethodInfo
    ResolveAppChooserWidgetMethod "getShowFallback" o = AppChooserWidgetGetShowFallbackMethodInfo
    ResolveAppChooserWidgetMethod "getShowOther" o = AppChooserWidgetGetShowOtherMethodInfo
    ResolveAppChooserWidgetMethod "getShowRecommended" o = AppChooserWidgetGetShowRecommendedMethodInfo
    ResolveAppChooserWidgetMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveAppChooserWidgetMethod "getSpacing" o = Gtk.Box.BoxGetSpacingMethodInfo
    ResolveAppChooserWidgetMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveAppChooserWidgetMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveAppChooserWidgetMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveAppChooserWidgetMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveAppChooserWidgetMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveAppChooserWidgetMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveAppChooserWidgetMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveAppChooserWidgetMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveAppChooserWidgetMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveAppChooserWidgetMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveAppChooserWidgetMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveAppChooserWidgetMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveAppChooserWidgetMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveAppChooserWidgetMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveAppChooserWidgetMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveAppChooserWidgetMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveAppChooserWidgetMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveAppChooserWidgetMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveAppChooserWidgetMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveAppChooserWidgetMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveAppChooserWidgetMethod "setBaselinePosition" o = Gtk.Box.BoxSetBaselinePositionMethodInfo
    ResolveAppChooserWidgetMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveAppChooserWidgetMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveAppChooserWidgetMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveAppChooserWidgetMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveAppChooserWidgetMethod "setCenterWidget" o = Gtk.Box.BoxSetCenterWidgetMethodInfo
    ResolveAppChooserWidgetMethod "setChildPacking" o = Gtk.Box.BoxSetChildPackingMethodInfo
    ResolveAppChooserWidgetMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveAppChooserWidgetMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveAppChooserWidgetMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveAppChooserWidgetMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveAppChooserWidgetMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveAppChooserWidgetMethod "setDefaultText" o = AppChooserWidgetSetDefaultTextMethodInfo
    ResolveAppChooserWidgetMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveAppChooserWidgetMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveAppChooserWidgetMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveAppChooserWidgetMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveAppChooserWidgetMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveAppChooserWidgetMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveAppChooserWidgetMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveAppChooserWidgetMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveAppChooserWidgetMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveAppChooserWidgetMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveAppChooserWidgetMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveAppChooserWidgetMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveAppChooserWidgetMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveAppChooserWidgetMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveAppChooserWidgetMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveAppChooserWidgetMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveAppChooserWidgetMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveAppChooserWidgetMethod "setHomogeneous" o = Gtk.Box.BoxSetHomogeneousMethodInfo
    ResolveAppChooserWidgetMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveAppChooserWidgetMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveAppChooserWidgetMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveAppChooserWidgetMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveAppChooserWidgetMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveAppChooserWidgetMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveAppChooserWidgetMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveAppChooserWidgetMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveAppChooserWidgetMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveAppChooserWidgetMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveAppChooserWidgetMethod "setOrientation" o = Gtk.Orientable.OrientableSetOrientationMethodInfo
    ResolveAppChooserWidgetMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveAppChooserWidgetMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveAppChooserWidgetMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveAppChooserWidgetMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveAppChooserWidgetMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveAppChooserWidgetMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveAppChooserWidgetMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveAppChooserWidgetMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveAppChooserWidgetMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveAppChooserWidgetMethod "setShowAll" o = AppChooserWidgetSetShowAllMethodInfo
    ResolveAppChooserWidgetMethod "setShowDefault" o = AppChooserWidgetSetShowDefaultMethodInfo
    ResolveAppChooserWidgetMethod "setShowFallback" o = AppChooserWidgetSetShowFallbackMethodInfo
    ResolveAppChooserWidgetMethod "setShowOther" o = AppChooserWidgetSetShowOtherMethodInfo
    ResolveAppChooserWidgetMethod "setShowRecommended" o = AppChooserWidgetSetShowRecommendedMethodInfo
    ResolveAppChooserWidgetMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveAppChooserWidgetMethod "setSpacing" o = Gtk.Box.BoxSetSpacingMethodInfo
    ResolveAppChooserWidgetMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveAppChooserWidgetMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveAppChooserWidgetMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveAppChooserWidgetMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveAppChooserWidgetMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveAppChooserWidgetMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveAppChooserWidgetMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveAppChooserWidgetMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveAppChooserWidgetMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveAppChooserWidgetMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveAppChooserWidgetMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveAppChooserWidgetMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveAppChooserWidgetMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveAppChooserWidgetMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveAppChooserWidgetMethod t AppChooserWidget, O.OverloadedMethod info AppChooserWidget p) => OL.IsLabel t (AppChooserWidget -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveAppChooserWidgetMethod t AppChooserWidget, O.OverloadedMethod info AppChooserWidget p, R.HasField t AppChooserWidget p) => R.HasField t AppChooserWidget p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveAppChooserWidgetMethod t AppChooserWidget, O.OverloadedMethodInfo info AppChooserWidget) => OL.IsLabel t (O.MethodProxy info AppChooserWidget) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal AppChooserWidget::application-activated
-- | Emitted when an application item is activated from the widget\'s list.
-- 
-- This usually happens when the user double clicks an item, or an item
-- is selected and the user presses one of the keys Space, Shift+Space,
-- Return or Enter.
type AppChooserWidgetApplicationActivatedCallback =
    Gio.AppInfo.AppInfo
    -- ^ /@application@/: the activated t'GI.Gio.Interfaces.AppInfo.AppInfo'
    -> IO ()

type C_AppChooserWidgetApplicationActivatedCallback =
    Ptr AppChooserWidget ->                 -- object
    Ptr Gio.AppInfo.AppInfo ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_AppChooserWidgetApplicationActivatedCallback`.
foreign import ccall "wrapper"
    mk_AppChooserWidgetApplicationActivatedCallback :: C_AppChooserWidgetApplicationActivatedCallback -> IO (FunPtr C_AppChooserWidgetApplicationActivatedCallback)

wrap_AppChooserWidgetApplicationActivatedCallback :: 
    GObject a => (a -> AppChooserWidgetApplicationActivatedCallback) ->
    C_AppChooserWidgetApplicationActivatedCallback
wrap_AppChooserWidgetApplicationActivatedCallback gi'cb gi'selfPtr application _ = do
    application' <- (newObject Gio.AppInfo.AppInfo) application
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  application'


-- | Connect a signal handler for the [applicationActivated](#signal:applicationActivated) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' appChooserWidget #applicationActivated callback
-- @
-- 
-- 
onAppChooserWidgetApplicationActivated :: (IsAppChooserWidget a, MonadIO m) => a -> ((?self :: a) => AppChooserWidgetApplicationActivatedCallback) -> m SignalHandlerId
onAppChooserWidgetApplicationActivated obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_AppChooserWidgetApplicationActivatedCallback wrapped
    wrapped'' <- mk_AppChooserWidgetApplicationActivatedCallback wrapped'
    connectSignalFunPtr obj "application-activated" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [applicationActivated](#signal:applicationActivated) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' appChooserWidget #applicationActivated callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterAppChooserWidgetApplicationActivated :: (IsAppChooserWidget a, MonadIO m) => a -> ((?self :: a) => AppChooserWidgetApplicationActivatedCallback) -> m SignalHandlerId
afterAppChooserWidgetApplicationActivated obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_AppChooserWidgetApplicationActivatedCallback wrapped
    wrapped'' <- mk_AppChooserWidgetApplicationActivatedCallback wrapped'
    connectSignalFunPtr obj "application-activated" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data AppChooserWidgetApplicationActivatedSignalInfo
instance SignalInfo AppChooserWidgetApplicationActivatedSignalInfo where
    type HaskellCallbackType AppChooserWidgetApplicationActivatedSignalInfo = AppChooserWidgetApplicationActivatedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_AppChooserWidgetApplicationActivatedCallback cb
        cb'' <- mk_AppChooserWidgetApplicationActivatedCallback cb'
        connectSignalFunPtr obj "application-activated" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserWidget::application-activated"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserWidget.html#g:signal:applicationActivated"})

#endif

-- signal AppChooserWidget::application-selected
-- | Emitted when an application item is selected from the widget\'s list.
type AppChooserWidgetApplicationSelectedCallback =
    Gio.AppInfo.AppInfo
    -- ^ /@application@/: the selected t'GI.Gio.Interfaces.AppInfo.AppInfo'
    -> IO ()

type C_AppChooserWidgetApplicationSelectedCallback =
    Ptr AppChooserWidget ->                 -- object
    Ptr Gio.AppInfo.AppInfo ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_AppChooserWidgetApplicationSelectedCallback`.
foreign import ccall "wrapper"
    mk_AppChooserWidgetApplicationSelectedCallback :: C_AppChooserWidgetApplicationSelectedCallback -> IO (FunPtr C_AppChooserWidgetApplicationSelectedCallback)

wrap_AppChooserWidgetApplicationSelectedCallback :: 
    GObject a => (a -> AppChooserWidgetApplicationSelectedCallback) ->
    C_AppChooserWidgetApplicationSelectedCallback
wrap_AppChooserWidgetApplicationSelectedCallback gi'cb gi'selfPtr application _ = do
    application' <- (newObject Gio.AppInfo.AppInfo) application
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  application'


-- | Connect a signal handler for the [applicationSelected](#signal:applicationSelected) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' appChooserWidget #applicationSelected callback
-- @
-- 
-- 
onAppChooserWidgetApplicationSelected :: (IsAppChooserWidget a, MonadIO m) => a -> ((?self :: a) => AppChooserWidgetApplicationSelectedCallback) -> m SignalHandlerId
onAppChooserWidgetApplicationSelected obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_AppChooserWidgetApplicationSelectedCallback wrapped
    wrapped'' <- mk_AppChooserWidgetApplicationSelectedCallback wrapped'
    connectSignalFunPtr obj "application-selected" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [applicationSelected](#signal:applicationSelected) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' appChooserWidget #applicationSelected callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterAppChooserWidgetApplicationSelected :: (IsAppChooserWidget a, MonadIO m) => a -> ((?self :: a) => AppChooserWidgetApplicationSelectedCallback) -> m SignalHandlerId
afterAppChooserWidgetApplicationSelected obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_AppChooserWidgetApplicationSelectedCallback wrapped
    wrapped'' <- mk_AppChooserWidgetApplicationSelectedCallback wrapped'
    connectSignalFunPtr obj "application-selected" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data AppChooserWidgetApplicationSelectedSignalInfo
instance SignalInfo AppChooserWidgetApplicationSelectedSignalInfo where
    type HaskellCallbackType AppChooserWidgetApplicationSelectedSignalInfo = AppChooserWidgetApplicationSelectedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_AppChooserWidgetApplicationSelectedCallback cb
        cb'' <- mk_AppChooserWidgetApplicationSelectedCallback cb'
        connectSignalFunPtr obj "application-selected" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserWidget::application-selected"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserWidget.html#g:signal:applicationSelected"})

#endif

-- signal AppChooserWidget::populate-popup
-- | Emitted when a context menu is about to popup over an application item.
-- Clients can insert menu items into the provided t'GI.Gtk.Objects.Menu.Menu' object in the
-- callback of this signal; the context menu will be shown over the item
-- if at least one item has been added to the menu.
type AppChooserWidgetPopulatePopupCallback =
    Gtk.Menu.Menu
    -- ^ /@menu@/: the t'GI.Gtk.Objects.Menu.Menu' to populate
    -> Gio.AppInfo.AppInfo
    -- ^ /@application@/: the current t'GI.Gio.Interfaces.AppInfo.AppInfo'
    -> IO ()

type C_AppChooserWidgetPopulatePopupCallback =
    Ptr AppChooserWidget ->                 -- object
    Ptr Gtk.Menu.Menu ->
    Ptr Gio.AppInfo.AppInfo ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_AppChooserWidgetPopulatePopupCallback`.
foreign import ccall "wrapper"
    mk_AppChooserWidgetPopulatePopupCallback :: C_AppChooserWidgetPopulatePopupCallback -> IO (FunPtr C_AppChooserWidgetPopulatePopupCallback)

wrap_AppChooserWidgetPopulatePopupCallback :: 
    GObject a => (a -> AppChooserWidgetPopulatePopupCallback) ->
    C_AppChooserWidgetPopulatePopupCallback
wrap_AppChooserWidgetPopulatePopupCallback gi'cb gi'selfPtr menu application _ = do
    menu' <- (newObject Gtk.Menu.Menu) menu
    application' <- (newObject Gio.AppInfo.AppInfo) application
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  menu' application'


-- | Connect a signal handler for the [populatePopup](#signal:populatePopup) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' appChooserWidget #populatePopup callback
-- @
-- 
-- 
onAppChooserWidgetPopulatePopup :: (IsAppChooserWidget a, MonadIO m) => a -> ((?self :: a) => AppChooserWidgetPopulatePopupCallback) -> m SignalHandlerId
onAppChooserWidgetPopulatePopup obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_AppChooserWidgetPopulatePopupCallback wrapped
    wrapped'' <- mk_AppChooserWidgetPopulatePopupCallback wrapped'
    connectSignalFunPtr obj "populate-popup" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [populatePopup](#signal:populatePopup) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' appChooserWidget #populatePopup callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterAppChooserWidgetPopulatePopup :: (IsAppChooserWidget a, MonadIO m) => a -> ((?self :: a) => AppChooserWidgetPopulatePopupCallback) -> m SignalHandlerId
afterAppChooserWidgetPopulatePopup obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_AppChooserWidgetPopulatePopupCallback wrapped
    wrapped'' <- mk_AppChooserWidgetPopulatePopupCallback wrapped'
    connectSignalFunPtr obj "populate-popup" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data AppChooserWidgetPopulatePopupSignalInfo
instance SignalInfo AppChooserWidgetPopulatePopupSignalInfo where
    type HaskellCallbackType AppChooserWidgetPopulatePopupSignalInfo = AppChooserWidgetPopulatePopupCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_AppChooserWidgetPopulatePopupCallback cb
        cb'' <- mk_AppChooserWidgetPopulatePopupCallback cb'
        connectSignalFunPtr obj "populate-popup" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserWidget::populate-popup"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserWidget.html#g:signal:populatePopup"})

#endif

-- VVV Prop "default-text"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@default-text@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' appChooserWidget #defaultText
-- @
getAppChooserWidgetDefaultText :: (MonadIO m, IsAppChooserWidget o) => o -> m T.Text
getAppChooserWidgetDefaultText obj = MIO.liftIO $ checkUnexpectedNothing "getAppChooserWidgetDefaultText" $ B.Properties.getObjectPropertyString obj "default-text"

-- | Set the value of the “@default-text@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' appChooserWidget [ #defaultText 'Data.GI.Base.Attributes.:=' value ]
-- @
setAppChooserWidgetDefaultText :: (MonadIO m, IsAppChooserWidget o) => o -> T.Text -> m ()
setAppChooserWidgetDefaultText obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "default-text" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@default-text@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructAppChooserWidgetDefaultText :: (IsAppChooserWidget o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructAppChooserWidgetDefaultText val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "default-text" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data AppChooserWidgetDefaultTextPropertyInfo
instance AttrInfo AppChooserWidgetDefaultTextPropertyInfo where
    type AttrAllowedOps AppChooserWidgetDefaultTextPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint AppChooserWidgetDefaultTextPropertyInfo = IsAppChooserWidget
    type AttrSetTypeConstraint AppChooserWidgetDefaultTextPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint AppChooserWidgetDefaultTextPropertyInfo = (~) T.Text
    type AttrTransferType AppChooserWidgetDefaultTextPropertyInfo = T.Text
    type AttrGetType AppChooserWidgetDefaultTextPropertyInfo = T.Text
    type AttrLabel AppChooserWidgetDefaultTextPropertyInfo = "default-text"
    type AttrOrigin AppChooserWidgetDefaultTextPropertyInfo = AppChooserWidget
    attrGet = getAppChooserWidgetDefaultText
    attrSet = setAppChooserWidgetDefaultText
    attrTransfer _ v = do
        return v
    attrConstruct = constructAppChooserWidgetDefaultText
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserWidget.defaultText"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserWidget.html#g:attr:defaultText"
        })
#endif

-- VVV Prop "show-all"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstruct]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@show-all@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' appChooserWidget #showAll
-- @
getAppChooserWidgetShowAll :: (MonadIO m, IsAppChooserWidget o) => o -> m Bool
getAppChooserWidgetShowAll obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "show-all"

-- | Set the value of the “@show-all@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' appChooserWidget [ #showAll 'Data.GI.Base.Attributes.:=' value ]
-- @
setAppChooserWidgetShowAll :: (MonadIO m, IsAppChooserWidget o) => o -> Bool -> m ()
setAppChooserWidgetShowAll obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "show-all" val

-- | Construct a `GValueConstruct` with valid value for the “@show-all@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructAppChooserWidgetShowAll :: (IsAppChooserWidget o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructAppChooserWidgetShowAll val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "show-all" val

#if defined(ENABLE_OVERLOADING)
data AppChooserWidgetShowAllPropertyInfo
instance AttrInfo AppChooserWidgetShowAllPropertyInfo where
    type AttrAllowedOps AppChooserWidgetShowAllPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint AppChooserWidgetShowAllPropertyInfo = IsAppChooserWidget
    type AttrSetTypeConstraint AppChooserWidgetShowAllPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint AppChooserWidgetShowAllPropertyInfo = (~) Bool
    type AttrTransferType AppChooserWidgetShowAllPropertyInfo = Bool
    type AttrGetType AppChooserWidgetShowAllPropertyInfo = Bool
    type AttrLabel AppChooserWidgetShowAllPropertyInfo = "show-all"
    type AttrOrigin AppChooserWidgetShowAllPropertyInfo = AppChooserWidget
    attrGet = getAppChooserWidgetShowAll
    attrSet = setAppChooserWidgetShowAll
    attrTransfer _ v = do
        return v
    attrConstruct = constructAppChooserWidgetShowAll
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserWidget.showAll"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserWidget.html#g:attr:showAll"
        })
#endif

-- VVV Prop "show-default"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstruct]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@show-default@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' appChooserWidget #showDefault
-- @
getAppChooserWidgetShowDefault :: (MonadIO m, IsAppChooserWidget o) => o -> m Bool
getAppChooserWidgetShowDefault obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "show-default"

-- | Set the value of the “@show-default@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' appChooserWidget [ #showDefault 'Data.GI.Base.Attributes.:=' value ]
-- @
setAppChooserWidgetShowDefault :: (MonadIO m, IsAppChooserWidget o) => o -> Bool -> m ()
setAppChooserWidgetShowDefault obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "show-default" val

-- | Construct a `GValueConstruct` with valid value for the “@show-default@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructAppChooserWidgetShowDefault :: (IsAppChooserWidget o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructAppChooserWidgetShowDefault val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "show-default" val

#if defined(ENABLE_OVERLOADING)
data AppChooserWidgetShowDefaultPropertyInfo
instance AttrInfo AppChooserWidgetShowDefaultPropertyInfo where
    type AttrAllowedOps AppChooserWidgetShowDefaultPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint AppChooserWidgetShowDefaultPropertyInfo = IsAppChooserWidget
    type AttrSetTypeConstraint AppChooserWidgetShowDefaultPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint AppChooserWidgetShowDefaultPropertyInfo = (~) Bool
    type AttrTransferType AppChooserWidgetShowDefaultPropertyInfo = Bool
    type AttrGetType AppChooserWidgetShowDefaultPropertyInfo = Bool
    type AttrLabel AppChooserWidgetShowDefaultPropertyInfo = "show-default"
    type AttrOrigin AppChooserWidgetShowDefaultPropertyInfo = AppChooserWidget
    attrGet = getAppChooserWidgetShowDefault
    attrSet = setAppChooserWidgetShowDefault
    attrTransfer _ v = do
        return v
    attrConstruct = constructAppChooserWidgetShowDefault
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserWidget.showDefault"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserWidget.html#g:attr:showDefault"
        })
#endif

-- VVV Prop "show-fallback"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstruct]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@show-fallback@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' appChooserWidget #showFallback
-- @
getAppChooserWidgetShowFallback :: (MonadIO m, IsAppChooserWidget o) => o -> m Bool
getAppChooserWidgetShowFallback obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "show-fallback"

-- | Set the value of the “@show-fallback@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' appChooserWidget [ #showFallback 'Data.GI.Base.Attributes.:=' value ]
-- @
setAppChooserWidgetShowFallback :: (MonadIO m, IsAppChooserWidget o) => o -> Bool -> m ()
setAppChooserWidgetShowFallback obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "show-fallback" val

-- | Construct a `GValueConstruct` with valid value for the “@show-fallback@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructAppChooserWidgetShowFallback :: (IsAppChooserWidget o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructAppChooserWidgetShowFallback val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "show-fallback" val

#if defined(ENABLE_OVERLOADING)
data AppChooserWidgetShowFallbackPropertyInfo
instance AttrInfo AppChooserWidgetShowFallbackPropertyInfo where
    type AttrAllowedOps AppChooserWidgetShowFallbackPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint AppChooserWidgetShowFallbackPropertyInfo = IsAppChooserWidget
    type AttrSetTypeConstraint AppChooserWidgetShowFallbackPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint AppChooserWidgetShowFallbackPropertyInfo = (~) Bool
    type AttrTransferType AppChooserWidgetShowFallbackPropertyInfo = Bool
    type AttrGetType AppChooserWidgetShowFallbackPropertyInfo = Bool
    type AttrLabel AppChooserWidgetShowFallbackPropertyInfo = "show-fallback"
    type AttrOrigin AppChooserWidgetShowFallbackPropertyInfo = AppChooserWidget
    attrGet = getAppChooserWidgetShowFallback
    attrSet = setAppChooserWidgetShowFallback
    attrTransfer _ v = do
        return v
    attrConstruct = constructAppChooserWidgetShowFallback
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserWidget.showFallback"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserWidget.html#g:attr:showFallback"
        })
#endif

-- VVV Prop "show-other"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstruct]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@show-other@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' appChooserWidget #showOther
-- @
getAppChooserWidgetShowOther :: (MonadIO m, IsAppChooserWidget o) => o -> m Bool
getAppChooserWidgetShowOther obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "show-other"

-- | Set the value of the “@show-other@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' appChooserWidget [ #showOther 'Data.GI.Base.Attributes.:=' value ]
-- @
setAppChooserWidgetShowOther :: (MonadIO m, IsAppChooserWidget o) => o -> Bool -> m ()
setAppChooserWidgetShowOther obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "show-other" val

-- | Construct a `GValueConstruct` with valid value for the “@show-other@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructAppChooserWidgetShowOther :: (IsAppChooserWidget o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructAppChooserWidgetShowOther val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "show-other" val

#if defined(ENABLE_OVERLOADING)
data AppChooserWidgetShowOtherPropertyInfo
instance AttrInfo AppChooserWidgetShowOtherPropertyInfo where
    type AttrAllowedOps AppChooserWidgetShowOtherPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint AppChooserWidgetShowOtherPropertyInfo = IsAppChooserWidget
    type AttrSetTypeConstraint AppChooserWidgetShowOtherPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint AppChooserWidgetShowOtherPropertyInfo = (~) Bool
    type AttrTransferType AppChooserWidgetShowOtherPropertyInfo = Bool
    type AttrGetType AppChooserWidgetShowOtherPropertyInfo = Bool
    type AttrLabel AppChooserWidgetShowOtherPropertyInfo = "show-other"
    type AttrOrigin AppChooserWidgetShowOtherPropertyInfo = AppChooserWidget
    attrGet = getAppChooserWidgetShowOther
    attrSet = setAppChooserWidgetShowOther
    attrTransfer _ v = do
        return v
    attrConstruct = constructAppChooserWidgetShowOther
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserWidget.showOther"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserWidget.html#g:attr:showOther"
        })
#endif

-- VVV Prop "show-recommended"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstruct]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@show-recommended@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' appChooserWidget #showRecommended
-- @
getAppChooserWidgetShowRecommended :: (MonadIO m, IsAppChooserWidget o) => o -> m Bool
getAppChooserWidgetShowRecommended obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "show-recommended"

-- | Set the value of the “@show-recommended@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' appChooserWidget [ #showRecommended 'Data.GI.Base.Attributes.:=' value ]
-- @
setAppChooserWidgetShowRecommended :: (MonadIO m, IsAppChooserWidget o) => o -> Bool -> m ()
setAppChooserWidgetShowRecommended obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "show-recommended" val

-- | Construct a `GValueConstruct` with valid value for the “@show-recommended@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructAppChooserWidgetShowRecommended :: (IsAppChooserWidget o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructAppChooserWidgetShowRecommended val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "show-recommended" val

#if defined(ENABLE_OVERLOADING)
data AppChooserWidgetShowRecommendedPropertyInfo
instance AttrInfo AppChooserWidgetShowRecommendedPropertyInfo where
    type AttrAllowedOps AppChooserWidgetShowRecommendedPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint AppChooserWidgetShowRecommendedPropertyInfo = IsAppChooserWidget
    type AttrSetTypeConstraint AppChooserWidgetShowRecommendedPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint AppChooserWidgetShowRecommendedPropertyInfo = (~) Bool
    type AttrTransferType AppChooserWidgetShowRecommendedPropertyInfo = Bool
    type AttrGetType AppChooserWidgetShowRecommendedPropertyInfo = Bool
    type AttrLabel AppChooserWidgetShowRecommendedPropertyInfo = "show-recommended"
    type AttrOrigin AppChooserWidgetShowRecommendedPropertyInfo = AppChooserWidget
    attrGet = getAppChooserWidgetShowRecommended
    attrSet = setAppChooserWidgetShowRecommended
    attrTransfer _ v = do
        return v
    attrConstruct = constructAppChooserWidgetShowRecommended
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserWidget.showRecommended"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserWidget.html#g:attr:showRecommended"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList AppChooserWidget
type instance O.AttributeList AppChooserWidget = AppChooserWidgetAttributeList
type AppChooserWidgetAttributeList = ('[ '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("baselinePosition", Gtk.Box.BoxBaselinePositionPropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("contentType", Gtk.AppChooser.AppChooserContentTypePropertyInfo), '("defaultText", AppChooserWidgetDefaultTextPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("homogeneous", Gtk.Box.BoxHomogeneousPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("orientation", Gtk.Orientable.OrientableOrientationPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("showAll", AppChooserWidgetShowAllPropertyInfo), '("showDefault", AppChooserWidgetShowDefaultPropertyInfo), '("showFallback", AppChooserWidgetShowFallbackPropertyInfo), '("showOther", AppChooserWidgetShowOtherPropertyInfo), '("showRecommended", AppChooserWidgetShowRecommendedPropertyInfo), '("spacing", Gtk.Box.BoxSpacingPropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
appChooserWidgetDefaultText :: AttrLabelProxy "defaultText"
appChooserWidgetDefaultText = AttrLabelProxy

appChooserWidgetShowAll :: AttrLabelProxy "showAll"
appChooserWidgetShowAll = AttrLabelProxy

appChooserWidgetShowDefault :: AttrLabelProxy "showDefault"
appChooserWidgetShowDefault = AttrLabelProxy

appChooserWidgetShowFallback :: AttrLabelProxy "showFallback"
appChooserWidgetShowFallback = AttrLabelProxy

appChooserWidgetShowOther :: AttrLabelProxy "showOther"
appChooserWidgetShowOther = AttrLabelProxy

appChooserWidgetShowRecommended :: AttrLabelProxy "showRecommended"
appChooserWidgetShowRecommended = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList AppChooserWidget = AppChooserWidgetSignalList
type AppChooserWidgetSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("applicationActivated", AppChooserWidgetApplicationActivatedSignalInfo), '("applicationSelected", AppChooserWidgetApplicationSelectedSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("populatePopup", AppChooserWidgetPopulatePopupSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method AppChooserWidget::new
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "content_type"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the content type to show applications for"
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
--               (TInterface Name { namespace = "Gtk" , name = "AppChooserWidget" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_app_chooser_widget_new" gtk_app_chooser_widget_new :: 
    CString ->                              -- content_type : TBasicType TUTF8
    IO (Ptr AppChooserWidget)

-- | Creates a new t'GI.Gtk.Objects.AppChooserWidget.AppChooserWidget' for applications
-- that can handle content of the given type.
-- 
-- /Since: 3.0/
appChooserWidgetNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@contentType@/: the content type to show applications for
    -> m AppChooserWidget
    -- ^ __Returns:__ a newly created t'GI.Gtk.Objects.AppChooserWidget.AppChooserWidget'
appChooserWidgetNew contentType = liftIO $ do
    contentType' <- textToCString contentType
    result <- gtk_app_chooser_widget_new contentType'
    checkUnexpectedReturnNULL "appChooserWidgetNew" result
    result' <- (newObject AppChooserWidget) result
    freeMem contentType'
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method AppChooserWidget::get_default_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AppChooserWidget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAppChooserWidget"
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

foreign import ccall "gtk_app_chooser_widget_get_default_text" gtk_app_chooser_widget_get_default_text :: 
    Ptr AppChooserWidget ->                 -- self : TInterface (Name {namespace = "Gtk", name = "AppChooserWidget"})
    IO CString

-- | Returns the text that is shown if there are not applications
-- that can handle the content type.
-- 
-- /Since: 3.0/
appChooserWidgetGetDefaultText ::
    (B.CallStack.HasCallStack, MonadIO m, IsAppChooserWidget a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.AppChooserWidget.AppChooserWidget'
    -> m T.Text
    -- ^ __Returns:__ the value of [AppChooserWidget:defaultText]("GI.Gtk.Objects.AppChooserWidget#g:attr:defaultText")
appChooserWidgetGetDefaultText self = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    result <- gtk_app_chooser_widget_get_default_text self'
    checkUnexpectedReturnNULL "appChooserWidgetGetDefaultText" result
    result' <- cstringToText result
    touchManagedPtr self
    return result'

#if defined(ENABLE_OVERLOADING)
data AppChooserWidgetGetDefaultTextMethodInfo
instance (signature ~ (m T.Text), MonadIO m, IsAppChooserWidget a) => O.OverloadedMethod AppChooserWidgetGetDefaultTextMethodInfo a signature where
    overloadedMethod = appChooserWidgetGetDefaultText

instance O.OverloadedMethodInfo AppChooserWidgetGetDefaultTextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserWidget.appChooserWidgetGetDefaultText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserWidget.html#v:appChooserWidgetGetDefaultText"
        })


#endif

-- method AppChooserWidget::get_show_all
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AppChooserWidget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAppChooserWidget"
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

foreign import ccall "gtk_app_chooser_widget_get_show_all" gtk_app_chooser_widget_get_show_all :: 
    Ptr AppChooserWidget ->                 -- self : TInterface (Name {namespace = "Gtk", name = "AppChooserWidget"})
    IO CInt

-- | Returns the current value of the [AppChooserWidget:showAll]("GI.Gtk.Objects.AppChooserWidget#g:attr:showAll")
-- property.
-- 
-- /Since: 3.0/
appChooserWidgetGetShowAll ::
    (B.CallStack.HasCallStack, MonadIO m, IsAppChooserWidget a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.AppChooserWidget.AppChooserWidget'
    -> m Bool
    -- ^ __Returns:__ the value of [AppChooserWidget:showAll]("GI.Gtk.Objects.AppChooserWidget#g:attr:showAll")
appChooserWidgetGetShowAll self = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    result <- gtk_app_chooser_widget_get_show_all self'
    let result' = (/= 0) result
    touchManagedPtr self
    return result'

#if defined(ENABLE_OVERLOADING)
data AppChooserWidgetGetShowAllMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsAppChooserWidget a) => O.OverloadedMethod AppChooserWidgetGetShowAllMethodInfo a signature where
    overloadedMethod = appChooserWidgetGetShowAll

instance O.OverloadedMethodInfo AppChooserWidgetGetShowAllMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserWidget.appChooserWidgetGetShowAll",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserWidget.html#v:appChooserWidgetGetShowAll"
        })


#endif

-- method AppChooserWidget::get_show_default
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AppChooserWidget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAppChooserWidget"
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

foreign import ccall "gtk_app_chooser_widget_get_show_default" gtk_app_chooser_widget_get_show_default :: 
    Ptr AppChooserWidget ->                 -- self : TInterface (Name {namespace = "Gtk", name = "AppChooserWidget"})
    IO CInt

-- | Returns the current value of the [AppChooserWidget:showDefault]("GI.Gtk.Objects.AppChooserWidget#g:attr:showDefault")
-- property.
-- 
-- /Since: 3.0/
appChooserWidgetGetShowDefault ::
    (B.CallStack.HasCallStack, MonadIO m, IsAppChooserWidget a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.AppChooserWidget.AppChooserWidget'
    -> m Bool
    -- ^ __Returns:__ the value of [AppChooserWidget:showDefault]("GI.Gtk.Objects.AppChooserWidget#g:attr:showDefault")
appChooserWidgetGetShowDefault self = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    result <- gtk_app_chooser_widget_get_show_default self'
    let result' = (/= 0) result
    touchManagedPtr self
    return result'

#if defined(ENABLE_OVERLOADING)
data AppChooserWidgetGetShowDefaultMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsAppChooserWidget a) => O.OverloadedMethod AppChooserWidgetGetShowDefaultMethodInfo a signature where
    overloadedMethod = appChooserWidgetGetShowDefault

instance O.OverloadedMethodInfo AppChooserWidgetGetShowDefaultMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserWidget.appChooserWidgetGetShowDefault",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserWidget.html#v:appChooserWidgetGetShowDefault"
        })


#endif

-- method AppChooserWidget::get_show_fallback
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AppChooserWidget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAppChooserWidget"
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

foreign import ccall "gtk_app_chooser_widget_get_show_fallback" gtk_app_chooser_widget_get_show_fallback :: 
    Ptr AppChooserWidget ->                 -- self : TInterface (Name {namespace = "Gtk", name = "AppChooserWidget"})
    IO CInt

-- | Returns the current value of the [AppChooserWidget:showFallback]("GI.Gtk.Objects.AppChooserWidget#g:attr:showFallback")
-- property.
-- 
-- /Since: 3.0/
appChooserWidgetGetShowFallback ::
    (B.CallStack.HasCallStack, MonadIO m, IsAppChooserWidget a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.AppChooserWidget.AppChooserWidget'
    -> m Bool
    -- ^ __Returns:__ the value of [AppChooserWidget:showFallback]("GI.Gtk.Objects.AppChooserWidget#g:attr:showFallback")
appChooserWidgetGetShowFallback self = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    result <- gtk_app_chooser_widget_get_show_fallback self'
    let result' = (/= 0) result
    touchManagedPtr self
    return result'

#if defined(ENABLE_OVERLOADING)
data AppChooserWidgetGetShowFallbackMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsAppChooserWidget a) => O.OverloadedMethod AppChooserWidgetGetShowFallbackMethodInfo a signature where
    overloadedMethod = appChooserWidgetGetShowFallback

instance O.OverloadedMethodInfo AppChooserWidgetGetShowFallbackMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserWidget.appChooserWidgetGetShowFallback",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserWidget.html#v:appChooserWidgetGetShowFallback"
        })


#endif

-- method AppChooserWidget::get_show_other
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AppChooserWidget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAppChooserWidget"
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

foreign import ccall "gtk_app_chooser_widget_get_show_other" gtk_app_chooser_widget_get_show_other :: 
    Ptr AppChooserWidget ->                 -- self : TInterface (Name {namespace = "Gtk", name = "AppChooserWidget"})
    IO CInt

-- | Returns the current value of the [AppChooserWidget:showOther]("GI.Gtk.Objects.AppChooserWidget#g:attr:showOther")
-- property.
-- 
-- /Since: 3.0/
appChooserWidgetGetShowOther ::
    (B.CallStack.HasCallStack, MonadIO m, IsAppChooserWidget a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.AppChooserWidget.AppChooserWidget'
    -> m Bool
    -- ^ __Returns:__ the value of [AppChooserWidget:showOther]("GI.Gtk.Objects.AppChooserWidget#g:attr:showOther")
appChooserWidgetGetShowOther self = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    result <- gtk_app_chooser_widget_get_show_other self'
    let result' = (/= 0) result
    touchManagedPtr self
    return result'

#if defined(ENABLE_OVERLOADING)
data AppChooserWidgetGetShowOtherMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsAppChooserWidget a) => O.OverloadedMethod AppChooserWidgetGetShowOtherMethodInfo a signature where
    overloadedMethod = appChooserWidgetGetShowOther

instance O.OverloadedMethodInfo AppChooserWidgetGetShowOtherMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserWidget.appChooserWidgetGetShowOther",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserWidget.html#v:appChooserWidgetGetShowOther"
        })


#endif

-- method AppChooserWidget::get_show_recommended
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AppChooserWidget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAppChooserWidget"
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

foreign import ccall "gtk_app_chooser_widget_get_show_recommended" gtk_app_chooser_widget_get_show_recommended :: 
    Ptr AppChooserWidget ->                 -- self : TInterface (Name {namespace = "Gtk", name = "AppChooserWidget"})
    IO CInt

-- | Returns the current value of the [AppChooserWidget:showRecommended]("GI.Gtk.Objects.AppChooserWidget#g:attr:showRecommended")
-- property.
-- 
-- /Since: 3.0/
appChooserWidgetGetShowRecommended ::
    (B.CallStack.HasCallStack, MonadIO m, IsAppChooserWidget a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.AppChooserWidget.AppChooserWidget'
    -> m Bool
    -- ^ __Returns:__ the value of [AppChooserWidget:showRecommended]("GI.Gtk.Objects.AppChooserWidget#g:attr:showRecommended")
appChooserWidgetGetShowRecommended self = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    result <- gtk_app_chooser_widget_get_show_recommended self'
    let result' = (/= 0) result
    touchManagedPtr self
    return result'

#if defined(ENABLE_OVERLOADING)
data AppChooserWidgetGetShowRecommendedMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsAppChooserWidget a) => O.OverloadedMethod AppChooserWidgetGetShowRecommendedMethodInfo a signature where
    overloadedMethod = appChooserWidgetGetShowRecommended

instance O.OverloadedMethodInfo AppChooserWidgetGetShowRecommendedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserWidget.appChooserWidgetGetShowRecommended",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserWidget.html#v:appChooserWidgetGetShowRecommended"
        })


#endif

-- method AppChooserWidget::set_default_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AppChooserWidget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAppChooserWidget"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "text"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the new value for #GtkAppChooserWidget:default-text"
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

foreign import ccall "gtk_app_chooser_widget_set_default_text" gtk_app_chooser_widget_set_default_text :: 
    Ptr AppChooserWidget ->                 -- self : TInterface (Name {namespace = "Gtk", name = "AppChooserWidget"})
    CString ->                              -- text : TBasicType TUTF8
    IO ()

-- | Sets the text that is shown if there are not applications
-- that can handle the content type.
appChooserWidgetSetDefaultText ::
    (B.CallStack.HasCallStack, MonadIO m, IsAppChooserWidget a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.AppChooserWidget.AppChooserWidget'
    -> T.Text
    -- ^ /@text@/: the new value for [AppChooserWidget:defaultText]("GI.Gtk.Objects.AppChooserWidget#g:attr:defaultText")
    -> m ()
appChooserWidgetSetDefaultText self text = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    text' <- textToCString text
    gtk_app_chooser_widget_set_default_text self' text'
    touchManagedPtr self
    freeMem text'
    return ()

#if defined(ENABLE_OVERLOADING)
data AppChooserWidgetSetDefaultTextMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsAppChooserWidget a) => O.OverloadedMethod AppChooserWidgetSetDefaultTextMethodInfo a signature where
    overloadedMethod = appChooserWidgetSetDefaultText

instance O.OverloadedMethodInfo AppChooserWidgetSetDefaultTextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserWidget.appChooserWidgetSetDefaultText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserWidget.html#v:appChooserWidgetSetDefaultText"
        })


#endif

-- method AppChooserWidget::set_show_all
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AppChooserWidget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAppChooserWidget"
--                 , sinceVersion = Nothing
--                 }
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
--                     Just "the new value for #GtkAppChooserWidget:show-all"
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

foreign import ccall "gtk_app_chooser_widget_set_show_all" gtk_app_chooser_widget_set_show_all :: 
    Ptr AppChooserWidget ->                 -- self : TInterface (Name {namespace = "Gtk", name = "AppChooserWidget"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | Sets whether the app chooser should show all applications
-- in a flat list.
-- 
-- /Since: 3.0/
appChooserWidgetSetShowAll ::
    (B.CallStack.HasCallStack, MonadIO m, IsAppChooserWidget a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.AppChooserWidget.AppChooserWidget'
    -> Bool
    -- ^ /@setting@/: the new value for [AppChooserWidget:showAll]("GI.Gtk.Objects.AppChooserWidget#g:attr:showAll")
    -> m ()
appChooserWidgetSetShowAll self setting = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    let setting' = (fromIntegral . fromEnum) setting
    gtk_app_chooser_widget_set_show_all self' setting'
    touchManagedPtr self
    return ()

#if defined(ENABLE_OVERLOADING)
data AppChooserWidgetSetShowAllMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsAppChooserWidget a) => O.OverloadedMethod AppChooserWidgetSetShowAllMethodInfo a signature where
    overloadedMethod = appChooserWidgetSetShowAll

instance O.OverloadedMethodInfo AppChooserWidgetSetShowAllMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserWidget.appChooserWidgetSetShowAll",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserWidget.html#v:appChooserWidgetSetShowAll"
        })


#endif

-- method AppChooserWidget::set_show_default
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AppChooserWidget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAppChooserWidget"
--                 , sinceVersion = Nothing
--                 }
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
--                     Just "the new value for #GtkAppChooserWidget:show-default"
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

foreign import ccall "gtk_app_chooser_widget_set_show_default" gtk_app_chooser_widget_set_show_default :: 
    Ptr AppChooserWidget ->                 -- self : TInterface (Name {namespace = "Gtk", name = "AppChooserWidget"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | Sets whether the app chooser should show the default handler
-- for the content type in a separate section.
-- 
-- /Since: 3.0/
appChooserWidgetSetShowDefault ::
    (B.CallStack.HasCallStack, MonadIO m, IsAppChooserWidget a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.AppChooserWidget.AppChooserWidget'
    -> Bool
    -- ^ /@setting@/: the new value for [AppChooserWidget:showDefault]("GI.Gtk.Objects.AppChooserWidget#g:attr:showDefault")
    -> m ()
appChooserWidgetSetShowDefault self setting = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    let setting' = (fromIntegral . fromEnum) setting
    gtk_app_chooser_widget_set_show_default self' setting'
    touchManagedPtr self
    return ()

#if defined(ENABLE_OVERLOADING)
data AppChooserWidgetSetShowDefaultMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsAppChooserWidget a) => O.OverloadedMethod AppChooserWidgetSetShowDefaultMethodInfo a signature where
    overloadedMethod = appChooserWidgetSetShowDefault

instance O.OverloadedMethodInfo AppChooserWidgetSetShowDefaultMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserWidget.appChooserWidgetSetShowDefault",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserWidget.html#v:appChooserWidgetSetShowDefault"
        })


#endif

-- method AppChooserWidget::set_show_fallback
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AppChooserWidget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAppChooserWidget"
--                 , sinceVersion = Nothing
--                 }
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
--                     Just "the new value for #GtkAppChooserWidget:show-fallback"
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

foreign import ccall "gtk_app_chooser_widget_set_show_fallback" gtk_app_chooser_widget_set_show_fallback :: 
    Ptr AppChooserWidget ->                 -- self : TInterface (Name {namespace = "Gtk", name = "AppChooserWidget"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | Sets whether the app chooser should show related applications
-- for the content type in a separate section.
-- 
-- /Since: 3.0/
appChooserWidgetSetShowFallback ::
    (B.CallStack.HasCallStack, MonadIO m, IsAppChooserWidget a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.AppChooserWidget.AppChooserWidget'
    -> Bool
    -- ^ /@setting@/: the new value for [AppChooserWidget:showFallback]("GI.Gtk.Objects.AppChooserWidget#g:attr:showFallback")
    -> m ()
appChooserWidgetSetShowFallback self setting = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    let setting' = (fromIntegral . fromEnum) setting
    gtk_app_chooser_widget_set_show_fallback self' setting'
    touchManagedPtr self
    return ()

#if defined(ENABLE_OVERLOADING)
data AppChooserWidgetSetShowFallbackMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsAppChooserWidget a) => O.OverloadedMethod AppChooserWidgetSetShowFallbackMethodInfo a signature where
    overloadedMethod = appChooserWidgetSetShowFallback

instance O.OverloadedMethodInfo AppChooserWidgetSetShowFallbackMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserWidget.appChooserWidgetSetShowFallback",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserWidget.html#v:appChooserWidgetSetShowFallback"
        })


#endif

-- method AppChooserWidget::set_show_other
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AppChooserWidget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAppChooserWidget"
--                 , sinceVersion = Nothing
--                 }
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
--                     Just "the new value for #GtkAppChooserWidget:show-other"
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

foreign import ccall "gtk_app_chooser_widget_set_show_other" gtk_app_chooser_widget_set_show_other :: 
    Ptr AppChooserWidget ->                 -- self : TInterface (Name {namespace = "Gtk", name = "AppChooserWidget"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | Sets whether the app chooser should show applications
-- which are unrelated to the content type.
-- 
-- /Since: 3.0/
appChooserWidgetSetShowOther ::
    (B.CallStack.HasCallStack, MonadIO m, IsAppChooserWidget a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.AppChooserWidget.AppChooserWidget'
    -> Bool
    -- ^ /@setting@/: the new value for [AppChooserWidget:showOther]("GI.Gtk.Objects.AppChooserWidget#g:attr:showOther")
    -> m ()
appChooserWidgetSetShowOther self setting = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    let setting' = (fromIntegral . fromEnum) setting
    gtk_app_chooser_widget_set_show_other self' setting'
    touchManagedPtr self
    return ()

#if defined(ENABLE_OVERLOADING)
data AppChooserWidgetSetShowOtherMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsAppChooserWidget a) => O.OverloadedMethod AppChooserWidgetSetShowOtherMethodInfo a signature where
    overloadedMethod = appChooserWidgetSetShowOther

instance O.OverloadedMethodInfo AppChooserWidgetSetShowOtherMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserWidget.appChooserWidgetSetShowOther",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserWidget.html#v:appChooserWidgetSetShowOther"
        })


#endif

-- method AppChooserWidget::set_show_recommended
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AppChooserWidget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAppChooserWidget"
--                 , sinceVersion = Nothing
--                 }
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
--                     Just "the new value for #GtkAppChooserWidget:show-recommended"
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

foreign import ccall "gtk_app_chooser_widget_set_show_recommended" gtk_app_chooser_widget_set_show_recommended :: 
    Ptr AppChooserWidget ->                 -- self : TInterface (Name {namespace = "Gtk", name = "AppChooserWidget"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | Sets whether the app chooser should show recommended applications
-- for the content type in a separate section.
-- 
-- /Since: 3.0/
appChooserWidgetSetShowRecommended ::
    (B.CallStack.HasCallStack, MonadIO m, IsAppChooserWidget a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.AppChooserWidget.AppChooserWidget'
    -> Bool
    -- ^ /@setting@/: the new value for [AppChooserWidget:showRecommended]("GI.Gtk.Objects.AppChooserWidget#g:attr:showRecommended")
    -> m ()
appChooserWidgetSetShowRecommended self setting = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    let setting' = (fromIntegral . fromEnum) setting
    gtk_app_chooser_widget_set_show_recommended self' setting'
    touchManagedPtr self
    return ()

#if defined(ENABLE_OVERLOADING)
data AppChooserWidgetSetShowRecommendedMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsAppChooserWidget a) => O.OverloadedMethod AppChooserWidgetSetShowRecommendedMethodInfo a signature where
    overloadedMethod = appChooserWidgetSetShowRecommended

instance O.OverloadedMethodInfo AppChooserWidgetSetShowRecommendedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserWidget.appChooserWidgetSetShowRecommended",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserWidget.html#v:appChooserWidgetSetShowRecommended"
        })


#endif


