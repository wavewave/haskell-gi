{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A t'GI.Gtk.Objects.Assistant.Assistant' is a widget used to represent a generally complex
-- operation splitted in several steps, guiding the user through its
-- pages and controlling the page flow to collect the necessary data.
-- 
-- The design of GtkAssistant is that it controls what buttons to show
-- and to make sensitive, based on what it knows about the page sequence
-- and the [type][GtkAssistantPageType] of each page,
-- in addition to state information like the page
-- [completion][gtk-assistant-set-page-complete]
-- and [committed][gtk-assistant-commit] status.
-- 
-- If you have a case that doesn’t quite fit in @/GtkAssistants/@ way of
-- handling buttons, you can use the @/GTK_ASSISTANT_PAGE_CUSTOM/@ page
-- type and handle buttons yourself.
-- 
-- = GtkAssistant as GtkBuildable
-- 
-- The GtkAssistant implementation of the t'GI.Gtk.Interfaces.Buildable.Buildable' interface
-- exposes the /@actionArea@/ as internal children with the name
-- “action_area”.
-- 
-- To add pages to an assistant in t'GI.Gtk.Objects.Builder.Builder', simply add it as a
-- child to the GtkAssistant object, and set its child properties
-- as necessary.
-- 
-- = CSS nodes
-- 
-- GtkAssistant has a single CSS node with the name assistant.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.Assistant
    ( 

-- * Exported types
    Assistant(..)                           ,
    IsAssistant                             ,
    toAssistant                             ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [activateDefault]("GI.Gtk.Objects.Window#g:method:activateDefault"), [activateFocus]("GI.Gtk.Objects.Window#g:method:activateFocus"), [activateKey]("GI.Gtk.Objects.Window#g:method:activateKey"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelGroup]("GI.Gtk.Objects.Window#g:method:addAccelGroup"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addActionWidget]("GI.Gtk.Objects.Assistant#g:method:addActionWidget"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonic]("GI.Gtk.Objects.Window#g:method:addMnemonic"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [appendPage]("GI.Gtk.Objects.Assistant#g:method:appendPage"), [beginMoveDrag]("GI.Gtk.Objects.Window#g:method:beginMoveDrag"), [beginResizeDrag]("GI.Gtk.Objects.Window#g:method:beginResizeDrag"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [close]("GI.Gtk.Objects.Window#g:method:close"), [commit]("GI.Gtk.Objects.Assistant#g:method:commit"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [deiconify]("GI.Gtk.Objects.Window#g:method:deiconify"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [fullscreen]("GI.Gtk.Objects.Window#g:method:fullscreen"), [fullscreenOnMonitor]("GI.Gtk.Objects.Window#g:method:fullscreenOnMonitor"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasGroup]("GI.Gtk.Objects.Window#g:method:hasGroup"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasToplevelFocus]("GI.Gtk.Objects.Window#g:method:hasToplevelFocus"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [iconify]("GI.Gtk.Objects.Window#g:method:iconify"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [insertPage]("GI.Gtk.Objects.Assistant#g:method:insertPage"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isActive]("GI.Gtk.Objects.Window#g:method:isActive"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isMaximized]("GI.Gtk.Objects.Window#g:method:isMaximized"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [maximize]("GI.Gtk.Objects.Window#g:method:maximize"), [mnemonicActivate]("GI.Gtk.Objects.Window#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [move]("GI.Gtk.Objects.Window#g:method:move"), [nextPage]("GI.Gtk.Objects.Assistant#g:method:nextPage"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parseGeometry]("GI.Gtk.Objects.Window#g:method:parseGeometry"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [prependPage]("GI.Gtk.Objects.Assistant#g:method:prependPage"), [present]("GI.Gtk.Objects.Window#g:method:present"), [presentWithTime]("GI.Gtk.Objects.Window#g:method:presentWithTime"), [previousPage]("GI.Gtk.Objects.Assistant#g:method:previousPage"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [propagateKeyEvent]("GI.Gtk.Objects.Window#g:method:propagateKeyEvent"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelGroup]("GI.Gtk.Objects.Window#g:method:removeAccelGroup"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeActionWidget]("GI.Gtk.Objects.Assistant#g:method:removeActionWidget"), [removeMnemonic]("GI.Gtk.Objects.Window#g:method:removeMnemonic"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removePage]("GI.Gtk.Objects.Assistant#g:method:removePage"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [reshowWithInitialSize]("GI.Gtk.Objects.Window#g:method:reshowWithInitialSize"), [resize]("GI.Gtk.Objects.Window#g:method:resize"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [resizeGripIsVisible]("GI.Gtk.Objects.Window#g:method:resizeGripIsVisible"), [resizeToGeometry]("GI.Gtk.Objects.Window#g:method:resizeToGeometry"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [stick]("GI.Gtk.Objects.Window#g:method:stick"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unfullscreen]("GI.Gtk.Objects.Window#g:method:unfullscreen"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unmaximize]("GI.Gtk.Objects.Window#g:method:unmaximize"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [unstick]("GI.Gtk.Objects.Window#g:method:unstick"), [updateButtonsState]("GI.Gtk.Objects.Assistant#g:method:updateButtonsState"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAcceptFocus]("GI.Gtk.Objects.Window#g:method:getAcceptFocus"), [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getApplication]("GI.Gtk.Objects.Window#g:method:getApplication"), [getAttachedTo]("GI.Gtk.Objects.Window#g:method:getAttachedTo"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChild]("GI.Gtk.Objects.Bin#g:method:getChild"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getCurrentPage]("GI.Gtk.Objects.Assistant#g:method:getCurrentPage"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDecorated]("GI.Gtk.Objects.Window#g:method:getDecorated"), [getDefaultSize]("GI.Gtk.Objects.Window#g:method:getDefaultSize"), [getDefaultWidget]("GI.Gtk.Objects.Window#g:method:getDefaultWidget"), [getDeletable]("GI.Gtk.Objects.Window#g:method:getDeletable"), [getDestroyWithParent]("GI.Gtk.Objects.Window#g:method:getDestroyWithParent"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocus]("GI.Gtk.Objects.Window#g:method:getFocus"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFocusOnMap]("GI.Gtk.Objects.Window#g:method:getFocusOnMap"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFocusVisible]("GI.Gtk.Objects.Window#g:method:getFocusVisible"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getGravity]("GI.Gtk.Objects.Window#g:method:getGravity"), [getGroup]("GI.Gtk.Objects.Window#g:method:getGroup"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasResizeGrip]("GI.Gtk.Objects.Window#g:method:getHasResizeGrip"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getHideTitlebarWhenMaximized]("GI.Gtk.Objects.Window#g:method:getHideTitlebarWhenMaximized"), [getIcon]("GI.Gtk.Objects.Window#g:method:getIcon"), [getIconList]("GI.Gtk.Objects.Window#g:method:getIconList"), [getIconName]("GI.Gtk.Objects.Window#g:method:getIconName"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getMnemonicModifier]("GI.Gtk.Objects.Window#g:method:getMnemonicModifier"), [getMnemonicsVisible]("GI.Gtk.Objects.Window#g:method:getMnemonicsVisible"), [getModal]("GI.Gtk.Objects.Window#g:method:getModal"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getNPages]("GI.Gtk.Objects.Assistant#g:method:getNPages"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getNthPage]("GI.Gtk.Objects.Assistant#g:method:getNthPage"), [getOpacity]("GI.Gtk.Objects.Window#g:method:getOpacity"), [getPageComplete]("GI.Gtk.Objects.Assistant#g:method:getPageComplete"), [getPageHasPadding]("GI.Gtk.Objects.Assistant#g:method:getPageHasPadding"), [getPageHeaderImage]("GI.Gtk.Objects.Assistant#g:method:getPageHeaderImage"), [getPageSideImage]("GI.Gtk.Objects.Assistant#g:method:getPageSideImage"), [getPageTitle]("GI.Gtk.Objects.Assistant#g:method:getPageTitle"), [getPageType]("GI.Gtk.Objects.Assistant#g:method:getPageType"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPosition]("GI.Gtk.Objects.Window#g:method:getPosition"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizable]("GI.Gtk.Objects.Window#g:method:getResizable"), [getResizeGripArea]("GI.Gtk.Objects.Window#g:method:getResizeGripArea"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRole]("GI.Gtk.Objects.Window#g:method:getRole"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Window#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSize]("GI.Gtk.Objects.Window#g:method:getSize"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getSkipPagerHint]("GI.Gtk.Objects.Window#g:method:getSkipPagerHint"), [getSkipTaskbarHint]("GI.Gtk.Objects.Window#g:method:getSkipTaskbarHint"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTitle]("GI.Gtk.Objects.Window#g:method:getTitle"), [getTitlebar]("GI.Gtk.Objects.Window#g:method:getTitlebar"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getTransientFor]("GI.Gtk.Objects.Window#g:method:getTransientFor"), [getTypeHint]("GI.Gtk.Objects.Window#g:method:getTypeHint"), [getUrgencyHint]("GI.Gtk.Objects.Window#g:method:getUrgencyHint"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow"), [getWindowType]("GI.Gtk.Objects.Window#g:method:getWindowType").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setAcceptFocus]("GI.Gtk.Objects.Window#g:method:setAcceptFocus"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setApplication]("GI.Gtk.Objects.Window#g:method:setApplication"), [setAttachedTo]("GI.Gtk.Objects.Window#g:method:setAttachedTo"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setCurrentPage]("GI.Gtk.Objects.Assistant#g:method:setCurrentPage"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDecorated]("GI.Gtk.Objects.Window#g:method:setDecorated"), [setDefault]("GI.Gtk.Objects.Window#g:method:setDefault"), [setDefaultGeometry]("GI.Gtk.Objects.Window#g:method:setDefaultGeometry"), [setDefaultSize]("GI.Gtk.Objects.Window#g:method:setDefaultSize"), [setDeletable]("GI.Gtk.Objects.Window#g:method:setDeletable"), [setDestroyWithParent]("GI.Gtk.Objects.Window#g:method:setDestroyWithParent"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocus]("GI.Gtk.Objects.Window#g:method:setFocus"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFocusOnMap]("GI.Gtk.Objects.Window#g:method:setFocusOnMap"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFocusVisible]("GI.Gtk.Objects.Window#g:method:setFocusVisible"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setForwardPageFunc]("GI.Gtk.Objects.Assistant#g:method:setForwardPageFunc"), [setGeometryHints]("GI.Gtk.Objects.Window#g:method:setGeometryHints"), [setGravity]("GI.Gtk.Objects.Window#g:method:setGravity"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasResizeGrip]("GI.Gtk.Objects.Window#g:method:setHasResizeGrip"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasUserRefCount]("GI.Gtk.Objects.Window#g:method:setHasUserRefCount"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setHideTitlebarWhenMaximized]("GI.Gtk.Objects.Window#g:method:setHideTitlebarWhenMaximized"), [setIcon]("GI.Gtk.Objects.Window#g:method:setIcon"), [setIconFromFile]("GI.Gtk.Objects.Window#g:method:setIconFromFile"), [setIconList]("GI.Gtk.Objects.Window#g:method:setIconList"), [setIconName]("GI.Gtk.Objects.Window#g:method:setIconName"), [setKeepAbove]("GI.Gtk.Objects.Window#g:method:setKeepAbove"), [setKeepBelow]("GI.Gtk.Objects.Window#g:method:setKeepBelow"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setMnemonicModifier]("GI.Gtk.Objects.Window#g:method:setMnemonicModifier"), [setMnemonicsVisible]("GI.Gtk.Objects.Window#g:method:setMnemonicsVisible"), [setModal]("GI.Gtk.Objects.Window#g:method:setModal"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Window#g:method:setOpacity"), [setPageComplete]("GI.Gtk.Objects.Assistant#g:method:setPageComplete"), [setPageHasPadding]("GI.Gtk.Objects.Assistant#g:method:setPageHasPadding"), [setPageHeaderImage]("GI.Gtk.Objects.Assistant#g:method:setPageHeaderImage"), [setPageSideImage]("GI.Gtk.Objects.Assistant#g:method:setPageSideImage"), [setPageTitle]("GI.Gtk.Objects.Assistant#g:method:setPageTitle"), [setPageType]("GI.Gtk.Objects.Assistant#g:method:setPageType"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setPosition]("GI.Gtk.Objects.Window#g:method:setPosition"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setResizable]("GI.Gtk.Objects.Window#g:method:setResizable"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setRole]("GI.Gtk.Objects.Window#g:method:setRole"), [setScreen]("GI.Gtk.Objects.Window#g:method:setScreen"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setSkipPagerHint]("GI.Gtk.Objects.Window#g:method:setSkipPagerHint"), [setSkipTaskbarHint]("GI.Gtk.Objects.Window#g:method:setSkipTaskbarHint"), [setStartupId]("GI.Gtk.Objects.Window#g:method:setStartupId"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTitle]("GI.Gtk.Objects.Window#g:method:setTitle"), [setTitlebar]("GI.Gtk.Objects.Window#g:method:setTitlebar"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setTransientFor]("GI.Gtk.Objects.Window#g:method:setTransientFor"), [setTypeHint]("GI.Gtk.Objects.Window#g:method:setTypeHint"), [setUrgencyHint]("GI.Gtk.Objects.Window#g:method:setUrgencyHint"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow"), [setWmclass]("GI.Gtk.Objects.Window#g:method:setWmclass").

#if defined(ENABLE_OVERLOADING)
    ResolveAssistantMethod                  ,
#endif

-- ** addActionWidget #method:addActionWidget#

#if defined(ENABLE_OVERLOADING)
    AssistantAddActionWidgetMethodInfo      ,
#endif
    assistantAddActionWidget                ,


-- ** appendPage #method:appendPage#

#if defined(ENABLE_OVERLOADING)
    AssistantAppendPageMethodInfo           ,
#endif
    assistantAppendPage                     ,


-- ** commit #method:commit#

#if defined(ENABLE_OVERLOADING)
    AssistantCommitMethodInfo               ,
#endif
    assistantCommit                         ,


-- ** getCurrentPage #method:getCurrentPage#

#if defined(ENABLE_OVERLOADING)
    AssistantGetCurrentPageMethodInfo       ,
#endif
    assistantGetCurrentPage                 ,


-- ** getNPages #method:getNPages#

#if defined(ENABLE_OVERLOADING)
    AssistantGetNPagesMethodInfo            ,
#endif
    assistantGetNPages                      ,


-- ** getNthPage #method:getNthPage#

#if defined(ENABLE_OVERLOADING)
    AssistantGetNthPageMethodInfo           ,
#endif
    assistantGetNthPage                     ,


-- ** getPageComplete #method:getPageComplete#

#if defined(ENABLE_OVERLOADING)
    AssistantGetPageCompleteMethodInfo      ,
#endif
    assistantGetPageComplete                ,


-- ** getPageHasPadding #method:getPageHasPadding#

#if defined(ENABLE_OVERLOADING)
    AssistantGetPageHasPaddingMethodInfo    ,
#endif
    assistantGetPageHasPadding              ,


-- ** getPageHeaderImage #method:getPageHeaderImage#

#if defined(ENABLE_OVERLOADING)
    AssistantGetPageHeaderImageMethodInfo   ,
#endif
    assistantGetPageHeaderImage             ,


-- ** getPageSideImage #method:getPageSideImage#

#if defined(ENABLE_OVERLOADING)
    AssistantGetPageSideImageMethodInfo     ,
#endif
    assistantGetPageSideImage               ,


-- ** getPageTitle #method:getPageTitle#

#if defined(ENABLE_OVERLOADING)
    AssistantGetPageTitleMethodInfo         ,
#endif
    assistantGetPageTitle                   ,


-- ** getPageType #method:getPageType#

#if defined(ENABLE_OVERLOADING)
    AssistantGetPageTypeMethodInfo          ,
#endif
    assistantGetPageType                    ,


-- ** insertPage #method:insertPage#

#if defined(ENABLE_OVERLOADING)
    AssistantInsertPageMethodInfo           ,
#endif
    assistantInsertPage                     ,


-- ** new #method:new#

    assistantNew                            ,


-- ** nextPage #method:nextPage#

#if defined(ENABLE_OVERLOADING)
    AssistantNextPageMethodInfo             ,
#endif
    assistantNextPage                       ,


-- ** prependPage #method:prependPage#

#if defined(ENABLE_OVERLOADING)
    AssistantPrependPageMethodInfo          ,
#endif
    assistantPrependPage                    ,


-- ** previousPage #method:previousPage#

#if defined(ENABLE_OVERLOADING)
    AssistantPreviousPageMethodInfo         ,
#endif
    assistantPreviousPage                   ,


-- ** removeActionWidget #method:removeActionWidget#

#if defined(ENABLE_OVERLOADING)
    AssistantRemoveActionWidgetMethodInfo   ,
#endif
    assistantRemoveActionWidget             ,


-- ** removePage #method:removePage#

#if defined(ENABLE_OVERLOADING)
    AssistantRemovePageMethodInfo           ,
#endif
    assistantRemovePage                     ,


-- ** setCurrentPage #method:setCurrentPage#

#if defined(ENABLE_OVERLOADING)
    AssistantSetCurrentPageMethodInfo       ,
#endif
    assistantSetCurrentPage                 ,


-- ** setForwardPageFunc #method:setForwardPageFunc#

#if defined(ENABLE_OVERLOADING)
    AssistantSetForwardPageFuncMethodInfo   ,
#endif
    assistantSetForwardPageFunc             ,


-- ** setPageComplete #method:setPageComplete#

#if defined(ENABLE_OVERLOADING)
    AssistantSetPageCompleteMethodInfo      ,
#endif
    assistantSetPageComplete                ,


-- ** setPageHasPadding #method:setPageHasPadding#

#if defined(ENABLE_OVERLOADING)
    AssistantSetPageHasPaddingMethodInfo    ,
#endif
    assistantSetPageHasPadding              ,


-- ** setPageHeaderImage #method:setPageHeaderImage#

#if defined(ENABLE_OVERLOADING)
    AssistantSetPageHeaderImageMethodInfo   ,
#endif
    assistantSetPageHeaderImage             ,


-- ** setPageSideImage #method:setPageSideImage#

#if defined(ENABLE_OVERLOADING)
    AssistantSetPageSideImageMethodInfo     ,
#endif
    assistantSetPageSideImage               ,


-- ** setPageTitle #method:setPageTitle#

#if defined(ENABLE_OVERLOADING)
    AssistantSetPageTitleMethodInfo         ,
#endif
    assistantSetPageTitle                   ,


-- ** setPageType #method:setPageType#

#if defined(ENABLE_OVERLOADING)
    AssistantSetPageTypeMethodInfo          ,
#endif
    assistantSetPageType                    ,


-- ** updateButtonsState #method:updateButtonsState#

#if defined(ENABLE_OVERLOADING)
    AssistantUpdateButtonsStateMethodInfo   ,
#endif
    assistantUpdateButtonsState             ,




 -- * Properties


-- ** useHeaderBar #attr:useHeaderBar#
-- | 'P.True' if the assistant uses a t'GI.Gtk.Objects.HeaderBar.HeaderBar' for action buttons
-- instead of the action-area.
-- 
-- For technical reasons, this property is declared as an integer
-- property, but you should only set it to 'P.True' or 'P.False'.
-- 
-- /Since: 3.12/

#if defined(ENABLE_OVERLOADING)
    AssistantUseHeaderBarPropertyInfo       ,
#endif
#if defined(ENABLE_OVERLOADING)
    assistantUseHeaderBar                   ,
#endif
    constructAssistantUseHeaderBar          ,
    getAssistantUseHeaderBar                ,




 -- * Signals


-- ** apply #signal:apply#

    AssistantApplyCallback                  ,
#if defined(ENABLE_OVERLOADING)
    AssistantApplySignalInfo                ,
#endif
    afterAssistantApply                     ,
    onAssistantApply                        ,


-- ** cancel #signal:cancel#

    AssistantCancelCallback                 ,
#if defined(ENABLE_OVERLOADING)
    AssistantCancelSignalInfo               ,
#endif
    afterAssistantCancel                    ,
    onAssistantCancel                       ,


-- ** close #signal:close#

    AssistantCloseCallback                  ,
#if defined(ENABLE_OVERLOADING)
    AssistantCloseSignalInfo                ,
#endif
    afterAssistantClose                     ,
    onAssistantClose                        ,


-- ** escape #signal:escape#

    AssistantEscapeCallback                 ,
#if defined(ENABLE_OVERLOADING)
    AssistantEscapeSignalInfo               ,
#endif
    afterAssistantEscape                    ,
    onAssistantEscape                       ,


-- ** prepare #signal:prepare#

    AssistantPrepareCallback                ,
#if defined(ENABLE_OVERLOADING)
    AssistantPrepareSignalInfo              ,
#endif
    afterAssistantPrepare                   ,
    onAssistantPrepare                      ,




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
import qualified GI.GLib.Callbacks as GLib.Callbacks
import qualified GI.GObject.Objects.Object as GObject.Object
import qualified GI.GdkPixbuf.Objects.Pixbuf as GdkPixbuf.Pixbuf
import qualified GI.Gtk.Callbacks as Gtk.Callbacks
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Objects.Bin as Gtk.Bin
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget
import {-# SOURCE #-} qualified GI.Gtk.Objects.Window as Gtk.Window

-- | Memory-managed wrapper type.
newtype Assistant = Assistant (SP.ManagedPtr Assistant)
    deriving (Eq)

instance SP.ManagedPtrNewtype Assistant where
    toManagedPtr (Assistant p) = p

foreign import ccall "gtk_assistant_get_type"
    c_gtk_assistant_get_type :: IO B.Types.GType

instance B.Types.TypedObject Assistant where
    glibType = c_gtk_assistant_get_type

instance B.Types.GObject Assistant

-- | Type class for types which can be safely cast to `Assistant`, for instance with `toAssistant`.
class (SP.GObject o, O.IsDescendantOf Assistant o) => IsAssistant o
instance (SP.GObject o, O.IsDescendantOf Assistant o) => IsAssistant o

instance O.HasParentTypes Assistant
type instance O.ParentTypes Assistant = '[Gtk.Window.Window, Gtk.Bin.Bin, Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable]

-- | Cast to `Assistant`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toAssistant :: (MIO.MonadIO m, IsAssistant o) => o -> m Assistant
toAssistant = MIO.liftIO . B.ManagedPtr.unsafeCastTo Assistant

-- | Convert 'Assistant' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe Assistant) where
    gvalueGType_ = c_gtk_assistant_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr Assistant)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr Assistant)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject Assistant ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveAssistantMethod (t :: Symbol) (o :: *) :: * where
    ResolveAssistantMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveAssistantMethod "activateDefault" o = Gtk.Window.WindowActivateDefaultMethodInfo
    ResolveAssistantMethod "activateFocus" o = Gtk.Window.WindowActivateFocusMethodInfo
    ResolveAssistantMethod "activateKey" o = Gtk.Window.WindowActivateKeyMethodInfo
    ResolveAssistantMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveAssistantMethod "addAccelGroup" o = Gtk.Window.WindowAddAccelGroupMethodInfo
    ResolveAssistantMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveAssistantMethod "addActionWidget" o = AssistantAddActionWidgetMethodInfo
    ResolveAssistantMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveAssistantMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveAssistantMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveAssistantMethod "addMnemonic" o = Gtk.Window.WindowAddMnemonicMethodInfo
    ResolveAssistantMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveAssistantMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveAssistantMethod "appendPage" o = AssistantAppendPageMethodInfo
    ResolveAssistantMethod "beginMoveDrag" o = Gtk.Window.WindowBeginMoveDragMethodInfo
    ResolveAssistantMethod "beginResizeDrag" o = Gtk.Window.WindowBeginResizeDragMethodInfo
    ResolveAssistantMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveAssistantMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveAssistantMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveAssistantMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveAssistantMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveAssistantMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveAssistantMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveAssistantMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveAssistantMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveAssistantMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveAssistantMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveAssistantMethod "close" o = Gtk.Window.WindowCloseMethodInfo
    ResolveAssistantMethod "commit" o = AssistantCommitMethodInfo
    ResolveAssistantMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveAssistantMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveAssistantMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveAssistantMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveAssistantMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveAssistantMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveAssistantMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveAssistantMethod "deiconify" o = Gtk.Window.WindowDeiconifyMethodInfo
    ResolveAssistantMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveAssistantMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveAssistantMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveAssistantMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveAssistantMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveAssistantMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveAssistantMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveAssistantMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveAssistantMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveAssistantMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveAssistantMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveAssistantMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveAssistantMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveAssistantMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveAssistantMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveAssistantMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveAssistantMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveAssistantMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveAssistantMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveAssistantMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveAssistantMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveAssistantMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveAssistantMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveAssistantMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveAssistantMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveAssistantMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveAssistantMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveAssistantMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveAssistantMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveAssistantMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveAssistantMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveAssistantMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveAssistantMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveAssistantMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveAssistantMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveAssistantMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveAssistantMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveAssistantMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveAssistantMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveAssistantMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveAssistantMethod "fullscreen" o = Gtk.Window.WindowFullscreenMethodInfo
    ResolveAssistantMethod "fullscreenOnMonitor" o = Gtk.Window.WindowFullscreenOnMonitorMethodInfo
    ResolveAssistantMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveAssistantMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveAssistantMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveAssistantMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveAssistantMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveAssistantMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveAssistantMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveAssistantMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveAssistantMethod "hasGroup" o = Gtk.Window.WindowHasGroupMethodInfo
    ResolveAssistantMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveAssistantMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveAssistantMethod "hasToplevelFocus" o = Gtk.Window.WindowHasToplevelFocusMethodInfo
    ResolveAssistantMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveAssistantMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveAssistantMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveAssistantMethod "iconify" o = Gtk.Window.WindowIconifyMethodInfo
    ResolveAssistantMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveAssistantMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveAssistantMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveAssistantMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveAssistantMethod "insertPage" o = AssistantInsertPageMethodInfo
    ResolveAssistantMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveAssistantMethod "isActive" o = Gtk.Window.WindowIsActiveMethodInfo
    ResolveAssistantMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveAssistantMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveAssistantMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveAssistantMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveAssistantMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveAssistantMethod "isMaximized" o = Gtk.Window.WindowIsMaximizedMethodInfo
    ResolveAssistantMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveAssistantMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveAssistantMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveAssistantMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveAssistantMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveAssistantMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveAssistantMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveAssistantMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveAssistantMethod "maximize" o = Gtk.Window.WindowMaximizeMethodInfo
    ResolveAssistantMethod "mnemonicActivate" o = Gtk.Window.WindowMnemonicActivateMethodInfo
    ResolveAssistantMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveAssistantMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveAssistantMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveAssistantMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveAssistantMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveAssistantMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveAssistantMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveAssistantMethod "move" o = Gtk.Window.WindowMoveMethodInfo
    ResolveAssistantMethod "nextPage" o = AssistantNextPageMethodInfo
    ResolveAssistantMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveAssistantMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveAssistantMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveAssistantMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveAssistantMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveAssistantMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveAssistantMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveAssistantMethod "parseGeometry" o = Gtk.Window.WindowParseGeometryMethodInfo
    ResolveAssistantMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveAssistantMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveAssistantMethod "prependPage" o = AssistantPrependPageMethodInfo
    ResolveAssistantMethod "present" o = Gtk.Window.WindowPresentMethodInfo
    ResolveAssistantMethod "presentWithTime" o = Gtk.Window.WindowPresentWithTimeMethodInfo
    ResolveAssistantMethod "previousPage" o = AssistantPreviousPageMethodInfo
    ResolveAssistantMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveAssistantMethod "propagateKeyEvent" o = Gtk.Window.WindowPropagateKeyEventMethodInfo
    ResolveAssistantMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveAssistantMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveAssistantMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveAssistantMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveAssistantMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveAssistantMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveAssistantMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveAssistantMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveAssistantMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveAssistantMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveAssistantMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveAssistantMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveAssistantMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolveAssistantMethod "removeAccelGroup" o = Gtk.Window.WindowRemoveAccelGroupMethodInfo
    ResolveAssistantMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveAssistantMethod "removeActionWidget" o = AssistantRemoveActionWidgetMethodInfo
    ResolveAssistantMethod "removeMnemonic" o = Gtk.Window.WindowRemoveMnemonicMethodInfo
    ResolveAssistantMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveAssistantMethod "removePage" o = AssistantRemovePageMethodInfo
    ResolveAssistantMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveAssistantMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveAssistantMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveAssistantMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveAssistantMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveAssistantMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveAssistantMethod "reshowWithInitialSize" o = Gtk.Window.WindowReshowWithInitialSizeMethodInfo
    ResolveAssistantMethod "resize" o = Gtk.Window.WindowResizeMethodInfo
    ResolveAssistantMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveAssistantMethod "resizeGripIsVisible" o = Gtk.Window.WindowResizeGripIsVisibleMethodInfo
    ResolveAssistantMethod "resizeToGeometry" o = Gtk.Window.WindowResizeToGeometryMethodInfo
    ResolveAssistantMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveAssistantMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveAssistantMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveAssistantMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveAssistantMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveAssistantMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveAssistantMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveAssistantMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveAssistantMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveAssistantMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveAssistantMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveAssistantMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveAssistantMethod "stick" o = Gtk.Window.WindowStickMethodInfo
    ResolveAssistantMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveAssistantMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveAssistantMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveAssistantMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveAssistantMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveAssistantMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveAssistantMethod "unfullscreen" o = Gtk.Window.WindowUnfullscreenMethodInfo
    ResolveAssistantMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveAssistantMethod "unmaximize" o = Gtk.Window.WindowUnmaximizeMethodInfo
    ResolveAssistantMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveAssistantMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveAssistantMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveAssistantMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveAssistantMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveAssistantMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveAssistantMethod "unstick" o = Gtk.Window.WindowUnstickMethodInfo
    ResolveAssistantMethod "updateButtonsState" o = AssistantUpdateButtonsStateMethodInfo
    ResolveAssistantMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveAssistantMethod "getAcceptFocus" o = Gtk.Window.WindowGetAcceptFocusMethodInfo
    ResolveAssistantMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveAssistantMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveAssistantMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveAssistantMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveAssistantMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveAssistantMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveAssistantMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveAssistantMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveAssistantMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveAssistantMethod "getApplication" o = Gtk.Window.WindowGetApplicationMethodInfo
    ResolveAssistantMethod "getAttachedTo" o = Gtk.Window.WindowGetAttachedToMethodInfo
    ResolveAssistantMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveAssistantMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveAssistantMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveAssistantMethod "getChild" o = Gtk.Bin.BinGetChildMethodInfo
    ResolveAssistantMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveAssistantMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveAssistantMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveAssistantMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveAssistantMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveAssistantMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveAssistantMethod "getCurrentPage" o = AssistantGetCurrentPageMethodInfo
    ResolveAssistantMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveAssistantMethod "getDecorated" o = Gtk.Window.WindowGetDecoratedMethodInfo
    ResolveAssistantMethod "getDefaultSize" o = Gtk.Window.WindowGetDefaultSizeMethodInfo
    ResolveAssistantMethod "getDefaultWidget" o = Gtk.Window.WindowGetDefaultWidgetMethodInfo
    ResolveAssistantMethod "getDeletable" o = Gtk.Window.WindowGetDeletableMethodInfo
    ResolveAssistantMethod "getDestroyWithParent" o = Gtk.Window.WindowGetDestroyWithParentMethodInfo
    ResolveAssistantMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveAssistantMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveAssistantMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveAssistantMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveAssistantMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveAssistantMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveAssistantMethod "getFocus" o = Gtk.Window.WindowGetFocusMethodInfo
    ResolveAssistantMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveAssistantMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveAssistantMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveAssistantMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveAssistantMethod "getFocusOnMap" o = Gtk.Window.WindowGetFocusOnMapMethodInfo
    ResolveAssistantMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveAssistantMethod "getFocusVisible" o = Gtk.Window.WindowGetFocusVisibleMethodInfo
    ResolveAssistantMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveAssistantMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveAssistantMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveAssistantMethod "getGravity" o = Gtk.Window.WindowGetGravityMethodInfo
    ResolveAssistantMethod "getGroup" o = Gtk.Window.WindowGetGroupMethodInfo
    ResolveAssistantMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveAssistantMethod "getHasResizeGrip" o = Gtk.Window.WindowGetHasResizeGripMethodInfo
    ResolveAssistantMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveAssistantMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveAssistantMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveAssistantMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveAssistantMethod "getHideTitlebarWhenMaximized" o = Gtk.Window.WindowGetHideTitlebarWhenMaximizedMethodInfo
    ResolveAssistantMethod "getIcon" o = Gtk.Window.WindowGetIconMethodInfo
    ResolveAssistantMethod "getIconList" o = Gtk.Window.WindowGetIconListMethodInfo
    ResolveAssistantMethod "getIconName" o = Gtk.Window.WindowGetIconNameMethodInfo
    ResolveAssistantMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveAssistantMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveAssistantMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveAssistantMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveAssistantMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveAssistantMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveAssistantMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveAssistantMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveAssistantMethod "getMnemonicModifier" o = Gtk.Window.WindowGetMnemonicModifierMethodInfo
    ResolveAssistantMethod "getMnemonicsVisible" o = Gtk.Window.WindowGetMnemonicsVisibleMethodInfo
    ResolveAssistantMethod "getModal" o = Gtk.Window.WindowGetModalMethodInfo
    ResolveAssistantMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveAssistantMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveAssistantMethod "getNPages" o = AssistantGetNPagesMethodInfo
    ResolveAssistantMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveAssistantMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveAssistantMethod "getNthPage" o = AssistantGetNthPageMethodInfo
    ResolveAssistantMethod "getOpacity" o = Gtk.Window.WindowGetOpacityMethodInfo
    ResolveAssistantMethod "getPageComplete" o = AssistantGetPageCompleteMethodInfo
    ResolveAssistantMethod "getPageHasPadding" o = AssistantGetPageHasPaddingMethodInfo
    ResolveAssistantMethod "getPageHeaderImage" o = AssistantGetPageHeaderImageMethodInfo
    ResolveAssistantMethod "getPageSideImage" o = AssistantGetPageSideImageMethodInfo
    ResolveAssistantMethod "getPageTitle" o = AssistantGetPageTitleMethodInfo
    ResolveAssistantMethod "getPageType" o = AssistantGetPageTypeMethodInfo
    ResolveAssistantMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveAssistantMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveAssistantMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveAssistantMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveAssistantMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveAssistantMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveAssistantMethod "getPosition" o = Gtk.Window.WindowGetPositionMethodInfo
    ResolveAssistantMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveAssistantMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveAssistantMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveAssistantMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveAssistantMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveAssistantMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveAssistantMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveAssistantMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveAssistantMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveAssistantMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveAssistantMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveAssistantMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveAssistantMethod "getResizable" o = Gtk.Window.WindowGetResizableMethodInfo
    ResolveAssistantMethod "getResizeGripArea" o = Gtk.Window.WindowGetResizeGripAreaMethodInfo
    ResolveAssistantMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveAssistantMethod "getRole" o = Gtk.Window.WindowGetRoleMethodInfo
    ResolveAssistantMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveAssistantMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveAssistantMethod "getScreen" o = Gtk.Window.WindowGetScreenMethodInfo
    ResolveAssistantMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveAssistantMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveAssistantMethod "getSize" o = Gtk.Window.WindowGetSizeMethodInfo
    ResolveAssistantMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveAssistantMethod "getSkipPagerHint" o = Gtk.Window.WindowGetSkipPagerHintMethodInfo
    ResolveAssistantMethod "getSkipTaskbarHint" o = Gtk.Window.WindowGetSkipTaskbarHintMethodInfo
    ResolveAssistantMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveAssistantMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveAssistantMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveAssistantMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveAssistantMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveAssistantMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveAssistantMethod "getTitle" o = Gtk.Window.WindowGetTitleMethodInfo
    ResolveAssistantMethod "getTitlebar" o = Gtk.Window.WindowGetTitlebarMethodInfo
    ResolveAssistantMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveAssistantMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveAssistantMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveAssistantMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveAssistantMethod "getTransientFor" o = Gtk.Window.WindowGetTransientForMethodInfo
    ResolveAssistantMethod "getTypeHint" o = Gtk.Window.WindowGetTypeHintMethodInfo
    ResolveAssistantMethod "getUrgencyHint" o = Gtk.Window.WindowGetUrgencyHintMethodInfo
    ResolveAssistantMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveAssistantMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveAssistantMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveAssistantMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveAssistantMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveAssistantMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveAssistantMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveAssistantMethod "getWindowType" o = Gtk.Window.WindowGetWindowTypeMethodInfo
    ResolveAssistantMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveAssistantMethod "setAcceptFocus" o = Gtk.Window.WindowSetAcceptFocusMethodInfo
    ResolveAssistantMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveAssistantMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveAssistantMethod "setApplication" o = Gtk.Window.WindowSetApplicationMethodInfo
    ResolveAssistantMethod "setAttachedTo" o = Gtk.Window.WindowSetAttachedToMethodInfo
    ResolveAssistantMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveAssistantMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveAssistantMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveAssistantMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveAssistantMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveAssistantMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveAssistantMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveAssistantMethod "setCurrentPage" o = AssistantSetCurrentPageMethodInfo
    ResolveAssistantMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveAssistantMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveAssistantMethod "setDecorated" o = Gtk.Window.WindowSetDecoratedMethodInfo
    ResolveAssistantMethod "setDefault" o = Gtk.Window.WindowSetDefaultMethodInfo
    ResolveAssistantMethod "setDefaultGeometry" o = Gtk.Window.WindowSetDefaultGeometryMethodInfo
    ResolveAssistantMethod "setDefaultSize" o = Gtk.Window.WindowSetDefaultSizeMethodInfo
    ResolveAssistantMethod "setDeletable" o = Gtk.Window.WindowSetDeletableMethodInfo
    ResolveAssistantMethod "setDestroyWithParent" o = Gtk.Window.WindowSetDestroyWithParentMethodInfo
    ResolveAssistantMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveAssistantMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveAssistantMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveAssistantMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveAssistantMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveAssistantMethod "setFocus" o = Gtk.Window.WindowSetFocusMethodInfo
    ResolveAssistantMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveAssistantMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveAssistantMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveAssistantMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveAssistantMethod "setFocusOnMap" o = Gtk.Window.WindowSetFocusOnMapMethodInfo
    ResolveAssistantMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveAssistantMethod "setFocusVisible" o = Gtk.Window.WindowSetFocusVisibleMethodInfo
    ResolveAssistantMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveAssistantMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveAssistantMethod "setForwardPageFunc" o = AssistantSetForwardPageFuncMethodInfo
    ResolveAssistantMethod "setGeometryHints" o = Gtk.Window.WindowSetGeometryHintsMethodInfo
    ResolveAssistantMethod "setGravity" o = Gtk.Window.WindowSetGravityMethodInfo
    ResolveAssistantMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveAssistantMethod "setHasResizeGrip" o = Gtk.Window.WindowSetHasResizeGripMethodInfo
    ResolveAssistantMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveAssistantMethod "setHasUserRefCount" o = Gtk.Window.WindowSetHasUserRefCountMethodInfo
    ResolveAssistantMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveAssistantMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveAssistantMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveAssistantMethod "setHideTitlebarWhenMaximized" o = Gtk.Window.WindowSetHideTitlebarWhenMaximizedMethodInfo
    ResolveAssistantMethod "setIcon" o = Gtk.Window.WindowSetIconMethodInfo
    ResolveAssistantMethod "setIconFromFile" o = Gtk.Window.WindowSetIconFromFileMethodInfo
    ResolveAssistantMethod "setIconList" o = Gtk.Window.WindowSetIconListMethodInfo
    ResolveAssistantMethod "setIconName" o = Gtk.Window.WindowSetIconNameMethodInfo
    ResolveAssistantMethod "setKeepAbove" o = Gtk.Window.WindowSetKeepAboveMethodInfo
    ResolveAssistantMethod "setKeepBelow" o = Gtk.Window.WindowSetKeepBelowMethodInfo
    ResolveAssistantMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveAssistantMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveAssistantMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveAssistantMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveAssistantMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveAssistantMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveAssistantMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveAssistantMethod "setMnemonicModifier" o = Gtk.Window.WindowSetMnemonicModifierMethodInfo
    ResolveAssistantMethod "setMnemonicsVisible" o = Gtk.Window.WindowSetMnemonicsVisibleMethodInfo
    ResolveAssistantMethod "setModal" o = Gtk.Window.WindowSetModalMethodInfo
    ResolveAssistantMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveAssistantMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveAssistantMethod "setOpacity" o = Gtk.Window.WindowSetOpacityMethodInfo
    ResolveAssistantMethod "setPageComplete" o = AssistantSetPageCompleteMethodInfo
    ResolveAssistantMethod "setPageHasPadding" o = AssistantSetPageHasPaddingMethodInfo
    ResolveAssistantMethod "setPageHeaderImage" o = AssistantSetPageHeaderImageMethodInfo
    ResolveAssistantMethod "setPageSideImage" o = AssistantSetPageSideImageMethodInfo
    ResolveAssistantMethod "setPageTitle" o = AssistantSetPageTitleMethodInfo
    ResolveAssistantMethod "setPageType" o = AssistantSetPageTypeMethodInfo
    ResolveAssistantMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveAssistantMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveAssistantMethod "setPosition" o = Gtk.Window.WindowSetPositionMethodInfo
    ResolveAssistantMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveAssistantMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveAssistantMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveAssistantMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveAssistantMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveAssistantMethod "setResizable" o = Gtk.Window.WindowSetResizableMethodInfo
    ResolveAssistantMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveAssistantMethod "setRole" o = Gtk.Window.WindowSetRoleMethodInfo
    ResolveAssistantMethod "setScreen" o = Gtk.Window.WindowSetScreenMethodInfo
    ResolveAssistantMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveAssistantMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveAssistantMethod "setSkipPagerHint" o = Gtk.Window.WindowSetSkipPagerHintMethodInfo
    ResolveAssistantMethod "setSkipTaskbarHint" o = Gtk.Window.WindowSetSkipTaskbarHintMethodInfo
    ResolveAssistantMethod "setStartupId" o = Gtk.Window.WindowSetStartupIdMethodInfo
    ResolveAssistantMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveAssistantMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveAssistantMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveAssistantMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveAssistantMethod "setTitle" o = Gtk.Window.WindowSetTitleMethodInfo
    ResolveAssistantMethod "setTitlebar" o = Gtk.Window.WindowSetTitlebarMethodInfo
    ResolveAssistantMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveAssistantMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveAssistantMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveAssistantMethod "setTransientFor" o = Gtk.Window.WindowSetTransientForMethodInfo
    ResolveAssistantMethod "setTypeHint" o = Gtk.Window.WindowSetTypeHintMethodInfo
    ResolveAssistantMethod "setUrgencyHint" o = Gtk.Window.WindowSetUrgencyHintMethodInfo
    ResolveAssistantMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveAssistantMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveAssistantMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveAssistantMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveAssistantMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveAssistantMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveAssistantMethod "setWmclass" o = Gtk.Window.WindowSetWmclassMethodInfo
    ResolveAssistantMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveAssistantMethod t Assistant, O.OverloadedMethod info Assistant p) => OL.IsLabel t (Assistant -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveAssistantMethod t Assistant, O.OverloadedMethod info Assistant p, R.HasField t Assistant p) => R.HasField t Assistant p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveAssistantMethod t Assistant, O.OverloadedMethodInfo info Assistant) => OL.IsLabel t (O.MethodProxy info Assistant) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal Assistant::apply
-- | The [apply](#g:signal:apply) signal is emitted when the apply button is clicked.
-- 
-- The default behavior of the t'GI.Gtk.Objects.Assistant.Assistant' is to switch to the page
-- after the current page, unless the current page is the last one.
-- 
-- A handler for the [apply](#g:signal:apply) signal should carry out the actions for
-- which the wizard has collected data. If the action takes a long time
-- to complete, you might consider putting a page of type
-- 'GI.Gtk.Enums.AssistantPageTypeProgress' after the confirmation page and handle
-- this operation within the [Assistant::prepare]("GI.Gtk.Objects.Assistant#g:signal:prepare") signal of the progress
-- page.
-- 
-- /Since: 2.10/
type AssistantApplyCallback =
    IO ()

type C_AssistantApplyCallback =
    Ptr Assistant ->                        -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_AssistantApplyCallback`.
foreign import ccall "wrapper"
    mk_AssistantApplyCallback :: C_AssistantApplyCallback -> IO (FunPtr C_AssistantApplyCallback)

wrap_AssistantApplyCallback :: 
    GObject a => (a -> AssistantApplyCallback) ->
    C_AssistantApplyCallback
wrap_AssistantApplyCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [apply](#signal:apply) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' assistant #apply callback
-- @
-- 
-- 
onAssistantApply :: (IsAssistant a, MonadIO m) => a -> ((?self :: a) => AssistantApplyCallback) -> m SignalHandlerId
onAssistantApply obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_AssistantApplyCallback wrapped
    wrapped'' <- mk_AssistantApplyCallback wrapped'
    connectSignalFunPtr obj "apply" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [apply](#signal:apply) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' assistant #apply callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterAssistantApply :: (IsAssistant a, MonadIO m) => a -> ((?self :: a) => AssistantApplyCallback) -> m SignalHandlerId
afterAssistantApply obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_AssistantApplyCallback wrapped
    wrapped'' <- mk_AssistantApplyCallback wrapped'
    connectSignalFunPtr obj "apply" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data AssistantApplySignalInfo
instance SignalInfo AssistantApplySignalInfo where
    type HaskellCallbackType AssistantApplySignalInfo = AssistantApplyCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_AssistantApplyCallback cb
        cb'' <- mk_AssistantApplyCallback cb'
        connectSignalFunPtr obj "apply" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Assistant::apply"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Assistant.html#g:signal:apply"})

#endif

-- signal Assistant::cancel
-- | The [cancel](#g:signal:cancel) signal is emitted when then the cancel button is clicked.
-- 
-- /Since: 2.10/
type AssistantCancelCallback =
    IO ()

type C_AssistantCancelCallback =
    Ptr Assistant ->                        -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_AssistantCancelCallback`.
foreign import ccall "wrapper"
    mk_AssistantCancelCallback :: C_AssistantCancelCallback -> IO (FunPtr C_AssistantCancelCallback)

wrap_AssistantCancelCallback :: 
    GObject a => (a -> AssistantCancelCallback) ->
    C_AssistantCancelCallback
wrap_AssistantCancelCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [cancel](#signal:cancel) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' assistant #cancel callback
-- @
-- 
-- 
onAssistantCancel :: (IsAssistant a, MonadIO m) => a -> ((?self :: a) => AssistantCancelCallback) -> m SignalHandlerId
onAssistantCancel obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_AssistantCancelCallback wrapped
    wrapped'' <- mk_AssistantCancelCallback wrapped'
    connectSignalFunPtr obj "cancel" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [cancel](#signal:cancel) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' assistant #cancel callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterAssistantCancel :: (IsAssistant a, MonadIO m) => a -> ((?self :: a) => AssistantCancelCallback) -> m SignalHandlerId
afterAssistantCancel obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_AssistantCancelCallback wrapped
    wrapped'' <- mk_AssistantCancelCallback wrapped'
    connectSignalFunPtr obj "cancel" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data AssistantCancelSignalInfo
instance SignalInfo AssistantCancelSignalInfo where
    type HaskellCallbackType AssistantCancelSignalInfo = AssistantCancelCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_AssistantCancelCallback cb
        cb'' <- mk_AssistantCancelCallback cb'
        connectSignalFunPtr obj "cancel" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Assistant::cancel"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Assistant.html#g:signal:cancel"})

#endif

-- signal Assistant::close
-- | The [close](#g:signal:close) signal is emitted either when the close button of
-- a summary page is clicked, or when the apply button in the last
-- page in the flow (of type 'GI.Gtk.Enums.AssistantPageTypeConfirm') is clicked.
-- 
-- /Since: 2.10/
type AssistantCloseCallback =
    IO ()

type C_AssistantCloseCallback =
    Ptr Assistant ->                        -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_AssistantCloseCallback`.
foreign import ccall "wrapper"
    mk_AssistantCloseCallback :: C_AssistantCloseCallback -> IO (FunPtr C_AssistantCloseCallback)

wrap_AssistantCloseCallback :: 
    GObject a => (a -> AssistantCloseCallback) ->
    C_AssistantCloseCallback
wrap_AssistantCloseCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [close](#signal:close) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' assistant #close callback
-- @
-- 
-- 
onAssistantClose :: (IsAssistant a, MonadIO m) => a -> ((?self :: a) => AssistantCloseCallback) -> m SignalHandlerId
onAssistantClose obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_AssistantCloseCallback wrapped
    wrapped'' <- mk_AssistantCloseCallback wrapped'
    connectSignalFunPtr obj "close" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [close](#signal:close) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' assistant #close callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterAssistantClose :: (IsAssistant a, MonadIO m) => a -> ((?self :: a) => AssistantCloseCallback) -> m SignalHandlerId
afterAssistantClose obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_AssistantCloseCallback wrapped
    wrapped'' <- mk_AssistantCloseCallback wrapped'
    connectSignalFunPtr obj "close" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data AssistantCloseSignalInfo
instance SignalInfo AssistantCloseSignalInfo where
    type HaskellCallbackType AssistantCloseSignalInfo = AssistantCloseCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_AssistantCloseCallback cb
        cb'' <- mk_AssistantCloseCallback cb'
        connectSignalFunPtr obj "close" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Assistant::close"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Assistant.html#g:signal:close"})

#endif

-- signal Assistant::escape
-- | /No description available in the introspection data./
type AssistantEscapeCallback =
    IO ()

type C_AssistantEscapeCallback =
    Ptr Assistant ->                        -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_AssistantEscapeCallback`.
foreign import ccall "wrapper"
    mk_AssistantEscapeCallback :: C_AssistantEscapeCallback -> IO (FunPtr C_AssistantEscapeCallback)

wrap_AssistantEscapeCallback :: 
    GObject a => (a -> AssistantEscapeCallback) ->
    C_AssistantEscapeCallback
wrap_AssistantEscapeCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [escape](#signal:escape) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' assistant #escape callback
-- @
-- 
-- 
onAssistantEscape :: (IsAssistant a, MonadIO m) => a -> ((?self :: a) => AssistantEscapeCallback) -> m SignalHandlerId
onAssistantEscape obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_AssistantEscapeCallback wrapped
    wrapped'' <- mk_AssistantEscapeCallback wrapped'
    connectSignalFunPtr obj "escape" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [escape](#signal:escape) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' assistant #escape callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterAssistantEscape :: (IsAssistant a, MonadIO m) => a -> ((?self :: a) => AssistantEscapeCallback) -> m SignalHandlerId
afterAssistantEscape obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_AssistantEscapeCallback wrapped
    wrapped'' <- mk_AssistantEscapeCallback wrapped'
    connectSignalFunPtr obj "escape" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data AssistantEscapeSignalInfo
instance SignalInfo AssistantEscapeSignalInfo where
    type HaskellCallbackType AssistantEscapeSignalInfo = AssistantEscapeCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_AssistantEscapeCallback cb
        cb'' <- mk_AssistantEscapeCallback cb'
        connectSignalFunPtr obj "escape" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Assistant::escape"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Assistant.html#g:signal:escape"})

#endif

-- signal Assistant::prepare
-- | The [prepare](#g:signal:prepare) signal is emitted when a new page is set as the
-- assistant\'s current page, before making the new page visible.
-- 
-- A handler for this signal can do any preparations which are
-- necessary before showing /@page@/.
-- 
-- /Since: 2.10/
type AssistantPrepareCallback =
    Gtk.Widget.Widget
    -- ^ /@page@/: the current page
    -> IO ()

type C_AssistantPrepareCallback =
    Ptr Assistant ->                        -- object
    Ptr Gtk.Widget.Widget ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_AssistantPrepareCallback`.
foreign import ccall "wrapper"
    mk_AssistantPrepareCallback :: C_AssistantPrepareCallback -> IO (FunPtr C_AssistantPrepareCallback)

wrap_AssistantPrepareCallback :: 
    GObject a => (a -> AssistantPrepareCallback) ->
    C_AssistantPrepareCallback
wrap_AssistantPrepareCallback gi'cb gi'selfPtr page _ = do
    page' <- (newObject Gtk.Widget.Widget) page
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  page'


-- | Connect a signal handler for the [prepare](#signal:prepare) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' assistant #prepare callback
-- @
-- 
-- 
onAssistantPrepare :: (IsAssistant a, MonadIO m) => a -> ((?self :: a) => AssistantPrepareCallback) -> m SignalHandlerId
onAssistantPrepare obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_AssistantPrepareCallback wrapped
    wrapped'' <- mk_AssistantPrepareCallback wrapped'
    connectSignalFunPtr obj "prepare" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [prepare](#signal:prepare) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' assistant #prepare callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterAssistantPrepare :: (IsAssistant a, MonadIO m) => a -> ((?self :: a) => AssistantPrepareCallback) -> m SignalHandlerId
afterAssistantPrepare obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_AssistantPrepareCallback wrapped
    wrapped'' <- mk_AssistantPrepareCallback wrapped'
    connectSignalFunPtr obj "prepare" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data AssistantPrepareSignalInfo
instance SignalInfo AssistantPrepareSignalInfo where
    type HaskellCallbackType AssistantPrepareSignalInfo = AssistantPrepareCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_AssistantPrepareCallback cb
        cb'' <- mk_AssistantPrepareCallback cb'
        connectSignalFunPtr obj "prepare" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Assistant::prepare"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Assistant.html#g:signal:prepare"})

#endif

-- VVV Prop "use-header-bar"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstructOnly]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@use-header-bar@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' assistant #useHeaderBar
-- @
getAssistantUseHeaderBar :: (MonadIO m, IsAssistant o) => o -> m Int32
getAssistantUseHeaderBar obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "use-header-bar"

-- | Construct a `GValueConstruct` with valid value for the “@use-header-bar@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructAssistantUseHeaderBar :: (IsAssistant o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructAssistantUseHeaderBar val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "use-header-bar" val

#if defined(ENABLE_OVERLOADING)
data AssistantUseHeaderBarPropertyInfo
instance AttrInfo AssistantUseHeaderBarPropertyInfo where
    type AttrAllowedOps AssistantUseHeaderBarPropertyInfo = '[ 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint AssistantUseHeaderBarPropertyInfo = IsAssistant
    type AttrSetTypeConstraint AssistantUseHeaderBarPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint AssistantUseHeaderBarPropertyInfo = (~) Int32
    type AttrTransferType AssistantUseHeaderBarPropertyInfo = Int32
    type AttrGetType AssistantUseHeaderBarPropertyInfo = Int32
    type AttrLabel AssistantUseHeaderBarPropertyInfo = "use-header-bar"
    type AttrOrigin AssistantUseHeaderBarPropertyInfo = Assistant
    attrGet = getAssistantUseHeaderBar
    attrSet = undefined
    attrTransfer _ v = do
        return v
    attrConstruct = constructAssistantUseHeaderBar
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Assistant.useHeaderBar"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Assistant.html#g:attr:useHeaderBar"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList Assistant
type instance O.AttributeList Assistant = AssistantAttributeList
type AssistantAttributeList = ('[ '("acceptFocus", Gtk.Window.WindowAcceptFocusPropertyInfo), '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("application", Gtk.Window.WindowApplicationPropertyInfo), '("attachedTo", Gtk.Window.WindowAttachedToPropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("decorated", Gtk.Window.WindowDecoratedPropertyInfo), '("defaultHeight", Gtk.Window.WindowDefaultHeightPropertyInfo), '("defaultWidth", Gtk.Window.WindowDefaultWidthPropertyInfo), '("deletable", Gtk.Window.WindowDeletablePropertyInfo), '("destroyWithParent", Gtk.Window.WindowDestroyWithParentPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("focusOnMap", Gtk.Window.WindowFocusOnMapPropertyInfo), '("focusVisible", Gtk.Window.WindowFocusVisiblePropertyInfo), '("gravity", Gtk.Window.WindowGravityPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasResizeGrip", Gtk.Window.WindowHasResizeGripPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("hasToplevelFocus", Gtk.Window.WindowHasToplevelFocusPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("hideTitlebarWhenMaximized", Gtk.Window.WindowHideTitlebarWhenMaximizedPropertyInfo), '("icon", Gtk.Window.WindowIconPropertyInfo), '("iconName", Gtk.Window.WindowIconNamePropertyInfo), '("isActive", Gtk.Window.WindowIsActivePropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("isMaximized", Gtk.Window.WindowIsMaximizedPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("mnemonicsVisible", Gtk.Window.WindowMnemonicsVisiblePropertyInfo), '("modal", Gtk.Window.WindowModalPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("resizable", Gtk.Window.WindowResizablePropertyInfo), '("resizeGripVisible", Gtk.Window.WindowResizeGripVisiblePropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("role", Gtk.Window.WindowRolePropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("screen", Gtk.Window.WindowScreenPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("skipPagerHint", Gtk.Window.WindowSkipPagerHintPropertyInfo), '("skipTaskbarHint", Gtk.Window.WindowSkipTaskbarHintPropertyInfo), '("startupId", Gtk.Window.WindowStartupIdPropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("title", Gtk.Window.WindowTitlePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("transientFor", Gtk.Window.WindowTransientForPropertyInfo), '("type", Gtk.Window.WindowTypePropertyInfo), '("typeHint", Gtk.Window.WindowTypeHintPropertyInfo), '("urgencyHint", Gtk.Window.WindowUrgencyHintPropertyInfo), '("useHeaderBar", AssistantUseHeaderBarPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo), '("windowPosition", Gtk.Window.WindowWindowPositionPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
assistantUseHeaderBar :: AttrLabelProxy "useHeaderBar"
assistantUseHeaderBar = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList Assistant = AssistantSignalList
type AssistantSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("activateDefault", Gtk.Window.WindowActivateDefaultSignalInfo), '("activateFocus", Gtk.Window.WindowActivateFocusSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("apply", AssistantApplySignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("cancel", AssistantCancelSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("close", AssistantCloseSignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enableDebugging", Gtk.Window.WindowEnableDebuggingSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("escape", AssistantEscapeSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("keysChanged", Gtk.Window.WindowKeysChangedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("prepare", AssistantPrepareSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocus", Gtk.Window.WindowSetFocusSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method Assistant::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Assistant" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_assistant_new" gtk_assistant_new :: 
    IO (Ptr Assistant)

-- | Creates a new t'GI.Gtk.Objects.Assistant.Assistant'.
-- 
-- /Since: 2.10/
assistantNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m Assistant
    -- ^ __Returns:__ a newly created t'GI.Gtk.Objects.Assistant.Assistant'
assistantNew  = liftIO $ do
    result <- gtk_assistant_new
    checkUnexpectedReturnNULL "assistantNew" result
    result' <- (newObject Assistant) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Assistant::add_action_widget
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "assistant"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Assistant" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAssistant" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a #GtkWidget" , sinceVersion = Nothing }
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

foreign import ccall "gtk_assistant_add_action_widget" gtk_assistant_add_action_widget :: 
    Ptr Assistant ->                        -- assistant : TInterface (Name {namespace = "Gtk", name = "Assistant"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | Adds a widget to the action area of a t'GI.Gtk.Objects.Assistant.Assistant'.
-- 
-- /Since: 2.10/
assistantAddActionWidget ::
    (B.CallStack.HasCallStack, MonadIO m, IsAssistant a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@assistant@/: a t'GI.Gtk.Objects.Assistant.Assistant'
    -> b
    -- ^ /@child@/: a t'GI.Gtk.Objects.Widget.Widget'
    -> m ()
assistantAddActionWidget assistant child = liftIO $ do
    assistant' <- unsafeManagedPtrCastPtr assistant
    child' <- unsafeManagedPtrCastPtr child
    gtk_assistant_add_action_widget assistant' child'
    touchManagedPtr assistant
    touchManagedPtr child
    return ()

#if defined(ENABLE_OVERLOADING)
data AssistantAddActionWidgetMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsAssistant a, Gtk.Widget.IsWidget b) => O.OverloadedMethod AssistantAddActionWidgetMethodInfo a signature where
    overloadedMethod = assistantAddActionWidget

instance O.OverloadedMethodInfo AssistantAddActionWidgetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Assistant.assistantAddActionWidget",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Assistant.html#v:assistantAddActionWidget"
        })


#endif

-- method Assistant::append_page
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "assistant"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Assistant" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAssistant" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "page"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidget" , sinceVersion = Nothing }
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

foreign import ccall "gtk_assistant_append_page" gtk_assistant_append_page :: 
    Ptr Assistant ->                        -- assistant : TInterface (Name {namespace = "Gtk", name = "Assistant"})
    Ptr Gtk.Widget.Widget ->                -- page : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO Int32

-- | Appends a page to the /@assistant@/.
-- 
-- /Since: 2.10/
assistantAppendPage ::
    (B.CallStack.HasCallStack, MonadIO m, IsAssistant a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@assistant@/: a t'GI.Gtk.Objects.Assistant.Assistant'
    -> b
    -- ^ /@page@/: a t'GI.Gtk.Objects.Widget.Widget'
    -> m Int32
    -- ^ __Returns:__ the index (starting at 0) of the inserted page
assistantAppendPage assistant page = liftIO $ do
    assistant' <- unsafeManagedPtrCastPtr assistant
    page' <- unsafeManagedPtrCastPtr page
    result <- gtk_assistant_append_page assistant' page'
    touchManagedPtr assistant
    touchManagedPtr page
    return result

#if defined(ENABLE_OVERLOADING)
data AssistantAppendPageMethodInfo
instance (signature ~ (b -> m Int32), MonadIO m, IsAssistant a, Gtk.Widget.IsWidget b) => O.OverloadedMethod AssistantAppendPageMethodInfo a signature where
    overloadedMethod = assistantAppendPage

instance O.OverloadedMethodInfo AssistantAppendPageMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Assistant.assistantAppendPage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Assistant.html#v:assistantAppendPage"
        })


#endif

-- method Assistant::commit
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "assistant"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Assistant" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAssistant" , sinceVersion = Nothing }
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

foreign import ccall "gtk_assistant_commit" gtk_assistant_commit :: 
    Ptr Assistant ->                        -- assistant : TInterface (Name {namespace = "Gtk", name = "Assistant"})
    IO ()

-- | Erases the visited page history so the back button is not
-- shown on the current page, and removes the cancel button
-- from subsequent pages.
-- 
-- Use this when the information provided up to the current
-- page is hereafter deemed permanent and cannot be modified
-- or undone. For example, showing a progress page to track
-- a long-running, unreversible operation after the user has
-- clicked apply on a confirmation page.
-- 
-- /Since: 2.22/
assistantCommit ::
    (B.CallStack.HasCallStack, MonadIO m, IsAssistant a) =>
    a
    -- ^ /@assistant@/: a t'GI.Gtk.Objects.Assistant.Assistant'
    -> m ()
assistantCommit assistant = liftIO $ do
    assistant' <- unsafeManagedPtrCastPtr assistant
    gtk_assistant_commit assistant'
    touchManagedPtr assistant
    return ()

#if defined(ENABLE_OVERLOADING)
data AssistantCommitMethodInfo
instance (signature ~ (m ()), MonadIO m, IsAssistant a) => O.OverloadedMethod AssistantCommitMethodInfo a signature where
    overloadedMethod = assistantCommit

instance O.OverloadedMethodInfo AssistantCommitMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Assistant.assistantCommit",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Assistant.html#v:assistantCommit"
        })


#endif

-- method Assistant::get_current_page
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "assistant"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Assistant" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAssistant" , sinceVersion = Nothing }
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

foreign import ccall "gtk_assistant_get_current_page" gtk_assistant_get_current_page :: 
    Ptr Assistant ->                        -- assistant : TInterface (Name {namespace = "Gtk", name = "Assistant"})
    IO Int32

-- | Returns the page number of the current page.
-- 
-- /Since: 2.10/
assistantGetCurrentPage ::
    (B.CallStack.HasCallStack, MonadIO m, IsAssistant a) =>
    a
    -- ^ /@assistant@/: a t'GI.Gtk.Objects.Assistant.Assistant'
    -> m Int32
    -- ^ __Returns:__ The index (starting from 0) of the current
    --     page in the /@assistant@/, or -1 if the /@assistant@/ has no pages,
    --     or no current page.
assistantGetCurrentPage assistant = liftIO $ do
    assistant' <- unsafeManagedPtrCastPtr assistant
    result <- gtk_assistant_get_current_page assistant'
    touchManagedPtr assistant
    return result

#if defined(ENABLE_OVERLOADING)
data AssistantGetCurrentPageMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsAssistant a) => O.OverloadedMethod AssistantGetCurrentPageMethodInfo a signature where
    overloadedMethod = assistantGetCurrentPage

instance O.OverloadedMethodInfo AssistantGetCurrentPageMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Assistant.assistantGetCurrentPage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Assistant.html#v:assistantGetCurrentPage"
        })


#endif

-- method Assistant::get_n_pages
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "assistant"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Assistant" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAssistant" , sinceVersion = Nothing }
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

foreign import ccall "gtk_assistant_get_n_pages" gtk_assistant_get_n_pages :: 
    Ptr Assistant ->                        -- assistant : TInterface (Name {namespace = "Gtk", name = "Assistant"})
    IO Int32

-- | Returns the number of pages in the /@assistant@/
-- 
-- /Since: 2.10/
assistantGetNPages ::
    (B.CallStack.HasCallStack, MonadIO m, IsAssistant a) =>
    a
    -- ^ /@assistant@/: a t'GI.Gtk.Objects.Assistant.Assistant'
    -> m Int32
    -- ^ __Returns:__ the number of pages in the /@assistant@/
assistantGetNPages assistant = liftIO $ do
    assistant' <- unsafeManagedPtrCastPtr assistant
    result <- gtk_assistant_get_n_pages assistant'
    touchManagedPtr assistant
    return result

#if defined(ENABLE_OVERLOADING)
data AssistantGetNPagesMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsAssistant a) => O.OverloadedMethod AssistantGetNPagesMethodInfo a signature where
    overloadedMethod = assistantGetNPages

instance O.OverloadedMethodInfo AssistantGetNPagesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Assistant.assistantGetNPages",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Assistant.html#v:assistantGetNPages"
        })


#endif

-- method Assistant::get_nth_page
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "assistant"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Assistant" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAssistant" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "page_num"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the index of a page in the @assistant,\n    or -1 to get the last page"
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

foreign import ccall "gtk_assistant_get_nth_page" gtk_assistant_get_nth_page :: 
    Ptr Assistant ->                        -- assistant : TInterface (Name {namespace = "Gtk", name = "Assistant"})
    Int32 ->                                -- page_num : TBasicType TInt
    IO (Ptr Gtk.Widget.Widget)

-- | Returns the child widget contained in page number /@pageNum@/.
-- 
-- /Since: 2.10/
assistantGetNthPage ::
    (B.CallStack.HasCallStack, MonadIO m, IsAssistant a) =>
    a
    -- ^ /@assistant@/: a t'GI.Gtk.Objects.Assistant.Assistant'
    -> Int32
    -- ^ /@pageNum@/: the index of a page in the /@assistant@/,
    --     or -1 to get the last page
    -> m (Maybe Gtk.Widget.Widget)
    -- ^ __Returns:__ the child widget, or 'P.Nothing'
    --     if /@pageNum@/ is out of bounds
assistantGetNthPage assistant pageNum = liftIO $ do
    assistant' <- unsafeManagedPtrCastPtr assistant
    result <- gtk_assistant_get_nth_page assistant' pageNum
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Widget.Widget) result'
        return result''
    touchManagedPtr assistant
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data AssistantGetNthPageMethodInfo
instance (signature ~ (Int32 -> m (Maybe Gtk.Widget.Widget)), MonadIO m, IsAssistant a) => O.OverloadedMethod AssistantGetNthPageMethodInfo a signature where
    overloadedMethod = assistantGetNthPage

instance O.OverloadedMethodInfo AssistantGetNthPageMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Assistant.assistantGetNthPage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Assistant.html#v:assistantGetNthPage"
        })


#endif

-- method Assistant::get_page_complete
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "assistant"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Assistant" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAssistant" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "page"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a page of @assistant"
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

foreign import ccall "gtk_assistant_get_page_complete" gtk_assistant_get_page_complete :: 
    Ptr Assistant ->                        -- assistant : TInterface (Name {namespace = "Gtk", name = "Assistant"})
    Ptr Gtk.Widget.Widget ->                -- page : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO CInt

-- | Gets whether /@page@/ is complete.
-- 
-- /Since: 2.10/
assistantGetPageComplete ::
    (B.CallStack.HasCallStack, MonadIO m, IsAssistant a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@assistant@/: a t'GI.Gtk.Objects.Assistant.Assistant'
    -> b
    -- ^ /@page@/: a page of /@assistant@/
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@page@/ is complete.
assistantGetPageComplete assistant page = liftIO $ do
    assistant' <- unsafeManagedPtrCastPtr assistant
    page' <- unsafeManagedPtrCastPtr page
    result <- gtk_assistant_get_page_complete assistant' page'
    let result' = (/= 0) result
    touchManagedPtr assistant
    touchManagedPtr page
    return result'

#if defined(ENABLE_OVERLOADING)
data AssistantGetPageCompleteMethodInfo
instance (signature ~ (b -> m Bool), MonadIO m, IsAssistant a, Gtk.Widget.IsWidget b) => O.OverloadedMethod AssistantGetPageCompleteMethodInfo a signature where
    overloadedMethod = assistantGetPageComplete

instance O.OverloadedMethodInfo AssistantGetPageCompleteMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Assistant.assistantGetPageComplete",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Assistant.html#v:assistantGetPageComplete"
        })


#endif

-- method Assistant::get_page_has_padding
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "assistant"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Assistant" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAssistant" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "page"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a page of @assistant"
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

foreign import ccall "gtk_assistant_get_page_has_padding" gtk_assistant_get_page_has_padding :: 
    Ptr Assistant ->                        -- assistant : TInterface (Name {namespace = "Gtk", name = "Assistant"})
    Ptr Gtk.Widget.Widget ->                -- page : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO CInt

-- | Gets whether page has padding.
-- 
-- /Since: 3.18/
assistantGetPageHasPadding ::
    (B.CallStack.HasCallStack, MonadIO m, IsAssistant a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@assistant@/: a t'GI.Gtk.Objects.Assistant.Assistant'
    -> b
    -- ^ /@page@/: a page of /@assistant@/
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@page@/ has padding
assistantGetPageHasPadding assistant page = liftIO $ do
    assistant' <- unsafeManagedPtrCastPtr assistant
    page' <- unsafeManagedPtrCastPtr page
    result <- gtk_assistant_get_page_has_padding assistant' page'
    let result' = (/= 0) result
    touchManagedPtr assistant
    touchManagedPtr page
    return result'

#if defined(ENABLE_OVERLOADING)
data AssistantGetPageHasPaddingMethodInfo
instance (signature ~ (b -> m Bool), MonadIO m, IsAssistant a, Gtk.Widget.IsWidget b) => O.OverloadedMethod AssistantGetPageHasPaddingMethodInfo a signature where
    overloadedMethod = assistantGetPageHasPadding

instance O.OverloadedMethodInfo AssistantGetPageHasPaddingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Assistant.assistantGetPageHasPadding",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Assistant.html#v:assistantGetPageHasPadding"
        })


#endif

-- method Assistant::get_page_header_image
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "assistant"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Assistant" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAssistant" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "page"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a page of @assistant"
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
--               (TInterface Name { namespace = "GdkPixbuf" , name = "Pixbuf" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_assistant_get_page_header_image" gtk_assistant_get_page_header_image :: 
    Ptr Assistant ->                        -- assistant : TInterface (Name {namespace = "Gtk", name = "Assistant"})
    Ptr Gtk.Widget.Widget ->                -- page : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO (Ptr GdkPixbuf.Pixbuf.Pixbuf)

{-# DEPRECATED assistantGetPageHeaderImage ["(Since version 3.2)","Since GTK+ 3.2, a header is no longer shown;","    add your header decoration to the page content instead."] #-}
-- | Gets the header image for /@page@/.
-- 
-- /Since: 2.10/
assistantGetPageHeaderImage ::
    (B.CallStack.HasCallStack, MonadIO m, IsAssistant a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@assistant@/: a t'GI.Gtk.Objects.Assistant.Assistant'
    -> b
    -- ^ /@page@/: a page of /@assistant@/
    -> m GdkPixbuf.Pixbuf.Pixbuf
    -- ^ __Returns:__ the header image for /@page@/,
    --     or 'P.Nothing' if there’s no header image for the page
assistantGetPageHeaderImage assistant page = liftIO $ do
    assistant' <- unsafeManagedPtrCastPtr assistant
    page' <- unsafeManagedPtrCastPtr page
    result <- gtk_assistant_get_page_header_image assistant' page'
    checkUnexpectedReturnNULL "assistantGetPageHeaderImage" result
    result' <- (newObject GdkPixbuf.Pixbuf.Pixbuf) result
    touchManagedPtr assistant
    touchManagedPtr page
    return result'

#if defined(ENABLE_OVERLOADING)
data AssistantGetPageHeaderImageMethodInfo
instance (signature ~ (b -> m GdkPixbuf.Pixbuf.Pixbuf), MonadIO m, IsAssistant a, Gtk.Widget.IsWidget b) => O.OverloadedMethod AssistantGetPageHeaderImageMethodInfo a signature where
    overloadedMethod = assistantGetPageHeaderImage

instance O.OverloadedMethodInfo AssistantGetPageHeaderImageMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Assistant.assistantGetPageHeaderImage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Assistant.html#v:assistantGetPageHeaderImage"
        })


#endif

-- method Assistant::get_page_side_image
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "assistant"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Assistant" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAssistant" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "page"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a page of @assistant"
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
--               (TInterface Name { namespace = "GdkPixbuf" , name = "Pixbuf" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_assistant_get_page_side_image" gtk_assistant_get_page_side_image :: 
    Ptr Assistant ->                        -- assistant : TInterface (Name {namespace = "Gtk", name = "Assistant"})
    Ptr Gtk.Widget.Widget ->                -- page : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO (Ptr GdkPixbuf.Pixbuf.Pixbuf)

{-# DEPRECATED assistantGetPageSideImage ["(Since version 3.2)","Since GTK+ 3.2, sidebar images are not","    shown anymore."] #-}
-- | Gets the side image for /@page@/.
-- 
-- /Since: 2.10/
assistantGetPageSideImage ::
    (B.CallStack.HasCallStack, MonadIO m, IsAssistant a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@assistant@/: a t'GI.Gtk.Objects.Assistant.Assistant'
    -> b
    -- ^ /@page@/: a page of /@assistant@/
    -> m GdkPixbuf.Pixbuf.Pixbuf
    -- ^ __Returns:__ the side image for /@page@/,
    --     or 'P.Nothing' if there’s no side image for the page
assistantGetPageSideImage assistant page = liftIO $ do
    assistant' <- unsafeManagedPtrCastPtr assistant
    page' <- unsafeManagedPtrCastPtr page
    result <- gtk_assistant_get_page_side_image assistant' page'
    checkUnexpectedReturnNULL "assistantGetPageSideImage" result
    result' <- (newObject GdkPixbuf.Pixbuf.Pixbuf) result
    touchManagedPtr assistant
    touchManagedPtr page
    return result'

#if defined(ENABLE_OVERLOADING)
data AssistantGetPageSideImageMethodInfo
instance (signature ~ (b -> m GdkPixbuf.Pixbuf.Pixbuf), MonadIO m, IsAssistant a, Gtk.Widget.IsWidget b) => O.OverloadedMethod AssistantGetPageSideImageMethodInfo a signature where
    overloadedMethod = assistantGetPageSideImage

instance O.OverloadedMethodInfo AssistantGetPageSideImageMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Assistant.assistantGetPageSideImage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Assistant.html#v:assistantGetPageSideImage"
        })


#endif

-- method Assistant::get_page_title
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "assistant"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Assistant" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAssistant" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "page"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a page of @assistant"
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

foreign import ccall "gtk_assistant_get_page_title" gtk_assistant_get_page_title :: 
    Ptr Assistant ->                        -- assistant : TInterface (Name {namespace = "Gtk", name = "Assistant"})
    Ptr Gtk.Widget.Widget ->                -- page : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO CString

-- | Gets the title for /@page@/.
-- 
-- /Since: 2.10/
assistantGetPageTitle ::
    (B.CallStack.HasCallStack, MonadIO m, IsAssistant a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@assistant@/: a t'GI.Gtk.Objects.Assistant.Assistant'
    -> b
    -- ^ /@page@/: a page of /@assistant@/
    -> m T.Text
    -- ^ __Returns:__ the title for /@page@/
assistantGetPageTitle assistant page = liftIO $ do
    assistant' <- unsafeManagedPtrCastPtr assistant
    page' <- unsafeManagedPtrCastPtr page
    result <- gtk_assistant_get_page_title assistant' page'
    checkUnexpectedReturnNULL "assistantGetPageTitle" result
    result' <- cstringToText result
    touchManagedPtr assistant
    touchManagedPtr page
    return result'

#if defined(ENABLE_OVERLOADING)
data AssistantGetPageTitleMethodInfo
instance (signature ~ (b -> m T.Text), MonadIO m, IsAssistant a, Gtk.Widget.IsWidget b) => O.OverloadedMethod AssistantGetPageTitleMethodInfo a signature where
    overloadedMethod = assistantGetPageTitle

instance O.OverloadedMethodInfo AssistantGetPageTitleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Assistant.assistantGetPageTitle",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Assistant.html#v:assistantGetPageTitle"
        })


#endif

-- method Assistant::get_page_type
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "assistant"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Assistant" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAssistant" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "page"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a page of @assistant"
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
--                  Name { namespace = "Gtk" , name = "AssistantPageType" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_assistant_get_page_type" gtk_assistant_get_page_type :: 
    Ptr Assistant ->                        -- assistant : TInterface (Name {namespace = "Gtk", name = "Assistant"})
    Ptr Gtk.Widget.Widget ->                -- page : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO CUInt

-- | Gets the page type of /@page@/.
-- 
-- /Since: 2.10/
assistantGetPageType ::
    (B.CallStack.HasCallStack, MonadIO m, IsAssistant a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@assistant@/: a t'GI.Gtk.Objects.Assistant.Assistant'
    -> b
    -- ^ /@page@/: a page of /@assistant@/
    -> m Gtk.Enums.AssistantPageType
    -- ^ __Returns:__ the page type of /@page@/
assistantGetPageType assistant page = liftIO $ do
    assistant' <- unsafeManagedPtrCastPtr assistant
    page' <- unsafeManagedPtrCastPtr page
    result <- gtk_assistant_get_page_type assistant' page'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr assistant
    touchManagedPtr page
    return result'

#if defined(ENABLE_OVERLOADING)
data AssistantGetPageTypeMethodInfo
instance (signature ~ (b -> m Gtk.Enums.AssistantPageType), MonadIO m, IsAssistant a, Gtk.Widget.IsWidget b) => O.OverloadedMethod AssistantGetPageTypeMethodInfo a signature where
    overloadedMethod = assistantGetPageType

instance O.OverloadedMethodInfo AssistantGetPageTypeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Assistant.assistantGetPageType",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Assistant.html#v:assistantGetPageType"
        })


#endif

-- method Assistant::insert_page
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "assistant"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Assistant" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAssistant" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "page"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidget" , sinceVersion = Nothing }
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
--                       "the index (starting at 0) at which to insert the page,\n    or -1 to append the page to the @assistant"
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

foreign import ccall "gtk_assistant_insert_page" gtk_assistant_insert_page :: 
    Ptr Assistant ->                        -- assistant : TInterface (Name {namespace = "Gtk", name = "Assistant"})
    Ptr Gtk.Widget.Widget ->                -- page : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Int32 ->                                -- position : TBasicType TInt
    IO Int32

-- | Inserts a page in the /@assistant@/ at a given position.
-- 
-- /Since: 2.10/
assistantInsertPage ::
    (B.CallStack.HasCallStack, MonadIO m, IsAssistant a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@assistant@/: a t'GI.Gtk.Objects.Assistant.Assistant'
    -> b
    -- ^ /@page@/: a t'GI.Gtk.Objects.Widget.Widget'
    -> Int32
    -- ^ /@position@/: the index (starting at 0) at which to insert the page,
    --     or -1 to append the page to the /@assistant@/
    -> m Int32
    -- ^ __Returns:__ the index (starting from 0) of the inserted page
assistantInsertPage assistant page position = liftIO $ do
    assistant' <- unsafeManagedPtrCastPtr assistant
    page' <- unsafeManagedPtrCastPtr page
    result <- gtk_assistant_insert_page assistant' page' position
    touchManagedPtr assistant
    touchManagedPtr page
    return result

#if defined(ENABLE_OVERLOADING)
data AssistantInsertPageMethodInfo
instance (signature ~ (b -> Int32 -> m Int32), MonadIO m, IsAssistant a, Gtk.Widget.IsWidget b) => O.OverloadedMethod AssistantInsertPageMethodInfo a signature where
    overloadedMethod = assistantInsertPage

instance O.OverloadedMethodInfo AssistantInsertPageMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Assistant.assistantInsertPage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Assistant.html#v:assistantInsertPage"
        })


#endif

-- method Assistant::next_page
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "assistant"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Assistant" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAssistant" , sinceVersion = Nothing }
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

foreign import ccall "gtk_assistant_next_page" gtk_assistant_next_page :: 
    Ptr Assistant ->                        -- assistant : TInterface (Name {namespace = "Gtk", name = "Assistant"})
    IO ()

-- | Navigate to the next page.
-- 
-- It is a programming error to call this function when
-- there is no next page.
-- 
-- This function is for use when creating pages of the
-- @/GTK_ASSISTANT_PAGE_CUSTOM/@ type.
-- 
-- /Since: 3.0/
assistantNextPage ::
    (B.CallStack.HasCallStack, MonadIO m, IsAssistant a) =>
    a
    -- ^ /@assistant@/: a t'GI.Gtk.Objects.Assistant.Assistant'
    -> m ()
assistantNextPage assistant = liftIO $ do
    assistant' <- unsafeManagedPtrCastPtr assistant
    gtk_assistant_next_page assistant'
    touchManagedPtr assistant
    return ()

#if defined(ENABLE_OVERLOADING)
data AssistantNextPageMethodInfo
instance (signature ~ (m ()), MonadIO m, IsAssistant a) => O.OverloadedMethod AssistantNextPageMethodInfo a signature where
    overloadedMethod = assistantNextPage

instance O.OverloadedMethodInfo AssistantNextPageMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Assistant.assistantNextPage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Assistant.html#v:assistantNextPage"
        })


#endif

-- method Assistant::prepend_page
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "assistant"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Assistant" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAssistant" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "page"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidget" , sinceVersion = Nothing }
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

foreign import ccall "gtk_assistant_prepend_page" gtk_assistant_prepend_page :: 
    Ptr Assistant ->                        -- assistant : TInterface (Name {namespace = "Gtk", name = "Assistant"})
    Ptr Gtk.Widget.Widget ->                -- page : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO Int32

-- | Prepends a page to the /@assistant@/.
-- 
-- /Since: 2.10/
assistantPrependPage ::
    (B.CallStack.HasCallStack, MonadIO m, IsAssistant a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@assistant@/: a t'GI.Gtk.Objects.Assistant.Assistant'
    -> b
    -- ^ /@page@/: a t'GI.Gtk.Objects.Widget.Widget'
    -> m Int32
    -- ^ __Returns:__ the index (starting at 0) of the inserted page
assistantPrependPage assistant page = liftIO $ do
    assistant' <- unsafeManagedPtrCastPtr assistant
    page' <- unsafeManagedPtrCastPtr page
    result <- gtk_assistant_prepend_page assistant' page'
    touchManagedPtr assistant
    touchManagedPtr page
    return result

#if defined(ENABLE_OVERLOADING)
data AssistantPrependPageMethodInfo
instance (signature ~ (b -> m Int32), MonadIO m, IsAssistant a, Gtk.Widget.IsWidget b) => O.OverloadedMethod AssistantPrependPageMethodInfo a signature where
    overloadedMethod = assistantPrependPage

instance O.OverloadedMethodInfo AssistantPrependPageMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Assistant.assistantPrependPage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Assistant.html#v:assistantPrependPage"
        })


#endif

-- method Assistant::previous_page
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "assistant"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Assistant" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAssistant" , sinceVersion = Nothing }
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

foreign import ccall "gtk_assistant_previous_page" gtk_assistant_previous_page :: 
    Ptr Assistant ->                        -- assistant : TInterface (Name {namespace = "Gtk", name = "Assistant"})
    IO ()

-- | Navigate to the previous visited page.
-- 
-- It is a programming error to call this function when
-- no previous page is available.
-- 
-- This function is for use when creating pages of the
-- @/GTK_ASSISTANT_PAGE_CUSTOM/@ type.
-- 
-- /Since: 3.0/
assistantPreviousPage ::
    (B.CallStack.HasCallStack, MonadIO m, IsAssistant a) =>
    a
    -- ^ /@assistant@/: a t'GI.Gtk.Objects.Assistant.Assistant'
    -> m ()
assistantPreviousPage assistant = liftIO $ do
    assistant' <- unsafeManagedPtrCastPtr assistant
    gtk_assistant_previous_page assistant'
    touchManagedPtr assistant
    return ()

#if defined(ENABLE_OVERLOADING)
data AssistantPreviousPageMethodInfo
instance (signature ~ (m ()), MonadIO m, IsAssistant a) => O.OverloadedMethod AssistantPreviousPageMethodInfo a signature where
    overloadedMethod = assistantPreviousPage

instance O.OverloadedMethodInfo AssistantPreviousPageMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Assistant.assistantPreviousPage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Assistant.html#v:assistantPreviousPage"
        })


#endif

-- method Assistant::remove_action_widget
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "assistant"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Assistant" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAssistant" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a #GtkWidget" , sinceVersion = Nothing }
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

foreign import ccall "gtk_assistant_remove_action_widget" gtk_assistant_remove_action_widget :: 
    Ptr Assistant ->                        -- assistant : TInterface (Name {namespace = "Gtk", name = "Assistant"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | Removes a widget from the action area of a t'GI.Gtk.Objects.Assistant.Assistant'.
-- 
-- /Since: 2.10/
assistantRemoveActionWidget ::
    (B.CallStack.HasCallStack, MonadIO m, IsAssistant a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@assistant@/: a t'GI.Gtk.Objects.Assistant.Assistant'
    -> b
    -- ^ /@child@/: a t'GI.Gtk.Objects.Widget.Widget'
    -> m ()
assistantRemoveActionWidget assistant child = liftIO $ do
    assistant' <- unsafeManagedPtrCastPtr assistant
    child' <- unsafeManagedPtrCastPtr child
    gtk_assistant_remove_action_widget assistant' child'
    touchManagedPtr assistant
    touchManagedPtr child
    return ()

#if defined(ENABLE_OVERLOADING)
data AssistantRemoveActionWidgetMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsAssistant a, Gtk.Widget.IsWidget b) => O.OverloadedMethod AssistantRemoveActionWidgetMethodInfo a signature where
    overloadedMethod = assistantRemoveActionWidget

instance O.OverloadedMethodInfo AssistantRemoveActionWidgetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Assistant.assistantRemoveActionWidget",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Assistant.html#v:assistantRemoveActionWidget"
        })


#endif

-- method Assistant::remove_page
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "assistant"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Assistant" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAssistant" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "page_num"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the index of a page in the @assistant,\n    or -1 to remove the last page"
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

foreign import ccall "gtk_assistant_remove_page" gtk_assistant_remove_page :: 
    Ptr Assistant ->                        -- assistant : TInterface (Name {namespace = "Gtk", name = "Assistant"})
    Int32 ->                                -- page_num : TBasicType TInt
    IO ()

-- | Removes the /@pageNum@/’s page from /@assistant@/.
-- 
-- /Since: 3.2/
assistantRemovePage ::
    (B.CallStack.HasCallStack, MonadIO m, IsAssistant a) =>
    a
    -- ^ /@assistant@/: a t'GI.Gtk.Objects.Assistant.Assistant'
    -> Int32
    -- ^ /@pageNum@/: the index of a page in the /@assistant@/,
    --     or -1 to remove the last page
    -> m ()
assistantRemovePage assistant pageNum = liftIO $ do
    assistant' <- unsafeManagedPtrCastPtr assistant
    gtk_assistant_remove_page assistant' pageNum
    touchManagedPtr assistant
    return ()

#if defined(ENABLE_OVERLOADING)
data AssistantRemovePageMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsAssistant a) => O.OverloadedMethod AssistantRemovePageMethodInfo a signature where
    overloadedMethod = assistantRemovePage

instance O.OverloadedMethodInfo AssistantRemovePageMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Assistant.assistantRemovePage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Assistant.html#v:assistantRemovePage"
        })


#endif

-- method Assistant::set_current_page
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "assistant"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Assistant" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAssistant" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "page_num"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "index of the page to switch to, starting from 0.\n    If negative, the last page will be used. If greater\n    than the number of pages in the @assistant, nothing\n    will be done."
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

foreign import ccall "gtk_assistant_set_current_page" gtk_assistant_set_current_page :: 
    Ptr Assistant ->                        -- assistant : TInterface (Name {namespace = "Gtk", name = "Assistant"})
    Int32 ->                                -- page_num : TBasicType TInt
    IO ()

-- | Switches the page to /@pageNum@/.
-- 
-- Note that this will only be necessary in custom buttons,
-- as the /@assistant@/ flow can be set with
-- 'GI.Gtk.Objects.Assistant.assistantSetForwardPageFunc'.
-- 
-- /Since: 2.10/
assistantSetCurrentPage ::
    (B.CallStack.HasCallStack, MonadIO m, IsAssistant a) =>
    a
    -- ^ /@assistant@/: a t'GI.Gtk.Objects.Assistant.Assistant'
    -> Int32
    -- ^ /@pageNum@/: index of the page to switch to, starting from 0.
    --     If negative, the last page will be used. If greater
    --     than the number of pages in the /@assistant@/, nothing
    --     will be done.
    -> m ()
assistantSetCurrentPage assistant pageNum = liftIO $ do
    assistant' <- unsafeManagedPtrCastPtr assistant
    gtk_assistant_set_current_page assistant' pageNum
    touchManagedPtr assistant
    return ()

#if defined(ENABLE_OVERLOADING)
data AssistantSetCurrentPageMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsAssistant a) => O.OverloadedMethod AssistantSetCurrentPageMethodInfo a signature where
    overloadedMethod = assistantSetCurrentPage

instance O.OverloadedMethodInfo AssistantSetCurrentPageMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Assistant.assistantSetCurrentPage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Assistant.html#v:assistantSetCurrentPage"
        })


#endif

-- method Assistant::set_forward_page_func
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "assistant"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Assistant" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAssistant" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "page_func"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AssistantPageFunc" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the #GtkAssistantPageFunc, or %NULL\n    to use the default one"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeNotified
--           , argClosure = 2
--           , argDestroy = 3
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "user data for @page_func"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "destroy"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "DestroyNotify" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "destroy notifier for @data"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeAsync
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

foreign import ccall "gtk_assistant_set_forward_page_func" gtk_assistant_set_forward_page_func :: 
    Ptr Assistant ->                        -- assistant : TInterface (Name {namespace = "Gtk", name = "Assistant"})
    FunPtr Gtk.Callbacks.C_AssistantPageFunc -> -- page_func : TInterface (Name {namespace = "Gtk", name = "AssistantPageFunc"})
    Ptr () ->                               -- data : TBasicType TPtr
    FunPtr GLib.Callbacks.C_DestroyNotify -> -- destroy : TInterface (Name {namespace = "GLib", name = "DestroyNotify"})
    IO ()

-- | Sets the page forwarding function to be /@pageFunc@/.
-- 
-- This function will be used to determine what will be
-- the next page when the user presses the forward button.
-- Setting /@pageFunc@/ to 'P.Nothing' will make the assistant to
-- use the default forward function, which just goes to the
-- next visible page.
-- 
-- /Since: 2.10/
assistantSetForwardPageFunc ::
    (B.CallStack.HasCallStack, MonadIO m, IsAssistant a) =>
    a
    -- ^ /@assistant@/: a t'GI.Gtk.Objects.Assistant.Assistant'
    -> Maybe (Gtk.Callbacks.AssistantPageFunc)
    -- ^ /@pageFunc@/: the t'GI.Gtk.Callbacks.AssistantPageFunc', or 'P.Nothing'
    --     to use the default one
    -> m ()
assistantSetForwardPageFunc assistant pageFunc = liftIO $ do
    assistant' <- unsafeManagedPtrCastPtr assistant
    maybePageFunc <- case pageFunc of
        Nothing -> return (castPtrToFunPtr nullPtr)
        Just jPageFunc -> do
            jPageFunc' <- Gtk.Callbacks.mk_AssistantPageFunc (Gtk.Callbacks.wrap_AssistantPageFunc Nothing (Gtk.Callbacks.drop_closures_AssistantPageFunc jPageFunc))
            return jPageFunc'
    let data_ = castFunPtrToPtr maybePageFunc
    let destroy = SP.safeFreeFunPtrPtr
    gtk_assistant_set_forward_page_func assistant' maybePageFunc data_ destroy
    touchManagedPtr assistant
    return ()

#if defined(ENABLE_OVERLOADING)
data AssistantSetForwardPageFuncMethodInfo
instance (signature ~ (Maybe (Gtk.Callbacks.AssistantPageFunc) -> m ()), MonadIO m, IsAssistant a) => O.OverloadedMethod AssistantSetForwardPageFuncMethodInfo a signature where
    overloadedMethod = assistantSetForwardPageFunc

instance O.OverloadedMethodInfo AssistantSetForwardPageFuncMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Assistant.assistantSetForwardPageFunc",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Assistant.html#v:assistantSetForwardPageFunc"
        })


#endif

-- method Assistant::set_page_complete
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "assistant"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Assistant" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAssistant" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "page"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a page of @assistant"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "complete"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the completeness status of the page"
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

foreign import ccall "gtk_assistant_set_page_complete" gtk_assistant_set_page_complete :: 
    Ptr Assistant ->                        -- assistant : TInterface (Name {namespace = "Gtk", name = "Assistant"})
    Ptr Gtk.Widget.Widget ->                -- page : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CInt ->                                 -- complete : TBasicType TBoolean
    IO ()

-- | Sets whether /@page@/ contents are complete.
-- 
-- This will make /@assistant@/ update the buttons state
-- to be able to continue the task.
-- 
-- /Since: 2.10/
assistantSetPageComplete ::
    (B.CallStack.HasCallStack, MonadIO m, IsAssistant a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@assistant@/: a t'GI.Gtk.Objects.Assistant.Assistant'
    -> b
    -- ^ /@page@/: a page of /@assistant@/
    -> Bool
    -- ^ /@complete@/: the completeness status of the page
    -> m ()
assistantSetPageComplete assistant page complete = liftIO $ do
    assistant' <- unsafeManagedPtrCastPtr assistant
    page' <- unsafeManagedPtrCastPtr page
    let complete' = (fromIntegral . fromEnum) complete
    gtk_assistant_set_page_complete assistant' page' complete'
    touchManagedPtr assistant
    touchManagedPtr page
    return ()

#if defined(ENABLE_OVERLOADING)
data AssistantSetPageCompleteMethodInfo
instance (signature ~ (b -> Bool -> m ()), MonadIO m, IsAssistant a, Gtk.Widget.IsWidget b) => O.OverloadedMethod AssistantSetPageCompleteMethodInfo a signature where
    overloadedMethod = assistantSetPageComplete

instance O.OverloadedMethodInfo AssistantSetPageCompleteMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Assistant.assistantSetPageComplete",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Assistant.html#v:assistantSetPageComplete"
        })


#endif

-- method Assistant::set_page_has_padding
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "assistant"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Assistant" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAssistant" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "page"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a page of @assistant"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "has_padding"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "whether this page has padding"
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

foreign import ccall "gtk_assistant_set_page_has_padding" gtk_assistant_set_page_has_padding :: 
    Ptr Assistant ->                        -- assistant : TInterface (Name {namespace = "Gtk", name = "Assistant"})
    Ptr Gtk.Widget.Widget ->                -- page : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CInt ->                                 -- has_padding : TBasicType TBoolean
    IO ()

-- | Sets whether the assistant is adding padding around
-- the page.
-- 
-- /Since: 3.18/
assistantSetPageHasPadding ::
    (B.CallStack.HasCallStack, MonadIO m, IsAssistant a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@assistant@/: a t'GI.Gtk.Objects.Assistant.Assistant'
    -> b
    -- ^ /@page@/: a page of /@assistant@/
    -> Bool
    -- ^ /@hasPadding@/: whether this page has padding
    -> m ()
assistantSetPageHasPadding assistant page hasPadding = liftIO $ do
    assistant' <- unsafeManagedPtrCastPtr assistant
    page' <- unsafeManagedPtrCastPtr page
    let hasPadding' = (fromIntegral . fromEnum) hasPadding
    gtk_assistant_set_page_has_padding assistant' page' hasPadding'
    touchManagedPtr assistant
    touchManagedPtr page
    return ()

#if defined(ENABLE_OVERLOADING)
data AssistantSetPageHasPaddingMethodInfo
instance (signature ~ (b -> Bool -> m ()), MonadIO m, IsAssistant a, Gtk.Widget.IsWidget b) => O.OverloadedMethod AssistantSetPageHasPaddingMethodInfo a signature where
    overloadedMethod = assistantSetPageHasPadding

instance O.OverloadedMethodInfo AssistantSetPageHasPaddingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Assistant.assistantSetPageHasPadding",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Assistant.html#v:assistantSetPageHasPadding"
        })


#endif

-- method Assistant::set_page_header_image
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "assistant"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Assistant" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAssistant" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "page"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a page of @assistant"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "the new header image @page"
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

foreign import ccall "gtk_assistant_set_page_header_image" gtk_assistant_set_page_header_image :: 
    Ptr Assistant ->                        -- assistant : TInterface (Name {namespace = "Gtk", name = "Assistant"})
    Ptr Gtk.Widget.Widget ->                -- page : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr GdkPixbuf.Pixbuf.Pixbuf ->          -- pixbuf : TInterface (Name {namespace = "GdkPixbuf", name = "Pixbuf"})
    IO ()

{-# DEPRECATED assistantSetPageHeaderImage ["(Since version 3.2)","Since GTK+ 3.2, a header is no longer shown;","    add your header decoration to the page content instead."] #-}
-- | Sets a header image for /@page@/.
-- 
-- /Since: 2.10/
assistantSetPageHeaderImage ::
    (B.CallStack.HasCallStack, MonadIO m, IsAssistant a, Gtk.Widget.IsWidget b, GdkPixbuf.Pixbuf.IsPixbuf c) =>
    a
    -- ^ /@assistant@/: a t'GI.Gtk.Objects.Assistant.Assistant'
    -> b
    -- ^ /@page@/: a page of /@assistant@/
    -> Maybe (c)
    -- ^ /@pixbuf@/: the new header image /@page@/
    -> m ()
assistantSetPageHeaderImage assistant page pixbuf = liftIO $ do
    assistant' <- unsafeManagedPtrCastPtr assistant
    page' <- unsafeManagedPtrCastPtr page
    maybePixbuf <- case pixbuf of
        Nothing -> return nullPtr
        Just jPixbuf -> do
            jPixbuf' <- unsafeManagedPtrCastPtr jPixbuf
            return jPixbuf'
    gtk_assistant_set_page_header_image assistant' page' maybePixbuf
    touchManagedPtr assistant
    touchManagedPtr page
    whenJust pixbuf touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data AssistantSetPageHeaderImageMethodInfo
instance (signature ~ (b -> Maybe (c) -> m ()), MonadIO m, IsAssistant a, Gtk.Widget.IsWidget b, GdkPixbuf.Pixbuf.IsPixbuf c) => O.OverloadedMethod AssistantSetPageHeaderImageMethodInfo a signature where
    overloadedMethod = assistantSetPageHeaderImage

instance O.OverloadedMethodInfo AssistantSetPageHeaderImageMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Assistant.assistantSetPageHeaderImage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Assistant.html#v:assistantSetPageHeaderImage"
        })


#endif

-- method Assistant::set_page_side_image
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "assistant"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Assistant" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAssistant" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "page"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a page of @assistant"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "the new side image @page"
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

foreign import ccall "gtk_assistant_set_page_side_image" gtk_assistant_set_page_side_image :: 
    Ptr Assistant ->                        -- assistant : TInterface (Name {namespace = "Gtk", name = "Assistant"})
    Ptr Gtk.Widget.Widget ->                -- page : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr GdkPixbuf.Pixbuf.Pixbuf ->          -- pixbuf : TInterface (Name {namespace = "GdkPixbuf", name = "Pixbuf"})
    IO ()

{-# DEPRECATED assistantSetPageSideImage ["(Since version 3.2)","Since GTK+ 3.2, sidebar images are not","    shown anymore."] #-}
-- | Sets a side image for /@page@/.
-- 
-- This image used to be displayed in the side area of the assistant
-- when /@page@/ is the current page.
-- 
-- /Since: 2.10/
assistantSetPageSideImage ::
    (B.CallStack.HasCallStack, MonadIO m, IsAssistant a, Gtk.Widget.IsWidget b, GdkPixbuf.Pixbuf.IsPixbuf c) =>
    a
    -- ^ /@assistant@/: a t'GI.Gtk.Objects.Assistant.Assistant'
    -> b
    -- ^ /@page@/: a page of /@assistant@/
    -> Maybe (c)
    -- ^ /@pixbuf@/: the new side image /@page@/
    -> m ()
assistantSetPageSideImage assistant page pixbuf = liftIO $ do
    assistant' <- unsafeManagedPtrCastPtr assistant
    page' <- unsafeManagedPtrCastPtr page
    maybePixbuf <- case pixbuf of
        Nothing -> return nullPtr
        Just jPixbuf -> do
            jPixbuf' <- unsafeManagedPtrCastPtr jPixbuf
            return jPixbuf'
    gtk_assistant_set_page_side_image assistant' page' maybePixbuf
    touchManagedPtr assistant
    touchManagedPtr page
    whenJust pixbuf touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data AssistantSetPageSideImageMethodInfo
instance (signature ~ (b -> Maybe (c) -> m ()), MonadIO m, IsAssistant a, Gtk.Widget.IsWidget b, GdkPixbuf.Pixbuf.IsPixbuf c) => O.OverloadedMethod AssistantSetPageSideImageMethodInfo a signature where
    overloadedMethod = assistantSetPageSideImage

instance O.OverloadedMethodInfo AssistantSetPageSideImageMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Assistant.assistantSetPageSideImage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Assistant.html#v:assistantSetPageSideImage"
        })


#endif

-- method Assistant::set_page_title
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "assistant"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Assistant" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAssistant" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "page"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a page of @assistant"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "title"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the new title for @page"
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

foreign import ccall "gtk_assistant_set_page_title" gtk_assistant_set_page_title :: 
    Ptr Assistant ->                        -- assistant : TInterface (Name {namespace = "Gtk", name = "Assistant"})
    Ptr Gtk.Widget.Widget ->                -- page : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CString ->                              -- title : TBasicType TUTF8
    IO ()

-- | Sets a title for /@page@/.
-- 
-- The title is displayed in the header area of the assistant
-- when /@page@/ is the current page.
-- 
-- /Since: 2.10/
assistantSetPageTitle ::
    (B.CallStack.HasCallStack, MonadIO m, IsAssistant a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@assistant@/: a t'GI.Gtk.Objects.Assistant.Assistant'
    -> b
    -- ^ /@page@/: a page of /@assistant@/
    -> T.Text
    -- ^ /@title@/: the new title for /@page@/
    -> m ()
assistantSetPageTitle assistant page title = liftIO $ do
    assistant' <- unsafeManagedPtrCastPtr assistant
    page' <- unsafeManagedPtrCastPtr page
    title' <- textToCString title
    gtk_assistant_set_page_title assistant' page' title'
    touchManagedPtr assistant
    touchManagedPtr page
    freeMem title'
    return ()

#if defined(ENABLE_OVERLOADING)
data AssistantSetPageTitleMethodInfo
instance (signature ~ (b -> T.Text -> m ()), MonadIO m, IsAssistant a, Gtk.Widget.IsWidget b) => O.OverloadedMethod AssistantSetPageTitleMethodInfo a signature where
    overloadedMethod = assistantSetPageTitle

instance O.OverloadedMethodInfo AssistantSetPageTitleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Assistant.assistantSetPageTitle",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Assistant.html#v:assistantSetPageTitle"
        })


#endif

-- method Assistant::set_page_type
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "assistant"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Assistant" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAssistant" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "page"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a page of @assistant"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AssistantPageType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the new type for @page"
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

foreign import ccall "gtk_assistant_set_page_type" gtk_assistant_set_page_type :: 
    Ptr Assistant ->                        -- assistant : TInterface (Name {namespace = "Gtk", name = "Assistant"})
    Ptr Gtk.Widget.Widget ->                -- page : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CUInt ->                                -- type : TInterface (Name {namespace = "Gtk", name = "AssistantPageType"})
    IO ()

-- | Sets the page type for /@page@/.
-- 
-- The page type determines the page behavior in the /@assistant@/.
-- 
-- /Since: 2.10/
assistantSetPageType ::
    (B.CallStack.HasCallStack, MonadIO m, IsAssistant a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@assistant@/: a t'GI.Gtk.Objects.Assistant.Assistant'
    -> b
    -- ^ /@page@/: a page of /@assistant@/
    -> Gtk.Enums.AssistantPageType
    -- ^ /@type@/: the new type for /@page@/
    -> m ()
assistantSetPageType assistant page type_ = liftIO $ do
    assistant' <- unsafeManagedPtrCastPtr assistant
    page' <- unsafeManagedPtrCastPtr page
    let type_' = (fromIntegral . fromEnum) type_
    gtk_assistant_set_page_type assistant' page' type_'
    touchManagedPtr assistant
    touchManagedPtr page
    return ()

#if defined(ENABLE_OVERLOADING)
data AssistantSetPageTypeMethodInfo
instance (signature ~ (b -> Gtk.Enums.AssistantPageType -> m ()), MonadIO m, IsAssistant a, Gtk.Widget.IsWidget b) => O.OverloadedMethod AssistantSetPageTypeMethodInfo a signature where
    overloadedMethod = assistantSetPageType

instance O.OverloadedMethodInfo AssistantSetPageTypeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Assistant.assistantSetPageType",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Assistant.html#v:assistantSetPageType"
        })


#endif

-- method Assistant::update_buttons_state
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "assistant"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Assistant" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAssistant" , sinceVersion = Nothing }
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

foreign import ccall "gtk_assistant_update_buttons_state" gtk_assistant_update_buttons_state :: 
    Ptr Assistant ->                        -- assistant : TInterface (Name {namespace = "Gtk", name = "Assistant"})
    IO ()

-- | Forces /@assistant@/ to recompute the buttons state.
-- 
-- GTK+ automatically takes care of this in most situations,
-- e.g. when the user goes to a different page, or when the
-- visibility or completeness of a page changes.
-- 
-- One situation where it can be necessary to call this
-- function is when changing a value on the current page
-- affects the future page flow of the assistant.
-- 
-- /Since: 2.10/
assistantUpdateButtonsState ::
    (B.CallStack.HasCallStack, MonadIO m, IsAssistant a) =>
    a
    -- ^ /@assistant@/: a t'GI.Gtk.Objects.Assistant.Assistant'
    -> m ()
assistantUpdateButtonsState assistant = liftIO $ do
    assistant' <- unsafeManagedPtrCastPtr assistant
    gtk_assistant_update_buttons_state assistant'
    touchManagedPtr assistant
    return ()

#if defined(ENABLE_OVERLOADING)
data AssistantUpdateButtonsStateMethodInfo
instance (signature ~ (m ()), MonadIO m, IsAssistant a) => O.OverloadedMethod AssistantUpdateButtonsStateMethodInfo a signature where
    overloadedMethod = assistantUpdateButtonsState

instance O.OverloadedMethodInfo AssistantUpdateButtonsStateMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Assistant.assistantUpdateButtonsState",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Assistant.html#v:assistantUpdateButtonsState"
        })


#endif


