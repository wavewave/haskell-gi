{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Objects.AppChooserDialog.AppChooserDialog' shows a t'GI.Gtk.Objects.AppChooserWidget.AppChooserWidget' inside a t'GI.Gtk.Objects.Dialog.Dialog'.
-- 
-- Note that t'GI.Gtk.Objects.AppChooserDialog.AppChooserDialog' does not have any interesting methods
-- of its own. Instead, you should get the embedded t'GI.Gtk.Objects.AppChooserWidget.AppChooserWidget'
-- using 'GI.Gtk.Objects.AppChooserDialog.appChooserDialogGetWidget' and call its methods if
-- the generic t'GI.Gtk.Interfaces.AppChooser.AppChooser' interface is not sufficient for your needs.
-- 
-- To set the heading that is shown above the t'GI.Gtk.Objects.AppChooserWidget.AppChooserWidget',
-- use 'GI.Gtk.Objects.AppChooserDialog.appChooserDialogSetHeading'.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.AppChooserDialog
    ( 

-- * Exported types
    AppChooserDialog(..)                    ,
    IsAppChooserDialog                      ,
    toAppChooserDialog                      ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [activateDefault]("GI.Gtk.Objects.Window#g:method:activateDefault"), [activateFocus]("GI.Gtk.Objects.Window#g:method:activateFocus"), [activateKey]("GI.Gtk.Objects.Window#g:method:activateKey"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelGroup]("GI.Gtk.Objects.Window#g:method:addAccelGroup"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addActionWidget]("GI.Gtk.Objects.Dialog#g:method:addActionWidget"), [addButton]("GI.Gtk.Objects.Dialog#g:method:addButton"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonic]("GI.Gtk.Objects.Window#g:method:addMnemonic"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [beginMoveDrag]("GI.Gtk.Objects.Window#g:method:beginMoveDrag"), [beginResizeDrag]("GI.Gtk.Objects.Window#g:method:beginResizeDrag"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [close]("GI.Gtk.Objects.Window#g:method:close"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [deiconify]("GI.Gtk.Objects.Window#g:method:deiconify"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [fullscreen]("GI.Gtk.Objects.Window#g:method:fullscreen"), [fullscreenOnMonitor]("GI.Gtk.Objects.Window#g:method:fullscreenOnMonitor"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasGroup]("GI.Gtk.Objects.Window#g:method:hasGroup"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasToplevelFocus]("GI.Gtk.Objects.Window#g:method:hasToplevelFocus"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [iconify]("GI.Gtk.Objects.Window#g:method:iconify"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isActive]("GI.Gtk.Objects.Window#g:method:isActive"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isMaximized]("GI.Gtk.Objects.Window#g:method:isMaximized"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [maximize]("GI.Gtk.Objects.Window#g:method:maximize"), [mnemonicActivate]("GI.Gtk.Objects.Window#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [move]("GI.Gtk.Objects.Window#g:method:move"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parseGeometry]("GI.Gtk.Objects.Window#g:method:parseGeometry"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [present]("GI.Gtk.Objects.Window#g:method:present"), [presentWithTime]("GI.Gtk.Objects.Window#g:method:presentWithTime"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [propagateKeyEvent]("GI.Gtk.Objects.Window#g:method:propagateKeyEvent"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [refresh]("GI.Gtk.Interfaces.AppChooser#g:method:refresh"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelGroup]("GI.Gtk.Objects.Window#g:method:removeAccelGroup"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonic]("GI.Gtk.Objects.Window#g:method:removeMnemonic"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [reshowWithInitialSize]("GI.Gtk.Objects.Window#g:method:reshowWithInitialSize"), [resize]("GI.Gtk.Objects.Window#g:method:resize"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [resizeGripIsVisible]("GI.Gtk.Objects.Window#g:method:resizeGripIsVisible"), [resizeToGeometry]("GI.Gtk.Objects.Window#g:method:resizeToGeometry"), [response]("GI.Gtk.Objects.Dialog#g:method:response"), [run]("GI.Gtk.Objects.Dialog#g:method:run"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [stick]("GI.Gtk.Objects.Window#g:method:stick"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unfullscreen]("GI.Gtk.Objects.Window#g:method:unfullscreen"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unmaximize]("GI.Gtk.Objects.Window#g:method:unmaximize"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [unstick]("GI.Gtk.Objects.Window#g:method:unstick"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAcceptFocus]("GI.Gtk.Objects.Window#g:method:getAcceptFocus"), [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionArea]("GI.Gtk.Objects.Dialog#g:method:getActionArea"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppInfo]("GI.Gtk.Interfaces.AppChooser#g:method:getAppInfo"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getApplication]("GI.Gtk.Objects.Window#g:method:getApplication"), [getAttachedTo]("GI.Gtk.Objects.Window#g:method:getAttachedTo"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChild]("GI.Gtk.Objects.Bin#g:method:getChild"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getContentArea]("GI.Gtk.Objects.Dialog#g:method:getContentArea"), [getContentType]("GI.Gtk.Interfaces.AppChooser#g:method:getContentType"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDecorated]("GI.Gtk.Objects.Window#g:method:getDecorated"), [getDefaultSize]("GI.Gtk.Objects.Window#g:method:getDefaultSize"), [getDefaultWidget]("GI.Gtk.Objects.Window#g:method:getDefaultWidget"), [getDeletable]("GI.Gtk.Objects.Window#g:method:getDeletable"), [getDestroyWithParent]("GI.Gtk.Objects.Window#g:method:getDestroyWithParent"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocus]("GI.Gtk.Objects.Window#g:method:getFocus"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFocusOnMap]("GI.Gtk.Objects.Window#g:method:getFocusOnMap"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFocusVisible]("GI.Gtk.Objects.Window#g:method:getFocusVisible"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getGravity]("GI.Gtk.Objects.Window#g:method:getGravity"), [getGroup]("GI.Gtk.Objects.Window#g:method:getGroup"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasResizeGrip]("GI.Gtk.Objects.Window#g:method:getHasResizeGrip"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHeaderBar]("GI.Gtk.Objects.Dialog#g:method:getHeaderBar"), [getHeading]("GI.Gtk.Objects.AppChooserDialog#g:method:getHeading"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getHideTitlebarWhenMaximized]("GI.Gtk.Objects.Window#g:method:getHideTitlebarWhenMaximized"), [getIcon]("GI.Gtk.Objects.Window#g:method:getIcon"), [getIconList]("GI.Gtk.Objects.Window#g:method:getIconList"), [getIconName]("GI.Gtk.Objects.Window#g:method:getIconName"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getMnemonicModifier]("GI.Gtk.Objects.Window#g:method:getMnemonicModifier"), [getMnemonicsVisible]("GI.Gtk.Objects.Window#g:method:getMnemonicsVisible"), [getModal]("GI.Gtk.Objects.Window#g:method:getModal"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Window#g:method:getOpacity"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPosition]("GI.Gtk.Objects.Window#g:method:getPosition"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizable]("GI.Gtk.Objects.Window#g:method:getResizable"), [getResizeGripArea]("GI.Gtk.Objects.Window#g:method:getResizeGripArea"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getResponseForWidget]("GI.Gtk.Objects.Dialog#g:method:getResponseForWidget"), [getRole]("GI.Gtk.Objects.Window#g:method:getRole"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Window#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSize]("GI.Gtk.Objects.Window#g:method:getSize"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getSkipPagerHint]("GI.Gtk.Objects.Window#g:method:getSkipPagerHint"), [getSkipTaskbarHint]("GI.Gtk.Objects.Window#g:method:getSkipTaskbarHint"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTitle]("GI.Gtk.Objects.Window#g:method:getTitle"), [getTitlebar]("GI.Gtk.Objects.Window#g:method:getTitlebar"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getTransientFor]("GI.Gtk.Objects.Window#g:method:getTransientFor"), [getTypeHint]("GI.Gtk.Objects.Window#g:method:getTypeHint"), [getUrgencyHint]("GI.Gtk.Objects.Window#g:method:getUrgencyHint"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWidget]("GI.Gtk.Objects.AppChooserDialog#g:method:getWidget"), [getWidgetForResponse]("GI.Gtk.Objects.Dialog#g:method:getWidgetForResponse"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow"), [getWindowType]("GI.Gtk.Objects.Window#g:method:getWindowType").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setAcceptFocus]("GI.Gtk.Objects.Window#g:method:setAcceptFocus"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAlternativeButtonOrderFromArray]("GI.Gtk.Objects.Dialog#g:method:setAlternativeButtonOrderFromArray"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setApplication]("GI.Gtk.Objects.Window#g:method:setApplication"), [setAttachedTo]("GI.Gtk.Objects.Window#g:method:setAttachedTo"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDecorated]("GI.Gtk.Objects.Window#g:method:setDecorated"), [setDefault]("GI.Gtk.Objects.Window#g:method:setDefault"), [setDefaultGeometry]("GI.Gtk.Objects.Window#g:method:setDefaultGeometry"), [setDefaultResponse]("GI.Gtk.Objects.Dialog#g:method:setDefaultResponse"), [setDefaultSize]("GI.Gtk.Objects.Window#g:method:setDefaultSize"), [setDeletable]("GI.Gtk.Objects.Window#g:method:setDeletable"), [setDestroyWithParent]("GI.Gtk.Objects.Window#g:method:setDestroyWithParent"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocus]("GI.Gtk.Objects.Window#g:method:setFocus"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFocusOnMap]("GI.Gtk.Objects.Window#g:method:setFocusOnMap"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFocusVisible]("GI.Gtk.Objects.Window#g:method:setFocusVisible"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setGeometryHints]("GI.Gtk.Objects.Window#g:method:setGeometryHints"), [setGravity]("GI.Gtk.Objects.Window#g:method:setGravity"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasResizeGrip]("GI.Gtk.Objects.Window#g:method:setHasResizeGrip"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasUserRefCount]("GI.Gtk.Objects.Window#g:method:setHasUserRefCount"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHeading]("GI.Gtk.Objects.AppChooserDialog#g:method:setHeading"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setHideTitlebarWhenMaximized]("GI.Gtk.Objects.Window#g:method:setHideTitlebarWhenMaximized"), [setIcon]("GI.Gtk.Objects.Window#g:method:setIcon"), [setIconFromFile]("GI.Gtk.Objects.Window#g:method:setIconFromFile"), [setIconList]("GI.Gtk.Objects.Window#g:method:setIconList"), [setIconName]("GI.Gtk.Objects.Window#g:method:setIconName"), [setKeepAbove]("GI.Gtk.Objects.Window#g:method:setKeepAbove"), [setKeepBelow]("GI.Gtk.Objects.Window#g:method:setKeepBelow"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setMnemonicModifier]("GI.Gtk.Objects.Window#g:method:setMnemonicModifier"), [setMnemonicsVisible]("GI.Gtk.Objects.Window#g:method:setMnemonicsVisible"), [setModal]("GI.Gtk.Objects.Window#g:method:setModal"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Window#g:method:setOpacity"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setPosition]("GI.Gtk.Objects.Window#g:method:setPosition"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setResizable]("GI.Gtk.Objects.Window#g:method:setResizable"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setResponseSensitive]("GI.Gtk.Objects.Dialog#g:method:setResponseSensitive"), [setRole]("GI.Gtk.Objects.Window#g:method:setRole"), [setScreen]("GI.Gtk.Objects.Window#g:method:setScreen"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setSkipPagerHint]("GI.Gtk.Objects.Window#g:method:setSkipPagerHint"), [setSkipTaskbarHint]("GI.Gtk.Objects.Window#g:method:setSkipTaskbarHint"), [setStartupId]("GI.Gtk.Objects.Window#g:method:setStartupId"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTitle]("GI.Gtk.Objects.Window#g:method:setTitle"), [setTitlebar]("GI.Gtk.Objects.Window#g:method:setTitlebar"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setTransientFor]("GI.Gtk.Objects.Window#g:method:setTransientFor"), [setTypeHint]("GI.Gtk.Objects.Window#g:method:setTypeHint"), [setUrgencyHint]("GI.Gtk.Objects.Window#g:method:setUrgencyHint"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow"), [setWmclass]("GI.Gtk.Objects.Window#g:method:setWmclass").

#if defined(ENABLE_OVERLOADING)
    ResolveAppChooserDialogMethod           ,
#endif

-- ** getHeading #method:getHeading#

#if defined(ENABLE_OVERLOADING)
    AppChooserDialogGetHeadingMethodInfo    ,
#endif
    appChooserDialogGetHeading              ,


-- ** getWidget #method:getWidget#

#if defined(ENABLE_OVERLOADING)
    AppChooserDialogGetWidgetMethodInfo     ,
#endif
    appChooserDialogGetWidget               ,


-- ** new #method:new#

    appChooserDialogNew                     ,


-- ** newForContentType #method:newForContentType#

    appChooserDialogNewForContentType       ,


-- ** setHeading #method:setHeading#

#if defined(ENABLE_OVERLOADING)
    AppChooserDialogSetHeadingMethodInfo    ,
#endif
    appChooserDialogSetHeading              ,




 -- * Properties


-- ** gfile #attr:gfile#
-- | The GFile used by the t'GI.Gtk.Objects.AppChooserDialog.AppChooserDialog'.
-- The dialog\'s t'GI.Gtk.Objects.AppChooserWidget.AppChooserWidget' content type will be guessed from the
-- file, if present.

#if defined(ENABLE_OVERLOADING)
    AppChooserDialogGfilePropertyInfo       ,
#endif
#if defined(ENABLE_OVERLOADING)
    appChooserDialogGfile                   ,
#endif
    constructAppChooserDialogGfile          ,
    getAppChooserDialogGfile                ,


-- ** heading #attr:heading#
-- | The text to show at the top of the dialog.
-- The string may contain Pango markup.

#if defined(ENABLE_OVERLOADING)
    AppChooserDialogHeadingPropertyInfo     ,
#endif
#if defined(ENABLE_OVERLOADING)
    appChooserDialogHeading                 ,
#endif
    constructAppChooserDialogHeading        ,
    getAppChooserDialogHeading              ,
    setAppChooserDialogHeading              ,




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
import qualified GI.Gio.Interfaces.File as Gio.File
import {-# SOURCE #-} qualified GI.Gtk.Flags as Gtk.Flags
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.AppChooser as Gtk.AppChooser
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Objects.Bin as Gtk.Bin
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.Dialog as Gtk.Dialog
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget
import {-# SOURCE #-} qualified GI.Gtk.Objects.Window as Gtk.Window

-- | Memory-managed wrapper type.
newtype AppChooserDialog = AppChooserDialog (SP.ManagedPtr AppChooserDialog)
    deriving (Eq)

instance SP.ManagedPtrNewtype AppChooserDialog where
    toManagedPtr (AppChooserDialog p) = p

foreign import ccall "gtk_app_chooser_dialog_get_type"
    c_gtk_app_chooser_dialog_get_type :: IO B.Types.GType

instance B.Types.TypedObject AppChooserDialog where
    glibType = c_gtk_app_chooser_dialog_get_type

instance B.Types.GObject AppChooserDialog

-- | Type class for types which can be safely cast to `AppChooserDialog`, for instance with `toAppChooserDialog`.
class (SP.GObject o, O.IsDescendantOf AppChooserDialog o) => IsAppChooserDialog o
instance (SP.GObject o, O.IsDescendantOf AppChooserDialog o) => IsAppChooserDialog o

instance O.HasParentTypes AppChooserDialog
type instance O.ParentTypes AppChooserDialog = '[Gtk.Dialog.Dialog, Gtk.Window.Window, Gtk.Bin.Bin, Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.AppChooser.AppChooser, Gtk.Buildable.Buildable]

-- | Cast to `AppChooserDialog`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toAppChooserDialog :: (MIO.MonadIO m, IsAppChooserDialog o) => o -> m AppChooserDialog
toAppChooserDialog = MIO.liftIO . B.ManagedPtr.unsafeCastTo AppChooserDialog

-- | Convert 'AppChooserDialog' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe AppChooserDialog) where
    gvalueGType_ = c_gtk_app_chooser_dialog_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr AppChooserDialog)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr AppChooserDialog)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject AppChooserDialog ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveAppChooserDialogMethod (t :: Symbol) (o :: *) :: * where
    ResolveAppChooserDialogMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveAppChooserDialogMethod "activateDefault" o = Gtk.Window.WindowActivateDefaultMethodInfo
    ResolveAppChooserDialogMethod "activateFocus" o = Gtk.Window.WindowActivateFocusMethodInfo
    ResolveAppChooserDialogMethod "activateKey" o = Gtk.Window.WindowActivateKeyMethodInfo
    ResolveAppChooserDialogMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveAppChooserDialogMethod "addAccelGroup" o = Gtk.Window.WindowAddAccelGroupMethodInfo
    ResolveAppChooserDialogMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveAppChooserDialogMethod "addActionWidget" o = Gtk.Dialog.DialogAddActionWidgetMethodInfo
    ResolveAppChooserDialogMethod "addButton" o = Gtk.Dialog.DialogAddButtonMethodInfo
    ResolveAppChooserDialogMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveAppChooserDialogMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveAppChooserDialogMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveAppChooserDialogMethod "addMnemonic" o = Gtk.Window.WindowAddMnemonicMethodInfo
    ResolveAppChooserDialogMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveAppChooserDialogMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveAppChooserDialogMethod "beginMoveDrag" o = Gtk.Window.WindowBeginMoveDragMethodInfo
    ResolveAppChooserDialogMethod "beginResizeDrag" o = Gtk.Window.WindowBeginResizeDragMethodInfo
    ResolveAppChooserDialogMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveAppChooserDialogMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveAppChooserDialogMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveAppChooserDialogMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveAppChooserDialogMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveAppChooserDialogMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveAppChooserDialogMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveAppChooserDialogMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveAppChooserDialogMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveAppChooserDialogMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveAppChooserDialogMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveAppChooserDialogMethod "close" o = Gtk.Window.WindowCloseMethodInfo
    ResolveAppChooserDialogMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveAppChooserDialogMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveAppChooserDialogMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveAppChooserDialogMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveAppChooserDialogMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveAppChooserDialogMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveAppChooserDialogMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveAppChooserDialogMethod "deiconify" o = Gtk.Window.WindowDeiconifyMethodInfo
    ResolveAppChooserDialogMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveAppChooserDialogMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveAppChooserDialogMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveAppChooserDialogMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveAppChooserDialogMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveAppChooserDialogMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveAppChooserDialogMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveAppChooserDialogMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveAppChooserDialogMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveAppChooserDialogMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveAppChooserDialogMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveAppChooserDialogMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveAppChooserDialogMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveAppChooserDialogMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveAppChooserDialogMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveAppChooserDialogMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveAppChooserDialogMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveAppChooserDialogMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveAppChooserDialogMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveAppChooserDialogMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveAppChooserDialogMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveAppChooserDialogMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveAppChooserDialogMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveAppChooserDialogMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveAppChooserDialogMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveAppChooserDialogMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveAppChooserDialogMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveAppChooserDialogMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveAppChooserDialogMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveAppChooserDialogMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveAppChooserDialogMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveAppChooserDialogMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveAppChooserDialogMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveAppChooserDialogMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveAppChooserDialogMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveAppChooserDialogMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveAppChooserDialogMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveAppChooserDialogMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveAppChooserDialogMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveAppChooserDialogMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveAppChooserDialogMethod "fullscreen" o = Gtk.Window.WindowFullscreenMethodInfo
    ResolveAppChooserDialogMethod "fullscreenOnMonitor" o = Gtk.Window.WindowFullscreenOnMonitorMethodInfo
    ResolveAppChooserDialogMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveAppChooserDialogMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveAppChooserDialogMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveAppChooserDialogMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveAppChooserDialogMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveAppChooserDialogMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveAppChooserDialogMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveAppChooserDialogMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveAppChooserDialogMethod "hasGroup" o = Gtk.Window.WindowHasGroupMethodInfo
    ResolveAppChooserDialogMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveAppChooserDialogMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveAppChooserDialogMethod "hasToplevelFocus" o = Gtk.Window.WindowHasToplevelFocusMethodInfo
    ResolveAppChooserDialogMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveAppChooserDialogMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveAppChooserDialogMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveAppChooserDialogMethod "iconify" o = Gtk.Window.WindowIconifyMethodInfo
    ResolveAppChooserDialogMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveAppChooserDialogMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveAppChooserDialogMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveAppChooserDialogMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveAppChooserDialogMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveAppChooserDialogMethod "isActive" o = Gtk.Window.WindowIsActiveMethodInfo
    ResolveAppChooserDialogMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveAppChooserDialogMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveAppChooserDialogMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveAppChooserDialogMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveAppChooserDialogMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveAppChooserDialogMethod "isMaximized" o = Gtk.Window.WindowIsMaximizedMethodInfo
    ResolveAppChooserDialogMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveAppChooserDialogMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveAppChooserDialogMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveAppChooserDialogMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveAppChooserDialogMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveAppChooserDialogMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveAppChooserDialogMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveAppChooserDialogMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveAppChooserDialogMethod "maximize" o = Gtk.Window.WindowMaximizeMethodInfo
    ResolveAppChooserDialogMethod "mnemonicActivate" o = Gtk.Window.WindowMnemonicActivateMethodInfo
    ResolveAppChooserDialogMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveAppChooserDialogMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveAppChooserDialogMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveAppChooserDialogMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveAppChooserDialogMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveAppChooserDialogMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveAppChooserDialogMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveAppChooserDialogMethod "move" o = Gtk.Window.WindowMoveMethodInfo
    ResolveAppChooserDialogMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveAppChooserDialogMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveAppChooserDialogMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveAppChooserDialogMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveAppChooserDialogMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveAppChooserDialogMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveAppChooserDialogMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveAppChooserDialogMethod "parseGeometry" o = Gtk.Window.WindowParseGeometryMethodInfo
    ResolveAppChooserDialogMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveAppChooserDialogMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveAppChooserDialogMethod "present" o = Gtk.Window.WindowPresentMethodInfo
    ResolveAppChooserDialogMethod "presentWithTime" o = Gtk.Window.WindowPresentWithTimeMethodInfo
    ResolveAppChooserDialogMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveAppChooserDialogMethod "propagateKeyEvent" o = Gtk.Window.WindowPropagateKeyEventMethodInfo
    ResolveAppChooserDialogMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveAppChooserDialogMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveAppChooserDialogMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveAppChooserDialogMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveAppChooserDialogMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveAppChooserDialogMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveAppChooserDialogMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveAppChooserDialogMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveAppChooserDialogMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveAppChooserDialogMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveAppChooserDialogMethod "refresh" o = Gtk.AppChooser.AppChooserRefreshMethodInfo
    ResolveAppChooserDialogMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveAppChooserDialogMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveAppChooserDialogMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolveAppChooserDialogMethod "removeAccelGroup" o = Gtk.Window.WindowRemoveAccelGroupMethodInfo
    ResolveAppChooserDialogMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveAppChooserDialogMethod "removeMnemonic" o = Gtk.Window.WindowRemoveMnemonicMethodInfo
    ResolveAppChooserDialogMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveAppChooserDialogMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveAppChooserDialogMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveAppChooserDialogMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveAppChooserDialogMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveAppChooserDialogMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveAppChooserDialogMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveAppChooserDialogMethod "reshowWithInitialSize" o = Gtk.Window.WindowReshowWithInitialSizeMethodInfo
    ResolveAppChooserDialogMethod "resize" o = Gtk.Window.WindowResizeMethodInfo
    ResolveAppChooserDialogMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveAppChooserDialogMethod "resizeGripIsVisible" o = Gtk.Window.WindowResizeGripIsVisibleMethodInfo
    ResolveAppChooserDialogMethod "resizeToGeometry" o = Gtk.Window.WindowResizeToGeometryMethodInfo
    ResolveAppChooserDialogMethod "response" o = Gtk.Dialog.DialogResponseMethodInfo
    ResolveAppChooserDialogMethod "run" o = Gtk.Dialog.DialogRunMethodInfo
    ResolveAppChooserDialogMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveAppChooserDialogMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveAppChooserDialogMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveAppChooserDialogMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveAppChooserDialogMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveAppChooserDialogMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveAppChooserDialogMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveAppChooserDialogMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveAppChooserDialogMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveAppChooserDialogMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveAppChooserDialogMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveAppChooserDialogMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveAppChooserDialogMethod "stick" o = Gtk.Window.WindowStickMethodInfo
    ResolveAppChooserDialogMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveAppChooserDialogMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveAppChooserDialogMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveAppChooserDialogMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveAppChooserDialogMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveAppChooserDialogMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveAppChooserDialogMethod "unfullscreen" o = Gtk.Window.WindowUnfullscreenMethodInfo
    ResolveAppChooserDialogMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveAppChooserDialogMethod "unmaximize" o = Gtk.Window.WindowUnmaximizeMethodInfo
    ResolveAppChooserDialogMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveAppChooserDialogMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveAppChooserDialogMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveAppChooserDialogMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveAppChooserDialogMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveAppChooserDialogMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveAppChooserDialogMethod "unstick" o = Gtk.Window.WindowUnstickMethodInfo
    ResolveAppChooserDialogMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveAppChooserDialogMethod "getAcceptFocus" o = Gtk.Window.WindowGetAcceptFocusMethodInfo
    ResolveAppChooserDialogMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveAppChooserDialogMethod "getActionArea" o = Gtk.Dialog.DialogGetActionAreaMethodInfo
    ResolveAppChooserDialogMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveAppChooserDialogMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveAppChooserDialogMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveAppChooserDialogMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveAppChooserDialogMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveAppChooserDialogMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveAppChooserDialogMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveAppChooserDialogMethod "getAppInfo" o = Gtk.AppChooser.AppChooserGetAppInfoMethodInfo
    ResolveAppChooserDialogMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveAppChooserDialogMethod "getApplication" o = Gtk.Window.WindowGetApplicationMethodInfo
    ResolveAppChooserDialogMethod "getAttachedTo" o = Gtk.Window.WindowGetAttachedToMethodInfo
    ResolveAppChooserDialogMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveAppChooserDialogMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveAppChooserDialogMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveAppChooserDialogMethod "getChild" o = Gtk.Bin.BinGetChildMethodInfo
    ResolveAppChooserDialogMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveAppChooserDialogMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveAppChooserDialogMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveAppChooserDialogMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveAppChooserDialogMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveAppChooserDialogMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveAppChooserDialogMethod "getContentArea" o = Gtk.Dialog.DialogGetContentAreaMethodInfo
    ResolveAppChooserDialogMethod "getContentType" o = Gtk.AppChooser.AppChooserGetContentTypeMethodInfo
    ResolveAppChooserDialogMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveAppChooserDialogMethod "getDecorated" o = Gtk.Window.WindowGetDecoratedMethodInfo
    ResolveAppChooserDialogMethod "getDefaultSize" o = Gtk.Window.WindowGetDefaultSizeMethodInfo
    ResolveAppChooserDialogMethod "getDefaultWidget" o = Gtk.Window.WindowGetDefaultWidgetMethodInfo
    ResolveAppChooserDialogMethod "getDeletable" o = Gtk.Window.WindowGetDeletableMethodInfo
    ResolveAppChooserDialogMethod "getDestroyWithParent" o = Gtk.Window.WindowGetDestroyWithParentMethodInfo
    ResolveAppChooserDialogMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveAppChooserDialogMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveAppChooserDialogMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveAppChooserDialogMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveAppChooserDialogMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveAppChooserDialogMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveAppChooserDialogMethod "getFocus" o = Gtk.Window.WindowGetFocusMethodInfo
    ResolveAppChooserDialogMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveAppChooserDialogMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveAppChooserDialogMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveAppChooserDialogMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveAppChooserDialogMethod "getFocusOnMap" o = Gtk.Window.WindowGetFocusOnMapMethodInfo
    ResolveAppChooserDialogMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveAppChooserDialogMethod "getFocusVisible" o = Gtk.Window.WindowGetFocusVisibleMethodInfo
    ResolveAppChooserDialogMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveAppChooserDialogMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveAppChooserDialogMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveAppChooserDialogMethod "getGravity" o = Gtk.Window.WindowGetGravityMethodInfo
    ResolveAppChooserDialogMethod "getGroup" o = Gtk.Window.WindowGetGroupMethodInfo
    ResolveAppChooserDialogMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveAppChooserDialogMethod "getHasResizeGrip" o = Gtk.Window.WindowGetHasResizeGripMethodInfo
    ResolveAppChooserDialogMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveAppChooserDialogMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveAppChooserDialogMethod "getHeaderBar" o = Gtk.Dialog.DialogGetHeaderBarMethodInfo
    ResolveAppChooserDialogMethod "getHeading" o = AppChooserDialogGetHeadingMethodInfo
    ResolveAppChooserDialogMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveAppChooserDialogMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveAppChooserDialogMethod "getHideTitlebarWhenMaximized" o = Gtk.Window.WindowGetHideTitlebarWhenMaximizedMethodInfo
    ResolveAppChooserDialogMethod "getIcon" o = Gtk.Window.WindowGetIconMethodInfo
    ResolveAppChooserDialogMethod "getIconList" o = Gtk.Window.WindowGetIconListMethodInfo
    ResolveAppChooserDialogMethod "getIconName" o = Gtk.Window.WindowGetIconNameMethodInfo
    ResolveAppChooserDialogMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveAppChooserDialogMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveAppChooserDialogMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveAppChooserDialogMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveAppChooserDialogMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveAppChooserDialogMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveAppChooserDialogMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveAppChooserDialogMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveAppChooserDialogMethod "getMnemonicModifier" o = Gtk.Window.WindowGetMnemonicModifierMethodInfo
    ResolveAppChooserDialogMethod "getMnemonicsVisible" o = Gtk.Window.WindowGetMnemonicsVisibleMethodInfo
    ResolveAppChooserDialogMethod "getModal" o = Gtk.Window.WindowGetModalMethodInfo
    ResolveAppChooserDialogMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveAppChooserDialogMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveAppChooserDialogMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveAppChooserDialogMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveAppChooserDialogMethod "getOpacity" o = Gtk.Window.WindowGetOpacityMethodInfo
    ResolveAppChooserDialogMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveAppChooserDialogMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveAppChooserDialogMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveAppChooserDialogMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveAppChooserDialogMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveAppChooserDialogMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveAppChooserDialogMethod "getPosition" o = Gtk.Window.WindowGetPositionMethodInfo
    ResolveAppChooserDialogMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveAppChooserDialogMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveAppChooserDialogMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveAppChooserDialogMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveAppChooserDialogMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveAppChooserDialogMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveAppChooserDialogMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveAppChooserDialogMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveAppChooserDialogMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveAppChooserDialogMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveAppChooserDialogMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveAppChooserDialogMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveAppChooserDialogMethod "getResizable" o = Gtk.Window.WindowGetResizableMethodInfo
    ResolveAppChooserDialogMethod "getResizeGripArea" o = Gtk.Window.WindowGetResizeGripAreaMethodInfo
    ResolveAppChooserDialogMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveAppChooserDialogMethod "getResponseForWidget" o = Gtk.Dialog.DialogGetResponseForWidgetMethodInfo
    ResolveAppChooserDialogMethod "getRole" o = Gtk.Window.WindowGetRoleMethodInfo
    ResolveAppChooserDialogMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveAppChooserDialogMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveAppChooserDialogMethod "getScreen" o = Gtk.Window.WindowGetScreenMethodInfo
    ResolveAppChooserDialogMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveAppChooserDialogMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveAppChooserDialogMethod "getSize" o = Gtk.Window.WindowGetSizeMethodInfo
    ResolveAppChooserDialogMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveAppChooserDialogMethod "getSkipPagerHint" o = Gtk.Window.WindowGetSkipPagerHintMethodInfo
    ResolveAppChooserDialogMethod "getSkipTaskbarHint" o = Gtk.Window.WindowGetSkipTaskbarHintMethodInfo
    ResolveAppChooserDialogMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveAppChooserDialogMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveAppChooserDialogMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveAppChooserDialogMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveAppChooserDialogMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveAppChooserDialogMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveAppChooserDialogMethod "getTitle" o = Gtk.Window.WindowGetTitleMethodInfo
    ResolveAppChooserDialogMethod "getTitlebar" o = Gtk.Window.WindowGetTitlebarMethodInfo
    ResolveAppChooserDialogMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveAppChooserDialogMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveAppChooserDialogMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveAppChooserDialogMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveAppChooserDialogMethod "getTransientFor" o = Gtk.Window.WindowGetTransientForMethodInfo
    ResolveAppChooserDialogMethod "getTypeHint" o = Gtk.Window.WindowGetTypeHintMethodInfo
    ResolveAppChooserDialogMethod "getUrgencyHint" o = Gtk.Window.WindowGetUrgencyHintMethodInfo
    ResolveAppChooserDialogMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveAppChooserDialogMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveAppChooserDialogMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveAppChooserDialogMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveAppChooserDialogMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveAppChooserDialogMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveAppChooserDialogMethod "getWidget" o = AppChooserDialogGetWidgetMethodInfo
    ResolveAppChooserDialogMethod "getWidgetForResponse" o = Gtk.Dialog.DialogGetWidgetForResponseMethodInfo
    ResolveAppChooserDialogMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveAppChooserDialogMethod "getWindowType" o = Gtk.Window.WindowGetWindowTypeMethodInfo
    ResolveAppChooserDialogMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveAppChooserDialogMethod "setAcceptFocus" o = Gtk.Window.WindowSetAcceptFocusMethodInfo
    ResolveAppChooserDialogMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveAppChooserDialogMethod "setAlternativeButtonOrderFromArray" o = Gtk.Dialog.DialogSetAlternativeButtonOrderFromArrayMethodInfo
    ResolveAppChooserDialogMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveAppChooserDialogMethod "setApplication" o = Gtk.Window.WindowSetApplicationMethodInfo
    ResolveAppChooserDialogMethod "setAttachedTo" o = Gtk.Window.WindowSetAttachedToMethodInfo
    ResolveAppChooserDialogMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveAppChooserDialogMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveAppChooserDialogMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveAppChooserDialogMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveAppChooserDialogMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveAppChooserDialogMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveAppChooserDialogMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveAppChooserDialogMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveAppChooserDialogMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveAppChooserDialogMethod "setDecorated" o = Gtk.Window.WindowSetDecoratedMethodInfo
    ResolveAppChooserDialogMethod "setDefault" o = Gtk.Window.WindowSetDefaultMethodInfo
    ResolveAppChooserDialogMethod "setDefaultGeometry" o = Gtk.Window.WindowSetDefaultGeometryMethodInfo
    ResolveAppChooserDialogMethod "setDefaultResponse" o = Gtk.Dialog.DialogSetDefaultResponseMethodInfo
    ResolveAppChooserDialogMethod "setDefaultSize" o = Gtk.Window.WindowSetDefaultSizeMethodInfo
    ResolveAppChooserDialogMethod "setDeletable" o = Gtk.Window.WindowSetDeletableMethodInfo
    ResolveAppChooserDialogMethod "setDestroyWithParent" o = Gtk.Window.WindowSetDestroyWithParentMethodInfo
    ResolveAppChooserDialogMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveAppChooserDialogMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveAppChooserDialogMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveAppChooserDialogMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveAppChooserDialogMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveAppChooserDialogMethod "setFocus" o = Gtk.Window.WindowSetFocusMethodInfo
    ResolveAppChooserDialogMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveAppChooserDialogMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveAppChooserDialogMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveAppChooserDialogMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveAppChooserDialogMethod "setFocusOnMap" o = Gtk.Window.WindowSetFocusOnMapMethodInfo
    ResolveAppChooserDialogMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveAppChooserDialogMethod "setFocusVisible" o = Gtk.Window.WindowSetFocusVisibleMethodInfo
    ResolveAppChooserDialogMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveAppChooserDialogMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveAppChooserDialogMethod "setGeometryHints" o = Gtk.Window.WindowSetGeometryHintsMethodInfo
    ResolveAppChooserDialogMethod "setGravity" o = Gtk.Window.WindowSetGravityMethodInfo
    ResolveAppChooserDialogMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveAppChooserDialogMethod "setHasResizeGrip" o = Gtk.Window.WindowSetHasResizeGripMethodInfo
    ResolveAppChooserDialogMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveAppChooserDialogMethod "setHasUserRefCount" o = Gtk.Window.WindowSetHasUserRefCountMethodInfo
    ResolveAppChooserDialogMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveAppChooserDialogMethod "setHeading" o = AppChooserDialogSetHeadingMethodInfo
    ResolveAppChooserDialogMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveAppChooserDialogMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveAppChooserDialogMethod "setHideTitlebarWhenMaximized" o = Gtk.Window.WindowSetHideTitlebarWhenMaximizedMethodInfo
    ResolveAppChooserDialogMethod "setIcon" o = Gtk.Window.WindowSetIconMethodInfo
    ResolveAppChooserDialogMethod "setIconFromFile" o = Gtk.Window.WindowSetIconFromFileMethodInfo
    ResolveAppChooserDialogMethod "setIconList" o = Gtk.Window.WindowSetIconListMethodInfo
    ResolveAppChooserDialogMethod "setIconName" o = Gtk.Window.WindowSetIconNameMethodInfo
    ResolveAppChooserDialogMethod "setKeepAbove" o = Gtk.Window.WindowSetKeepAboveMethodInfo
    ResolveAppChooserDialogMethod "setKeepBelow" o = Gtk.Window.WindowSetKeepBelowMethodInfo
    ResolveAppChooserDialogMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveAppChooserDialogMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveAppChooserDialogMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveAppChooserDialogMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveAppChooserDialogMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveAppChooserDialogMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveAppChooserDialogMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveAppChooserDialogMethod "setMnemonicModifier" o = Gtk.Window.WindowSetMnemonicModifierMethodInfo
    ResolveAppChooserDialogMethod "setMnemonicsVisible" o = Gtk.Window.WindowSetMnemonicsVisibleMethodInfo
    ResolveAppChooserDialogMethod "setModal" o = Gtk.Window.WindowSetModalMethodInfo
    ResolveAppChooserDialogMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveAppChooserDialogMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveAppChooserDialogMethod "setOpacity" o = Gtk.Window.WindowSetOpacityMethodInfo
    ResolveAppChooserDialogMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveAppChooserDialogMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveAppChooserDialogMethod "setPosition" o = Gtk.Window.WindowSetPositionMethodInfo
    ResolveAppChooserDialogMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveAppChooserDialogMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveAppChooserDialogMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveAppChooserDialogMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveAppChooserDialogMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveAppChooserDialogMethod "setResizable" o = Gtk.Window.WindowSetResizableMethodInfo
    ResolveAppChooserDialogMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveAppChooserDialogMethod "setResponseSensitive" o = Gtk.Dialog.DialogSetResponseSensitiveMethodInfo
    ResolveAppChooserDialogMethod "setRole" o = Gtk.Window.WindowSetRoleMethodInfo
    ResolveAppChooserDialogMethod "setScreen" o = Gtk.Window.WindowSetScreenMethodInfo
    ResolveAppChooserDialogMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveAppChooserDialogMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveAppChooserDialogMethod "setSkipPagerHint" o = Gtk.Window.WindowSetSkipPagerHintMethodInfo
    ResolveAppChooserDialogMethod "setSkipTaskbarHint" o = Gtk.Window.WindowSetSkipTaskbarHintMethodInfo
    ResolveAppChooserDialogMethod "setStartupId" o = Gtk.Window.WindowSetStartupIdMethodInfo
    ResolveAppChooserDialogMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveAppChooserDialogMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveAppChooserDialogMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveAppChooserDialogMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveAppChooserDialogMethod "setTitle" o = Gtk.Window.WindowSetTitleMethodInfo
    ResolveAppChooserDialogMethod "setTitlebar" o = Gtk.Window.WindowSetTitlebarMethodInfo
    ResolveAppChooserDialogMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveAppChooserDialogMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveAppChooserDialogMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveAppChooserDialogMethod "setTransientFor" o = Gtk.Window.WindowSetTransientForMethodInfo
    ResolveAppChooserDialogMethod "setTypeHint" o = Gtk.Window.WindowSetTypeHintMethodInfo
    ResolveAppChooserDialogMethod "setUrgencyHint" o = Gtk.Window.WindowSetUrgencyHintMethodInfo
    ResolveAppChooserDialogMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveAppChooserDialogMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveAppChooserDialogMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveAppChooserDialogMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveAppChooserDialogMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveAppChooserDialogMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveAppChooserDialogMethod "setWmclass" o = Gtk.Window.WindowSetWmclassMethodInfo
    ResolveAppChooserDialogMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveAppChooserDialogMethod t AppChooserDialog, O.OverloadedMethod info AppChooserDialog p) => OL.IsLabel t (AppChooserDialog -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveAppChooserDialogMethod t AppChooserDialog, O.OverloadedMethod info AppChooserDialog p, R.HasField t AppChooserDialog p) => R.HasField t AppChooserDialog p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveAppChooserDialogMethod t AppChooserDialog, O.OverloadedMethodInfo info AppChooserDialog) => OL.IsLabel t (O.MethodProxy info AppChooserDialog) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- VVV Prop "gfile"
   -- Type: TInterface (Name {namespace = "Gio", name = "File"})
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstructOnly]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gfile@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' appChooserDialog #gfile
-- @
getAppChooserDialogGfile :: (MonadIO m, IsAppChooserDialog o) => o -> m (Maybe Gio.File.File)
getAppChooserDialogGfile obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "gfile" Gio.File.File

-- | Construct a `GValueConstruct` with valid value for the “@gfile@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructAppChooserDialogGfile :: (IsAppChooserDialog o, MIO.MonadIO m, Gio.File.IsFile a) => a -> m (GValueConstruct o)
constructAppChooserDialogGfile val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "gfile" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data AppChooserDialogGfilePropertyInfo
instance AttrInfo AppChooserDialogGfilePropertyInfo where
    type AttrAllowedOps AppChooserDialogGfilePropertyInfo = '[ 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint AppChooserDialogGfilePropertyInfo = IsAppChooserDialog
    type AttrSetTypeConstraint AppChooserDialogGfilePropertyInfo = Gio.File.IsFile
    type AttrTransferTypeConstraint AppChooserDialogGfilePropertyInfo = Gio.File.IsFile
    type AttrTransferType AppChooserDialogGfilePropertyInfo = Gio.File.File
    type AttrGetType AppChooserDialogGfilePropertyInfo = (Maybe Gio.File.File)
    type AttrLabel AppChooserDialogGfilePropertyInfo = "gfile"
    type AttrOrigin AppChooserDialogGfilePropertyInfo = AppChooserDialog
    attrGet = getAppChooserDialogGfile
    attrSet = undefined
    attrTransfer _ v = do
        unsafeCastTo Gio.File.File v
    attrConstruct = constructAppChooserDialogGfile
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserDialog.gfile"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserDialog.html#g:attr:gfile"
        })
#endif

-- VVV Prop "heading"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just False)

-- | Get the value of the “@heading@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' appChooserDialog #heading
-- @
getAppChooserDialogHeading :: (MonadIO m, IsAppChooserDialog o) => o -> m (Maybe T.Text)
getAppChooserDialogHeading obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "heading"

-- | Set the value of the “@heading@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' appChooserDialog [ #heading 'Data.GI.Base.Attributes.:=' value ]
-- @
setAppChooserDialogHeading :: (MonadIO m, IsAppChooserDialog o) => o -> T.Text -> m ()
setAppChooserDialogHeading obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "heading" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@heading@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructAppChooserDialogHeading :: (IsAppChooserDialog o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructAppChooserDialogHeading val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "heading" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data AppChooserDialogHeadingPropertyInfo
instance AttrInfo AppChooserDialogHeadingPropertyInfo where
    type AttrAllowedOps AppChooserDialogHeadingPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint AppChooserDialogHeadingPropertyInfo = IsAppChooserDialog
    type AttrSetTypeConstraint AppChooserDialogHeadingPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint AppChooserDialogHeadingPropertyInfo = (~) T.Text
    type AttrTransferType AppChooserDialogHeadingPropertyInfo = T.Text
    type AttrGetType AppChooserDialogHeadingPropertyInfo = (Maybe T.Text)
    type AttrLabel AppChooserDialogHeadingPropertyInfo = "heading"
    type AttrOrigin AppChooserDialogHeadingPropertyInfo = AppChooserDialog
    attrGet = getAppChooserDialogHeading
    attrSet = setAppChooserDialogHeading
    attrTransfer _ v = do
        return v
    attrConstruct = constructAppChooserDialogHeading
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserDialog.heading"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserDialog.html#g:attr:heading"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList AppChooserDialog
type instance O.AttributeList AppChooserDialog = AppChooserDialogAttributeList
type AppChooserDialogAttributeList = ('[ '("acceptFocus", Gtk.Window.WindowAcceptFocusPropertyInfo), '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("application", Gtk.Window.WindowApplicationPropertyInfo), '("attachedTo", Gtk.Window.WindowAttachedToPropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("contentType", Gtk.AppChooser.AppChooserContentTypePropertyInfo), '("decorated", Gtk.Window.WindowDecoratedPropertyInfo), '("defaultHeight", Gtk.Window.WindowDefaultHeightPropertyInfo), '("defaultWidth", Gtk.Window.WindowDefaultWidthPropertyInfo), '("deletable", Gtk.Window.WindowDeletablePropertyInfo), '("destroyWithParent", Gtk.Window.WindowDestroyWithParentPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("focusOnMap", Gtk.Window.WindowFocusOnMapPropertyInfo), '("focusVisible", Gtk.Window.WindowFocusVisiblePropertyInfo), '("gfile", AppChooserDialogGfilePropertyInfo), '("gravity", Gtk.Window.WindowGravityPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasResizeGrip", Gtk.Window.WindowHasResizeGripPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("hasToplevelFocus", Gtk.Window.WindowHasToplevelFocusPropertyInfo), '("heading", AppChooserDialogHeadingPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("hideTitlebarWhenMaximized", Gtk.Window.WindowHideTitlebarWhenMaximizedPropertyInfo), '("icon", Gtk.Window.WindowIconPropertyInfo), '("iconName", Gtk.Window.WindowIconNamePropertyInfo), '("isActive", Gtk.Window.WindowIsActivePropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("isMaximized", Gtk.Window.WindowIsMaximizedPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("mnemonicsVisible", Gtk.Window.WindowMnemonicsVisiblePropertyInfo), '("modal", Gtk.Window.WindowModalPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("resizable", Gtk.Window.WindowResizablePropertyInfo), '("resizeGripVisible", Gtk.Window.WindowResizeGripVisiblePropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("role", Gtk.Window.WindowRolePropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("screen", Gtk.Window.WindowScreenPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("skipPagerHint", Gtk.Window.WindowSkipPagerHintPropertyInfo), '("skipTaskbarHint", Gtk.Window.WindowSkipTaskbarHintPropertyInfo), '("startupId", Gtk.Window.WindowStartupIdPropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("title", Gtk.Window.WindowTitlePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("transientFor", Gtk.Window.WindowTransientForPropertyInfo), '("type", Gtk.Window.WindowTypePropertyInfo), '("typeHint", Gtk.Window.WindowTypeHintPropertyInfo), '("urgencyHint", Gtk.Window.WindowUrgencyHintPropertyInfo), '("useHeaderBar", Gtk.Dialog.DialogUseHeaderBarPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo), '("windowPosition", Gtk.Window.WindowWindowPositionPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
appChooserDialogGfile :: AttrLabelProxy "gfile"
appChooserDialogGfile = AttrLabelProxy

appChooserDialogHeading :: AttrLabelProxy "heading"
appChooserDialogHeading = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList AppChooserDialog = AppChooserDialogSignalList
type AppChooserDialogSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("activateDefault", Gtk.Window.WindowActivateDefaultSignalInfo), '("activateFocus", Gtk.Window.WindowActivateFocusSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("close", Gtk.Dialog.DialogCloseSignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enableDebugging", Gtk.Window.WindowEnableDebuggingSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("keysChanged", Gtk.Window.WindowKeysChangedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("response", Gtk.Dialog.DialogResponseSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocus", Gtk.Window.WindowSetFocusSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method AppChooserDialog::new
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "parent"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow, or %NULL"
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
--               TInterface Name { namespace = "Gtk" , name = "DialogFlags" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "flags for this dialog"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "file"
--           , argType = TInterface Name { namespace = "Gio" , name = "File" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GFile" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "AppChooserDialog" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_app_chooser_dialog_new" gtk_app_chooser_dialog_new :: 
    Ptr Gtk.Window.Window ->                -- parent : TInterface (Name {namespace = "Gtk", name = "Window"})
    CUInt ->                                -- flags : TInterface (Name {namespace = "Gtk", name = "DialogFlags"})
    Ptr Gio.File.File ->                    -- file : TInterface (Name {namespace = "Gio", name = "File"})
    IO (Ptr AppChooserDialog)

-- | Creates a new t'GI.Gtk.Objects.AppChooserDialog.AppChooserDialog' for the provided t'GI.Gio.Interfaces.File.File',
-- to allow the user to select an application for it.
-- 
-- /Since: 3.0/
appChooserDialogNew ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Window.IsWindow a, Gio.File.IsFile b) =>
    Maybe (a)
    -- ^ /@parent@/: a t'GI.Gtk.Objects.Window.Window', or 'P.Nothing'
    -> [Gtk.Flags.DialogFlags]
    -- ^ /@flags@/: flags for this dialog
    -> b
    -- ^ /@file@/: a t'GI.Gio.Interfaces.File.File'
    -> m AppChooserDialog
    -- ^ __Returns:__ a newly created t'GI.Gtk.Objects.AppChooserDialog.AppChooserDialog'
appChooserDialogNew parent flags file = liftIO $ do
    maybeParent <- case parent of
        Nothing -> return nullPtr
        Just jParent -> do
            jParent' <- unsafeManagedPtrCastPtr jParent
            return jParent'
    let flags' = gflagsToWord flags
    file' <- unsafeManagedPtrCastPtr file
    result <- gtk_app_chooser_dialog_new maybeParent flags' file'
    checkUnexpectedReturnNULL "appChooserDialogNew" result
    result' <- (newObject AppChooserDialog) result
    whenJust parent touchManagedPtr
    touchManagedPtr file
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method AppChooserDialog::new_for_content_type
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "parent"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow, or %NULL"
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
--               TInterface Name { namespace = "Gtk" , name = "DialogFlags" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "flags for this dialog"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "content_type"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a content type string"
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
--               (TInterface Name { namespace = "Gtk" , name = "AppChooserDialog" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_app_chooser_dialog_new_for_content_type" gtk_app_chooser_dialog_new_for_content_type :: 
    Ptr Gtk.Window.Window ->                -- parent : TInterface (Name {namespace = "Gtk", name = "Window"})
    CUInt ->                                -- flags : TInterface (Name {namespace = "Gtk", name = "DialogFlags"})
    CString ->                              -- content_type : TBasicType TUTF8
    IO (Ptr AppChooserDialog)

-- | Creates a new t'GI.Gtk.Objects.AppChooserDialog.AppChooserDialog' for the provided content type,
-- to allow the user to select an application for it.
-- 
-- /Since: 3.0/
appChooserDialogNewForContentType ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Window.IsWindow a) =>
    Maybe (a)
    -- ^ /@parent@/: a t'GI.Gtk.Objects.Window.Window', or 'P.Nothing'
    -> [Gtk.Flags.DialogFlags]
    -- ^ /@flags@/: flags for this dialog
    -> T.Text
    -- ^ /@contentType@/: a content type string
    -> m AppChooserDialog
    -- ^ __Returns:__ a newly created t'GI.Gtk.Objects.AppChooserDialog.AppChooserDialog'
appChooserDialogNewForContentType parent flags contentType = liftIO $ do
    maybeParent <- case parent of
        Nothing -> return nullPtr
        Just jParent -> do
            jParent' <- unsafeManagedPtrCastPtr jParent
            return jParent'
    let flags' = gflagsToWord flags
    contentType' <- textToCString contentType
    result <- gtk_app_chooser_dialog_new_for_content_type maybeParent flags' contentType'
    checkUnexpectedReturnNULL "appChooserDialogNewForContentType" result
    result' <- (newObject AppChooserDialog) result
    whenJust parent touchManagedPtr
    freeMem contentType'
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method AppChooserDialog::get_heading
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AppChooserDialog" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAppChooserDialog"
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

foreign import ccall "gtk_app_chooser_dialog_get_heading" gtk_app_chooser_dialog_get_heading :: 
    Ptr AppChooserDialog ->                 -- self : TInterface (Name {namespace = "Gtk", name = "AppChooserDialog"})
    IO CString

-- | Returns the text to display at the top of the dialog.
appChooserDialogGetHeading ::
    (B.CallStack.HasCallStack, MonadIO m, IsAppChooserDialog a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.AppChooserDialog.AppChooserDialog'
    -> m (Maybe T.Text)
    -- ^ __Returns:__ the text to display at the top of the dialog, or 'P.Nothing', in which
    --     case a default text is displayed
appChooserDialogGetHeading self = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    result <- gtk_app_chooser_dialog_get_heading self'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToText result'
        return result''
    touchManagedPtr self
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data AppChooserDialogGetHeadingMethodInfo
instance (signature ~ (m (Maybe T.Text)), MonadIO m, IsAppChooserDialog a) => O.OverloadedMethod AppChooserDialogGetHeadingMethodInfo a signature where
    overloadedMethod = appChooserDialogGetHeading

instance O.OverloadedMethodInfo AppChooserDialogGetHeadingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserDialog.appChooserDialogGetHeading",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserDialog.html#v:appChooserDialogGetHeading"
        })


#endif

-- method AppChooserDialog::get_widget
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AppChooserDialog" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAppChooserDialog"
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

foreign import ccall "gtk_app_chooser_dialog_get_widget" gtk_app_chooser_dialog_get_widget :: 
    Ptr AppChooserDialog ->                 -- self : TInterface (Name {namespace = "Gtk", name = "AppChooserDialog"})
    IO (Ptr Gtk.Widget.Widget)

-- | Returns the t'GI.Gtk.Objects.AppChooserWidget.AppChooserWidget' of this dialog.
-- 
-- /Since: 3.0/
appChooserDialogGetWidget ::
    (B.CallStack.HasCallStack, MonadIO m, IsAppChooserDialog a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.AppChooserDialog.AppChooserDialog'
    -> m Gtk.Widget.Widget
    -- ^ __Returns:__ the t'GI.Gtk.Objects.AppChooserWidget.AppChooserWidget' of /@self@/
appChooserDialogGetWidget self = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    result <- gtk_app_chooser_dialog_get_widget self'
    checkUnexpectedReturnNULL "appChooserDialogGetWidget" result
    result' <- (newObject Gtk.Widget.Widget) result
    touchManagedPtr self
    return result'

#if defined(ENABLE_OVERLOADING)
data AppChooserDialogGetWidgetMethodInfo
instance (signature ~ (m Gtk.Widget.Widget), MonadIO m, IsAppChooserDialog a) => O.OverloadedMethod AppChooserDialogGetWidgetMethodInfo a signature where
    overloadedMethod = appChooserDialogGetWidget

instance O.OverloadedMethodInfo AppChooserDialogGetWidgetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserDialog.appChooserDialogGetWidget",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserDialog.html#v:appChooserDialogGetWidget"
        })


#endif

-- method AppChooserDialog::set_heading
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AppChooserDialog" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAppChooserDialog"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "heading"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a string containing Pango markup"
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

foreign import ccall "gtk_app_chooser_dialog_set_heading" gtk_app_chooser_dialog_set_heading :: 
    Ptr AppChooserDialog ->                 -- self : TInterface (Name {namespace = "Gtk", name = "AppChooserDialog"})
    CString ->                              -- heading : TBasicType TUTF8
    IO ()

-- | Sets the text to display at the top of the dialog.
-- If the heading is not set, the dialog displays a default text.
appChooserDialogSetHeading ::
    (B.CallStack.HasCallStack, MonadIO m, IsAppChooserDialog a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.AppChooserDialog.AppChooserDialog'
    -> T.Text
    -- ^ /@heading@/: a string containing Pango markup
    -> m ()
appChooserDialogSetHeading self heading = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    heading' <- textToCString heading
    gtk_app_chooser_dialog_set_heading self' heading'
    touchManagedPtr self
    freeMem heading'
    return ()

#if defined(ENABLE_OVERLOADING)
data AppChooserDialogSetHeadingMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsAppChooserDialog a) => O.OverloadedMethod AppChooserDialogSetHeadingMethodInfo a signature where
    overloadedMethod = appChooserDialogSetHeading

instance O.OverloadedMethodInfo AppChooserDialogSetHeadingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserDialog.appChooserDialogSetHeading",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserDialog.html#v:appChooserDialogSetHeading"
        })


#endif


