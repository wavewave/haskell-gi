{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Objects.MessageDialog.MessageDialog' presents a dialog with some message text. It’s simply a
-- convenience widget; you could construct the equivalent of t'GI.Gtk.Objects.MessageDialog.MessageDialog'
-- from t'GI.Gtk.Objects.Dialog.Dialog' without too much effort, but t'GI.Gtk.Objects.MessageDialog.MessageDialog' saves typing.
-- 
-- One difference from t'GI.Gtk.Objects.Dialog.Dialog' is that t'GI.Gtk.Objects.MessageDialog.MessageDialog' sets the
-- [Window:skipTaskbarHint]("GI.Gtk.Objects.Window#g:attr:skipTaskbarHint") property to 'P.True', so that the dialog is hidden
-- from the taskbar by default.
-- 
-- The easiest way to do a modal message dialog is to use 'GI.Gtk.Objects.Dialog.dialogRun', though
-- you can also pass in the 'GI.Gtk.Flags.DialogFlagsModal' flag, 'GI.Gtk.Objects.Dialog.dialogRun' automatically
-- makes the dialog modal and waits for the user to respond to it. 'GI.Gtk.Objects.Dialog.dialogRun'
-- returns when any dialog button is clicked.
-- 
-- An example for using a modal dialog:
-- 
-- === /C code/
-- >
-- > GtkDialogFlags flags = GTK_DIALOG_DESTROY_WITH_PARENT;
-- > dialog = gtk_message_dialog_new (parent_window,
-- >                                  flags,
-- >                                  GTK_MESSAGE_ERROR,
-- >                                  GTK_BUTTONS_CLOSE,
-- >                                  "Error reading “%s”: %s",
-- >                                  filename,
-- >                                  g_strerror (errno));
-- > gtk_dialog_run (GTK_DIALOG (dialog));
-- > gtk_widget_destroy (dialog);
-- 
-- 
-- You might do a non-modal t'GI.Gtk.Objects.MessageDialog.MessageDialog' as follows:
-- 
-- An example for a non-modal dialog:
-- 
-- === /C code/
-- >
-- > GtkDialogFlags flags = GTK_DIALOG_DESTROY_WITH_PARENT;
-- > dialog = gtk_message_dialog_new (parent_window,
-- >                                  flags,
-- >                                  GTK_MESSAGE_ERROR,
-- >                                  GTK_BUTTONS_CLOSE,
-- >                                  "Error reading “%s”: %s",
-- >                                  filename,
-- >                                  g_strerror (errno));
-- >
-- > // Destroy the dialog when the user responds to it
-- > // (e.g. clicks a button)
-- >
-- > g_signal_connect_swapped (dialog, "response",
-- >                           G_CALLBACK (gtk_widget_destroy),
-- >                           dialog);
-- 
-- 
-- = GtkMessageDialog as GtkBuildable
-- 
-- The GtkMessageDialog implementation of the GtkBuildable interface exposes
-- the message area as an internal child with the name “message_area”.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.MessageDialog
    ( 

-- * Exported types
    MessageDialog(..)                       ,
    IsMessageDialog                         ,
    toMessageDialog                         ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [activateDefault]("GI.Gtk.Objects.Window#g:method:activateDefault"), [activateFocus]("GI.Gtk.Objects.Window#g:method:activateFocus"), [activateKey]("GI.Gtk.Objects.Window#g:method:activateKey"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelGroup]("GI.Gtk.Objects.Window#g:method:addAccelGroup"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addActionWidget]("GI.Gtk.Objects.Dialog#g:method:addActionWidget"), [addButton]("GI.Gtk.Objects.Dialog#g:method:addButton"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonic]("GI.Gtk.Objects.Window#g:method:addMnemonic"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [beginMoveDrag]("GI.Gtk.Objects.Window#g:method:beginMoveDrag"), [beginResizeDrag]("GI.Gtk.Objects.Window#g:method:beginResizeDrag"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [close]("GI.Gtk.Objects.Window#g:method:close"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [deiconify]("GI.Gtk.Objects.Window#g:method:deiconify"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [fullscreen]("GI.Gtk.Objects.Window#g:method:fullscreen"), [fullscreenOnMonitor]("GI.Gtk.Objects.Window#g:method:fullscreenOnMonitor"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasGroup]("GI.Gtk.Objects.Window#g:method:hasGroup"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasToplevelFocus]("GI.Gtk.Objects.Window#g:method:hasToplevelFocus"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [iconify]("GI.Gtk.Objects.Window#g:method:iconify"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isActive]("GI.Gtk.Objects.Window#g:method:isActive"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isMaximized]("GI.Gtk.Objects.Window#g:method:isMaximized"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [maximize]("GI.Gtk.Objects.Window#g:method:maximize"), [mnemonicActivate]("GI.Gtk.Objects.Window#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [move]("GI.Gtk.Objects.Window#g:method:move"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parseGeometry]("GI.Gtk.Objects.Window#g:method:parseGeometry"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [present]("GI.Gtk.Objects.Window#g:method:present"), [presentWithTime]("GI.Gtk.Objects.Window#g:method:presentWithTime"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [propagateKeyEvent]("GI.Gtk.Objects.Window#g:method:propagateKeyEvent"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelGroup]("GI.Gtk.Objects.Window#g:method:removeAccelGroup"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonic]("GI.Gtk.Objects.Window#g:method:removeMnemonic"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [reshowWithInitialSize]("GI.Gtk.Objects.Window#g:method:reshowWithInitialSize"), [resize]("GI.Gtk.Objects.Window#g:method:resize"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [resizeGripIsVisible]("GI.Gtk.Objects.Window#g:method:resizeGripIsVisible"), [resizeToGeometry]("GI.Gtk.Objects.Window#g:method:resizeToGeometry"), [response]("GI.Gtk.Objects.Dialog#g:method:response"), [run]("GI.Gtk.Objects.Dialog#g:method:run"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [stick]("GI.Gtk.Objects.Window#g:method:stick"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unfullscreen]("GI.Gtk.Objects.Window#g:method:unfullscreen"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unmaximize]("GI.Gtk.Objects.Window#g:method:unmaximize"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [unstick]("GI.Gtk.Objects.Window#g:method:unstick"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAcceptFocus]("GI.Gtk.Objects.Window#g:method:getAcceptFocus"), [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionArea]("GI.Gtk.Objects.Dialog#g:method:getActionArea"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getApplication]("GI.Gtk.Objects.Window#g:method:getApplication"), [getAttachedTo]("GI.Gtk.Objects.Window#g:method:getAttachedTo"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChild]("GI.Gtk.Objects.Bin#g:method:getChild"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getContentArea]("GI.Gtk.Objects.Dialog#g:method:getContentArea"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDecorated]("GI.Gtk.Objects.Window#g:method:getDecorated"), [getDefaultSize]("GI.Gtk.Objects.Window#g:method:getDefaultSize"), [getDefaultWidget]("GI.Gtk.Objects.Window#g:method:getDefaultWidget"), [getDeletable]("GI.Gtk.Objects.Window#g:method:getDeletable"), [getDestroyWithParent]("GI.Gtk.Objects.Window#g:method:getDestroyWithParent"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocus]("GI.Gtk.Objects.Window#g:method:getFocus"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFocusOnMap]("GI.Gtk.Objects.Window#g:method:getFocusOnMap"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFocusVisible]("GI.Gtk.Objects.Window#g:method:getFocusVisible"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getGravity]("GI.Gtk.Objects.Window#g:method:getGravity"), [getGroup]("GI.Gtk.Objects.Window#g:method:getGroup"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasResizeGrip]("GI.Gtk.Objects.Window#g:method:getHasResizeGrip"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHeaderBar]("GI.Gtk.Objects.Dialog#g:method:getHeaderBar"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getHideTitlebarWhenMaximized]("GI.Gtk.Objects.Window#g:method:getHideTitlebarWhenMaximized"), [getIcon]("GI.Gtk.Objects.Window#g:method:getIcon"), [getIconList]("GI.Gtk.Objects.Window#g:method:getIconList"), [getIconName]("GI.Gtk.Objects.Window#g:method:getIconName"), [getImage]("GI.Gtk.Objects.MessageDialog#g:method:getImage"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getMessageArea]("GI.Gtk.Objects.MessageDialog#g:method:getMessageArea"), [getMnemonicModifier]("GI.Gtk.Objects.Window#g:method:getMnemonicModifier"), [getMnemonicsVisible]("GI.Gtk.Objects.Window#g:method:getMnemonicsVisible"), [getModal]("GI.Gtk.Objects.Window#g:method:getModal"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Window#g:method:getOpacity"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPosition]("GI.Gtk.Objects.Window#g:method:getPosition"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizable]("GI.Gtk.Objects.Window#g:method:getResizable"), [getResizeGripArea]("GI.Gtk.Objects.Window#g:method:getResizeGripArea"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getResponseForWidget]("GI.Gtk.Objects.Dialog#g:method:getResponseForWidget"), [getRole]("GI.Gtk.Objects.Window#g:method:getRole"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Window#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSize]("GI.Gtk.Objects.Window#g:method:getSize"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getSkipPagerHint]("GI.Gtk.Objects.Window#g:method:getSkipPagerHint"), [getSkipTaskbarHint]("GI.Gtk.Objects.Window#g:method:getSkipTaskbarHint"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTitle]("GI.Gtk.Objects.Window#g:method:getTitle"), [getTitlebar]("GI.Gtk.Objects.Window#g:method:getTitlebar"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getTransientFor]("GI.Gtk.Objects.Window#g:method:getTransientFor"), [getTypeHint]("GI.Gtk.Objects.Window#g:method:getTypeHint"), [getUrgencyHint]("GI.Gtk.Objects.Window#g:method:getUrgencyHint"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWidgetForResponse]("GI.Gtk.Objects.Dialog#g:method:getWidgetForResponse"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow"), [getWindowType]("GI.Gtk.Objects.Window#g:method:getWindowType").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setAcceptFocus]("GI.Gtk.Objects.Window#g:method:setAcceptFocus"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAlternativeButtonOrderFromArray]("GI.Gtk.Objects.Dialog#g:method:setAlternativeButtonOrderFromArray"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setApplication]("GI.Gtk.Objects.Window#g:method:setApplication"), [setAttachedTo]("GI.Gtk.Objects.Window#g:method:setAttachedTo"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDecorated]("GI.Gtk.Objects.Window#g:method:setDecorated"), [setDefault]("GI.Gtk.Objects.Window#g:method:setDefault"), [setDefaultGeometry]("GI.Gtk.Objects.Window#g:method:setDefaultGeometry"), [setDefaultResponse]("GI.Gtk.Objects.Dialog#g:method:setDefaultResponse"), [setDefaultSize]("GI.Gtk.Objects.Window#g:method:setDefaultSize"), [setDeletable]("GI.Gtk.Objects.Window#g:method:setDeletable"), [setDestroyWithParent]("GI.Gtk.Objects.Window#g:method:setDestroyWithParent"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocus]("GI.Gtk.Objects.Window#g:method:setFocus"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFocusOnMap]("GI.Gtk.Objects.Window#g:method:setFocusOnMap"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFocusVisible]("GI.Gtk.Objects.Window#g:method:setFocusVisible"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setGeometryHints]("GI.Gtk.Objects.Window#g:method:setGeometryHints"), [setGravity]("GI.Gtk.Objects.Window#g:method:setGravity"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasResizeGrip]("GI.Gtk.Objects.Window#g:method:setHasResizeGrip"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasUserRefCount]("GI.Gtk.Objects.Window#g:method:setHasUserRefCount"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setHideTitlebarWhenMaximized]("GI.Gtk.Objects.Window#g:method:setHideTitlebarWhenMaximized"), [setIcon]("GI.Gtk.Objects.Window#g:method:setIcon"), [setIconFromFile]("GI.Gtk.Objects.Window#g:method:setIconFromFile"), [setIconList]("GI.Gtk.Objects.Window#g:method:setIconList"), [setIconName]("GI.Gtk.Objects.Window#g:method:setIconName"), [setImage]("GI.Gtk.Objects.MessageDialog#g:method:setImage"), [setKeepAbove]("GI.Gtk.Objects.Window#g:method:setKeepAbove"), [setKeepBelow]("GI.Gtk.Objects.Window#g:method:setKeepBelow"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setMarkup]("GI.Gtk.Objects.MessageDialog#g:method:setMarkup"), [setMnemonicModifier]("GI.Gtk.Objects.Window#g:method:setMnemonicModifier"), [setMnemonicsVisible]("GI.Gtk.Objects.Window#g:method:setMnemonicsVisible"), [setModal]("GI.Gtk.Objects.Window#g:method:setModal"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Window#g:method:setOpacity"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setPosition]("GI.Gtk.Objects.Window#g:method:setPosition"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setResizable]("GI.Gtk.Objects.Window#g:method:setResizable"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setResponseSensitive]("GI.Gtk.Objects.Dialog#g:method:setResponseSensitive"), [setRole]("GI.Gtk.Objects.Window#g:method:setRole"), [setScreen]("GI.Gtk.Objects.Window#g:method:setScreen"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setSkipPagerHint]("GI.Gtk.Objects.Window#g:method:setSkipPagerHint"), [setSkipTaskbarHint]("GI.Gtk.Objects.Window#g:method:setSkipTaskbarHint"), [setStartupId]("GI.Gtk.Objects.Window#g:method:setStartupId"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTitle]("GI.Gtk.Objects.Window#g:method:setTitle"), [setTitlebar]("GI.Gtk.Objects.Window#g:method:setTitlebar"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setTransientFor]("GI.Gtk.Objects.Window#g:method:setTransientFor"), [setTypeHint]("GI.Gtk.Objects.Window#g:method:setTypeHint"), [setUrgencyHint]("GI.Gtk.Objects.Window#g:method:setUrgencyHint"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow"), [setWmclass]("GI.Gtk.Objects.Window#g:method:setWmclass").

#if defined(ENABLE_OVERLOADING)
    ResolveMessageDialogMethod              ,
#endif

-- ** getImage #method:getImage#

#if defined(ENABLE_OVERLOADING)
    MessageDialogGetImageMethodInfo         ,
#endif
    messageDialogGetImage                   ,


-- ** getMessageArea #method:getMessageArea#

#if defined(ENABLE_OVERLOADING)
    MessageDialogGetMessageAreaMethodInfo   ,
#endif
    messageDialogGetMessageArea             ,


-- ** setImage #method:setImage#

#if defined(ENABLE_OVERLOADING)
    MessageDialogSetImageMethodInfo         ,
#endif
    messageDialogSetImage                   ,


-- ** setMarkup #method:setMarkup#

#if defined(ENABLE_OVERLOADING)
    MessageDialogSetMarkupMethodInfo        ,
#endif
    messageDialogSetMarkup                  ,




 -- * Properties


-- ** buttons #attr:buttons#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    MessageDialogButtonsPropertyInfo        ,
#endif
    constructMessageDialogButtons           ,
#if defined(ENABLE_OVERLOADING)
    messageDialogButtons                    ,
#endif


-- ** image #attr:image#
-- | The image for this dialog.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    MessageDialogImagePropertyInfo          ,
#endif
    constructMessageDialogImage             ,
    getMessageDialogImage                   ,
#if defined(ENABLE_OVERLOADING)
    messageDialogImage                      ,
#endif
    setMessageDialogImage                   ,


-- ** messageArea #attr:messageArea#
-- | The t'GI.Gtk.Objects.Box.Box' that corresponds to the message area of this dialog.  See
-- 'GI.Gtk.Objects.MessageDialog.messageDialogGetMessageArea' for a detailed description of this
-- area.
-- 
-- /Since: 2.22/

#if defined(ENABLE_OVERLOADING)
    MessageDialogMessageAreaPropertyInfo    ,
#endif
    getMessageDialogMessageArea             ,
#if defined(ENABLE_OVERLOADING)
    messageDialogMessageArea                ,
#endif


-- ** messageType #attr:messageType#
-- | The type of the message.

#if defined(ENABLE_OVERLOADING)
    MessageDialogMessageTypePropertyInfo    ,
#endif
    constructMessageDialogMessageType       ,
    getMessageDialogMessageType             ,
#if defined(ENABLE_OVERLOADING)
    messageDialogMessageType                ,
#endif
    setMessageDialogMessageType             ,


-- ** secondaryText #attr:secondaryText#
-- | The secondary text of the message dialog.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    MessageDialogSecondaryTextPropertyInfo  ,
#endif
    clearMessageDialogSecondaryText         ,
    constructMessageDialogSecondaryText     ,
    getMessageDialogSecondaryText           ,
#if defined(ENABLE_OVERLOADING)
    messageDialogSecondaryText              ,
#endif
    setMessageDialogSecondaryText           ,


-- ** secondaryUseMarkup #attr:secondaryUseMarkup#
-- | 'P.True' if the secondary text of the dialog includes Pango markup.
-- See 'GI.Pango.Functions.parseMarkup'.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    MessageDialogSecondaryUseMarkupPropertyInfo,
#endif
    constructMessageDialogSecondaryUseMarkup,
    getMessageDialogSecondaryUseMarkup      ,
#if defined(ENABLE_OVERLOADING)
    messageDialogSecondaryUseMarkup         ,
#endif
    setMessageDialogSecondaryUseMarkup      ,


-- ** text #attr:text#
-- | The primary text of the message dialog. If the dialog has
-- a secondary text, this will appear as the title.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    MessageDialogTextPropertyInfo           ,
#endif
    clearMessageDialogText                  ,
    constructMessageDialogText              ,
    getMessageDialogText                    ,
#if defined(ENABLE_OVERLOADING)
    messageDialogText                       ,
#endif
    setMessageDialogText                    ,


-- ** useMarkup #attr:useMarkup#
-- | 'P.True' if the primary text of the dialog includes Pango markup.
-- See 'GI.Pango.Functions.parseMarkup'.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    MessageDialogUseMarkupPropertyInfo      ,
#endif
    constructMessageDialogUseMarkup         ,
    getMessageDialogUseMarkup               ,
#if defined(ENABLE_OVERLOADING)
    messageDialogUseMarkup                  ,
#endif
    setMessageDialogUseMarkup               ,




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
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Objects.Bin as Gtk.Bin
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.Dialog as Gtk.Dialog
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget
import {-# SOURCE #-} qualified GI.Gtk.Objects.Window as Gtk.Window

-- | Memory-managed wrapper type.
newtype MessageDialog = MessageDialog (SP.ManagedPtr MessageDialog)
    deriving (Eq)

instance SP.ManagedPtrNewtype MessageDialog where
    toManagedPtr (MessageDialog p) = p

foreign import ccall "gtk_message_dialog_get_type"
    c_gtk_message_dialog_get_type :: IO B.Types.GType

instance B.Types.TypedObject MessageDialog where
    glibType = c_gtk_message_dialog_get_type

instance B.Types.GObject MessageDialog

-- | Type class for types which can be safely cast to `MessageDialog`, for instance with `toMessageDialog`.
class (SP.GObject o, O.IsDescendantOf MessageDialog o) => IsMessageDialog o
instance (SP.GObject o, O.IsDescendantOf MessageDialog o) => IsMessageDialog o

instance O.HasParentTypes MessageDialog
type instance O.ParentTypes MessageDialog = '[Gtk.Dialog.Dialog, Gtk.Window.Window, Gtk.Bin.Bin, Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable]

-- | Cast to `MessageDialog`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toMessageDialog :: (MIO.MonadIO m, IsMessageDialog o) => o -> m MessageDialog
toMessageDialog = MIO.liftIO . B.ManagedPtr.unsafeCastTo MessageDialog

-- | Convert 'MessageDialog' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe MessageDialog) where
    gvalueGType_ = c_gtk_message_dialog_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr MessageDialog)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr MessageDialog)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject MessageDialog ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveMessageDialogMethod (t :: Symbol) (o :: *) :: * where
    ResolveMessageDialogMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveMessageDialogMethod "activateDefault" o = Gtk.Window.WindowActivateDefaultMethodInfo
    ResolveMessageDialogMethod "activateFocus" o = Gtk.Window.WindowActivateFocusMethodInfo
    ResolveMessageDialogMethod "activateKey" o = Gtk.Window.WindowActivateKeyMethodInfo
    ResolveMessageDialogMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveMessageDialogMethod "addAccelGroup" o = Gtk.Window.WindowAddAccelGroupMethodInfo
    ResolveMessageDialogMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveMessageDialogMethod "addActionWidget" o = Gtk.Dialog.DialogAddActionWidgetMethodInfo
    ResolveMessageDialogMethod "addButton" o = Gtk.Dialog.DialogAddButtonMethodInfo
    ResolveMessageDialogMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveMessageDialogMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveMessageDialogMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveMessageDialogMethod "addMnemonic" o = Gtk.Window.WindowAddMnemonicMethodInfo
    ResolveMessageDialogMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveMessageDialogMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveMessageDialogMethod "beginMoveDrag" o = Gtk.Window.WindowBeginMoveDragMethodInfo
    ResolveMessageDialogMethod "beginResizeDrag" o = Gtk.Window.WindowBeginResizeDragMethodInfo
    ResolveMessageDialogMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveMessageDialogMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveMessageDialogMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveMessageDialogMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveMessageDialogMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveMessageDialogMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveMessageDialogMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveMessageDialogMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveMessageDialogMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveMessageDialogMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveMessageDialogMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveMessageDialogMethod "close" o = Gtk.Window.WindowCloseMethodInfo
    ResolveMessageDialogMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveMessageDialogMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveMessageDialogMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveMessageDialogMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveMessageDialogMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveMessageDialogMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveMessageDialogMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveMessageDialogMethod "deiconify" o = Gtk.Window.WindowDeiconifyMethodInfo
    ResolveMessageDialogMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveMessageDialogMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveMessageDialogMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveMessageDialogMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveMessageDialogMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveMessageDialogMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveMessageDialogMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveMessageDialogMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveMessageDialogMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveMessageDialogMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveMessageDialogMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveMessageDialogMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveMessageDialogMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveMessageDialogMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveMessageDialogMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveMessageDialogMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveMessageDialogMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveMessageDialogMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveMessageDialogMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveMessageDialogMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveMessageDialogMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveMessageDialogMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveMessageDialogMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveMessageDialogMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveMessageDialogMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveMessageDialogMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveMessageDialogMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveMessageDialogMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveMessageDialogMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveMessageDialogMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveMessageDialogMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveMessageDialogMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveMessageDialogMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveMessageDialogMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveMessageDialogMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveMessageDialogMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveMessageDialogMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveMessageDialogMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveMessageDialogMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveMessageDialogMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveMessageDialogMethod "fullscreen" o = Gtk.Window.WindowFullscreenMethodInfo
    ResolveMessageDialogMethod "fullscreenOnMonitor" o = Gtk.Window.WindowFullscreenOnMonitorMethodInfo
    ResolveMessageDialogMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveMessageDialogMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveMessageDialogMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveMessageDialogMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveMessageDialogMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveMessageDialogMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveMessageDialogMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveMessageDialogMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveMessageDialogMethod "hasGroup" o = Gtk.Window.WindowHasGroupMethodInfo
    ResolveMessageDialogMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveMessageDialogMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveMessageDialogMethod "hasToplevelFocus" o = Gtk.Window.WindowHasToplevelFocusMethodInfo
    ResolveMessageDialogMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveMessageDialogMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveMessageDialogMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveMessageDialogMethod "iconify" o = Gtk.Window.WindowIconifyMethodInfo
    ResolveMessageDialogMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveMessageDialogMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveMessageDialogMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveMessageDialogMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveMessageDialogMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveMessageDialogMethod "isActive" o = Gtk.Window.WindowIsActiveMethodInfo
    ResolveMessageDialogMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveMessageDialogMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveMessageDialogMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveMessageDialogMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveMessageDialogMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveMessageDialogMethod "isMaximized" o = Gtk.Window.WindowIsMaximizedMethodInfo
    ResolveMessageDialogMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveMessageDialogMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveMessageDialogMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveMessageDialogMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveMessageDialogMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveMessageDialogMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveMessageDialogMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveMessageDialogMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveMessageDialogMethod "maximize" o = Gtk.Window.WindowMaximizeMethodInfo
    ResolveMessageDialogMethod "mnemonicActivate" o = Gtk.Window.WindowMnemonicActivateMethodInfo
    ResolveMessageDialogMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveMessageDialogMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveMessageDialogMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveMessageDialogMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveMessageDialogMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveMessageDialogMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveMessageDialogMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveMessageDialogMethod "move" o = Gtk.Window.WindowMoveMethodInfo
    ResolveMessageDialogMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveMessageDialogMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveMessageDialogMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveMessageDialogMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveMessageDialogMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveMessageDialogMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveMessageDialogMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveMessageDialogMethod "parseGeometry" o = Gtk.Window.WindowParseGeometryMethodInfo
    ResolveMessageDialogMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveMessageDialogMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveMessageDialogMethod "present" o = Gtk.Window.WindowPresentMethodInfo
    ResolveMessageDialogMethod "presentWithTime" o = Gtk.Window.WindowPresentWithTimeMethodInfo
    ResolveMessageDialogMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveMessageDialogMethod "propagateKeyEvent" o = Gtk.Window.WindowPropagateKeyEventMethodInfo
    ResolveMessageDialogMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveMessageDialogMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveMessageDialogMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveMessageDialogMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveMessageDialogMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveMessageDialogMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveMessageDialogMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveMessageDialogMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveMessageDialogMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveMessageDialogMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveMessageDialogMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveMessageDialogMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveMessageDialogMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolveMessageDialogMethod "removeAccelGroup" o = Gtk.Window.WindowRemoveAccelGroupMethodInfo
    ResolveMessageDialogMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveMessageDialogMethod "removeMnemonic" o = Gtk.Window.WindowRemoveMnemonicMethodInfo
    ResolveMessageDialogMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveMessageDialogMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveMessageDialogMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveMessageDialogMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveMessageDialogMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveMessageDialogMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveMessageDialogMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveMessageDialogMethod "reshowWithInitialSize" o = Gtk.Window.WindowReshowWithInitialSizeMethodInfo
    ResolveMessageDialogMethod "resize" o = Gtk.Window.WindowResizeMethodInfo
    ResolveMessageDialogMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveMessageDialogMethod "resizeGripIsVisible" o = Gtk.Window.WindowResizeGripIsVisibleMethodInfo
    ResolveMessageDialogMethod "resizeToGeometry" o = Gtk.Window.WindowResizeToGeometryMethodInfo
    ResolveMessageDialogMethod "response" o = Gtk.Dialog.DialogResponseMethodInfo
    ResolveMessageDialogMethod "run" o = Gtk.Dialog.DialogRunMethodInfo
    ResolveMessageDialogMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveMessageDialogMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveMessageDialogMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveMessageDialogMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveMessageDialogMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveMessageDialogMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveMessageDialogMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveMessageDialogMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveMessageDialogMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveMessageDialogMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveMessageDialogMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveMessageDialogMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveMessageDialogMethod "stick" o = Gtk.Window.WindowStickMethodInfo
    ResolveMessageDialogMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveMessageDialogMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveMessageDialogMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveMessageDialogMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveMessageDialogMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveMessageDialogMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveMessageDialogMethod "unfullscreen" o = Gtk.Window.WindowUnfullscreenMethodInfo
    ResolveMessageDialogMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveMessageDialogMethod "unmaximize" o = Gtk.Window.WindowUnmaximizeMethodInfo
    ResolveMessageDialogMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveMessageDialogMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveMessageDialogMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveMessageDialogMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveMessageDialogMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveMessageDialogMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveMessageDialogMethod "unstick" o = Gtk.Window.WindowUnstickMethodInfo
    ResolveMessageDialogMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveMessageDialogMethod "getAcceptFocus" o = Gtk.Window.WindowGetAcceptFocusMethodInfo
    ResolveMessageDialogMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveMessageDialogMethod "getActionArea" o = Gtk.Dialog.DialogGetActionAreaMethodInfo
    ResolveMessageDialogMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveMessageDialogMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveMessageDialogMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveMessageDialogMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveMessageDialogMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveMessageDialogMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveMessageDialogMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveMessageDialogMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveMessageDialogMethod "getApplication" o = Gtk.Window.WindowGetApplicationMethodInfo
    ResolveMessageDialogMethod "getAttachedTo" o = Gtk.Window.WindowGetAttachedToMethodInfo
    ResolveMessageDialogMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveMessageDialogMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveMessageDialogMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveMessageDialogMethod "getChild" o = Gtk.Bin.BinGetChildMethodInfo
    ResolveMessageDialogMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveMessageDialogMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveMessageDialogMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveMessageDialogMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveMessageDialogMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveMessageDialogMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveMessageDialogMethod "getContentArea" o = Gtk.Dialog.DialogGetContentAreaMethodInfo
    ResolveMessageDialogMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveMessageDialogMethod "getDecorated" o = Gtk.Window.WindowGetDecoratedMethodInfo
    ResolveMessageDialogMethod "getDefaultSize" o = Gtk.Window.WindowGetDefaultSizeMethodInfo
    ResolveMessageDialogMethod "getDefaultWidget" o = Gtk.Window.WindowGetDefaultWidgetMethodInfo
    ResolveMessageDialogMethod "getDeletable" o = Gtk.Window.WindowGetDeletableMethodInfo
    ResolveMessageDialogMethod "getDestroyWithParent" o = Gtk.Window.WindowGetDestroyWithParentMethodInfo
    ResolveMessageDialogMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveMessageDialogMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveMessageDialogMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveMessageDialogMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveMessageDialogMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveMessageDialogMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveMessageDialogMethod "getFocus" o = Gtk.Window.WindowGetFocusMethodInfo
    ResolveMessageDialogMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveMessageDialogMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveMessageDialogMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveMessageDialogMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveMessageDialogMethod "getFocusOnMap" o = Gtk.Window.WindowGetFocusOnMapMethodInfo
    ResolveMessageDialogMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveMessageDialogMethod "getFocusVisible" o = Gtk.Window.WindowGetFocusVisibleMethodInfo
    ResolveMessageDialogMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveMessageDialogMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveMessageDialogMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveMessageDialogMethod "getGravity" o = Gtk.Window.WindowGetGravityMethodInfo
    ResolveMessageDialogMethod "getGroup" o = Gtk.Window.WindowGetGroupMethodInfo
    ResolveMessageDialogMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveMessageDialogMethod "getHasResizeGrip" o = Gtk.Window.WindowGetHasResizeGripMethodInfo
    ResolveMessageDialogMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveMessageDialogMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveMessageDialogMethod "getHeaderBar" o = Gtk.Dialog.DialogGetHeaderBarMethodInfo
    ResolveMessageDialogMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveMessageDialogMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveMessageDialogMethod "getHideTitlebarWhenMaximized" o = Gtk.Window.WindowGetHideTitlebarWhenMaximizedMethodInfo
    ResolveMessageDialogMethod "getIcon" o = Gtk.Window.WindowGetIconMethodInfo
    ResolveMessageDialogMethod "getIconList" o = Gtk.Window.WindowGetIconListMethodInfo
    ResolveMessageDialogMethod "getIconName" o = Gtk.Window.WindowGetIconNameMethodInfo
    ResolveMessageDialogMethod "getImage" o = MessageDialogGetImageMethodInfo
    ResolveMessageDialogMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveMessageDialogMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveMessageDialogMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveMessageDialogMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveMessageDialogMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveMessageDialogMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveMessageDialogMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveMessageDialogMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveMessageDialogMethod "getMessageArea" o = MessageDialogGetMessageAreaMethodInfo
    ResolveMessageDialogMethod "getMnemonicModifier" o = Gtk.Window.WindowGetMnemonicModifierMethodInfo
    ResolveMessageDialogMethod "getMnemonicsVisible" o = Gtk.Window.WindowGetMnemonicsVisibleMethodInfo
    ResolveMessageDialogMethod "getModal" o = Gtk.Window.WindowGetModalMethodInfo
    ResolveMessageDialogMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveMessageDialogMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveMessageDialogMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveMessageDialogMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveMessageDialogMethod "getOpacity" o = Gtk.Window.WindowGetOpacityMethodInfo
    ResolveMessageDialogMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveMessageDialogMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveMessageDialogMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveMessageDialogMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveMessageDialogMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveMessageDialogMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveMessageDialogMethod "getPosition" o = Gtk.Window.WindowGetPositionMethodInfo
    ResolveMessageDialogMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveMessageDialogMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveMessageDialogMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveMessageDialogMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveMessageDialogMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveMessageDialogMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveMessageDialogMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveMessageDialogMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveMessageDialogMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveMessageDialogMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveMessageDialogMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveMessageDialogMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveMessageDialogMethod "getResizable" o = Gtk.Window.WindowGetResizableMethodInfo
    ResolveMessageDialogMethod "getResizeGripArea" o = Gtk.Window.WindowGetResizeGripAreaMethodInfo
    ResolveMessageDialogMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveMessageDialogMethod "getResponseForWidget" o = Gtk.Dialog.DialogGetResponseForWidgetMethodInfo
    ResolveMessageDialogMethod "getRole" o = Gtk.Window.WindowGetRoleMethodInfo
    ResolveMessageDialogMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveMessageDialogMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveMessageDialogMethod "getScreen" o = Gtk.Window.WindowGetScreenMethodInfo
    ResolveMessageDialogMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveMessageDialogMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveMessageDialogMethod "getSize" o = Gtk.Window.WindowGetSizeMethodInfo
    ResolveMessageDialogMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveMessageDialogMethod "getSkipPagerHint" o = Gtk.Window.WindowGetSkipPagerHintMethodInfo
    ResolveMessageDialogMethod "getSkipTaskbarHint" o = Gtk.Window.WindowGetSkipTaskbarHintMethodInfo
    ResolveMessageDialogMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveMessageDialogMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveMessageDialogMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveMessageDialogMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveMessageDialogMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveMessageDialogMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveMessageDialogMethod "getTitle" o = Gtk.Window.WindowGetTitleMethodInfo
    ResolveMessageDialogMethod "getTitlebar" o = Gtk.Window.WindowGetTitlebarMethodInfo
    ResolveMessageDialogMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveMessageDialogMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveMessageDialogMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveMessageDialogMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveMessageDialogMethod "getTransientFor" o = Gtk.Window.WindowGetTransientForMethodInfo
    ResolveMessageDialogMethod "getTypeHint" o = Gtk.Window.WindowGetTypeHintMethodInfo
    ResolveMessageDialogMethod "getUrgencyHint" o = Gtk.Window.WindowGetUrgencyHintMethodInfo
    ResolveMessageDialogMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveMessageDialogMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveMessageDialogMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveMessageDialogMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveMessageDialogMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveMessageDialogMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveMessageDialogMethod "getWidgetForResponse" o = Gtk.Dialog.DialogGetWidgetForResponseMethodInfo
    ResolveMessageDialogMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveMessageDialogMethod "getWindowType" o = Gtk.Window.WindowGetWindowTypeMethodInfo
    ResolveMessageDialogMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveMessageDialogMethod "setAcceptFocus" o = Gtk.Window.WindowSetAcceptFocusMethodInfo
    ResolveMessageDialogMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveMessageDialogMethod "setAlternativeButtonOrderFromArray" o = Gtk.Dialog.DialogSetAlternativeButtonOrderFromArrayMethodInfo
    ResolveMessageDialogMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveMessageDialogMethod "setApplication" o = Gtk.Window.WindowSetApplicationMethodInfo
    ResolveMessageDialogMethod "setAttachedTo" o = Gtk.Window.WindowSetAttachedToMethodInfo
    ResolveMessageDialogMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveMessageDialogMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveMessageDialogMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveMessageDialogMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveMessageDialogMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveMessageDialogMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveMessageDialogMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveMessageDialogMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveMessageDialogMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveMessageDialogMethod "setDecorated" o = Gtk.Window.WindowSetDecoratedMethodInfo
    ResolveMessageDialogMethod "setDefault" o = Gtk.Window.WindowSetDefaultMethodInfo
    ResolveMessageDialogMethod "setDefaultGeometry" o = Gtk.Window.WindowSetDefaultGeometryMethodInfo
    ResolveMessageDialogMethod "setDefaultResponse" o = Gtk.Dialog.DialogSetDefaultResponseMethodInfo
    ResolveMessageDialogMethod "setDefaultSize" o = Gtk.Window.WindowSetDefaultSizeMethodInfo
    ResolveMessageDialogMethod "setDeletable" o = Gtk.Window.WindowSetDeletableMethodInfo
    ResolveMessageDialogMethod "setDestroyWithParent" o = Gtk.Window.WindowSetDestroyWithParentMethodInfo
    ResolveMessageDialogMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveMessageDialogMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveMessageDialogMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveMessageDialogMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveMessageDialogMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveMessageDialogMethod "setFocus" o = Gtk.Window.WindowSetFocusMethodInfo
    ResolveMessageDialogMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveMessageDialogMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveMessageDialogMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveMessageDialogMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveMessageDialogMethod "setFocusOnMap" o = Gtk.Window.WindowSetFocusOnMapMethodInfo
    ResolveMessageDialogMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveMessageDialogMethod "setFocusVisible" o = Gtk.Window.WindowSetFocusVisibleMethodInfo
    ResolveMessageDialogMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveMessageDialogMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveMessageDialogMethod "setGeometryHints" o = Gtk.Window.WindowSetGeometryHintsMethodInfo
    ResolveMessageDialogMethod "setGravity" o = Gtk.Window.WindowSetGravityMethodInfo
    ResolveMessageDialogMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveMessageDialogMethod "setHasResizeGrip" o = Gtk.Window.WindowSetHasResizeGripMethodInfo
    ResolveMessageDialogMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveMessageDialogMethod "setHasUserRefCount" o = Gtk.Window.WindowSetHasUserRefCountMethodInfo
    ResolveMessageDialogMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveMessageDialogMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveMessageDialogMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveMessageDialogMethod "setHideTitlebarWhenMaximized" o = Gtk.Window.WindowSetHideTitlebarWhenMaximizedMethodInfo
    ResolveMessageDialogMethod "setIcon" o = Gtk.Window.WindowSetIconMethodInfo
    ResolveMessageDialogMethod "setIconFromFile" o = Gtk.Window.WindowSetIconFromFileMethodInfo
    ResolveMessageDialogMethod "setIconList" o = Gtk.Window.WindowSetIconListMethodInfo
    ResolveMessageDialogMethod "setIconName" o = Gtk.Window.WindowSetIconNameMethodInfo
    ResolveMessageDialogMethod "setImage" o = MessageDialogSetImageMethodInfo
    ResolveMessageDialogMethod "setKeepAbove" o = Gtk.Window.WindowSetKeepAboveMethodInfo
    ResolveMessageDialogMethod "setKeepBelow" o = Gtk.Window.WindowSetKeepBelowMethodInfo
    ResolveMessageDialogMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveMessageDialogMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveMessageDialogMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveMessageDialogMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveMessageDialogMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveMessageDialogMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveMessageDialogMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveMessageDialogMethod "setMarkup" o = MessageDialogSetMarkupMethodInfo
    ResolveMessageDialogMethod "setMnemonicModifier" o = Gtk.Window.WindowSetMnemonicModifierMethodInfo
    ResolveMessageDialogMethod "setMnemonicsVisible" o = Gtk.Window.WindowSetMnemonicsVisibleMethodInfo
    ResolveMessageDialogMethod "setModal" o = Gtk.Window.WindowSetModalMethodInfo
    ResolveMessageDialogMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveMessageDialogMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveMessageDialogMethod "setOpacity" o = Gtk.Window.WindowSetOpacityMethodInfo
    ResolveMessageDialogMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveMessageDialogMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveMessageDialogMethod "setPosition" o = Gtk.Window.WindowSetPositionMethodInfo
    ResolveMessageDialogMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveMessageDialogMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveMessageDialogMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveMessageDialogMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveMessageDialogMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveMessageDialogMethod "setResizable" o = Gtk.Window.WindowSetResizableMethodInfo
    ResolveMessageDialogMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveMessageDialogMethod "setResponseSensitive" o = Gtk.Dialog.DialogSetResponseSensitiveMethodInfo
    ResolveMessageDialogMethod "setRole" o = Gtk.Window.WindowSetRoleMethodInfo
    ResolveMessageDialogMethod "setScreen" o = Gtk.Window.WindowSetScreenMethodInfo
    ResolveMessageDialogMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveMessageDialogMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveMessageDialogMethod "setSkipPagerHint" o = Gtk.Window.WindowSetSkipPagerHintMethodInfo
    ResolveMessageDialogMethod "setSkipTaskbarHint" o = Gtk.Window.WindowSetSkipTaskbarHintMethodInfo
    ResolveMessageDialogMethod "setStartupId" o = Gtk.Window.WindowSetStartupIdMethodInfo
    ResolveMessageDialogMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveMessageDialogMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveMessageDialogMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveMessageDialogMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveMessageDialogMethod "setTitle" o = Gtk.Window.WindowSetTitleMethodInfo
    ResolveMessageDialogMethod "setTitlebar" o = Gtk.Window.WindowSetTitlebarMethodInfo
    ResolveMessageDialogMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveMessageDialogMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveMessageDialogMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveMessageDialogMethod "setTransientFor" o = Gtk.Window.WindowSetTransientForMethodInfo
    ResolveMessageDialogMethod "setTypeHint" o = Gtk.Window.WindowSetTypeHintMethodInfo
    ResolveMessageDialogMethod "setUrgencyHint" o = Gtk.Window.WindowSetUrgencyHintMethodInfo
    ResolveMessageDialogMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveMessageDialogMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveMessageDialogMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveMessageDialogMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveMessageDialogMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveMessageDialogMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveMessageDialogMethod "setWmclass" o = Gtk.Window.WindowSetWmclassMethodInfo
    ResolveMessageDialogMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveMessageDialogMethod t MessageDialog, O.OverloadedMethod info MessageDialog p) => OL.IsLabel t (MessageDialog -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveMessageDialogMethod t MessageDialog, O.OverloadedMethod info MessageDialog p, R.HasField t MessageDialog p) => R.HasField t MessageDialog p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveMessageDialogMethod t MessageDialog, O.OverloadedMethodInfo info MessageDialog) => OL.IsLabel t (O.MethodProxy info MessageDialog) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- VVV Prop "buttons"
   -- Type: TInterface (Name {namespace = "Gtk", name = "ButtonsType"})
   -- Flags: [PropertyWritable,PropertyConstructOnly]
   -- Nullable: (Nothing,Nothing)

-- | Construct a `GValueConstruct` with valid value for the “@buttons@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructMessageDialogButtons :: (IsMessageDialog o, MIO.MonadIO m) => Gtk.Enums.ButtonsType -> m (GValueConstruct o)
constructMessageDialogButtons val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "buttons" val

#if defined(ENABLE_OVERLOADING)
data MessageDialogButtonsPropertyInfo
instance AttrInfo MessageDialogButtonsPropertyInfo where
    type AttrAllowedOps MessageDialogButtonsPropertyInfo = '[ 'AttrConstruct]
    type AttrBaseTypeConstraint MessageDialogButtonsPropertyInfo = IsMessageDialog
    type AttrSetTypeConstraint MessageDialogButtonsPropertyInfo = (~) Gtk.Enums.ButtonsType
    type AttrTransferTypeConstraint MessageDialogButtonsPropertyInfo = (~) Gtk.Enums.ButtonsType
    type AttrTransferType MessageDialogButtonsPropertyInfo = Gtk.Enums.ButtonsType
    type AttrGetType MessageDialogButtonsPropertyInfo = ()
    type AttrLabel MessageDialogButtonsPropertyInfo = "buttons"
    type AttrOrigin MessageDialogButtonsPropertyInfo = MessageDialog
    attrGet = undefined
    attrSet = undefined
    attrTransfer _ v = do
        return v
    attrConstruct = constructMessageDialogButtons
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MessageDialog.buttons"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MessageDialog.html#g:attr:buttons"
        })
#endif

-- VVV Prop "image"
   -- Type: TInterface (Name {namespace = "Gtk", name = "Widget"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@image@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' messageDialog #image
-- @
getMessageDialogImage :: (MonadIO m, IsMessageDialog o) => o -> m Gtk.Widget.Widget
getMessageDialogImage obj = MIO.liftIO $ checkUnexpectedNothing "getMessageDialogImage" $ B.Properties.getObjectPropertyObject obj "image" Gtk.Widget.Widget

-- | Set the value of the “@image@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' messageDialog [ #image 'Data.GI.Base.Attributes.:=' value ]
-- @
setMessageDialogImage :: (MonadIO m, IsMessageDialog o, Gtk.Widget.IsWidget a) => o -> a -> m ()
setMessageDialogImage obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "image" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@image@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructMessageDialogImage :: (IsMessageDialog o, MIO.MonadIO m, Gtk.Widget.IsWidget a) => a -> m (GValueConstruct o)
constructMessageDialogImage val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "image" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data MessageDialogImagePropertyInfo
instance AttrInfo MessageDialogImagePropertyInfo where
    type AttrAllowedOps MessageDialogImagePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint MessageDialogImagePropertyInfo = IsMessageDialog
    type AttrSetTypeConstraint MessageDialogImagePropertyInfo = Gtk.Widget.IsWidget
    type AttrTransferTypeConstraint MessageDialogImagePropertyInfo = Gtk.Widget.IsWidget
    type AttrTransferType MessageDialogImagePropertyInfo = Gtk.Widget.Widget
    type AttrGetType MessageDialogImagePropertyInfo = Gtk.Widget.Widget
    type AttrLabel MessageDialogImagePropertyInfo = "image"
    type AttrOrigin MessageDialogImagePropertyInfo = MessageDialog
    attrGet = getMessageDialogImage
    attrSet = setMessageDialogImage
    attrTransfer _ v = do
        unsafeCastTo Gtk.Widget.Widget v
    attrConstruct = constructMessageDialogImage
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MessageDialog.image"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MessageDialog.html#g:attr:image"
        })
#endif

-- VVV Prop "message-area"
   -- Type: TInterface (Name {namespace = "Gtk", name = "Widget"})
   -- Flags: [PropertyReadable]
   -- Nullable: (Just False,Nothing)

-- | Get the value of the “@message-area@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' messageDialog #messageArea
-- @
getMessageDialogMessageArea :: (MonadIO m, IsMessageDialog o) => o -> m Gtk.Widget.Widget
getMessageDialogMessageArea obj = MIO.liftIO $ checkUnexpectedNothing "getMessageDialogMessageArea" $ B.Properties.getObjectPropertyObject obj "message-area" Gtk.Widget.Widget

#if defined(ENABLE_OVERLOADING)
data MessageDialogMessageAreaPropertyInfo
instance AttrInfo MessageDialogMessageAreaPropertyInfo where
    type AttrAllowedOps MessageDialogMessageAreaPropertyInfo = '[ 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint MessageDialogMessageAreaPropertyInfo = IsMessageDialog
    type AttrSetTypeConstraint MessageDialogMessageAreaPropertyInfo = (~) ()
    type AttrTransferTypeConstraint MessageDialogMessageAreaPropertyInfo = (~) ()
    type AttrTransferType MessageDialogMessageAreaPropertyInfo = ()
    type AttrGetType MessageDialogMessageAreaPropertyInfo = Gtk.Widget.Widget
    type AttrLabel MessageDialogMessageAreaPropertyInfo = "message-area"
    type AttrOrigin MessageDialogMessageAreaPropertyInfo = MessageDialog
    attrGet = getMessageDialogMessageArea
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MessageDialog.messageArea"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MessageDialog.html#g:attr:messageArea"
        })
#endif

-- VVV Prop "message-type"
   -- Type: TInterface (Name {namespace = "Gtk", name = "MessageType"})
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstruct]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@message-type@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' messageDialog #messageType
-- @
getMessageDialogMessageType :: (MonadIO m, IsMessageDialog o) => o -> m Gtk.Enums.MessageType
getMessageDialogMessageType obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "message-type"

-- | Set the value of the “@message-type@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' messageDialog [ #messageType 'Data.GI.Base.Attributes.:=' value ]
-- @
setMessageDialogMessageType :: (MonadIO m, IsMessageDialog o) => o -> Gtk.Enums.MessageType -> m ()
setMessageDialogMessageType obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "message-type" val

-- | Construct a `GValueConstruct` with valid value for the “@message-type@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructMessageDialogMessageType :: (IsMessageDialog o, MIO.MonadIO m) => Gtk.Enums.MessageType -> m (GValueConstruct o)
constructMessageDialogMessageType val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "message-type" val

#if defined(ENABLE_OVERLOADING)
data MessageDialogMessageTypePropertyInfo
instance AttrInfo MessageDialogMessageTypePropertyInfo where
    type AttrAllowedOps MessageDialogMessageTypePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint MessageDialogMessageTypePropertyInfo = IsMessageDialog
    type AttrSetTypeConstraint MessageDialogMessageTypePropertyInfo = (~) Gtk.Enums.MessageType
    type AttrTransferTypeConstraint MessageDialogMessageTypePropertyInfo = (~) Gtk.Enums.MessageType
    type AttrTransferType MessageDialogMessageTypePropertyInfo = Gtk.Enums.MessageType
    type AttrGetType MessageDialogMessageTypePropertyInfo = Gtk.Enums.MessageType
    type AttrLabel MessageDialogMessageTypePropertyInfo = "message-type"
    type AttrOrigin MessageDialogMessageTypePropertyInfo = MessageDialog
    attrGet = getMessageDialogMessageType
    attrSet = setMessageDialogMessageType
    attrTransfer _ v = do
        return v
    attrConstruct = constructMessageDialogMessageType
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MessageDialog.messageType"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MessageDialog.html#g:attr:messageType"
        })
#endif

-- VVV Prop "secondary-text"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@secondary-text@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' messageDialog #secondaryText
-- @
getMessageDialogSecondaryText :: (MonadIO m, IsMessageDialog o) => o -> m (Maybe T.Text)
getMessageDialogSecondaryText obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "secondary-text"

-- | Set the value of the “@secondary-text@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' messageDialog [ #secondaryText 'Data.GI.Base.Attributes.:=' value ]
-- @
setMessageDialogSecondaryText :: (MonadIO m, IsMessageDialog o) => o -> T.Text -> m ()
setMessageDialogSecondaryText obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "secondary-text" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@secondary-text@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructMessageDialogSecondaryText :: (IsMessageDialog o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructMessageDialogSecondaryText val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "secondary-text" (P.Just val)

-- | Set the value of the “@secondary-text@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #secondaryText
-- @
clearMessageDialogSecondaryText :: (MonadIO m, IsMessageDialog o) => o -> m ()
clearMessageDialogSecondaryText obj = liftIO $ B.Properties.setObjectPropertyString obj "secondary-text" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data MessageDialogSecondaryTextPropertyInfo
instance AttrInfo MessageDialogSecondaryTextPropertyInfo where
    type AttrAllowedOps MessageDialogSecondaryTextPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint MessageDialogSecondaryTextPropertyInfo = IsMessageDialog
    type AttrSetTypeConstraint MessageDialogSecondaryTextPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint MessageDialogSecondaryTextPropertyInfo = (~) T.Text
    type AttrTransferType MessageDialogSecondaryTextPropertyInfo = T.Text
    type AttrGetType MessageDialogSecondaryTextPropertyInfo = (Maybe T.Text)
    type AttrLabel MessageDialogSecondaryTextPropertyInfo = "secondary-text"
    type AttrOrigin MessageDialogSecondaryTextPropertyInfo = MessageDialog
    attrGet = getMessageDialogSecondaryText
    attrSet = setMessageDialogSecondaryText
    attrTransfer _ v = do
        return v
    attrConstruct = constructMessageDialogSecondaryText
    attrClear = clearMessageDialogSecondaryText
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MessageDialog.secondaryText"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MessageDialog.html#g:attr:secondaryText"
        })
#endif

-- VVV Prop "secondary-use-markup"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@secondary-use-markup@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' messageDialog #secondaryUseMarkup
-- @
getMessageDialogSecondaryUseMarkup :: (MonadIO m, IsMessageDialog o) => o -> m Bool
getMessageDialogSecondaryUseMarkup obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "secondary-use-markup"

-- | Set the value of the “@secondary-use-markup@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' messageDialog [ #secondaryUseMarkup 'Data.GI.Base.Attributes.:=' value ]
-- @
setMessageDialogSecondaryUseMarkup :: (MonadIO m, IsMessageDialog o) => o -> Bool -> m ()
setMessageDialogSecondaryUseMarkup obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "secondary-use-markup" val

-- | Construct a `GValueConstruct` with valid value for the “@secondary-use-markup@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructMessageDialogSecondaryUseMarkup :: (IsMessageDialog o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructMessageDialogSecondaryUseMarkup val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "secondary-use-markup" val

#if defined(ENABLE_OVERLOADING)
data MessageDialogSecondaryUseMarkupPropertyInfo
instance AttrInfo MessageDialogSecondaryUseMarkupPropertyInfo where
    type AttrAllowedOps MessageDialogSecondaryUseMarkupPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint MessageDialogSecondaryUseMarkupPropertyInfo = IsMessageDialog
    type AttrSetTypeConstraint MessageDialogSecondaryUseMarkupPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint MessageDialogSecondaryUseMarkupPropertyInfo = (~) Bool
    type AttrTransferType MessageDialogSecondaryUseMarkupPropertyInfo = Bool
    type AttrGetType MessageDialogSecondaryUseMarkupPropertyInfo = Bool
    type AttrLabel MessageDialogSecondaryUseMarkupPropertyInfo = "secondary-use-markup"
    type AttrOrigin MessageDialogSecondaryUseMarkupPropertyInfo = MessageDialog
    attrGet = getMessageDialogSecondaryUseMarkup
    attrSet = setMessageDialogSecondaryUseMarkup
    attrTransfer _ v = do
        return v
    attrConstruct = constructMessageDialogSecondaryUseMarkup
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MessageDialog.secondaryUseMarkup"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MessageDialog.html#g:attr:secondaryUseMarkup"
        })
#endif

-- VVV Prop "text"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@text@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' messageDialog #text
-- @
getMessageDialogText :: (MonadIO m, IsMessageDialog o) => o -> m (Maybe T.Text)
getMessageDialogText obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "text"

-- | Set the value of the “@text@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' messageDialog [ #text 'Data.GI.Base.Attributes.:=' value ]
-- @
setMessageDialogText :: (MonadIO m, IsMessageDialog o) => o -> T.Text -> m ()
setMessageDialogText obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "text" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@text@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructMessageDialogText :: (IsMessageDialog o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructMessageDialogText val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "text" (P.Just val)

-- | Set the value of the “@text@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #text
-- @
clearMessageDialogText :: (MonadIO m, IsMessageDialog o) => o -> m ()
clearMessageDialogText obj = liftIO $ B.Properties.setObjectPropertyString obj "text" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data MessageDialogTextPropertyInfo
instance AttrInfo MessageDialogTextPropertyInfo where
    type AttrAllowedOps MessageDialogTextPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint MessageDialogTextPropertyInfo = IsMessageDialog
    type AttrSetTypeConstraint MessageDialogTextPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint MessageDialogTextPropertyInfo = (~) T.Text
    type AttrTransferType MessageDialogTextPropertyInfo = T.Text
    type AttrGetType MessageDialogTextPropertyInfo = (Maybe T.Text)
    type AttrLabel MessageDialogTextPropertyInfo = "text"
    type AttrOrigin MessageDialogTextPropertyInfo = MessageDialog
    attrGet = getMessageDialogText
    attrSet = setMessageDialogText
    attrTransfer _ v = do
        return v
    attrConstruct = constructMessageDialogText
    attrClear = clearMessageDialogText
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MessageDialog.text"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MessageDialog.html#g:attr:text"
        })
#endif

-- VVV Prop "use-markup"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@use-markup@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' messageDialog #useMarkup
-- @
getMessageDialogUseMarkup :: (MonadIO m, IsMessageDialog o) => o -> m Bool
getMessageDialogUseMarkup obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "use-markup"

-- | Set the value of the “@use-markup@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' messageDialog [ #useMarkup 'Data.GI.Base.Attributes.:=' value ]
-- @
setMessageDialogUseMarkup :: (MonadIO m, IsMessageDialog o) => o -> Bool -> m ()
setMessageDialogUseMarkup obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "use-markup" val

-- | Construct a `GValueConstruct` with valid value for the “@use-markup@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructMessageDialogUseMarkup :: (IsMessageDialog o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructMessageDialogUseMarkup val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "use-markup" val

#if defined(ENABLE_OVERLOADING)
data MessageDialogUseMarkupPropertyInfo
instance AttrInfo MessageDialogUseMarkupPropertyInfo where
    type AttrAllowedOps MessageDialogUseMarkupPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint MessageDialogUseMarkupPropertyInfo = IsMessageDialog
    type AttrSetTypeConstraint MessageDialogUseMarkupPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint MessageDialogUseMarkupPropertyInfo = (~) Bool
    type AttrTransferType MessageDialogUseMarkupPropertyInfo = Bool
    type AttrGetType MessageDialogUseMarkupPropertyInfo = Bool
    type AttrLabel MessageDialogUseMarkupPropertyInfo = "use-markup"
    type AttrOrigin MessageDialogUseMarkupPropertyInfo = MessageDialog
    attrGet = getMessageDialogUseMarkup
    attrSet = setMessageDialogUseMarkup
    attrTransfer _ v = do
        return v
    attrConstruct = constructMessageDialogUseMarkup
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MessageDialog.useMarkup"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MessageDialog.html#g:attr:useMarkup"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList MessageDialog
type instance O.AttributeList MessageDialog = MessageDialogAttributeList
type MessageDialogAttributeList = ('[ '("acceptFocus", Gtk.Window.WindowAcceptFocusPropertyInfo), '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("application", Gtk.Window.WindowApplicationPropertyInfo), '("attachedTo", Gtk.Window.WindowAttachedToPropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("buttons", MessageDialogButtonsPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("decorated", Gtk.Window.WindowDecoratedPropertyInfo), '("defaultHeight", Gtk.Window.WindowDefaultHeightPropertyInfo), '("defaultWidth", Gtk.Window.WindowDefaultWidthPropertyInfo), '("deletable", Gtk.Window.WindowDeletablePropertyInfo), '("destroyWithParent", Gtk.Window.WindowDestroyWithParentPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("focusOnMap", Gtk.Window.WindowFocusOnMapPropertyInfo), '("focusVisible", Gtk.Window.WindowFocusVisiblePropertyInfo), '("gravity", Gtk.Window.WindowGravityPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasResizeGrip", Gtk.Window.WindowHasResizeGripPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("hasToplevelFocus", Gtk.Window.WindowHasToplevelFocusPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("hideTitlebarWhenMaximized", Gtk.Window.WindowHideTitlebarWhenMaximizedPropertyInfo), '("icon", Gtk.Window.WindowIconPropertyInfo), '("iconName", Gtk.Window.WindowIconNamePropertyInfo), '("image", MessageDialogImagePropertyInfo), '("isActive", Gtk.Window.WindowIsActivePropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("isMaximized", Gtk.Window.WindowIsMaximizedPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("messageArea", MessageDialogMessageAreaPropertyInfo), '("messageType", MessageDialogMessageTypePropertyInfo), '("mnemonicsVisible", Gtk.Window.WindowMnemonicsVisiblePropertyInfo), '("modal", Gtk.Window.WindowModalPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("resizable", Gtk.Window.WindowResizablePropertyInfo), '("resizeGripVisible", Gtk.Window.WindowResizeGripVisiblePropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("role", Gtk.Window.WindowRolePropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("screen", Gtk.Window.WindowScreenPropertyInfo), '("secondaryText", MessageDialogSecondaryTextPropertyInfo), '("secondaryUseMarkup", MessageDialogSecondaryUseMarkupPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("skipPagerHint", Gtk.Window.WindowSkipPagerHintPropertyInfo), '("skipTaskbarHint", Gtk.Window.WindowSkipTaskbarHintPropertyInfo), '("startupId", Gtk.Window.WindowStartupIdPropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("text", MessageDialogTextPropertyInfo), '("title", Gtk.Window.WindowTitlePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("transientFor", Gtk.Window.WindowTransientForPropertyInfo), '("type", Gtk.Window.WindowTypePropertyInfo), '("typeHint", Gtk.Window.WindowTypeHintPropertyInfo), '("urgencyHint", Gtk.Window.WindowUrgencyHintPropertyInfo), '("useHeaderBar", Gtk.Dialog.DialogUseHeaderBarPropertyInfo), '("useMarkup", MessageDialogUseMarkupPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo), '("windowPosition", Gtk.Window.WindowWindowPositionPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
messageDialogButtons :: AttrLabelProxy "buttons"
messageDialogButtons = AttrLabelProxy

messageDialogImage :: AttrLabelProxy "image"
messageDialogImage = AttrLabelProxy

messageDialogMessageArea :: AttrLabelProxy "messageArea"
messageDialogMessageArea = AttrLabelProxy

messageDialogMessageType :: AttrLabelProxy "messageType"
messageDialogMessageType = AttrLabelProxy

messageDialogSecondaryText :: AttrLabelProxy "secondaryText"
messageDialogSecondaryText = AttrLabelProxy

messageDialogSecondaryUseMarkup :: AttrLabelProxy "secondaryUseMarkup"
messageDialogSecondaryUseMarkup = AttrLabelProxy

messageDialogText :: AttrLabelProxy "text"
messageDialogText = AttrLabelProxy

messageDialogUseMarkup :: AttrLabelProxy "useMarkup"
messageDialogUseMarkup = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList MessageDialog = MessageDialogSignalList
type MessageDialogSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("activateDefault", Gtk.Window.WindowActivateDefaultSignalInfo), '("activateFocus", Gtk.Window.WindowActivateFocusSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("close", Gtk.Dialog.DialogCloseSignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enableDebugging", Gtk.Window.WindowEnableDebuggingSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("keysChanged", Gtk.Window.WindowKeysChangedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("response", Gtk.Dialog.DialogResponseSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocus", Gtk.Window.WindowSetFocusSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method MessageDialog::get_image
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "dialog"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MessageDialog" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMessageDialog"
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

foreign import ccall "gtk_message_dialog_get_image" gtk_message_dialog_get_image :: 
    Ptr MessageDialog ->                    -- dialog : TInterface (Name {namespace = "Gtk", name = "MessageDialog"})
    IO (Ptr Gtk.Widget.Widget)

{-# DEPRECATED messageDialogGetImage ["(Since version 3.12)","Use t'GI.Gtk.Objects.Dialog.Dialog' for dialogs with images"] #-}
-- | Gets the dialog’s image.
-- 
-- /Since: 2.14/
messageDialogGetImage ::
    (B.CallStack.HasCallStack, MonadIO m, IsMessageDialog a) =>
    a
    -- ^ /@dialog@/: a t'GI.Gtk.Objects.MessageDialog.MessageDialog'
    -> m Gtk.Widget.Widget
    -- ^ __Returns:__ the dialog’s image
messageDialogGetImage dialog = liftIO $ do
    dialog' <- unsafeManagedPtrCastPtr dialog
    result <- gtk_message_dialog_get_image dialog'
    checkUnexpectedReturnNULL "messageDialogGetImage" result
    result' <- (newObject Gtk.Widget.Widget) result
    touchManagedPtr dialog
    return result'

#if defined(ENABLE_OVERLOADING)
data MessageDialogGetImageMethodInfo
instance (signature ~ (m Gtk.Widget.Widget), MonadIO m, IsMessageDialog a) => O.OverloadedMethod MessageDialogGetImageMethodInfo a signature where
    overloadedMethod = messageDialogGetImage

instance O.OverloadedMethodInfo MessageDialogGetImageMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MessageDialog.messageDialogGetImage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MessageDialog.html#v:messageDialogGetImage"
        })


#endif

-- method MessageDialog::get_message_area
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "message_dialog"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MessageDialog" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMessageDialog"
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

foreign import ccall "gtk_message_dialog_get_message_area" gtk_message_dialog_get_message_area :: 
    Ptr MessageDialog ->                    -- message_dialog : TInterface (Name {namespace = "Gtk", name = "MessageDialog"})
    IO (Ptr Gtk.Widget.Widget)

-- | Returns the message area of the dialog. This is the box where the
-- dialog’s primary and secondary labels are packed. You can add your
-- own extra content to that box and it will appear below those labels.
-- See 'GI.Gtk.Objects.Dialog.dialogGetContentArea' for the corresponding
-- function in the parent t'GI.Gtk.Objects.Dialog.Dialog'.
-- 
-- /Since: 2.22/
messageDialogGetMessageArea ::
    (B.CallStack.HasCallStack, MonadIO m, IsMessageDialog a) =>
    a
    -- ^ /@messageDialog@/: a t'GI.Gtk.Objects.MessageDialog.MessageDialog'
    -> m Gtk.Widget.Widget
    -- ^ __Returns:__ A t'GI.Gtk.Objects.Box.Box' corresponding to the
    --     “message area” in the /@messageDialog@/.
messageDialogGetMessageArea messageDialog = liftIO $ do
    messageDialog' <- unsafeManagedPtrCastPtr messageDialog
    result <- gtk_message_dialog_get_message_area messageDialog'
    checkUnexpectedReturnNULL "messageDialogGetMessageArea" result
    result' <- (newObject Gtk.Widget.Widget) result
    touchManagedPtr messageDialog
    return result'

#if defined(ENABLE_OVERLOADING)
data MessageDialogGetMessageAreaMethodInfo
instance (signature ~ (m Gtk.Widget.Widget), MonadIO m, IsMessageDialog a) => O.OverloadedMethod MessageDialogGetMessageAreaMethodInfo a signature where
    overloadedMethod = messageDialogGetMessageArea

instance O.OverloadedMethodInfo MessageDialogGetMessageAreaMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MessageDialog.messageDialogGetMessageArea",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MessageDialog.html#v:messageDialogGetMessageArea"
        })


#endif

-- method MessageDialog::set_image
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "dialog"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MessageDialog" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMessageDialog"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "image"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the image" , sinceVersion = Nothing }
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

foreign import ccall "gtk_message_dialog_set_image" gtk_message_dialog_set_image :: 
    Ptr MessageDialog ->                    -- dialog : TInterface (Name {namespace = "Gtk", name = "MessageDialog"})
    Ptr Gtk.Widget.Widget ->                -- image : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

{-# DEPRECATED messageDialogSetImage ["(Since version 3.12)","Use t'GI.Gtk.Objects.Dialog.Dialog' to create dialogs with images"] #-}
-- | Sets the dialog’s image to /@image@/.
-- 
-- /Since: 2.10/
messageDialogSetImage ::
    (B.CallStack.HasCallStack, MonadIO m, IsMessageDialog a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@dialog@/: a t'GI.Gtk.Objects.MessageDialog.MessageDialog'
    -> b
    -- ^ /@image@/: the image
    -> m ()
messageDialogSetImage dialog image = liftIO $ do
    dialog' <- unsafeManagedPtrCastPtr dialog
    image' <- unsafeManagedPtrCastPtr image
    gtk_message_dialog_set_image dialog' image'
    touchManagedPtr dialog
    touchManagedPtr image
    return ()

#if defined(ENABLE_OVERLOADING)
data MessageDialogSetImageMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsMessageDialog a, Gtk.Widget.IsWidget b) => O.OverloadedMethod MessageDialogSetImageMethodInfo a signature where
    overloadedMethod = messageDialogSetImage

instance O.OverloadedMethodInfo MessageDialogSetImageMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MessageDialog.messageDialogSetImage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MessageDialog.html#v:messageDialogSetImage"
        })


#endif

-- method MessageDialog::set_markup
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "message_dialog"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "MessageDialog" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkMessageDialog"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "str"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "markup string (see [Pango markup format][PangoMarkupFormat])"
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

foreign import ccall "gtk_message_dialog_set_markup" gtk_message_dialog_set_markup :: 
    Ptr MessageDialog ->                    -- message_dialog : TInterface (Name {namespace = "Gtk", name = "MessageDialog"})
    CString ->                              -- str : TBasicType TUTF8
    IO ()

-- | Sets the text of the message dialog to be /@str@/, which is marked
-- up with the [Pango text markup language][PangoMarkupFormat].
-- 
-- /Since: 2.4/
messageDialogSetMarkup ::
    (B.CallStack.HasCallStack, MonadIO m, IsMessageDialog a) =>
    a
    -- ^ /@messageDialog@/: a t'GI.Gtk.Objects.MessageDialog.MessageDialog'
    -> T.Text
    -- ^ /@str@/: markup string (see [Pango markup format][PangoMarkupFormat])
    -> m ()
messageDialogSetMarkup messageDialog str = liftIO $ do
    messageDialog' <- unsafeManagedPtrCastPtr messageDialog
    str' <- textToCString str
    gtk_message_dialog_set_markup messageDialog' str'
    touchManagedPtr messageDialog
    freeMem str'
    return ()

#if defined(ENABLE_OVERLOADING)
data MessageDialogSetMarkupMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsMessageDialog a) => O.OverloadedMethod MessageDialogSetMarkupMethodInfo a signature where
    overloadedMethod = messageDialogSetMarkup

instance O.OverloadedMethodInfo MessageDialogSetMarkupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.MessageDialog.messageDialogSetMarkup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-MessageDialog.html#v:messageDialogSetMarkup"
        })


#endif


