{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A GtkWindow is a toplevel window which can contain other widgets.
-- Windows normally have decorations that are under the control
-- of the windowing system and allow the user to manipulate the window
-- (resize it, move it, close it,...).
-- 
-- = GtkWindow as GtkBuildable
-- 
-- The GtkWindow implementation of the t'GI.Gtk.Interfaces.Buildable.Buildable' interface supports a
-- custom @\<accel-groups>@ element, which supports any number of @\<group>@
-- elements representing the t'GI.Gtk.Objects.AccelGroup.AccelGroup' objects you want to add to
-- your window (synonymous with 'GI.Gtk.Objects.Window.windowAddAccelGroup'.
-- 
-- It also supports the @\<initial-focus>@ element, whose name property names
-- the widget to receive the focus when the window is mapped.
-- 
-- An example of a UI definition fragment with accel groups:
-- 
-- 
-- === /xml code/
-- >
-- ><object class="GtkWindow">
-- >  <accel-groups>
-- >    <group name="accelgroup1"/>
-- >  </accel-groups>
-- >  <initial-focus name="thunderclap"/>
-- ></object>
-- >
-- >...
-- >
-- ><object class="GtkAccelGroup" id="accelgroup1"/>
-- 
-- 
-- The GtkWindow implementation of the t'GI.Gtk.Interfaces.Buildable.Buildable' interface supports
-- setting a child as the titlebar by specifying “titlebar” as the “type”
-- attribute of a @\<child>@ element.
-- 
-- = CSS nodes
-- 
-- 
-- === /plain code/
-- >
-- >window.background
-- >├── decoration
-- >├── <titlebar child>.titlebar [.default-decoration]
-- >╰── <child>
-- 
-- 
-- GtkWindow has a main CSS node with name window and style class .background,
-- and a subnode with name decoration.
-- 
-- Style classes that are typically used with the main CSS node are .csd (when
-- client-side decorations are in use), .solid-csd (for client-side decorations
-- without invisible borders), .ssd (used by mutter when rendering server-side
-- decorations). GtkWindow also represents window states with the following
-- style classes on the main node: .tiled, .maximized, .fullscreen. Specialized
-- types of window often add their own discriminating style classes, such as
-- .popup or .tooltip.
-- 
-- GtkWindow adds the .titlebar and .default-decoration style classes to the
-- widget that is added as a titlebar child.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.Window
    ( 

-- * Exported types
    Window(..)                              ,
    IsWindow                                ,
    toWindow                                ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [activateDefault]("GI.Gtk.Objects.Window#g:method:activateDefault"), [activateFocus]("GI.Gtk.Objects.Window#g:method:activateFocus"), [activateKey]("GI.Gtk.Objects.Window#g:method:activateKey"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelGroup]("GI.Gtk.Objects.Window#g:method:addAccelGroup"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonic]("GI.Gtk.Objects.Window#g:method:addMnemonic"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [beginMoveDrag]("GI.Gtk.Objects.Window#g:method:beginMoveDrag"), [beginResizeDrag]("GI.Gtk.Objects.Window#g:method:beginResizeDrag"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [close]("GI.Gtk.Objects.Window#g:method:close"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [deiconify]("GI.Gtk.Objects.Window#g:method:deiconify"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [fullscreen]("GI.Gtk.Objects.Window#g:method:fullscreen"), [fullscreenOnMonitor]("GI.Gtk.Objects.Window#g:method:fullscreenOnMonitor"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasGroup]("GI.Gtk.Objects.Window#g:method:hasGroup"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasToplevelFocus]("GI.Gtk.Objects.Window#g:method:hasToplevelFocus"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [iconify]("GI.Gtk.Objects.Window#g:method:iconify"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isActive]("GI.Gtk.Objects.Window#g:method:isActive"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isMaximized]("GI.Gtk.Objects.Window#g:method:isMaximized"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [maximize]("GI.Gtk.Objects.Window#g:method:maximize"), [mnemonicActivate]("GI.Gtk.Objects.Window#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [move]("GI.Gtk.Objects.Window#g:method:move"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parseGeometry]("GI.Gtk.Objects.Window#g:method:parseGeometry"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [present]("GI.Gtk.Objects.Window#g:method:present"), [presentWithTime]("GI.Gtk.Objects.Window#g:method:presentWithTime"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [propagateKeyEvent]("GI.Gtk.Objects.Window#g:method:propagateKeyEvent"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelGroup]("GI.Gtk.Objects.Window#g:method:removeAccelGroup"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonic]("GI.Gtk.Objects.Window#g:method:removeMnemonic"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [reshowWithInitialSize]("GI.Gtk.Objects.Window#g:method:reshowWithInitialSize"), [resize]("GI.Gtk.Objects.Window#g:method:resize"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [resizeGripIsVisible]("GI.Gtk.Objects.Window#g:method:resizeGripIsVisible"), [resizeToGeometry]("GI.Gtk.Objects.Window#g:method:resizeToGeometry"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [stick]("GI.Gtk.Objects.Window#g:method:stick"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unfullscreen]("GI.Gtk.Objects.Window#g:method:unfullscreen"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unmaximize]("GI.Gtk.Objects.Window#g:method:unmaximize"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [unstick]("GI.Gtk.Objects.Window#g:method:unstick"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAcceptFocus]("GI.Gtk.Objects.Window#g:method:getAcceptFocus"), [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getApplication]("GI.Gtk.Objects.Window#g:method:getApplication"), [getAttachedTo]("GI.Gtk.Objects.Window#g:method:getAttachedTo"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChild]("GI.Gtk.Objects.Bin#g:method:getChild"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDecorated]("GI.Gtk.Objects.Window#g:method:getDecorated"), [getDefaultSize]("GI.Gtk.Objects.Window#g:method:getDefaultSize"), [getDefaultWidget]("GI.Gtk.Objects.Window#g:method:getDefaultWidget"), [getDeletable]("GI.Gtk.Objects.Window#g:method:getDeletable"), [getDestroyWithParent]("GI.Gtk.Objects.Window#g:method:getDestroyWithParent"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocus]("GI.Gtk.Objects.Window#g:method:getFocus"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFocusOnMap]("GI.Gtk.Objects.Window#g:method:getFocusOnMap"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFocusVisible]("GI.Gtk.Objects.Window#g:method:getFocusVisible"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getGravity]("GI.Gtk.Objects.Window#g:method:getGravity"), [getGroup]("GI.Gtk.Objects.Window#g:method:getGroup"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasResizeGrip]("GI.Gtk.Objects.Window#g:method:getHasResizeGrip"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getHideTitlebarWhenMaximized]("GI.Gtk.Objects.Window#g:method:getHideTitlebarWhenMaximized"), [getIcon]("GI.Gtk.Objects.Window#g:method:getIcon"), [getIconList]("GI.Gtk.Objects.Window#g:method:getIconList"), [getIconName]("GI.Gtk.Objects.Window#g:method:getIconName"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getMnemonicModifier]("GI.Gtk.Objects.Window#g:method:getMnemonicModifier"), [getMnemonicsVisible]("GI.Gtk.Objects.Window#g:method:getMnemonicsVisible"), [getModal]("GI.Gtk.Objects.Window#g:method:getModal"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Window#g:method:getOpacity"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPosition]("GI.Gtk.Objects.Window#g:method:getPosition"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizable]("GI.Gtk.Objects.Window#g:method:getResizable"), [getResizeGripArea]("GI.Gtk.Objects.Window#g:method:getResizeGripArea"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRole]("GI.Gtk.Objects.Window#g:method:getRole"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Window#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSize]("GI.Gtk.Objects.Window#g:method:getSize"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getSkipPagerHint]("GI.Gtk.Objects.Window#g:method:getSkipPagerHint"), [getSkipTaskbarHint]("GI.Gtk.Objects.Window#g:method:getSkipTaskbarHint"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTitle]("GI.Gtk.Objects.Window#g:method:getTitle"), [getTitlebar]("GI.Gtk.Objects.Window#g:method:getTitlebar"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getTransientFor]("GI.Gtk.Objects.Window#g:method:getTransientFor"), [getTypeHint]("GI.Gtk.Objects.Window#g:method:getTypeHint"), [getUrgencyHint]("GI.Gtk.Objects.Window#g:method:getUrgencyHint"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow"), [getWindowType]("GI.Gtk.Objects.Window#g:method:getWindowType").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setAcceptFocus]("GI.Gtk.Objects.Window#g:method:setAcceptFocus"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setApplication]("GI.Gtk.Objects.Window#g:method:setApplication"), [setAttachedTo]("GI.Gtk.Objects.Window#g:method:setAttachedTo"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDecorated]("GI.Gtk.Objects.Window#g:method:setDecorated"), [setDefault]("GI.Gtk.Objects.Window#g:method:setDefault"), [setDefaultGeometry]("GI.Gtk.Objects.Window#g:method:setDefaultGeometry"), [setDefaultSize]("GI.Gtk.Objects.Window#g:method:setDefaultSize"), [setDeletable]("GI.Gtk.Objects.Window#g:method:setDeletable"), [setDestroyWithParent]("GI.Gtk.Objects.Window#g:method:setDestroyWithParent"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocus]("GI.Gtk.Objects.Window#g:method:setFocus"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFocusOnMap]("GI.Gtk.Objects.Window#g:method:setFocusOnMap"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFocusVisible]("GI.Gtk.Objects.Window#g:method:setFocusVisible"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setGeometryHints]("GI.Gtk.Objects.Window#g:method:setGeometryHints"), [setGravity]("GI.Gtk.Objects.Window#g:method:setGravity"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasResizeGrip]("GI.Gtk.Objects.Window#g:method:setHasResizeGrip"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasUserRefCount]("GI.Gtk.Objects.Window#g:method:setHasUserRefCount"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setHideTitlebarWhenMaximized]("GI.Gtk.Objects.Window#g:method:setHideTitlebarWhenMaximized"), [setIcon]("GI.Gtk.Objects.Window#g:method:setIcon"), [setIconFromFile]("GI.Gtk.Objects.Window#g:method:setIconFromFile"), [setIconList]("GI.Gtk.Objects.Window#g:method:setIconList"), [setIconName]("GI.Gtk.Objects.Window#g:method:setIconName"), [setKeepAbove]("GI.Gtk.Objects.Window#g:method:setKeepAbove"), [setKeepBelow]("GI.Gtk.Objects.Window#g:method:setKeepBelow"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setMnemonicModifier]("GI.Gtk.Objects.Window#g:method:setMnemonicModifier"), [setMnemonicsVisible]("GI.Gtk.Objects.Window#g:method:setMnemonicsVisible"), [setModal]("GI.Gtk.Objects.Window#g:method:setModal"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Window#g:method:setOpacity"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setPosition]("GI.Gtk.Objects.Window#g:method:setPosition"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setResizable]("GI.Gtk.Objects.Window#g:method:setResizable"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setRole]("GI.Gtk.Objects.Window#g:method:setRole"), [setScreen]("GI.Gtk.Objects.Window#g:method:setScreen"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setSkipPagerHint]("GI.Gtk.Objects.Window#g:method:setSkipPagerHint"), [setSkipTaskbarHint]("GI.Gtk.Objects.Window#g:method:setSkipTaskbarHint"), [setStartupId]("GI.Gtk.Objects.Window#g:method:setStartupId"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTitle]("GI.Gtk.Objects.Window#g:method:setTitle"), [setTitlebar]("GI.Gtk.Objects.Window#g:method:setTitlebar"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setTransientFor]("GI.Gtk.Objects.Window#g:method:setTransientFor"), [setTypeHint]("GI.Gtk.Objects.Window#g:method:setTypeHint"), [setUrgencyHint]("GI.Gtk.Objects.Window#g:method:setUrgencyHint"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow"), [setWmclass]("GI.Gtk.Objects.Window#g:method:setWmclass").

#if defined(ENABLE_OVERLOADING)
    ResolveWindowMethod                     ,
#endif

-- ** activateDefault #method:activateDefault#

#if defined(ENABLE_OVERLOADING)
    WindowActivateDefaultMethodInfo         ,
#endif
    windowActivateDefault                   ,


-- ** activateFocus #method:activateFocus#

#if defined(ENABLE_OVERLOADING)
    WindowActivateFocusMethodInfo           ,
#endif
    windowActivateFocus                     ,


-- ** activateKey #method:activateKey#

#if defined(ENABLE_OVERLOADING)
    WindowActivateKeyMethodInfo             ,
#endif
    windowActivateKey                       ,


-- ** addAccelGroup #method:addAccelGroup#

#if defined(ENABLE_OVERLOADING)
    WindowAddAccelGroupMethodInfo           ,
#endif
    windowAddAccelGroup                     ,


-- ** addMnemonic #method:addMnemonic#

#if defined(ENABLE_OVERLOADING)
    WindowAddMnemonicMethodInfo             ,
#endif
    windowAddMnemonic                       ,


-- ** beginMoveDrag #method:beginMoveDrag#

#if defined(ENABLE_OVERLOADING)
    WindowBeginMoveDragMethodInfo           ,
#endif
    windowBeginMoveDrag                     ,


-- ** beginResizeDrag #method:beginResizeDrag#

#if defined(ENABLE_OVERLOADING)
    WindowBeginResizeDragMethodInfo         ,
#endif
    windowBeginResizeDrag                   ,


-- ** close #method:close#

#if defined(ENABLE_OVERLOADING)
    WindowCloseMethodInfo                   ,
#endif
    windowClose                             ,


-- ** deiconify #method:deiconify#

#if defined(ENABLE_OVERLOADING)
    WindowDeiconifyMethodInfo               ,
#endif
    windowDeiconify                         ,


-- ** fullscreen #method:fullscreen#

#if defined(ENABLE_OVERLOADING)
    WindowFullscreenMethodInfo              ,
#endif
    windowFullscreen                        ,


-- ** fullscreenOnMonitor #method:fullscreenOnMonitor#

#if defined(ENABLE_OVERLOADING)
    WindowFullscreenOnMonitorMethodInfo     ,
#endif
    windowFullscreenOnMonitor               ,


-- ** getAcceptFocus #method:getAcceptFocus#

#if defined(ENABLE_OVERLOADING)
    WindowGetAcceptFocusMethodInfo          ,
#endif
    windowGetAcceptFocus                    ,


-- ** getApplication #method:getApplication#

#if defined(ENABLE_OVERLOADING)
    WindowGetApplicationMethodInfo          ,
#endif
    windowGetApplication                    ,


-- ** getAttachedTo #method:getAttachedTo#

#if defined(ENABLE_OVERLOADING)
    WindowGetAttachedToMethodInfo           ,
#endif
    windowGetAttachedTo                     ,


-- ** getDecorated #method:getDecorated#

#if defined(ENABLE_OVERLOADING)
    WindowGetDecoratedMethodInfo            ,
#endif
    windowGetDecorated                      ,


-- ** getDefaultIconList #method:getDefaultIconList#

    windowGetDefaultIconList                ,


-- ** getDefaultIconName #method:getDefaultIconName#

    windowGetDefaultIconName                ,


-- ** getDefaultSize #method:getDefaultSize#

#if defined(ENABLE_OVERLOADING)
    WindowGetDefaultSizeMethodInfo          ,
#endif
    windowGetDefaultSize                    ,


-- ** getDefaultWidget #method:getDefaultWidget#

#if defined(ENABLE_OVERLOADING)
    WindowGetDefaultWidgetMethodInfo        ,
#endif
    windowGetDefaultWidget                  ,


-- ** getDeletable #method:getDeletable#

#if defined(ENABLE_OVERLOADING)
    WindowGetDeletableMethodInfo            ,
#endif
    windowGetDeletable                      ,


-- ** getDestroyWithParent #method:getDestroyWithParent#

#if defined(ENABLE_OVERLOADING)
    WindowGetDestroyWithParentMethodInfo    ,
#endif
    windowGetDestroyWithParent              ,


-- ** getFocus #method:getFocus#

#if defined(ENABLE_OVERLOADING)
    WindowGetFocusMethodInfo                ,
#endif
    windowGetFocus                          ,


-- ** getFocusOnMap #method:getFocusOnMap#

#if defined(ENABLE_OVERLOADING)
    WindowGetFocusOnMapMethodInfo           ,
#endif
    windowGetFocusOnMap                     ,


-- ** getFocusVisible #method:getFocusVisible#

#if defined(ENABLE_OVERLOADING)
    WindowGetFocusVisibleMethodInfo         ,
#endif
    windowGetFocusVisible                   ,


-- ** getGravity #method:getGravity#

#if defined(ENABLE_OVERLOADING)
    WindowGetGravityMethodInfo              ,
#endif
    windowGetGravity                        ,


-- ** getGroup #method:getGroup#

#if defined(ENABLE_OVERLOADING)
    WindowGetGroupMethodInfo                ,
#endif
    windowGetGroup                          ,


-- ** getHasResizeGrip #method:getHasResizeGrip#

#if defined(ENABLE_OVERLOADING)
    WindowGetHasResizeGripMethodInfo        ,
#endif
    windowGetHasResizeGrip                  ,


-- ** getHideTitlebarWhenMaximized #method:getHideTitlebarWhenMaximized#

#if defined(ENABLE_OVERLOADING)
    WindowGetHideTitlebarWhenMaximizedMethodInfo,
#endif
    windowGetHideTitlebarWhenMaximized      ,


-- ** getIcon #method:getIcon#

#if defined(ENABLE_OVERLOADING)
    WindowGetIconMethodInfo                 ,
#endif
    windowGetIcon                           ,


-- ** getIconList #method:getIconList#

#if defined(ENABLE_OVERLOADING)
    WindowGetIconListMethodInfo             ,
#endif
    windowGetIconList                       ,


-- ** getIconName #method:getIconName#

#if defined(ENABLE_OVERLOADING)
    WindowGetIconNameMethodInfo             ,
#endif
    windowGetIconName                       ,


-- ** getMnemonicModifier #method:getMnemonicModifier#

#if defined(ENABLE_OVERLOADING)
    WindowGetMnemonicModifierMethodInfo     ,
#endif
    windowGetMnemonicModifier               ,


-- ** getMnemonicsVisible #method:getMnemonicsVisible#

#if defined(ENABLE_OVERLOADING)
    WindowGetMnemonicsVisibleMethodInfo     ,
#endif
    windowGetMnemonicsVisible               ,


-- ** getModal #method:getModal#

#if defined(ENABLE_OVERLOADING)
    WindowGetModalMethodInfo                ,
#endif
    windowGetModal                          ,


-- ** getOpacity #method:getOpacity#

#if defined(ENABLE_OVERLOADING)
    WindowGetOpacityMethodInfo              ,
#endif
    windowGetOpacity                        ,


-- ** getPosition #method:getPosition#

#if defined(ENABLE_OVERLOADING)
    WindowGetPositionMethodInfo             ,
#endif
    windowGetPosition                       ,


-- ** getResizable #method:getResizable#

#if defined(ENABLE_OVERLOADING)
    WindowGetResizableMethodInfo            ,
#endif
    windowGetResizable                      ,


-- ** getResizeGripArea #method:getResizeGripArea#

#if defined(ENABLE_OVERLOADING)
    WindowGetResizeGripAreaMethodInfo       ,
#endif
    windowGetResizeGripArea                 ,


-- ** getRole #method:getRole#

#if defined(ENABLE_OVERLOADING)
    WindowGetRoleMethodInfo                 ,
#endif
    windowGetRole                           ,


-- ** getScreen #method:getScreen#

#if defined(ENABLE_OVERLOADING)
    WindowGetScreenMethodInfo               ,
#endif
    windowGetScreen                         ,


-- ** getSize #method:getSize#

#if defined(ENABLE_OVERLOADING)
    WindowGetSizeMethodInfo                 ,
#endif
    windowGetSize                           ,


-- ** getSkipPagerHint #method:getSkipPagerHint#

#if defined(ENABLE_OVERLOADING)
    WindowGetSkipPagerHintMethodInfo        ,
#endif
    windowGetSkipPagerHint                  ,


-- ** getSkipTaskbarHint #method:getSkipTaskbarHint#

#if defined(ENABLE_OVERLOADING)
    WindowGetSkipTaskbarHintMethodInfo      ,
#endif
    windowGetSkipTaskbarHint                ,


-- ** getTitle #method:getTitle#

#if defined(ENABLE_OVERLOADING)
    WindowGetTitleMethodInfo                ,
#endif
    windowGetTitle                          ,


-- ** getTitlebar #method:getTitlebar#

#if defined(ENABLE_OVERLOADING)
    WindowGetTitlebarMethodInfo             ,
#endif
    windowGetTitlebar                       ,


-- ** getTransientFor #method:getTransientFor#

#if defined(ENABLE_OVERLOADING)
    WindowGetTransientForMethodInfo         ,
#endif
    windowGetTransientFor                   ,


-- ** getTypeHint #method:getTypeHint#

#if defined(ENABLE_OVERLOADING)
    WindowGetTypeHintMethodInfo             ,
#endif
    windowGetTypeHint                       ,


-- ** getUrgencyHint #method:getUrgencyHint#

#if defined(ENABLE_OVERLOADING)
    WindowGetUrgencyHintMethodInfo          ,
#endif
    windowGetUrgencyHint                    ,


-- ** getWindowType #method:getWindowType#

#if defined(ENABLE_OVERLOADING)
    WindowGetWindowTypeMethodInfo           ,
#endif
    windowGetWindowType                     ,


-- ** hasGroup #method:hasGroup#

#if defined(ENABLE_OVERLOADING)
    WindowHasGroupMethodInfo                ,
#endif
    windowHasGroup                          ,


-- ** hasToplevelFocus #method:hasToplevelFocus#

#if defined(ENABLE_OVERLOADING)
    WindowHasToplevelFocusMethodInfo        ,
#endif
    windowHasToplevelFocus                  ,


-- ** iconify #method:iconify#

#if defined(ENABLE_OVERLOADING)
    WindowIconifyMethodInfo                 ,
#endif
    windowIconify                           ,


-- ** isActive #method:isActive#

#if defined(ENABLE_OVERLOADING)
    WindowIsActiveMethodInfo                ,
#endif
    windowIsActive                          ,


-- ** isMaximized #method:isMaximized#

#if defined(ENABLE_OVERLOADING)
    WindowIsMaximizedMethodInfo             ,
#endif
    windowIsMaximized                       ,


-- ** listToplevels #method:listToplevels#

    windowListToplevels                     ,


-- ** maximize #method:maximize#

#if defined(ENABLE_OVERLOADING)
    WindowMaximizeMethodInfo                ,
#endif
    windowMaximize                          ,


-- ** mnemonicActivate #method:mnemonicActivate#

#if defined(ENABLE_OVERLOADING)
    WindowMnemonicActivateMethodInfo        ,
#endif
    windowMnemonicActivate                  ,


-- ** move #method:move#

#if defined(ENABLE_OVERLOADING)
    WindowMoveMethodInfo                    ,
#endif
    windowMove                              ,


-- ** new #method:new#

    windowNew                               ,


-- ** parseGeometry #method:parseGeometry#

#if defined(ENABLE_OVERLOADING)
    WindowParseGeometryMethodInfo           ,
#endif
    windowParseGeometry                     ,


-- ** present #method:present#

#if defined(ENABLE_OVERLOADING)
    WindowPresentMethodInfo                 ,
#endif
    windowPresent                           ,


-- ** presentWithTime #method:presentWithTime#

#if defined(ENABLE_OVERLOADING)
    WindowPresentWithTimeMethodInfo         ,
#endif
    windowPresentWithTime                   ,


-- ** propagateKeyEvent #method:propagateKeyEvent#

#if defined(ENABLE_OVERLOADING)
    WindowPropagateKeyEventMethodInfo       ,
#endif
    windowPropagateKeyEvent                 ,


-- ** removeAccelGroup #method:removeAccelGroup#

#if defined(ENABLE_OVERLOADING)
    WindowRemoveAccelGroupMethodInfo        ,
#endif
    windowRemoveAccelGroup                  ,


-- ** removeMnemonic #method:removeMnemonic#

#if defined(ENABLE_OVERLOADING)
    WindowRemoveMnemonicMethodInfo          ,
#endif
    windowRemoveMnemonic                    ,


-- ** reshowWithInitialSize #method:reshowWithInitialSize#

#if defined(ENABLE_OVERLOADING)
    WindowReshowWithInitialSizeMethodInfo   ,
#endif
    windowReshowWithInitialSize             ,


-- ** resize #method:resize#

#if defined(ENABLE_OVERLOADING)
    WindowResizeMethodInfo                  ,
#endif
    windowResize                            ,


-- ** resizeGripIsVisible #method:resizeGripIsVisible#

#if defined(ENABLE_OVERLOADING)
    WindowResizeGripIsVisibleMethodInfo     ,
#endif
    windowResizeGripIsVisible               ,


-- ** resizeToGeometry #method:resizeToGeometry#

#if defined(ENABLE_OVERLOADING)
    WindowResizeToGeometryMethodInfo        ,
#endif
    windowResizeToGeometry                  ,


-- ** setAcceptFocus #method:setAcceptFocus#

#if defined(ENABLE_OVERLOADING)
    WindowSetAcceptFocusMethodInfo          ,
#endif
    windowSetAcceptFocus                    ,


-- ** setApplication #method:setApplication#

#if defined(ENABLE_OVERLOADING)
    WindowSetApplicationMethodInfo          ,
#endif
    windowSetApplication                    ,


-- ** setAttachedTo #method:setAttachedTo#

#if defined(ENABLE_OVERLOADING)
    WindowSetAttachedToMethodInfo           ,
#endif
    windowSetAttachedTo                     ,


-- ** setAutoStartupNotification #method:setAutoStartupNotification#

    windowSetAutoStartupNotification        ,


-- ** setDecorated #method:setDecorated#

#if defined(ENABLE_OVERLOADING)
    WindowSetDecoratedMethodInfo            ,
#endif
    windowSetDecorated                      ,


-- ** setDefault #method:setDefault#

#if defined(ENABLE_OVERLOADING)
    WindowSetDefaultMethodInfo              ,
#endif
    windowSetDefault                        ,


-- ** setDefaultGeometry #method:setDefaultGeometry#

#if defined(ENABLE_OVERLOADING)
    WindowSetDefaultGeometryMethodInfo      ,
#endif
    windowSetDefaultGeometry                ,


-- ** setDefaultIcon #method:setDefaultIcon#

    windowSetDefaultIcon                    ,


-- ** setDefaultIconFromFile #method:setDefaultIconFromFile#

    windowSetDefaultIconFromFile            ,


-- ** setDefaultIconList #method:setDefaultIconList#

    windowSetDefaultIconList                ,


-- ** setDefaultIconName #method:setDefaultIconName#

    windowSetDefaultIconName                ,


-- ** setDefaultSize #method:setDefaultSize#

#if defined(ENABLE_OVERLOADING)
    WindowSetDefaultSizeMethodInfo          ,
#endif
    windowSetDefaultSize                    ,


-- ** setDeletable #method:setDeletable#

#if defined(ENABLE_OVERLOADING)
    WindowSetDeletableMethodInfo            ,
#endif
    windowSetDeletable                      ,


-- ** setDestroyWithParent #method:setDestroyWithParent#

#if defined(ENABLE_OVERLOADING)
    WindowSetDestroyWithParentMethodInfo    ,
#endif
    windowSetDestroyWithParent              ,


-- ** setFocus #method:setFocus#

#if defined(ENABLE_OVERLOADING)
    WindowSetFocusMethodInfo                ,
#endif
    windowSetFocus                          ,


-- ** setFocusOnMap #method:setFocusOnMap#

#if defined(ENABLE_OVERLOADING)
    WindowSetFocusOnMapMethodInfo           ,
#endif
    windowSetFocusOnMap                     ,


-- ** setFocusVisible #method:setFocusVisible#

#if defined(ENABLE_OVERLOADING)
    WindowSetFocusVisibleMethodInfo         ,
#endif
    windowSetFocusVisible                   ,


-- ** setGeometryHints #method:setGeometryHints#

#if defined(ENABLE_OVERLOADING)
    WindowSetGeometryHintsMethodInfo        ,
#endif
    windowSetGeometryHints                  ,


-- ** setGravity #method:setGravity#

#if defined(ENABLE_OVERLOADING)
    WindowSetGravityMethodInfo              ,
#endif
    windowSetGravity                        ,


-- ** setHasResizeGrip #method:setHasResizeGrip#

#if defined(ENABLE_OVERLOADING)
    WindowSetHasResizeGripMethodInfo        ,
#endif
    windowSetHasResizeGrip                  ,


-- ** setHasUserRefCount #method:setHasUserRefCount#

#if defined(ENABLE_OVERLOADING)
    WindowSetHasUserRefCountMethodInfo      ,
#endif
    windowSetHasUserRefCount                ,


-- ** setHideTitlebarWhenMaximized #method:setHideTitlebarWhenMaximized#

#if defined(ENABLE_OVERLOADING)
    WindowSetHideTitlebarWhenMaximizedMethodInfo,
#endif
    windowSetHideTitlebarWhenMaximized      ,


-- ** setIcon #method:setIcon#

#if defined(ENABLE_OVERLOADING)
    WindowSetIconMethodInfo                 ,
#endif
    windowSetIcon                           ,


-- ** setIconFromFile #method:setIconFromFile#

#if defined(ENABLE_OVERLOADING)
    WindowSetIconFromFileMethodInfo         ,
#endif
    windowSetIconFromFile                   ,


-- ** setIconList #method:setIconList#

#if defined(ENABLE_OVERLOADING)
    WindowSetIconListMethodInfo             ,
#endif
    windowSetIconList                       ,


-- ** setIconName #method:setIconName#

#if defined(ENABLE_OVERLOADING)
    WindowSetIconNameMethodInfo             ,
#endif
    windowSetIconName                       ,


-- ** setInteractiveDebugging #method:setInteractiveDebugging#

    windowSetInteractiveDebugging           ,


-- ** setKeepAbove #method:setKeepAbove#

#if defined(ENABLE_OVERLOADING)
    WindowSetKeepAboveMethodInfo            ,
#endif
    windowSetKeepAbove                      ,


-- ** setKeepBelow #method:setKeepBelow#

#if defined(ENABLE_OVERLOADING)
    WindowSetKeepBelowMethodInfo            ,
#endif
    windowSetKeepBelow                      ,


-- ** setMnemonicModifier #method:setMnemonicModifier#

#if defined(ENABLE_OVERLOADING)
    WindowSetMnemonicModifierMethodInfo     ,
#endif
    windowSetMnemonicModifier               ,


-- ** setMnemonicsVisible #method:setMnemonicsVisible#

#if defined(ENABLE_OVERLOADING)
    WindowSetMnemonicsVisibleMethodInfo     ,
#endif
    windowSetMnemonicsVisible               ,


-- ** setModal #method:setModal#

#if defined(ENABLE_OVERLOADING)
    WindowSetModalMethodInfo                ,
#endif
    windowSetModal                          ,


-- ** setOpacity #method:setOpacity#

#if defined(ENABLE_OVERLOADING)
    WindowSetOpacityMethodInfo              ,
#endif
    windowSetOpacity                        ,


-- ** setPosition #method:setPosition#

#if defined(ENABLE_OVERLOADING)
    WindowSetPositionMethodInfo             ,
#endif
    windowSetPosition                       ,


-- ** setResizable #method:setResizable#

#if defined(ENABLE_OVERLOADING)
    WindowSetResizableMethodInfo            ,
#endif
    windowSetResizable                      ,


-- ** setRole #method:setRole#

#if defined(ENABLE_OVERLOADING)
    WindowSetRoleMethodInfo                 ,
#endif
    windowSetRole                           ,


-- ** setScreen #method:setScreen#

#if defined(ENABLE_OVERLOADING)
    WindowSetScreenMethodInfo               ,
#endif
    windowSetScreen                         ,


-- ** setSkipPagerHint #method:setSkipPagerHint#

#if defined(ENABLE_OVERLOADING)
    WindowSetSkipPagerHintMethodInfo        ,
#endif
    windowSetSkipPagerHint                  ,


-- ** setSkipTaskbarHint #method:setSkipTaskbarHint#

#if defined(ENABLE_OVERLOADING)
    WindowSetSkipTaskbarHintMethodInfo      ,
#endif
    windowSetSkipTaskbarHint                ,


-- ** setStartupId #method:setStartupId#

#if defined(ENABLE_OVERLOADING)
    WindowSetStartupIdMethodInfo            ,
#endif
    windowSetStartupId                      ,


-- ** setTitle #method:setTitle#

#if defined(ENABLE_OVERLOADING)
    WindowSetTitleMethodInfo                ,
#endif
    windowSetTitle                          ,


-- ** setTitlebar #method:setTitlebar#

#if defined(ENABLE_OVERLOADING)
    WindowSetTitlebarMethodInfo             ,
#endif
    windowSetTitlebar                       ,


-- ** setTransientFor #method:setTransientFor#

#if defined(ENABLE_OVERLOADING)
    WindowSetTransientForMethodInfo         ,
#endif
    windowSetTransientFor                   ,


-- ** setTypeHint #method:setTypeHint#

#if defined(ENABLE_OVERLOADING)
    WindowSetTypeHintMethodInfo             ,
#endif
    windowSetTypeHint                       ,


-- ** setUrgencyHint #method:setUrgencyHint#

#if defined(ENABLE_OVERLOADING)
    WindowSetUrgencyHintMethodInfo          ,
#endif
    windowSetUrgencyHint                    ,


-- ** setWmclass #method:setWmclass#

#if defined(ENABLE_OVERLOADING)
    WindowSetWmclassMethodInfo              ,
#endif
    windowSetWmclass                        ,


-- ** stick #method:stick#

#if defined(ENABLE_OVERLOADING)
    WindowStickMethodInfo                   ,
#endif
    windowStick                             ,


-- ** unfullscreen #method:unfullscreen#

#if defined(ENABLE_OVERLOADING)
    WindowUnfullscreenMethodInfo            ,
#endif
    windowUnfullscreen                      ,


-- ** unmaximize #method:unmaximize#

#if defined(ENABLE_OVERLOADING)
    WindowUnmaximizeMethodInfo              ,
#endif
    windowUnmaximize                        ,


-- ** unstick #method:unstick#

#if defined(ENABLE_OVERLOADING)
    WindowUnstickMethodInfo                 ,
#endif
    windowUnstick                           ,




 -- * Properties


-- ** acceptFocus #attr:acceptFocus#
-- | Whether the window should receive the input focus.
-- 
-- /Since: 2.4/

#if defined(ENABLE_OVERLOADING)
    WindowAcceptFocusPropertyInfo           ,
#endif
    constructWindowAcceptFocus              ,
    getWindowAcceptFocus                    ,
    setWindowAcceptFocus                    ,
#if defined(ENABLE_OVERLOADING)
    windowAcceptFocus                       ,
#endif


-- ** application #attr:application#
-- | The t'GI.Gtk.Objects.Application.Application' associated with the window.
-- 
-- The application will be kept alive for at least as long as it
-- has any windows associated with it (see 'GI.Gio.Objects.Application.applicationHold'
-- for a way to keep it alive without windows).
-- 
-- Normally, the connection between the application and the window
-- will remain until the window is destroyed, but you can explicitly
-- remove it by setting the :application property to 'P.Nothing'.
-- 
-- /Since: 3.0/

#if defined(ENABLE_OVERLOADING)
    WindowApplicationPropertyInfo           ,
#endif
    clearWindowApplication                  ,
    constructWindowApplication              ,
    getWindowApplication                    ,
    setWindowApplication                    ,
#if defined(ENABLE_OVERLOADING)
    windowApplication                       ,
#endif


-- ** attachedTo #attr:attachedTo#
-- | The widget to which this window is attached.
-- See 'GI.Gtk.Objects.Window.windowSetAttachedTo'.
-- 
-- Examples of places where specifying this relation is useful are
-- for instance a t'GI.Gtk.Objects.Menu.Menu' created by a t'GI.Gtk.Objects.ComboBox.ComboBox', a completion
-- popup window created by t'GI.Gtk.Objects.Entry.Entry' or a typeahead search entry
-- created by t'GI.Gtk.Objects.TreeView.TreeView'.
-- 
-- /Since: 3.4/

#if defined(ENABLE_OVERLOADING)
    WindowAttachedToPropertyInfo            ,
#endif
    clearWindowAttachedTo                   ,
    constructWindowAttachedTo               ,
    getWindowAttachedTo                     ,
    setWindowAttachedTo                     ,
#if defined(ENABLE_OVERLOADING)
    windowAttachedTo                        ,
#endif


-- ** decorated #attr:decorated#
-- | Whether the window should be decorated by the window manager.
-- 
-- /Since: 2.4/

#if defined(ENABLE_OVERLOADING)
    WindowDecoratedPropertyInfo             ,
#endif
    constructWindowDecorated                ,
    getWindowDecorated                      ,
    setWindowDecorated                      ,
#if defined(ENABLE_OVERLOADING)
    windowDecorated                         ,
#endif


-- ** defaultHeight #attr:defaultHeight#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    WindowDefaultHeightPropertyInfo         ,
#endif
    constructWindowDefaultHeight            ,
    getWindowDefaultHeight                  ,
    setWindowDefaultHeight                  ,
#if defined(ENABLE_OVERLOADING)
    windowDefaultHeight                     ,
#endif


-- ** defaultWidth #attr:defaultWidth#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    WindowDefaultWidthPropertyInfo          ,
#endif
    constructWindowDefaultWidth             ,
    getWindowDefaultWidth                   ,
    setWindowDefaultWidth                   ,
#if defined(ENABLE_OVERLOADING)
    windowDefaultWidth                      ,
#endif


-- ** deletable #attr:deletable#
-- | Whether the window frame should have a close button.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    WindowDeletablePropertyInfo             ,
#endif
    constructWindowDeletable                ,
    getWindowDeletable                      ,
    setWindowDeletable                      ,
#if defined(ENABLE_OVERLOADING)
    windowDeletable                         ,
#endif


-- ** destroyWithParent #attr:destroyWithParent#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    WindowDestroyWithParentPropertyInfo     ,
#endif
    constructWindowDestroyWithParent        ,
    getWindowDestroyWithParent              ,
    setWindowDestroyWithParent              ,
#if defined(ENABLE_OVERLOADING)
    windowDestroyWithParent                 ,
#endif


-- ** focusOnMap #attr:focusOnMap#
-- | Whether the window should receive the input focus when mapped.
-- 
-- /Since: 2.6/

#if defined(ENABLE_OVERLOADING)
    WindowFocusOnMapPropertyInfo            ,
#endif
    constructWindowFocusOnMap               ,
    getWindowFocusOnMap                     ,
    setWindowFocusOnMap                     ,
#if defined(ENABLE_OVERLOADING)
    windowFocusOnMap                        ,
#endif


-- ** focusVisible #attr:focusVisible#
-- | Whether \'focus rectangles\' are currently visible in this window.
-- 
-- This property is maintained by GTK+ based on user input
-- and should not be set by applications.
-- 
-- /Since: 2.20/

#if defined(ENABLE_OVERLOADING)
    WindowFocusVisiblePropertyInfo          ,
#endif
    constructWindowFocusVisible             ,
    getWindowFocusVisible                   ,
    setWindowFocusVisible                   ,
#if defined(ENABLE_OVERLOADING)
    windowFocusVisible                      ,
#endif


-- ** gravity #attr:gravity#
-- | The window gravity of the window. See 'GI.Gtk.Objects.Window.windowMove' and t'GI.Gdk.Enums.Gravity' for
-- more details about window gravity.
-- 
-- /Since: 2.4/

#if defined(ENABLE_OVERLOADING)
    WindowGravityPropertyInfo               ,
#endif
    constructWindowGravity                  ,
    getWindowGravity                        ,
    setWindowGravity                        ,
#if defined(ENABLE_OVERLOADING)
    windowGravity                           ,
#endif


-- ** hasResizeGrip #attr:hasResizeGrip#
-- | Whether the window has a corner resize grip.
-- 
-- Note that the resize grip is only shown if the window is
-- actually resizable and not maximized. Use
-- [Window:resizeGripVisible]("GI.Gtk.Objects.Window#g:attr:resizeGripVisible") to find out if the resize
-- grip is currently shown.
-- 
-- /Since: 3.0/

#if defined(ENABLE_OVERLOADING)
    WindowHasResizeGripPropertyInfo         ,
#endif
    constructWindowHasResizeGrip            ,
    getWindowHasResizeGrip                  ,
    setWindowHasResizeGrip                  ,
#if defined(ENABLE_OVERLOADING)
    windowHasResizeGrip                     ,
#endif


-- ** hasToplevelFocus #attr:hasToplevelFocus#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    WindowHasToplevelFocusPropertyInfo      ,
#endif
    getWindowHasToplevelFocus               ,


-- ** hideTitlebarWhenMaximized #attr:hideTitlebarWhenMaximized#
-- | Whether the titlebar should be hidden during maximization.
-- 
-- /Since: 3.4/

#if defined(ENABLE_OVERLOADING)
    WindowHideTitlebarWhenMaximizedPropertyInfo,
#endif
    constructWindowHideTitlebarWhenMaximized,
    getWindowHideTitlebarWhenMaximized      ,
    setWindowHideTitlebarWhenMaximized      ,
#if defined(ENABLE_OVERLOADING)
    windowHideTitlebarWhenMaximized         ,
#endif


-- ** icon #attr:icon#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    WindowIconPropertyInfo                  ,
#endif
    clearWindowIcon                         ,
    constructWindowIcon                     ,
    getWindowIcon                           ,
    setWindowIcon                           ,
#if defined(ENABLE_OVERLOADING)
    windowIcon                              ,
#endif


-- ** iconName #attr:iconName#
-- | The :icon-name property specifies the name of the themed icon to
-- use as the window icon. See t'GI.Gtk.Objects.IconTheme.IconTheme' for more details.
-- 
-- /Since: 2.6/

#if defined(ENABLE_OVERLOADING)
    WindowIconNamePropertyInfo              ,
#endif
    clearWindowIconName                     ,
    constructWindowIconName                 ,
    getWindowIconName                       ,
    setWindowIconName                       ,
#if defined(ENABLE_OVERLOADING)
    windowIconName                          ,
#endif


-- ** isActive #attr:isActive#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    WindowIsActivePropertyInfo              ,
#endif
    getWindowIsActive                       ,


-- ** isMaximized #attr:isMaximized#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    WindowIsMaximizedPropertyInfo           ,
#endif
    getWindowIsMaximized                    ,


-- ** mnemonicsVisible #attr:mnemonicsVisible#
-- | Whether mnemonics are currently visible in this window.
-- 
-- This property is maintained by GTK+ based on user input,
-- and should not be set by applications.
-- 
-- /Since: 2.20/

#if defined(ENABLE_OVERLOADING)
    WindowMnemonicsVisiblePropertyInfo      ,
#endif
    constructWindowMnemonicsVisible         ,
    getWindowMnemonicsVisible               ,
    setWindowMnemonicsVisible               ,
#if defined(ENABLE_OVERLOADING)
    windowMnemonicsVisible                  ,
#endif


-- ** modal #attr:modal#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    WindowModalPropertyInfo                 ,
#endif
    constructWindowModal                    ,
    getWindowModal                          ,
    setWindowModal                          ,
#if defined(ENABLE_OVERLOADING)
    windowModal                             ,
#endif


-- ** resizable #attr:resizable#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    WindowResizablePropertyInfo             ,
#endif
    constructWindowResizable                ,
    getWindowResizable                      ,
    setWindowResizable                      ,
#if defined(ENABLE_OVERLOADING)
    windowResizable                         ,
#endif


-- ** resizeGripVisible #attr:resizeGripVisible#
-- | Whether a corner resize grip is currently shown.
-- 
-- /Since: 3.0/

#if defined(ENABLE_OVERLOADING)
    WindowResizeGripVisiblePropertyInfo     ,
#endif
    getWindowResizeGripVisible              ,
#if defined(ENABLE_OVERLOADING)
    windowResizeGripVisible                 ,
#endif


-- ** role #attr:role#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    WindowRolePropertyInfo                  ,
#endif
    constructWindowRole                     ,
    getWindowRole                           ,
    setWindowRole                           ,
#if defined(ENABLE_OVERLOADING)
    windowRole                              ,
#endif


-- ** screen #attr:screen#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    WindowScreenPropertyInfo                ,
#endif
    constructWindowScreen                   ,
    getWindowScreen                         ,
    setWindowScreen                         ,
#if defined(ENABLE_OVERLOADING)
    windowScreen                            ,
#endif


-- ** skipPagerHint #attr:skipPagerHint#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    WindowSkipPagerHintPropertyInfo         ,
#endif
    constructWindowSkipPagerHint            ,
    getWindowSkipPagerHint                  ,
    setWindowSkipPagerHint                  ,
#if defined(ENABLE_OVERLOADING)
    windowSkipPagerHint                     ,
#endif


-- ** skipTaskbarHint #attr:skipTaskbarHint#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    WindowSkipTaskbarHintPropertyInfo       ,
#endif
    constructWindowSkipTaskbarHint          ,
    getWindowSkipTaskbarHint                ,
    setWindowSkipTaskbarHint                ,
#if defined(ENABLE_OVERLOADING)
    windowSkipTaskbarHint                   ,
#endif


-- ** startupId #attr:startupId#
-- | The :startup-id is a write-only property for setting window\'s
-- startup notification identifier. See 'GI.Gtk.Objects.Window.windowSetStartupId'
-- for more details.
-- 
-- /Since: 2.12/

#if defined(ENABLE_OVERLOADING)
    WindowStartupIdPropertyInfo             ,
#endif
    constructWindowStartupId                ,
    setWindowStartupId                      ,
#if defined(ENABLE_OVERLOADING)
    windowStartupId                         ,
#endif


-- ** title #attr:title#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    WindowTitlePropertyInfo                 ,
#endif
    constructWindowTitle                    ,
    getWindowTitle                          ,
    setWindowTitle                          ,
#if defined(ENABLE_OVERLOADING)
    windowTitle                             ,
#endif


-- ** transientFor #attr:transientFor#
-- | The transient parent of the window. See 'GI.Gtk.Objects.Window.windowSetTransientFor' for
-- more details about transient windows.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    WindowTransientForPropertyInfo          ,
#endif
    clearWindowTransientFor                 ,
    constructWindowTransientFor             ,
    getWindowTransientFor                   ,
    setWindowTransientFor                   ,
#if defined(ENABLE_OVERLOADING)
    windowTransientFor                      ,
#endif


-- ** type #attr:type#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    WindowTypePropertyInfo                  ,
#endif
    constructWindowType                     ,
    getWindowType                           ,
#if defined(ENABLE_OVERLOADING)
    windowType                              ,
#endif


-- ** typeHint #attr:typeHint#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    WindowTypeHintPropertyInfo              ,
#endif
    constructWindowTypeHint                 ,
    getWindowTypeHint                       ,
    setWindowTypeHint                       ,
#if defined(ENABLE_OVERLOADING)
    windowTypeHint                          ,
#endif


-- ** urgencyHint #attr:urgencyHint#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    WindowUrgencyHintPropertyInfo           ,
#endif
    constructWindowUrgencyHint              ,
    getWindowUrgencyHint                    ,
    setWindowUrgencyHint                    ,
#if defined(ENABLE_OVERLOADING)
    windowUrgencyHint                       ,
#endif


-- ** windowPosition #attr:windowPosition#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    WindowWindowPositionPropertyInfo        ,
#endif
    constructWindowWindowPosition           ,
    getWindowWindowPosition                 ,
    setWindowWindowPosition                 ,
#if defined(ENABLE_OVERLOADING)
    windowWindowPosition                    ,
#endif




 -- * Signals


-- ** activateDefault #signal:activateDefault#

    WindowActivateDefaultCallback           ,
#if defined(ENABLE_OVERLOADING)
    WindowActivateDefaultSignalInfo         ,
#endif
    afterWindowActivateDefault              ,
    onWindowActivateDefault                 ,


-- ** activateFocus #signal:activateFocus#

    WindowActivateFocusCallback             ,
#if defined(ENABLE_OVERLOADING)
    WindowActivateFocusSignalInfo           ,
#endif
    afterWindowActivateFocus                ,
    onWindowActivateFocus                   ,


-- ** enableDebugging #signal:enableDebugging#

    WindowEnableDebuggingCallback           ,
#if defined(ENABLE_OVERLOADING)
    WindowEnableDebuggingSignalInfo         ,
#endif
    afterWindowEnableDebugging              ,
    onWindowEnableDebugging                 ,


-- ** keysChanged #signal:keysChanged#

    WindowKeysChangedCallback               ,
#if defined(ENABLE_OVERLOADING)
    WindowKeysChangedSignalInfo             ,
#endif
    afterWindowKeysChanged                  ,
    onWindowKeysChanged                     ,


-- ** setFocus #signal:setFocus#

    WindowSetFocusCallback                  ,
#if defined(ENABLE_OVERLOADING)
    WindowSetFocusSignalInfo                ,
#endif
    afterWindowSetFocus                     ,
    onWindowSetFocus                        ,




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
import qualified GI.Gdk.Enums as Gdk.Enums
import qualified GI.Gdk.Flags as Gdk.Flags
import qualified GI.Gdk.Objects.Screen as Gdk.Screen
import qualified GI.Gdk.Structs.EventKey as Gdk.EventKey
import qualified GI.Gdk.Structs.Geometry as Gdk.Geometry
import qualified GI.Gdk.Structs.Rectangle as Gdk.Rectangle
import qualified GI.GdkPixbuf.Objects.Pixbuf as GdkPixbuf.Pixbuf
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Objects.AccelGroup as Gtk.AccelGroup
import {-# SOURCE #-} qualified GI.Gtk.Objects.Application as Gtk.Application
import {-# SOURCE #-} qualified GI.Gtk.Objects.Bin as Gtk.Bin
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget
import {-# SOURCE #-} qualified GI.Gtk.Objects.WindowGroup as Gtk.WindowGroup

-- | Memory-managed wrapper type.
newtype Window = Window (SP.ManagedPtr Window)
    deriving (Eq)

instance SP.ManagedPtrNewtype Window where
    toManagedPtr (Window p) = p

foreign import ccall "gtk_window_get_type"
    c_gtk_window_get_type :: IO B.Types.GType

instance B.Types.TypedObject Window where
    glibType = c_gtk_window_get_type

instance B.Types.GObject Window

-- | Type class for types which can be safely cast to `Window`, for instance with `toWindow`.
class (SP.GObject o, O.IsDescendantOf Window o) => IsWindow o
instance (SP.GObject o, O.IsDescendantOf Window o) => IsWindow o

instance O.HasParentTypes Window
type instance O.ParentTypes Window = '[Gtk.Bin.Bin, Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable]

-- | Cast to `Window`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toWindow :: (MIO.MonadIO m, IsWindow o) => o -> m Window
toWindow = MIO.liftIO . B.ManagedPtr.unsafeCastTo Window

-- | Convert 'Window' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe Window) where
    gvalueGType_ = c_gtk_window_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr Window)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr Window)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject Window ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveWindowMethod (t :: Symbol) (o :: *) :: * where
    ResolveWindowMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveWindowMethod "activateDefault" o = WindowActivateDefaultMethodInfo
    ResolveWindowMethod "activateFocus" o = WindowActivateFocusMethodInfo
    ResolveWindowMethod "activateKey" o = WindowActivateKeyMethodInfo
    ResolveWindowMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveWindowMethod "addAccelGroup" o = WindowAddAccelGroupMethodInfo
    ResolveWindowMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveWindowMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveWindowMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveWindowMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveWindowMethod "addMnemonic" o = WindowAddMnemonicMethodInfo
    ResolveWindowMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveWindowMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveWindowMethod "beginMoveDrag" o = WindowBeginMoveDragMethodInfo
    ResolveWindowMethod "beginResizeDrag" o = WindowBeginResizeDragMethodInfo
    ResolveWindowMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveWindowMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveWindowMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveWindowMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveWindowMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveWindowMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveWindowMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveWindowMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveWindowMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveWindowMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveWindowMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveWindowMethod "close" o = WindowCloseMethodInfo
    ResolveWindowMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveWindowMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveWindowMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveWindowMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveWindowMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveWindowMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveWindowMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveWindowMethod "deiconify" o = WindowDeiconifyMethodInfo
    ResolveWindowMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveWindowMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveWindowMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveWindowMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveWindowMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveWindowMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveWindowMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveWindowMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveWindowMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveWindowMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveWindowMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveWindowMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveWindowMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveWindowMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveWindowMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveWindowMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveWindowMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveWindowMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveWindowMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveWindowMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveWindowMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveWindowMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveWindowMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveWindowMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveWindowMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveWindowMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveWindowMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveWindowMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveWindowMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveWindowMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveWindowMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveWindowMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveWindowMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveWindowMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveWindowMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveWindowMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveWindowMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveWindowMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveWindowMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveWindowMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveWindowMethod "fullscreen" o = WindowFullscreenMethodInfo
    ResolveWindowMethod "fullscreenOnMonitor" o = WindowFullscreenOnMonitorMethodInfo
    ResolveWindowMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveWindowMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveWindowMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveWindowMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveWindowMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveWindowMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveWindowMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveWindowMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveWindowMethod "hasGroup" o = WindowHasGroupMethodInfo
    ResolveWindowMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveWindowMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveWindowMethod "hasToplevelFocus" o = WindowHasToplevelFocusMethodInfo
    ResolveWindowMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveWindowMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveWindowMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveWindowMethod "iconify" o = WindowIconifyMethodInfo
    ResolveWindowMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveWindowMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveWindowMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveWindowMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveWindowMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveWindowMethod "isActive" o = WindowIsActiveMethodInfo
    ResolveWindowMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveWindowMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveWindowMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveWindowMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveWindowMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveWindowMethod "isMaximized" o = WindowIsMaximizedMethodInfo
    ResolveWindowMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveWindowMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveWindowMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveWindowMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveWindowMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveWindowMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveWindowMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveWindowMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveWindowMethod "maximize" o = WindowMaximizeMethodInfo
    ResolveWindowMethod "mnemonicActivate" o = WindowMnemonicActivateMethodInfo
    ResolveWindowMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveWindowMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveWindowMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveWindowMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveWindowMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveWindowMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveWindowMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveWindowMethod "move" o = WindowMoveMethodInfo
    ResolveWindowMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveWindowMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveWindowMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveWindowMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveWindowMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveWindowMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveWindowMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveWindowMethod "parseGeometry" o = WindowParseGeometryMethodInfo
    ResolveWindowMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveWindowMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveWindowMethod "present" o = WindowPresentMethodInfo
    ResolveWindowMethod "presentWithTime" o = WindowPresentWithTimeMethodInfo
    ResolveWindowMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveWindowMethod "propagateKeyEvent" o = WindowPropagateKeyEventMethodInfo
    ResolveWindowMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveWindowMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveWindowMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveWindowMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveWindowMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveWindowMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveWindowMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveWindowMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveWindowMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveWindowMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveWindowMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveWindowMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveWindowMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolveWindowMethod "removeAccelGroup" o = WindowRemoveAccelGroupMethodInfo
    ResolveWindowMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveWindowMethod "removeMnemonic" o = WindowRemoveMnemonicMethodInfo
    ResolveWindowMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveWindowMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveWindowMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveWindowMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveWindowMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveWindowMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveWindowMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveWindowMethod "reshowWithInitialSize" o = WindowReshowWithInitialSizeMethodInfo
    ResolveWindowMethod "resize" o = WindowResizeMethodInfo
    ResolveWindowMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveWindowMethod "resizeGripIsVisible" o = WindowResizeGripIsVisibleMethodInfo
    ResolveWindowMethod "resizeToGeometry" o = WindowResizeToGeometryMethodInfo
    ResolveWindowMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveWindowMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveWindowMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveWindowMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveWindowMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveWindowMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveWindowMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveWindowMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveWindowMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveWindowMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveWindowMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveWindowMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveWindowMethod "stick" o = WindowStickMethodInfo
    ResolveWindowMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveWindowMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveWindowMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveWindowMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveWindowMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveWindowMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveWindowMethod "unfullscreen" o = WindowUnfullscreenMethodInfo
    ResolveWindowMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveWindowMethod "unmaximize" o = WindowUnmaximizeMethodInfo
    ResolveWindowMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveWindowMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveWindowMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveWindowMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveWindowMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveWindowMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveWindowMethod "unstick" o = WindowUnstickMethodInfo
    ResolveWindowMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveWindowMethod "getAcceptFocus" o = WindowGetAcceptFocusMethodInfo
    ResolveWindowMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveWindowMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveWindowMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveWindowMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveWindowMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveWindowMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveWindowMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveWindowMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveWindowMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveWindowMethod "getApplication" o = WindowGetApplicationMethodInfo
    ResolveWindowMethod "getAttachedTo" o = WindowGetAttachedToMethodInfo
    ResolveWindowMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveWindowMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveWindowMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveWindowMethod "getChild" o = Gtk.Bin.BinGetChildMethodInfo
    ResolveWindowMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveWindowMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveWindowMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveWindowMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveWindowMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveWindowMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveWindowMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveWindowMethod "getDecorated" o = WindowGetDecoratedMethodInfo
    ResolveWindowMethod "getDefaultSize" o = WindowGetDefaultSizeMethodInfo
    ResolveWindowMethod "getDefaultWidget" o = WindowGetDefaultWidgetMethodInfo
    ResolveWindowMethod "getDeletable" o = WindowGetDeletableMethodInfo
    ResolveWindowMethod "getDestroyWithParent" o = WindowGetDestroyWithParentMethodInfo
    ResolveWindowMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveWindowMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveWindowMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveWindowMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveWindowMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveWindowMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveWindowMethod "getFocus" o = WindowGetFocusMethodInfo
    ResolveWindowMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveWindowMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveWindowMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveWindowMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveWindowMethod "getFocusOnMap" o = WindowGetFocusOnMapMethodInfo
    ResolveWindowMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveWindowMethod "getFocusVisible" o = WindowGetFocusVisibleMethodInfo
    ResolveWindowMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveWindowMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveWindowMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveWindowMethod "getGravity" o = WindowGetGravityMethodInfo
    ResolveWindowMethod "getGroup" o = WindowGetGroupMethodInfo
    ResolveWindowMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveWindowMethod "getHasResizeGrip" o = WindowGetHasResizeGripMethodInfo
    ResolveWindowMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveWindowMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveWindowMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveWindowMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveWindowMethod "getHideTitlebarWhenMaximized" o = WindowGetHideTitlebarWhenMaximizedMethodInfo
    ResolveWindowMethod "getIcon" o = WindowGetIconMethodInfo
    ResolveWindowMethod "getIconList" o = WindowGetIconListMethodInfo
    ResolveWindowMethod "getIconName" o = WindowGetIconNameMethodInfo
    ResolveWindowMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveWindowMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveWindowMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveWindowMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveWindowMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveWindowMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveWindowMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveWindowMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveWindowMethod "getMnemonicModifier" o = WindowGetMnemonicModifierMethodInfo
    ResolveWindowMethod "getMnemonicsVisible" o = WindowGetMnemonicsVisibleMethodInfo
    ResolveWindowMethod "getModal" o = WindowGetModalMethodInfo
    ResolveWindowMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveWindowMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveWindowMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveWindowMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveWindowMethod "getOpacity" o = WindowGetOpacityMethodInfo
    ResolveWindowMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveWindowMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveWindowMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveWindowMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveWindowMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveWindowMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveWindowMethod "getPosition" o = WindowGetPositionMethodInfo
    ResolveWindowMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveWindowMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveWindowMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveWindowMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveWindowMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveWindowMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveWindowMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveWindowMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveWindowMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveWindowMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveWindowMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveWindowMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveWindowMethod "getResizable" o = WindowGetResizableMethodInfo
    ResolveWindowMethod "getResizeGripArea" o = WindowGetResizeGripAreaMethodInfo
    ResolveWindowMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveWindowMethod "getRole" o = WindowGetRoleMethodInfo
    ResolveWindowMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveWindowMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveWindowMethod "getScreen" o = WindowGetScreenMethodInfo
    ResolveWindowMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveWindowMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveWindowMethod "getSize" o = WindowGetSizeMethodInfo
    ResolveWindowMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveWindowMethod "getSkipPagerHint" o = WindowGetSkipPagerHintMethodInfo
    ResolveWindowMethod "getSkipTaskbarHint" o = WindowGetSkipTaskbarHintMethodInfo
    ResolveWindowMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveWindowMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveWindowMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveWindowMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveWindowMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveWindowMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveWindowMethod "getTitle" o = WindowGetTitleMethodInfo
    ResolveWindowMethod "getTitlebar" o = WindowGetTitlebarMethodInfo
    ResolveWindowMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveWindowMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveWindowMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveWindowMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveWindowMethod "getTransientFor" o = WindowGetTransientForMethodInfo
    ResolveWindowMethod "getTypeHint" o = WindowGetTypeHintMethodInfo
    ResolveWindowMethod "getUrgencyHint" o = WindowGetUrgencyHintMethodInfo
    ResolveWindowMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveWindowMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveWindowMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveWindowMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveWindowMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveWindowMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveWindowMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveWindowMethod "getWindowType" o = WindowGetWindowTypeMethodInfo
    ResolveWindowMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveWindowMethod "setAcceptFocus" o = WindowSetAcceptFocusMethodInfo
    ResolveWindowMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveWindowMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveWindowMethod "setApplication" o = WindowSetApplicationMethodInfo
    ResolveWindowMethod "setAttachedTo" o = WindowSetAttachedToMethodInfo
    ResolveWindowMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveWindowMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveWindowMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveWindowMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveWindowMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveWindowMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveWindowMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveWindowMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveWindowMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveWindowMethod "setDecorated" o = WindowSetDecoratedMethodInfo
    ResolveWindowMethod "setDefault" o = WindowSetDefaultMethodInfo
    ResolveWindowMethod "setDefaultGeometry" o = WindowSetDefaultGeometryMethodInfo
    ResolveWindowMethod "setDefaultSize" o = WindowSetDefaultSizeMethodInfo
    ResolveWindowMethod "setDeletable" o = WindowSetDeletableMethodInfo
    ResolveWindowMethod "setDestroyWithParent" o = WindowSetDestroyWithParentMethodInfo
    ResolveWindowMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveWindowMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveWindowMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveWindowMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveWindowMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveWindowMethod "setFocus" o = WindowSetFocusMethodInfo
    ResolveWindowMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveWindowMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveWindowMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveWindowMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveWindowMethod "setFocusOnMap" o = WindowSetFocusOnMapMethodInfo
    ResolveWindowMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveWindowMethod "setFocusVisible" o = WindowSetFocusVisibleMethodInfo
    ResolveWindowMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveWindowMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveWindowMethod "setGeometryHints" o = WindowSetGeometryHintsMethodInfo
    ResolveWindowMethod "setGravity" o = WindowSetGravityMethodInfo
    ResolveWindowMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveWindowMethod "setHasResizeGrip" o = WindowSetHasResizeGripMethodInfo
    ResolveWindowMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveWindowMethod "setHasUserRefCount" o = WindowSetHasUserRefCountMethodInfo
    ResolveWindowMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveWindowMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveWindowMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveWindowMethod "setHideTitlebarWhenMaximized" o = WindowSetHideTitlebarWhenMaximizedMethodInfo
    ResolveWindowMethod "setIcon" o = WindowSetIconMethodInfo
    ResolveWindowMethod "setIconFromFile" o = WindowSetIconFromFileMethodInfo
    ResolveWindowMethod "setIconList" o = WindowSetIconListMethodInfo
    ResolveWindowMethod "setIconName" o = WindowSetIconNameMethodInfo
    ResolveWindowMethod "setKeepAbove" o = WindowSetKeepAboveMethodInfo
    ResolveWindowMethod "setKeepBelow" o = WindowSetKeepBelowMethodInfo
    ResolveWindowMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveWindowMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveWindowMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveWindowMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveWindowMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveWindowMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveWindowMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveWindowMethod "setMnemonicModifier" o = WindowSetMnemonicModifierMethodInfo
    ResolveWindowMethod "setMnemonicsVisible" o = WindowSetMnemonicsVisibleMethodInfo
    ResolveWindowMethod "setModal" o = WindowSetModalMethodInfo
    ResolveWindowMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveWindowMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveWindowMethod "setOpacity" o = WindowSetOpacityMethodInfo
    ResolveWindowMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveWindowMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveWindowMethod "setPosition" o = WindowSetPositionMethodInfo
    ResolveWindowMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveWindowMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveWindowMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveWindowMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveWindowMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveWindowMethod "setResizable" o = WindowSetResizableMethodInfo
    ResolveWindowMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveWindowMethod "setRole" o = WindowSetRoleMethodInfo
    ResolveWindowMethod "setScreen" o = WindowSetScreenMethodInfo
    ResolveWindowMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveWindowMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveWindowMethod "setSkipPagerHint" o = WindowSetSkipPagerHintMethodInfo
    ResolveWindowMethod "setSkipTaskbarHint" o = WindowSetSkipTaskbarHintMethodInfo
    ResolveWindowMethod "setStartupId" o = WindowSetStartupIdMethodInfo
    ResolveWindowMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveWindowMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveWindowMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveWindowMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveWindowMethod "setTitle" o = WindowSetTitleMethodInfo
    ResolveWindowMethod "setTitlebar" o = WindowSetTitlebarMethodInfo
    ResolveWindowMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveWindowMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveWindowMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveWindowMethod "setTransientFor" o = WindowSetTransientForMethodInfo
    ResolveWindowMethod "setTypeHint" o = WindowSetTypeHintMethodInfo
    ResolveWindowMethod "setUrgencyHint" o = WindowSetUrgencyHintMethodInfo
    ResolveWindowMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveWindowMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveWindowMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveWindowMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveWindowMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveWindowMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveWindowMethod "setWmclass" o = WindowSetWmclassMethodInfo
    ResolveWindowMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveWindowMethod t Window, O.OverloadedMethod info Window p) => OL.IsLabel t (Window -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveWindowMethod t Window, O.OverloadedMethod info Window p, R.HasField t Window p) => R.HasField t Window p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveWindowMethod t Window, O.OverloadedMethodInfo info Window) => OL.IsLabel t (O.MethodProxy info Window) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal Window::activate-default
-- | The [activateDefault](#g:signal:activateDefault) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted when the user activates the default widget
-- of /@window@/.
type WindowActivateDefaultCallback =
    IO ()

type C_WindowActivateDefaultCallback =
    Ptr Window ->                           -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_WindowActivateDefaultCallback`.
foreign import ccall "wrapper"
    mk_WindowActivateDefaultCallback :: C_WindowActivateDefaultCallback -> IO (FunPtr C_WindowActivateDefaultCallback)

wrap_WindowActivateDefaultCallback :: 
    GObject a => (a -> WindowActivateDefaultCallback) ->
    C_WindowActivateDefaultCallback
wrap_WindowActivateDefaultCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [activateDefault](#signal:activateDefault) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' window #activateDefault callback
-- @
-- 
-- 
onWindowActivateDefault :: (IsWindow a, MonadIO m) => a -> ((?self :: a) => WindowActivateDefaultCallback) -> m SignalHandlerId
onWindowActivateDefault obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_WindowActivateDefaultCallback wrapped
    wrapped'' <- mk_WindowActivateDefaultCallback wrapped'
    connectSignalFunPtr obj "activate-default" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [activateDefault](#signal:activateDefault) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' window #activateDefault callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterWindowActivateDefault :: (IsWindow a, MonadIO m) => a -> ((?self :: a) => WindowActivateDefaultCallback) -> m SignalHandlerId
afterWindowActivateDefault obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_WindowActivateDefaultCallback wrapped
    wrapped'' <- mk_WindowActivateDefaultCallback wrapped'
    connectSignalFunPtr obj "activate-default" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data WindowActivateDefaultSignalInfo
instance SignalInfo WindowActivateDefaultSignalInfo where
    type HaskellCallbackType WindowActivateDefaultSignalInfo = WindowActivateDefaultCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_WindowActivateDefaultCallback cb
        cb'' <- mk_WindowActivateDefaultCallback cb'
        connectSignalFunPtr obj "activate-default" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window::activate-default"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:signal:activateDefault"})

#endif

-- signal Window::activate-focus
-- | The [activateFocus](#g:signal:activateFocus) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted when the user activates the currently
-- focused widget of /@window@/.
type WindowActivateFocusCallback =
    IO ()

type C_WindowActivateFocusCallback =
    Ptr Window ->                           -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_WindowActivateFocusCallback`.
foreign import ccall "wrapper"
    mk_WindowActivateFocusCallback :: C_WindowActivateFocusCallback -> IO (FunPtr C_WindowActivateFocusCallback)

wrap_WindowActivateFocusCallback :: 
    GObject a => (a -> WindowActivateFocusCallback) ->
    C_WindowActivateFocusCallback
wrap_WindowActivateFocusCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [activateFocus](#signal:activateFocus) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' window #activateFocus callback
-- @
-- 
-- 
onWindowActivateFocus :: (IsWindow a, MonadIO m) => a -> ((?self :: a) => WindowActivateFocusCallback) -> m SignalHandlerId
onWindowActivateFocus obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_WindowActivateFocusCallback wrapped
    wrapped'' <- mk_WindowActivateFocusCallback wrapped'
    connectSignalFunPtr obj "activate-focus" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [activateFocus](#signal:activateFocus) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' window #activateFocus callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterWindowActivateFocus :: (IsWindow a, MonadIO m) => a -> ((?self :: a) => WindowActivateFocusCallback) -> m SignalHandlerId
afterWindowActivateFocus obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_WindowActivateFocusCallback wrapped
    wrapped'' <- mk_WindowActivateFocusCallback wrapped'
    connectSignalFunPtr obj "activate-focus" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data WindowActivateFocusSignalInfo
instance SignalInfo WindowActivateFocusSignalInfo where
    type HaskellCallbackType WindowActivateFocusSignalInfo = WindowActivateFocusCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_WindowActivateFocusCallback cb
        cb'' <- mk_WindowActivateFocusCallback cb'
        connectSignalFunPtr obj "activate-focus" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window::activate-focus"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:signal:activateFocus"})

#endif

-- signal Window::enable-debugging
-- | The [enableDebugging](#g:signal:enableDebugging) signal is a [keybinding signal][GtkBindingSignal]
-- which gets emitted when the user enables or disables interactive
-- debugging. When /@toggle@/ is 'P.True', interactive debugging is toggled
-- on or off, when it is 'P.False', the debugger will be pointed at the
-- widget under the pointer.
-- 
-- The default bindings for this signal are Ctrl-Shift-I
-- and Ctrl-Shift-D.
type WindowEnableDebuggingCallback =
    Bool
    -- ^ /@toggle@/: toggle the debugger
    -> IO Bool
    -- ^ __Returns:__ 'P.True' if the key binding was handled

type C_WindowEnableDebuggingCallback =
    Ptr Window ->                           -- object
    CInt ->
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_WindowEnableDebuggingCallback`.
foreign import ccall "wrapper"
    mk_WindowEnableDebuggingCallback :: C_WindowEnableDebuggingCallback -> IO (FunPtr C_WindowEnableDebuggingCallback)

wrap_WindowEnableDebuggingCallback :: 
    GObject a => (a -> WindowEnableDebuggingCallback) ->
    C_WindowEnableDebuggingCallback
wrap_WindowEnableDebuggingCallback gi'cb gi'selfPtr toggle _ = do
    let toggle' = (/= 0) toggle
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  toggle'
    let result' = (fromIntegral . fromEnum) result
    return result'


-- | Connect a signal handler for the [enableDebugging](#signal:enableDebugging) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' window #enableDebugging callback
-- @
-- 
-- 
onWindowEnableDebugging :: (IsWindow a, MonadIO m) => a -> ((?self :: a) => WindowEnableDebuggingCallback) -> m SignalHandlerId
onWindowEnableDebugging obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_WindowEnableDebuggingCallback wrapped
    wrapped'' <- mk_WindowEnableDebuggingCallback wrapped'
    connectSignalFunPtr obj "enable-debugging" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [enableDebugging](#signal:enableDebugging) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' window #enableDebugging callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterWindowEnableDebugging :: (IsWindow a, MonadIO m) => a -> ((?self :: a) => WindowEnableDebuggingCallback) -> m SignalHandlerId
afterWindowEnableDebugging obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_WindowEnableDebuggingCallback wrapped
    wrapped'' <- mk_WindowEnableDebuggingCallback wrapped'
    connectSignalFunPtr obj "enable-debugging" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data WindowEnableDebuggingSignalInfo
instance SignalInfo WindowEnableDebuggingSignalInfo where
    type HaskellCallbackType WindowEnableDebuggingSignalInfo = WindowEnableDebuggingCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_WindowEnableDebuggingCallback cb
        cb'' <- mk_WindowEnableDebuggingCallback cb'
        connectSignalFunPtr obj "enable-debugging" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window::enable-debugging"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:signal:enableDebugging"})

#endif

-- signal Window::keys-changed
-- | The [keysChanged](#g:signal:keysChanged) signal gets emitted when the set of accelerators
-- or mnemonics that are associated with /@window@/ changes.
type WindowKeysChangedCallback =
    IO ()

type C_WindowKeysChangedCallback =
    Ptr Window ->                           -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_WindowKeysChangedCallback`.
foreign import ccall "wrapper"
    mk_WindowKeysChangedCallback :: C_WindowKeysChangedCallback -> IO (FunPtr C_WindowKeysChangedCallback)

wrap_WindowKeysChangedCallback :: 
    GObject a => (a -> WindowKeysChangedCallback) ->
    C_WindowKeysChangedCallback
wrap_WindowKeysChangedCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [keysChanged](#signal:keysChanged) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' window #keysChanged callback
-- @
-- 
-- 
onWindowKeysChanged :: (IsWindow a, MonadIO m) => a -> ((?self :: a) => WindowKeysChangedCallback) -> m SignalHandlerId
onWindowKeysChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_WindowKeysChangedCallback wrapped
    wrapped'' <- mk_WindowKeysChangedCallback wrapped'
    connectSignalFunPtr obj "keys-changed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [keysChanged](#signal:keysChanged) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' window #keysChanged callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterWindowKeysChanged :: (IsWindow a, MonadIO m) => a -> ((?self :: a) => WindowKeysChangedCallback) -> m SignalHandlerId
afterWindowKeysChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_WindowKeysChangedCallback wrapped
    wrapped'' <- mk_WindowKeysChangedCallback wrapped'
    connectSignalFunPtr obj "keys-changed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data WindowKeysChangedSignalInfo
instance SignalInfo WindowKeysChangedSignalInfo where
    type HaskellCallbackType WindowKeysChangedSignalInfo = WindowKeysChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_WindowKeysChangedCallback cb
        cb'' <- mk_WindowKeysChangedCallback cb'
        connectSignalFunPtr obj "keys-changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window::keys-changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:signal:keysChanged"})

#endif

-- signal Window::set-focus
-- | This signal is emitted whenever the currently focused widget in
-- this window changes.
-- 
-- /Since: 2.24/
type WindowSetFocusCallback =
    Maybe Gtk.Widget.Widget
    -- ^ /@widget@/: the newly focused widget (or 'P.Nothing' for no focus)
    -> IO ()

type C_WindowSetFocusCallback =
    Ptr Window ->                           -- object
    Ptr Gtk.Widget.Widget ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_WindowSetFocusCallback`.
foreign import ccall "wrapper"
    mk_WindowSetFocusCallback :: C_WindowSetFocusCallback -> IO (FunPtr C_WindowSetFocusCallback)

wrap_WindowSetFocusCallback :: 
    GObject a => (a -> WindowSetFocusCallback) ->
    C_WindowSetFocusCallback
wrap_WindowSetFocusCallback gi'cb gi'selfPtr widget _ = do
    maybeWidget <-
        if widget == nullPtr
        then return Nothing
        else do
            widget' <- (newObject Gtk.Widget.Widget) widget
            return $ Just widget'
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  maybeWidget


-- | Connect a signal handler for the [setFocus](#signal:setFocus) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' window #setFocus callback
-- @
-- 
-- 
onWindowSetFocus :: (IsWindow a, MonadIO m) => a -> ((?self :: a) => WindowSetFocusCallback) -> m SignalHandlerId
onWindowSetFocus obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_WindowSetFocusCallback wrapped
    wrapped'' <- mk_WindowSetFocusCallback wrapped'
    connectSignalFunPtr obj "set-focus" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [setFocus](#signal:setFocus) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' window #setFocus callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterWindowSetFocus :: (IsWindow a, MonadIO m) => a -> ((?self :: a) => WindowSetFocusCallback) -> m SignalHandlerId
afterWindowSetFocus obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_WindowSetFocusCallback wrapped
    wrapped'' <- mk_WindowSetFocusCallback wrapped'
    connectSignalFunPtr obj "set-focus" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data WindowSetFocusSignalInfo
instance SignalInfo WindowSetFocusSignalInfo where
    type HaskellCallbackType WindowSetFocusSignalInfo = WindowSetFocusCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_WindowSetFocusCallback cb
        cb'' <- mk_WindowSetFocusCallback cb'
        connectSignalFunPtr obj "set-focus" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window::set-focus"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:signal:setFocus"})

#endif

-- VVV Prop "accept-focus"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@accept-focus@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' window #acceptFocus
-- @
getWindowAcceptFocus :: (MonadIO m, IsWindow o) => o -> m Bool
getWindowAcceptFocus obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "accept-focus"

-- | Set the value of the “@accept-focus@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' window [ #acceptFocus 'Data.GI.Base.Attributes.:=' value ]
-- @
setWindowAcceptFocus :: (MonadIO m, IsWindow o) => o -> Bool -> m ()
setWindowAcceptFocus obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "accept-focus" val

-- | Construct a `GValueConstruct` with valid value for the “@accept-focus@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructWindowAcceptFocus :: (IsWindow o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructWindowAcceptFocus val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "accept-focus" val

#if defined(ENABLE_OVERLOADING)
data WindowAcceptFocusPropertyInfo
instance AttrInfo WindowAcceptFocusPropertyInfo where
    type AttrAllowedOps WindowAcceptFocusPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint WindowAcceptFocusPropertyInfo = IsWindow
    type AttrSetTypeConstraint WindowAcceptFocusPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint WindowAcceptFocusPropertyInfo = (~) Bool
    type AttrTransferType WindowAcceptFocusPropertyInfo = Bool
    type AttrGetType WindowAcceptFocusPropertyInfo = Bool
    type AttrLabel WindowAcceptFocusPropertyInfo = "accept-focus"
    type AttrOrigin WindowAcceptFocusPropertyInfo = Window
    attrGet = getWindowAcceptFocus
    attrSet = setWindowAcceptFocus
    attrTransfer _ v = do
        return v
    attrConstruct = constructWindowAcceptFocus
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.acceptFocus"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:attr:acceptFocus"
        })
#endif

-- VVV Prop "application"
   -- Type: TInterface (Name {namespace = "Gtk", name = "Application"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just True)

-- | Get the value of the “@application@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' window #application
-- @
getWindowApplication :: (MonadIO m, IsWindow o) => o -> m (Maybe Gtk.Application.Application)
getWindowApplication obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "application" Gtk.Application.Application

-- | Set the value of the “@application@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' window [ #application 'Data.GI.Base.Attributes.:=' value ]
-- @
setWindowApplication :: (MonadIO m, IsWindow o, Gtk.Application.IsApplication a) => o -> a -> m ()
setWindowApplication obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "application" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@application@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructWindowApplication :: (IsWindow o, MIO.MonadIO m, Gtk.Application.IsApplication a) => a -> m (GValueConstruct o)
constructWindowApplication val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "application" (P.Just val)

-- | Set the value of the “@application@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #application
-- @
clearWindowApplication :: (MonadIO m, IsWindow o) => o -> m ()
clearWindowApplication obj = liftIO $ B.Properties.setObjectPropertyObject obj "application" (Nothing :: Maybe Gtk.Application.Application)

#if defined(ENABLE_OVERLOADING)
data WindowApplicationPropertyInfo
instance AttrInfo WindowApplicationPropertyInfo where
    type AttrAllowedOps WindowApplicationPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint WindowApplicationPropertyInfo = IsWindow
    type AttrSetTypeConstraint WindowApplicationPropertyInfo = Gtk.Application.IsApplication
    type AttrTransferTypeConstraint WindowApplicationPropertyInfo = Gtk.Application.IsApplication
    type AttrTransferType WindowApplicationPropertyInfo = Gtk.Application.Application
    type AttrGetType WindowApplicationPropertyInfo = (Maybe Gtk.Application.Application)
    type AttrLabel WindowApplicationPropertyInfo = "application"
    type AttrOrigin WindowApplicationPropertyInfo = Window
    attrGet = getWindowApplication
    attrSet = setWindowApplication
    attrTransfer _ v = do
        unsafeCastTo Gtk.Application.Application v
    attrConstruct = constructWindowApplication
    attrClear = clearWindowApplication
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.application"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:attr:application"
        })
#endif

-- VVV Prop "attached-to"
   -- Type: TInterface (Name {namespace = "Gtk", name = "Widget"})
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstruct]
   -- Nullable: (Just True,Just True)

-- | Get the value of the “@attached-to@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' window #attachedTo
-- @
getWindowAttachedTo :: (MonadIO m, IsWindow o) => o -> m (Maybe Gtk.Widget.Widget)
getWindowAttachedTo obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "attached-to" Gtk.Widget.Widget

-- | Set the value of the “@attached-to@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' window [ #attachedTo 'Data.GI.Base.Attributes.:=' value ]
-- @
setWindowAttachedTo :: (MonadIO m, IsWindow o, Gtk.Widget.IsWidget a) => o -> a -> m ()
setWindowAttachedTo obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "attached-to" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@attached-to@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructWindowAttachedTo :: (IsWindow o, MIO.MonadIO m, Gtk.Widget.IsWidget a) => a -> m (GValueConstruct o)
constructWindowAttachedTo val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "attached-to" (P.Just val)

-- | Set the value of the “@attached-to@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #attachedTo
-- @
clearWindowAttachedTo :: (MonadIO m, IsWindow o) => o -> m ()
clearWindowAttachedTo obj = liftIO $ B.Properties.setObjectPropertyObject obj "attached-to" (Nothing :: Maybe Gtk.Widget.Widget)

#if defined(ENABLE_OVERLOADING)
data WindowAttachedToPropertyInfo
instance AttrInfo WindowAttachedToPropertyInfo where
    type AttrAllowedOps WindowAttachedToPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint WindowAttachedToPropertyInfo = IsWindow
    type AttrSetTypeConstraint WindowAttachedToPropertyInfo = Gtk.Widget.IsWidget
    type AttrTransferTypeConstraint WindowAttachedToPropertyInfo = Gtk.Widget.IsWidget
    type AttrTransferType WindowAttachedToPropertyInfo = Gtk.Widget.Widget
    type AttrGetType WindowAttachedToPropertyInfo = (Maybe Gtk.Widget.Widget)
    type AttrLabel WindowAttachedToPropertyInfo = "attached-to"
    type AttrOrigin WindowAttachedToPropertyInfo = Window
    attrGet = getWindowAttachedTo
    attrSet = setWindowAttachedTo
    attrTransfer _ v = do
        unsafeCastTo Gtk.Widget.Widget v
    attrConstruct = constructWindowAttachedTo
    attrClear = clearWindowAttachedTo
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.attachedTo"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:attr:attachedTo"
        })
#endif

-- VVV Prop "decorated"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@decorated@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' window #decorated
-- @
getWindowDecorated :: (MonadIO m, IsWindow o) => o -> m Bool
getWindowDecorated obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "decorated"

-- | Set the value of the “@decorated@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' window [ #decorated 'Data.GI.Base.Attributes.:=' value ]
-- @
setWindowDecorated :: (MonadIO m, IsWindow o) => o -> Bool -> m ()
setWindowDecorated obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "decorated" val

-- | Construct a `GValueConstruct` with valid value for the “@decorated@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructWindowDecorated :: (IsWindow o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructWindowDecorated val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "decorated" val

#if defined(ENABLE_OVERLOADING)
data WindowDecoratedPropertyInfo
instance AttrInfo WindowDecoratedPropertyInfo where
    type AttrAllowedOps WindowDecoratedPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint WindowDecoratedPropertyInfo = IsWindow
    type AttrSetTypeConstraint WindowDecoratedPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint WindowDecoratedPropertyInfo = (~) Bool
    type AttrTransferType WindowDecoratedPropertyInfo = Bool
    type AttrGetType WindowDecoratedPropertyInfo = Bool
    type AttrLabel WindowDecoratedPropertyInfo = "decorated"
    type AttrOrigin WindowDecoratedPropertyInfo = Window
    attrGet = getWindowDecorated
    attrSet = setWindowDecorated
    attrTransfer _ v = do
        return v
    attrConstruct = constructWindowDecorated
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.decorated"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:attr:decorated"
        })
#endif

-- VVV Prop "default-height"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@default-height@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' window #defaultHeight
-- @
getWindowDefaultHeight :: (MonadIO m, IsWindow o) => o -> m Int32
getWindowDefaultHeight obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "default-height"

-- | Set the value of the “@default-height@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' window [ #defaultHeight 'Data.GI.Base.Attributes.:=' value ]
-- @
setWindowDefaultHeight :: (MonadIO m, IsWindow o) => o -> Int32 -> m ()
setWindowDefaultHeight obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "default-height" val

-- | Construct a `GValueConstruct` with valid value for the “@default-height@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructWindowDefaultHeight :: (IsWindow o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructWindowDefaultHeight val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "default-height" val

#if defined(ENABLE_OVERLOADING)
data WindowDefaultHeightPropertyInfo
instance AttrInfo WindowDefaultHeightPropertyInfo where
    type AttrAllowedOps WindowDefaultHeightPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint WindowDefaultHeightPropertyInfo = IsWindow
    type AttrSetTypeConstraint WindowDefaultHeightPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint WindowDefaultHeightPropertyInfo = (~) Int32
    type AttrTransferType WindowDefaultHeightPropertyInfo = Int32
    type AttrGetType WindowDefaultHeightPropertyInfo = Int32
    type AttrLabel WindowDefaultHeightPropertyInfo = "default-height"
    type AttrOrigin WindowDefaultHeightPropertyInfo = Window
    attrGet = getWindowDefaultHeight
    attrSet = setWindowDefaultHeight
    attrTransfer _ v = do
        return v
    attrConstruct = constructWindowDefaultHeight
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.defaultHeight"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:attr:defaultHeight"
        })
#endif

-- VVV Prop "default-width"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@default-width@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' window #defaultWidth
-- @
getWindowDefaultWidth :: (MonadIO m, IsWindow o) => o -> m Int32
getWindowDefaultWidth obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "default-width"

-- | Set the value of the “@default-width@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' window [ #defaultWidth 'Data.GI.Base.Attributes.:=' value ]
-- @
setWindowDefaultWidth :: (MonadIO m, IsWindow o) => o -> Int32 -> m ()
setWindowDefaultWidth obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "default-width" val

-- | Construct a `GValueConstruct` with valid value for the “@default-width@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructWindowDefaultWidth :: (IsWindow o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructWindowDefaultWidth val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "default-width" val

#if defined(ENABLE_OVERLOADING)
data WindowDefaultWidthPropertyInfo
instance AttrInfo WindowDefaultWidthPropertyInfo where
    type AttrAllowedOps WindowDefaultWidthPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint WindowDefaultWidthPropertyInfo = IsWindow
    type AttrSetTypeConstraint WindowDefaultWidthPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint WindowDefaultWidthPropertyInfo = (~) Int32
    type AttrTransferType WindowDefaultWidthPropertyInfo = Int32
    type AttrGetType WindowDefaultWidthPropertyInfo = Int32
    type AttrLabel WindowDefaultWidthPropertyInfo = "default-width"
    type AttrOrigin WindowDefaultWidthPropertyInfo = Window
    attrGet = getWindowDefaultWidth
    attrSet = setWindowDefaultWidth
    attrTransfer _ v = do
        return v
    attrConstruct = constructWindowDefaultWidth
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.defaultWidth"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:attr:defaultWidth"
        })
#endif

-- VVV Prop "deletable"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@deletable@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' window #deletable
-- @
getWindowDeletable :: (MonadIO m, IsWindow o) => o -> m Bool
getWindowDeletable obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "deletable"

-- | Set the value of the “@deletable@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' window [ #deletable 'Data.GI.Base.Attributes.:=' value ]
-- @
setWindowDeletable :: (MonadIO m, IsWindow o) => o -> Bool -> m ()
setWindowDeletable obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "deletable" val

-- | Construct a `GValueConstruct` with valid value for the “@deletable@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructWindowDeletable :: (IsWindow o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructWindowDeletable val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "deletable" val

#if defined(ENABLE_OVERLOADING)
data WindowDeletablePropertyInfo
instance AttrInfo WindowDeletablePropertyInfo where
    type AttrAllowedOps WindowDeletablePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint WindowDeletablePropertyInfo = IsWindow
    type AttrSetTypeConstraint WindowDeletablePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint WindowDeletablePropertyInfo = (~) Bool
    type AttrTransferType WindowDeletablePropertyInfo = Bool
    type AttrGetType WindowDeletablePropertyInfo = Bool
    type AttrLabel WindowDeletablePropertyInfo = "deletable"
    type AttrOrigin WindowDeletablePropertyInfo = Window
    attrGet = getWindowDeletable
    attrSet = setWindowDeletable
    attrTransfer _ v = do
        return v
    attrConstruct = constructWindowDeletable
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.deletable"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:attr:deletable"
        })
#endif

-- VVV Prop "destroy-with-parent"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@destroy-with-parent@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' window #destroyWithParent
-- @
getWindowDestroyWithParent :: (MonadIO m, IsWindow o) => o -> m Bool
getWindowDestroyWithParent obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "destroy-with-parent"

-- | Set the value of the “@destroy-with-parent@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' window [ #destroyWithParent 'Data.GI.Base.Attributes.:=' value ]
-- @
setWindowDestroyWithParent :: (MonadIO m, IsWindow o) => o -> Bool -> m ()
setWindowDestroyWithParent obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "destroy-with-parent" val

-- | Construct a `GValueConstruct` with valid value for the “@destroy-with-parent@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructWindowDestroyWithParent :: (IsWindow o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructWindowDestroyWithParent val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "destroy-with-parent" val

#if defined(ENABLE_OVERLOADING)
data WindowDestroyWithParentPropertyInfo
instance AttrInfo WindowDestroyWithParentPropertyInfo where
    type AttrAllowedOps WindowDestroyWithParentPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint WindowDestroyWithParentPropertyInfo = IsWindow
    type AttrSetTypeConstraint WindowDestroyWithParentPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint WindowDestroyWithParentPropertyInfo = (~) Bool
    type AttrTransferType WindowDestroyWithParentPropertyInfo = Bool
    type AttrGetType WindowDestroyWithParentPropertyInfo = Bool
    type AttrLabel WindowDestroyWithParentPropertyInfo = "destroy-with-parent"
    type AttrOrigin WindowDestroyWithParentPropertyInfo = Window
    attrGet = getWindowDestroyWithParent
    attrSet = setWindowDestroyWithParent
    attrTransfer _ v = do
        return v
    attrConstruct = constructWindowDestroyWithParent
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.destroyWithParent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:attr:destroyWithParent"
        })
#endif

-- VVV Prop "focus-on-map"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@focus-on-map@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' window #focusOnMap
-- @
getWindowFocusOnMap :: (MonadIO m, IsWindow o) => o -> m Bool
getWindowFocusOnMap obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "focus-on-map"

-- | Set the value of the “@focus-on-map@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' window [ #focusOnMap 'Data.GI.Base.Attributes.:=' value ]
-- @
setWindowFocusOnMap :: (MonadIO m, IsWindow o) => o -> Bool -> m ()
setWindowFocusOnMap obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "focus-on-map" val

-- | Construct a `GValueConstruct` with valid value for the “@focus-on-map@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructWindowFocusOnMap :: (IsWindow o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructWindowFocusOnMap val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "focus-on-map" val

#if defined(ENABLE_OVERLOADING)
data WindowFocusOnMapPropertyInfo
instance AttrInfo WindowFocusOnMapPropertyInfo where
    type AttrAllowedOps WindowFocusOnMapPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint WindowFocusOnMapPropertyInfo = IsWindow
    type AttrSetTypeConstraint WindowFocusOnMapPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint WindowFocusOnMapPropertyInfo = (~) Bool
    type AttrTransferType WindowFocusOnMapPropertyInfo = Bool
    type AttrGetType WindowFocusOnMapPropertyInfo = Bool
    type AttrLabel WindowFocusOnMapPropertyInfo = "focus-on-map"
    type AttrOrigin WindowFocusOnMapPropertyInfo = Window
    attrGet = getWindowFocusOnMap
    attrSet = setWindowFocusOnMap
    attrTransfer _ v = do
        return v
    attrConstruct = constructWindowFocusOnMap
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.focusOnMap"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:attr:focusOnMap"
        })
#endif

-- VVV Prop "focus-visible"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@focus-visible@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' window #focusVisible
-- @
getWindowFocusVisible :: (MonadIO m, IsWindow o) => o -> m Bool
getWindowFocusVisible obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "focus-visible"

-- | Set the value of the “@focus-visible@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' window [ #focusVisible 'Data.GI.Base.Attributes.:=' value ]
-- @
setWindowFocusVisible :: (MonadIO m, IsWindow o) => o -> Bool -> m ()
setWindowFocusVisible obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "focus-visible" val

-- | Construct a `GValueConstruct` with valid value for the “@focus-visible@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructWindowFocusVisible :: (IsWindow o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructWindowFocusVisible val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "focus-visible" val

#if defined(ENABLE_OVERLOADING)
data WindowFocusVisiblePropertyInfo
instance AttrInfo WindowFocusVisiblePropertyInfo where
    type AttrAllowedOps WindowFocusVisiblePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint WindowFocusVisiblePropertyInfo = IsWindow
    type AttrSetTypeConstraint WindowFocusVisiblePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint WindowFocusVisiblePropertyInfo = (~) Bool
    type AttrTransferType WindowFocusVisiblePropertyInfo = Bool
    type AttrGetType WindowFocusVisiblePropertyInfo = Bool
    type AttrLabel WindowFocusVisiblePropertyInfo = "focus-visible"
    type AttrOrigin WindowFocusVisiblePropertyInfo = Window
    attrGet = getWindowFocusVisible
    attrSet = setWindowFocusVisible
    attrTransfer _ v = do
        return v
    attrConstruct = constructWindowFocusVisible
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.focusVisible"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:attr:focusVisible"
        })
#endif

-- VVV Prop "gravity"
   -- Type: TInterface (Name {namespace = "Gdk", name = "Gravity"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@gravity@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' window #gravity
-- @
getWindowGravity :: (MonadIO m, IsWindow o) => o -> m Gdk.Enums.Gravity
getWindowGravity obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "gravity"

-- | Set the value of the “@gravity@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' window [ #gravity 'Data.GI.Base.Attributes.:=' value ]
-- @
setWindowGravity :: (MonadIO m, IsWindow o) => o -> Gdk.Enums.Gravity -> m ()
setWindowGravity obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "gravity" val

-- | Construct a `GValueConstruct` with valid value for the “@gravity@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructWindowGravity :: (IsWindow o, MIO.MonadIO m) => Gdk.Enums.Gravity -> m (GValueConstruct o)
constructWindowGravity val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "gravity" val

#if defined(ENABLE_OVERLOADING)
data WindowGravityPropertyInfo
instance AttrInfo WindowGravityPropertyInfo where
    type AttrAllowedOps WindowGravityPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint WindowGravityPropertyInfo = IsWindow
    type AttrSetTypeConstraint WindowGravityPropertyInfo = (~) Gdk.Enums.Gravity
    type AttrTransferTypeConstraint WindowGravityPropertyInfo = (~) Gdk.Enums.Gravity
    type AttrTransferType WindowGravityPropertyInfo = Gdk.Enums.Gravity
    type AttrGetType WindowGravityPropertyInfo = Gdk.Enums.Gravity
    type AttrLabel WindowGravityPropertyInfo = "gravity"
    type AttrOrigin WindowGravityPropertyInfo = Window
    attrGet = getWindowGravity
    attrSet = setWindowGravity
    attrTransfer _ v = do
        return v
    attrConstruct = constructWindowGravity
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.gravity"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:attr:gravity"
        })
#endif

-- VVV Prop "has-resize-grip"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@has-resize-grip@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' window #hasResizeGrip
-- @
getWindowHasResizeGrip :: (MonadIO m, IsWindow o) => o -> m Bool
getWindowHasResizeGrip obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "has-resize-grip"

-- | Set the value of the “@has-resize-grip@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' window [ #hasResizeGrip 'Data.GI.Base.Attributes.:=' value ]
-- @
setWindowHasResizeGrip :: (MonadIO m, IsWindow o) => o -> Bool -> m ()
setWindowHasResizeGrip obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "has-resize-grip" val

-- | Construct a `GValueConstruct` with valid value for the “@has-resize-grip@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructWindowHasResizeGrip :: (IsWindow o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructWindowHasResizeGrip val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "has-resize-grip" val

#if defined(ENABLE_OVERLOADING)
data WindowHasResizeGripPropertyInfo
instance AttrInfo WindowHasResizeGripPropertyInfo where
    type AttrAllowedOps WindowHasResizeGripPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint WindowHasResizeGripPropertyInfo = IsWindow
    type AttrSetTypeConstraint WindowHasResizeGripPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint WindowHasResizeGripPropertyInfo = (~) Bool
    type AttrTransferType WindowHasResizeGripPropertyInfo = Bool
    type AttrGetType WindowHasResizeGripPropertyInfo = Bool
    type AttrLabel WindowHasResizeGripPropertyInfo = "has-resize-grip"
    type AttrOrigin WindowHasResizeGripPropertyInfo = Window
    attrGet = getWindowHasResizeGrip
    attrSet = setWindowHasResizeGrip
    attrTransfer _ v = do
        return v
    attrConstruct = constructWindowHasResizeGrip
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.hasResizeGrip"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:attr:hasResizeGrip"
        })
#endif

-- VVV Prop "has-toplevel-focus"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@has-toplevel-focus@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' window #hasToplevelFocus
-- @
getWindowHasToplevelFocus :: (MonadIO m, IsWindow o) => o -> m Bool
getWindowHasToplevelFocus obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "has-toplevel-focus"

#if defined(ENABLE_OVERLOADING)
data WindowHasToplevelFocusPropertyInfo
instance AttrInfo WindowHasToplevelFocusPropertyInfo where
    type AttrAllowedOps WindowHasToplevelFocusPropertyInfo = '[ 'AttrGet]
    type AttrBaseTypeConstraint WindowHasToplevelFocusPropertyInfo = IsWindow
    type AttrSetTypeConstraint WindowHasToplevelFocusPropertyInfo = (~) ()
    type AttrTransferTypeConstraint WindowHasToplevelFocusPropertyInfo = (~) ()
    type AttrTransferType WindowHasToplevelFocusPropertyInfo = ()
    type AttrGetType WindowHasToplevelFocusPropertyInfo = Bool
    type AttrLabel WindowHasToplevelFocusPropertyInfo = "has-toplevel-focus"
    type AttrOrigin WindowHasToplevelFocusPropertyInfo = Window
    attrGet = getWindowHasToplevelFocus
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.hasToplevelFocus"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:attr:hasToplevelFocus"
        })
#endif

-- VVV Prop "hide-titlebar-when-maximized"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@hide-titlebar-when-maximized@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' window #hideTitlebarWhenMaximized
-- @
getWindowHideTitlebarWhenMaximized :: (MonadIO m, IsWindow o) => o -> m Bool
getWindowHideTitlebarWhenMaximized obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "hide-titlebar-when-maximized"

-- | Set the value of the “@hide-titlebar-when-maximized@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' window [ #hideTitlebarWhenMaximized 'Data.GI.Base.Attributes.:=' value ]
-- @
setWindowHideTitlebarWhenMaximized :: (MonadIO m, IsWindow o) => o -> Bool -> m ()
setWindowHideTitlebarWhenMaximized obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "hide-titlebar-when-maximized" val

-- | Construct a `GValueConstruct` with valid value for the “@hide-titlebar-when-maximized@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructWindowHideTitlebarWhenMaximized :: (IsWindow o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructWindowHideTitlebarWhenMaximized val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "hide-titlebar-when-maximized" val

#if defined(ENABLE_OVERLOADING)
data WindowHideTitlebarWhenMaximizedPropertyInfo
instance AttrInfo WindowHideTitlebarWhenMaximizedPropertyInfo where
    type AttrAllowedOps WindowHideTitlebarWhenMaximizedPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint WindowHideTitlebarWhenMaximizedPropertyInfo = IsWindow
    type AttrSetTypeConstraint WindowHideTitlebarWhenMaximizedPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint WindowHideTitlebarWhenMaximizedPropertyInfo = (~) Bool
    type AttrTransferType WindowHideTitlebarWhenMaximizedPropertyInfo = Bool
    type AttrGetType WindowHideTitlebarWhenMaximizedPropertyInfo = Bool
    type AttrLabel WindowHideTitlebarWhenMaximizedPropertyInfo = "hide-titlebar-when-maximized"
    type AttrOrigin WindowHideTitlebarWhenMaximizedPropertyInfo = Window
    attrGet = getWindowHideTitlebarWhenMaximized
    attrSet = setWindowHideTitlebarWhenMaximized
    attrTransfer _ v = do
        return v
    attrConstruct = constructWindowHideTitlebarWhenMaximized
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.hideTitlebarWhenMaximized"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:attr:hideTitlebarWhenMaximized"
        })
#endif

-- VVV Prop "icon"
   -- Type: TInterface (Name {namespace = "GdkPixbuf", name = "Pixbuf"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just True)

-- | Get the value of the “@icon@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' window #icon
-- @
getWindowIcon :: (MonadIO m, IsWindow o) => o -> m (Maybe GdkPixbuf.Pixbuf.Pixbuf)
getWindowIcon obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "icon" GdkPixbuf.Pixbuf.Pixbuf

-- | Set the value of the “@icon@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' window [ #icon 'Data.GI.Base.Attributes.:=' value ]
-- @
setWindowIcon :: (MonadIO m, IsWindow o, GdkPixbuf.Pixbuf.IsPixbuf a) => o -> a -> m ()
setWindowIcon obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "icon" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@icon@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructWindowIcon :: (IsWindow o, MIO.MonadIO m, GdkPixbuf.Pixbuf.IsPixbuf a) => a -> m (GValueConstruct o)
constructWindowIcon val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "icon" (P.Just val)

-- | Set the value of the “@icon@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #icon
-- @
clearWindowIcon :: (MonadIO m, IsWindow o) => o -> m ()
clearWindowIcon obj = liftIO $ B.Properties.setObjectPropertyObject obj "icon" (Nothing :: Maybe GdkPixbuf.Pixbuf.Pixbuf)

#if defined(ENABLE_OVERLOADING)
data WindowIconPropertyInfo
instance AttrInfo WindowIconPropertyInfo where
    type AttrAllowedOps WindowIconPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint WindowIconPropertyInfo = IsWindow
    type AttrSetTypeConstraint WindowIconPropertyInfo = GdkPixbuf.Pixbuf.IsPixbuf
    type AttrTransferTypeConstraint WindowIconPropertyInfo = GdkPixbuf.Pixbuf.IsPixbuf
    type AttrTransferType WindowIconPropertyInfo = GdkPixbuf.Pixbuf.Pixbuf
    type AttrGetType WindowIconPropertyInfo = (Maybe GdkPixbuf.Pixbuf.Pixbuf)
    type AttrLabel WindowIconPropertyInfo = "icon"
    type AttrOrigin WindowIconPropertyInfo = Window
    attrGet = getWindowIcon
    attrSet = setWindowIcon
    attrTransfer _ v = do
        unsafeCastTo GdkPixbuf.Pixbuf.Pixbuf v
    attrConstruct = constructWindowIcon
    attrClear = clearWindowIcon
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.icon"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:attr:icon"
        })
#endif

-- VVV Prop "icon-name"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just True)

-- | Get the value of the “@icon-name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' window #iconName
-- @
getWindowIconName :: (MonadIO m, IsWindow o) => o -> m (Maybe T.Text)
getWindowIconName obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "icon-name"

-- | Set the value of the “@icon-name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' window [ #iconName 'Data.GI.Base.Attributes.:=' value ]
-- @
setWindowIconName :: (MonadIO m, IsWindow o) => o -> T.Text -> m ()
setWindowIconName obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "icon-name" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@icon-name@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructWindowIconName :: (IsWindow o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructWindowIconName val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "icon-name" (P.Just val)

-- | Set the value of the “@icon-name@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #iconName
-- @
clearWindowIconName :: (MonadIO m, IsWindow o) => o -> m ()
clearWindowIconName obj = liftIO $ B.Properties.setObjectPropertyString obj "icon-name" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data WindowIconNamePropertyInfo
instance AttrInfo WindowIconNamePropertyInfo where
    type AttrAllowedOps WindowIconNamePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint WindowIconNamePropertyInfo = IsWindow
    type AttrSetTypeConstraint WindowIconNamePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint WindowIconNamePropertyInfo = (~) T.Text
    type AttrTransferType WindowIconNamePropertyInfo = T.Text
    type AttrGetType WindowIconNamePropertyInfo = (Maybe T.Text)
    type AttrLabel WindowIconNamePropertyInfo = "icon-name"
    type AttrOrigin WindowIconNamePropertyInfo = Window
    attrGet = getWindowIconName
    attrSet = setWindowIconName
    attrTransfer _ v = do
        return v
    attrConstruct = constructWindowIconName
    attrClear = clearWindowIconName
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.iconName"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:attr:iconName"
        })
#endif

-- VVV Prop "is-active"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@is-active@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' window #isActive
-- @
getWindowIsActive :: (MonadIO m, IsWindow o) => o -> m Bool
getWindowIsActive obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "is-active"

#if defined(ENABLE_OVERLOADING)
data WindowIsActivePropertyInfo
instance AttrInfo WindowIsActivePropertyInfo where
    type AttrAllowedOps WindowIsActivePropertyInfo = '[ 'AttrGet]
    type AttrBaseTypeConstraint WindowIsActivePropertyInfo = IsWindow
    type AttrSetTypeConstraint WindowIsActivePropertyInfo = (~) ()
    type AttrTransferTypeConstraint WindowIsActivePropertyInfo = (~) ()
    type AttrTransferType WindowIsActivePropertyInfo = ()
    type AttrGetType WindowIsActivePropertyInfo = Bool
    type AttrLabel WindowIsActivePropertyInfo = "is-active"
    type AttrOrigin WindowIsActivePropertyInfo = Window
    attrGet = getWindowIsActive
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.isActive"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:attr:isActive"
        })
#endif

-- VVV Prop "is-maximized"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@is-maximized@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' window #isMaximized
-- @
getWindowIsMaximized :: (MonadIO m, IsWindow o) => o -> m Bool
getWindowIsMaximized obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "is-maximized"

#if defined(ENABLE_OVERLOADING)
data WindowIsMaximizedPropertyInfo
instance AttrInfo WindowIsMaximizedPropertyInfo where
    type AttrAllowedOps WindowIsMaximizedPropertyInfo = '[ 'AttrGet]
    type AttrBaseTypeConstraint WindowIsMaximizedPropertyInfo = IsWindow
    type AttrSetTypeConstraint WindowIsMaximizedPropertyInfo = (~) ()
    type AttrTransferTypeConstraint WindowIsMaximizedPropertyInfo = (~) ()
    type AttrTransferType WindowIsMaximizedPropertyInfo = ()
    type AttrGetType WindowIsMaximizedPropertyInfo = Bool
    type AttrLabel WindowIsMaximizedPropertyInfo = "is-maximized"
    type AttrOrigin WindowIsMaximizedPropertyInfo = Window
    attrGet = getWindowIsMaximized
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.isMaximized"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:attr:isMaximized"
        })
#endif

-- VVV Prop "mnemonics-visible"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@mnemonics-visible@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' window #mnemonicsVisible
-- @
getWindowMnemonicsVisible :: (MonadIO m, IsWindow o) => o -> m Bool
getWindowMnemonicsVisible obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "mnemonics-visible"

-- | Set the value of the “@mnemonics-visible@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' window [ #mnemonicsVisible 'Data.GI.Base.Attributes.:=' value ]
-- @
setWindowMnemonicsVisible :: (MonadIO m, IsWindow o) => o -> Bool -> m ()
setWindowMnemonicsVisible obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "mnemonics-visible" val

-- | Construct a `GValueConstruct` with valid value for the “@mnemonics-visible@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructWindowMnemonicsVisible :: (IsWindow o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructWindowMnemonicsVisible val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "mnemonics-visible" val

#if defined(ENABLE_OVERLOADING)
data WindowMnemonicsVisiblePropertyInfo
instance AttrInfo WindowMnemonicsVisiblePropertyInfo where
    type AttrAllowedOps WindowMnemonicsVisiblePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint WindowMnemonicsVisiblePropertyInfo = IsWindow
    type AttrSetTypeConstraint WindowMnemonicsVisiblePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint WindowMnemonicsVisiblePropertyInfo = (~) Bool
    type AttrTransferType WindowMnemonicsVisiblePropertyInfo = Bool
    type AttrGetType WindowMnemonicsVisiblePropertyInfo = Bool
    type AttrLabel WindowMnemonicsVisiblePropertyInfo = "mnemonics-visible"
    type AttrOrigin WindowMnemonicsVisiblePropertyInfo = Window
    attrGet = getWindowMnemonicsVisible
    attrSet = setWindowMnemonicsVisible
    attrTransfer _ v = do
        return v
    attrConstruct = constructWindowMnemonicsVisible
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.mnemonicsVisible"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:attr:mnemonicsVisible"
        })
#endif

-- VVV Prop "modal"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@modal@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' window #modal
-- @
getWindowModal :: (MonadIO m, IsWindow o) => o -> m Bool
getWindowModal obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "modal"

-- | Set the value of the “@modal@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' window [ #modal 'Data.GI.Base.Attributes.:=' value ]
-- @
setWindowModal :: (MonadIO m, IsWindow o) => o -> Bool -> m ()
setWindowModal obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "modal" val

-- | Construct a `GValueConstruct` with valid value for the “@modal@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructWindowModal :: (IsWindow o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructWindowModal val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "modal" val

#if defined(ENABLE_OVERLOADING)
data WindowModalPropertyInfo
instance AttrInfo WindowModalPropertyInfo where
    type AttrAllowedOps WindowModalPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint WindowModalPropertyInfo = IsWindow
    type AttrSetTypeConstraint WindowModalPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint WindowModalPropertyInfo = (~) Bool
    type AttrTransferType WindowModalPropertyInfo = Bool
    type AttrGetType WindowModalPropertyInfo = Bool
    type AttrLabel WindowModalPropertyInfo = "modal"
    type AttrOrigin WindowModalPropertyInfo = Window
    attrGet = getWindowModal
    attrSet = setWindowModal
    attrTransfer _ v = do
        return v
    attrConstruct = constructWindowModal
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.modal"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:attr:modal"
        })
#endif

-- VVV Prop "resizable"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@resizable@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' window #resizable
-- @
getWindowResizable :: (MonadIO m, IsWindow o) => o -> m Bool
getWindowResizable obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "resizable"

-- | Set the value of the “@resizable@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' window [ #resizable 'Data.GI.Base.Attributes.:=' value ]
-- @
setWindowResizable :: (MonadIO m, IsWindow o) => o -> Bool -> m ()
setWindowResizable obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "resizable" val

-- | Construct a `GValueConstruct` with valid value for the “@resizable@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructWindowResizable :: (IsWindow o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructWindowResizable val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "resizable" val

#if defined(ENABLE_OVERLOADING)
data WindowResizablePropertyInfo
instance AttrInfo WindowResizablePropertyInfo where
    type AttrAllowedOps WindowResizablePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint WindowResizablePropertyInfo = IsWindow
    type AttrSetTypeConstraint WindowResizablePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint WindowResizablePropertyInfo = (~) Bool
    type AttrTransferType WindowResizablePropertyInfo = Bool
    type AttrGetType WindowResizablePropertyInfo = Bool
    type AttrLabel WindowResizablePropertyInfo = "resizable"
    type AttrOrigin WindowResizablePropertyInfo = Window
    attrGet = getWindowResizable
    attrSet = setWindowResizable
    attrTransfer _ v = do
        return v
    attrConstruct = constructWindowResizable
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.resizable"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:attr:resizable"
        })
#endif

-- VVV Prop "resize-grip-visible"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@resize-grip-visible@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' window #resizeGripVisible
-- @
getWindowResizeGripVisible :: (MonadIO m, IsWindow o) => o -> m Bool
getWindowResizeGripVisible obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "resize-grip-visible"

#if defined(ENABLE_OVERLOADING)
data WindowResizeGripVisiblePropertyInfo
instance AttrInfo WindowResizeGripVisiblePropertyInfo where
    type AttrAllowedOps WindowResizeGripVisiblePropertyInfo = '[ 'AttrGet]
    type AttrBaseTypeConstraint WindowResizeGripVisiblePropertyInfo = IsWindow
    type AttrSetTypeConstraint WindowResizeGripVisiblePropertyInfo = (~) ()
    type AttrTransferTypeConstraint WindowResizeGripVisiblePropertyInfo = (~) ()
    type AttrTransferType WindowResizeGripVisiblePropertyInfo = ()
    type AttrGetType WindowResizeGripVisiblePropertyInfo = Bool
    type AttrLabel WindowResizeGripVisiblePropertyInfo = "resize-grip-visible"
    type AttrOrigin WindowResizeGripVisiblePropertyInfo = Window
    attrGet = getWindowResizeGripVisible
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.resizeGripVisible"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:attr:resizeGripVisible"
        })
#endif

-- VVV Prop "role"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just False)

-- | Get the value of the “@role@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' window #role
-- @
getWindowRole :: (MonadIO m, IsWindow o) => o -> m (Maybe T.Text)
getWindowRole obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "role"

-- | Set the value of the “@role@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' window [ #role 'Data.GI.Base.Attributes.:=' value ]
-- @
setWindowRole :: (MonadIO m, IsWindow o) => o -> T.Text -> m ()
setWindowRole obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "role" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@role@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructWindowRole :: (IsWindow o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructWindowRole val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "role" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data WindowRolePropertyInfo
instance AttrInfo WindowRolePropertyInfo where
    type AttrAllowedOps WindowRolePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint WindowRolePropertyInfo = IsWindow
    type AttrSetTypeConstraint WindowRolePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint WindowRolePropertyInfo = (~) T.Text
    type AttrTransferType WindowRolePropertyInfo = T.Text
    type AttrGetType WindowRolePropertyInfo = (Maybe T.Text)
    type AttrLabel WindowRolePropertyInfo = "role"
    type AttrOrigin WindowRolePropertyInfo = Window
    attrGet = getWindowRole
    attrSet = setWindowRole
    attrTransfer _ v = do
        return v
    attrConstruct = constructWindowRole
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.role"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:attr:role"
        })
#endif

-- VVV Prop "screen"
   -- Type: TInterface (Name {namespace = "Gdk", name = "Screen"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@screen@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' window #screen
-- @
getWindowScreen :: (MonadIO m, IsWindow o) => o -> m Gdk.Screen.Screen
getWindowScreen obj = MIO.liftIO $ checkUnexpectedNothing "getWindowScreen" $ B.Properties.getObjectPropertyObject obj "screen" Gdk.Screen.Screen

-- | Set the value of the “@screen@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' window [ #screen 'Data.GI.Base.Attributes.:=' value ]
-- @
setWindowScreen :: (MonadIO m, IsWindow o, Gdk.Screen.IsScreen a) => o -> a -> m ()
setWindowScreen obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "screen" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@screen@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructWindowScreen :: (IsWindow o, MIO.MonadIO m, Gdk.Screen.IsScreen a) => a -> m (GValueConstruct o)
constructWindowScreen val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "screen" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data WindowScreenPropertyInfo
instance AttrInfo WindowScreenPropertyInfo where
    type AttrAllowedOps WindowScreenPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint WindowScreenPropertyInfo = IsWindow
    type AttrSetTypeConstraint WindowScreenPropertyInfo = Gdk.Screen.IsScreen
    type AttrTransferTypeConstraint WindowScreenPropertyInfo = Gdk.Screen.IsScreen
    type AttrTransferType WindowScreenPropertyInfo = Gdk.Screen.Screen
    type AttrGetType WindowScreenPropertyInfo = Gdk.Screen.Screen
    type AttrLabel WindowScreenPropertyInfo = "screen"
    type AttrOrigin WindowScreenPropertyInfo = Window
    attrGet = getWindowScreen
    attrSet = setWindowScreen
    attrTransfer _ v = do
        unsafeCastTo Gdk.Screen.Screen v
    attrConstruct = constructWindowScreen
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.screen"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:attr:screen"
        })
#endif

-- VVV Prop "skip-pager-hint"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@skip-pager-hint@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' window #skipPagerHint
-- @
getWindowSkipPagerHint :: (MonadIO m, IsWindow o) => o -> m Bool
getWindowSkipPagerHint obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "skip-pager-hint"

-- | Set the value of the “@skip-pager-hint@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' window [ #skipPagerHint 'Data.GI.Base.Attributes.:=' value ]
-- @
setWindowSkipPagerHint :: (MonadIO m, IsWindow o) => o -> Bool -> m ()
setWindowSkipPagerHint obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "skip-pager-hint" val

-- | Construct a `GValueConstruct` with valid value for the “@skip-pager-hint@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructWindowSkipPagerHint :: (IsWindow o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructWindowSkipPagerHint val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "skip-pager-hint" val

#if defined(ENABLE_OVERLOADING)
data WindowSkipPagerHintPropertyInfo
instance AttrInfo WindowSkipPagerHintPropertyInfo where
    type AttrAllowedOps WindowSkipPagerHintPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint WindowSkipPagerHintPropertyInfo = IsWindow
    type AttrSetTypeConstraint WindowSkipPagerHintPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint WindowSkipPagerHintPropertyInfo = (~) Bool
    type AttrTransferType WindowSkipPagerHintPropertyInfo = Bool
    type AttrGetType WindowSkipPagerHintPropertyInfo = Bool
    type AttrLabel WindowSkipPagerHintPropertyInfo = "skip-pager-hint"
    type AttrOrigin WindowSkipPagerHintPropertyInfo = Window
    attrGet = getWindowSkipPagerHint
    attrSet = setWindowSkipPagerHint
    attrTransfer _ v = do
        return v
    attrConstruct = constructWindowSkipPagerHint
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.skipPagerHint"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:attr:skipPagerHint"
        })
#endif

-- VVV Prop "skip-taskbar-hint"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@skip-taskbar-hint@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' window #skipTaskbarHint
-- @
getWindowSkipTaskbarHint :: (MonadIO m, IsWindow o) => o -> m Bool
getWindowSkipTaskbarHint obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "skip-taskbar-hint"

-- | Set the value of the “@skip-taskbar-hint@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' window [ #skipTaskbarHint 'Data.GI.Base.Attributes.:=' value ]
-- @
setWindowSkipTaskbarHint :: (MonadIO m, IsWindow o) => o -> Bool -> m ()
setWindowSkipTaskbarHint obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "skip-taskbar-hint" val

-- | Construct a `GValueConstruct` with valid value for the “@skip-taskbar-hint@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructWindowSkipTaskbarHint :: (IsWindow o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructWindowSkipTaskbarHint val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "skip-taskbar-hint" val

#if defined(ENABLE_OVERLOADING)
data WindowSkipTaskbarHintPropertyInfo
instance AttrInfo WindowSkipTaskbarHintPropertyInfo where
    type AttrAllowedOps WindowSkipTaskbarHintPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint WindowSkipTaskbarHintPropertyInfo = IsWindow
    type AttrSetTypeConstraint WindowSkipTaskbarHintPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint WindowSkipTaskbarHintPropertyInfo = (~) Bool
    type AttrTransferType WindowSkipTaskbarHintPropertyInfo = Bool
    type AttrGetType WindowSkipTaskbarHintPropertyInfo = Bool
    type AttrLabel WindowSkipTaskbarHintPropertyInfo = "skip-taskbar-hint"
    type AttrOrigin WindowSkipTaskbarHintPropertyInfo = Window
    attrGet = getWindowSkipTaskbarHint
    attrSet = setWindowSkipTaskbarHint
    attrTransfer _ v = do
        return v
    attrConstruct = constructWindowSkipTaskbarHint
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.skipTaskbarHint"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:attr:skipTaskbarHint"
        })
#endif

-- VVV Prop "startup-id"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyWritable]
   -- Nullable: (Nothing,Just False)

-- | Set the value of the “@startup-id@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' window [ #startupId 'Data.GI.Base.Attributes.:=' value ]
-- @
setWindowStartupId :: (MonadIO m, IsWindow o) => o -> T.Text -> m ()
setWindowStartupId obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "startup-id" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@startup-id@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructWindowStartupId :: (IsWindow o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructWindowStartupId val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "startup-id" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data WindowStartupIdPropertyInfo
instance AttrInfo WindowStartupIdPropertyInfo where
    type AttrAllowedOps WindowStartupIdPropertyInfo = '[ 'AttrSet, 'AttrConstruct]
    type AttrBaseTypeConstraint WindowStartupIdPropertyInfo = IsWindow
    type AttrSetTypeConstraint WindowStartupIdPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint WindowStartupIdPropertyInfo = (~) T.Text
    type AttrTransferType WindowStartupIdPropertyInfo = T.Text
    type AttrGetType WindowStartupIdPropertyInfo = ()
    type AttrLabel WindowStartupIdPropertyInfo = "startup-id"
    type AttrOrigin WindowStartupIdPropertyInfo = Window
    attrGet = undefined
    attrSet = setWindowStartupId
    attrTransfer _ v = do
        return v
    attrConstruct = constructWindowStartupId
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.startupId"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:attr:startupId"
        })
#endif

-- VVV Prop "title"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just False)

-- | Get the value of the “@title@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' window #title
-- @
getWindowTitle :: (MonadIO m, IsWindow o) => o -> m (Maybe T.Text)
getWindowTitle obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "title"

-- | Set the value of the “@title@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' window [ #title 'Data.GI.Base.Attributes.:=' value ]
-- @
setWindowTitle :: (MonadIO m, IsWindow o) => o -> T.Text -> m ()
setWindowTitle obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "title" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@title@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructWindowTitle :: (IsWindow o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructWindowTitle val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "title" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data WindowTitlePropertyInfo
instance AttrInfo WindowTitlePropertyInfo where
    type AttrAllowedOps WindowTitlePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint WindowTitlePropertyInfo = IsWindow
    type AttrSetTypeConstraint WindowTitlePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint WindowTitlePropertyInfo = (~) T.Text
    type AttrTransferType WindowTitlePropertyInfo = T.Text
    type AttrGetType WindowTitlePropertyInfo = (Maybe T.Text)
    type AttrLabel WindowTitlePropertyInfo = "title"
    type AttrOrigin WindowTitlePropertyInfo = Window
    attrGet = getWindowTitle
    attrSet = setWindowTitle
    attrTransfer _ v = do
        return v
    attrConstruct = constructWindowTitle
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.title"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:attr:title"
        })
#endif

-- VVV Prop "transient-for"
   -- Type: TInterface (Name {namespace = "Gtk", name = "Window"})
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstruct]
   -- Nullable: (Just True,Just True)

-- | Get the value of the “@transient-for@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' window #transientFor
-- @
getWindowTransientFor :: (MonadIO m, IsWindow o) => o -> m (Maybe Window)
getWindowTransientFor obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "transient-for" Window

-- | Set the value of the “@transient-for@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' window [ #transientFor 'Data.GI.Base.Attributes.:=' value ]
-- @
setWindowTransientFor :: (MonadIO m, IsWindow o, IsWindow a) => o -> a -> m ()
setWindowTransientFor obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "transient-for" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@transient-for@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructWindowTransientFor :: (IsWindow o, MIO.MonadIO m, IsWindow a) => a -> m (GValueConstruct o)
constructWindowTransientFor val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "transient-for" (P.Just val)

-- | Set the value of the “@transient-for@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #transientFor
-- @
clearWindowTransientFor :: (MonadIO m, IsWindow o) => o -> m ()
clearWindowTransientFor obj = liftIO $ B.Properties.setObjectPropertyObject obj "transient-for" (Nothing :: Maybe Window)

#if defined(ENABLE_OVERLOADING)
data WindowTransientForPropertyInfo
instance AttrInfo WindowTransientForPropertyInfo where
    type AttrAllowedOps WindowTransientForPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint WindowTransientForPropertyInfo = IsWindow
    type AttrSetTypeConstraint WindowTransientForPropertyInfo = IsWindow
    type AttrTransferTypeConstraint WindowTransientForPropertyInfo = IsWindow
    type AttrTransferType WindowTransientForPropertyInfo = Window
    type AttrGetType WindowTransientForPropertyInfo = (Maybe Window)
    type AttrLabel WindowTransientForPropertyInfo = "transient-for"
    type AttrOrigin WindowTransientForPropertyInfo = Window
    attrGet = getWindowTransientFor
    attrSet = setWindowTransientFor
    attrTransfer _ v = do
        unsafeCastTo Window v
    attrConstruct = constructWindowTransientFor
    attrClear = clearWindowTransientFor
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.transientFor"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:attr:transientFor"
        })
#endif

-- VVV Prop "type"
   -- Type: TInterface (Name {namespace = "Gtk", name = "WindowType"})
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstructOnly]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@type@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' window #type
-- @
getWindowType :: (MonadIO m, IsWindow o) => o -> m Gtk.Enums.WindowType
getWindowType obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "type"

-- | Construct a `GValueConstruct` with valid value for the “@type@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructWindowType :: (IsWindow o, MIO.MonadIO m) => Gtk.Enums.WindowType -> m (GValueConstruct o)
constructWindowType val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "type" val

#if defined(ENABLE_OVERLOADING)
data WindowTypePropertyInfo
instance AttrInfo WindowTypePropertyInfo where
    type AttrAllowedOps WindowTypePropertyInfo = '[ 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint WindowTypePropertyInfo = IsWindow
    type AttrSetTypeConstraint WindowTypePropertyInfo = (~) Gtk.Enums.WindowType
    type AttrTransferTypeConstraint WindowTypePropertyInfo = (~) Gtk.Enums.WindowType
    type AttrTransferType WindowTypePropertyInfo = Gtk.Enums.WindowType
    type AttrGetType WindowTypePropertyInfo = Gtk.Enums.WindowType
    type AttrLabel WindowTypePropertyInfo = "type"
    type AttrOrigin WindowTypePropertyInfo = Window
    attrGet = getWindowType
    attrSet = undefined
    attrTransfer _ v = do
        return v
    attrConstruct = constructWindowType
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.type"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:attr:type"
        })
#endif

-- VVV Prop "type-hint"
   -- Type: TInterface (Name {namespace = "Gdk", name = "WindowTypeHint"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@type-hint@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' window #typeHint
-- @
getWindowTypeHint :: (MonadIO m, IsWindow o) => o -> m Gdk.Enums.WindowTypeHint
getWindowTypeHint obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "type-hint"

-- | Set the value of the “@type-hint@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' window [ #typeHint 'Data.GI.Base.Attributes.:=' value ]
-- @
setWindowTypeHint :: (MonadIO m, IsWindow o) => o -> Gdk.Enums.WindowTypeHint -> m ()
setWindowTypeHint obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "type-hint" val

-- | Construct a `GValueConstruct` with valid value for the “@type-hint@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructWindowTypeHint :: (IsWindow o, MIO.MonadIO m) => Gdk.Enums.WindowTypeHint -> m (GValueConstruct o)
constructWindowTypeHint val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "type-hint" val

#if defined(ENABLE_OVERLOADING)
data WindowTypeHintPropertyInfo
instance AttrInfo WindowTypeHintPropertyInfo where
    type AttrAllowedOps WindowTypeHintPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint WindowTypeHintPropertyInfo = IsWindow
    type AttrSetTypeConstraint WindowTypeHintPropertyInfo = (~) Gdk.Enums.WindowTypeHint
    type AttrTransferTypeConstraint WindowTypeHintPropertyInfo = (~) Gdk.Enums.WindowTypeHint
    type AttrTransferType WindowTypeHintPropertyInfo = Gdk.Enums.WindowTypeHint
    type AttrGetType WindowTypeHintPropertyInfo = Gdk.Enums.WindowTypeHint
    type AttrLabel WindowTypeHintPropertyInfo = "type-hint"
    type AttrOrigin WindowTypeHintPropertyInfo = Window
    attrGet = getWindowTypeHint
    attrSet = setWindowTypeHint
    attrTransfer _ v = do
        return v
    attrConstruct = constructWindowTypeHint
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.typeHint"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:attr:typeHint"
        })
#endif

-- VVV Prop "urgency-hint"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@urgency-hint@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' window #urgencyHint
-- @
getWindowUrgencyHint :: (MonadIO m, IsWindow o) => o -> m Bool
getWindowUrgencyHint obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "urgency-hint"

-- | Set the value of the “@urgency-hint@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' window [ #urgencyHint 'Data.GI.Base.Attributes.:=' value ]
-- @
setWindowUrgencyHint :: (MonadIO m, IsWindow o) => o -> Bool -> m ()
setWindowUrgencyHint obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "urgency-hint" val

-- | Construct a `GValueConstruct` with valid value for the “@urgency-hint@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructWindowUrgencyHint :: (IsWindow o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructWindowUrgencyHint val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "urgency-hint" val

#if defined(ENABLE_OVERLOADING)
data WindowUrgencyHintPropertyInfo
instance AttrInfo WindowUrgencyHintPropertyInfo where
    type AttrAllowedOps WindowUrgencyHintPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint WindowUrgencyHintPropertyInfo = IsWindow
    type AttrSetTypeConstraint WindowUrgencyHintPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint WindowUrgencyHintPropertyInfo = (~) Bool
    type AttrTransferType WindowUrgencyHintPropertyInfo = Bool
    type AttrGetType WindowUrgencyHintPropertyInfo = Bool
    type AttrLabel WindowUrgencyHintPropertyInfo = "urgency-hint"
    type AttrOrigin WindowUrgencyHintPropertyInfo = Window
    attrGet = getWindowUrgencyHint
    attrSet = setWindowUrgencyHint
    attrTransfer _ v = do
        return v
    attrConstruct = constructWindowUrgencyHint
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.urgencyHint"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:attr:urgencyHint"
        })
#endif

-- VVV Prop "window-position"
   -- Type: TInterface (Name {namespace = "Gtk", name = "WindowPosition"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@window-position@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' window #windowPosition
-- @
getWindowWindowPosition :: (MonadIO m, IsWindow o) => o -> m Gtk.Enums.WindowPosition
getWindowWindowPosition obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "window-position"

-- | Set the value of the “@window-position@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' window [ #windowPosition 'Data.GI.Base.Attributes.:=' value ]
-- @
setWindowWindowPosition :: (MonadIO m, IsWindow o) => o -> Gtk.Enums.WindowPosition -> m ()
setWindowWindowPosition obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "window-position" val

-- | Construct a `GValueConstruct` with valid value for the “@window-position@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructWindowWindowPosition :: (IsWindow o, MIO.MonadIO m) => Gtk.Enums.WindowPosition -> m (GValueConstruct o)
constructWindowWindowPosition val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "window-position" val

#if defined(ENABLE_OVERLOADING)
data WindowWindowPositionPropertyInfo
instance AttrInfo WindowWindowPositionPropertyInfo where
    type AttrAllowedOps WindowWindowPositionPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint WindowWindowPositionPropertyInfo = IsWindow
    type AttrSetTypeConstraint WindowWindowPositionPropertyInfo = (~) Gtk.Enums.WindowPosition
    type AttrTransferTypeConstraint WindowWindowPositionPropertyInfo = (~) Gtk.Enums.WindowPosition
    type AttrTransferType WindowWindowPositionPropertyInfo = Gtk.Enums.WindowPosition
    type AttrGetType WindowWindowPositionPropertyInfo = Gtk.Enums.WindowPosition
    type AttrLabel WindowWindowPositionPropertyInfo = "window-position"
    type AttrOrigin WindowWindowPositionPropertyInfo = Window
    attrGet = getWindowWindowPosition
    attrSet = setWindowWindowPosition
    attrTransfer _ v = do
        return v
    attrConstruct = constructWindowWindowPosition
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowPosition"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#g:attr:windowPosition"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList Window
type instance O.AttributeList Window = WindowAttributeList
type WindowAttributeList = ('[ '("acceptFocus", WindowAcceptFocusPropertyInfo), '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("application", WindowApplicationPropertyInfo), '("attachedTo", WindowAttachedToPropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("decorated", WindowDecoratedPropertyInfo), '("defaultHeight", WindowDefaultHeightPropertyInfo), '("defaultWidth", WindowDefaultWidthPropertyInfo), '("deletable", WindowDeletablePropertyInfo), '("destroyWithParent", WindowDestroyWithParentPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("focusOnMap", WindowFocusOnMapPropertyInfo), '("focusVisible", WindowFocusVisiblePropertyInfo), '("gravity", WindowGravityPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasResizeGrip", WindowHasResizeGripPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("hasToplevelFocus", WindowHasToplevelFocusPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("hideTitlebarWhenMaximized", WindowHideTitlebarWhenMaximizedPropertyInfo), '("icon", WindowIconPropertyInfo), '("iconName", WindowIconNamePropertyInfo), '("isActive", WindowIsActivePropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("isMaximized", WindowIsMaximizedPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("mnemonicsVisible", WindowMnemonicsVisiblePropertyInfo), '("modal", WindowModalPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("resizable", WindowResizablePropertyInfo), '("resizeGripVisible", WindowResizeGripVisiblePropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("role", WindowRolePropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("screen", WindowScreenPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("skipPagerHint", WindowSkipPagerHintPropertyInfo), '("skipTaskbarHint", WindowSkipTaskbarHintPropertyInfo), '("startupId", WindowStartupIdPropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("title", WindowTitlePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("transientFor", WindowTransientForPropertyInfo), '("type", WindowTypePropertyInfo), '("typeHint", WindowTypeHintPropertyInfo), '("urgencyHint", WindowUrgencyHintPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo), '("windowPosition", WindowWindowPositionPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
windowAcceptFocus :: AttrLabelProxy "acceptFocus"
windowAcceptFocus = AttrLabelProxy

windowApplication :: AttrLabelProxy "application"
windowApplication = AttrLabelProxy

windowAttachedTo :: AttrLabelProxy "attachedTo"
windowAttachedTo = AttrLabelProxy

windowDecorated :: AttrLabelProxy "decorated"
windowDecorated = AttrLabelProxy

windowDefaultHeight :: AttrLabelProxy "defaultHeight"
windowDefaultHeight = AttrLabelProxy

windowDefaultWidth :: AttrLabelProxy "defaultWidth"
windowDefaultWidth = AttrLabelProxy

windowDeletable :: AttrLabelProxy "deletable"
windowDeletable = AttrLabelProxy

windowDestroyWithParent :: AttrLabelProxy "destroyWithParent"
windowDestroyWithParent = AttrLabelProxy

windowFocusOnMap :: AttrLabelProxy "focusOnMap"
windowFocusOnMap = AttrLabelProxy

windowFocusVisible :: AttrLabelProxy "focusVisible"
windowFocusVisible = AttrLabelProxy

windowGravity :: AttrLabelProxy "gravity"
windowGravity = AttrLabelProxy

windowHasResizeGrip :: AttrLabelProxy "hasResizeGrip"
windowHasResizeGrip = AttrLabelProxy

windowHideTitlebarWhenMaximized :: AttrLabelProxy "hideTitlebarWhenMaximized"
windowHideTitlebarWhenMaximized = AttrLabelProxy

windowIcon :: AttrLabelProxy "icon"
windowIcon = AttrLabelProxy

windowIconName :: AttrLabelProxy "iconName"
windowIconName = AttrLabelProxy

windowMnemonicsVisible :: AttrLabelProxy "mnemonicsVisible"
windowMnemonicsVisible = AttrLabelProxy

windowModal :: AttrLabelProxy "modal"
windowModal = AttrLabelProxy

windowResizable :: AttrLabelProxy "resizable"
windowResizable = AttrLabelProxy

windowResizeGripVisible :: AttrLabelProxy "resizeGripVisible"
windowResizeGripVisible = AttrLabelProxy

windowRole :: AttrLabelProxy "role"
windowRole = AttrLabelProxy

windowScreen :: AttrLabelProxy "screen"
windowScreen = AttrLabelProxy

windowSkipPagerHint :: AttrLabelProxy "skipPagerHint"
windowSkipPagerHint = AttrLabelProxy

windowSkipTaskbarHint :: AttrLabelProxy "skipTaskbarHint"
windowSkipTaskbarHint = AttrLabelProxy

windowStartupId :: AttrLabelProxy "startupId"
windowStartupId = AttrLabelProxy

windowTitle :: AttrLabelProxy "title"
windowTitle = AttrLabelProxy

windowTransientFor :: AttrLabelProxy "transientFor"
windowTransientFor = AttrLabelProxy

windowType :: AttrLabelProxy "type"
windowType = AttrLabelProxy

windowTypeHint :: AttrLabelProxy "typeHint"
windowTypeHint = AttrLabelProxy

windowUrgencyHint :: AttrLabelProxy "urgencyHint"
windowUrgencyHint = AttrLabelProxy

windowWindowPosition :: AttrLabelProxy "windowPosition"
windowWindowPosition = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList Window = WindowSignalList
type WindowSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("activateDefault", WindowActivateDefaultSignalInfo), '("activateFocus", WindowActivateFocusSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enableDebugging", WindowEnableDebuggingSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("keysChanged", WindowKeysChangedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocus", WindowSetFocusSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method Window::new
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WindowType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "type of window" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Window" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_window_new" gtk_window_new :: 
    CUInt ->                                -- type : TInterface (Name {namespace = "Gtk", name = "WindowType"})
    IO (Ptr Window)

-- | Creates a new t'GI.Gtk.Objects.Window.Window', which is a toplevel window that can
-- contain other widgets. Nearly always, the type of the window should
-- be @/GTK_WINDOW_TOPLEVEL/@. If you’re implementing something like a
-- popup menu from scratch (which is a bad idea, just use t'GI.Gtk.Objects.Menu.Menu'),
-- you might use @/GTK_WINDOW_POPUP/@. @/GTK_WINDOW_POPUP/@ is not for
-- dialogs, though in some other toolkits dialogs are called “popups”.
-- In GTK+, @/GTK_WINDOW_POPUP/@ means a pop-up menu or pop-up tooltip.
-- On X11, popup windows are not controlled by the
-- [window manager][gtk-X11-arch].
-- 
-- If you simply want an undecorated window (no window borders), use
-- 'GI.Gtk.Objects.Window.windowSetDecorated', don’t use @/GTK_WINDOW_POPUP/@.
-- 
-- All top-level windows created by 'GI.Gtk.Objects.Window.windowNew' are stored in
-- an internal top-level window list.  This list can be obtained from
-- 'GI.Gtk.Objects.Window.windowListToplevels'.  Due to Gtk+ keeping a reference to
-- the window internally, 'GI.Gtk.Objects.Window.windowNew' does not return a reference
-- to the caller.
-- 
-- To delete a t'GI.Gtk.Objects.Window.Window', call 'GI.Gtk.Objects.Widget.widgetDestroy'.
windowNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Gtk.Enums.WindowType
    -- ^ /@type@/: type of window
    -> m Window
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.Window.Window'.
windowNew type_ = liftIO $ do
    let type_' = (fromIntegral . fromEnum) type_
    result <- gtk_window_new type_'
    checkUnexpectedReturnNULL "windowNew" result
    result' <- (newObject Window) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Window::activate_default
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_activate_default" gtk_window_activate_default :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO CInt

-- | Activates the default widget for the window, unless the current
-- focused widget has been configured to receive the default action
-- (see 'GI.Gtk.Objects.Widget.widgetSetReceivesDefault'), in which case the
-- focused widget is activated.
windowActivateDefault ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if a widget got activated.
windowActivateDefault window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_activate_default window'
    let result' = (/= 0) result
    touchManagedPtr window
    return result'

#if defined(ENABLE_OVERLOADING)
data WindowActivateDefaultMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsWindow a) => O.OverloadedMethod WindowActivateDefaultMethodInfo a signature where
    overloadedMethod = windowActivateDefault

instance O.OverloadedMethodInfo WindowActivateDefaultMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowActivateDefault",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowActivateDefault"
        })


#endif

-- method Window::activate_focus
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_activate_focus" gtk_window_activate_focus :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO CInt

-- | Activates the current focused widget within the window.
windowActivateFocus ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if a widget got activated.
windowActivateFocus window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_activate_focus window'
    let result' = (/= 0) result
    touchManagedPtr window
    return result'

#if defined(ENABLE_OVERLOADING)
data WindowActivateFocusMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsWindow a) => O.OverloadedMethod WindowActivateFocusMethodInfo a signature where
    overloadedMethod = windowActivateFocus

instance O.OverloadedMethodInfo WindowActivateFocusMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowActivateFocus",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowActivateFocus"
        })


#endif

-- method Window::activate_key
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "event"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "EventKey" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkEventKey" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_activate_key" gtk_window_activate_key :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    Ptr Gdk.EventKey.EventKey ->            -- event : TInterface (Name {namespace = "Gdk", name = "EventKey"})
    IO CInt

-- | Activates mnemonics and accelerators for this t'GI.Gtk.Objects.Window.Window'. This is normally
-- called by the default [key_press_event](#g:signal:key_press_event) handler for toplevel windows,
-- however in some cases it may be useful to call this directly when
-- overriding the standard key handling for a toplevel window.
-- 
-- /Since: 2.4/
windowActivateKey ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Gdk.EventKey.EventKey
    -- ^ /@event@/: a t'GI.Gdk.Structs.EventKey.EventKey'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if a mnemonic or accelerator was found and activated.
windowActivateKey window event = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    event' <- unsafeManagedPtrGetPtr event
    result <- gtk_window_activate_key window' event'
    let result' = (/= 0) result
    touchManagedPtr window
    touchManagedPtr event
    return result'

#if defined(ENABLE_OVERLOADING)
data WindowActivateKeyMethodInfo
instance (signature ~ (Gdk.EventKey.EventKey -> m Bool), MonadIO m, IsWindow a) => O.OverloadedMethod WindowActivateKeyMethodInfo a signature where
    overloadedMethod = windowActivateKey

instance O.OverloadedMethodInfo WindowActivateKeyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowActivateKey",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowActivateKey"
        })


#endif

-- method Window::add_accel_group
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "window to attach accelerator group to"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "accel_group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AccelGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAccelGroup" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_add_accel_group" gtk_window_add_accel_group :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    Ptr Gtk.AccelGroup.AccelGroup ->        -- accel_group : TInterface (Name {namespace = "Gtk", name = "AccelGroup"})
    IO ()

-- | Associate /@accelGroup@/ with /@window@/, such that calling
-- 'GI.Gtk.Functions.accelGroupsActivate' on /@window@/ will activate accelerators
-- in /@accelGroup@/.
windowAddAccelGroup ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a, Gtk.AccelGroup.IsAccelGroup b) =>
    a
    -- ^ /@window@/: window to attach accelerator group to
    -> b
    -- ^ /@accelGroup@/: a t'GI.Gtk.Objects.AccelGroup.AccelGroup'
    -> m ()
windowAddAccelGroup window accelGroup = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    accelGroup' <- unsafeManagedPtrCastPtr accelGroup
    gtk_window_add_accel_group window' accelGroup'
    touchManagedPtr window
    touchManagedPtr accelGroup
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowAddAccelGroupMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsWindow a, Gtk.AccelGroup.IsAccelGroup b) => O.OverloadedMethod WindowAddAccelGroupMethodInfo a signature where
    overloadedMethod = windowAddAccelGroup

instance O.OverloadedMethodInfo WindowAddAccelGroupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowAddAccelGroup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowAddAccelGroup"
        })


#endif

-- method Window::add_mnemonic
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "keyval"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the mnemonic" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "target"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the widget that gets activated by the mnemonic"
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

foreign import ccall "gtk_window_add_mnemonic" gtk_window_add_mnemonic :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    Word32 ->                               -- keyval : TBasicType TUInt
    Ptr Gtk.Widget.Widget ->                -- target : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | Adds a mnemonic to this window.
windowAddMnemonic ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Word32
    -- ^ /@keyval@/: the mnemonic
    -> b
    -- ^ /@target@/: the widget that gets activated by the mnemonic
    -> m ()
windowAddMnemonic window keyval target = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    target' <- unsafeManagedPtrCastPtr target
    gtk_window_add_mnemonic window' keyval target'
    touchManagedPtr window
    touchManagedPtr target
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowAddMnemonicMethodInfo
instance (signature ~ (Word32 -> b -> m ()), MonadIO m, IsWindow a, Gtk.Widget.IsWidget b) => O.OverloadedMethod WindowAddMnemonicMethodInfo a signature where
    overloadedMethod = windowAddMnemonic

instance O.OverloadedMethodInfo WindowAddMnemonicMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowAddMnemonic",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowAddMnemonic"
        })


#endif

-- method Window::begin_move_drag
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "button"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "mouse button that initiated the drag"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "root_x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "X position where the user clicked to initiate the drag, in root window coordinates"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "root_y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "Y position where the user clicked to initiate the drag"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "timestamp"
--           , argType = TBasicType TUInt32
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "timestamp from the click event that initiated the drag"
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

foreign import ccall "gtk_window_begin_move_drag" gtk_window_begin_move_drag :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    Int32 ->                                -- button : TBasicType TInt
    Int32 ->                                -- root_x : TBasicType TInt
    Int32 ->                                -- root_y : TBasicType TInt
    Word32 ->                               -- timestamp : TBasicType TUInt32
    IO ()

-- | Starts moving a window. This function is used if an application has
-- window movement grips. When GDK can support it, the window movement
-- will be done using the standard mechanism for the
-- [window manager][gtk-X11-arch] or windowing
-- system. Otherwise, GDK will try to emulate window movement,
-- potentially not all that well, depending on the windowing system.
windowBeginMoveDrag ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Int32
    -- ^ /@button@/: mouse button that initiated the drag
    -> Int32
    -- ^ /@rootX@/: X position where the user clicked to initiate the drag, in root window coordinates
    -> Int32
    -- ^ /@rootY@/: Y position where the user clicked to initiate the drag
    -> Word32
    -- ^ /@timestamp@/: timestamp from the click event that initiated the drag
    -> m ()
windowBeginMoveDrag window button rootX rootY timestamp = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    gtk_window_begin_move_drag window' button rootX rootY timestamp
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowBeginMoveDragMethodInfo
instance (signature ~ (Int32 -> Int32 -> Int32 -> Word32 -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowBeginMoveDragMethodInfo a signature where
    overloadedMethod = windowBeginMoveDrag

instance O.OverloadedMethodInfo WindowBeginMoveDragMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowBeginMoveDrag",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowBeginMoveDrag"
        })


#endif

-- method Window::begin_resize_drag
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "edge"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "WindowEdge" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "position of the resize control"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "button"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "mouse button that initiated the drag"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "root_x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "X position where the user clicked to initiate the drag, in root window coordinates"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "root_y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "Y position where the user clicked to initiate the drag"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "timestamp"
--           , argType = TBasicType TUInt32
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "timestamp from the click event that initiated the drag"
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

foreign import ccall "gtk_window_begin_resize_drag" gtk_window_begin_resize_drag :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    CUInt ->                                -- edge : TInterface (Name {namespace = "Gdk", name = "WindowEdge"})
    Int32 ->                                -- button : TBasicType TInt
    Int32 ->                                -- root_x : TBasicType TInt
    Int32 ->                                -- root_y : TBasicType TInt
    Word32 ->                               -- timestamp : TBasicType TUInt32
    IO ()

-- | Starts resizing a window. This function is used if an application
-- has window resizing controls. When GDK can support it, the resize
-- will be done using the standard mechanism for the
-- [window manager][gtk-X11-arch] or windowing
-- system. Otherwise, GDK will try to emulate window resizing,
-- potentially not all that well, depending on the windowing system.
windowBeginResizeDrag ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Gdk.Enums.WindowEdge
    -- ^ /@edge@/: position of the resize control
    -> Int32
    -- ^ /@button@/: mouse button that initiated the drag
    -> Int32
    -- ^ /@rootX@/: X position where the user clicked to initiate the drag, in root window coordinates
    -> Int32
    -- ^ /@rootY@/: Y position where the user clicked to initiate the drag
    -> Word32
    -- ^ /@timestamp@/: timestamp from the click event that initiated the drag
    -> m ()
windowBeginResizeDrag window edge button rootX rootY timestamp = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    let edge' = (fromIntegral . fromEnum) edge
    gtk_window_begin_resize_drag window' edge' button rootX rootY timestamp
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowBeginResizeDragMethodInfo
instance (signature ~ (Gdk.Enums.WindowEdge -> Int32 -> Int32 -> Int32 -> Word32 -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowBeginResizeDragMethodInfo a signature where
    overloadedMethod = windowBeginResizeDrag

instance O.OverloadedMethodInfo WindowBeginResizeDragMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowBeginResizeDrag",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowBeginResizeDrag"
        })


#endif

-- method Window::close
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_close" gtk_window_close :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO ()

-- | Requests that the window is closed, similar to what happens
-- when a window manager close button is clicked.
-- 
-- This function can be used with close buttons in custom
-- titlebars.
-- 
-- /Since: 3.10/
windowClose ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m ()
windowClose window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    gtk_window_close window'
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowCloseMethodInfo
instance (signature ~ (m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowCloseMethodInfo a signature where
    overloadedMethod = windowClose

instance O.OverloadedMethodInfo WindowCloseMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowClose",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowClose"
        })


#endif

-- method Window::deiconify
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_deiconify" gtk_window_deiconify :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO ()

-- | Asks to deiconify (i.e. unminimize) the specified /@window@/. Note
-- that you shouldn’t assume the window is definitely deiconified
-- afterward, because other entities (e.g. the user or
-- [window manager][gtk-X11-arch])) could iconify it
-- again before your code which assumes deiconification gets to run.
-- 
-- You can track iconification via the “window-state-event” signal
-- on t'GI.Gtk.Objects.Widget.Widget'.
windowDeiconify ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m ()
windowDeiconify window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    gtk_window_deiconify window'
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowDeiconifyMethodInfo
instance (signature ~ (m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowDeiconifyMethodInfo a signature where
    overloadedMethod = windowDeiconify

instance O.OverloadedMethodInfo WindowDeiconifyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowDeiconify",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowDeiconify"
        })


#endif

-- method Window::fullscreen
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_fullscreen" gtk_window_fullscreen :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO ()

-- | Asks to place /@window@/ in the fullscreen state. Note that you
-- shouldn’t assume the window is definitely full screen afterward,
-- because other entities (e.g. the user or
-- [window manager][gtk-X11-arch]) could unfullscreen it
-- again, and not all window managers honor requests to fullscreen
-- windows. But normally the window will end up fullscreen. Just
-- don’t write code that crashes if not.
-- 
-- You can track the fullscreen state via the “window-state-event” signal
-- on t'GI.Gtk.Objects.Widget.Widget'.
-- 
-- /Since: 2.2/
windowFullscreen ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m ()
windowFullscreen window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    gtk_window_fullscreen window'
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowFullscreenMethodInfo
instance (signature ~ (m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowFullscreenMethodInfo a signature where
    overloadedMethod = windowFullscreen

instance O.OverloadedMethodInfo WindowFullscreenMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowFullscreen",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowFullscreen"
        })


#endif

-- method Window::fullscreen_on_monitor
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "screen"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Screen" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkScreen to draw to"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "monitor"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "which monitor to go fullscreen on"
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

foreign import ccall "gtk_window_fullscreen_on_monitor" gtk_window_fullscreen_on_monitor :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    Ptr Gdk.Screen.Screen ->                -- screen : TInterface (Name {namespace = "Gdk", name = "Screen"})
    Int32 ->                                -- monitor : TBasicType TInt
    IO ()

-- | Asks to place /@window@/ in the fullscreen state. Note that you shouldn\'t assume
-- the window is definitely full screen afterward.
-- 
-- You can track the fullscreen state via the \"window-state-event\" signal
-- on t'GI.Gtk.Objects.Widget.Widget'.
-- 
-- /Since: 3.18/
windowFullscreenOnMonitor ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a, Gdk.Screen.IsScreen b) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> b
    -- ^ /@screen@/: a t'GI.Gdk.Objects.Screen.Screen' to draw to
    -> Int32
    -- ^ /@monitor@/: which monitor to go fullscreen on
    -> m ()
windowFullscreenOnMonitor window screen monitor = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    screen' <- unsafeManagedPtrCastPtr screen
    gtk_window_fullscreen_on_monitor window' screen' monitor
    touchManagedPtr window
    touchManagedPtr screen
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowFullscreenOnMonitorMethodInfo
instance (signature ~ (b -> Int32 -> m ()), MonadIO m, IsWindow a, Gdk.Screen.IsScreen b) => O.OverloadedMethod WindowFullscreenOnMonitorMethodInfo a signature where
    overloadedMethod = windowFullscreenOnMonitor

instance O.OverloadedMethodInfo WindowFullscreenOnMonitorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowFullscreenOnMonitor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowFullscreenOnMonitor"
        })


#endif

-- method Window::get_accept_focus
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_get_accept_focus" gtk_window_get_accept_focus :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO CInt

-- | Gets the value set by 'GI.Gtk.Objects.Window.windowSetAcceptFocus'.
-- 
-- /Since: 2.4/
windowGetAcceptFocus ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if window should receive the input focus
windowGetAcceptFocus window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_get_accept_focus window'
    let result' = (/= 0) result
    touchManagedPtr window
    return result'

#if defined(ENABLE_OVERLOADING)
data WindowGetAcceptFocusMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetAcceptFocusMethodInfo a signature where
    overloadedMethod = windowGetAcceptFocus

instance O.OverloadedMethodInfo WindowGetAcceptFocusMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetAcceptFocus",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetAcceptFocus"
        })


#endif

-- method Window::get_application
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Application" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_window_get_application" gtk_window_get_application :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO (Ptr Gtk.Application.Application)

-- | Gets the t'GI.Gtk.Objects.Application.Application' associated with the window (if any).
-- 
-- /Since: 3.0/
windowGetApplication ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m (Maybe Gtk.Application.Application)
    -- ^ __Returns:__ a t'GI.Gtk.Objects.Application.Application', or 'P.Nothing'
windowGetApplication window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_get_application window'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Application.Application) result'
        return result''
    touchManagedPtr window
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data WindowGetApplicationMethodInfo
instance (signature ~ (m (Maybe Gtk.Application.Application)), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetApplicationMethodInfo a signature where
    overloadedMethod = windowGetApplication

instance O.OverloadedMethodInfo WindowGetApplicationMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetApplication",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetApplication"
        })


#endif

-- method Window::get_attached_to
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_get_attached_to" gtk_window_get_attached_to :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO (Ptr Gtk.Widget.Widget)

-- | Fetches the attach widget for this window. See
-- 'GI.Gtk.Objects.Window.windowSetAttachedTo'.
-- 
-- /Since: 3.4/
windowGetAttachedTo ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m (Maybe Gtk.Widget.Widget)
    -- ^ __Returns:__ the widget where the window
    -- is attached, or 'P.Nothing' if the window is not attached to any widget.
windowGetAttachedTo window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_get_attached_to window'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Widget.Widget) result'
        return result''
    touchManagedPtr window
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data WindowGetAttachedToMethodInfo
instance (signature ~ (m (Maybe Gtk.Widget.Widget)), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetAttachedToMethodInfo a signature where
    overloadedMethod = windowGetAttachedTo

instance O.OverloadedMethodInfo WindowGetAttachedToMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetAttachedTo",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetAttachedTo"
        })


#endif

-- method Window::get_decorated
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_get_decorated" gtk_window_get_decorated :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO CInt

-- | Returns whether the window has been set to have decorations
-- such as a title bar via 'GI.Gtk.Objects.Window.windowSetDecorated'.
windowGetDecorated ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the window has been set to have decorations
windowGetDecorated window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_get_decorated window'
    let result' = (/= 0) result
    touchManagedPtr window
    return result'

#if defined(ENABLE_OVERLOADING)
data WindowGetDecoratedMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetDecoratedMethodInfo a signature where
    overloadedMethod = windowGetDecorated

instance O.OverloadedMethodInfo WindowGetDecoratedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetDecorated",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetDecorated"
        })


#endif

-- method Window::get_default_size
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "width"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "location to store the default width, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "height"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location to store the default height, or %NULL"
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

foreign import ccall "gtk_window_get_default_size" gtk_window_get_default_size :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    Ptr Int32 ->                            -- width : TBasicType TInt
    Ptr Int32 ->                            -- height : TBasicType TInt
    IO ()

-- | Gets the default size of the window. A value of -1 for the width or
-- height indicates that a default size has not been explicitly set
-- for that dimension, so the “natural” size of the window will be
-- used.
windowGetDefaultSize ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m ((Int32, Int32))
windowGetDefaultSize window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    width <- allocMem :: IO (Ptr Int32)
    height <- allocMem :: IO (Ptr Int32)
    gtk_window_get_default_size window' width height
    width' <- peek width
    height' <- peek height
    touchManagedPtr window
    freeMem width
    freeMem height
    return (width', height')

#if defined(ENABLE_OVERLOADING)
data WindowGetDefaultSizeMethodInfo
instance (signature ~ (m ((Int32, Int32))), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetDefaultSizeMethodInfo a signature where
    overloadedMethod = windowGetDefaultSize

instance O.OverloadedMethodInfo WindowGetDefaultSizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetDefaultSize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetDefaultSize"
        })


#endif

-- method Window::get_default_widget
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_get_default_widget" gtk_window_get_default_widget :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO (Ptr Gtk.Widget.Widget)

-- | Returns the default widget for /@window@/. See
-- 'GI.Gtk.Objects.Window.windowSetDefault' for more details.
-- 
-- /Since: 2.14/
windowGetDefaultWidget ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m (Maybe Gtk.Widget.Widget)
    -- ^ __Returns:__ the default widget, or 'P.Nothing'
    -- if there is none.
windowGetDefaultWidget window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_get_default_widget window'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Widget.Widget) result'
        return result''
    touchManagedPtr window
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data WindowGetDefaultWidgetMethodInfo
instance (signature ~ (m (Maybe Gtk.Widget.Widget)), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetDefaultWidgetMethodInfo a signature where
    overloadedMethod = windowGetDefaultWidget

instance O.OverloadedMethodInfo WindowGetDefaultWidgetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetDefaultWidget",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetDefaultWidget"
        })


#endif

-- method Window::get_deletable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_get_deletable" gtk_window_get_deletable :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO CInt

-- | Returns whether the window has been set to have a close button
-- via 'GI.Gtk.Objects.Window.windowSetDeletable'.
-- 
-- /Since: 2.10/
windowGetDeletable ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the window has been set to have a close button
windowGetDeletable window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_get_deletable window'
    let result' = (/= 0) result
    touchManagedPtr window
    return result'

#if defined(ENABLE_OVERLOADING)
data WindowGetDeletableMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetDeletableMethodInfo a signature where
    overloadedMethod = windowGetDeletable

instance O.OverloadedMethodInfo WindowGetDeletableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetDeletable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetDeletable"
        })


#endif

-- method Window::get_destroy_with_parent
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_get_destroy_with_parent" gtk_window_get_destroy_with_parent :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO CInt

-- | Returns whether the window will be destroyed with its transient parent. See
-- gtk_window_set_destroy_with_parent ().
windowGetDestroyWithParent ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the window will be destroyed with its transient parent.
windowGetDestroyWithParent window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_get_destroy_with_parent window'
    let result' = (/= 0) result
    touchManagedPtr window
    return result'

#if defined(ENABLE_OVERLOADING)
data WindowGetDestroyWithParentMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetDestroyWithParentMethodInfo a signature where
    overloadedMethod = windowGetDestroyWithParent

instance O.OverloadedMethodInfo WindowGetDestroyWithParentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetDestroyWithParent",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetDestroyWithParent"
        })


#endif

-- method Window::get_focus
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_get_focus" gtk_window_get_focus :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO (Ptr Gtk.Widget.Widget)

-- | Retrieves the current focused widget within the window.
-- Note that this is the widget that would have the focus
-- if the toplevel window focused; if the toplevel window
-- is not focused then  @gtk_widget_has_focus (widget)@ will
-- not be 'P.True' for the widget.
windowGetFocus ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m (Maybe Gtk.Widget.Widget)
    -- ^ __Returns:__ the currently focused widget,
    -- or 'P.Nothing' if there is none.
windowGetFocus window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_get_focus window'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Widget.Widget) result'
        return result''
    touchManagedPtr window
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data WindowGetFocusMethodInfo
instance (signature ~ (m (Maybe Gtk.Widget.Widget)), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetFocusMethodInfo a signature where
    overloadedMethod = windowGetFocus

instance O.OverloadedMethodInfo WindowGetFocusMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetFocus",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetFocus"
        })


#endif

-- method Window::get_focus_on_map
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_get_focus_on_map" gtk_window_get_focus_on_map :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO CInt

-- | Gets the value set by 'GI.Gtk.Objects.Window.windowSetFocusOnMap'.
-- 
-- /Since: 2.6/
windowGetFocusOnMap ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if window should receive the input focus when
    -- mapped.
windowGetFocusOnMap window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_get_focus_on_map window'
    let result' = (/= 0) result
    touchManagedPtr window
    return result'

#if defined(ENABLE_OVERLOADING)
data WindowGetFocusOnMapMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetFocusOnMapMethodInfo a signature where
    overloadedMethod = windowGetFocusOnMap

instance O.OverloadedMethodInfo WindowGetFocusOnMapMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetFocusOnMap",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetFocusOnMap"
        })


#endif

-- method Window::get_focus_visible
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_get_focus_visible" gtk_window_get_focus_visible :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO CInt

-- | Gets the value of the [Window:focusVisible]("GI.Gtk.Objects.Window#g:attr:focusVisible") property.
-- 
-- /Since: 3.2/
windowGetFocusVisible ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if “focus rectangles” are supposed to be visible
    --     in this window.
windowGetFocusVisible window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_get_focus_visible window'
    let result' = (/= 0) result
    touchManagedPtr window
    return result'

#if defined(ENABLE_OVERLOADING)
data WindowGetFocusVisibleMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetFocusVisibleMethodInfo a signature where
    overloadedMethod = windowGetFocusVisible

instance O.OverloadedMethodInfo WindowGetFocusVisibleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetFocusVisible",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetFocusVisible"
        })


#endif

-- method Window::get_gravity
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gdk" , name = "Gravity" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_window_get_gravity" gtk_window_get_gravity :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO CUInt

-- | Gets the value set by 'GI.Gtk.Objects.Window.windowSetGravity'.
windowGetGravity ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m Gdk.Enums.Gravity
    -- ^ __Returns:__ window gravity
windowGetGravity window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_get_gravity window'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr window
    return result'

#if defined(ENABLE_OVERLOADING)
data WindowGetGravityMethodInfo
instance (signature ~ (m Gdk.Enums.Gravity), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetGravityMethodInfo a signature where
    overloadedMethod = windowGetGravity

instance O.OverloadedMethodInfo WindowGetGravityMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetGravity",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetGravity"
        })


#endif

-- method Window::get_group
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
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
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "WindowGroup" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_window_get_group" gtk_window_get_group :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO (Ptr Gtk.WindowGroup.WindowGroup)

-- | Returns the group for /@window@/ or the default group, if
-- /@window@/ is 'P.Nothing' or if /@window@/ does not have an explicit
-- window group.
-- 
-- /Since: 2.10/
windowGetGroup ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window', or 'P.Nothing'
    -> m Gtk.WindowGroup.WindowGroup
    -- ^ __Returns:__ the t'GI.Gtk.Objects.WindowGroup.WindowGroup' for a window or the default group
windowGetGroup window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_get_group window'
    checkUnexpectedReturnNULL "windowGetGroup" result
    result' <- (newObject Gtk.WindowGroup.WindowGroup) result
    touchManagedPtr window
    return result'

#if defined(ENABLE_OVERLOADING)
data WindowGetGroupMethodInfo
instance (signature ~ (m Gtk.WindowGroup.WindowGroup), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetGroupMethodInfo a signature where
    overloadedMethod = windowGetGroup

instance O.OverloadedMethodInfo WindowGetGroupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetGroup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetGroup"
        })


#endif

-- method Window::get_has_resize_grip
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_get_has_resize_grip" gtk_window_get_has_resize_grip :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO CInt

{-# DEPRECATED windowGetHasResizeGrip ["(Since version 3.14)","Resize grips have been removed."] #-}
-- | Determines whether the window may have a resize grip.
-- 
-- /Since: 3.0/
windowGetHasResizeGrip ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the window has a resize grip
windowGetHasResizeGrip window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_get_has_resize_grip window'
    let result' = (/= 0) result
    touchManagedPtr window
    return result'

#if defined(ENABLE_OVERLOADING)
data WindowGetHasResizeGripMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetHasResizeGripMethodInfo a signature where
    overloadedMethod = windowGetHasResizeGrip

instance O.OverloadedMethodInfo WindowGetHasResizeGripMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetHasResizeGrip",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetHasResizeGrip"
        })


#endif

-- method Window::get_hide_titlebar_when_maximized
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_get_hide_titlebar_when_maximized" gtk_window_get_hide_titlebar_when_maximized :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO CInt

-- | Returns whether the window has requested to have its titlebar hidden
-- when maximized. See gtk_window_set_hide_titlebar_when_maximized ().
-- 
-- /Since: 3.4/
windowGetHideTitlebarWhenMaximized ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the window has requested to have its titlebar
    --               hidden when maximized
windowGetHideTitlebarWhenMaximized window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_get_hide_titlebar_when_maximized window'
    let result' = (/= 0) result
    touchManagedPtr window
    return result'

#if defined(ENABLE_OVERLOADING)
data WindowGetHideTitlebarWhenMaximizedMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetHideTitlebarWhenMaximizedMethodInfo a signature where
    overloadedMethod = windowGetHideTitlebarWhenMaximized

instance O.OverloadedMethodInfo WindowGetHideTitlebarWhenMaximizedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetHideTitlebarWhenMaximized",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetHideTitlebarWhenMaximized"
        })


#endif

-- method Window::get_icon
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_get_icon" gtk_window_get_icon :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO (Ptr GdkPixbuf.Pixbuf.Pixbuf)

-- | Gets the value set by 'GI.Gtk.Objects.Window.windowSetIcon' (or if you\'ve
-- called 'GI.Gtk.Objects.Window.windowSetIconList', gets the first icon in
-- the icon list).
windowGetIcon ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m (Maybe GdkPixbuf.Pixbuf.Pixbuf)
    -- ^ __Returns:__ icon for window or 'P.Nothing' if none
windowGetIcon window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_get_icon window'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject GdkPixbuf.Pixbuf.Pixbuf) result'
        return result''
    touchManagedPtr window
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data WindowGetIconMethodInfo
instance (signature ~ (m (Maybe GdkPixbuf.Pixbuf.Pixbuf)), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetIconMethodInfo a signature where
    overloadedMethod = windowGetIcon

instance O.OverloadedMethodInfo WindowGetIconMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetIcon",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetIcon"
        })


#endif

-- method Window::get_icon_list
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TGList
--                  (TInterface Name { namespace = "GdkPixbuf" , name = "Pixbuf" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_window_get_icon_list" gtk_window_get_icon_list :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO (Ptr (GList (Ptr GdkPixbuf.Pixbuf.Pixbuf)))

-- | Retrieves the list of icons set by 'GI.Gtk.Objects.Window.windowSetIconList'.
-- The list is copied, but the reference count on each
-- member won’t be incremented.
windowGetIconList ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m [GdkPixbuf.Pixbuf.Pixbuf]
    -- ^ __Returns:__ copy of window’s icon list
windowGetIconList window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_get_icon_list window'
    result' <- unpackGList result
    result'' <- mapM (newObject GdkPixbuf.Pixbuf.Pixbuf) result'
    g_list_free result
    touchManagedPtr window
    return result''

#if defined(ENABLE_OVERLOADING)
data WindowGetIconListMethodInfo
instance (signature ~ (m [GdkPixbuf.Pixbuf.Pixbuf]), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetIconListMethodInfo a signature where
    overloadedMethod = windowGetIconList

instance O.OverloadedMethodInfo WindowGetIconListMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetIconList",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetIconList"
        })


#endif

-- method Window::get_icon_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_get_icon_name" gtk_window_get_icon_name :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO CString

-- | Returns the name of the themed icon for the window,
-- see 'GI.Gtk.Objects.Window.windowSetIconName'.
-- 
-- /Since: 2.6/
windowGetIconName ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m (Maybe T.Text)
    -- ^ __Returns:__ the icon name or 'P.Nothing' if the window has
    -- no themed icon
windowGetIconName window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_get_icon_name window'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToText result'
        return result''
    touchManagedPtr window
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data WindowGetIconNameMethodInfo
instance (signature ~ (m (Maybe T.Text)), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetIconNameMethodInfo a signature where
    overloadedMethod = windowGetIconName

instance O.OverloadedMethodInfo WindowGetIconNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetIconName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetIconName"
        })


#endif

-- method Window::get_mnemonic_modifier
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gdk" , name = "ModifierType" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_window_get_mnemonic_modifier" gtk_window_get_mnemonic_modifier :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO CUInt

-- | Returns the mnemonic modifier for this window. See
-- 'GI.Gtk.Objects.Window.windowSetMnemonicModifier'.
windowGetMnemonicModifier ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m [Gdk.Flags.ModifierType]
    -- ^ __Returns:__ the modifier mask used to activate
    --               mnemonics on this window.
windowGetMnemonicModifier window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_get_mnemonic_modifier window'
    let result' = wordToGFlags result
    touchManagedPtr window
    return result'

#if defined(ENABLE_OVERLOADING)
data WindowGetMnemonicModifierMethodInfo
instance (signature ~ (m [Gdk.Flags.ModifierType]), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetMnemonicModifierMethodInfo a signature where
    overloadedMethod = windowGetMnemonicModifier

instance O.OverloadedMethodInfo WindowGetMnemonicModifierMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetMnemonicModifier",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetMnemonicModifier"
        })


#endif

-- method Window::get_mnemonics_visible
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_get_mnemonics_visible" gtk_window_get_mnemonics_visible :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO CInt

-- | Gets the value of the [Window:mnemonicsVisible]("GI.Gtk.Objects.Window#g:attr:mnemonicsVisible") property.
-- 
-- /Since: 2.20/
windowGetMnemonicsVisible ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if mnemonics are supposed to be visible
    -- in this window.
windowGetMnemonicsVisible window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_get_mnemonics_visible window'
    let result' = (/= 0) result
    touchManagedPtr window
    return result'

#if defined(ENABLE_OVERLOADING)
data WindowGetMnemonicsVisibleMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetMnemonicsVisibleMethodInfo a signature where
    overloadedMethod = windowGetMnemonicsVisible

instance O.OverloadedMethodInfo WindowGetMnemonicsVisibleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetMnemonicsVisible",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetMnemonicsVisible"
        })


#endif

-- method Window::get_modal
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_get_modal" gtk_window_get_modal :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO CInt

-- | Returns whether the window is modal. See 'GI.Gtk.Objects.Window.windowSetModal'.
windowGetModal ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the window is set to be modal and
    --               establishes a grab when shown
windowGetModal window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_get_modal window'
    let result' = (/= 0) result
    touchManagedPtr window
    return result'

#if defined(ENABLE_OVERLOADING)
data WindowGetModalMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetModalMethodInfo a signature where
    overloadedMethod = windowGetModal

instance O.OverloadedMethodInfo WindowGetModalMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetModal",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetModal"
        })


#endif

-- method Window::get_opacity
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TDouble)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_window_get_opacity" gtk_window_get_opacity :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO CDouble

{-# DEPRECATED windowGetOpacity ["(Since version 3.8)","Use gtk_widget_get_opacity instead."] #-}
-- | Fetches the requested opacity for this window. See
-- 'GI.Gtk.Objects.Window.windowSetOpacity'.
-- 
-- /Since: 2.12/
windowGetOpacity ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m Double
    -- ^ __Returns:__ the requested opacity for this window.
windowGetOpacity window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_get_opacity window'
    let result' = realToFrac result
    touchManagedPtr window
    return result'

#if defined(ENABLE_OVERLOADING)
data WindowGetOpacityMethodInfo
instance (signature ~ (m Double), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetOpacityMethodInfo a signature where
    overloadedMethod = windowGetOpacity

instance O.OverloadedMethodInfo WindowGetOpacityMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetOpacity",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetOpacity"
        })


#endif

-- method Window::get_position
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "root_x"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "return location for X coordinate of\n    gravity-determined reference point, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "root_y"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "return location for Y coordinate of\n    gravity-determined reference point, or %NULL"
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

foreign import ccall "gtk_window_get_position" gtk_window_get_position :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    Ptr Int32 ->                            -- root_x : TBasicType TInt
    Ptr Int32 ->                            -- root_y : TBasicType TInt
    IO ()

-- | This function returns the position you need to pass to
-- 'GI.Gtk.Objects.Window.windowMove' to keep /@window@/ in its current position.
-- This means that the meaning of the returned value varies with
-- window gravity. See 'GI.Gtk.Objects.Window.windowMove' for more details.
-- 
-- The reliability of this function depends on the windowing system
-- currently in use. Some windowing systems, such as Wayland, do not
-- support a global coordinate system, and thus the position of the
-- window will always be (0, 0). Others, like X11, do not have a reliable
-- way to obtain the geometry of the decorations of a window if they are
-- provided by the window manager. Additionally, on X11, window manager
-- have been known to mismanage window gravity, which result in windows
-- moving even if you use the coordinates of the current position as
-- returned by this function.
-- 
-- If you haven’t changed the window gravity, its gravity will be
-- @/GDK_GRAVITY_NORTH_WEST/@. This means that 'GI.Gtk.Objects.Window.windowGetPosition'
-- gets the position of the top-left corner of the window manager
-- frame for the window. 'GI.Gtk.Objects.Window.windowMove' sets the position of this
-- same top-left corner.
-- 
-- If a window has gravity @/GDK_GRAVITY_STATIC/@ the window manager
-- frame is not relevant, and thus 'GI.Gtk.Objects.Window.windowGetPosition' will
-- always produce accurate results. However you can’t use static
-- gravity to do things like place a window in a corner of the screen,
-- because static gravity ignores the window manager decorations.
-- 
-- Ideally, this function should return appropriate values if the
-- window has client side decorations, assuming that the windowing
-- system supports global coordinates.
-- 
-- In practice, saving the window position should not be left to
-- applications, as they lack enough knowledge of the windowing
-- system and the window manager state to effectively do so. The
-- appropriate way to implement saving the window position is to
-- use a platform-specific protocol, wherever that is available.
windowGetPosition ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m ((Int32, Int32))
windowGetPosition window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    rootX <- allocMem :: IO (Ptr Int32)
    rootY <- allocMem :: IO (Ptr Int32)
    gtk_window_get_position window' rootX rootY
    rootX' <- peek rootX
    rootY' <- peek rootY
    touchManagedPtr window
    freeMem rootX
    freeMem rootY
    return (rootX', rootY')

#if defined(ENABLE_OVERLOADING)
data WindowGetPositionMethodInfo
instance (signature ~ (m ((Int32, Int32))), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetPositionMethodInfo a signature where
    overloadedMethod = windowGetPosition

instance O.OverloadedMethodInfo WindowGetPositionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetPosition",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetPosition"
        })


#endif

-- method Window::get_resizable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_get_resizable" gtk_window_get_resizable :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO CInt

-- | Gets the value set by 'GI.Gtk.Objects.Window.windowSetResizable'.
windowGetResizable ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the user can resize the window
windowGetResizable window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_get_resizable window'
    let result' = (/= 0) result
    touchManagedPtr window
    return result'

#if defined(ENABLE_OVERLOADING)
data WindowGetResizableMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetResizableMethodInfo a signature where
    overloadedMethod = windowGetResizable

instance O.OverloadedMethodInfo WindowGetResizableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetResizable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetResizable"
        })


#endif

-- method Window::get_resize_grip_area
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "rect"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a pointer to a #GdkRectangle which we should store\n    the resize grip area"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_window_get_resize_grip_area" gtk_window_get_resize_grip_area :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    Ptr Gdk.Rectangle.Rectangle ->          -- rect : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    IO CInt

{-# DEPRECATED windowGetResizeGripArea ["(Since version 3.14)","Resize grips have been removed."] #-}
-- | If a window has a resize grip, this will retrieve the grip
-- position, width and height into the specified t'GI.Gdk.Structs.Rectangle.Rectangle'.
-- 
-- /Since: 3.0/
windowGetResizeGripArea ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m ((Bool, Gdk.Rectangle.Rectangle))
    -- ^ __Returns:__ 'P.True' if the resize grip’s area was retrieved
windowGetResizeGripArea window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    rect <- SP.callocBoxedBytes 16 :: IO (Ptr Gdk.Rectangle.Rectangle)
    result <- gtk_window_get_resize_grip_area window' rect
    let result' = (/= 0) result
    rect' <- (wrapBoxed Gdk.Rectangle.Rectangle) rect
    touchManagedPtr window
    return (result', rect')

#if defined(ENABLE_OVERLOADING)
data WindowGetResizeGripAreaMethodInfo
instance (signature ~ (m ((Bool, Gdk.Rectangle.Rectangle))), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetResizeGripAreaMethodInfo a signature where
    overloadedMethod = windowGetResizeGripArea

instance O.OverloadedMethodInfo WindowGetResizeGripAreaMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetResizeGripArea",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetResizeGripArea"
        })


#endif

-- method Window::get_role
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_get_role" gtk_window_get_role :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO CString

-- | Returns the role of the window. See 'GI.Gtk.Objects.Window.windowSetRole' for
-- further explanation.
windowGetRole ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m (Maybe T.Text)
    -- ^ __Returns:__ the role of the window if set, or 'P.Nothing'. The
    -- returned is owned by the widget and must not be modified or freed.
windowGetRole window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_get_role window'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToText result'
        return result''
    touchManagedPtr window
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data WindowGetRoleMethodInfo
instance (signature ~ (m (Maybe T.Text)), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetRoleMethodInfo a signature where
    overloadedMethod = windowGetRole

instance O.OverloadedMethodInfo WindowGetRoleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetRole",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetRole"
        })


#endif

-- method Window::get_screen
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gdk" , name = "Screen" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_window_get_screen" gtk_window_get_screen :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO (Ptr Gdk.Screen.Screen)

-- | Returns the t'GI.Gdk.Objects.Screen.Screen' associated with /@window@/.
-- 
-- /Since: 2.2/
windowGetScreen ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'.
    -> m Gdk.Screen.Screen
    -- ^ __Returns:__ a t'GI.Gdk.Objects.Screen.Screen'.
windowGetScreen window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_get_screen window'
    checkUnexpectedReturnNULL "windowGetScreen" result
    result' <- (newObject Gdk.Screen.Screen) result
    touchManagedPtr window
    return result'

#if defined(ENABLE_OVERLOADING)
data WindowGetScreenMethodInfo
instance (signature ~ (m Gdk.Screen.Screen), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetScreenMethodInfo a signature where
    overloadedMethod = windowGetScreen

instance O.OverloadedMethodInfo WindowGetScreenMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetScreen",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetScreen"
        })


#endif

-- method Window::get_size
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "width"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return location for width, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "height"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return location for height, or %NULL"
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

foreign import ccall "gtk_window_get_size" gtk_window_get_size :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    Ptr Int32 ->                            -- width : TBasicType TInt
    Ptr Int32 ->                            -- height : TBasicType TInt
    IO ()

-- | Obtains the current size of /@window@/.
-- 
-- If /@window@/ is not visible on screen, this function return the size GTK+
-- will suggest to the [window manager][gtk-X11-arch] for the initial window
-- size (but this is not reliably the same as the size the window manager
-- will actually select). See: 'GI.Gtk.Objects.Window.windowSetDefaultSize'.
-- 
-- Depending on the windowing system and the window manager constraints,
-- the size returned by this function may not match the size set using
-- 'GI.Gtk.Objects.Window.windowResize'; additionally, since 'GI.Gtk.Objects.Window.windowResize' may be
-- implemented as an asynchronous operation, GTK+ cannot guarantee in any
-- way that this code:
-- 
-- 
-- === /C code/
-- >
-- >  // width and height are set elsewhere
-- >  gtk_window_resize (window, width, height);
-- >
-- >  int new_width, new_height;
-- >  gtk_window_get_size (window, &new_width, &new_height);
-- 
-- 
-- will result in @new_width@ and @new_height@ matching @width@ and
-- @height@, respectively.
-- 
-- This function will return the logical size of the t'GI.Gtk.Objects.Window.Window',
-- excluding the widgets used in client side decorations; there is,
-- however, no guarantee that the result will be completely accurate
-- because client side decoration may include widgets that depend on
-- the user preferences and that may not be visibile at the time you
-- call this function.
-- 
-- The dimensions returned by this function are suitable for being
-- stored across sessions; use 'GI.Gtk.Objects.Window.windowSetDefaultSize' to
-- restore them when before showing the window.
-- 
-- To avoid potential race conditions, you should only call this
-- function in response to a size change notification, for instance
-- inside a handler for the [Widget::sizeAllocate]("GI.Gtk.Objects.Widget#g:signal:sizeAllocate") signal, or
-- inside a handler for the [Widget::configureEvent]("GI.Gtk.Objects.Widget#g:signal:configureEvent") signal:
-- 
-- 
-- === /C code/
-- >
-- >static void
-- >on_size_allocate (GtkWidget *widget, GtkAllocation *allocation)
-- >{
-- >  int new_width, new_height;
-- >
-- >  gtk_window_get_size (GTK_WINDOW (widget), &new_width, &new_height);
-- >
-- >  ...
-- >}
-- 
-- 
-- Note that, if you connect to the [Widget::sizeAllocate]("GI.Gtk.Objects.Widget#g:signal:sizeAllocate") signal,
-- you should not use the dimensions of the @/GtkAllocation/@ passed to
-- the signal handler, as the allocation may contain client side
-- decorations added by GTK+, depending on the windowing system in
-- use.
-- 
-- If you are getting a window size in order to position the window
-- on the screen, you should, instead, simply set the window’s semantic
-- type with 'GI.Gtk.Objects.Window.windowSetTypeHint', which allows the window manager
-- to e.g. center dialogs. Also, if you set the transient parent of
-- dialogs with 'GI.Gtk.Objects.Window.windowSetTransientFor' window managers will
-- often center the dialog over its parent window. It\'s much preferred
-- to let the window manager handle these cases rather than doing it
-- yourself, because all apps will behave consistently and according to
-- user or system preferences, if the window manager handles it. Also,
-- the window manager can take into account the size of the window
-- decorations and border that it may add, and of which GTK+ has no
-- knowledge. Additionally, positioning windows in global screen coordinates
-- may not be allowed by the windowing system. For more information,
-- see: 'GI.Gtk.Objects.Window.windowSetPosition'.
windowGetSize ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m ((Int32, Int32))
windowGetSize window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    width <- allocMem :: IO (Ptr Int32)
    height <- allocMem :: IO (Ptr Int32)
    gtk_window_get_size window' width height
    width' <- peek width
    height' <- peek height
    touchManagedPtr window
    freeMem width
    freeMem height
    return (width', height')

#if defined(ENABLE_OVERLOADING)
data WindowGetSizeMethodInfo
instance (signature ~ (m ((Int32, Int32))), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetSizeMethodInfo a signature where
    overloadedMethod = windowGetSize

instance O.OverloadedMethodInfo WindowGetSizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetSize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetSize"
        })


#endif

-- method Window::get_skip_pager_hint
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_get_skip_pager_hint" gtk_window_get_skip_pager_hint :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO CInt

-- | Gets the value set by 'GI.Gtk.Objects.Window.windowSetSkipPagerHint'.
-- 
-- /Since: 2.2/
windowGetSkipPagerHint ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if window shouldn’t be in pager
windowGetSkipPagerHint window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_get_skip_pager_hint window'
    let result' = (/= 0) result
    touchManagedPtr window
    return result'

#if defined(ENABLE_OVERLOADING)
data WindowGetSkipPagerHintMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetSkipPagerHintMethodInfo a signature where
    overloadedMethod = windowGetSkipPagerHint

instance O.OverloadedMethodInfo WindowGetSkipPagerHintMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetSkipPagerHint",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetSkipPagerHint"
        })


#endif

-- method Window::get_skip_taskbar_hint
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_get_skip_taskbar_hint" gtk_window_get_skip_taskbar_hint :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO CInt

-- | Gets the value set by 'GI.Gtk.Objects.Window.windowSetSkipTaskbarHint'
-- 
-- /Since: 2.2/
windowGetSkipTaskbarHint ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if window shouldn’t be in taskbar
windowGetSkipTaskbarHint window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_get_skip_taskbar_hint window'
    let result' = (/= 0) result
    touchManagedPtr window
    return result'

#if defined(ENABLE_OVERLOADING)
data WindowGetSkipTaskbarHintMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetSkipTaskbarHintMethodInfo a signature where
    overloadedMethod = windowGetSkipTaskbarHint

instance O.OverloadedMethodInfo WindowGetSkipTaskbarHintMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetSkipTaskbarHint",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetSkipTaskbarHint"
        })


#endif

-- method Window::get_title
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_get_title" gtk_window_get_title :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO CString

-- | Retrieves the title of the window. See 'GI.Gtk.Objects.Window.windowSetTitle'.
windowGetTitle ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m (Maybe T.Text)
    -- ^ __Returns:__ the title of the window, or 'P.Nothing' if none has
    -- been set explicitly. The returned string is owned by the widget
    -- and must not be modified or freed.
windowGetTitle window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_get_title window'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToText result'
        return result''
    touchManagedPtr window
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data WindowGetTitleMethodInfo
instance (signature ~ (m (Maybe T.Text)), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetTitleMethodInfo a signature where
    overloadedMethod = windowGetTitle

instance O.OverloadedMethodInfo WindowGetTitleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetTitle",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetTitle"
        })


#endif

-- method Window::get_titlebar
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_get_titlebar" gtk_window_get_titlebar :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO (Ptr Gtk.Widget.Widget)

-- | Returns the custom titlebar that has been set with
-- 'GI.Gtk.Objects.Window.windowSetTitlebar'.
-- 
-- /Since: 3.16/
windowGetTitlebar ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m (Maybe Gtk.Widget.Widget)
    -- ^ __Returns:__ the custom titlebar, or 'P.Nothing'
windowGetTitlebar window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_get_titlebar window'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Widget.Widget) result'
        return result''
    touchManagedPtr window
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data WindowGetTitlebarMethodInfo
instance (signature ~ (m (Maybe Gtk.Widget.Widget)), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetTitlebarMethodInfo a signature where
    overloadedMethod = windowGetTitlebar

instance O.OverloadedMethodInfo WindowGetTitlebarMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetTitlebar",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetTitlebar"
        })


#endif

-- method Window::get_transient_for
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Window" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_window_get_transient_for" gtk_window_get_transient_for :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO (Ptr Window)

-- | Fetches the transient parent for this window. See
-- 'GI.Gtk.Objects.Window.windowSetTransientFor'.
windowGetTransientFor ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m (Maybe Window)
    -- ^ __Returns:__ the transient parent for this
    -- window, or 'P.Nothing' if no transient parent has been set.
windowGetTransientFor window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_get_transient_for window'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Window) result'
        return result''
    touchManagedPtr window
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data WindowGetTransientForMethodInfo
instance (signature ~ (m (Maybe Window)), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetTransientForMethodInfo a signature where
    overloadedMethod = windowGetTransientFor

instance O.OverloadedMethodInfo WindowGetTransientForMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetTransientFor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetTransientFor"
        })


#endif

-- method Window::get_type_hint
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gdk" , name = "WindowTypeHint" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_window_get_type_hint" gtk_window_get_type_hint :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO CUInt

-- | Gets the type hint for this window. See 'GI.Gtk.Objects.Window.windowSetTypeHint'.
windowGetTypeHint ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m Gdk.Enums.WindowTypeHint
    -- ^ __Returns:__ the type hint for /@window@/.
windowGetTypeHint window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_get_type_hint window'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr window
    return result'

#if defined(ENABLE_OVERLOADING)
data WindowGetTypeHintMethodInfo
instance (signature ~ (m Gdk.Enums.WindowTypeHint), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetTypeHintMethodInfo a signature where
    overloadedMethod = windowGetTypeHint

instance O.OverloadedMethodInfo WindowGetTypeHintMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetTypeHint",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetTypeHint"
        })


#endif

-- method Window::get_urgency_hint
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_get_urgency_hint" gtk_window_get_urgency_hint :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO CInt

-- | Gets the value set by 'GI.Gtk.Objects.Window.windowSetUrgencyHint'
-- 
-- /Since: 2.8/
windowGetUrgencyHint ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if window is urgent
windowGetUrgencyHint window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_get_urgency_hint window'
    let result' = (/= 0) result
    touchManagedPtr window
    return result'

#if defined(ENABLE_OVERLOADING)
data WindowGetUrgencyHintMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetUrgencyHintMethodInfo a signature where
    overloadedMethod = windowGetUrgencyHint

instance O.OverloadedMethodInfo WindowGetUrgencyHintMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetUrgencyHint",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetUrgencyHint"
        })


#endif

-- method Window::get_window_type
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "WindowType" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_window_get_window_type" gtk_window_get_window_type :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO CUInt

-- | Gets the type of the window. See t'GI.Gtk.Enums.WindowType'.
-- 
-- /Since: 2.20/
windowGetWindowType ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m Gtk.Enums.WindowType
    -- ^ __Returns:__ the type of the window
windowGetWindowType window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_get_window_type window'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr window
    return result'

#if defined(ENABLE_OVERLOADING)
data WindowGetWindowTypeMethodInfo
instance (signature ~ (m Gtk.Enums.WindowType), MonadIO m, IsWindow a) => O.OverloadedMethod WindowGetWindowTypeMethodInfo a signature where
    overloadedMethod = windowGetWindowType

instance O.OverloadedMethodInfo WindowGetWindowTypeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowGetWindowType",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowGetWindowType"
        })


#endif

-- method Window::has_group
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_has_group" gtk_window_has_group :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO CInt

-- | Returns whether /@window@/ has an explicit window group.
windowHasGroup ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@window@/ has an explicit window group.
    -- 
    -- Since 2.22
windowHasGroup window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_has_group window'
    let result' = (/= 0) result
    touchManagedPtr window
    return result'

#if defined(ENABLE_OVERLOADING)
data WindowHasGroupMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsWindow a) => O.OverloadedMethod WindowHasGroupMethodInfo a signature where
    overloadedMethod = windowHasGroup

instance O.OverloadedMethodInfo WindowHasGroupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowHasGroup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowHasGroup"
        })


#endif

-- method Window::has_toplevel_focus
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_has_toplevel_focus" gtk_window_has_toplevel_focus :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO CInt

-- | Returns whether the input focus is within this GtkWindow.
-- For real toplevel windows, this is identical to 'GI.Gtk.Objects.Window.windowIsActive',
-- but for embedded windows, like @/GtkPlug/@, the results will differ.
-- 
-- /Since: 2.4/
windowHasToplevelFocus ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the input focus is within this GtkWindow
windowHasToplevelFocus window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_has_toplevel_focus window'
    let result' = (/= 0) result
    touchManagedPtr window
    return result'

#if defined(ENABLE_OVERLOADING)
data WindowHasToplevelFocusMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsWindow a) => O.OverloadedMethod WindowHasToplevelFocusMethodInfo a signature where
    overloadedMethod = windowHasToplevelFocus

instance O.OverloadedMethodInfo WindowHasToplevelFocusMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowHasToplevelFocus",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowHasToplevelFocus"
        })


#endif

-- method Window::iconify
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_iconify" gtk_window_iconify :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO ()

-- | Asks to iconify (i.e. minimize) the specified /@window@/. Note that
-- you shouldn’t assume the window is definitely iconified afterward,
-- because other entities (e.g. the user or
-- [window manager][gtk-X11-arch]) could deiconify it
-- again, or there may not be a window manager in which case
-- iconification isn’t possible, etc. But normally the window will end
-- up iconified. Just don’t write code that crashes if not.
-- 
-- It’s permitted to call this function before showing a window,
-- in which case the window will be iconified before it ever appears
-- onscreen.
-- 
-- You can track iconification via the “window-state-event” signal
-- on t'GI.Gtk.Objects.Widget.Widget'.
windowIconify ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m ()
windowIconify window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    gtk_window_iconify window'
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowIconifyMethodInfo
instance (signature ~ (m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowIconifyMethodInfo a signature where
    overloadedMethod = windowIconify

instance O.OverloadedMethodInfo WindowIconifyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowIconify",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowIconify"
        })


#endif

-- method Window::is_active
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_is_active" gtk_window_is_active :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO CInt

-- | Returns whether the window is part of the current active toplevel.
-- (That is, the toplevel window receiving keystrokes.)
-- The return value is 'P.True' if the window is active toplevel
-- itself, but also if it is, say, a @/GtkPlug/@ embedded in the active toplevel.
-- You might use this function if you wanted to draw a widget
-- differently in an active window from a widget in an inactive window.
-- See 'GI.Gtk.Objects.Window.windowHasToplevelFocus'
-- 
-- /Since: 2.4/
windowIsActive ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the window part of the current active window.
windowIsActive window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_is_active window'
    let result' = (/= 0) result
    touchManagedPtr window
    return result'

#if defined(ENABLE_OVERLOADING)
data WindowIsActiveMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsWindow a) => O.OverloadedMethod WindowIsActiveMethodInfo a signature where
    overloadedMethod = windowIsActive

instance O.OverloadedMethodInfo WindowIsActiveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowIsActive",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowIsActive"
        })


#endif

-- method Window::is_maximized
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_is_maximized" gtk_window_is_maximized :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO CInt

-- | Retrieves the current maximized state of /@window@/.
-- 
-- Note that since maximization is ultimately handled by the window
-- manager and happens asynchronously to an application request, you
-- shouldn’t assume the return value of this function changing
-- immediately (or at all), as an effect of calling
-- 'GI.Gtk.Objects.Window.windowMaximize' or 'GI.Gtk.Objects.Window.windowUnmaximize'.
-- 
-- /Since: 3.12/
windowIsMaximized ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m Bool
    -- ^ __Returns:__ whether the window has a maximized state.
windowIsMaximized window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_is_maximized window'
    let result' = (/= 0) result
    touchManagedPtr window
    return result'

#if defined(ENABLE_OVERLOADING)
data WindowIsMaximizedMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsWindow a) => O.OverloadedMethod WindowIsMaximizedMethodInfo a signature where
    overloadedMethod = windowIsMaximized

instance O.OverloadedMethodInfo WindowIsMaximizedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowIsMaximized",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowIsMaximized"
        })


#endif

-- method Window::maximize
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_maximize" gtk_window_maximize :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO ()

-- | Asks to maximize /@window@/, so that it becomes full-screen. Note that
-- you shouldn’t assume the window is definitely maximized afterward,
-- because other entities (e.g. the user or
-- [window manager][gtk-X11-arch]) could unmaximize it
-- again, and not all window managers support maximization. But
-- normally the window will end up maximized. Just don’t write code
-- that crashes if not.
-- 
-- It’s permitted to call this function before showing a window,
-- in which case the window will be maximized when it appears onscreen
-- initially.
-- 
-- You can track maximization via the “window-state-event” signal
-- on t'GI.Gtk.Objects.Widget.Widget', or by listening to notifications on the
-- [Window:isMaximized]("GI.Gtk.Objects.Window#g:attr:isMaximized") property.
windowMaximize ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m ()
windowMaximize window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    gtk_window_maximize window'
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowMaximizeMethodInfo
instance (signature ~ (m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowMaximizeMethodInfo a signature where
    overloadedMethod = windowMaximize

instance O.OverloadedMethodInfo WindowMaximizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowMaximize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowMaximize"
        })


#endif

-- method Window::mnemonic_activate
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "keyval"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the mnemonic" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "modifier"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "ModifierType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the modifiers" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_mnemonic_activate" gtk_window_mnemonic_activate :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    Word32 ->                               -- keyval : TBasicType TUInt
    CUInt ->                                -- modifier : TInterface (Name {namespace = "Gdk", name = "ModifierType"})
    IO CInt

-- | Activates the targets associated with the mnemonic.
windowMnemonicActivate ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Word32
    -- ^ /@keyval@/: the mnemonic
    -> [Gdk.Flags.ModifierType]
    -- ^ /@modifier@/: the modifiers
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the activation is done.
windowMnemonicActivate window keyval modifier = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    let modifier' = gflagsToWord modifier
    result <- gtk_window_mnemonic_activate window' keyval modifier'
    let result' = (/= 0) result
    touchManagedPtr window
    return result'

#if defined(ENABLE_OVERLOADING)
data WindowMnemonicActivateMethodInfo
instance (signature ~ (Word32 -> [Gdk.Flags.ModifierType] -> m Bool), MonadIO m, IsWindow a) => O.OverloadedMethod WindowMnemonicActivateMethodInfo a signature where
    overloadedMethod = windowMnemonicActivate

instance O.OverloadedMethodInfo WindowMnemonicActivateMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowMnemonicActivate",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowMnemonicActivate"
        })


#endif

-- method Window::move
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "X coordinate to move window to"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "Y coordinate to move window to"
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

foreign import ccall "gtk_window_move" gtk_window_move :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    Int32 ->                                -- x : TBasicType TInt
    Int32 ->                                -- y : TBasicType TInt
    IO ()

-- | Asks the [window manager][gtk-X11-arch] to move
-- /@window@/ to the given position.  Window managers are free to ignore
-- this; most window managers ignore requests for initial window
-- positions (instead using a user-defined placement algorithm) and
-- honor requests after the window has already been shown.
-- 
-- Note: the position is the position of the gravity-determined
-- reference point for the window. The gravity determines two things:
-- first, the location of the reference point in root window
-- coordinates; and second, which point on the window is positioned at
-- the reference point.
-- 
-- By default the gravity is @/GDK_GRAVITY_NORTH_WEST/@, so the reference
-- point is simply the /@x@/, /@y@/ supplied to 'GI.Gtk.Objects.Window.windowMove'. The
-- top-left corner of the window decorations (aka window frame or
-- border) will be placed at /@x@/, /@y@/.  Therefore, to position a window
-- at the top left of the screen, you want to use the default gravity
-- (which is @/GDK_GRAVITY_NORTH_WEST/@) and move the window to 0,0.
-- 
-- To position a window at the bottom right corner of the screen, you
-- would set @/GDK_GRAVITY_SOUTH_EAST/@, which means that the reference
-- point is at /@x@/ + the window width and /@y@/ + the window height, and
-- the bottom-right corner of the window border will be placed at that
-- reference point. So, to place a window in the bottom right corner
-- you would first set gravity to south east, then write:
-- @gtk_window_move (window, gdk_screen_width () - window_width,
-- gdk_screen_height () - window_height)@ (note that this
-- example does not take multi-head scenarios into account).
-- 
-- The <http://www.freedesktop.org/Standards/wm-spec Extended Window Manager Hints Specification>
-- has a nice table of gravities in the “implementation notes” section.
-- 
-- The 'GI.Gtk.Objects.Window.windowGetPosition' documentation may also be relevant.
windowMove ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Int32
    -- ^ /@x@/: X coordinate to move window to
    -> Int32
    -- ^ /@y@/: Y coordinate to move window to
    -> m ()
windowMove window x y = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    gtk_window_move window' x y
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowMoveMethodInfo
instance (signature ~ (Int32 -> Int32 -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowMoveMethodInfo a signature where
    overloadedMethod = windowMove

instance O.OverloadedMethodInfo WindowMoveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowMove",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowMove"
        })


#endif

-- method Window::parse_geometry
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "geometry"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "geometry string" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_parse_geometry" gtk_window_parse_geometry :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    CString ->                              -- geometry : TBasicType TUTF8
    IO CInt

{-# DEPRECATED windowParseGeometry ["(Since version 3.20)","Geometry handling in GTK is deprecated."] #-}
-- | Parses a standard X Window System geometry string - see the
-- manual page for X (type “man X”) for details on this.
-- 'GI.Gtk.Objects.Window.windowParseGeometry' does work on all GTK+ ports
-- including Win32 but is primarily intended for an X environment.
-- 
-- If either a size or a position can be extracted from the
-- geometry string, 'GI.Gtk.Objects.Window.windowParseGeometry' returns 'P.True'
-- and calls 'GI.Gtk.Objects.Window.windowSetDefaultSize' and\/or 'GI.Gtk.Objects.Window.windowMove'
-- to resize\/move the window.
-- 
-- If 'GI.Gtk.Objects.Window.windowParseGeometry' returns 'P.True', it will also
-- set the @/GDK_HINT_USER_POS/@ and\/or @/GDK_HINT_USER_SIZE/@ hints
-- indicating to the window manager that the size\/position of
-- the window was user-specified. This causes most window
-- managers to honor the geometry.
-- 
-- Note that for 'GI.Gtk.Objects.Window.windowParseGeometry' to work as expected, it has
-- to be called when the window has its “final” size, i.e. after calling
-- 'GI.Gtk.Objects.Widget.widgetShowAll' on the contents and 'GI.Gtk.Objects.Window.windowSetGeometryHints'
-- on the window.
-- 
-- === /C code/
-- >
-- >#include <gtk/gtk.h>
-- >
-- >static void
-- >fill_with_content (GtkWidget *vbox)
-- >{
-- >  // fill with content...
-- >}
-- >
-- >int
-- >main (int argc, char *argv[])
-- >{
-- >  GtkWidget *window, *vbox;
-- >  GdkGeometry size_hints = {
-- >    100, 50, 0, 0, 100, 50, 10,
-- >    10, 0.0, 0.0, GDK_GRAVITY_NORTH_WEST
-- >  };
-- >
-- >  gtk_init (&argc, &argv);
-- >
-- >  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
-- >  vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
-- >
-- >  gtk_container_add (GTK_CONTAINER (window), vbox);
-- >  fill_with_content (vbox);
-- >  gtk_widget_show_all (vbox);
-- >
-- >  gtk_window_set_geometry_hints (GTK_WINDOW (window),
-- >	  			    NULL,
-- >				    &size_hints,
-- >				    GDK_HINT_MIN_SIZE |
-- >				    GDK_HINT_BASE_SIZE |
-- >				    GDK_HINT_RESIZE_INC);
-- >
-- >  if (argc > 1)
-- >    {
-- >      gboolean res;
-- >      res = gtk_window_parse_geometry (GTK_WINDOW (window),
-- >                                       argv[1]);
-- >      if (! res)
-- >        fprintf (stderr,
-- >                 "Failed to parse “%s”\n",
-- >                 argv[1]);
-- >    }
-- >
-- >  gtk_widget_show_all (window);
-- >  gtk_main ();
-- >
-- >  return 0;
-- >}
windowParseGeometry ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> T.Text
    -- ^ /@geometry@/: geometry string
    -> m Bool
    -- ^ __Returns:__ 'P.True' if string was parsed successfully
windowParseGeometry window geometry = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    geometry' <- textToCString geometry
    result <- gtk_window_parse_geometry window' geometry'
    let result' = (/= 0) result
    touchManagedPtr window
    freeMem geometry'
    return result'

#if defined(ENABLE_OVERLOADING)
data WindowParseGeometryMethodInfo
instance (signature ~ (T.Text -> m Bool), MonadIO m, IsWindow a) => O.OverloadedMethod WindowParseGeometryMethodInfo a signature where
    overloadedMethod = windowParseGeometry

instance O.OverloadedMethodInfo WindowParseGeometryMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowParseGeometry",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowParseGeometry"
        })


#endif

-- method Window::present
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_present" gtk_window_present :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO ()

-- | Presents a window to the user. This function should not be used
-- as when it is called, it is too late to gather a valid timestamp
-- to allow focus stealing prevention to work correctly.
windowPresent ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m ()
windowPresent window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    gtk_window_present window'
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowPresentMethodInfo
instance (signature ~ (m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowPresentMethodInfo a signature where
    overloadedMethod = windowPresent

instance O.OverloadedMethodInfo WindowPresentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowPresent",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowPresent"
        })


#endif

-- method Window::present_with_time
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "timestamp"
--           , argType = TBasicType TUInt32
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the timestamp of the user interaction (typically a\n  button or key press event) which triggered this call"
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

foreign import ccall "gtk_window_present_with_time" gtk_window_present_with_time :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    Word32 ->                               -- timestamp : TBasicType TUInt32
    IO ()

-- | Presents a window to the user. This may mean raising the window
-- in the stacking order, deiconifying it, moving it to the current
-- desktop, and\/or giving it the keyboard focus, possibly dependent
-- on the user’s platform, window manager, and preferences.
-- 
-- If /@window@/ is hidden, this function calls 'GI.Gtk.Objects.Widget.widgetShow'
-- as well.
-- 
-- This function should be used when the user tries to open a window
-- that’s already open. Say for example the preferences dialog is
-- currently open, and the user chooses Preferences from the menu
-- a second time; use 'GI.Gtk.Objects.Window.windowPresent' to move the already-open dialog
-- where the user can see it.
-- 
-- Presents a window to the user in response to a user interaction. The
-- timestamp should be gathered when the window was requested to be shown
-- (when clicking a link for example), rather than once the window is
-- ready to be shown.
-- 
-- /Since: 2.8/
windowPresentWithTime ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Word32
    -- ^ /@timestamp@/: the timestamp of the user interaction (typically a
    --   button or key press event) which triggered this call
    -> m ()
windowPresentWithTime window timestamp = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    gtk_window_present_with_time window' timestamp
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowPresentWithTimeMethodInfo
instance (signature ~ (Word32 -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowPresentWithTimeMethodInfo a signature where
    overloadedMethod = windowPresentWithTime

instance O.OverloadedMethodInfo WindowPresentWithTimeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowPresentWithTime",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowPresentWithTime"
        })


#endif

-- method Window::propagate_key_event
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "event"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "EventKey" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkEventKey" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_propagate_key_event" gtk_window_propagate_key_event :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    Ptr Gdk.EventKey.EventKey ->            -- event : TInterface (Name {namespace = "Gdk", name = "EventKey"})
    IO CInt

-- | Propagate a key press or release event to the focus widget and
-- up the focus container chain until a widget handles /@event@/.
-- This is normally called by the default [key_press_event](#g:signal:key_press_event) and
-- [key_release_event](#g:signal:key_release_event) handlers for toplevel windows,
-- however in some cases it may be useful to call this directly when
-- overriding the standard key handling for a toplevel window.
-- 
-- /Since: 2.4/
windowPropagateKeyEvent ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Gdk.EventKey.EventKey
    -- ^ /@event@/: a t'GI.Gdk.Structs.EventKey.EventKey'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if a widget in the focus chain handled the event.
windowPropagateKeyEvent window event = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    event' <- unsafeManagedPtrGetPtr event
    result <- gtk_window_propagate_key_event window' event'
    let result' = (/= 0) result
    touchManagedPtr window
    touchManagedPtr event
    return result'

#if defined(ENABLE_OVERLOADING)
data WindowPropagateKeyEventMethodInfo
instance (signature ~ (Gdk.EventKey.EventKey -> m Bool), MonadIO m, IsWindow a) => O.OverloadedMethod WindowPropagateKeyEventMethodInfo a signature where
    overloadedMethod = windowPropagateKeyEvent

instance O.OverloadedMethodInfo WindowPropagateKeyEventMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowPropagateKeyEvent",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowPropagateKeyEvent"
        })


#endif

-- method Window::remove_accel_group
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "accel_group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AccelGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAccelGroup" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_remove_accel_group" gtk_window_remove_accel_group :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    Ptr Gtk.AccelGroup.AccelGroup ->        -- accel_group : TInterface (Name {namespace = "Gtk", name = "AccelGroup"})
    IO ()

-- | Reverses the effects of 'GI.Gtk.Objects.Window.windowAddAccelGroup'.
windowRemoveAccelGroup ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a, Gtk.AccelGroup.IsAccelGroup b) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> b
    -- ^ /@accelGroup@/: a t'GI.Gtk.Objects.AccelGroup.AccelGroup'
    -> m ()
windowRemoveAccelGroup window accelGroup = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    accelGroup' <- unsafeManagedPtrCastPtr accelGroup
    gtk_window_remove_accel_group window' accelGroup'
    touchManagedPtr window
    touchManagedPtr accelGroup
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowRemoveAccelGroupMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsWindow a, Gtk.AccelGroup.IsAccelGroup b) => O.OverloadedMethod WindowRemoveAccelGroupMethodInfo a signature where
    overloadedMethod = windowRemoveAccelGroup

instance O.OverloadedMethodInfo WindowRemoveAccelGroupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowRemoveAccelGroup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowRemoveAccelGroup"
        })


#endif

-- method Window::remove_mnemonic
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "keyval"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the mnemonic" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "target"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the widget that gets activated by the mnemonic"
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

foreign import ccall "gtk_window_remove_mnemonic" gtk_window_remove_mnemonic :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    Word32 ->                               -- keyval : TBasicType TUInt
    Ptr Gtk.Widget.Widget ->                -- target : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | Removes a mnemonic from this window.
windowRemoveMnemonic ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Word32
    -- ^ /@keyval@/: the mnemonic
    -> b
    -- ^ /@target@/: the widget that gets activated by the mnemonic
    -> m ()
windowRemoveMnemonic window keyval target = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    target' <- unsafeManagedPtrCastPtr target
    gtk_window_remove_mnemonic window' keyval target'
    touchManagedPtr window
    touchManagedPtr target
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowRemoveMnemonicMethodInfo
instance (signature ~ (Word32 -> b -> m ()), MonadIO m, IsWindow a, Gtk.Widget.IsWidget b) => O.OverloadedMethod WindowRemoveMnemonicMethodInfo a signature where
    overloadedMethod = windowRemoveMnemonic

instance O.OverloadedMethodInfo WindowRemoveMnemonicMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowRemoveMnemonic",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowRemoveMnemonic"
        })


#endif

-- method Window::reshow_with_initial_size
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_reshow_with_initial_size" gtk_window_reshow_with_initial_size :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO ()

{-# DEPRECATED windowReshowWithInitialSize ["(Since version 3.10)","GUI builders can call 'GI.Gtk.Objects.Widget.widgetHide',","  'GI.Gtk.Objects.Widget.widgetUnrealize' and then 'GI.Gtk.Objects.Widget.widgetShow' on /@window@/","  themselves, if they still need this functionality."] #-}
-- | Hides /@window@/, then reshows it, resetting the
-- default size and position of the window. Used
-- by GUI builders only.
windowReshowWithInitialSize ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m ()
windowReshowWithInitialSize window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    gtk_window_reshow_with_initial_size window'
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowReshowWithInitialSizeMethodInfo
instance (signature ~ (m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowReshowWithInitialSizeMethodInfo a signature where
    overloadedMethod = windowReshowWithInitialSize

instance O.OverloadedMethodInfo WindowReshowWithInitialSizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowReshowWithInitialSize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowReshowWithInitialSize"
        })


#endif

-- method Window::resize
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "width"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "width in pixels to resize the window to"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "height"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "height in pixels to resize the window to"
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

foreign import ccall "gtk_window_resize" gtk_window_resize :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    Int32 ->                                -- width : TBasicType TInt
    Int32 ->                                -- height : TBasicType TInt
    IO ()

-- | Resizes the window as if the user had done so, obeying geometry
-- constraints. The default geometry constraint is that windows may
-- not be smaller than their size request; to override this
-- constraint, call 'GI.Gtk.Objects.Widget.widgetSetSizeRequest' to set the window\'s
-- request to a smaller value.
-- 
-- If 'GI.Gtk.Objects.Window.windowResize' is called before showing a window for the
-- first time, it overrides any default size set with
-- 'GI.Gtk.Objects.Window.windowSetDefaultSize'.
-- 
-- Windows may not be resized smaller than 1 by 1 pixels.
-- 
-- When using client side decorations, GTK+ will do its best to adjust
-- the given size so that the resulting window size matches the
-- requested size without the title bar, borders and shadows added for
-- the client side decorations, but there is no guarantee that the
-- result will be totally accurate because these widgets added for
-- client side decorations depend on the theme and may not be realized
-- or visible at the time 'GI.Gtk.Objects.Window.windowResize' is issued.
-- 
-- If the GtkWindow has a titlebar widget (see 'GI.Gtk.Objects.Window.windowSetTitlebar'), then
-- typically, 'GI.Gtk.Objects.Window.windowResize' will compensate for the height of the titlebar
-- widget only if the height is known when the resulting GtkWindow configuration
-- is issued.
-- For example, if new widgets are added after the GtkWindow configuration
-- and cause the titlebar widget to grow in height, this will result in a
-- window content smaller that specified by 'GI.Gtk.Objects.Window.windowResize' and not
-- a larger window.
windowResize ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Int32
    -- ^ /@width@/: width in pixels to resize the window to
    -> Int32
    -- ^ /@height@/: height in pixels to resize the window to
    -> m ()
windowResize window width height = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    gtk_window_resize window' width height
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowResizeMethodInfo
instance (signature ~ (Int32 -> Int32 -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowResizeMethodInfo a signature where
    overloadedMethod = windowResize

instance O.OverloadedMethodInfo WindowResizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowResize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowResize"
        })


#endif

-- method Window::resize_grip_is_visible
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_resize_grip_is_visible" gtk_window_resize_grip_is_visible :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO CInt

{-# DEPRECATED windowResizeGripIsVisible ["(Since version 3.14)","Resize grips have been removed."] #-}
-- | Determines whether a resize grip is visible for the specified window.
-- 
-- /Since: 3.0/
windowResizeGripIsVisible ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if a resize grip exists and is visible
windowResizeGripIsVisible window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_window_resize_grip_is_visible window'
    let result' = (/= 0) result
    touchManagedPtr window
    return result'

#if defined(ENABLE_OVERLOADING)
data WindowResizeGripIsVisibleMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsWindow a) => O.OverloadedMethod WindowResizeGripIsVisibleMethodInfo a signature where
    overloadedMethod = windowResizeGripIsVisible

instance O.OverloadedMethodInfo WindowResizeGripIsVisibleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowResizeGripIsVisible",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowResizeGripIsVisible"
        })


#endif

-- method Window::resize_to_geometry
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "width"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "width in resize increments to resize the window to"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "height"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "height in resize increments to resize the window to"
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

foreign import ccall "gtk_window_resize_to_geometry" gtk_window_resize_to_geometry :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    Int32 ->                                -- width : TBasicType TInt
    Int32 ->                                -- height : TBasicType TInt
    IO ()

{-# DEPRECATED windowResizeToGeometry ["(Since version 3.20)","This function does nothing. Use","   'GI.Gtk.Objects.Window.windowResize' and compute the geometry yourself."] #-}
-- | Like 'GI.Gtk.Objects.Window.windowResize', but /@width@/ and /@height@/ are interpreted
-- in terms of the base size and increment set with
-- gtk_window_set_geometry_hints.
-- 
-- /Since: 3.0/
windowResizeToGeometry ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Int32
    -- ^ /@width@/: width in resize increments to resize the window to
    -> Int32
    -- ^ /@height@/: height in resize increments to resize the window to
    -> m ()
windowResizeToGeometry window width height = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    gtk_window_resize_to_geometry window' width height
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowResizeToGeometryMethodInfo
instance (signature ~ (Int32 -> Int32 -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowResizeToGeometryMethodInfo a signature where
    overloadedMethod = windowResizeToGeometry

instance O.OverloadedMethodInfo WindowResizeToGeometryMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowResizeToGeometry",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowResizeToGeometry"
        })


#endif

-- method Window::set_accept_focus
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "%TRUE to let this window receive input focus"
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

foreign import ccall "gtk_window_set_accept_focus" gtk_window_set_accept_focus :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | Windows may set a hint asking the desktop environment not to receive
-- the input focus. This function sets this hint.
-- 
-- /Since: 2.4/
windowSetAcceptFocus ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Bool
    -- ^ /@setting@/: 'P.True' to let this window receive input focus
    -> m ()
windowSetAcceptFocus window setting = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    let setting' = (fromIntegral . fromEnum) setting
    gtk_window_set_accept_focus window' setting'
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetAcceptFocusMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowSetAcceptFocusMethodInfo a signature where
    overloadedMethod = windowSetAcceptFocus

instance O.OverloadedMethodInfo WindowSetAcceptFocusMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetAcceptFocus",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetAcceptFocus"
        })


#endif

-- method Window::set_application
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "application"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Application" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkApplication, or %NULL to unset"
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

foreign import ccall "gtk_window_set_application" gtk_window_set_application :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    Ptr Gtk.Application.Application ->      -- application : TInterface (Name {namespace = "Gtk", name = "Application"})
    IO ()

-- | Sets or unsets the t'GI.Gtk.Objects.Application.Application' associated with the window.
-- 
-- The application will be kept alive for at least as long as it has any windows
-- associated with it (see 'GI.Gio.Objects.Application.applicationHold' for a way to keep it alive
-- without windows).
-- 
-- Normally, the connection between the application and the window will remain
-- until the window is destroyed, but you can explicitly remove it by setting
-- the /@application@/ to 'P.Nothing'.
-- 
-- This is equivalent to calling 'GI.Gtk.Objects.Application.applicationRemoveWindow' and\/or
-- 'GI.Gtk.Objects.Application.applicationAddWindow' on the old\/new applications as relevant.
-- 
-- /Since: 3.0/
windowSetApplication ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a, Gtk.Application.IsApplication b) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Maybe (b)
    -- ^ /@application@/: a t'GI.Gtk.Objects.Application.Application', or 'P.Nothing' to unset
    -> m ()
windowSetApplication window application = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    maybeApplication <- case application of
        Nothing -> return nullPtr
        Just jApplication -> do
            jApplication' <- unsafeManagedPtrCastPtr jApplication
            return jApplication'
    gtk_window_set_application window' maybeApplication
    touchManagedPtr window
    whenJust application touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetApplicationMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsWindow a, Gtk.Application.IsApplication b) => O.OverloadedMethod WindowSetApplicationMethodInfo a signature where
    overloadedMethod = windowSetApplication

instance O.OverloadedMethodInfo WindowSetApplicationMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetApplication",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetApplication"
        })


#endif

-- method Window::set_attached_to
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "attach_widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidget, or %NULL"
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

foreign import ccall "gtk_window_set_attached_to" gtk_window_set_attached_to :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    Ptr Gtk.Widget.Widget ->                -- attach_widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | Marks /@window@/ as attached to /@attachWidget@/. This creates a logical binding
-- between the window and the widget it belongs to, which is used by GTK+ to
-- propagate information such as styling or accessibility to /@window@/ as if it
-- was a children of /@attachWidget@/.
-- 
-- Examples of places where specifying this relation is useful are for instance
-- a t'GI.Gtk.Objects.Menu.Menu' created by a t'GI.Gtk.Objects.ComboBox.ComboBox', a completion popup window
-- created by t'GI.Gtk.Objects.Entry.Entry' or a typeahead search entry created by t'GI.Gtk.Objects.TreeView.TreeView'.
-- 
-- Note that this function should not be confused with
-- 'GI.Gtk.Objects.Window.windowSetTransientFor', which specifies a window manager relation
-- between two toplevels instead.
-- 
-- Passing 'P.Nothing' for /@attachWidget@/ detaches the window.
-- 
-- /Since: 3.4/
windowSetAttachedTo ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Maybe (b)
    -- ^ /@attachWidget@/: a t'GI.Gtk.Objects.Widget.Widget', or 'P.Nothing'
    -> m ()
windowSetAttachedTo window attachWidget = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    maybeAttachWidget <- case attachWidget of
        Nothing -> return nullPtr
        Just jAttachWidget -> do
            jAttachWidget' <- unsafeManagedPtrCastPtr jAttachWidget
            return jAttachWidget'
    gtk_window_set_attached_to window' maybeAttachWidget
    touchManagedPtr window
    whenJust attachWidget touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetAttachedToMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsWindow a, Gtk.Widget.IsWidget b) => O.OverloadedMethod WindowSetAttachedToMethodInfo a signature where
    overloadedMethod = windowSetAttachedTo

instance O.OverloadedMethodInfo WindowSetAttachedToMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetAttachedTo",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetAttachedTo"
        })


#endif

-- method Window::set_decorated
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "%TRUE to decorate the window"
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

foreign import ccall "gtk_window_set_decorated" gtk_window_set_decorated :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | By default, windows are decorated with a title bar, resize
-- controls, etc.  Some [window managers][gtk-X11-arch]
-- allow GTK+ to disable these decorations, creating a
-- borderless window. If you set the decorated property to 'P.False'
-- using this function, GTK+ will do its best to convince the window
-- manager not to decorate the window. Depending on the system, this
-- function may not have any effect when called on a window that is
-- already visible, so you should call it before calling 'GI.Gtk.Objects.Widget.widgetShow'.
-- 
-- On Windows, this function always works, since there’s no window manager
-- policy involved.
windowSetDecorated ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Bool
    -- ^ /@setting@/: 'P.True' to decorate the window
    -> m ()
windowSetDecorated window setting = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    let setting' = (fromIntegral . fromEnum) setting
    gtk_window_set_decorated window' setting'
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetDecoratedMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowSetDecoratedMethodInfo a signature where
    overloadedMethod = windowSetDecorated

instance O.OverloadedMethodInfo WindowSetDecoratedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetDecorated",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetDecorated"
        })


#endif

-- method Window::set_default
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "default_widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "widget to be the default, or %NULL\n    to unset the default widget for the toplevel"
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

foreign import ccall "gtk_window_set_default" gtk_window_set_default :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    Ptr Gtk.Widget.Widget ->                -- default_widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | The default widget is the widget that’s activated when the user
-- presses Enter in a dialog (for example). This function sets or
-- unsets the default widget for a t'GI.Gtk.Objects.Window.Window'. When setting (rather
-- than unsetting) the default widget it’s generally easier to call
-- 'GI.Gtk.Objects.Widget.widgetGrabDefault' on the widget. Before making a widget
-- the default widget, you must call 'GI.Gtk.Objects.Widget.widgetSetCanDefault' on
-- the widget you’d like to make the default.
windowSetDefault ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Maybe (b)
    -- ^ /@defaultWidget@/: widget to be the default, or 'P.Nothing'
    --     to unset the default widget for the toplevel
    -> m ()
windowSetDefault window defaultWidget = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    maybeDefaultWidget <- case defaultWidget of
        Nothing -> return nullPtr
        Just jDefaultWidget -> do
            jDefaultWidget' <- unsafeManagedPtrCastPtr jDefaultWidget
            return jDefaultWidget'
    gtk_window_set_default window' maybeDefaultWidget
    touchManagedPtr window
    whenJust defaultWidget touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetDefaultMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsWindow a, Gtk.Widget.IsWidget b) => O.OverloadedMethod WindowSetDefaultMethodInfo a signature where
    overloadedMethod = windowSetDefault

instance O.OverloadedMethodInfo WindowSetDefaultMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetDefault",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetDefault"
        })


#endif

-- method Window::set_default_geometry
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "width"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "width in resize increments, or -1 to unset the default width"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "height"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "height in resize increments, or -1 to unset the default height"
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

foreign import ccall "gtk_window_set_default_geometry" gtk_window_set_default_geometry :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    Int32 ->                                -- width : TBasicType TInt
    Int32 ->                                -- height : TBasicType TInt
    IO ()

{-# DEPRECATED windowSetDefaultGeometry ["(Since version 3.20)","This function does nothing. If you want to set a default","    size, use 'GI.Gtk.Objects.Window.windowSetDefaultSize' instead."] #-}
-- | Like 'GI.Gtk.Objects.Window.windowSetDefaultSize', but /@width@/ and /@height@/ are interpreted
-- in terms of the base size and increment set with
-- gtk_window_set_geometry_hints.
-- 
-- /Since: 3.0/
windowSetDefaultGeometry ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Int32
    -- ^ /@width@/: width in resize increments, or -1 to unset the default width
    -> Int32
    -- ^ /@height@/: height in resize increments, or -1 to unset the default height
    -> m ()
windowSetDefaultGeometry window width height = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    gtk_window_set_default_geometry window' width height
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetDefaultGeometryMethodInfo
instance (signature ~ (Int32 -> Int32 -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowSetDefaultGeometryMethodInfo a signature where
    overloadedMethod = windowSetDefaultGeometry

instance O.OverloadedMethodInfo WindowSetDefaultGeometryMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetDefaultGeometry",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetDefaultGeometry"
        })


#endif

-- method Window::set_default_size
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "width"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "width in pixels, or -1 to unset the default width"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "height"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "height in pixels, or -1 to unset the default height"
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

foreign import ccall "gtk_window_set_default_size" gtk_window_set_default_size :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    Int32 ->                                -- width : TBasicType TInt
    Int32 ->                                -- height : TBasicType TInt
    IO ()

-- | Sets the default size of a window. If the window’s “natural” size
-- (its size request) is larger than the default, the default will be
-- ignored. More generally, if the default size does not obey the
-- geometry hints for the window ('GI.Gtk.Objects.Window.windowSetGeometryHints' can
-- be used to set these explicitly), the default size will be clamped
-- to the nearest permitted size.
-- 
-- Unlike 'GI.Gtk.Objects.Widget.widgetSetSizeRequest', which sets a size request for
-- a widget and thus would keep users from shrinking the window, this
-- function only sets the initial size, just as if the user had
-- resized the window themselves. Users can still shrink the window
-- again as they normally would. Setting a default size of -1 means to
-- use the “natural” default size (the size request of the window).
-- 
-- For more control over a window’s initial size and how resizing works,
-- investigate 'GI.Gtk.Objects.Window.windowSetGeometryHints'.
-- 
-- For some uses, 'GI.Gtk.Objects.Window.windowResize' is a more appropriate function.
-- 'GI.Gtk.Objects.Window.windowResize' changes the current size of the window, rather
-- than the size to be used on initial display. 'GI.Gtk.Objects.Window.windowResize' always
-- affects the window itself, not the geometry widget.
-- 
-- The default size of a window only affects the first time a window is
-- shown; if a window is hidden and re-shown, it will remember the size
-- it had prior to hiding, rather than using the default size.
-- 
-- Windows can’t actually be 0x0 in size, they must be at least 1x1, but
-- passing 0 for /@width@/ and /@height@/ is OK, resulting in a 1x1 default size.
-- 
-- If you use this function to reestablish a previously saved window size,
-- note that the appropriate size to save is the one returned by
-- 'GI.Gtk.Objects.Window.windowGetSize'. Using the window allocation directly will not
-- work in all circumstances and can lead to growing or shrinking windows.
windowSetDefaultSize ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Int32
    -- ^ /@width@/: width in pixels, or -1 to unset the default width
    -> Int32
    -- ^ /@height@/: height in pixels, or -1 to unset the default height
    -> m ()
windowSetDefaultSize window width height = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    gtk_window_set_default_size window' width height
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetDefaultSizeMethodInfo
instance (signature ~ (Int32 -> Int32 -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowSetDefaultSizeMethodInfo a signature where
    overloadedMethod = windowSetDefaultSize

instance O.OverloadedMethodInfo WindowSetDefaultSizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetDefaultSize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetDefaultSize"
        })


#endif

-- method Window::set_deletable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "%TRUE to decorate the window as deletable"
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

foreign import ccall "gtk_window_set_deletable" gtk_window_set_deletable :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | By default, windows have a close button in the window frame. Some
-- [window managers][gtk-X11-arch] allow GTK+ to
-- disable this button. If you set the deletable property to 'P.False'
-- using this function, GTK+ will do its best to convince the window
-- manager not to show a close button. Depending on the system, this
-- function may not have any effect when called on a window that is
-- already visible, so you should call it before calling 'GI.Gtk.Objects.Widget.widgetShow'.
-- 
-- On Windows, this function always works, since there’s no window manager
-- policy involved.
-- 
-- /Since: 2.10/
windowSetDeletable ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Bool
    -- ^ /@setting@/: 'P.True' to decorate the window as deletable
    -> m ()
windowSetDeletable window setting = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    let setting' = (fromIntegral . fromEnum) setting
    gtk_window_set_deletable window' setting'
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetDeletableMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowSetDeletableMethodInfo a signature where
    overloadedMethod = windowSetDeletable

instance O.OverloadedMethodInfo WindowSetDeletableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetDeletable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetDeletable"
        })


#endif

-- method Window::set_destroy_with_parent
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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
--                     Just "whether to destroy @window with its transient parent"
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

foreign import ccall "gtk_window_set_destroy_with_parent" gtk_window_set_destroy_with_parent :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | If /@setting@/ is 'P.True', then destroying the transient parent of /@window@/
-- will also destroy /@window@/ itself. This is useful for dialogs that
-- shouldn’t persist beyond the lifetime of the main window they\'re
-- associated with, for example.
windowSetDestroyWithParent ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Bool
    -- ^ /@setting@/: whether to destroy /@window@/ with its transient parent
    -> m ()
windowSetDestroyWithParent window setting = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    let setting' = (fromIntegral . fromEnum) setting
    gtk_window_set_destroy_with_parent window' setting'
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetDestroyWithParentMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowSetDestroyWithParentMethodInfo a signature where
    overloadedMethod = windowSetDestroyWithParent

instance O.OverloadedMethodInfo WindowSetDestroyWithParentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetDestroyWithParent",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetDestroyWithParent"
        })


#endif

-- method Window::set_focus
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "focus"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "widget to be the new focus widget, or %NULL to unset\n  any focus widget for the toplevel window."
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

foreign import ccall "gtk_window_set_focus" gtk_window_set_focus :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    Ptr Gtk.Widget.Widget ->                -- focus : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | If /@focus@/ is not the current focus widget, and is focusable, sets
-- it as the focus widget for the window. If /@focus@/ is 'P.Nothing', unsets
-- the focus widget for this window. To set the focus to a particular
-- widget in the toplevel, it is usually more convenient to use
-- 'GI.Gtk.Objects.Widget.widgetGrabFocus' instead of this function.
windowSetFocus ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Maybe (b)
    -- ^ /@focus@/: widget to be the new focus widget, or 'P.Nothing' to unset
    --   any focus widget for the toplevel window.
    -> m ()
windowSetFocus window focus = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    maybeFocus <- case focus of
        Nothing -> return nullPtr
        Just jFocus -> do
            jFocus' <- unsafeManagedPtrCastPtr jFocus
            return jFocus'
    gtk_window_set_focus window' maybeFocus
    touchManagedPtr window
    whenJust focus touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetFocusMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsWindow a, Gtk.Widget.IsWidget b) => O.OverloadedMethod WindowSetFocusMethodInfo a signature where
    overloadedMethod = windowSetFocus

instance O.OverloadedMethodInfo WindowSetFocusMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetFocus",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetFocus"
        })


#endif

-- method Window::set_focus_on_map
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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
--                     Just "%TRUE to let this window receive input focus on map"
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

foreign import ccall "gtk_window_set_focus_on_map" gtk_window_set_focus_on_map :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | Windows may set a hint asking the desktop environment not to receive
-- the input focus when the window is mapped.  This function sets this
-- hint.
-- 
-- /Since: 2.6/
windowSetFocusOnMap ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Bool
    -- ^ /@setting@/: 'P.True' to let this window receive input focus on map
    -> m ()
windowSetFocusOnMap window setting = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    let setting' = (fromIntegral . fromEnum) setting
    gtk_window_set_focus_on_map window' setting'
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetFocusOnMapMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowSetFocusOnMapMethodInfo a signature where
    overloadedMethod = windowSetFocusOnMap

instance O.OverloadedMethodInfo WindowSetFocusOnMapMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetFocusOnMap",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetFocusOnMap"
        })


#endif

-- method Window::set_focus_visible
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_set_focus_visible" gtk_window_set_focus_visible :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | Sets the [Window:focusVisible]("GI.Gtk.Objects.Window#g:attr:focusVisible") property.
-- 
-- /Since: 3.2/
windowSetFocusVisible ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Bool
    -- ^ /@setting@/: the new value
    -> m ()
windowSetFocusVisible window setting = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    let setting' = (fromIntegral . fromEnum) setting
    gtk_window_set_focus_visible window' setting'
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetFocusVisibleMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowSetFocusVisibleMethodInfo a signature where
    overloadedMethod = windowSetFocusVisible

instance O.OverloadedMethodInfo WindowSetFocusVisibleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetFocusVisible",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetFocusVisible"
        })


#endif

-- method Window::set_geometry_hints
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "geometry_widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "widget the geometry hints used to be applied to\n  or %NULL. Since 3.20 this argument is ignored and GTK behaves as if %NULL was\n  set."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "geometry"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Geometry" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "struct containing geometry information or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "geom_mask"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "WindowHints" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "mask indicating which struct fields should be paid attention to"
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

foreign import ccall "gtk_window_set_geometry_hints" gtk_window_set_geometry_hints :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    Ptr Gtk.Widget.Widget ->                -- geometry_widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gdk.Geometry.Geometry ->            -- geometry : TInterface (Name {namespace = "Gdk", name = "Geometry"})
    CUInt ->                                -- geom_mask : TInterface (Name {namespace = "Gdk", name = "WindowHints"})
    IO ()

-- | This function sets up hints about how a window can be resized by
-- the user.  You can set a minimum and maximum size; allowed resize
-- increments (e.g. for xterm, you can only resize by the size of a
-- character); aspect ratios; and more. See the t'GI.Gdk.Structs.Geometry.Geometry' struct.
windowSetGeometryHints ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Maybe (b)
    -- ^ /@geometryWidget@/: widget the geometry hints used to be applied to
    --   or 'P.Nothing'. Since 3.20 this argument is ignored and GTK behaves as if 'P.Nothing' was
    --   set.
    -> Maybe (Gdk.Geometry.Geometry)
    -- ^ /@geometry@/: struct containing geometry information or 'P.Nothing'
    -> [Gdk.Flags.WindowHints]
    -- ^ /@geomMask@/: mask indicating which struct fields should be paid attention to
    -> m ()
windowSetGeometryHints window geometryWidget geometry geomMask = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    maybeGeometryWidget <- case geometryWidget of
        Nothing -> return nullPtr
        Just jGeometryWidget -> do
            jGeometryWidget' <- unsafeManagedPtrCastPtr jGeometryWidget
            return jGeometryWidget'
    maybeGeometry <- case geometry of
        Nothing -> return nullPtr
        Just jGeometry -> do
            jGeometry' <- unsafeManagedPtrGetPtr jGeometry
            return jGeometry'
    let geomMask' = gflagsToWord geomMask
    gtk_window_set_geometry_hints window' maybeGeometryWidget maybeGeometry geomMask'
    touchManagedPtr window
    whenJust geometryWidget touchManagedPtr
    whenJust geometry touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetGeometryHintsMethodInfo
instance (signature ~ (Maybe (b) -> Maybe (Gdk.Geometry.Geometry) -> [Gdk.Flags.WindowHints] -> m ()), MonadIO m, IsWindow a, Gtk.Widget.IsWidget b) => O.OverloadedMethod WindowSetGeometryHintsMethodInfo a signature where
    overloadedMethod = windowSetGeometryHints

instance O.OverloadedMethodInfo WindowSetGeometryHintsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetGeometryHints",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetGeometryHints"
        })


#endif

-- method Window::set_gravity
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "gravity"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Gravity" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "window gravity" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_set_gravity" gtk_window_set_gravity :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    CUInt ->                                -- gravity : TInterface (Name {namespace = "Gdk", name = "Gravity"})
    IO ()

-- | Window gravity defines the meaning of coordinates passed to
-- 'GI.Gtk.Objects.Window.windowMove'. See 'GI.Gtk.Objects.Window.windowMove' and t'GI.Gdk.Enums.Gravity' for
-- more details.
-- 
-- The default window gravity is @/GDK_GRAVITY_NORTH_WEST/@ which will
-- typically “do what you mean.”
windowSetGravity ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Gdk.Enums.Gravity
    -- ^ /@gravity@/: window gravity
    -> m ()
windowSetGravity window gravity = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    let gravity' = (fromIntegral . fromEnum) gravity
    gtk_window_set_gravity window' gravity'
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetGravityMethodInfo
instance (signature ~ (Gdk.Enums.Gravity -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowSetGravityMethodInfo a signature where
    overloadedMethod = windowSetGravity

instance O.OverloadedMethodInfo WindowSetGravityMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetGravity",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetGravity"
        })


#endif

-- method Window::set_has_resize_grip
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "value"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE to allow a resize grip"
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

foreign import ccall "gtk_window_set_has_resize_grip" gtk_window_set_has_resize_grip :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    CInt ->                                 -- value : TBasicType TBoolean
    IO ()

{-# DEPRECATED windowSetHasResizeGrip ["(Since version 3.14)","Resize grips have been removed."] #-}
-- | Sets whether /@window@/ has a corner resize grip.
-- 
-- Note that the resize grip is only shown if the window
-- is actually resizable and not maximized. Use
-- 'GI.Gtk.Objects.Window.windowResizeGripIsVisible' to find out if the
-- resize grip is currently shown.
-- 
-- /Since: 3.0/
windowSetHasResizeGrip ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Bool
    -- ^ /@value@/: 'P.True' to allow a resize grip
    -> m ()
windowSetHasResizeGrip window value = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    let value' = (fromIntegral . fromEnum) value
    gtk_window_set_has_resize_grip window' value'
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetHasResizeGripMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowSetHasResizeGripMethodInfo a signature where
    overloadedMethod = windowSetHasResizeGrip

instance O.OverloadedMethodInfo WindowSetHasResizeGripMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetHasResizeGrip",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetHasResizeGrip"
        })


#endif

-- method Window::set_has_user_ref_count
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_set_has_user_ref_count" gtk_window_set_has_user_ref_count :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | Tells GTK+ whether to drop its extra reference to the window
-- when 'GI.Gtk.Objects.Widget.widgetDestroy' is called.
-- 
-- This function is only exported for the benefit of language
-- bindings which may need to keep the window alive until their
-- wrapper object is garbage collected. There is no justification
-- for ever calling this function in an application.
-- 
-- /Since: 3.0/
windowSetHasUserRefCount ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Bool
    -- ^ /@setting@/: the new value
    -> m ()
windowSetHasUserRefCount window setting = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    let setting' = (fromIntegral . fromEnum) setting
    gtk_window_set_has_user_ref_count window' setting'
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetHasUserRefCountMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowSetHasUserRefCountMethodInfo a signature where
    overloadedMethod = windowSetHasUserRefCount

instance O.OverloadedMethodInfo WindowSetHasUserRefCountMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetHasUserRefCount",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetHasUserRefCount"
        })


#endif

-- method Window::set_hide_titlebar_when_maximized
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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
--                     Just "whether to hide the titlebar when @window is maximized"
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

foreign import ccall "gtk_window_set_hide_titlebar_when_maximized" gtk_window_set_hide_titlebar_when_maximized :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | If /@setting@/ is 'P.True', then /@window@/ will request that it’s titlebar
-- should be hidden when maximized.
-- This is useful for windows that don’t convey any information other
-- than the application name in the titlebar, to put the available
-- screen space to better use. If the underlying window system does not
-- support the request, the setting will not have any effect.
-- 
-- Note that custom titlebars set with 'GI.Gtk.Objects.Window.windowSetTitlebar' are
-- not affected by this. The application is in full control of their
-- content and visibility anyway.
-- 
-- /Since: 3.4/
windowSetHideTitlebarWhenMaximized ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Bool
    -- ^ /@setting@/: whether to hide the titlebar when /@window@/ is maximized
    -> m ()
windowSetHideTitlebarWhenMaximized window setting = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    let setting' = (fromIntegral . fromEnum) setting
    gtk_window_set_hide_titlebar_when_maximized window' setting'
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetHideTitlebarWhenMaximizedMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowSetHideTitlebarWhenMaximizedMethodInfo a signature where
    overloadedMethod = windowSetHideTitlebarWhenMaximized

instance O.OverloadedMethodInfo WindowSetHideTitlebarWhenMaximizedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetHideTitlebarWhenMaximized",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetHideTitlebarWhenMaximized"
        })


#endif

-- method Window::set_icon
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icon"
--           , argType =
--               TInterface Name { namespace = "GdkPixbuf" , name = "Pixbuf" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "icon image, or %NULL"
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

foreign import ccall "gtk_window_set_icon" gtk_window_set_icon :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    Ptr GdkPixbuf.Pixbuf.Pixbuf ->          -- icon : TInterface (Name {namespace = "GdkPixbuf", name = "Pixbuf"})
    IO ()

-- | Sets up the icon representing a t'GI.Gtk.Objects.Window.Window'. This icon is used when
-- the window is minimized (also known as iconified).  Some window
-- managers or desktop environments may also place it in the window
-- frame, or display it in other contexts. On others, the icon is not
-- used at all, so your mileage may vary.
-- 
-- The icon should be provided in whatever size it was naturally
-- drawn; that is, don’t scale the image before passing it to
-- GTK+. Scaling is postponed until the last minute, when the desired
-- final size is known, to allow best quality.
-- 
-- If you have your icon hand-drawn in multiple sizes, use
-- 'GI.Gtk.Objects.Window.windowSetIconList'. Then the best size will be used.
-- 
-- This function is equivalent to calling 'GI.Gtk.Objects.Window.windowSetIconList'
-- with a 1-element list.
-- 
-- See also 'GI.Gtk.Objects.Window.windowSetDefaultIconList' to set the icon
-- for all windows in your application in one go.
windowSetIcon ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a, GdkPixbuf.Pixbuf.IsPixbuf b) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Maybe (b)
    -- ^ /@icon@/: icon image, or 'P.Nothing'
    -> m ()
windowSetIcon window icon = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    maybeIcon <- case icon of
        Nothing -> return nullPtr
        Just jIcon -> do
            jIcon' <- unsafeManagedPtrCastPtr jIcon
            return jIcon'
    gtk_window_set_icon window' maybeIcon
    touchManagedPtr window
    whenJust icon touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetIconMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsWindow a, GdkPixbuf.Pixbuf.IsPixbuf b) => O.OverloadedMethod WindowSetIconMethodInfo a signature where
    overloadedMethod = windowSetIcon

instance O.OverloadedMethodInfo WindowSetIconMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetIcon",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetIcon"
        })


#endif

-- method Window::set_icon_from_file
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "filename"
--           , argType = TBasicType TFileName
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "location of icon file"
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
-- throws : True
-- Skip return : False

foreign import ccall "gtk_window_set_icon_from_file" gtk_window_set_icon_from_file :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    CString ->                              -- filename : TBasicType TFileName
    Ptr (Ptr GError) ->                     -- error
    IO CInt

-- | Sets the icon for /@window@/.
-- Warns on failure if /@err@/ is 'P.Nothing'.
-- 
-- This function is equivalent to calling 'GI.Gtk.Objects.Window.windowSetIcon'
-- with a pixbuf created by loading the image from /@filename@/.
-- 
-- /Since: 2.2/
windowSetIconFromFile ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> [Char]
    -- ^ /@filename@/: location of icon file
    -> m ()
    -- ^ /(Can throw 'Data.GI.Base.GError.GError')/
windowSetIconFromFile window filename = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    filename' <- stringToCString filename
    onException (do
        _ <- propagateGError $ gtk_window_set_icon_from_file window' filename'
        touchManagedPtr window
        freeMem filename'
        return ()
     ) (do
        freeMem filename'
     )

#if defined(ENABLE_OVERLOADING)
data WindowSetIconFromFileMethodInfo
instance (signature ~ ([Char] -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowSetIconFromFileMethodInfo a signature where
    overloadedMethod = windowSetIconFromFile

instance O.OverloadedMethodInfo WindowSetIconFromFileMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetIconFromFile",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetIconFromFile"
        })


#endif

-- method Window::set_icon_list
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "list"
--           , argType =
--               TGList
--                 (TInterface Name { namespace = "GdkPixbuf" , name = "Pixbuf" })
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "list of #GdkPixbuf" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_set_icon_list" gtk_window_set_icon_list :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    Ptr (GList (Ptr GdkPixbuf.Pixbuf.Pixbuf)) -> -- list : TGList (TInterface (Name {namespace = "GdkPixbuf", name = "Pixbuf"}))
    IO ()

-- | Sets up the icon representing a t'GI.Gtk.Objects.Window.Window'. The icon is used when
-- the window is minimized (also known as iconified).  Some window
-- managers or desktop environments may also place it in the window
-- frame, or display it in other contexts. On others, the icon is not
-- used at all, so your mileage may vary.
-- 
-- 'GI.Gtk.Objects.Window.windowSetIconList' allows you to pass in the same icon in
-- several hand-drawn sizes. The list should contain the natural sizes
-- your icon is available in; that is, don’t scale the image before
-- passing it to GTK+. Scaling is postponed until the last minute,
-- when the desired final size is known, to allow best quality.
-- 
-- By passing several sizes, you may improve the final image quality
-- of the icon, by reducing or eliminating automatic image scaling.
-- 
-- Recommended sizes to provide: 16x16, 32x32, 48x48 at minimum, and
-- larger images (64x64, 128x128) if you have them.
-- 
-- See also 'GI.Gtk.Objects.Window.windowSetDefaultIconList' to set the icon
-- for all windows in your application in one go.
-- 
-- Note that transient windows (those who have been set transient for another
-- window using 'GI.Gtk.Objects.Window.windowSetTransientFor') will inherit their
-- icon from their transient parent. So there’s no need to explicitly
-- set the icon on transient windows.
windowSetIconList ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a, GdkPixbuf.Pixbuf.IsPixbuf b) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> [b]
    -- ^ /@list@/: list of t'GI.GdkPixbuf.Objects.Pixbuf.Pixbuf'
    -> m ()
windowSetIconList window list = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    list' <- mapM unsafeManagedPtrCastPtr list
    list'' <- packGList list'
    gtk_window_set_icon_list window' list''
    touchManagedPtr window
    mapM_ touchManagedPtr list
    g_list_free list''
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetIconListMethodInfo
instance (signature ~ ([b] -> m ()), MonadIO m, IsWindow a, GdkPixbuf.Pixbuf.IsPixbuf b) => O.OverloadedMethod WindowSetIconListMethodInfo a signature where
    overloadedMethod = windowSetIconList

instance O.OverloadedMethodInfo WindowSetIconListMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetIconList",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetIconList"
        })


#endif

-- method Window::set_icon_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the name of the themed icon"
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

foreign import ccall "gtk_window_set_icon_name" gtk_window_set_icon_name :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    CString ->                              -- name : TBasicType TUTF8
    IO ()

-- | Sets the icon for the window from a named themed icon.
-- See the docs for t'GI.Gtk.Objects.IconTheme.IconTheme' for more details.
-- On some platforms, the window icon is not used at all.
-- 
-- Note that this has nothing to do with the WM_ICON_NAME
-- property which is mentioned in the ICCCM.
-- 
-- /Since: 2.6/
windowSetIconName ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Maybe (T.Text)
    -- ^ /@name@/: the name of the themed icon
    -> m ()
windowSetIconName window name = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    maybeName <- case name of
        Nothing -> return nullPtr
        Just jName -> do
            jName' <- textToCString jName
            return jName'
    gtk_window_set_icon_name window' maybeName
    touchManagedPtr window
    freeMem maybeName
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetIconNameMethodInfo
instance (signature ~ (Maybe (T.Text) -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowSetIconNameMethodInfo a signature where
    overloadedMethod = windowSetIconName

instance O.OverloadedMethodInfo WindowSetIconNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetIconName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetIconName"
        })


#endif

-- method Window::set_keep_above
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "whether to keep @window above other windows"
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

foreign import ccall "gtk_window_set_keep_above" gtk_window_set_keep_above :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | Asks to keep /@window@/ above, so that it stays on top. Note that
-- you shouldn’t assume the window is definitely above afterward,
-- because other entities (e.g. the user or
-- [window manager][gtk-X11-arch]) could not keep it above,
-- and not all window managers support keeping windows above. But
-- normally the window will end kept above. Just don’t write code
-- that crashes if not.
-- 
-- It’s permitted to call this function before showing a window,
-- in which case the window will be kept above when it appears onscreen
-- initially.
-- 
-- You can track the above state via the “window-state-event” signal
-- on t'GI.Gtk.Objects.Widget.Widget'.
-- 
-- Note that, according to the
-- <http://www.freedesktop.org/Standards/wm-spec Extended Window Manager Hints Specification>,
-- the above state is mainly meant for user preferences and should not
-- be used by applications e.g. for drawing attention to their
-- dialogs.
-- 
-- /Since: 2.4/
windowSetKeepAbove ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Bool
    -- ^ /@setting@/: whether to keep /@window@/ above other windows
    -> m ()
windowSetKeepAbove window setting = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    let setting' = (fromIntegral . fromEnum) setting
    gtk_window_set_keep_above window' setting'
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetKeepAboveMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowSetKeepAboveMethodInfo a signature where
    overloadedMethod = windowSetKeepAbove

instance O.OverloadedMethodInfo WindowSetKeepAboveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetKeepAbove",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetKeepAbove"
        })


#endif

-- method Window::set_keep_below
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "whether to keep @window below other windows"
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

foreign import ccall "gtk_window_set_keep_below" gtk_window_set_keep_below :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | Asks to keep /@window@/ below, so that it stays in bottom. Note that
-- you shouldn’t assume the window is definitely below afterward,
-- because other entities (e.g. the user or
-- [window manager][gtk-X11-arch]) could not keep it below,
-- and not all window managers support putting windows below. But
-- normally the window will be kept below. Just don’t write code
-- that crashes if not.
-- 
-- It’s permitted to call this function before showing a window,
-- in which case the window will be kept below when it appears onscreen
-- initially.
-- 
-- You can track the below state via the “window-state-event” signal
-- on t'GI.Gtk.Objects.Widget.Widget'.
-- 
-- Note that, according to the
-- <http://www.freedesktop.org/Standards/wm-spec Extended Window Manager Hints Specification>,
-- the above state is mainly meant for user preferences and should not
-- be used by applications e.g. for drawing attention to their
-- dialogs.
-- 
-- /Since: 2.4/
windowSetKeepBelow ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Bool
    -- ^ /@setting@/: whether to keep /@window@/ below other windows
    -> m ()
windowSetKeepBelow window setting = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    let setting' = (fromIntegral . fromEnum) setting
    gtk_window_set_keep_below window' setting'
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetKeepBelowMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowSetKeepBelowMethodInfo a signature where
    overloadedMethod = windowSetKeepBelow

instance O.OverloadedMethodInfo WindowSetKeepBelowMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetKeepBelow",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetKeepBelow"
        })


#endif

-- method Window::set_mnemonic_modifier
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "modifier"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "ModifierType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the modifier mask used to activate\n              mnemonics on this window."
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

foreign import ccall "gtk_window_set_mnemonic_modifier" gtk_window_set_mnemonic_modifier :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    CUInt ->                                -- modifier : TInterface (Name {namespace = "Gdk", name = "ModifierType"})
    IO ()

-- | Sets the mnemonic modifier for this window.
windowSetMnemonicModifier ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> [Gdk.Flags.ModifierType]
    -- ^ /@modifier@/: the modifier mask used to activate
    --               mnemonics on this window.
    -> m ()
windowSetMnemonicModifier window modifier = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    let modifier' = gflagsToWord modifier
    gtk_window_set_mnemonic_modifier window' modifier'
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetMnemonicModifierMethodInfo
instance (signature ~ ([Gdk.Flags.ModifierType] -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowSetMnemonicModifierMethodInfo a signature where
    overloadedMethod = windowSetMnemonicModifier

instance O.OverloadedMethodInfo WindowSetMnemonicModifierMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetMnemonicModifier",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetMnemonicModifier"
        })


#endif

-- method Window::set_mnemonics_visible
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_set_mnemonics_visible" gtk_window_set_mnemonics_visible :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | Sets the [Window:mnemonicsVisible]("GI.Gtk.Objects.Window#g:attr:mnemonicsVisible") property.
-- 
-- /Since: 2.20/
windowSetMnemonicsVisible ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Bool
    -- ^ /@setting@/: the new value
    -> m ()
windowSetMnemonicsVisible window setting = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    let setting' = (fromIntegral . fromEnum) setting
    gtk_window_set_mnemonics_visible window' setting'
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetMnemonicsVisibleMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowSetMnemonicsVisibleMethodInfo a signature where
    overloadedMethod = windowSetMnemonicsVisible

instance O.OverloadedMethodInfo WindowSetMnemonicsVisibleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetMnemonicsVisible",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetMnemonicsVisible"
        })


#endif

-- method Window::set_modal
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "modal"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "whether the window is modal"
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

foreign import ccall "gtk_window_set_modal" gtk_window_set_modal :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    CInt ->                                 -- modal : TBasicType TBoolean
    IO ()

-- | Sets a window modal or non-modal. Modal windows prevent interaction
-- with other windows in the same application. To keep modal dialogs
-- on top of main application windows, use
-- 'GI.Gtk.Objects.Window.windowSetTransientFor' to make the dialog transient for the
-- parent; most [window managers][gtk-X11-arch]
-- will then disallow lowering the dialog below the parent.
windowSetModal ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Bool
    -- ^ /@modal@/: whether the window is modal
    -> m ()
windowSetModal window modal = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    let modal' = (fromIntegral . fromEnum) modal
    gtk_window_set_modal window' modal'
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetModalMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowSetModalMethodInfo a signature where
    overloadedMethod = windowSetModal

instance O.OverloadedMethodInfo WindowSetModalMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetModal",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetModal"
        })


#endif

-- method Window::set_opacity
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "opacity"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "desired opacity, between 0 and 1"
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

foreign import ccall "gtk_window_set_opacity" gtk_window_set_opacity :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    CDouble ->                              -- opacity : TBasicType TDouble
    IO ()

{-# DEPRECATED windowSetOpacity ["(Since version 3.8)","Use gtk_widget_set_opacity instead."] #-}
-- | Request the windowing system to make /@window@/ partially transparent,
-- with opacity 0 being fully transparent and 1 fully opaque. (Values
-- of the opacity parameter are clamped to the [0,1] range.) On X11
-- this has any effect only on X screens with a compositing manager
-- running. See 'GI.Gtk.Objects.Widget.widgetIsComposited'. On Windows it should work
-- always.
-- 
-- Note that setting a window’s opacity after the window has been
-- shown causes it to flicker once on Windows.
-- 
-- /Since: 2.12/
windowSetOpacity ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Double
    -- ^ /@opacity@/: desired opacity, between 0 and 1
    -> m ()
windowSetOpacity window opacity = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    let opacity' = realToFrac opacity
    gtk_window_set_opacity window' opacity'
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetOpacityMethodInfo
instance (signature ~ (Double -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowSetOpacityMethodInfo a signature where
    overloadedMethod = windowSetOpacity

instance O.OverloadedMethodInfo WindowSetOpacityMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetOpacity",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetOpacity"
        })


#endif

-- method Window::set_position
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "position"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WindowPosition" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a position constraint."
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

foreign import ccall "gtk_window_set_position" gtk_window_set_position :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    CUInt ->                                -- position : TInterface (Name {namespace = "Gtk", name = "WindowPosition"})
    IO ()

-- | Sets a position constraint for this window. If the old or new
-- constraint is 'GI.Gtk.Enums.WindowPositionCenterAlways', this will also cause
-- the window to be repositioned to satisfy the new constraint.
windowSetPosition ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'.
    -> Gtk.Enums.WindowPosition
    -- ^ /@position@/: a position constraint.
    -> m ()
windowSetPosition window position = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    let position' = (fromIntegral . fromEnum) position
    gtk_window_set_position window' position'
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetPositionMethodInfo
instance (signature ~ (Gtk.Enums.WindowPosition -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowSetPositionMethodInfo a signature where
    overloadedMethod = windowSetPosition

instance O.OverloadedMethodInfo WindowSetPositionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetPosition",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetPosition"
        })


#endif

-- method Window::set_resizable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "resizable"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE if the user can resize this window"
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

foreign import ccall "gtk_window_set_resizable" gtk_window_set_resizable :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    CInt ->                                 -- resizable : TBasicType TBoolean
    IO ()

-- | Sets whether the user can resize a window. Windows are user resizable
-- by default.
windowSetResizable ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Bool
    -- ^ /@resizable@/: 'P.True' if the user can resize this window
    -> m ()
windowSetResizable window resizable = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    let resizable' = (fromIntegral . fromEnum) resizable
    gtk_window_set_resizable window' resizable'
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetResizableMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowSetResizableMethodInfo a signature where
    overloadedMethod = windowSetResizable

instance O.OverloadedMethodInfo WindowSetResizableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetResizable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetResizable"
        })


#endif

-- method Window::set_role
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "role"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "unique identifier for the window to be used when restoring a session"
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

foreign import ccall "gtk_window_set_role" gtk_window_set_role :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    CString ->                              -- role : TBasicType TUTF8
    IO ()

-- | This function is only useful on X11, not with other GTK+ targets.
-- 
-- In combination with the window title, the window role allows a
-- [window manager][gtk-X11-arch] to identify \"the
-- same\" window when an application is restarted. So for example you
-- might set the “toolbox” role on your app’s toolbox window, so that
-- when the user restarts their session, the window manager can put
-- the toolbox back in the same place.
-- 
-- If a window already has a unique title, you don’t need to set the
-- role, since the WM can use the title to identify the window when
-- restoring the session.
windowSetRole ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> T.Text
    -- ^ /@role@/: unique identifier for the window to be used when restoring a session
    -> m ()
windowSetRole window role = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    role' <- textToCString role
    gtk_window_set_role window' role'
    touchManagedPtr window
    freeMem role'
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetRoleMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowSetRoleMethodInfo a signature where
    overloadedMethod = windowSetRole

instance O.OverloadedMethodInfo WindowSetRoleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetRole",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetRole"
        })


#endif

-- method Window::set_screen
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "screen"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Screen" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkScreen." , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_set_screen" gtk_window_set_screen :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    Ptr Gdk.Screen.Screen ->                -- screen : TInterface (Name {namespace = "Gdk", name = "Screen"})
    IO ()

-- | Sets the t'GI.Gdk.Objects.Screen.Screen' where the /@window@/ is displayed; if
-- the window is already mapped, it will be unmapped, and
-- then remapped on the new screen.
-- 
-- /Since: 2.2/
windowSetScreen ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a, Gdk.Screen.IsScreen b) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'.
    -> b
    -- ^ /@screen@/: a t'GI.Gdk.Objects.Screen.Screen'.
    -> m ()
windowSetScreen window screen = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    screen' <- unsafeManagedPtrCastPtr screen
    gtk_window_set_screen window' screen'
    touchManagedPtr window
    touchManagedPtr screen
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetScreenMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsWindow a, Gdk.Screen.IsScreen b) => O.OverloadedMethod WindowSetScreenMethodInfo a signature where
    overloadedMethod = windowSetScreen

instance O.OverloadedMethodInfo WindowSetScreenMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetScreen",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetScreen"
        })


#endif

-- method Window::set_skip_pager_hint
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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
--                     Just "%TRUE to keep this window from appearing in the pager"
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

foreign import ccall "gtk_window_set_skip_pager_hint" gtk_window_set_skip_pager_hint :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | Windows may set a hint asking the desktop environment not to display
-- the window in the pager. This function sets this hint.
-- (A \"pager\" is any desktop navigation tool such as a workspace
-- switcher that displays a thumbnail representation of the windows
-- on the screen.)
-- 
-- /Since: 2.2/
windowSetSkipPagerHint ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Bool
    -- ^ /@setting@/: 'P.True' to keep this window from appearing in the pager
    -> m ()
windowSetSkipPagerHint window setting = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    let setting' = (fromIntegral . fromEnum) setting
    gtk_window_set_skip_pager_hint window' setting'
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetSkipPagerHintMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowSetSkipPagerHintMethodInfo a signature where
    overloadedMethod = windowSetSkipPagerHint

instance O.OverloadedMethodInfo WindowSetSkipPagerHintMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetSkipPagerHint",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetSkipPagerHint"
        })


#endif

-- method Window::set_skip_taskbar_hint
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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
--                     Just "%TRUE to keep this window from appearing in the task bar"
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

foreign import ccall "gtk_window_set_skip_taskbar_hint" gtk_window_set_skip_taskbar_hint :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | Windows may set a hint asking the desktop environment not to display
-- the window in the task bar. This function sets this hint.
-- 
-- /Since: 2.2/
windowSetSkipTaskbarHint ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Bool
    -- ^ /@setting@/: 'P.True' to keep this window from appearing in the task bar
    -> m ()
windowSetSkipTaskbarHint window setting = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    let setting' = (fromIntegral . fromEnum) setting
    gtk_window_set_skip_taskbar_hint window' setting'
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetSkipTaskbarHintMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowSetSkipTaskbarHintMethodInfo a signature where
    overloadedMethod = windowSetSkipTaskbarHint

instance O.OverloadedMethodInfo WindowSetSkipTaskbarHintMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetSkipTaskbarHint",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetSkipTaskbarHint"
        })


#endif

-- method Window::set_startup_id
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "startup_id"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a string with startup-notification identifier"
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

foreign import ccall "gtk_window_set_startup_id" gtk_window_set_startup_id :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    CString ->                              -- startup_id : TBasicType TUTF8
    IO ()

-- | Startup notification identifiers are used by desktop environment to
-- track application startup, to provide user feedback and other
-- features. This function changes the corresponding property on the
-- underlying GdkWindow. Normally, startup identifier is managed
-- automatically and you should only use this function in special cases
-- like transferring focus from other processes. You should use this
-- function before calling 'GI.Gtk.Objects.Window.windowPresent' or any equivalent
-- function generating a window map event.
-- 
-- This function is only useful on X11, not with other GTK+ targets.
-- 
-- /Since: 2.12/
windowSetStartupId ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> T.Text
    -- ^ /@startupId@/: a string with startup-notification identifier
    -> m ()
windowSetStartupId window startupId = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    startupId' <- textToCString startupId
    gtk_window_set_startup_id window' startupId'
    touchManagedPtr window
    freeMem startupId'
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetStartupIdMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowSetStartupIdMethodInfo a signature where
    overloadedMethod = windowSetStartupId

instance O.OverloadedMethodInfo WindowSetStartupIdMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetStartupId",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetStartupId"
        })


#endif

-- method Window::set_title
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "title of the window"
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

foreign import ccall "gtk_window_set_title" gtk_window_set_title :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    CString ->                              -- title : TBasicType TUTF8
    IO ()

-- | Sets the title of the t'GI.Gtk.Objects.Window.Window'. The title of a window will be
-- displayed in its title bar; on the X Window System, the title bar
-- is rendered by the [window manager][gtk-X11-arch],
-- so exactly how the title appears to users may vary
-- according to a user’s exact configuration. The title should help a
-- user distinguish this window from other windows they may have
-- open. A good title might include the application name and current
-- document filename, for example.
windowSetTitle ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> T.Text
    -- ^ /@title@/: title of the window
    -> m ()
windowSetTitle window title = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    title' <- textToCString title
    gtk_window_set_title window' title'
    touchManagedPtr window
    freeMem title'
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetTitleMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowSetTitleMethodInfo a signature where
    overloadedMethod = windowSetTitle

instance O.OverloadedMethodInfo WindowSetTitleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetTitle",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetTitle"
        })


#endif

-- method Window::set_titlebar
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "titlebar"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the widget to use as titlebar"
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

foreign import ccall "gtk_window_set_titlebar" gtk_window_set_titlebar :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    Ptr Gtk.Widget.Widget ->                -- titlebar : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | Sets a custom titlebar for /@window@/.
-- 
-- A typical widget used here is t'GI.Gtk.Objects.HeaderBar.HeaderBar', as it provides various features
-- expected of a titlebar while allowing the addition of child widgets to it.
-- 
-- If you set a custom titlebar, GTK+ will do its best to convince
-- the window manager not to put its own titlebar on the window.
-- Depending on the system, this function may not work for a window
-- that is already visible, so you set the titlebar before calling
-- 'GI.Gtk.Objects.Widget.widgetShow'.
-- 
-- /Since: 3.10/
windowSetTitlebar ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Maybe (b)
    -- ^ /@titlebar@/: the widget to use as titlebar
    -> m ()
windowSetTitlebar window titlebar = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    maybeTitlebar <- case titlebar of
        Nothing -> return nullPtr
        Just jTitlebar -> do
            jTitlebar' <- unsafeManagedPtrCastPtr jTitlebar
            return jTitlebar'
    gtk_window_set_titlebar window' maybeTitlebar
    touchManagedPtr window
    whenJust titlebar touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetTitlebarMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsWindow a, Gtk.Widget.IsWidget b) => O.OverloadedMethod WindowSetTitlebarMethodInfo a signature where
    overloadedMethod = windowSetTitlebar

instance O.OverloadedMethodInfo WindowSetTitlebarMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetTitlebar",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetTitlebar"
        })


#endif

-- method Window::set_transient_for
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "parent"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "parent window, or %NULL"
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

foreign import ccall "gtk_window_set_transient_for" gtk_window_set_transient_for :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    Ptr Window ->                           -- parent : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO ()

-- | Dialog windows should be set transient for the main application
-- window they were spawned from. This allows
-- [window managers][gtk-X11-arch] to e.g. keep the
-- dialog on top of the main window, or center the dialog over the
-- main window. @/gtk_dialog_new_with_buttons()/@ and other convenience
-- functions in GTK+ will sometimes call
-- 'GI.Gtk.Objects.Window.windowSetTransientFor' on your behalf.
-- 
-- Passing 'P.Nothing' for /@parent@/ unsets the current transient window.
-- 
-- On Wayland, this function can also be used to attach a new
-- @/GTK_WINDOW_POPUP/@ to a @/GTK_WINDOW_TOPLEVEL/@ parent already mapped
-- on screen so that the @/GTK_WINDOW_POPUP/@ will be created as a
-- subsurface-based window @/GDK_WINDOW_SUBSURFACE/@ which can be
-- positioned at will relatively to the @/GTK_WINDOW_TOPLEVEL/@ surface.
-- 
-- On Windows, this function puts the child window on top of the parent,
-- much as the window manager would have done on X.
windowSetTransientFor ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a, IsWindow b) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Maybe (b)
    -- ^ /@parent@/: parent window, or 'P.Nothing'
    -> m ()
windowSetTransientFor window parent = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    maybeParent <- case parent of
        Nothing -> return nullPtr
        Just jParent -> do
            jParent' <- unsafeManagedPtrCastPtr jParent
            return jParent'
    gtk_window_set_transient_for window' maybeParent
    touchManagedPtr window
    whenJust parent touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetTransientForMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsWindow a, IsWindow b) => O.OverloadedMethod WindowSetTransientForMethodInfo a signature where
    overloadedMethod = windowSetTransientFor

instance O.OverloadedMethodInfo WindowSetTransientForMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetTransientFor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetTransientFor"
        })


#endif

-- method Window::set_type_hint
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "hint"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "WindowTypeHint" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the window type" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_set_type_hint" gtk_window_set_type_hint :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    CUInt ->                                -- hint : TInterface (Name {namespace = "Gdk", name = "WindowTypeHint"})
    IO ()

-- | By setting the type hint for the window, you allow the window
-- manager to decorate and handle the window in a way which is
-- suitable to the function of the window in your application.
-- 
-- This function should be called before the window becomes visible.
-- 
-- @/gtk_dialog_new_with_buttons()/@ and other convenience functions in GTK+
-- will sometimes call 'GI.Gtk.Objects.Window.windowSetTypeHint' on your behalf.
windowSetTypeHint ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Gdk.Enums.WindowTypeHint
    -- ^ /@hint@/: the window type
    -> m ()
windowSetTypeHint window hint = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    let hint' = (fromIntegral . fromEnum) hint
    gtk_window_set_type_hint window' hint'
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetTypeHintMethodInfo
instance (signature ~ (Gdk.Enums.WindowTypeHint -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowSetTypeHintMethodInfo a signature where
    overloadedMethod = windowSetTypeHint

instance O.OverloadedMethodInfo WindowSetTypeHintMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetTypeHint",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetTypeHint"
        })


#endif

-- method Window::set_urgency_hint
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "%TRUE to mark this window as urgent"
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

foreign import ccall "gtk_window_set_urgency_hint" gtk_window_set_urgency_hint :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | Windows may set a hint asking the desktop environment to draw
-- the users attention to the window. This function sets this hint.
-- 
-- /Since: 2.8/
windowSetUrgencyHint ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> Bool
    -- ^ /@setting@/: 'P.True' to mark this window as urgent
    -> m ()
windowSetUrgencyHint window setting = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    let setting' = (fromIntegral . fromEnum) setting
    gtk_window_set_urgency_hint window' setting'
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetUrgencyHintMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowSetUrgencyHintMethodInfo a signature where
    overloadedMethod = windowSetUrgencyHint

instance O.OverloadedMethodInfo WindowSetUrgencyHintMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetUrgencyHint",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetUrgencyHint"
        })


#endif

-- method Window::set_wmclass
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "wmclass_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "window name hint" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "wmclass_class"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "window class hint" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_set_wmclass" gtk_window_set_wmclass :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    CString ->                              -- wmclass_name : TBasicType TUTF8
    CString ->                              -- wmclass_class : TBasicType TUTF8
    IO ()

{-# DEPRECATED windowSetWmclass ["(Since version 3.22)"] #-}
-- | Don’t use this function. It sets the X Window System “class” and
-- “name” hints for a window.  According to the ICCCM, you should
-- always set these to the same value for all windows in an
-- application, and GTK+ sets them to that value by default, so calling
-- this function is sort of pointless. However, you may want to call
-- 'GI.Gtk.Objects.Window.windowSetRole' on each window in your application, for the
-- benefit of the session manager. Setting the role allows the window
-- manager to restore window positions when loading a saved session.
windowSetWmclass ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> T.Text
    -- ^ /@wmclassName@/: window name hint
    -> T.Text
    -- ^ /@wmclassClass@/: window class hint
    -> m ()
windowSetWmclass window wmclassName wmclassClass = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    wmclassName' <- textToCString wmclassName
    wmclassClass' <- textToCString wmclassClass
    gtk_window_set_wmclass window' wmclassName' wmclassClass'
    touchManagedPtr window
    freeMem wmclassName'
    freeMem wmclassClass'
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowSetWmclassMethodInfo
instance (signature ~ (T.Text -> T.Text -> m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowSetWmclassMethodInfo a signature where
    overloadedMethod = windowSetWmclass

instance O.OverloadedMethodInfo WindowSetWmclassMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowSetWmclass",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowSetWmclass"
        })


#endif

-- method Window::stick
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_stick" gtk_window_stick :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO ()

-- | Asks to stick /@window@/, which means that it will appear on all user
-- desktops. Note that you shouldn’t assume the window is definitely
-- stuck afterward, because other entities (e.g. the user or
-- [window manager][gtk-X11-arch] could unstick it
-- again, and some window managers do not support sticking
-- windows. But normally the window will end up stuck. Just don\'t
-- write code that crashes if not.
-- 
-- It’s permitted to call this function before showing a window.
-- 
-- You can track stickiness via the “window-state-event” signal
-- on t'GI.Gtk.Objects.Widget.Widget'.
windowStick ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m ()
windowStick window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    gtk_window_stick window'
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowStickMethodInfo
instance (signature ~ (m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowStickMethodInfo a signature where
    overloadedMethod = windowStick

instance O.OverloadedMethodInfo WindowStickMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowStick",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowStick"
        })


#endif

-- method Window::unfullscreen
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_unfullscreen" gtk_window_unfullscreen :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO ()

-- | Asks to toggle off the fullscreen state for /@window@/. Note that you
-- shouldn’t assume the window is definitely not full screen
-- afterward, because other entities (e.g. the user or
-- [window manager][gtk-X11-arch]) could fullscreen it
-- again, and not all window managers honor requests to unfullscreen
-- windows. But normally the window will end up restored to its normal
-- state. Just don’t write code that crashes if not.
-- 
-- You can track the fullscreen state via the “window-state-event” signal
-- on t'GI.Gtk.Objects.Widget.Widget'.
-- 
-- /Since: 2.2/
windowUnfullscreen ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m ()
windowUnfullscreen window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    gtk_window_unfullscreen window'
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowUnfullscreenMethodInfo
instance (signature ~ (m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowUnfullscreenMethodInfo a signature where
    overloadedMethod = windowUnfullscreen

instance O.OverloadedMethodInfo WindowUnfullscreenMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowUnfullscreen",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowUnfullscreen"
        })


#endif

-- method Window::unmaximize
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_unmaximize" gtk_window_unmaximize :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO ()

-- | Asks to unmaximize /@window@/. Note that you shouldn’t assume the
-- window is definitely unmaximized afterward, because other entities
-- (e.g. the user or [window manager][gtk-X11-arch])
-- could maximize it again, and not all window
-- managers honor requests to unmaximize. But normally the window will
-- end up unmaximized. Just don’t write code that crashes if not.
-- 
-- You can track maximization via the “window-state-event” signal
-- on t'GI.Gtk.Objects.Widget.Widget'.
windowUnmaximize ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m ()
windowUnmaximize window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    gtk_window_unmaximize window'
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowUnmaximizeMethodInfo
instance (signature ~ (m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowUnmaximizeMethodInfo a signature where
    overloadedMethod = windowUnmaximize

instance O.OverloadedMethodInfo WindowUnmaximizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowUnmaximize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowUnmaximize"
        })


#endif

-- method Window::unstick
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindow" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_unstick" gtk_window_unstick :: 
    Ptr Window ->                           -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO ()

-- | Asks to unstick /@window@/, which means that it will appear on only
-- one of the user’s desktops. Note that you shouldn’t assume the
-- window is definitely unstuck afterward, because other entities
-- (e.g. the user or [window manager][gtk-X11-arch]) could
-- stick it again. But normally the window will
-- end up unstuck. Just don’t write code that crashes if not.
-- 
-- You can track stickiness via the “window-state-event” signal
-- on t'GI.Gtk.Objects.Widget.Widget'.
windowUnstick ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindow a) =>
    a
    -- ^ /@window@/: a t'GI.Gtk.Objects.Window.Window'
    -> m ()
windowUnstick window = liftIO $ do
    window' <- unsafeManagedPtrCastPtr window
    gtk_window_unstick window'
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowUnstickMethodInfo
instance (signature ~ (m ()), MonadIO m, IsWindow a) => O.OverloadedMethod WindowUnstickMethodInfo a signature where
    overloadedMethod = windowUnstick

instance O.OverloadedMethodInfo WindowUnstickMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Window.windowUnstick",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Window.html#v:windowUnstick"
        })


#endif

-- method Window::get_default_icon_list
-- method type : MemberFunction
-- Args: []
-- Lengths: []
-- returnType: Just
--               (TGList
--                  (TInterface Name { namespace = "GdkPixbuf" , name = "Pixbuf" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_window_get_default_icon_list" gtk_window_get_default_icon_list :: 
    IO (Ptr (GList (Ptr GdkPixbuf.Pixbuf.Pixbuf)))

-- | Gets the value set by 'GI.Gtk.Objects.Window.windowSetDefaultIconList'.
-- The list is a copy and should be freed with @/g_list_free()/@,
-- but the pixbufs in the list have not had their reference count
-- incremented.
windowGetDefaultIconList ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m [GdkPixbuf.Pixbuf.Pixbuf]
    -- ^ __Returns:__ copy of default icon list
windowGetDefaultIconList  = liftIO $ do
    result <- gtk_window_get_default_icon_list
    result' <- unpackGList result
    result'' <- mapM (newObject GdkPixbuf.Pixbuf.Pixbuf) result'
    g_list_free result
    return result''

#if defined(ENABLE_OVERLOADING)
#endif

-- method Window::get_default_icon_name
-- method type : MemberFunction
-- Args: []
-- Lengths: []
-- returnType: Just (TBasicType TUTF8)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_window_get_default_icon_name" gtk_window_get_default_icon_name :: 
    IO CString

-- | Returns the fallback icon name for windows that has been set
-- with 'GI.Gtk.Objects.Window.windowSetDefaultIconName'. The returned
-- string is owned by GTK+ and should not be modified. It
-- is only valid until the next call to
-- 'GI.Gtk.Objects.Window.windowSetDefaultIconName'.
-- 
-- /Since: 2.16/
windowGetDefaultIconName ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m T.Text
    -- ^ __Returns:__ the fallback icon name for windows
windowGetDefaultIconName  = liftIO $ do
    result <- gtk_window_get_default_icon_name
    checkUnexpectedReturnNULL "windowGetDefaultIconName" result
    result' <- cstringToText result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Window::list_toplevels
-- method type : MemberFunction
-- Args: []
-- Lengths: []
-- returnType: Just
--               (TGList (TInterface Name { namespace = "Gtk" , name = "Widget" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_window_list_toplevels" gtk_window_list_toplevels :: 
    IO (Ptr (GList (Ptr Gtk.Widget.Widget)))

-- | Returns a list of all existing toplevel windows. The widgets
-- in the list are not individually referenced. If you want
-- to iterate through the list and perform actions involving
-- callbacks that might destroy the widgets, you must call
-- @g_list_foreach (result, (GFunc)g_object_ref, NULL)@ first, and
-- then unref all the widgets afterwards.
windowListToplevels ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m [Gtk.Widget.Widget]
    -- ^ __Returns:__ list of toplevel widgets
windowListToplevels  = liftIO $ do
    result <- gtk_window_list_toplevels
    result' <- unpackGList result
    result'' <- mapM (newObject Gtk.Widget.Widget) result'
    g_list_free result
    return result''

#if defined(ENABLE_OVERLOADING)
#endif

-- method Window::set_auto_startup_notification
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "setting"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "%TRUE to automatically do startup notification"
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

foreign import ccall "gtk_window_set_auto_startup_notification" gtk_window_set_auto_startup_notification :: 
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | By default, after showing the first t'GI.Gtk.Objects.Window.Window', GTK+ calls
-- 'GI.Gdk.Functions.notifyStartupComplete'.  Call this function to disable
-- the automatic startup notification. You might do this if your
-- first window is a splash screen, and you want to delay notification
-- until after your real main window has been shown, for example.
-- 
-- In that example, you would disable startup notification
-- temporarily, show your splash screen, then re-enable it so that
-- showing the main window would automatically result in notification.
-- 
-- /Since: 2.2/
windowSetAutoStartupNotification ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Bool
    -- ^ /@setting@/: 'P.True' to automatically do startup notification
    -> m ()
windowSetAutoStartupNotification setting = liftIO $ do
    let setting' = (fromIntegral . fromEnum) setting
    gtk_window_set_auto_startup_notification setting'
    return ()

#if defined(ENABLE_OVERLOADING)
#endif

-- method Window::set_default_icon
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "icon"
--           , argType =
--               TInterface Name { namespace = "GdkPixbuf" , name = "Pixbuf" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the icon" , sinceVersion = Nothing }
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

foreign import ccall "gtk_window_set_default_icon" gtk_window_set_default_icon :: 
    Ptr GdkPixbuf.Pixbuf.Pixbuf ->          -- icon : TInterface (Name {namespace = "GdkPixbuf", name = "Pixbuf"})
    IO ()

-- | Sets an icon to be used as fallback for windows that haven\'t
-- had 'GI.Gtk.Objects.Window.windowSetIcon' called on them from a pixbuf.
-- 
-- /Since: 2.4/
windowSetDefaultIcon ::
    (B.CallStack.HasCallStack, MonadIO m, GdkPixbuf.Pixbuf.IsPixbuf a) =>
    a
    -- ^ /@icon@/: the icon
    -> m ()
windowSetDefaultIcon icon = liftIO $ do
    icon' <- unsafeManagedPtrCastPtr icon
    gtk_window_set_default_icon icon'
    touchManagedPtr icon
    return ()

#if defined(ENABLE_OVERLOADING)
#endif

-- method Window::set_default_icon_from_file
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "filename"
--           , argType = TBasicType TFileName
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "location of icon file"
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
-- throws : True
-- Skip return : False

foreign import ccall "gtk_window_set_default_icon_from_file" gtk_window_set_default_icon_from_file :: 
    CString ->                              -- filename : TBasicType TFileName
    Ptr (Ptr GError) ->                     -- error
    IO CInt

-- | Sets an icon to be used as fallback for windows that haven\'t
-- had 'GI.Gtk.Objects.Window.windowSetIconList' called on them from a file
-- on disk. Warns on failure if /@err@/ is 'P.Nothing'.
-- 
-- /Since: 2.2/
windowSetDefaultIconFromFile ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    [Char]
    -- ^ /@filename@/: location of icon file
    -> m ()
    -- ^ /(Can throw 'Data.GI.Base.GError.GError')/
windowSetDefaultIconFromFile filename = liftIO $ do
    filename' <- stringToCString filename
    onException (do
        _ <- propagateGError $ gtk_window_set_default_icon_from_file filename'
        freeMem filename'
        return ()
     ) (do
        freeMem filename'
     )

#if defined(ENABLE_OVERLOADING)
#endif

-- method Window::set_default_icon_list
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "list"
--           , argType =
--               TGList
--                 (TInterface Name { namespace = "GdkPixbuf" , name = "Pixbuf" })
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a list of #GdkPixbuf"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferContainer
--           }
--       ]
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_window_set_default_icon_list" gtk_window_set_default_icon_list :: 
    Ptr (GList (Ptr GdkPixbuf.Pixbuf.Pixbuf)) -> -- list : TGList (TInterface (Name {namespace = "GdkPixbuf", name = "Pixbuf"}))
    IO ()

-- | Sets an icon list to be used as fallback for windows that haven\'t
-- had 'GI.Gtk.Objects.Window.windowSetIconList' called on them to set up a
-- window-specific icon list. This function allows you to set up the
-- icon for all windows in your app at once.
-- 
-- See 'GI.Gtk.Objects.Window.windowSetIconList' for more details.
windowSetDefaultIconList ::
    (B.CallStack.HasCallStack, MonadIO m, GdkPixbuf.Pixbuf.IsPixbuf a) =>
    [a]
    -- ^ /@list@/: a list of t'GI.GdkPixbuf.Objects.Pixbuf.Pixbuf'
    -> m ()
windowSetDefaultIconList list = liftIO $ do
    list' <- mapM unsafeManagedPtrCastPtr list
    list'' <- packGList list'
    gtk_window_set_default_icon_list list''
    mapM_ touchManagedPtr list
    return ()

#if defined(ENABLE_OVERLOADING)
#endif

-- method Window::set_default_icon_name
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the name of the themed icon"
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

foreign import ccall "gtk_window_set_default_icon_name" gtk_window_set_default_icon_name :: 
    CString ->                              -- name : TBasicType TUTF8
    IO ()

-- | Sets an icon to be used as fallback for windows that haven\'t
-- had 'GI.Gtk.Objects.Window.windowSetIconList' called on them from a named
-- themed icon, see 'GI.Gtk.Objects.Window.windowSetIconName'.
-- 
-- /Since: 2.6/
windowSetDefaultIconName ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@name@/: the name of the themed icon
    -> m ()
windowSetDefaultIconName name = liftIO $ do
    name' <- textToCString name
    gtk_window_set_default_icon_name name'
    freeMem name'
    return ()

#if defined(ENABLE_OVERLOADING)
#endif

-- method Window::set_interactive_debugging
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "enable"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE to enable interactive debugging"
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

foreign import ccall "gtk_window_set_interactive_debugging" gtk_window_set_interactive_debugging :: 
    CInt ->                                 -- enable : TBasicType TBoolean
    IO ()

-- | Opens or closes the [interactive debugger][interactive-debugging],
-- which offers access to the widget hierarchy of the application
-- and to useful debugging tools.
-- 
-- /Since: 3.14/
windowSetInteractiveDebugging ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Bool
    -- ^ /@enable@/: 'P.True' to enable interactive debugging
    -> m ()
windowSetInteractiveDebugging enable = liftIO $ do
    let enable' = (fromIntegral . fromEnum) enable
    gtk_window_set_interactive_debugging enable'
    return ()

#if defined(ENABLE_OVERLOADING)
#endif


