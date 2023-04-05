{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- GtkOverlay is a container which contains a single main child, on top
-- of which it can place “overlay” widgets. The position of each overlay
-- widget is determined by its [Widget:halign]("GI.Gtk.Objects.Widget#g:attr:halign") and [Widget:valign]("GI.Gtk.Objects.Widget#g:attr:valign")
-- properties. E.g. a widget with both alignments set to 'GI.Gtk.Enums.AlignStart'
-- will be placed at the top left corner of the GtkOverlay container,
-- whereas an overlay with halign set to 'GI.Gtk.Enums.AlignCenter' and valign set
-- to 'GI.Gtk.Enums.AlignEnd' will be placed a the bottom edge of the GtkOverlay,
-- horizontally centered. The position can be adjusted by setting the margin
-- properties of the child to non-zero values.
-- 
-- More complicated placement of overlays is possible by connecting
-- to the [Overlay::getChildPosition]("GI.Gtk.Objects.Overlay#g:signal:getChildPosition") signal.
-- 
-- An overlay’s minimum and natural sizes are those of its main child. The sizes
-- of overlay children are not considered when measuring these preferred sizes.
-- 
-- = GtkOverlay as GtkBuildable
-- 
-- The GtkOverlay implementation of the GtkBuildable interface
-- supports placing a child as an overlay by specifying “overlay” as
-- the “type” attribute of a @\<child>@ element.
-- 
-- = CSS nodes
-- 
-- GtkOverlay has a single CSS node with the name “overlay”. Overlay children
-- whose alignments cause them to be positioned at an edge get the style classes
-- “.left”, “.right”, “.top”, and\/or “.bottom” according to their position.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.Overlay
    ( 

-- * Exported types
    Overlay(..)                             ,
    IsOverlay                               ,
    toOverlay                               ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addOverlay]("GI.Gtk.Objects.Overlay#g:method:addOverlay"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reorderOverlay]("GI.Gtk.Objects.Overlay#g:method:reorderOverlay"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChild]("GI.Gtk.Objects.Bin#g:method:getChild"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getOverlayPassThrough]("GI.Gtk.Objects.Overlay#g:method:getOverlayPassThrough"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setOverlayPassThrough]("GI.Gtk.Objects.Overlay#g:method:setOverlayPassThrough"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveOverlayMethod                    ,
#endif

-- ** addOverlay #method:addOverlay#

#if defined(ENABLE_OVERLOADING)
    OverlayAddOverlayMethodInfo             ,
#endif
    overlayAddOverlay                       ,


-- ** getOverlayPassThrough #method:getOverlayPassThrough#

#if defined(ENABLE_OVERLOADING)
    OverlayGetOverlayPassThroughMethodInfo  ,
#endif
    overlayGetOverlayPassThrough            ,


-- ** new #method:new#

    overlayNew                              ,


-- ** reorderOverlay #method:reorderOverlay#

#if defined(ENABLE_OVERLOADING)
    OverlayReorderOverlayMethodInfo         ,
#endif
    overlayReorderOverlay                   ,


-- ** setOverlayPassThrough #method:setOverlayPassThrough#

#if defined(ENABLE_OVERLOADING)
    OverlaySetOverlayPassThroughMethodInfo  ,
#endif
    overlaySetOverlayPassThrough            ,




 -- * Signals


-- ** getChildPosition #signal:getChildPosition#

    OverlayGetChildPositionCallback         ,
#if defined(ENABLE_OVERLOADING)
    OverlayGetChildPositionSignalInfo       ,
#endif
    afterOverlayGetChildPosition            ,
    onOverlayGetChildPosition               ,




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
import qualified GI.Gdk.Structs.Rectangle as Gdk.Rectangle
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Objects.Bin as Gtk.Bin
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype Overlay = Overlay (SP.ManagedPtr Overlay)
    deriving (Eq)

instance SP.ManagedPtrNewtype Overlay where
    toManagedPtr (Overlay p) = p

foreign import ccall "gtk_overlay_get_type"
    c_gtk_overlay_get_type :: IO B.Types.GType

instance B.Types.TypedObject Overlay where
    glibType = c_gtk_overlay_get_type

instance B.Types.GObject Overlay

-- | Type class for types which can be safely cast to `Overlay`, for instance with `toOverlay`.
class (SP.GObject o, O.IsDescendantOf Overlay o) => IsOverlay o
instance (SP.GObject o, O.IsDescendantOf Overlay o) => IsOverlay o

instance O.HasParentTypes Overlay
type instance O.ParentTypes Overlay = '[Gtk.Bin.Bin, Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable]

-- | Cast to `Overlay`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toOverlay :: (MIO.MonadIO m, IsOverlay o) => o -> m Overlay
toOverlay = MIO.liftIO . B.ManagedPtr.unsafeCastTo Overlay

-- | Convert 'Overlay' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe Overlay) where
    gvalueGType_ = c_gtk_overlay_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr Overlay)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr Overlay)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject Overlay ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveOverlayMethod (t :: Symbol) (o :: *) :: * where
    ResolveOverlayMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveOverlayMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveOverlayMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveOverlayMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveOverlayMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveOverlayMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveOverlayMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveOverlayMethod "addOverlay" o = OverlayAddOverlayMethodInfo
    ResolveOverlayMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveOverlayMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveOverlayMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveOverlayMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveOverlayMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveOverlayMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveOverlayMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveOverlayMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveOverlayMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveOverlayMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveOverlayMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveOverlayMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveOverlayMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveOverlayMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveOverlayMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveOverlayMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveOverlayMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveOverlayMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveOverlayMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveOverlayMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveOverlayMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveOverlayMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveOverlayMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveOverlayMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveOverlayMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveOverlayMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveOverlayMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveOverlayMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveOverlayMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveOverlayMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveOverlayMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveOverlayMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveOverlayMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveOverlayMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveOverlayMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveOverlayMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveOverlayMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveOverlayMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveOverlayMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveOverlayMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveOverlayMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveOverlayMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveOverlayMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveOverlayMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveOverlayMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveOverlayMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveOverlayMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveOverlayMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveOverlayMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveOverlayMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveOverlayMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveOverlayMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveOverlayMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveOverlayMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveOverlayMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveOverlayMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveOverlayMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveOverlayMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveOverlayMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveOverlayMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveOverlayMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveOverlayMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveOverlayMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveOverlayMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveOverlayMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveOverlayMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveOverlayMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveOverlayMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveOverlayMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveOverlayMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveOverlayMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveOverlayMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveOverlayMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveOverlayMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveOverlayMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveOverlayMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveOverlayMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveOverlayMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveOverlayMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveOverlayMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveOverlayMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveOverlayMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveOverlayMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveOverlayMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveOverlayMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveOverlayMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveOverlayMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveOverlayMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveOverlayMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveOverlayMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveOverlayMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveOverlayMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveOverlayMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveOverlayMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveOverlayMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveOverlayMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveOverlayMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveOverlayMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveOverlayMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveOverlayMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveOverlayMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveOverlayMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveOverlayMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveOverlayMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveOverlayMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveOverlayMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveOverlayMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveOverlayMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveOverlayMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveOverlayMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveOverlayMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveOverlayMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveOverlayMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveOverlayMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveOverlayMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveOverlayMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveOverlayMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveOverlayMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveOverlayMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveOverlayMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveOverlayMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolveOverlayMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveOverlayMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveOverlayMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveOverlayMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveOverlayMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveOverlayMethod "reorderOverlay" o = OverlayReorderOverlayMethodInfo
    ResolveOverlayMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveOverlayMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveOverlayMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveOverlayMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveOverlayMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveOverlayMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveOverlayMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveOverlayMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveOverlayMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveOverlayMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveOverlayMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveOverlayMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveOverlayMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveOverlayMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveOverlayMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveOverlayMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveOverlayMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveOverlayMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveOverlayMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveOverlayMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveOverlayMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveOverlayMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveOverlayMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveOverlayMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveOverlayMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveOverlayMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveOverlayMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveOverlayMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveOverlayMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveOverlayMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveOverlayMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveOverlayMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveOverlayMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveOverlayMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveOverlayMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveOverlayMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveOverlayMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveOverlayMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveOverlayMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveOverlayMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveOverlayMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveOverlayMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveOverlayMethod "getChild" o = Gtk.Bin.BinGetChildMethodInfo
    ResolveOverlayMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveOverlayMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveOverlayMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveOverlayMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveOverlayMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveOverlayMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveOverlayMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveOverlayMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveOverlayMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveOverlayMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveOverlayMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveOverlayMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveOverlayMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveOverlayMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveOverlayMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveOverlayMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveOverlayMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveOverlayMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveOverlayMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveOverlayMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveOverlayMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveOverlayMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveOverlayMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveOverlayMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveOverlayMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveOverlayMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveOverlayMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveOverlayMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveOverlayMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveOverlayMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveOverlayMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveOverlayMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveOverlayMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveOverlayMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveOverlayMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveOverlayMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveOverlayMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveOverlayMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveOverlayMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveOverlayMethod "getOverlayPassThrough" o = OverlayGetOverlayPassThroughMethodInfo
    ResolveOverlayMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveOverlayMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveOverlayMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveOverlayMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveOverlayMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveOverlayMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveOverlayMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveOverlayMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveOverlayMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveOverlayMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveOverlayMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveOverlayMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveOverlayMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveOverlayMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveOverlayMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveOverlayMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveOverlayMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveOverlayMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveOverlayMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveOverlayMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveOverlayMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveOverlayMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveOverlayMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveOverlayMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveOverlayMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveOverlayMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveOverlayMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveOverlayMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveOverlayMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveOverlayMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveOverlayMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveOverlayMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveOverlayMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveOverlayMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveOverlayMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveOverlayMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveOverlayMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveOverlayMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveOverlayMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveOverlayMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveOverlayMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveOverlayMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveOverlayMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveOverlayMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveOverlayMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveOverlayMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveOverlayMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveOverlayMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveOverlayMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveOverlayMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveOverlayMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveOverlayMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveOverlayMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveOverlayMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveOverlayMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveOverlayMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveOverlayMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveOverlayMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveOverlayMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveOverlayMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveOverlayMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveOverlayMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveOverlayMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveOverlayMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveOverlayMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveOverlayMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveOverlayMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveOverlayMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveOverlayMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveOverlayMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveOverlayMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveOverlayMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveOverlayMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveOverlayMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveOverlayMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveOverlayMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveOverlayMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveOverlayMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveOverlayMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveOverlayMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveOverlayMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveOverlayMethod "setOverlayPassThrough" o = OverlaySetOverlayPassThroughMethodInfo
    ResolveOverlayMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveOverlayMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveOverlayMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveOverlayMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveOverlayMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveOverlayMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveOverlayMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveOverlayMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveOverlayMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveOverlayMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveOverlayMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveOverlayMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveOverlayMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveOverlayMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveOverlayMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveOverlayMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveOverlayMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveOverlayMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveOverlayMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveOverlayMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveOverlayMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveOverlayMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveOverlayMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveOverlayMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveOverlayMethod t Overlay, O.OverloadedMethod info Overlay p) => OL.IsLabel t (Overlay -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveOverlayMethod t Overlay, O.OverloadedMethod info Overlay p, R.HasField t Overlay p) => R.HasField t Overlay p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveOverlayMethod t Overlay, O.OverloadedMethodInfo info Overlay) => OL.IsLabel t (O.MethodProxy info Overlay) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal Overlay::get-child-position
-- | The [getChildPosition](#g:signal:getChildPosition) signal is emitted to determine
-- the position and size of any overlay child widgets. A
-- handler for this signal should fill /@allocation@/ with
-- the desired position and size for /@widget@/, relative to
-- the \'main\' child of /@overlay@/.
-- 
-- The default handler for this signal uses the /@widget@/\'s
-- halign and valign properties to determine the position
-- and gives the widget its natural size (except that an
-- alignment of 'GI.Gtk.Enums.AlignFill' will cause the overlay to
-- be full-width\/height). If the main child is a
-- t'GI.Gtk.Objects.ScrolledWindow.ScrolledWindow', the overlays are placed relative
-- to its contents.
type OverlayGetChildPositionCallback =
    Gtk.Widget.Widget
    -- ^ /@widget@/: the child widget to position
    -> Gdk.Rectangle.Rectangle
    -- ^ /@allocation@/: return
    --   location for the allocation
    -> IO Bool
    -- ^ __Returns:__ 'P.True' if the /@allocation@/ has been filled

type C_OverlayGetChildPositionCallback =
    Ptr Overlay ->                          -- object
    Ptr Gtk.Widget.Widget ->
    Ptr Gdk.Rectangle.Rectangle ->
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_OverlayGetChildPositionCallback`.
foreign import ccall "wrapper"
    mk_OverlayGetChildPositionCallback :: C_OverlayGetChildPositionCallback -> IO (FunPtr C_OverlayGetChildPositionCallback)

wrap_OverlayGetChildPositionCallback :: 
    GObject a => (a -> OverlayGetChildPositionCallback) ->
    C_OverlayGetChildPositionCallback
wrap_OverlayGetChildPositionCallback gi'cb gi'selfPtr widget allocation _ = do
    widget' <- (newObject Gtk.Widget.Widget) widget
    B.ManagedPtr.withTransient  allocation $ \allocation' -> do
        result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  widget' allocation'
        let result' = (fromIntegral . fromEnum) result
        return result'


-- | Connect a signal handler for the [getChildPosition](#signal:getChildPosition) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' overlay #getChildPosition callback
-- @
-- 
-- 
onOverlayGetChildPosition :: (IsOverlay a, MonadIO m) => a -> ((?self :: a) => OverlayGetChildPositionCallback) -> m SignalHandlerId
onOverlayGetChildPosition obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_OverlayGetChildPositionCallback wrapped
    wrapped'' <- mk_OverlayGetChildPositionCallback wrapped'
    connectSignalFunPtr obj "get-child-position" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [getChildPosition](#signal:getChildPosition) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' overlay #getChildPosition callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterOverlayGetChildPosition :: (IsOverlay a, MonadIO m) => a -> ((?self :: a) => OverlayGetChildPositionCallback) -> m SignalHandlerId
afterOverlayGetChildPosition obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_OverlayGetChildPositionCallback wrapped
    wrapped'' <- mk_OverlayGetChildPositionCallback wrapped'
    connectSignalFunPtr obj "get-child-position" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data OverlayGetChildPositionSignalInfo
instance SignalInfo OverlayGetChildPositionSignalInfo where
    type HaskellCallbackType OverlayGetChildPositionSignalInfo = OverlayGetChildPositionCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_OverlayGetChildPositionCallback cb
        cb'' <- mk_OverlayGetChildPositionCallback cb'
        connectSignalFunPtr obj "get-child-position" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Overlay::get-child-position"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Overlay.html#g:signal:getChildPosition"})

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList Overlay
type instance O.AttributeList Overlay = OverlayAttributeList
type OverlayAttributeList = ('[ '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList Overlay = OverlaySignalList
type OverlaySignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("getChildPosition", OverlayGetChildPositionSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method Overlay::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Overlay" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_overlay_new" gtk_overlay_new :: 
    IO (Ptr Overlay)

-- | Creates a new t'GI.Gtk.Objects.Overlay.Overlay'.
-- 
-- /Since: 3.2/
overlayNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m Overlay
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.Overlay.Overlay' object.
overlayNew  = liftIO $ do
    result <- gtk_overlay_new
    checkUnexpectedReturnNULL "overlayNew" result
    result' <- (newObject Overlay) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Overlay::add_overlay
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "overlay"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Overlay" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkOverlay" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a #GtkWidget to be added to the container"
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

foreign import ccall "gtk_overlay_add_overlay" gtk_overlay_add_overlay :: 
    Ptr Overlay ->                          -- overlay : TInterface (Name {namespace = "Gtk", name = "Overlay"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | Adds /@widget@/ to /@overlay@/.
-- 
-- The widget will be stacked on top of the main widget
-- added with 'GI.Gtk.Objects.Container.containerAdd'.
-- 
-- The position at which /@widget@/ is placed is determined
-- from its [Widget:halign]("GI.Gtk.Objects.Widget#g:attr:halign") and [Widget:valign]("GI.Gtk.Objects.Widget#g:attr:valign") properties.
-- 
-- /Since: 3.2/
overlayAddOverlay ::
    (B.CallStack.HasCallStack, MonadIO m, IsOverlay a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@overlay@/: a t'GI.Gtk.Objects.Overlay.Overlay'
    -> b
    -- ^ /@widget@/: a t'GI.Gtk.Objects.Widget.Widget' to be added to the container
    -> m ()
overlayAddOverlay overlay widget = liftIO $ do
    overlay' <- unsafeManagedPtrCastPtr overlay
    widget' <- unsafeManagedPtrCastPtr widget
    gtk_overlay_add_overlay overlay' widget'
    touchManagedPtr overlay
    touchManagedPtr widget
    return ()

#if defined(ENABLE_OVERLOADING)
data OverlayAddOverlayMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsOverlay a, Gtk.Widget.IsWidget b) => O.OverloadedMethod OverlayAddOverlayMethodInfo a signature where
    overloadedMethod = overlayAddOverlay

instance O.OverloadedMethodInfo OverlayAddOverlayMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Overlay.overlayAddOverlay",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Overlay.html#v:overlayAddOverlay"
        })


#endif

-- method Overlay::get_overlay_pass_through
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "overlay"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Overlay" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkOverlay" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "an overlay child of #GtkOverlay"
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

foreign import ccall "gtk_overlay_get_overlay_pass_through" gtk_overlay_get_overlay_pass_through :: 
    Ptr Overlay ->                          -- overlay : TInterface (Name {namespace = "Gtk", name = "Overlay"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO CInt

-- | Convenience function to get the value of the t'GI.Gtk.Objects.Overlay.Overlay':@/pass-through/@
-- child property for /@widget@/.
-- 
-- /Since: 3.18/
overlayGetOverlayPassThrough ::
    (B.CallStack.HasCallStack, MonadIO m, IsOverlay a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@overlay@/: a t'GI.Gtk.Objects.Overlay.Overlay'
    -> b
    -- ^ /@widget@/: an overlay child of t'GI.Gtk.Objects.Overlay.Overlay'
    -> m Bool
    -- ^ __Returns:__ whether the widget is a pass through child.
overlayGetOverlayPassThrough overlay widget = liftIO $ do
    overlay' <- unsafeManagedPtrCastPtr overlay
    widget' <- unsafeManagedPtrCastPtr widget
    result <- gtk_overlay_get_overlay_pass_through overlay' widget'
    let result' = (/= 0) result
    touchManagedPtr overlay
    touchManagedPtr widget
    return result'

#if defined(ENABLE_OVERLOADING)
data OverlayGetOverlayPassThroughMethodInfo
instance (signature ~ (b -> m Bool), MonadIO m, IsOverlay a, Gtk.Widget.IsWidget b) => O.OverloadedMethod OverlayGetOverlayPassThroughMethodInfo a signature where
    overloadedMethod = overlayGetOverlayPassThrough

instance O.OverloadedMethodInfo OverlayGetOverlayPassThroughMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Overlay.overlayGetOverlayPassThrough",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Overlay.html#v:overlayGetOverlayPassThrough"
        })


#endif

-- method Overlay::reorder_overlay
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "overlay"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Overlay" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkOverlay" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the overlaid #GtkWidget to move"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "index_"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the new index for @child in the list of overlay children\n  of @overlay, starting from 0. If negative, indicates the end of\n  the list"
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

foreign import ccall "gtk_overlay_reorder_overlay" gtk_overlay_reorder_overlay :: 
    Ptr Overlay ->                          -- overlay : TInterface (Name {namespace = "Gtk", name = "Overlay"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Int32 ->                                -- index_ : TBasicType TInt
    IO ()

-- | Moves /@child@/ to a new /@index@/ in the list of /@overlay@/ children.
-- The list contains overlays in the order that these were
-- added to /@overlay@/ by default. See also t'GI.Gtk.Objects.Overlay.Overlay':@/index/@.
-- 
-- A widget’s index in the /@overlay@/ children list determines which order
-- the children are drawn if they overlap. The first child is drawn at
-- the bottom. It also affects the default focus chain order.
-- 
-- /Since: 3.18/
overlayReorderOverlay ::
    (B.CallStack.HasCallStack, MonadIO m, IsOverlay a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@overlay@/: a t'GI.Gtk.Objects.Overlay.Overlay'
    -> b
    -- ^ /@child@/: the overlaid t'GI.Gtk.Objects.Widget.Widget' to move
    -> Int32
    -- ^ /@index_@/: the new index for /@child@/ in the list of overlay children
    --   of /@overlay@/, starting from 0. If negative, indicates the end of
    --   the list
    -> m ()
overlayReorderOverlay overlay child index_ = liftIO $ do
    overlay' <- unsafeManagedPtrCastPtr overlay
    child' <- unsafeManagedPtrCastPtr child
    gtk_overlay_reorder_overlay overlay' child' index_
    touchManagedPtr overlay
    touchManagedPtr child
    return ()

#if defined(ENABLE_OVERLOADING)
data OverlayReorderOverlayMethodInfo
instance (signature ~ (b -> Int32 -> m ()), MonadIO m, IsOverlay a, Gtk.Widget.IsWidget b) => O.OverloadedMethod OverlayReorderOverlayMethodInfo a signature where
    overloadedMethod = overlayReorderOverlay

instance O.OverloadedMethodInfo OverlayReorderOverlayMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Overlay.overlayReorderOverlay",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Overlay.html#v:overlayReorderOverlay"
        })


#endif

-- method Overlay::set_overlay_pass_through
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "overlay"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Overlay" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkOverlay" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "an overlay child of #GtkOverlay"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pass_through"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "whether the child should pass the input through"
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

foreign import ccall "gtk_overlay_set_overlay_pass_through" gtk_overlay_set_overlay_pass_through :: 
    Ptr Overlay ->                          -- overlay : TInterface (Name {namespace = "Gtk", name = "Overlay"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CInt ->                                 -- pass_through : TBasicType TBoolean
    IO ()

-- | Convenience function to set the value of the t'GI.Gtk.Objects.Overlay.Overlay':@/pass-through/@
-- child property for /@widget@/.
-- 
-- /Since: 3.18/
overlaySetOverlayPassThrough ::
    (B.CallStack.HasCallStack, MonadIO m, IsOverlay a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@overlay@/: a t'GI.Gtk.Objects.Overlay.Overlay'
    -> b
    -- ^ /@widget@/: an overlay child of t'GI.Gtk.Objects.Overlay.Overlay'
    -> Bool
    -- ^ /@passThrough@/: whether the child should pass the input through
    -> m ()
overlaySetOverlayPassThrough overlay widget passThrough = liftIO $ do
    overlay' <- unsafeManagedPtrCastPtr overlay
    widget' <- unsafeManagedPtrCastPtr widget
    let passThrough' = (fromIntegral . fromEnum) passThrough
    gtk_overlay_set_overlay_pass_through overlay' widget' passThrough'
    touchManagedPtr overlay
    touchManagedPtr widget
    return ()

#if defined(ENABLE_OVERLOADING)
data OverlaySetOverlayPassThroughMethodInfo
instance (signature ~ (b -> Bool -> m ()), MonadIO m, IsOverlay a, Gtk.Widget.IsWidget b) => O.OverloadedMethod OverlaySetOverlayPassThroughMethodInfo a signature where
    overloadedMethod = overlaySetOverlayPassThrough

instance O.OverloadedMethodInfo OverlaySetOverlayPassThroughMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Overlay.overlaySetOverlayPassThrough",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Overlay.html#v:overlaySetOverlayPassThrough"
        })


#endif


