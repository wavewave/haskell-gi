{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- GtkArrow should be used to draw simple arrows that need to point in
-- one of the four cardinal directions (up, down, left, or right).  The
-- style of the arrow can be one of shadow in, shadow out, etched in, or
-- etched out.  Note that these directions and style types may be
-- amended in versions of GTK+ to come.
-- 
-- GtkArrow will fill any space alloted to it, but since it is inherited
-- from t'GI.Gtk.Objects.Misc.Misc', it can be padded and\/or aligned, to fill exactly the
-- space the programmer desires.
-- 
-- Arrows are created with a call to 'GI.Gtk.Objects.Arrow.arrowNew'.  The direction or
-- style of an arrow can be changed after creation by using 'GI.Gtk.Objects.Arrow.arrowSet'.
-- 
-- GtkArrow has been deprecated; you can simply use a t'GI.Gtk.Objects.Image.Image' with a
-- suitable icon name, such as “pan-down-symbolic“. When replacing
-- GtkArrow by an image, pay attention to the fact that GtkArrow is
-- doing automatic flipping between @/GTK_ARROW_LEFT/@ and @/GTK_ARROW_RIGHT/@,
-- depending on the text direction. To get the same effect with an image,
-- use the icon names “pan-start-symbolic“ and “pan-end-symbolic“, which
-- react to the text direction.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.Arrow
    ( 

-- * Exported types
    Arrow(..)                               ,
    IsArrow                                 ,
    toArrow                                 ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childNotify]("GI.Gtk.Objects.Widget#g:method:childNotify"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [set]("GI.Gtk.Objects.Arrow#g:method:set"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getAlignment]("GI.Gtk.Objects.Misc#g:method:getAlignment"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getPadding]("GI.Gtk.Objects.Misc#g:method:getPadding"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setAlignment]("GI.Gtk.Objects.Misc#g:method:setAlignment"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setPadding]("GI.Gtk.Objects.Misc#g:method:setPadding"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveArrowMethod                      ,
#endif

-- ** new #method:new#

    arrowNew                                ,


-- ** set #method:set#

#if defined(ENABLE_OVERLOADING)
    ArrowSetMethodInfo                      ,
#endif
    arrowSet                                ,




 -- * Properties


-- ** arrowType #attr:arrowType#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ArrowArrowTypePropertyInfo              ,
#endif
#if defined(ENABLE_OVERLOADING)
    arrowArrowType                          ,
#endif
    constructArrowArrowType                 ,
    getArrowArrowType                       ,
    setArrowArrowType                       ,


-- ** shadowType #attr:shadowType#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ArrowShadowTypePropertyInfo             ,
#endif
#if defined(ENABLE_OVERLOADING)
    arrowShadowType                         ,
#endif
    constructArrowShadowType                ,
    getArrowShadowType                      ,
    setArrowShadowType                      ,




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
import {-# SOURCE #-} qualified GI.Gtk.Objects.Misc as Gtk.Misc
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype Arrow = Arrow (SP.ManagedPtr Arrow)
    deriving (Eq)

instance SP.ManagedPtrNewtype Arrow where
    toManagedPtr (Arrow p) = p

foreign import ccall "gtk_arrow_get_type"
    c_gtk_arrow_get_type :: IO B.Types.GType

instance B.Types.TypedObject Arrow where
    glibType = c_gtk_arrow_get_type

instance B.Types.GObject Arrow

-- | Type class for types which can be safely cast to `Arrow`, for instance with `toArrow`.
class (SP.GObject o, O.IsDescendantOf Arrow o) => IsArrow o
instance (SP.GObject o, O.IsDescendantOf Arrow o) => IsArrow o

instance O.HasParentTypes Arrow
type instance O.ParentTypes Arrow = '[Gtk.Misc.Misc, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable]

-- | Cast to `Arrow`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toArrow :: (MIO.MonadIO m, IsArrow o) => o -> m Arrow
toArrow = MIO.liftIO . B.ManagedPtr.unsafeCastTo Arrow

-- | Convert 'Arrow' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe Arrow) where
    gvalueGType_ = c_gtk_arrow_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr Arrow)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr Arrow)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject Arrow ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveArrowMethod (t :: Symbol) (o :: *) :: * where
    ResolveArrowMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveArrowMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveArrowMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveArrowMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveArrowMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveArrowMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveArrowMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveArrowMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveArrowMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveArrowMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveArrowMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveArrowMethod "childNotify" o = Gtk.Widget.WidgetChildNotifyMethodInfo
    ResolveArrowMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveArrowMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveArrowMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveArrowMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveArrowMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveArrowMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveArrowMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveArrowMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveArrowMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveArrowMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveArrowMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveArrowMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveArrowMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveArrowMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveArrowMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveArrowMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveArrowMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveArrowMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveArrowMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveArrowMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveArrowMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveArrowMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveArrowMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveArrowMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveArrowMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveArrowMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveArrowMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveArrowMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveArrowMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveArrowMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveArrowMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveArrowMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveArrowMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveArrowMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveArrowMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveArrowMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveArrowMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveArrowMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveArrowMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveArrowMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveArrowMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveArrowMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveArrowMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveArrowMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveArrowMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveArrowMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveArrowMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveArrowMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveArrowMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveArrowMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveArrowMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveArrowMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveArrowMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveArrowMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveArrowMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveArrowMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveArrowMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveArrowMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveArrowMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveArrowMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveArrowMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveArrowMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveArrowMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveArrowMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveArrowMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveArrowMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveArrowMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveArrowMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveArrowMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveArrowMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveArrowMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveArrowMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveArrowMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveArrowMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveArrowMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveArrowMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveArrowMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveArrowMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveArrowMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveArrowMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveArrowMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveArrowMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveArrowMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveArrowMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveArrowMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveArrowMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveArrowMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveArrowMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveArrowMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveArrowMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveArrowMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveArrowMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveArrowMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveArrowMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveArrowMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveArrowMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveArrowMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveArrowMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveArrowMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveArrowMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveArrowMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveArrowMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveArrowMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveArrowMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveArrowMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveArrowMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveArrowMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveArrowMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveArrowMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveArrowMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveArrowMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveArrowMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveArrowMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveArrowMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveArrowMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveArrowMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveArrowMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveArrowMethod "set" o = ArrowSetMethodInfo
    ResolveArrowMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveArrowMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveArrowMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveArrowMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveArrowMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveArrowMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveArrowMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveArrowMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveArrowMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveArrowMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveArrowMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveArrowMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveArrowMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveArrowMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveArrowMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveArrowMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveArrowMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveArrowMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveArrowMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveArrowMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveArrowMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveArrowMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveArrowMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveArrowMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveArrowMethod "getAlignment" o = Gtk.Misc.MiscGetAlignmentMethodInfo
    ResolveArrowMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveArrowMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveArrowMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveArrowMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveArrowMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveArrowMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveArrowMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveArrowMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveArrowMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveArrowMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveArrowMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveArrowMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveArrowMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveArrowMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveArrowMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveArrowMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveArrowMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveArrowMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveArrowMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveArrowMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveArrowMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveArrowMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveArrowMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveArrowMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveArrowMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveArrowMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveArrowMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveArrowMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveArrowMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveArrowMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveArrowMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveArrowMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveArrowMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveArrowMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveArrowMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveArrowMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveArrowMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveArrowMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveArrowMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveArrowMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveArrowMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveArrowMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveArrowMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveArrowMethod "getPadding" o = Gtk.Misc.MiscGetPaddingMethodInfo
    ResolveArrowMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveArrowMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveArrowMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveArrowMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveArrowMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveArrowMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveArrowMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveArrowMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveArrowMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveArrowMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveArrowMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveArrowMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveArrowMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveArrowMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveArrowMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveArrowMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveArrowMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveArrowMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveArrowMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveArrowMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveArrowMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveArrowMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveArrowMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveArrowMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveArrowMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveArrowMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveArrowMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveArrowMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveArrowMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveArrowMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveArrowMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveArrowMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveArrowMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveArrowMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveArrowMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveArrowMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveArrowMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveArrowMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveArrowMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveArrowMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveArrowMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveArrowMethod "setAlignment" o = Gtk.Misc.MiscSetAlignmentMethodInfo
    ResolveArrowMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveArrowMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveArrowMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveArrowMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveArrowMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveArrowMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveArrowMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveArrowMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveArrowMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveArrowMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveArrowMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveArrowMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveArrowMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveArrowMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveArrowMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveArrowMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveArrowMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveArrowMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveArrowMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveArrowMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveArrowMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveArrowMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveArrowMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveArrowMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveArrowMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveArrowMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveArrowMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveArrowMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveArrowMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveArrowMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveArrowMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveArrowMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveArrowMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveArrowMethod "setPadding" o = Gtk.Misc.MiscSetPaddingMethodInfo
    ResolveArrowMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveArrowMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveArrowMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveArrowMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveArrowMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveArrowMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveArrowMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveArrowMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveArrowMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveArrowMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveArrowMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveArrowMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveArrowMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveArrowMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveArrowMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveArrowMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveArrowMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveArrowMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveArrowMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveArrowMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveArrowMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveArrowMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveArrowMethod t Arrow, O.OverloadedMethod info Arrow p) => OL.IsLabel t (Arrow -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveArrowMethod t Arrow, O.OverloadedMethod info Arrow p, R.HasField t Arrow p) => R.HasField t Arrow p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveArrowMethod t Arrow, O.OverloadedMethodInfo info Arrow) => OL.IsLabel t (O.MethodProxy info Arrow) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- VVV Prop "arrow-type"
   -- Type: TInterface (Name {namespace = "Gtk", name = "ArrowType"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@arrow-type@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' arrow #arrowType
-- @
getArrowArrowType :: (MonadIO m, IsArrow o) => o -> m Gtk.Enums.ArrowType
getArrowArrowType obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "arrow-type"

-- | Set the value of the “@arrow-type@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' arrow [ #arrowType 'Data.GI.Base.Attributes.:=' value ]
-- @
setArrowArrowType :: (MonadIO m, IsArrow o) => o -> Gtk.Enums.ArrowType -> m ()
setArrowArrowType obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "arrow-type" val

-- | Construct a `GValueConstruct` with valid value for the “@arrow-type@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructArrowArrowType :: (IsArrow o, MIO.MonadIO m) => Gtk.Enums.ArrowType -> m (GValueConstruct o)
constructArrowArrowType val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "arrow-type" val

#if defined(ENABLE_OVERLOADING)
data ArrowArrowTypePropertyInfo
instance AttrInfo ArrowArrowTypePropertyInfo where
    type AttrAllowedOps ArrowArrowTypePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ArrowArrowTypePropertyInfo = IsArrow
    type AttrSetTypeConstraint ArrowArrowTypePropertyInfo = (~) Gtk.Enums.ArrowType
    type AttrTransferTypeConstraint ArrowArrowTypePropertyInfo = (~) Gtk.Enums.ArrowType
    type AttrTransferType ArrowArrowTypePropertyInfo = Gtk.Enums.ArrowType
    type AttrGetType ArrowArrowTypePropertyInfo = Gtk.Enums.ArrowType
    type AttrLabel ArrowArrowTypePropertyInfo = "arrow-type"
    type AttrOrigin ArrowArrowTypePropertyInfo = Arrow
    attrGet = getArrowArrowType
    attrSet = setArrowArrowType
    attrTransfer _ v = do
        return v
    attrConstruct = constructArrowArrowType
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Arrow.arrowType"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Arrow.html#g:attr:arrowType"
        })
#endif

-- VVV Prop "shadow-type"
   -- Type: TInterface (Name {namespace = "Gtk", name = "ShadowType"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@shadow-type@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' arrow #shadowType
-- @
getArrowShadowType :: (MonadIO m, IsArrow o) => o -> m Gtk.Enums.ShadowType
getArrowShadowType obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "shadow-type"

-- | Set the value of the “@shadow-type@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' arrow [ #shadowType 'Data.GI.Base.Attributes.:=' value ]
-- @
setArrowShadowType :: (MonadIO m, IsArrow o) => o -> Gtk.Enums.ShadowType -> m ()
setArrowShadowType obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "shadow-type" val

-- | Construct a `GValueConstruct` with valid value for the “@shadow-type@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructArrowShadowType :: (IsArrow o, MIO.MonadIO m) => Gtk.Enums.ShadowType -> m (GValueConstruct o)
constructArrowShadowType val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "shadow-type" val

#if defined(ENABLE_OVERLOADING)
data ArrowShadowTypePropertyInfo
instance AttrInfo ArrowShadowTypePropertyInfo where
    type AttrAllowedOps ArrowShadowTypePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ArrowShadowTypePropertyInfo = IsArrow
    type AttrSetTypeConstraint ArrowShadowTypePropertyInfo = (~) Gtk.Enums.ShadowType
    type AttrTransferTypeConstraint ArrowShadowTypePropertyInfo = (~) Gtk.Enums.ShadowType
    type AttrTransferType ArrowShadowTypePropertyInfo = Gtk.Enums.ShadowType
    type AttrGetType ArrowShadowTypePropertyInfo = Gtk.Enums.ShadowType
    type AttrLabel ArrowShadowTypePropertyInfo = "shadow-type"
    type AttrOrigin ArrowShadowTypePropertyInfo = Arrow
    attrGet = getArrowShadowType
    attrSet = setArrowShadowType
    attrTransfer _ v = do
        return v
    attrConstruct = constructArrowShadowType
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Arrow.shadowType"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Arrow.html#g:attr:shadowType"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList Arrow
type instance O.AttributeList Arrow = ArrowAttributeList
type ArrowAttributeList = ('[ '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("arrowType", ArrowArrowTypePropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("shadowType", ArrowShadowTypePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo), '("xalign", Gtk.Misc.MiscXalignPropertyInfo), '("xpad", Gtk.Misc.MiscXpadPropertyInfo), '("yalign", Gtk.Misc.MiscYalignPropertyInfo), '("ypad", Gtk.Misc.MiscYpadPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
arrowArrowType :: AttrLabelProxy "arrowType"
arrowArrowType = AttrLabelProxy

arrowShadowType :: AttrLabelProxy "shadowType"
arrowShadowType = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList Arrow = ArrowSignalList
type ArrowSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method Arrow::new
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "arrow_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ArrowType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a valid #GtkArrowType."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "shadow_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ShadowType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a valid #GtkShadowType."
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Arrow" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_arrow_new" gtk_arrow_new :: 
    CUInt ->                                -- arrow_type : TInterface (Name {namespace = "Gtk", name = "ArrowType"})
    CUInt ->                                -- shadow_type : TInterface (Name {namespace = "Gtk", name = "ShadowType"})
    IO (Ptr Arrow)

{-# DEPRECATED arrowNew ["(Since version 3.14)","Use a t'GI.Gtk.Objects.Image.Image' with a suitable icon."] #-}
-- | Creates a new t'GI.Gtk.Objects.Arrow.Arrow' widget.
arrowNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Gtk.Enums.ArrowType
    -- ^ /@arrowType@/: a valid t'GI.Gtk.Enums.ArrowType'.
    -> Gtk.Enums.ShadowType
    -- ^ /@shadowType@/: a valid t'GI.Gtk.Enums.ShadowType'.
    -> m Arrow
    -- ^ __Returns:__ the new t'GI.Gtk.Objects.Arrow.Arrow' widget.
arrowNew arrowType shadowType = liftIO $ do
    let arrowType' = (fromIntegral . fromEnum) arrowType
    let shadowType' = (fromIntegral . fromEnum) shadowType
    result <- gtk_arrow_new arrowType' shadowType'
    checkUnexpectedReturnNULL "arrowNew" result
    result' <- (newObject Arrow) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Arrow::set
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "arrow"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Arrow" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a widget of type #GtkArrow."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "arrow_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ArrowType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a valid #GtkArrowType."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "shadow_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ShadowType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a valid #GtkShadowType."
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

foreign import ccall "gtk_arrow_set" gtk_arrow_set :: 
    Ptr Arrow ->                            -- arrow : TInterface (Name {namespace = "Gtk", name = "Arrow"})
    CUInt ->                                -- arrow_type : TInterface (Name {namespace = "Gtk", name = "ArrowType"})
    CUInt ->                                -- shadow_type : TInterface (Name {namespace = "Gtk", name = "ShadowType"})
    IO ()

{-# DEPRECATED arrowSet ["(Since version 3.14)","Use a t'GI.Gtk.Objects.Image.Image' with a suitable icon."] #-}
-- | Sets the direction and style of the t'GI.Gtk.Objects.Arrow.Arrow', /@arrow@/.
arrowSet ::
    (B.CallStack.HasCallStack, MonadIO m, IsArrow a) =>
    a
    -- ^ /@arrow@/: a widget of type t'GI.Gtk.Objects.Arrow.Arrow'.
    -> Gtk.Enums.ArrowType
    -- ^ /@arrowType@/: a valid t'GI.Gtk.Enums.ArrowType'.
    -> Gtk.Enums.ShadowType
    -- ^ /@shadowType@/: a valid t'GI.Gtk.Enums.ShadowType'.
    -> m ()
arrowSet arrow arrowType shadowType = liftIO $ do
    arrow' <- unsafeManagedPtrCastPtr arrow
    let arrowType' = (fromIntegral . fromEnum) arrowType
    let shadowType' = (fromIntegral . fromEnum) shadowType
    gtk_arrow_set arrow' arrowType' shadowType'
    touchManagedPtr arrow
    return ()

#if defined(ENABLE_OVERLOADING)
data ArrowSetMethodInfo
instance (signature ~ (Gtk.Enums.ArrowType -> Gtk.Enums.ShadowType -> m ()), MonadIO m, IsArrow a) => O.OverloadedMethod ArrowSetMethodInfo a signature where
    overloadedMethod = arrowSet

instance O.OverloadedMethodInfo ArrowSetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Arrow.arrowSet",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Arrow.html#v:arrowSet"
        })


#endif


