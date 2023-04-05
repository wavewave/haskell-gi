{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- The frame widget is a bin that surrounds its child with a decorative
-- frame and an optional label. If present, the label is drawn in a gap
-- in the top side of the frame. The position of the label can be
-- controlled with 'GI.Gtk.Objects.Frame.frameSetLabelAlign'.
-- 
-- = GtkFrame as GtkBuildable
-- 
-- The GtkFrame implementation of the t'GI.Gtk.Interfaces.Buildable.Buildable' interface supports
-- placing a child in the label position by specifying “label” as the
-- “type” attribute of a @\<child>@ element. A normal content child can
-- be specified without specifying a @\<child>@ type attribute.
-- 
-- An example of a UI definition fragment with @GtkFrame@:
-- 
-- 
-- === /xml code/
-- >
-- ><object class="GtkFrame">
-- >  <child type="label">
-- >    <object class="GtkLabel" id="frame-label"/>
-- >  </child>
-- >  <child>
-- >    <object class="GtkEntry" id="frame-content"/>
-- >  </child>
-- ></object>
-- 
-- 
-- = CSS nodes
-- 
-- 
-- === /plain code/
-- >
-- >frame
-- >├── border[.flat]
-- >├── <label widget>
-- >╰── <child>
-- 
-- 
-- GtkFrame has a main CSS node named “frame” and a subnode named “border”. The
-- “border” node is used to draw the visible border. You can set the appearance
-- of the border using CSS properties like “border-style” on the “border” node.
-- 
-- The border node can be given the style class “.flat”, which is used by themes
-- to disable drawing of the border. To do this from code, call
-- 'GI.Gtk.Objects.Frame.frameSetShadowType' with 'GI.Gtk.Enums.ShadowTypeNone' to add the “.flat” class or
-- any other shadow type to remove it.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.Frame
    ( 

-- * Exported types
    Frame(..)                               ,
    IsFrame                                 ,
    toFrame                                 ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChild]("GI.Gtk.Objects.Bin#g:method:getChild"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getLabel]("GI.Gtk.Objects.Frame#g:method:getLabel"), [getLabelAlign]("GI.Gtk.Objects.Frame#g:method:getLabelAlign"), [getLabelWidget]("GI.Gtk.Objects.Frame#g:method:getLabelWidget"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getShadowType]("GI.Gtk.Objects.Frame#g:method:getShadowType"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setLabel]("GI.Gtk.Objects.Frame#g:method:setLabel"), [setLabelAlign]("GI.Gtk.Objects.Frame#g:method:setLabelAlign"), [setLabelWidget]("GI.Gtk.Objects.Frame#g:method:setLabelWidget"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setShadowType]("GI.Gtk.Objects.Frame#g:method:setShadowType"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveFrameMethod                      ,
#endif

-- ** getLabel #method:getLabel#

#if defined(ENABLE_OVERLOADING)
    FrameGetLabelMethodInfo                 ,
#endif
    frameGetLabel                           ,


-- ** getLabelAlign #method:getLabelAlign#

#if defined(ENABLE_OVERLOADING)
    FrameGetLabelAlignMethodInfo            ,
#endif
    frameGetLabelAlign                      ,


-- ** getLabelWidget #method:getLabelWidget#

#if defined(ENABLE_OVERLOADING)
    FrameGetLabelWidgetMethodInfo           ,
#endif
    frameGetLabelWidget                     ,


-- ** getShadowType #method:getShadowType#

#if defined(ENABLE_OVERLOADING)
    FrameGetShadowTypeMethodInfo            ,
#endif
    frameGetShadowType                      ,


-- ** new #method:new#

    frameNew                                ,


-- ** setLabel #method:setLabel#

#if defined(ENABLE_OVERLOADING)
    FrameSetLabelMethodInfo                 ,
#endif
    frameSetLabel                           ,


-- ** setLabelAlign #method:setLabelAlign#

#if defined(ENABLE_OVERLOADING)
    FrameSetLabelAlignMethodInfo            ,
#endif
    frameSetLabelAlign                      ,


-- ** setLabelWidget #method:setLabelWidget#

#if defined(ENABLE_OVERLOADING)
    FrameSetLabelWidgetMethodInfo           ,
#endif
    frameSetLabelWidget                     ,


-- ** setShadowType #method:setShadowType#

#if defined(ENABLE_OVERLOADING)
    FrameSetShadowTypeMethodInfo            ,
#endif
    frameSetShadowType                      ,




 -- * Properties


-- ** label #attr:label#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    FrameLabelPropertyInfo                  ,
#endif
    clearFrameLabel                         ,
    constructFrameLabel                     ,
#if defined(ENABLE_OVERLOADING)
    frameLabel                              ,
#endif
    getFrameLabel                           ,
    setFrameLabel                           ,


-- ** labelWidget #attr:labelWidget#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    FrameLabelWidgetPropertyInfo            ,
#endif
    clearFrameLabelWidget                   ,
    constructFrameLabelWidget               ,
#if defined(ENABLE_OVERLOADING)
    frameLabelWidget                        ,
#endif
    getFrameLabelWidget                     ,
    setFrameLabelWidget                     ,


-- ** labelXalign #attr:labelXalign#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    FrameLabelXalignPropertyInfo            ,
#endif
    constructFrameLabelXalign               ,
#if defined(ENABLE_OVERLOADING)
    frameLabelXalign                        ,
#endif
    getFrameLabelXalign                     ,
    setFrameLabelXalign                     ,


-- ** labelYalign #attr:labelYalign#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    FrameLabelYalignPropertyInfo            ,
#endif
    constructFrameLabelYalign               ,
#if defined(ENABLE_OVERLOADING)
    frameLabelYalign                        ,
#endif
    getFrameLabelYalign                     ,
    setFrameLabelYalign                     ,


-- ** shadowType #attr:shadowType#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    FrameShadowTypePropertyInfo             ,
#endif
    constructFrameShadowType                ,
#if defined(ENABLE_OVERLOADING)
    frameShadowType                         ,
#endif
    getFrameShadowType                      ,
    setFrameShadowType                      ,




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
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype Frame = Frame (SP.ManagedPtr Frame)
    deriving (Eq)

instance SP.ManagedPtrNewtype Frame where
    toManagedPtr (Frame p) = p

foreign import ccall "gtk_frame_get_type"
    c_gtk_frame_get_type :: IO B.Types.GType

instance B.Types.TypedObject Frame where
    glibType = c_gtk_frame_get_type

instance B.Types.GObject Frame

-- | Type class for types which can be safely cast to `Frame`, for instance with `toFrame`.
class (SP.GObject o, O.IsDescendantOf Frame o) => IsFrame o
instance (SP.GObject o, O.IsDescendantOf Frame o) => IsFrame o

instance O.HasParentTypes Frame
type instance O.ParentTypes Frame = '[Gtk.Bin.Bin, Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable]

-- | Cast to `Frame`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toFrame :: (MIO.MonadIO m, IsFrame o) => o -> m Frame
toFrame = MIO.liftIO . B.ManagedPtr.unsafeCastTo Frame

-- | Convert 'Frame' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe Frame) where
    gvalueGType_ = c_gtk_frame_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr Frame)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr Frame)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject Frame ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveFrameMethod (t :: Symbol) (o :: *) :: * where
    ResolveFrameMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveFrameMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveFrameMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveFrameMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveFrameMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveFrameMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveFrameMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveFrameMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveFrameMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveFrameMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveFrameMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveFrameMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveFrameMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveFrameMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveFrameMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveFrameMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveFrameMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveFrameMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveFrameMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveFrameMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveFrameMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveFrameMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveFrameMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveFrameMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveFrameMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveFrameMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveFrameMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveFrameMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveFrameMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveFrameMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveFrameMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveFrameMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveFrameMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveFrameMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveFrameMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveFrameMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveFrameMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveFrameMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveFrameMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveFrameMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveFrameMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveFrameMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveFrameMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveFrameMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveFrameMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveFrameMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveFrameMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveFrameMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveFrameMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveFrameMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveFrameMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveFrameMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveFrameMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveFrameMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveFrameMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveFrameMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveFrameMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveFrameMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveFrameMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveFrameMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveFrameMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveFrameMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveFrameMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveFrameMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveFrameMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveFrameMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveFrameMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveFrameMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveFrameMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveFrameMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveFrameMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveFrameMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveFrameMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveFrameMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveFrameMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveFrameMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveFrameMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveFrameMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveFrameMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveFrameMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveFrameMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveFrameMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveFrameMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveFrameMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveFrameMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveFrameMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveFrameMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveFrameMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveFrameMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveFrameMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveFrameMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveFrameMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveFrameMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveFrameMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveFrameMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveFrameMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveFrameMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveFrameMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveFrameMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveFrameMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveFrameMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveFrameMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveFrameMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveFrameMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveFrameMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveFrameMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveFrameMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveFrameMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveFrameMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveFrameMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveFrameMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveFrameMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveFrameMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveFrameMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveFrameMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveFrameMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveFrameMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveFrameMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveFrameMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveFrameMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveFrameMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveFrameMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveFrameMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveFrameMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveFrameMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveFrameMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveFrameMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveFrameMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolveFrameMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveFrameMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveFrameMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveFrameMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveFrameMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveFrameMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveFrameMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveFrameMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveFrameMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveFrameMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveFrameMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveFrameMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveFrameMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveFrameMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveFrameMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveFrameMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveFrameMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveFrameMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveFrameMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveFrameMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveFrameMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveFrameMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveFrameMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveFrameMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveFrameMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveFrameMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveFrameMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveFrameMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveFrameMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveFrameMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveFrameMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveFrameMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveFrameMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveFrameMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveFrameMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveFrameMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveFrameMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveFrameMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveFrameMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveFrameMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveFrameMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveFrameMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveFrameMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveFrameMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveFrameMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveFrameMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveFrameMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveFrameMethod "getChild" o = Gtk.Bin.BinGetChildMethodInfo
    ResolveFrameMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveFrameMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveFrameMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveFrameMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveFrameMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveFrameMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveFrameMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveFrameMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveFrameMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveFrameMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveFrameMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveFrameMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveFrameMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveFrameMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveFrameMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveFrameMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveFrameMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveFrameMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveFrameMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveFrameMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveFrameMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveFrameMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveFrameMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveFrameMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveFrameMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveFrameMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveFrameMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveFrameMethod "getLabel" o = FrameGetLabelMethodInfo
    ResolveFrameMethod "getLabelAlign" o = FrameGetLabelAlignMethodInfo
    ResolveFrameMethod "getLabelWidget" o = FrameGetLabelWidgetMethodInfo
    ResolveFrameMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveFrameMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveFrameMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveFrameMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveFrameMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveFrameMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveFrameMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveFrameMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveFrameMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveFrameMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveFrameMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveFrameMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveFrameMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveFrameMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveFrameMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveFrameMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveFrameMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveFrameMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveFrameMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveFrameMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveFrameMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveFrameMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveFrameMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveFrameMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveFrameMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveFrameMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveFrameMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveFrameMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveFrameMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveFrameMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveFrameMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveFrameMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveFrameMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveFrameMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveFrameMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveFrameMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveFrameMethod "getShadowType" o = FrameGetShadowTypeMethodInfo
    ResolveFrameMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveFrameMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveFrameMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveFrameMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveFrameMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveFrameMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveFrameMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveFrameMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveFrameMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveFrameMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveFrameMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveFrameMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveFrameMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveFrameMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveFrameMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveFrameMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveFrameMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveFrameMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveFrameMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveFrameMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveFrameMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveFrameMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveFrameMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveFrameMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveFrameMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveFrameMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveFrameMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveFrameMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveFrameMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveFrameMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveFrameMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveFrameMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveFrameMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveFrameMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveFrameMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveFrameMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveFrameMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveFrameMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveFrameMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveFrameMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveFrameMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveFrameMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveFrameMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveFrameMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveFrameMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveFrameMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveFrameMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveFrameMethod "setLabel" o = FrameSetLabelMethodInfo
    ResolveFrameMethod "setLabelAlign" o = FrameSetLabelAlignMethodInfo
    ResolveFrameMethod "setLabelWidget" o = FrameSetLabelWidgetMethodInfo
    ResolveFrameMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveFrameMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveFrameMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveFrameMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveFrameMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveFrameMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveFrameMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveFrameMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveFrameMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveFrameMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveFrameMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveFrameMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveFrameMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveFrameMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveFrameMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveFrameMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveFrameMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveFrameMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveFrameMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveFrameMethod "setShadowType" o = FrameSetShadowTypeMethodInfo
    ResolveFrameMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveFrameMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveFrameMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveFrameMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveFrameMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveFrameMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveFrameMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveFrameMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveFrameMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveFrameMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveFrameMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveFrameMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveFrameMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveFrameMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveFrameMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveFrameMethod t Frame, O.OverloadedMethod info Frame p) => OL.IsLabel t (Frame -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveFrameMethod t Frame, O.OverloadedMethod info Frame p, R.HasField t Frame p) => R.HasField t Frame p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveFrameMethod t Frame, O.OverloadedMethodInfo info Frame) => OL.IsLabel t (O.MethodProxy info Frame) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- VVV Prop "label"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just True)

-- | Get the value of the “@label@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' frame #label
-- @
getFrameLabel :: (MonadIO m, IsFrame o) => o -> m (Maybe T.Text)
getFrameLabel obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "label"

-- | Set the value of the “@label@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' frame [ #label 'Data.GI.Base.Attributes.:=' value ]
-- @
setFrameLabel :: (MonadIO m, IsFrame o) => o -> T.Text -> m ()
setFrameLabel obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "label" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@label@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructFrameLabel :: (IsFrame o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructFrameLabel val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "label" (P.Just val)

-- | Set the value of the “@label@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #label
-- @
clearFrameLabel :: (MonadIO m, IsFrame o) => o -> m ()
clearFrameLabel obj = liftIO $ B.Properties.setObjectPropertyString obj "label" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data FrameLabelPropertyInfo
instance AttrInfo FrameLabelPropertyInfo where
    type AttrAllowedOps FrameLabelPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint FrameLabelPropertyInfo = IsFrame
    type AttrSetTypeConstraint FrameLabelPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint FrameLabelPropertyInfo = (~) T.Text
    type AttrTransferType FrameLabelPropertyInfo = T.Text
    type AttrGetType FrameLabelPropertyInfo = (Maybe T.Text)
    type AttrLabel FrameLabelPropertyInfo = "label"
    type AttrOrigin FrameLabelPropertyInfo = Frame
    attrGet = getFrameLabel
    attrSet = setFrameLabel
    attrTransfer _ v = do
        return v
    attrConstruct = constructFrameLabel
    attrClear = clearFrameLabel
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Frame.label"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Frame.html#g:attr:label"
        })
#endif

-- VVV Prop "label-widget"
   -- Type: TInterface (Name {namespace = "Gtk", name = "Widget"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just True)

-- | Get the value of the “@label-widget@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' frame #labelWidget
-- @
getFrameLabelWidget :: (MonadIO m, IsFrame o) => o -> m (Maybe Gtk.Widget.Widget)
getFrameLabelWidget obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "label-widget" Gtk.Widget.Widget

-- | Set the value of the “@label-widget@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' frame [ #labelWidget 'Data.GI.Base.Attributes.:=' value ]
-- @
setFrameLabelWidget :: (MonadIO m, IsFrame o, Gtk.Widget.IsWidget a) => o -> a -> m ()
setFrameLabelWidget obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "label-widget" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@label-widget@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructFrameLabelWidget :: (IsFrame o, MIO.MonadIO m, Gtk.Widget.IsWidget a) => a -> m (GValueConstruct o)
constructFrameLabelWidget val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "label-widget" (P.Just val)

-- | Set the value of the “@label-widget@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #labelWidget
-- @
clearFrameLabelWidget :: (MonadIO m, IsFrame o) => o -> m ()
clearFrameLabelWidget obj = liftIO $ B.Properties.setObjectPropertyObject obj "label-widget" (Nothing :: Maybe Gtk.Widget.Widget)

#if defined(ENABLE_OVERLOADING)
data FrameLabelWidgetPropertyInfo
instance AttrInfo FrameLabelWidgetPropertyInfo where
    type AttrAllowedOps FrameLabelWidgetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint FrameLabelWidgetPropertyInfo = IsFrame
    type AttrSetTypeConstraint FrameLabelWidgetPropertyInfo = Gtk.Widget.IsWidget
    type AttrTransferTypeConstraint FrameLabelWidgetPropertyInfo = Gtk.Widget.IsWidget
    type AttrTransferType FrameLabelWidgetPropertyInfo = Gtk.Widget.Widget
    type AttrGetType FrameLabelWidgetPropertyInfo = (Maybe Gtk.Widget.Widget)
    type AttrLabel FrameLabelWidgetPropertyInfo = "label-widget"
    type AttrOrigin FrameLabelWidgetPropertyInfo = Frame
    attrGet = getFrameLabelWidget
    attrSet = setFrameLabelWidget
    attrTransfer _ v = do
        unsafeCastTo Gtk.Widget.Widget v
    attrConstruct = constructFrameLabelWidget
    attrClear = clearFrameLabelWidget
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Frame.labelWidget"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Frame.html#g:attr:labelWidget"
        })
#endif

-- VVV Prop "label-xalign"
   -- Type: TBasicType TFloat
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@label-xalign@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' frame #labelXalign
-- @
getFrameLabelXalign :: (MonadIO m, IsFrame o) => o -> m Float
getFrameLabelXalign obj = MIO.liftIO $ B.Properties.getObjectPropertyFloat obj "label-xalign"

-- | Set the value of the “@label-xalign@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' frame [ #labelXalign 'Data.GI.Base.Attributes.:=' value ]
-- @
setFrameLabelXalign :: (MonadIO m, IsFrame o) => o -> Float -> m ()
setFrameLabelXalign obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyFloat obj "label-xalign" val

-- | Construct a `GValueConstruct` with valid value for the “@label-xalign@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructFrameLabelXalign :: (IsFrame o, MIO.MonadIO m) => Float -> m (GValueConstruct o)
constructFrameLabelXalign val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyFloat "label-xalign" val

#if defined(ENABLE_OVERLOADING)
data FrameLabelXalignPropertyInfo
instance AttrInfo FrameLabelXalignPropertyInfo where
    type AttrAllowedOps FrameLabelXalignPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint FrameLabelXalignPropertyInfo = IsFrame
    type AttrSetTypeConstraint FrameLabelXalignPropertyInfo = (~) Float
    type AttrTransferTypeConstraint FrameLabelXalignPropertyInfo = (~) Float
    type AttrTransferType FrameLabelXalignPropertyInfo = Float
    type AttrGetType FrameLabelXalignPropertyInfo = Float
    type AttrLabel FrameLabelXalignPropertyInfo = "label-xalign"
    type AttrOrigin FrameLabelXalignPropertyInfo = Frame
    attrGet = getFrameLabelXalign
    attrSet = setFrameLabelXalign
    attrTransfer _ v = do
        return v
    attrConstruct = constructFrameLabelXalign
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Frame.labelXalign"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Frame.html#g:attr:labelXalign"
        })
#endif

-- VVV Prop "label-yalign"
   -- Type: TBasicType TFloat
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@label-yalign@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' frame #labelYalign
-- @
getFrameLabelYalign :: (MonadIO m, IsFrame o) => o -> m Float
getFrameLabelYalign obj = MIO.liftIO $ B.Properties.getObjectPropertyFloat obj "label-yalign"

-- | Set the value of the “@label-yalign@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' frame [ #labelYalign 'Data.GI.Base.Attributes.:=' value ]
-- @
setFrameLabelYalign :: (MonadIO m, IsFrame o) => o -> Float -> m ()
setFrameLabelYalign obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyFloat obj "label-yalign" val

-- | Construct a `GValueConstruct` with valid value for the “@label-yalign@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructFrameLabelYalign :: (IsFrame o, MIO.MonadIO m) => Float -> m (GValueConstruct o)
constructFrameLabelYalign val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyFloat "label-yalign" val

#if defined(ENABLE_OVERLOADING)
data FrameLabelYalignPropertyInfo
instance AttrInfo FrameLabelYalignPropertyInfo where
    type AttrAllowedOps FrameLabelYalignPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint FrameLabelYalignPropertyInfo = IsFrame
    type AttrSetTypeConstraint FrameLabelYalignPropertyInfo = (~) Float
    type AttrTransferTypeConstraint FrameLabelYalignPropertyInfo = (~) Float
    type AttrTransferType FrameLabelYalignPropertyInfo = Float
    type AttrGetType FrameLabelYalignPropertyInfo = Float
    type AttrLabel FrameLabelYalignPropertyInfo = "label-yalign"
    type AttrOrigin FrameLabelYalignPropertyInfo = Frame
    attrGet = getFrameLabelYalign
    attrSet = setFrameLabelYalign
    attrTransfer _ v = do
        return v
    attrConstruct = constructFrameLabelYalign
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Frame.labelYalign"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Frame.html#g:attr:labelYalign"
        })
#endif

-- VVV Prop "shadow-type"
   -- Type: TInterface (Name {namespace = "Gtk", name = "ShadowType"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@shadow-type@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' frame #shadowType
-- @
getFrameShadowType :: (MonadIO m, IsFrame o) => o -> m Gtk.Enums.ShadowType
getFrameShadowType obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "shadow-type"

-- | Set the value of the “@shadow-type@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' frame [ #shadowType 'Data.GI.Base.Attributes.:=' value ]
-- @
setFrameShadowType :: (MonadIO m, IsFrame o) => o -> Gtk.Enums.ShadowType -> m ()
setFrameShadowType obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "shadow-type" val

-- | Construct a `GValueConstruct` with valid value for the “@shadow-type@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructFrameShadowType :: (IsFrame o, MIO.MonadIO m) => Gtk.Enums.ShadowType -> m (GValueConstruct o)
constructFrameShadowType val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "shadow-type" val

#if defined(ENABLE_OVERLOADING)
data FrameShadowTypePropertyInfo
instance AttrInfo FrameShadowTypePropertyInfo where
    type AttrAllowedOps FrameShadowTypePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint FrameShadowTypePropertyInfo = IsFrame
    type AttrSetTypeConstraint FrameShadowTypePropertyInfo = (~) Gtk.Enums.ShadowType
    type AttrTransferTypeConstraint FrameShadowTypePropertyInfo = (~) Gtk.Enums.ShadowType
    type AttrTransferType FrameShadowTypePropertyInfo = Gtk.Enums.ShadowType
    type AttrGetType FrameShadowTypePropertyInfo = Gtk.Enums.ShadowType
    type AttrLabel FrameShadowTypePropertyInfo = "shadow-type"
    type AttrOrigin FrameShadowTypePropertyInfo = Frame
    attrGet = getFrameShadowType
    attrSet = setFrameShadowType
    attrTransfer _ v = do
        return v
    attrConstruct = constructFrameShadowType
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Frame.shadowType"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Frame.html#g:attr:shadowType"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList Frame
type instance O.AttributeList Frame = FrameAttributeList
type FrameAttributeList = ('[ '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("label", FrameLabelPropertyInfo), '("labelWidget", FrameLabelWidgetPropertyInfo), '("labelXalign", FrameLabelXalignPropertyInfo), '("labelYalign", FrameLabelYalignPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("shadowType", FrameShadowTypePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
frameLabel :: AttrLabelProxy "label"
frameLabel = AttrLabelProxy

frameLabelWidget :: AttrLabelProxy "labelWidget"
frameLabelWidget = AttrLabelProxy

frameLabelXalign :: AttrLabelProxy "labelXalign"
frameLabelXalign = AttrLabelProxy

frameLabelYalign :: AttrLabelProxy "labelYalign"
frameLabelYalign = AttrLabelProxy

frameShadowType :: AttrLabelProxy "shadowType"
frameShadowType = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList Frame = FrameSignalList
type FrameSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method Frame::new
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the text to use as the label of the frame"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Frame" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_frame_new" gtk_frame_new :: 
    CString ->                              -- label : TBasicType TUTF8
    IO (Ptr Frame)

-- | Creates a new t'GI.Gtk.Objects.Frame.Frame', with optional label /@label@/.
-- If /@label@/ is 'P.Nothing', the label is omitted.
frameNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Maybe (T.Text)
    -- ^ /@label@/: the text to use as the label of the frame
    -> m Frame
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.Frame.Frame' widget
frameNew label = liftIO $ do
    maybeLabel <- case label of
        Nothing -> return nullPtr
        Just jLabel -> do
            jLabel' <- textToCString jLabel
            return jLabel'
    result <- gtk_frame_new maybeLabel
    checkUnexpectedReturnNULL "frameNew" result
    result' <- (newObject Frame) result
    freeMem maybeLabel
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Frame::get_label
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "frame"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Frame" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFrame" , sinceVersion = Nothing }
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

foreign import ccall "gtk_frame_get_label" gtk_frame_get_label :: 
    Ptr Frame ->                            -- frame : TInterface (Name {namespace = "Gtk", name = "Frame"})
    IO CString

-- | If the frame’s label widget is a t'GI.Gtk.Objects.Label.Label', returns the
-- text in the label widget. (The frame will have a t'GI.Gtk.Objects.Label.Label'
-- for the label widget if a non-'P.Nothing' argument was passed
-- to 'GI.Gtk.Objects.Frame.frameNew'.)
frameGetLabel ::
    (B.CallStack.HasCallStack, MonadIO m, IsFrame a) =>
    a
    -- ^ /@frame@/: a t'GI.Gtk.Objects.Frame.Frame'
    -> m (Maybe T.Text)
    -- ^ __Returns:__ the text in the label, or 'P.Nothing' if there
    --               was no label widget or the lable widget was not
    --               a t'GI.Gtk.Objects.Label.Label'. This string is owned by GTK+ and
    --               must not be modified or freed.
frameGetLabel frame = liftIO $ do
    frame' <- unsafeManagedPtrCastPtr frame
    result <- gtk_frame_get_label frame'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToText result'
        return result''
    touchManagedPtr frame
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data FrameGetLabelMethodInfo
instance (signature ~ (m (Maybe T.Text)), MonadIO m, IsFrame a) => O.OverloadedMethod FrameGetLabelMethodInfo a signature where
    overloadedMethod = frameGetLabel

instance O.OverloadedMethodInfo FrameGetLabelMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Frame.frameGetLabel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Frame.html#v:frameGetLabel"
        })


#endif

-- method Frame::get_label_align
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "frame"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Frame" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFrame" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "xalign"
--           , argType = TBasicType TFloat
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "location to store X alignment of\n    frame\8217s label, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "yalign"
--           , argType = TBasicType TFloat
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "location to store X alignment of\n    frame\8217s label, or %NULL"
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

foreign import ccall "gtk_frame_get_label_align" gtk_frame_get_label_align :: 
    Ptr Frame ->                            -- frame : TInterface (Name {namespace = "Gtk", name = "Frame"})
    Ptr CFloat ->                           -- xalign : TBasicType TFloat
    Ptr CFloat ->                           -- yalign : TBasicType TFloat
    IO ()

-- | Retrieves the X and Y alignment of the frame’s label. See
-- 'GI.Gtk.Objects.Frame.frameSetLabelAlign'.
frameGetLabelAlign ::
    (B.CallStack.HasCallStack, MonadIO m, IsFrame a) =>
    a
    -- ^ /@frame@/: a t'GI.Gtk.Objects.Frame.Frame'
    -> m ((Float, Float))
frameGetLabelAlign frame = liftIO $ do
    frame' <- unsafeManagedPtrCastPtr frame
    xalign <- allocMem :: IO (Ptr CFloat)
    yalign <- allocMem :: IO (Ptr CFloat)
    gtk_frame_get_label_align frame' xalign yalign
    xalign' <- peek xalign
    let xalign'' = realToFrac xalign'
    yalign' <- peek yalign
    let yalign'' = realToFrac yalign'
    touchManagedPtr frame
    freeMem xalign
    freeMem yalign
    return (xalign'', yalign'')

#if defined(ENABLE_OVERLOADING)
data FrameGetLabelAlignMethodInfo
instance (signature ~ (m ((Float, Float))), MonadIO m, IsFrame a) => O.OverloadedMethod FrameGetLabelAlignMethodInfo a signature where
    overloadedMethod = frameGetLabelAlign

instance O.OverloadedMethodInfo FrameGetLabelAlignMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Frame.frameGetLabelAlign",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Frame.html#v:frameGetLabelAlign"
        })


#endif

-- method Frame::get_label_widget
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "frame"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Frame" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFrame" , sinceVersion = Nothing }
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

foreign import ccall "gtk_frame_get_label_widget" gtk_frame_get_label_widget :: 
    Ptr Frame ->                            -- frame : TInterface (Name {namespace = "Gtk", name = "Frame"})
    IO (Ptr Gtk.Widget.Widget)

-- | Retrieves the label widget for the frame. See
-- 'GI.Gtk.Objects.Frame.frameSetLabelWidget'.
frameGetLabelWidget ::
    (B.CallStack.HasCallStack, MonadIO m, IsFrame a) =>
    a
    -- ^ /@frame@/: a t'GI.Gtk.Objects.Frame.Frame'
    -> m (Maybe Gtk.Widget.Widget)
    -- ^ __Returns:__ the label widget, or 'P.Nothing' if
    -- there is none.
frameGetLabelWidget frame = liftIO $ do
    frame' <- unsafeManagedPtrCastPtr frame
    result <- gtk_frame_get_label_widget frame'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Widget.Widget) result'
        return result''
    touchManagedPtr frame
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data FrameGetLabelWidgetMethodInfo
instance (signature ~ (m (Maybe Gtk.Widget.Widget)), MonadIO m, IsFrame a) => O.OverloadedMethod FrameGetLabelWidgetMethodInfo a signature where
    overloadedMethod = frameGetLabelWidget

instance O.OverloadedMethodInfo FrameGetLabelWidgetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Frame.frameGetLabelWidget",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Frame.html#v:frameGetLabelWidget"
        })


#endif

-- method Frame::get_shadow_type
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "frame"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Frame" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFrame" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "ShadowType" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_frame_get_shadow_type" gtk_frame_get_shadow_type :: 
    Ptr Frame ->                            -- frame : TInterface (Name {namespace = "Gtk", name = "Frame"})
    IO CUInt

-- | Retrieves the shadow type of the frame. See
-- 'GI.Gtk.Objects.Frame.frameSetShadowType'.
frameGetShadowType ::
    (B.CallStack.HasCallStack, MonadIO m, IsFrame a) =>
    a
    -- ^ /@frame@/: a t'GI.Gtk.Objects.Frame.Frame'
    -> m Gtk.Enums.ShadowType
    -- ^ __Returns:__ the current shadow type of the frame.
frameGetShadowType frame = liftIO $ do
    frame' <- unsafeManagedPtrCastPtr frame
    result <- gtk_frame_get_shadow_type frame'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr frame
    return result'

#if defined(ENABLE_OVERLOADING)
data FrameGetShadowTypeMethodInfo
instance (signature ~ (m Gtk.Enums.ShadowType), MonadIO m, IsFrame a) => O.OverloadedMethod FrameGetShadowTypeMethodInfo a signature where
    overloadedMethod = frameGetShadowType

instance O.OverloadedMethodInfo FrameGetShadowTypeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Frame.frameGetShadowType",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Frame.html#v:frameGetShadowType"
        })


#endif

-- method Frame::set_label
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "frame"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Frame" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFrame" , sinceVersion = Nothing }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the text to use as the label of the frame"
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

foreign import ccall "gtk_frame_set_label" gtk_frame_set_label :: 
    Ptr Frame ->                            -- frame : TInterface (Name {namespace = "Gtk", name = "Frame"})
    CString ->                              -- label : TBasicType TUTF8
    IO ()

-- | Removes the current [Frame:labelWidget]("GI.Gtk.Objects.Frame#g:attr:labelWidget"). If /@label@/ is not 'P.Nothing', creates a
-- new t'GI.Gtk.Objects.Label.Label' with that text and adds it as the [Frame:labelWidget]("GI.Gtk.Objects.Frame#g:attr:labelWidget").
frameSetLabel ::
    (B.CallStack.HasCallStack, MonadIO m, IsFrame a) =>
    a
    -- ^ /@frame@/: a t'GI.Gtk.Objects.Frame.Frame'
    -> Maybe (T.Text)
    -- ^ /@label@/: the text to use as the label of the frame
    -> m ()
frameSetLabel frame label = liftIO $ do
    frame' <- unsafeManagedPtrCastPtr frame
    maybeLabel <- case label of
        Nothing -> return nullPtr
        Just jLabel -> do
            jLabel' <- textToCString jLabel
            return jLabel'
    gtk_frame_set_label frame' maybeLabel
    touchManagedPtr frame
    freeMem maybeLabel
    return ()

#if defined(ENABLE_OVERLOADING)
data FrameSetLabelMethodInfo
instance (signature ~ (Maybe (T.Text) -> m ()), MonadIO m, IsFrame a) => O.OverloadedMethod FrameSetLabelMethodInfo a signature where
    overloadedMethod = frameSetLabel

instance O.OverloadedMethodInfo FrameSetLabelMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Frame.frameSetLabel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Frame.html#v:frameSetLabel"
        })


#endif

-- method Frame::set_label_align
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "frame"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Frame" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFrame" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "xalign"
--           , argType = TBasicType TFloat
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "The position of the label along the top edge\n  of the widget. A value of 0.0 represents left alignment;\n  1.0 represents right alignment."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "yalign"
--           , argType = TBasicType TFloat
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "The y alignment of the label. A value of 0.0 aligns under\n  the frame; 1.0 aligns above the frame. If the values are exactly\n  0.0 or 1.0 the gap in the frame won\8217t be painted because the label\n  will be completely above or below the frame."
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

foreign import ccall "gtk_frame_set_label_align" gtk_frame_set_label_align :: 
    Ptr Frame ->                            -- frame : TInterface (Name {namespace = "Gtk", name = "Frame"})
    CFloat ->                               -- xalign : TBasicType TFloat
    CFloat ->                               -- yalign : TBasicType TFloat
    IO ()

-- | Sets the alignment of the frame widget’s label. The
-- default values for a newly created frame are 0.0 and 0.5.
frameSetLabelAlign ::
    (B.CallStack.HasCallStack, MonadIO m, IsFrame a) =>
    a
    -- ^ /@frame@/: a t'GI.Gtk.Objects.Frame.Frame'
    -> Float
    -- ^ /@xalign@/: The position of the label along the top edge
    --   of the widget. A value of 0.0 represents left alignment;
    --   1.0 represents right alignment.
    -> Float
    -- ^ /@yalign@/: The y alignment of the label. A value of 0.0 aligns under
    --   the frame; 1.0 aligns above the frame. If the values are exactly
    --   0.0 or 1.0 the gap in the frame won’t be painted because the label
    --   will be completely above or below the frame.
    -> m ()
frameSetLabelAlign frame xalign yalign = liftIO $ do
    frame' <- unsafeManagedPtrCastPtr frame
    let xalign' = realToFrac xalign
    let yalign' = realToFrac yalign
    gtk_frame_set_label_align frame' xalign' yalign'
    touchManagedPtr frame
    return ()

#if defined(ENABLE_OVERLOADING)
data FrameSetLabelAlignMethodInfo
instance (signature ~ (Float -> Float -> m ()), MonadIO m, IsFrame a) => O.OverloadedMethod FrameSetLabelAlignMethodInfo a signature where
    overloadedMethod = frameSetLabelAlign

instance O.OverloadedMethodInfo FrameSetLabelAlignMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Frame.frameSetLabelAlign",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Frame.html#v:frameSetLabelAlign"
        })


#endif

-- method Frame::set_label_widget
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "frame"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Frame" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFrame" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "label_widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the new label widget"
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

foreign import ccall "gtk_frame_set_label_widget" gtk_frame_set_label_widget :: 
    Ptr Frame ->                            -- frame : TInterface (Name {namespace = "Gtk", name = "Frame"})
    Ptr Gtk.Widget.Widget ->                -- label_widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | Sets the [Frame:labelWidget]("GI.Gtk.Objects.Frame#g:attr:labelWidget") for the frame. This is the widget that
-- will appear embedded in the top edge of the frame as a title.
frameSetLabelWidget ::
    (B.CallStack.HasCallStack, MonadIO m, IsFrame a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@frame@/: a t'GI.Gtk.Objects.Frame.Frame'
    -> Maybe (b)
    -- ^ /@labelWidget@/: the new label widget
    -> m ()
frameSetLabelWidget frame labelWidget = liftIO $ do
    frame' <- unsafeManagedPtrCastPtr frame
    maybeLabelWidget <- case labelWidget of
        Nothing -> return nullPtr
        Just jLabelWidget -> do
            jLabelWidget' <- unsafeManagedPtrCastPtr jLabelWidget
            return jLabelWidget'
    gtk_frame_set_label_widget frame' maybeLabelWidget
    touchManagedPtr frame
    whenJust labelWidget touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data FrameSetLabelWidgetMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsFrame a, Gtk.Widget.IsWidget b) => O.OverloadedMethod FrameSetLabelWidgetMethodInfo a signature where
    overloadedMethod = frameSetLabelWidget

instance O.OverloadedMethodInfo FrameSetLabelWidgetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Frame.frameSetLabelWidget",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Frame.html#v:frameSetLabelWidget"
        })


#endif

-- method Frame::set_shadow_type
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "frame"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Frame" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFrame" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ShadowType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the new #GtkShadowType"
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

foreign import ccall "gtk_frame_set_shadow_type" gtk_frame_set_shadow_type :: 
    Ptr Frame ->                            -- frame : TInterface (Name {namespace = "Gtk", name = "Frame"})
    CUInt ->                                -- type : TInterface (Name {namespace = "Gtk", name = "ShadowType"})
    IO ()

-- | Sets the [Frame:shadowType]("GI.Gtk.Objects.Frame#g:attr:shadowType") for /@frame@/, i.e. whether it is drawn without
-- ('GI.Gtk.Enums.ShadowTypeNone') or with (other values) a visible border. Values other than
-- 'GI.Gtk.Enums.ShadowTypeNone' are treated identically by GtkFrame. The chosen type is
-- applied by removing or adding the .flat class to the CSS node named border.
frameSetShadowType ::
    (B.CallStack.HasCallStack, MonadIO m, IsFrame a) =>
    a
    -- ^ /@frame@/: a t'GI.Gtk.Objects.Frame.Frame'
    -> Gtk.Enums.ShadowType
    -- ^ /@type@/: the new t'GI.Gtk.Enums.ShadowType'
    -> m ()
frameSetShadowType frame type_ = liftIO $ do
    frame' <- unsafeManagedPtrCastPtr frame
    let type_' = (fromIntegral . fromEnum) type_
    gtk_frame_set_shadow_type frame' type_'
    touchManagedPtr frame
    return ()

#if defined(ENABLE_OVERLOADING)
data FrameSetShadowTypeMethodInfo
instance (signature ~ (Gtk.Enums.ShadowType -> m ()), MonadIO m, IsFrame a) => O.OverloadedMethod FrameSetShadowTypeMethodInfo a signature where
    overloadedMethod = frameSetShadowType

instance O.OverloadedMethodInfo FrameSetShadowTypeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Frame.frameSetShadowType",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Frame.html#v:frameSetShadowType"
        })


#endif


